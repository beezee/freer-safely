import Control.Applicative
import Control.Arrow
import Control.Exception hiding (assert)
import Control.Monad.Freer hiding (run)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL 
import Data.Functor.Compose
import Data.Functor.Identity
import Data.List (intercalate)
import Data.Text hiding (init, intercalate, reverse)
import GHC.Exts (fromList)
import Lib
import Math
import Test.QuickCheck hiding (reason)
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property

newtype Arb a = Arb a

unarb :: Arb a -> a
unarb (Arb a) = a

instance Arbitrary (Arb Value) where
  arbitrary = Arb <$> sized sizedArbitraryValue

-- thanks https://gist.github.com/chrisdone/7b0c4ebb5b9b94514959206df8992076
sizedArbitraryValue :: Int -> Gen Value
sizedArbitraryValue n
  | n <= 0 = oneof [pure Null, bool, number, string]
  | otherwise = resize n' $ oneof [pure Null, bool, number, string, array, object']
  where
    n' = n `div` 2
    bool = Bool <$> arbitrary
    number = (Number . fromRational . toRational @Double) <$> arbitrary
    string = (String . pack) <$> arbitrary
    array = (Array . fromList) . (unarb <$>) <$> arbitrary
    object' = (Object . fromList . ((pack *** unarb) <$>)) <$> arbitrary

newtype Run a = Run (Eff '[Math, Compose Logged Math, Safely, Logger Log, IO] a)

unRun :: Run a -> Eff '[Math, Compose Logged Math, Safely, Logger Log, IO] a
unRun (Run fa) = fa

instance Arbitrary (Arb Level) where
  arbitrary = Arb <$> elements [Debug, Info, Warn, Err]

genMathWithLogged :: Gen ((Run Int), (Run Int))
genMathWithLogged =
  ((Run *** Run) <$>) $ oneof [
    (\(m, x, y) -> (addLogged m x y, add x y)) <$> arbitrary,
    (\(m, x, y) -> (mulLogged m x y, mul x y)) <$> arbitrary]

genMath :: Gen (Run Int)
genMath = elements [fst, snd] <*> genMathWithLogged

newtype LogNoLog a = LogNoLog a

unLnl :: LogNoLog a -> a
unLnl (LogNoLog a) = a

instance Show (LogNoLog a) where
  show _ = "<LogNoLog>"

instance Show (Run Int) where
  show _ = "<Run Int>"

instance Arbitrary (LogNoLog (Run Int, Run Int)) where
  arbitrary = LogNoLog <$> oneof [
    genMathWithLogged, genMathWithLogged, 
    ((id &&& id) . unNoFail) <$> arbitrary]

newtype NoFail a = NoFail a

unNoFail :: NoFail a -> a
unNoFail (NoFail a) = a

instance Arbitrary (NoFail (Run Int)) where
  arbitrary = (NoFail <$>) $ oneof [
    sized . sizedArbitrarySafely_ $ return return,
    genMath]

-- LOL. this locks the callstack down for log comparisons
constErr :: String -> a -> IO b
constErr = const . error

instance Arbitrary (Run Int) where
  arbitrary = oneof [
    sized . sizedArbitrarySafely_ $ elements =<< mightFail,
    genMath]
    where
    mightFail :: Gen [Int -> IO Int]
    mightFail =
      flip (:) [return, return, return] <$> constErr <$> arbitrary

newtype AllLogged a = AllLogged a

instance Show a => Show (AllLogged a) where
  show (AllLogged a) = show a

unAllLogged :: AllLogged a -> a
unAllLogged (AllLogged a) = a

instance Arbitrary (AllLogged (Run Int)) where
  arbitrary = AllLogged <$> oneof [
    sized . sizedArbitrarySafely_ $ elements =<< mightFail,
    fst <$> genMathWithLogged]
    where
    mightFail :: Gen [Int -> IO Int]
    mightFail =
      flip (:) [return, return, return] <$> constErr <$> arbitrary

sizedArbitrarySafely_ :: Gen (Int -> IO Int) -> Int -> Gen (Run Int)
sizedArbitrarySafely_ ret n = 
  (Run <$>) $ safeLog_
    <$> (unarb <$> arbitrary) <*> arbitrary 
    <*> arbitrary <*> ret <*> (Just . LogSanitized <$> sizedArbitraryValue n)

data f ~> g = NT (forall a. f a -> g a )

instance Show (f ~> g) where
  show _ = "<natural transformation>"

instance Arbitrary (Math ~> Eff '[Safely, Logger Log, IO]) where
  arbitrary = do
    adder <- arbitrary @((Int, Int) -> Int)
    multer <- arbitrary @((Int, Int) -> Int)
    return $ NT (\e -> case e of
      Add x y -> return . adder $ (x, y)
      Mul x y -> return . multer $ (x, y))

dbl :: (a -> b) -> (a, a) -> (b, b)
dbl f = f *** f

seqTuple :: Applicative f => (f a, f b) -> f (a, b)
seqTuple (fa, fb) = (,) <$> fa <*> fb

prop_liftPreservesBehavior 
  :: [LogNoLog (Run Int, Run Int)] 
  -> (Math ~> Eff '[Safely, Logger Log, IO]) -> Property
prop_liftPreservesBehavior progs (NT interp) = monadicIO $ do
  ((logR, _), (noLogR, _)) <- seqTuple $ dbl (ev . traverse unRun) $ unzip $ unLnl <$> progs
  assert $ logR == noLogR
  where
  ev = run . runM . runLogger @Log (emptyLog "test") . runSafely . runWithLogged "Math" interp

strToLevel :: (MonadFail m) => String -> m Level
strToLevel "Debug" = return Debug
strToLevel "Info" = return Info
strToLevel "Warn" = return Warn
strToLevel "Err" = return Err
strToLevel i = fail ("Invalid log level: " <> i)

data MkProg = M (String, Math Int) | S (Safely Int) | E String

parseTrace :: Value -> Parser MkProg
parseTrace v = 
  ((withObject "Trace" $ \o -> do
    (x:y:[]) <- o .: "input" >>= (.: "times")
    m <- o .: "msg"
    return . M $ (m, Mul x y)) v)
  <|> ((withObject "Trace" $ \o -> do
        (x:y:[]) <- o .: "input" >>= (.: "plus")
        m <- o .: "msg"
        return . M $ (m, Add x y)) v)
  <|> ((withObject "Trace" $ \o -> do
        i <- o .: "input"
        m <- o .: "msg"
        l <- (o .: "level") >>= strToLevel
        a <- o .: "aux"
        return . S $ Safely m (Pairf (Identity i, logSanitize i)) return (LogSanitized a) l) v)
  <|> ((withObject "Trace" $ \o -> do
        l <- o .: "aux" >>= (.: "exceptionLines")
        let e = intercalate "\n" (init . init $ l)
        return . E $ e) v)

instance {-# OVERLAPPING #-} FromJSON [Run Int] where
  parseJSON = withObject "Log" $ \o -> do
    pvs <- o .: "trace"
    ps <- traverse parseTrace pvs
    return . reverse . traces $ ps
    where
    traces :: [MkProg] -> [Run Int]
    traces [] = []
    traces (h:t) = tracesRec [] h t
    tracesRec :: [Run Int] -> MkProg -> [MkProg] -> [Run Int]
    tracesRec acc (S (Safely m i _ a l)) ((E e):_) =
      (Run . send $ Safely m i (constErr $ e) a l):acc
    tracesRec acc prev [] = (build prev):acc
    tracesRec acc prev (h:t) = tracesRec ((build prev):acc) h t
    build :: MkProg -> Run Int
    build (E e) = Run $ safeErr "Impossible" (0 :: Int) (constErr $ e) Nothing
    build (S x) = Run . send $ x
    build (M (m, x)) = Run $ logged m x

prop_parseLogIsSection
  :: [AllLogged (Run Int)]
  -> (Math ~> Eff '[Safely, Logger Log, IO]) -> Property
prop_parseLogIsSection progs (NT interp) = monadicIO $ do
  (res, logs) <- ev $ traverse (unRun . unAllLogged) progs
  newProgs <- either (\e -> fail (e <> show logs)) return $ eitherDecode . BL.pack . show $ logs
  (newRes, newLogs) <- ev $ traverse unRun newProgs
  _ <- return $ 
    if res == newRes then succeeded else failed { reason = show res <> " /= " <> show newRes }
  return $ 
    if logs == newLogs then succeeded else failed { reason = show logs <> " /= " <> show newLogs }
  where
  ev = run . h . runM . runLogger @Log (emptyLog "test") . runSafely 
    . runWithLogged "Math" interp
  h x = (\e -> case e of
    Left (LoggedErr (l, _ :: SomeException)) -> ([], l)
    Right r -> r) <$> try x

main :: IO ()
main = 
  quickCheck prop_liftPreservesBehavior >> 
  quickCheck prop_parseLogIsSection
