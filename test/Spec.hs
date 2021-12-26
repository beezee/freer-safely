import Control.Arrow
import Control.Monad.Freer hiding (run)
import Data.Aeson
import Data.Functor.Compose
import Data.Text
import GHC.Exts (fromList)
import Lib
import Math
import Test.QuickCheck
import Test.QuickCheck.Monadic

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

instance Arbitrary (Run Int) where
  arbitrary = oneof [
    sized . sizedArbitrarySafely_ $ elements =<< mightFail,
    genMath]
    where
    mightFail :: Gen [Int -> IO Int]
    mightFail =
      flip (:) [return, return, return] <$> const . error <$> arbitrary

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

main :: IO ()
main = quickCheck prop_liftPreservesBehavior
