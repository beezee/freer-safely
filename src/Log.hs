module Log where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8
import Data.Functor.Const
import Data.Semigroup

newtype LogSanitize a = LogSanitize a
newtype LogSanitized a = LogSanitized Value
  deriving (Show)

logSanitizedJSON :: LogSanitized a -> Value
logSanitizedJSON (LogSanitized v) = v

class NoSanitizeNeeded a where

instance ToJSON (LogSanitize String) where
  toJSON = unsanitize

instance ToJSON (LogSanitize Int) where
  toJSON = unsanitize

unsanitize :: ToJSON a => LogSanitize a -> Value
unsanitize (LogSanitize a) = toJSON a

lsToConst :: LogSanitized a -> Const Value a
lsToConst (LogSanitized v) = Const v

lsFromConst :: Const Value a -> LogSanitized a
lsFromConst (Const v) = LogSanitized v

instance Functor LogSanitized where
  fmap f = lsFromConst . fmap f . lsToConst

data Level = Debug | Info | Warn | Err
  deriving (Eq, Show)

levelNum :: Level -> Int
levelNum Debug = 0
levelNum Info = 1
levelNum Warn = 2
levelNum Err = 3

instance Ord Level where
  l1 <= l2 = levelNum l1 <= levelNum l2

data Trace = Trace { 
  traceMsg :: String , traceLevel :: Level , aux :: LogSanitized (),
  input :: LogSanitized () }
  deriving (Show)

instance ToJSON Trace where
  toJSON trace = object [
    "msg" .= traceMsg trace, "level" .= (show . traceLevel $ trace),
    "aux" .= (logSanitizedJSON . aux $ trace),
    "input" .= (logSanitizedJSON . input $ trace) ]

data Log = Log { 
  logMsg :: First String , logLevel :: Max Level , logTrace :: [Trace] }

instance Show Log where
  show l = unpack . encodePretty . toJSON $ l

instance ToJSON Log where
  toJSON l = object [
    "msg" .= logMsg l, "level" .= (show . getMax . logLevel $ l),
    "trace" .= (toJSONList . logTrace $ l) ]

instance Semigroup Log where
  (Log m1 l1 t1) <> (Log m2 l2 t2) = Log (m1 <> m2) (l1 <> l2) (t1 <> t2)

emptyLog :: String -> Log
emptyLog msg = Log { logMsg = (First msg), logLevel = (Max Debug), logTrace = [] }

instance Monoid Log where
  mempty = emptyLog "No log message set"
