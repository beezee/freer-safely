module Safely where

import Control.Arrow
import Control.Exception
import Control.Monad.Freer
import Data.Aeson
import Data.Functor
import Data.Functor.Identity
import Data.Semigroup
import Log
import Logger

data LoggedErr e = LoggedErr (Log, e)
  deriving (Show)

instance Exception e => Exception (LoggedErr e)

newtype Pairf f g a = Pairf (f a , g a)

data Safely r where
  Safely 
    :: String -> Pairf Identity LogSanitized a -> (a -> IO b)
    -> LogSanitized () -> Level -> Safely b

safely :: (ToJSON (LogSanitize a), Member Safely effs) => String -> a -> (a -> IO b) -> Eff effs b
safely msg arg k = send $ Safely msg (Pairf . pr $ arg) k (LogSanitized $ object []) Info
  where pr = Identity &&& LogSanitized . toJSON . LogSanitize

safeLog_
  :: (Member Safely effs, ToJSON (LogSanitize a))
  => Level -> String -> a -> (a -> IO b) -> Maybe (LogSanitized ()) -> Eff effs b
safeLog_ l msg arg k a = 
  send $ Safely msg (Pairf . pr $ arg) k (maybe (LogSanitized $ object []) id a) l
    where pr = Identity &&& LogSanitized . toJSON . LogSanitize 

safeDebug
  :: (Member Safely effs, ToJSON (LogSanitize a))
  => String -> a -> (a -> IO b) -> Maybe (LogSanitized ()) -> Eff effs b
safeDebug = safeLog_ Debug

safeInfo
  :: (Member Safely effs, ToJSON (LogSanitize a))
  => String -> a -> (a -> IO b) -> Maybe (LogSanitized ()) -> Eff effs b
safeInfo = safeLog_ Info

safeWarn
  :: (Member Safely effs, ToJSON (LogSanitize a))
  => String -> a -> (a -> IO b) -> Maybe (LogSanitized ()) -> Eff effs b
safeWarn = safeLog_ Warn

safeErr
  :: (Member Safely effs, ToJSON (LogSanitize a))
  => String -> a -> (a -> IO b) -> Maybe (LogSanitized ()) -> Eff effs b
safeErr = safeLog_ Err


liftSafely 
  :: forall effs a
   . (Member IO effs, Member (Logger Log) effs) => Safely a -> Eff effs a
liftSafely (Safely msg (Pairf (Identity arg, logArg)) k a l) = do
  _ <- tell (Log (First msg) (Max l) [Trace msg l a (logArg $> ())])
  recover k arg
  where
  recover :: (x -> IO y) -> x -> Eff effs y
  recover f x = do
    t <- send . try $ f x
    case t of
      Left (e :: SomeException) -> 
        (send . throwIO . LoggedErr . (flip (,) e)) =<< 
          (flip (<>) (traceAux Err (msg <> ": Exception") e) <$> told @Log ())
      Right r -> return r

runSafely 
  :: forall effs a
   . (Member IO effs, Member (Logger Log) effs) => Eff (Safely ': effs) a -> Eff effs a
runSafely = 
  handleRelay return ((>>=) . liftSafely)
