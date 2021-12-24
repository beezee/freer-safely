{-# LANGUAGE RankNTypes #-}
module Logged where

import Control.Monad.Freer
import Control.Monad.Freer.Util
import Data.Aeson
import Data.Functor
import Data.Functor.Compose
import Data.Semigroup
import Log
import Logger

data Logged a where
  Logged :: String -> Level -> LogSanitized () -> (LogSanitized a, a) -> Logged a

logged_
  :: (Member (Compose Logged eff) effs, ToJSON (LogSanitize (eff a)))
  => Level -> String -> eff a -> Maybe (LogSanitized ()) -> Eff effs a
logged_ l msg arg a = 
  send . Compose $ Logged msg l (maybe (LogSanitized $ object []) id a) (la, arg)
    where la = LogSanitized . toJSON . LogSanitize $ arg

logged
  :: (Member (Compose Logged eff) effs, ToJSON (LogSanitize (eff a)))
  => String -> eff a -> Eff effs a
logged msg arg =
  logged_ Info msg arg Nothing 

loggedDebug
  :: (Member (Compose Logged eff) effs, ToJSON (LogSanitize (eff a)))
  => String -> eff a -> Maybe (LogSanitized ()) -> Eff effs a
loggedDebug msg arg a =
  logged_ Debug msg arg a

loggedInfo
  :: (Member (Compose Logged eff) effs, ToJSON (LogSanitize (eff a)))
  => String -> eff a -> Maybe (LogSanitized ()) -> Eff effs a
loggedInfo msg arg a =
  logged_ Info msg arg a

loggedWarn
  :: (Member (Compose Logged eff) effs, ToJSON (LogSanitize (eff a)))
  => String -> eff a -> Maybe (LogSanitized ()) -> Eff effs a
loggedWarn msg arg a =
  logged_ Warn msg arg a

loggedErr
  :: (Member (Compose Logged eff) effs, ToJSON (LogSanitize (eff a)))
  => String -> eff a -> Maybe (LogSanitized ()) -> Eff effs a
loggedErr msg arg a =
  logged_ Err msg arg a

runLogged 
  :: forall eff effs a
   . Member (Logger Log) effs
  => (forall x. eff x -> Eff effs x)
  -> Eff ((Compose Logged eff) ': effs) a
  -> Eff effs a
runLogged nt m = handleRelay return run_ m
  where
    run_ :: (Compose Logged eff) v -> (v -> Eff effs b) -> Eff effs b
    run_ (Compose (Logged msg l a (la, fa))) k = do
      _ <- tell (Log (First msg) (Max l) [Trace msg l a (la $> ())])
      nt fa >>= k

runWithLogged 
  :: forall eff effs a
   . Member (Logger Log) (eff ': effs)
  => (forall x. eff x -> Eff effs x)
  -> Eff ((Compose Logged eff) ': eff ': effs) a
  -> Eff effs a
runWithLogged nt = 
  (handleRelay return ((>>=) . nt)) . (runLogged $ raise . nt)
