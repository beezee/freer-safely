module Logger where

import Control.Monad.Freer

data Logger w a where
  Tell :: w -> Logger w ()
  Reset :: w -> Logger w ()
  Told :: () -> Logger w w

tell :: Member (Logger w) effs => w -> Eff effs ()
tell = send . Tell

reset :: Member (Logger w) effs => w -> Eff effs ()
reset = send . Reset

told :: Member (Logger w) effs => () -> Eff effs w
told = send . Told

runLogger :: Semigroup w => w -> Eff (Logger w ': effs) a -> Eff effs (a, w)
runLogger sw = handleRelayS sw (\w a -> pure (a, w)) $ \w l -> case l of
  (Tell w1) -> \k -> (k (w <> w1) ())
  (Reset w1) -> \k -> (k w1 ())
  (Told _) -> \k -> k w w
