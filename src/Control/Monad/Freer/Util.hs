module Control.Monad.Freer.Util where

import Control.Monad.Freer
import Control.Monad.Freer.Internal

raise :: forall e effs a. Eff effs a -> Eff (e ': effs) a
raise = loop
  where
    loop (Val x) = pure x
    loop (E u q) = E (weaken u) . tsingleton $ qComp q loop
{-# INLINE raise #-}
