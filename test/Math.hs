module Math where

import Control.Monad.Freer
import Data.Aeson
import Data.Functor.Compose
import GHC.Exts
import Lib

jsonPair :: Value -> Value -> Value
jsonPair x y = Array . fromList $ [x, y]

instance ToJSON (LogSanitize (Math a)) where
  toJSON (LogSanitize (Mul x y)) = object ["times" .= jsonPair (toJSON x) (toJSON y)]
  toJSON (LogSanitize (Add x y)) = object ["plus" .= jsonPair (toJSON x) (toJSON y)]

data Math a where
  Mul :: Int -> Int -> Math Int
  Add :: Int -> Int -> Math Int

mulLogged :: (Member (Compose Logged Math) effs) => String -> Int -> Int -> Eff effs Int
mulLogged msg x y = loggedInfo msg (Mul x y) Nothing

addLogged :: (Member (Compose Logged Math) effs) => String -> Int -> Int -> Eff effs Int
addLogged msg x y = loggedInfo msg (Add x y) Nothing

mul :: (Member Math effs) => Int -> Int -> Eff effs Int
mul x y = send $ Mul x y

add :: (Member Math effs) => Int -> Int -> Eff effs Int
add x y = send $ Add x y
