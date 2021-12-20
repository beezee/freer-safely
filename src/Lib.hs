module Lib
    ( 
      module Log,
      module Logger,
      module Safely,
      someFunc
    ) where

import Log
import Logger
import Safely

someFunc :: IO ()
someFunc = putStrLn "someFunc"
