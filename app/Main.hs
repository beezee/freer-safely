module Main where

import Control.Exception
import Control.Monad.Freer
import Lib

run_ :: IO ()
run_ = do 
  (l, w) <- runM . runLogger @Log (emptyLog "Do a sample program") . runSafely $ do
    x <- safely "Say hi" "Hello" return
    y <- safeWarn "To the world" "World" return Nothing
    _ <- safely "Now" (x <> " " <> y) putStrLn
    _ <- safely @Int "Add 2 to 3" 2 (return . (+3))
    _ <- safely "Boom" "Big bada boom" error
    told @Log ()
  putStrLn . show $ w
  putStrLn . show $ l

main :: IO ()
main = do
  r <- try run_
  case r of
    Left (LoggedErr (w, e :: SomeException)) -> (putStrLn . show $ w) >> (putStrLn . show $ e)
    Right u -> return u
