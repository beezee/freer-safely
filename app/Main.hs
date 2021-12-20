module Main where

import Control.Exception
import Control.Monad.Freer
import Lib

run_ :: IO ()
run_ = do 
  (l, w) <- runM . runLogger @Log (emptyLog "Do a sample program") . runSafely $ do
    x <- safely "Construct the string 'Hello'" "Hello" return
    y <- safeWarn "Construct the string 'World'" "World" return Nothing
    _ <- safely "Concat them and print them out" (x <> " " <> y) putStrLn
    _ <- safely @Int "Add 2 to 3" 2 (return . (+3))
    _ <- safely "Do a step that will fail badly" "Big bada boom" error
    told @Log ()
  putStrLn . show $ w
  putStrLn . show $ l

main :: IO ()
main = do
  r <- try run_
  case r of
    Left (LoggedErr (w, e :: SomeException)) -> (putStrLn . show $ w) >> (putStrLn . show $ e)
    Right u -> return u
