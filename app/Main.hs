module Main where

import Control.Exception
import Control.Monad.Freer
import Lib
import Math

run_ :: IO ()
run_ = do 
  (l, w) <- runM . runLogger @Log (emptyLog "Do a sample program") . runSafely 
              . (runWithLogged "Math" liftMath) $ do
    x <- safely "Construct the string 'Hello'" "Hello" return
    y <- safeWarn "Construct the string 'World'" "World" return Nothing
    _ <- safely "Concat them and print them out" (x <> " " <> y) putStrLn
    two <- mul 1 2
    five <- safely @Int "Add 2 to 3" two (return . (+3))
    _ <- addLogged "Add 2 more" five 2
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
