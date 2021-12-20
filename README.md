# freer-safely

You don't know what you need to log until you need it in the log, and by then it's late.

If you statically enforce attaching a log message, as well as log the input given, to
every kleisli you evaluate in your primary runtime effect, every evaluation of your
code can record the equivalent of a step-through debug session in machine and human
readable format, and life will be beautiful.

For example this code

```haskell
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
```

produces this output

```shell
Hello World
{
    "trace": [
        {
            "aux": {},
            "input": "Hello",
            "msg": "Say hi",
            "level": "Info"
        },
        {
            "aux": {},
            "input": "World",
            "msg": "To the world",
            "level": "Warn"
        },
        {
            "aux": {},
            "input": "Hello World",
            "msg": "Now",
            "level": "Info"
        },
        {
            "aux": {},
            "input": 2,
            "msg": "Add 2 to 3",
            "level": "Info"
        },
        {
            "aux": {},
            "input": "Big bada boom",
            "msg": "Boom",
            "level": "Info"
        }
    ],
    "msg": "Do a sample program",
    "level": "Warn"
}
Big bada boom
CallStack (from HasCallStack):
  error, called at app/Main.hs:14:40 in main:Main
```

Say goodbye to forensic investigation, your system is now an omnipresent informant with
unbeatable note-taking skills
