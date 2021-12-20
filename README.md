# freer-safely

You don't know what you need to log until you need it in the log, and by then it's late.

If you statically enforce attaching a log message, as well as log the input given, to
every kleisli you evaluate in your primary runtime effect, every execution of your
code can record the equivalent of a step-through debug session in machine and human
readable format, and life will be beautiful.

For example this code

```haskell
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
```

produces this output

```shell
Hello World
{
    "trace": [
        {
            "aux": {},
            "input": "Hello",
            "msg": "Construct the string 'Hello'",
            "level": "Info"
        },
        {
            "aux": {},
            "input": "World",
            "msg": "Construct the string 'World'",
            "level": "Warn"
        },
        {
            "aux": {},
            "input": "Hello World",
            "msg": "Concat them and print them out",
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
            "msg": "Do a step that will fail badly",
            "level": "Info"
        }
    ],
    "msg": "Do a sample program",
    "level": "Warn"
}
Big bada boom
CallStack (from HasCallStack):
  error, called at app/Main.hs:14:66 in main:Main
```

Say goodbye to forensic investigation, your system is now an omnipresent informant with
unbeatable note-taking skills
