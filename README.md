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
        },
        {
            "aux": {
                "exceptionLines": [
                    "Big bada boom",
                    "CallStack (from HasCallStack):",
                    "  error, called at app/Main.hs:14:66 in main:Main"
                ]
            },
            "input": {},
            "msg": "Do a step that will fail badly: Exception",
            "level": "Err"
        }
    ],
    "msg": "Do a sample program",
    "level": "Err"
}
Big bada boom
CallStack (from HasCallStack):
  error, called at app/Main.hs:14:66 in main:Main
```

Say goodbye to forensic investigation, your system is now an omnipresent informant with
unbeatable note-taking skills

## Existing Algebras

Existing algebras can be trivially lifted to provide the same logging semantics on
interpretation as programs written entirely in the Safely algebra, simply lift
your algebra into `Compose Logged`.

If you migrate all constructors effectively eliminating the unlifted algebra, 
use the `runLogged` combinator to lift the unlifted interpreter to operate on the lifted algebra.

If you maintain the original constructors alongside the lifted constructors (expected when
proliferation of constructor call-sites makes modification of constructor signatures expensive,
lifting algebras without modifying signatures is not recommended unless a log message is already
part of a constructor signature), use the `runWithLogged` combinator to lift the unlifted
interpreter to operate on the lifted algebra _and_ unlifted algebra. Unlifted interpretation
will attach notes to the log to make known when and where unlogged calls are still being
triggered.

For example, expanding the following algebra:

```haskell
data Math a where
  Mul :: Int -> Int -> Math Int
  Add :: Int -> Int -> Math Int

mul :: (Member Math effs) => Int -> Int -> Eff effs Int
mul x y = send $ Mul x y

add :: (Member Math effs) => Int -> Int -> Eff effs Int
add x y = send $ Add x y

liftMath :: Math a -> Eff effs a
liftMath (Mul x y) = return $ x * y
liftMath (Add x y) = return $ x + y

runMath :: Eff (Math ': effs) a -> Eff effs a
runMath = handleRelay return ((>>=) . liftMath)
```

with the following constructors

```haskell
mulLogged :: (Member (Compose Logged Math) effs) => String -> Int -> Int -> Eff effs Int
mulLogged msg x y = loggedInfo msg (Mul x y) Nothing

addLogged :: (Member (Compose Logged Math) effs) => String -> Int -> Int -> Eff effs Int
addLogged msg x y = loggedInfo msg (Add x y) Nothing
```

allows the following program

```haskell
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
```

to produce the following output

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
            "input": {},
            "msg": "Unlogged Math call",
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
            "input": {
                "plus": [
                    5,
                    2
                ]
            },
            "msg": "Add 2 more",
            "level": "Info"
        },
        {
            "aux": {},
            "input": "Big bada boom",
            "msg": "Do a step that will fail badly",
            "level": "Info"
        },
        {
            "aux": {
                "exceptionLines": [
                    "Big bada boom",
                    "CallStack (from HasCallStack):",
                    "  error, called at app/Main.hs:18:66 in main:Main"
                ]
            },
            "input": {},
            "msg": "Do a step that will fail badly: Exception",
            "level": "Err"
        }
    ],
    "msg": "Do a sample program",
    "level": "Err"
}
Big bada boom
CallStack (from HasCallStack):
  error, called at app/Main.hs:18:66 in main:Main
```
