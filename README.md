conditions
=====

Conditions for Haskell.

In this library, a condition does not uwnind the call stack, instead
it makes a non-local transfer of control to the condition handler
which can permit the handler to continue the computation with
different conditions, retry, signal a new condition, or abort with an
exception.

A condition must be an instance of the `Condition` class:

``` haskell
class (Exception c) => Condition c r | c -> r
```

The `r` states the return value of the code signalling the
condition. Signalling is done with the `signal` function:

``` haskell
signal :: (Handlers,Condition c r) => c -> r
```

And handling is done using `handler`:

``` haskell
handler :: (Handlers,Condition c r)
        => (Handlers => c -> r) -- ^ Condition handler.
        -> (Handlers => a)      -- ^ Scope of the condition handler.
        -> a
```

In the absence of any handlers, a signal is thrown as an exception.

## Examples

An IO example condition:

``` haskell
data OpenFileCondition = OpenFileCondition FilePath IOMode
  deriving (Typeable,Show)
instance Exception OpenFileCondition
instance Condition OpenFileCondition (IO Handle)
```

Later you can implement an `openFile` function which signals a
condition, for example:

``` haskell
openFile :: Handlers => FilePath -> IOMode -> IO Handle
openFile fp mode =
  do result <- try (IO.openFile fp mode)
     case result of
       Left (_ :: IOException) -> signal (OpenFileCondition fp mode)
       Right h -> return h
```

Now you can handle signals with `handler`, for example:

``` haskell
example :: IO Handle
example =
  withConditions
    (handler (\(OpenFileCondition _ mode) ->
                do putStrLn "Oh noes. How about a different file:"
                   fp <- getLine
                   openFile fp mode)
             (handler (\c@(OpenFileCondition fp mode) ->
                         do putStrLn "Oops, problem opening file. Retry once more?"
                            y <- getLine
                            if y == "y"
                               then openFile fp mode
                               else signal c)
                      (openFile "/foo/bar" ReadMode)))
```
