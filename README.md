conditions
=====

Conditions for Haskell.

In this library, a condition does not unwind the call stack, instead
it makes a non-local transfer of control to the condition handler
which can permit the handler to continue the computation with
different conditions, retry, signal a new condition, or abort with an
exception.

[Haddocks](http://chrisdone.com/conditions/Control-Condition.html)

See the `examples/` directory for examples.

### Explanation

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
