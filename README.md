conditions
=====

Conditions for Haskell.

[Haddocks](http://chrisdone.com/conditions/Control-Condition.html)

In this library, a condition does not unwind the call stack, instead
it makes a non-local transfer of control to the condition handler
which can permit the handler to continue the computation with
different conditions, retry, signal a new condition, or abort with an
exception.

### Motivation

In languages with comprehensive condition systems like Common Lisp, it
is possible for the development environment to reify conditions into
something meaningful to allow the programmer to take some corrective
action without bailing out the whole program. Examples:

* In the case of failing to open a file during a long-running
  computation, the IDE can prompt the developer to provide another
  file name, retry, abort, etc.
* Parsing a CSV and encountering a line that does not parse can allow
  the developer to attempt to re-parse, change the parsing function on
  the fly to suite new discovered requirements or substitute a value
  to be used for that row, or ignore it.

See the `examples/` directory for code versions of these examples.

### Library

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
