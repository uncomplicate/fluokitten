# [Fluokitten](http://fluokitten.uncomplicate.org) - notable changes between versions

## 0.3.0

New features:

* mdo macro as a syntactic sugar for chained bind calls.
* Implicit context (monad, functor, applicative) supported inside the dynamic scope of bind and all functions/macros that depend on it (>>=, mdo etc.)
* return (also called unit) function, a version of pure that uses the implicit context.
* with-context macro enables setting the implicit context for arbitrary body of expressions.
* >=> and <=< functions that compose monadic functions.

Changes:

* Just implementation of monoid interface now treats nil as an id element.
* changed the bind and fapply methods of curried functions.

## 0.2.0

Changes:

* Changed the order of arguments in uncomplicate.fluokitten.core/bind. Now the function is the last argument, previously it was the second. The protocols/bind function remains unchanged.
* join implementation for persistent collections and join no longer flattens the collection completely but only one level deep.
