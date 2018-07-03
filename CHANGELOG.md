# [Fluokitten](http://fluokitten.uncomplicate.org) - notable changes between versions

## 0.7.0

* Support volatile.
* Support Java arrays.

## 0.6.1

* Clojure dep updated to 1.9.0

## 0.6.0

Changes:
* protocols/op now returns the monoid operation instead of evaluating it.

## 0.5.2

* Fix #20

## 0.5.1

* Fixed apply op bug for vectors (in algo/reducible-op)

## 0.5.0

* Performance improvement of op for Double and Float

## 0.4.0

New features:

* Added PseudoFunctor, PseudoApplicative, and PseudoMonad, to support destructive operations in Neanderthal.
* Better support for functions and curried functions.
* fold, foldmap, and op much improved with variadic versions.
* Varargs versions of pure, return, and unit.

Changes:

* fmap implementation for function changed to be in line with bind; supports multi-arity functions and offer super-comp.
* Collections use reducers where appropriate.
* op, fold, foldmap, support multiple arguments, have better implementations.

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
