# [Fluokitten](http://fluokitten.uncomplicate.org) - notable changes between versions

## 0.4.0-adavydow
### Briefly: 

Functors now supports only non-variadic fmap. 
Applicatives are Functors now with the following laws:

1. `(fmap f x) = (fapply (pure f) x`
2. `(fmap apply x y z) = (fapply x y z)`
3. `(fmap f x y) = (fapply (fmap (curry f) x) y)`

### Regressions:

* No default implementations for protocols.
    - Why: it is to easy to make an error and try to
	fmap over collection which do not implement Functor
	protocol with unexpected result.
    - How to solve: Identity functor can be implemented
	with the same behavior unwrapped Objects had.
* Variadic fmap now requires Applicative instance.
    - Why: variadic fmap defines applicative functor
	structure on objects, functor structure is not
	enough to implement variadic fmap.
    - How to solve: If your type defined variadic fmap,
	than it has an applicative functor structure. Provide
	Applicative instance for it.
* No Applicative instance for maps and map-entries.
    - Why: it was defined in the following way:
        `(fmap + {:a 1} {:a 2 :c 4} {:b 3 :c 5}) = {:a 3 :c
	9 : b 3})`. While it is convenient to use this
	function it could not be extended to applicative
	functor structure. For example: what is the result
	of `(fmap subs {:a "1234" :b "abcd"} {:a 1 :c 3})`
	- How to solve: TotalMap functor can be implemented,
	or another realization of applicative for maps could
	be provided with result map containing only keys
	which were present in all maps.
* No Applicative instance for CollReduce.
    - Why: it is not an Applicative functor.
* No Functor instance for types supporting Applicative
  protocol.
    - Why: it is not needed now, as any Applicative can
	now act as a Functor.
    - How to solve: they can be added in the future for
	performance reason. However the following law must
	hold: `(fmaf f x) = (fapply (pure f) x)`

### Semantic changes:
* Variadic fmap and fapply now follows the following laws:
  `(fmap f x y) = (fapply (fmap (curry f) x) y)`.
  `(fapply f x y) = (fapply (fapply f x) y)`.
  Several types now implements fmap in the different way
  compared to previous version:
    - Sequential collections
        + Before: `(fmap + [1 2] [3 4]) = [4 6]`
        + Now: `(fmap + [1 2] [3 4]) = [4 5 5 6]`
	  (nondeterministic computation pattern).
    - Functions
        + Before: `((fmap inc ((curry *) 3) ((curry +) 2))
	  7 3 1) = 40`
        + Now: type error
    - Non heterogenic fmaps
        + Before: `(fmap + '(1) [2]) = '(3)`
        + Now: `(fmap + '(1) [2]) = [3]` All type and
          methadata information in heterogeneous fmaps is
	  now take from the last parameter.

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
