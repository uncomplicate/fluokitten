(ns uncomplicate.fluokitten.core
  (:require [uncomplicate.fluokitten.protocols :as p])
  (:require [uncomplicate.fluokitten.algo :as algo]))

(defn fmap
  "Applies a function f to the value(s) inside functor's context
   while preserving the context. More about Functors can be found
   in the doc for uncomplicate.fluokitten.protocols/Functor.

   Returns a functor instance consisting of the result of applying f
   to the value(s) inside the functor's context. Vararg version applies
   f to value(s) provided by the functors' contexts. If called with only
   one argument, lifts function f so it can be applied to functor,
   i.e creates a new function that can reach inside the functor's
   context and return the result of applying the original function f.

   fmap can be thought of in two ways:
   1. As a function that takes a function f and functor and then maps
      the function over the functor value. In this sense, it is similar
      to ubiquitous map function, with the difference that fmap works
      on any Functor instance and it preserves the context type (while
      map always converts its source to a sequence).
   2. As a function that takes a function f so it can operate on functor
      values instead on plain values.

   Function f should work on plain values without regard to the functor.
   Functor must be an extension of Functor protocol and MUST satisfy
   the functor laws
   (see the doc for uncomplicate.fluokitten.protocols/Functor)

   Some common Clojure constructs that are Functors:
   - persistent collections
   - reducibles
   - functions
   - nil
   - atoms, refs
   - strings
   - keywords
   - all Objects. (fmap f o) equals (f o) if nothing more specific has
     been defined for object's type

   ---- Example 1: Clojure collections are functors

   (fmap str [1 2 3])
   => [\"1\" \"2\" \"3\"]

   Since clojure vector is a functor, it represents a context for its
   elements. Function inc works on the elements of the vector, and
   does not know anything about the vector itself. The result is a
   vector of transformed elements.

   ---- Example 2: Clojure functions are functors

   ((fmap str +) 1 2 3)
   => \"6\"

   In this example, + is a context for its arguments. fmapping str
   function over + functor (which is also a function), we get another
   function that applies string to an argument, but with the context of
   incrementing preserved.

   ---- Example 3: lifting a function
   ((fmap str) [1 2 3])
   => [\"1\" \"2\" \"3\"]
  "
  ([f functor]
    (p/fmap functor f))
  ([f]
     (if (= identity f)
       identity
       (fn
         ([functor & functors]
            (apply fmap f functor functors)))))
  ([f functor & functors]
    (p/fmap functor f functors)))

(defn pure
  "Takes any value x and wraps it in a minimal, default, context
   of the same type as the context of the applicative value.

   If called with only one argument, creates a pure function that
   can wrap any value into a specific default context determined
   by applicative.

   The behavior is largely constrained by the laws that should be
   satisfied by any applicative functor in regard to the behavior
   or fapply and pure (see the doc for
   uncomplicate.fluokitten.protocols/Applicative)

   ---- Example 1: putting a number in a pure vector context
   (pure [] 1)
   => [1]

   ---- Example 2: a number in a pure curried function context:
   ((pure curried 1) 17)
   => 1
  "
  ([applicative]
     #(p/pure applicative %))
  ([applicative x]
     (p/pure applicative x)))

(defn fapply
  "Applies the function(s) inside af's context to the value(s)
   inside av's context while preserving the context. Both contexts
   should be of the same (or compatible) type, and the type of
   the resulting context is determined by av's type.
   af and af stand for \"applicative functor function\"
   and \"applicative functor value\".

   Returns an applicative functor instance consisting of
   the result of applying the function(s) inside af's context
   to the value(s) inside the av's context. Vararg version
   applies the function(s) inside af to the value(s)
   provided by the avs' contexts. If called with only one argument,
   lifts av so it can be applied to an applicative functor,
   i.e creates a new function that can reach inside
   the applicative functor's context and return the result
   of applying the original context af.

   fapply can be thought of as fmap that, instead of
   plain function f receives one or many functions inside
   the context af, extracts those functions from the context
   and applies them to the values inside the context av.

   The behavior is largely constrained by the laws that should be
   satisfied by any applicative functor in regard to the behavior
   or fapply and pure (see the doc for
   uncomplicate.fluokitten.protocols/Applicative)

   Some common Clojure constructs that are Applicative functors:
   - persistent collections
   - reducibles
   - nil
   - atoms, refs

   ---- Example 1: Clojure collections are applicative functors

   (fapply [inc dec] [1 2 3])
   => [2 3 4 0 1 2]

   ---- Example 2: atoms are applicative functors
   (fapply (atom inc) (atom 1))
   => 2
  "
  ([af]
     (fn [av & avs]
       (apply fapply af av avs)))
  ([af av]
     (p/fapply af av))
  ([af av & avs]
     (p/fapply af av avs)))

(defn <*>
  "Performs a Haskell-style left-associative fapply
   on its arguments. (<*> f g h) is equivalent to
   (fapply (fapply f g) h). It always uses a two-argument fapply.

   If only two arguments are supplied, it is equivalent to fapply.
   When called with one argument, creates a function
   that can accept the rest of the arguments and apply <*>.
  "
  ([af]
     (fn [a & as]
       (apply <*> af a as) ))
  ([af av]
     (p/fapply af av))
  ([af av & avs]
     (reduce p/fapply af (cons av avs))))

(defn join [monadic]
  "Flattens multiple monads nested in monadic into a single
   flat monad that contains ordinary, non-monadic value.

   ---- Example with collections:
   (join [[1 2] [3 4]])
   => [1 2 3 4]

   ---- Example with atoms:
   (join (atom (atom (atom 1))))
   => #<Atom: 1>
  "
  (p/join monadic))

(defn bind
  "Takes a value inside the context of a monad (monadic value)
   and a function f that take a normal value (without the
   context) and produces the result inside of a context
   of the monadic type.

   If called with more arguments, the additional arguments,
   monadics will be used as additional arguments to function
   f. When called with one argument only, produces a function
   with f fixed, so it just accepts monadic values and feeds
   them to f.

   bind can be also thought of as a function that combines
   two computations into one large computation.

   The behavior is largely constrained by the laws that should be
   satisfied by any monad in regard to the behavior
   or bind and pure (see the doc for
   uncomplicate.fluokitten.protocols/Monad)

   Some common Clojure constructs that are Monads:
   - persistent collections
   - reducibles
   - nil
   - atoms, refs

   ---- Example with collections:
   (bind [1 2 3] (comp list inc))
   => [2 3 4]

   ---- Example with atoms:
   (bind (atom 1) (comp atom inc))
   => #<Atom: 2>
  "
  ([f]
     (fn
       ([monadic & ms]
          (apply bind monadic f ms))))
  ([monadic f]
     (p/bind monadic f))
  ([monadic f & monadics]
     (p/bind monadic f monadics)))

(defn >>=
  "Performs a Haskell-style left-associative bind
   on its arguments. (>>= f g h) is equivalent to
   (bind (bind f g) h). It always uses a two-argument bind.

   If only two arguments are supplied, it is equivalent to bind.
   When called with one argument, creates a function
   that can accept the rest of the arguments and apply >>=.
  "
  ([monadic]
     (fn [f & fs]
       (apply >>= monadic f fs) ))
  ([monadic f]
     (p/bind monadic f))
  ([monadic f & fs]
     (reduce p/bind monadic (cons f fs))))

(defn fold [foldable]
  "Folds all the contents of a foldable context by either
   getting the single element or if there are more than one
   elements in the context by combining them all in the one
   aggregate value. How exactly these elements
   are combined depends on the actual context. Collections,
   that are often used as contexts, require that the elements
   are monoids, so they can be easily combined using op.

   Some common Clojure constructs that are Foldable:
   - persistent collections
   - reducibles
   - nil
   - atoms, refs
   - all objects are foldable in a sense that
     (fold o) => o if there is no specific implementation

   Some common Clojure constructs that are Monoids:
   - persistent collections
   - reducibles
   - functions
   - nil
   - strings
   - keywords
   - numbers

   ---- Example: vector is foldable, while a set of real
   numbers form a monoid with a binary operation + and identity
   element 0. Thus,
   (fold [1 2 3])
   => 6
  "
  (p/fold foldable))

(defn foldmap
  "Folds the contest of a foldable context by applying
   function f on each of its elements to produce a monoid
   and then uses the operation op of that monoid to combine
   the elements into a single aggregate value.
   See the doc for fold, the only difference is that foldmap
   can fold foldables that contain elements that are not
   monoids, by using function f to transform them to monoids
   before folding.

   If called with only one argument, f, creates a function
   that can fold a foldable that contains non-monoid values
   by first using f to convert them to monoids.
  "
  ([f]
     (fn [foldable]
       (p/foldmap foldable f)))
  ([f foldable]
     (p/foldmap foldable f)))

(defn op
  "Applies the monoid operation op determined by the type
   of monoids x and y. Since op is closed on that monoid,
   the result is also in the same monoid and can be further
   combined by op with other elemens of the same monoid.

   The vararg version is equivalent with (op (op x y) z),
   but the actual algorithm for a given monoid depends
   on the implementation. For example, (+ 1 2 3) is used
   for numbers instead of (+ (1 2) 3)

   ---- Example 1: numbers as monoids
   (op 1 2 3)
   => 6

   ---- Example 2: strings ar monoids
   (op \"some\" \"thing\")
   => \"something\"
  "
  ([x y]
     (p/op x y))
  ([x y & ys]
     (p/op x y ys)))

(defn id [x]
  "Returns the identity element of the monoid that x is
   an element of.

   ---- Example 1: numbers
   (id 3)
   => 0

   ---- Example 2: strings
   (id \"something\")
   => \"\"
  "
  (p/id x))

(def just
  "Creates the context of Maybe monad and puts
   the supplied value in it."
  algo/->Just)
