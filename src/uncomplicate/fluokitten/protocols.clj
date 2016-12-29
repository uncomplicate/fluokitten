;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Defines Fluokitten protocols for various categorical concepts.
You need to use or require this namespace and provide your implementations
of these protocols if you want to extend Fluokitten with specific instances
of categorical concepts beyond those from Clojure core that Fluokitten
itself extends to be categorical. To use your own implementations, you need
to use or require the namespace where you define the implementations
from the calling code's namespace, as well as to use or require Fluokitten
core. Your implementations are normally not called directly from the client
code. The client code should call the generic functions from Fluokitten core."
      :author "Dragan Djuric"}
    uncomplicate.fluokitten.protocols)

(defprotocol Curry
  (arity [f])
  (curry [f] [f arity])
  (uncurry [f]))

(defprotocol Functor
  "Functor is an abstraction for a context (box, container,
   computation) along with the abiliity to apply a function
   to all the things inside that context. A typical example
   is a clojure sequence, wich is a container of elements,
   with the ability to apply a function to each of the elements,
   wich produces a new sequence of elements transformed by
   that function.

   You create a new functor type by extending the Functor
   protocol and implementing fmap method, while observing
   functor laws:

   1. (fmap identity) => identity ,
      that is (fmap identity x) => (identity x)

   2. (fmap (comp f g)) => (fmap f (fmap g))
      or, when applied to a concrete functor
      (fmap (comp f g) x) => (fmap f (fmap g x))

   (please note that core's fmap that has a different
   order of arguments has been used in these examples,
   as it would be used by clients)

   Fluokitten's test library contains macros that generate
   tests for functor laws.

   The fmap method is not intended to be used directly by
   the caller, although you should use it directly from
   the implementation of this protocol if needed from
   other methods.
  "
  (fmap [fv g] [fv g fvs]
    "Applies function g to the value(s) inside the context
     of the functor fv. The result is a functor of the same
     type as fv. If more functor values are supplied in a
     sequence fvs, uses them as arguments for a vararg g.
     This method is intended to be used by fluokitten core's
     fmap, not directly by clients. The first two arguments
     are reversed compared to core's fmap because protocol's
     polymorphism is based on java-based dispatch. The third
     parameter, fvs, contains a sequence of all additional
     arguments, normally supplied by core fmap's varargs
     (protocol methods do not support varargs)."))

(defprotocol PseudoFunctor
  "A variant of Functor that may be changed during the fmap!.
  In a puritan sense, it is not a functor, but it behaves in
  the analog way as proper functors. Typical use case is
  fmapping a primitive array and overwriting each element with
  the result of f."
  (fmap! [x f] [x f y] [x f y z] [x f y z v] [x f y z v ws]
    "Impure version of Functor's fmap."))

(defprotocol Applicative
  "Applicative (applicative functor) is an abstraction for a
   context (box, container, computation) along with the abiliity
   to apply function(s) contained in the same type of context to
   all the things inside that context. Every Applicative should
   also implement Functor, although it can not be automatically
   forced by Clojure protocols.

   A typical example is a clojure sequence, wich is a container
   of elements, with the ability to apply all the functions
   contained in another sequence to each of the elements,
   wich produces a new sequence of elements transformed by
   all the functions.

   You create a new applicative functor type by extending
   the Applicative protocol and implementing pure and fapply
   methods, while observing applicative functor laws:

   1. (fapply (pure x f) x) => (fmap f x)

   2. Identity Law: (fapply (pure x identity) x) => x

   3. Composition Law:
      (fapply (fapply (fapply (pure x (curry comp)) u) v) x)
      => (fapply u (fapply v x))

   4. Homomorphism Law: (fapply (pure a f) (pure a x)) => (f x)

   5. Interchange Law:
      (fapply u (pure a y)) => (fapply (pure a #(% y)) u)

   Fluokitten's test library contains macros that generate
   tests for applicative functor laws.

   The pure and fapply methods are not intended to be used
   directly by the caller, although you should use them directly
   from the implementation of this protocol if needed from
   other methods.
  "
  (pure [av v] [av v vs]
    "Takes any context av and any value a, and puts
     the value a in the same type of context. a should be put
     in the most minimal context possible that has appropriate
     type. av is needed only for proper dispatching and is
     not changed in any way.")
  (fapply [av ag] [av ag avs]
    "Applies the function(s) inside ag's context to the value(s)
     inside av's context while preserving the context. Both contexts
     should be of the same (or compatible) type, and the type of
     the resulting context is determined by av's type.
     If more applicative functor values are supplied in a
     sequence avs, uses them as arguments for vararg
     function(s) inside the context ag.
     This method is intended to be used by fluokitten core's
     fapply, not directly by clients. The third argument, avs,
     contains a sequence of all additional arguments, normally
     supplied by core fapply's varargs (protocol methods do not
     support varargs)."))

(defprotocol PseudoApplicative
  "A variant of Applicative that may be changed during the fapply!.
  In a puritan sense, it is not an applicative functor, but it behaves in
  an analog way. Typical use case is fapply-ing a Java array and overwriting
  each element with the result of g."
  (fapply! [av ag] [av ag avs]
    "An impure version of fapply."))

(defprotocol Monad
  "Monad is an abstraction for a context (box, container,
   computation) along with the ability to apply a function
   that accepts the value without the context and produces
   the result in a context. The resulting context may be
   different than the starting context. While the main idea
   with functors and applicatives is modifying the values
   inside the context, monad is more oriented towards modifying
   the context.  Every Monad should also implement
   Applicative and Functor protocols, although this can not be
   automatically forced by Clojure compiler.

   You create a new monad type by extending the Monad protocol
   and implementing bind and join methods, while observing
   monad laws:

   1. Left Identity Law: (bind (pure m x) f) => (g x)

   2. Right Identity Law: (bind m (pure m)) => m

   3. Associativity Law:
      (bind (bind m f) g) => (bind m (fn [x] (bind (f x) g)

   Fluokitten's test library contains macros that generate
   tests for monad laws.

   The bind and join methods are not intended to be used
   directly by the caller, although you should use them directly
   from the implementation of this protocol if needed from
   other methods.
  "
  (bind [mv g] [mv g mvs]
    "Applies the function g to the value(s) inside mv's context.
     Function g produces the result inside with the context,
     in contrast to fmap where function g is expect to produce
     normal values. If more monadic values are supplied in a
     sequence mvs, uses them as arguments for a vararg g.
     This method is intended to be used by fluokitten core's
     bind, not directly by clients. The third argument, mvs,
     contains a sequence of all additional arguments, normally
     supplied by core bind's varargs (protocol methods do not
     support varargs).
    ")
  (join [mv]
    "Flattens multiple monads nested in monadic into a single
     flat monad that contains ordinary, non-monadic value."))

(defprotocol PseudoMonad
  "A dirty heretic cousin of Monad that may be changed during the bind! or join!
  In a puritan sense, it is not a monad , but it behaves in an analog way.
  Typical use case is binding a Java array and overwriting each
  element with the result of g."
  (bind! [mv g] [mv g mvs]
    "Impure variant of bind.")
  (join! [mv]
    "Impure variant of join."))

(defprotocol Magma
  "Magma is an abstraction of elements that have an operation op,
   which combines its arguments into an element of the same
   type (op is closed on the set of all objects that have that type).
   If the operation op is also associative, then that magma
   is also a semigroup.

   You create a new magma type by extending the Magma protocol
   and implementing op method while observing the following laws:

   1. op is closed: (instance? (type x) (op x y)) => true

   2. associativity (only for semigroups):
      (op (op a b)) => (op a (op b c))

   Fluokitten's test library contains macros that generate
   tests for checking whether op is closed and/or associative.

   The op method is not intended to be used
   directly by the caller, although you should use it directly
   from the implementation of this protocol if needed from
   other methods.

   If called with only one argument, returns a convenient
   op function, that, when called with no arguments, returns
   the id element.
  "
  (op [x] [x y] [x y z] [x y z w] [x y z w ws]
    "Operation that combines elements x and y (and optionally z, w)
     into an element of the same type. If more elements are supplied in
     a sequence ws, combines them all."))

(defprotocol Monoid
  "Monoid is an abstraction of elements that are magmas whose
   op has an identity element. (op x (id x)) => x. Every Monoid
   should also implement Magma protocols, although this can not be
   automatically forced by Clojure compiler.

   You create a new monoid type by extending the Monoid protocol
   and implementing id method, while observing monoid law:

   1. identity element for op exists:
      (op x (id x)) => x
      (op (id x) x) => x

   Fluokitten's test library contains macros that generate
   monoid tests.

   The id method is not intended to be used
   directly by the caller, although you should use it directly
   from the implementation of this protocol if needed from
   other methods.
  "
  (id [m]))

(defprotocol Foldable
  "Foldable is an abstraction for a context (box, container,
   computation) along with the ability to extract the summary
   value of its contents. Foldable implementations  do not
   have to implement other categorical protocols, although
   it is convenient to view fold as an opposite of the function
   pure: pure puts values in minimal context, while fold
   gets the value outside of the context. With some Foldables,
   (such as Atom) context contains a single value that can be
   accessed by fold, while some (such as PersistentColection)
   contain many values, so they can only extract it as some
   summary value.

   You create a new foldable type by extending the Foldable
   protocol and implementing fold and foldmap methods.

   The fold and foldmap methods are not intended to be used
   directly by the caller, although you should use them directly
   from the implementation of this protocol if needed from
   other methods.
  "
  (fold [x] [x f init] [x f init y] [x f init y z]
    [x f init y z w] [x f init y z w ws]
    "Extracts the value(s) from the context and returns it
     as one single value. Contexts that contain multiple values
     typically require that values are Monoids and use op
     to combine them.")
  (foldmap [x g] [x g f init] [x g f init y] [x g f init y z]
    [x g f init y z w] [x g f init y z w ws]
    "Similar to fold, but before returning the sole value
     from the context or combining multiple values into
     a summary, applies the function g to transform it
     (to a Monoid if needed)."))

(defprotocol Maybe
  (value [m]))
