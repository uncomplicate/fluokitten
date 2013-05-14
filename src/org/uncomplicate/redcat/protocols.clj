(ns org.uncomplicate.redcat.protocols)

(defprotocol Curried
  (arity [f]))

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

   Redcat's test library contains macros that generate
   tests for functor laws.

   The fmap method is not intended to be used directly by
   the caller, although you should use it directly from
   the implementation of this protocol if needed from
   other methods.
  "
  (fmap [fv g] [fv g fvs]
    "Applies function g to the value(s) inside the context
     of the functor fv. The result is a functor of the same
     type as fv.
     This method is intended to be used by redcat core's
     fmap, not directly by clients. The first two arguments
     are reversed compared to core's fmap because protocol's
     polymorphism is based on java-based dispatch. The third
     parameter, fvs, contains a sequence of all additional
     arguments, normally supplied by core fmap's varargs
     (protocol methods do not support varargs)."))

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

   Redcat's test library contains macros that generate
   tests for applicative functor laws.

   The pure and fapply methods are not intended to be used
   directly by the caller, although you should use them directly
   from the implementation of this protocol if needed from
   other methods.
  "
  (pure [av v]
    "Takes any context av and any value a, and puts
     the value a in the same type of context. a should be put
     in the most minimal context possible that has appropriate
     type. av is needed only for proper dispatching and is
     not changed in any way.")
  (fapply [ag av] [ag av avs]
    "Applies the function(s) inside ag's context to the value(s)
     inside av's context while preserving the context. Both contexts
     should be of the same (or compatible) type, and the type of
     the resulting context is determined by av's type.
     This method is intended to be used by redcat core's
     fapply, not directly by clients. The third argument, avs,
     contains a sequence of all additional arguments, normally
     supplied by core fapply's varargs (protocol methods do not
     support varargs)."))

(defprotocol Monad
  "Monad is an abstraction for a context (box, container,
   computation) along with the ability to apply a function
   that accepts the value without the context and produces
   the result in a context. Every Monad should also implement
   Applicative and Functor protocols, although this can not be
   automatically forced by Clojure compiler.

   You create a new monad type by extending the Monad protocol
   and implementing bind and join methods, while observing
   monad laws:

   1. Left Identity Law: (bind (pure m x) f) => (g x)

   2. Right Identity Law: (bind m (pure m)) => m

   3. Associativity Law:
      (bind (bind m f) g) => (bind m (fn [x] (bind (f x) g)

   Redcat's test library contains macros that generate
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
     normal values.
     This method is intended to be used by redcat core's
     bind, not directly by clients. The third argument, mvs,
     contains a sequence of all additional arguments, normally
     supplied by core bind's varargs (protocol methods do not
     support varargs).
    ")
  (join [mv]
    "Flattens multiple monads nested in monadic into a single
     flat monad that contains ordinary, non-monadic value."))

(defprotocol Magma
  (op [x y] [x y ys]))

(defprotocol Semigroup)

(defprotocol Monoid
  (id [m]))

(defprotocol Foldable
  (fold [tm])
  (foldmap [ta f]))
