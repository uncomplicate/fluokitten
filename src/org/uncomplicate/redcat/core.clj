(ns org.uncomplicate.redcat.core
  (:require [org.uncomplicate.redcat.protocols :as p]))

(defn fmap
  "Applies a function f to the value inside functor's context
   while preserving the context.

  Returns a functor instance consisting of the result of applying f
  to the value(s) inside the functor's context. If called with only
  one argument, lifts function f so it can be applied to functor,
  i.e creates a new function that can reach inside the functor's
  context and return the result of applying the original function f.

  fmap can be thought of in two ways:
  1. As a function that takes a function f and functor and then maps
     the function over functor value.
  2. As a function that takes a function f so it can operate on functor
     values instead on plain values.

  Function f should work on plain values without regard to the functor.
  Functor must be an extension of Functor protocol.

  Some common Clojure Functors that come built-in:
  - all IPersistentCollection collections
  - all functions
  - nil
  - atoms, refs
  - strings
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

  ((fmap str inc) 1)
  => \"2\"

  In this example, inc is a context for its arguments. fmapping str
  function over inc functor (which is also a function), we get another
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

;TODO COMMENT

(defn pure
  ([av]
     #(p/pure av %))
  ([av v]
     (p/pure av v)))

(defn fapply
  ([af]
     (fn
       ([av & avs]
          (apply fapply af av avs))))
  ([af av]
     (p/fapply av af))
  ([af av & avs]
     (p/fapply av af avs)))

(def <*> fapply)

(def liftm fmap)

(defn sequencem [smv]
  (p/pure (first smv) (fmap deref smv)))

(defn replicatem [n ma]
  (sequence (repeat n ma)))

(defn join [x] (p/join x))

(defn bind
  ([f]
     (fn
       ([m & ms]
          (apply bind m f ms))))
  ([m f]
     (p/bind m f))
  ([m f & ms]
     (p/bind m f ms)))

(def >>= bind)

(defn =<< [f mv & mvs]
  (apply bind mv f mvs))

(defn mapm [m f s] (sequence (map f s)))

(defn foldmap [f ta] (p/foldmap ta f))

(def fold p/fold)
