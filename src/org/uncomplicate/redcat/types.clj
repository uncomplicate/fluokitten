(ns org.uncomplicate.redcat.types
  (:use [org.uncomplicate.redcat.protocols]))

(deftype Identity [v]
  clojure.lang.IDeref
    (deref [_] v)
  Functor
    (fmap [fv g] (pure fv (g v)))
  Applicative
    (pure [_ v] (Identity. v))
    (<*> [fg fv] (pure fg (v (deref fv))))
  Monad
    (bind [mv g] (g (.v mv)))
    (join [mmv] v))

(deftype Maybe [v]
  clojure.lang.IDeref
    (deref [_] v)
  Functor
    (fmap [fv g] (pure fv (g v)))
  Applicative
    (pure [_ v] (Maybe. v))
    (<*> [fg fv] (pure fg (v (deref fv))))
  Monad
    (bind [jv g] (if jv (g (.v jv)) nil))
    (join [mmv] v))
