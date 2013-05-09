(ns org.uncomplicate.redcat.types
  (:use [org.uncomplicate.redcat.protocols]))

(comment
  (deftype Identity [v]
    Functor
    (fmap [fv g] (pure fv (g v)))
    Applicative
    (pure [_ v] (Identity. v))
    (fapply [fg fv] (pure fg (v (deref fv))))
    Monad
    (bind [mv g] (g (.v mv)))
    (join [mmv] v)))

(deftype Just [v]
  clojure.lang.IDeref
  (deref [j] (.v j))
  Functor
  (fmap [jv g]
    (Just. (g v)))
  (fmap [jv g jvs]
    (Just. (apply g v (map deref jvs))))
  Applicative
  (pure [_ v]
    (Just. v))
  (fapply [jv jg]
    (fmap nil nil))
  Monad
  (bind [jv g] (if jv (g (.v jv)) nil))
  (join [mmv] v)  )

(defn just [v] (Just. v))
