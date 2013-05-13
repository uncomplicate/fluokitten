(ns org.uncomplicate.redcat.types
  (:use [org.uncomplicate.redcat.protocols]))

(deftype Just [v]
  clojure.lang.IDeref
  (deref [_] v)
  Object
  (hashCode [_]
    (.hashCode v))
  (equals [this that]
    (or (identical? this that)
        (and (instance? Just that)
             (= v @that))))
  Functor
  (fmap [_ g]
    (Just. (g v)))
  (fmap [_ g jvs]
    (if (some nil? jvs)
      nil
      (Just. (apply g v (map deref jvs)))))
  Applicative
  (pure [_ x]
    (Just. x))
  (fapply [_ jv]
    (fmap jv v))
  (fapply [_ jv jvs]
    (fmap jv v jvs))
  Monad
  (bind [_ g]
    (g v))
  (bind [_ g jvs]
    (if (some nil? jvs)
      nil
      (apply g v (map deref jvs))))
  (join [jjv]
    (if (instance? Just v) (join v) jjv)))

(def just ->Just)
