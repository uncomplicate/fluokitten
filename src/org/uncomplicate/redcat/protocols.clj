(ns org.uncomplicate.redcat.protocols)

(defprotocol Functor
  "TODO documentation"
  (fmap [fv g] [fv g fvs] "TODO documentation"))

(defprotocol Applicative
  (pure [av v] "TODO documentation")
  (fapply [av ag] [av ag avs] "TODO documentaion"))

(defprotocol Monad
  "TODO documentation"
  (bind [mv g] [mv g mvs])
  (join [mv]))

; Algebraic structures
(defprotocol Semigroup
  (op [x y]))

(defprotocol Monoid
  (id [x]))

(defprotocol Foldable
  (fold [tm])
  (foldmap [ta f]))
