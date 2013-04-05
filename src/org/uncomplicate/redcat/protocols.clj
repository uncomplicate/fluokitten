(ns org.uncomplicate.redcat.protocols)

(defprotocol Functor
  "TODO documentation"
  (fmap [fv g] [fv g fvs] "TODO documentation"))

(defprotocol Applicative
  (pure [av v] "TODO documentation")
  (fapply [av ag] [av ag avs] "TODO documentaion"))
  ;perhaps we should always check that all parameters are of f typer and
  ;if they are not, call wrap to wrap it. Then we do not have to explicitly wrap the parameters

(defprotocol Monad
  "TODO documentation"
  (bind [mv g])
  (join [mv]))

; Algebraic structures
(defprotocol Semigroup
  (op [x y]))

(defprotocol Monoid
  (id [x]))

(defprotocol Foldable
  (fold [tm])
  (foldmap [ta f]))
