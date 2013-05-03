(ns org.uncomplicate.redcat.protocols)

(defprotocol Curried
  (original [f])
  (arity [f]))

(defprotocol Functor
  "1. (= (fmap identity) identity) ,
      that is (= (fmap identity x) (identity x))
   2. (= (fmap (comp f g)) (fmap f (fmap g)))
      or, when applied to a concrete functor
      (= (fmap (comp f g) x) (fmap f (fmap g x)))
  "

  (fmap [fv g] [fv g fvs] "TODO documentation"))

(defprotocol Applicative
  "1. (= (fapply (pure x f) x) (fmap f x))
   2. identity law: (= (fapply (pure x identity) x) x))
   3. composition law:
      (= (fapply (fapply (fapply (pure x (curry comp)) u) v) x)
         (fapply u (fapply v x)))
   4. homomorphism law: (= (fapply (pure a f) (pure a x)) (f x))
   5. interchange law: (= (fapply u (pure a y))
                          (fapply (pure a #(% y)) u))
  "
  (pure [av v] "TODO documentation")
  (fapply [av ag] [av ag avs] "TODO documentaion"))

(defprotocol Monad
  "TODO documentation"
  (bind [mv g] [mv g mvs])
  (join [mv]))

; Algebraic structures
(defprotocol Magma
  (op [x y] [x y ys]))

(defprotocol Semigroup)

(defprotocol Monoid
  (id [m])
  (monoidf [m]))

(defprotocol Foldable
  (fold [tm])
  (foldmap [ta f]))
