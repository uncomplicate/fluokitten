(ns org.uncomplicate.redcat.jvm
  (:use [org.uncomplicate.redcat.protocols])
  (:require [clojure.core.reducers :as r]))

(set! *warn-on-reflection* true)

(extend-type nil
  Functor
  (fmap [_ _] nil)
  Applicative
  (wrap [_ _] nil)
  (<*> [_ _] nil)
  Monad
  (bind [_ _] nil)
  (join [_] nil))

(extend-type Object
  Functor
  (fmap [o f] (f o)))

(defn op-fun 
  ([e op] (r/monoid op (constantly e)))
  ([x] (op-fun (id x) op)))

(extend-type clojure.lang.IPersistentCollection
  Functor
  (fmap [c g] (into (empty c) (r/map g c)))
  Applicative
  (pure [c v] (conj (empty c) v))
  (<*> [cg sv] (bind cg (partial fmap sv)))
  Monad
  (join [mc] (r/reduce into (empty mc) mc))
  (bind [c g] (join (fmap c g)))
  Semigroup
  (op [c s] (into c s))
  Monoid
  (id [c] (empty c))
  Foldable
  (fold [c] (r/fold (op-fun (first c)) c))
  (foldmap [c g] (fold (fmap c g))))

(extend-type clojure.lang.LazySeq
  Functor
  (fmap [s g] (map g s))
  Applicative
  (pure [_ v] (lazy-seq [v]))
  (<*> [sg sv] (bind sg (partial fmap sv))))

(extend-type clojure.lang.MapEntry
  Functor
  (fmap [e g] (clojure.lang.MapEntry. (key e) (g (val e)))))
  
(extend-type String
  Functor
  (fmap [s g] (apply str (g s))))

(extend-type Number
  Semigroup
  (op [n y] (+ n y))
  Monoid 
  (id [n] 0))

(extend-type clojure.lang.Fn
  Functor
  (fmap [f g] (comp g f)))

(extend-type clojure.lang.Atom
  Functor
  (fmap [a g] (do (swap! a g) a))
  Applicative
  (pure [a v] (atom v))
  (<*> [ag av] (fmap av (deref ag)))
  Monad
  (join [ma] (fmap ma deref))
  (bind [a g] (join (fmap a g))))

(extend-type clojure.lang.Ref
  Functor
  (fmap [r g] (do (alter r g) r))
  Applicative
  (pure [r v] (ref v))
  (<*> [rg rv] (fmap rv (deref rg)))
  Monad
  (join [ma] (fmap ma deref))
  (bind [a g] (join (fmap a g))))