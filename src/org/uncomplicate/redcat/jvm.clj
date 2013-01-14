(ns org.uncomplicate.redcat.jvm
  (:use [org.uncomplicate.redcat.protocols])
  (:require [clojure.core.reducers :as r]))

(set! *warn-on-reflection* true)

(extend-type nil
  Functor
  (fmap
    ([_ _] nil)
    ([_ _ args] nil))
  Applicative
  (wrap [_ _] nil)
  (<*> 
    ([_ _] nil)
    ([_ _ args] nil))
  Monad
  (bind [_ _] nil)
  (join [_] nil)
  Semigroup
  (op [_ _] nil)
  Monoid
  (id [c] nil))

(extend-type Object
  Functor
  (fmap 
    ([o f]
      (f o))
    ([o f os] 
      (if (some nil? os) 
        nil 
        (apply f o os)))))

(defn op-fun 
  ([e op] (r/monoid op (constantly e)))
  ([x] (op-fun (id x) op)))

(defn monoidalf
  ([e] 
    (let [ide (id e)]
      (fn 
        ([] ide)
        ([e1 e2] (op e1 e2))))))

(extend-type clojure.lang.IPersistentCollection
  Functor
  (fmap
    ([c g] 
      (into (empty c) (r/map g c)))
    ([c g cs]
      (into (empty c) (apply map g c cs))))
  Applicative
  (pure [c v] 
    (conj (empty c) v))
  (<*> 
    ([cv sg] 
      (into (empty cv) (r/mapcat #(r/map % cv) sg)));(bind cg (partial fmap sv)))
    ([cv sg svs] 
      (into (empty cv) (r/mapcat #(apply map % cv svs) sg))))
  Monad
  (join [mc] 
    (into (empty mc) (r/flatten mc)))
  (bind [c g]
    (into (empty c) (r/mapcat g c))); (join (fmap c g)))
  Semigroup
  (op [c s] 
    (into c s))
  Monoid
  (id [c]
    (empty c))
  Foldable
  (fold [c] 
    (r/fold (monoidalf (first c)) c))
  (foldmap [c g] 
    (fold (fmap c g))))

(extend-type clojure.lang.LazySeq
  Functor
  (fmap 
    ([s g] (map g s))
    ([s g ss] (apply map g s ss)))
  Applicative
  (pure [_ v] 
    (let [p [v]] 
      (lazy-seq p)))
  (<*> 
    ([sv sg] 
      (mapcat #(map % sv) sg)) ;(bind sg (partial fmap sv))))
    ([sv sg svs]
      (mapcat #(apply map % sv svs) sg))))

(extend-type clojure.lang.MapEntry
  Functor
  (fmap 
    ([e g] (clojure.lang.MapEntry. (key e) (g (val e))))
    ([e g es] (clojure.lang.MapEntry. (key e) (apply g (val e) (vals es))))))
  
(extend-type String
  Functor
  (fmap 
    ([s g] 
      (apply str (g s)))
    ([s g ss] (apply str (apply g s ss))))
  Semigroup
  (op [s s1] 
    (str s s1))
  Monoid
  (id [s] (str)))

(extend-type Number
  Semigroup
  (op [n y] (+ n y))
  Monoid 
  (id [n] 0))

(extend-type clojure.lang.Fn
  Functor
  (fmap 
    ([f g] 
      (comp g f))
    ([f g gs] 
      (apply comp g f gs))))

(extend-type clojure.lang.Atom
  Functor
  (fmap 
    ([a g] (do (swap! a g) a))
    ([a g args] (do (apply swap! a g args) a)))
  Applicative
  (pure [a v] (atom v))
  (<*> [av ag] (fmap av (deref ag)))
  Monad
  (join [ma] (fmap ma deref))
  (bind [a g] (join (fmap a g))))

(extend-type clojure.lang.Ref
  Functor
  (fmap 
    ([r g] (do (alter r g) r))
    ([a g args] (do (apply alter a g args) a)))
  Applicative
  (pure [r v] (ref v))
  (<*> [rv rg] (fmap rv (deref rg)))
  Monad
  (join [ma] (fmap ma deref))
  (bind [a g] (join (fmap a g))))