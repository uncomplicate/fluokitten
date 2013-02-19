(ns org.uncomplicate.redcat.jvm
  (:use org.uncomplicate.redcat.protocols)
  (:require [clojure.core.reducers :as r]))

(set! *warn-on-reflection* true)

(extend-type nil
  Functor
  (fmap
    ([_ _] nil)
    ([_ _ args] nil))
  Applicative
  (pure [_ _] nil)
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

;=============== Functor implementations =========================
;;--------------- fmap implementations ---------------
(defn reducible-fmap
  ([c g]
    (into (empty c) (r/map g c)))
  ([c g cs]
    (into (empty c) (apply map g c cs))))

(defn seq-fmap
  ([s g]
    (into (empty s) (into (list) (map g s))))
  ([s g ss]
    (into (empty s) (into (list) (apply map g s ss)))))

(defn lazy-fmap
  ([s g]
    (map g s))
  ([s g ss]
    (apply map g s ss)))

(defn coll-fmap
  ([c g]
    (into (empty c) (map g c)))
  ([c g ss]
    (into (empty c) (apply map g c ss))))

;;================ Applicative implementations ==================
;;-------------- <*> implementations ----------------
(defn reducible-<*>
  ([cv sg]
     (into (empty cv) (r/mapcat #(r/map % cv) sg)))
  ([cv sg svs]
     (into (empty cv) (r/mapcat #(apply map % cv svs) sg))))

(defn map-<*>
  ([mv mg]
     (into (empty mv) (r/mapcat #(r/map (val %) mv) mg)))
  ([mv mg mvs]
     (into (empty mv) (r/mapcat #(apply map (val %) mv mvs) mg))))

(defn seq-<*>
  ([cv sg]
     (into (empty cv) (into (list) (mapcat #(map % cv) sg))));(bind cg (partial fmap sv)))
  ([cv sg svs]
     (into (empty cv) (into (list) (mapcat #(apply map % cv svs) sg)))))

(defn lazy-<*>
  ([cv sg]
     (mapcat #(map % cv) sg))
  ([cv sg svs]
     (mapcat #(apply map % cv svs) sg)))

(defn coll-<*>
  ([cv sg]
     (into (empty cv) (mapcat #(map % cv) sg)));(bind cg (partial fmap sv)))
  ([cv sg svs]
     (into (empty cv) (mapcat #(apply map % cv svs) sg))))

(defn coll-pure [cv v]
  (conj (empty cv) v))

;;================== Collections Extensions =====================
(extend clojure.lang.IPersistentCollection
  Functor
  {:fmap coll-fmap}
  Applicative
  {:pure coll-pure
   :<*> coll-<*>})

(extend clojure.lang.APersistentVector
  Functor
  {:fmap reducible-fmap}
  Applicative
  {:pure coll-pure
   :<*> reducible-<*>})

(extend clojure.lang.ASeq
  Functor
  {:fmap seq-fmap}
  Applicative
  {:pure coll-pure
   :<*> seq-<*>})

(extend clojure.lang.LazySeq
  Functor
  {:fmap lazy-fmap}
  Applicative
  {:pure (fn [cv v] (lazy-seq (coll-pure cv v)))
   :<*> lazy-<*>})

(extend clojure.lang.APersistentSet
  Functor
  {:fmap reducible-fmap}
  Applicative
  {:pure coll-pure
   :<*> reducible-<*>})

(extend clojure.lang.APersistentMap
  Functor
  {:fmap reducible-fmap}
  Applicative
  {:pure (fn [m v] (assoc (empty m)
                    (if-let [[k _] (first m)]
                      (id k)
                      nil)
                    v))
   :<*> map-<*>})

(extend-type clojure.lang.MapEntry
  Functor
  (fmap
    ([[ke ve] g]
       (clojure.lang.MapEntry. ke (g ve)))
    ([[ke ve] g es]
       (clojure.lang.MapEntry.
        ke
        (apply g ve (vals es)))))
  Applicative
  (pure [[ke _] v]
    (clojure.lang.MapEntry. (id ke) v))
  (<*> [[kv vv] [_ vg]]
    (clojure.lang.MapEntry. kv (vg vv))))

(extend-type clojure.lang.IPersistentCollection
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

;;===================== Literals Extensions ================
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

;;===================== Function ===========================
(extend-type clojure.lang.Fn
  Functor
  (fmap
    ([f g]
      (comp g f))
    ([f g gs]
      (apply comp g f gs))))

;;====================== References ========================
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
