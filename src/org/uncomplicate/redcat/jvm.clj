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
     (into (empty c)
           (r/map g c)))
  ([c g cs]
     (into (empty c)
           (apply map g c cs))))

(defn map-fmap
  ([m g]
     (into (empty m)
           (r/map
            (fn [[k v]] [k (g v)])
            m)))
  ([m g ms]
     (into (empty m)
           (apply map
                  (fn [[k v] & es]
                    [k (apply g v (vals es))])
                  m ms))))

(defn list-fmap
  ([s g]
     (apply list (map g s)))
  ([s g ss]
     (apply list (apply map g s ss))))

(defn seq-fmap
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
     (let [f (fn [[res m] [kg vg]]
                 (if-let [[kv vv] (find m kg)]
                   [(conj res [kv (vg vv)]) m]
                   [res m]))]
       (let [imv (if-let [ig (mg nil)]
                   (map-fmap mv ig)
                   mv)]
         (merge imv (first (reduce f [{} imv] (dissoc mg nil)))))))
  ([mv mg mvs]
     (throw (java.lang.UnsupportedOperationException. "TODO"))));;TODO

(defn list-<*>
  ([cv sg]
     (apply list (mapcat #(map % cv) sg)))
  ([cv sg svs]
     (apply list (mapcat #(apply map % cv svs) sg))))

(defn seq-<*>
  ([cv sg]
     (mapcat #(map % cv) sg))
  ([cv sg svs]
     (mapcat #(apply map % cv svs) sg)))

(defn coll-<*>
  ([cv sg]
     (into (empty cv)
           (mapcat #(map % cv) sg)))
  ;;(bind cg (partial fmap sv)))
  ([cv sg svs]
     (into (empty cv)
           (mapcat #(apply map % cv svs) sg))))

(defn coll-pure [cv v]
  (conj (empty cv) v))

(defn lazyseq-pure [cv v]
  (lazy-seq (coll-pure cv v)))

(defn map-pure [m v]
  (coll-pure m [nil v]))

;;================== Monad Implementations ======================

(defn default-bind [c g]
  (join (fmap c g)))

(defn reducible-join [c]
  (into (empty c) (r/flatten c)))

(defn reducible-bind [c g]
  (into (empty c) (r/mapcat g c)))

(defn map-bind [c g]
  (into (empty c)
        (r/mapcat (fn [[k v]]
                    (r/map (fn [[kx vx]]
                             (if (nil? k)
                               [kx vx]
                               (if (nil? kx)
                                 [k vx]
                                 [(join [k kx]) vx])))
                           (g v)))
                  c)))

(defn coll-join [c]
  (into (empty c) (flatten c)))

(defn coll-bind [c g]
  (into (empty c) (mapcat g c)))

(defn list-join [c]
  (apply list (flatten c)))

(defn list-bind [c g]
  (apply list (mapcat g c)))

(defn seq-bind [c g]
  (mapcat g c))
;;================== Collections Extensions =====================
(extend clojure.lang.IPersistentCollection
  Functor
  {:fmap coll-fmap}
  Applicative
  {:pure coll-pure
   :<*> coll-<*>}
  Monad
  {:join coll-join
   :bind coll-bind}
  Semigroup
  {:op into}
  Monoid
  {:id empty})

(extend clojure.lang.APersistentVector
  Functor
  {:fmap reducible-fmap}
  Applicative
  {:pure coll-pure
   :<*> reducible-<*>}
  Monad
  {:join reducible-join
   :bind reducible-bind})

(extend clojure.lang.PersistentList
  Functor
  {:fmap list-fmap}
  Applicative
  {:pure coll-pure
   :<*> list-<*>}
  Monad
  {:join list-join
   :bind list-bind})

(extend clojure.lang.ASeq
  Functor
  {:fmap seq-fmap}
  Applicative
  {:pure coll-pure
   :<*> seq-<*>}
  Monad
  {:join flatten
   :bind seq-bind})

(extend clojure.lang.LazySeq
  Functor
  {:fmap seq-fmap}
  Applicative
  {:pure lazyseq-pure
   :<*> seq-<*>}
  Monad
  {:join flatten
   :bind seq-bind})

(extend clojure.lang.APersistentSet
  Functor
  {:fmap reducible-fmap}
  Applicative
  {:pure coll-pure
   :<*> reducible-<*>}
  Monad
  {:join reducible-join
   :bind reducible-bind})

(extend clojure.lang.APersistentMap
  Functor
  {:fmap map-fmap}
  Applicative
  {:pure map-pure
   :<*> map-<*>}
  Monad
  {:join nil
   :bind map-bind})

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
  (pure [e v]
    (clojure.lang.MapEntry. nil v))
  (<*> [[ke ve :as e] [kg vg]]
    (if (or (nil? kg) (= ke kg))
      (clojure.lang.MapEntry. ke (vg ve))
      e))
  Monad
  (join [e] (throw (UnsupportedOperationException. "TODO"))) ;;TODO
  (bind [[ke ve] g]
    (let [[kg vg] (g ve)]
      (clojure.lang.MapEntry. (if (and ke kg)
                                (join [ke kg])
                                (or ke kg))
                              vg))))

(extend-type clojure.lang.IPersistentCollection
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
    ([s g ss]
       (apply str (apply g s ss))))
  Semigroup
  (op [s s1]
    (str s s1))
  Monoid
  (id [s] ""))

(extend-type Number
  Semigroup
  (op [n y]
    (+ n y))
  Monoid
  (id [n] 0))

;;===================== Function ===========================
(extend-type clojure.lang.Fn
  Functor
  (fmap
    ([f g]
      (comp g f))
    ([f g gs]
       (apply comp g f gs)))
  Semigroup
  (op [f g] (comp f g))
  Monoid
  (id [f] identity))

;;====================== References ========================
(extend-type clojure.lang.Atom
  Functor
  (fmap
    ([a g]
       (do (swap! a g) a))
    ([a g args]
       (do (apply swap! a g (map deref args)) a)))
  Applicative
  (pure [a v]
    (atom v))
  (<*> [av ag]
    (fmap av (deref ag)))
  Monad
  (join [ma]
    (fmap ma deref))
  (bind [a g]
    (join (fmap a g))))

(extend-type clojure.lang.Ref
  Functor
  (fmap
    ([r g]
       (do (alter r g) r))
    ([a g args]
       (do (apply alter a g (map deref args)) a)))
  Applicative
  (pure [r v] (ref v))
  (<*> [rv rg]
    (fmap rv (deref rg)))
  Monad
  (join [ma]
    (fmap ma deref))
  (bind [a g]
    (join (fmap a g))))
