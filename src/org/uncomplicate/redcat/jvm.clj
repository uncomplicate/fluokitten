(ns org.uncomplicate.redcat.jvm
  (:use org.uncomplicate.redcat.protocols)
  (:require [clojure.core.reducers :as r]))

(set! *warn-on-reflection* true)

(defn deref? [x]
  (instance? clojure.lang.IDeref x))

(defn reducible? [x]
  (instance? clojure.core.protocols.CollReduce x))

(extend-type nil
  Functor
  (fmap
    ([_ _] nil)
    ([_ _ args] nil))
  Applicative
  (pure [_ _] nil)
  (fapply
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

(defn reducible [c]
  (r/map identity c))

(defn realize [c cr]
  (into (empty c) cr))

;=============== Functor implementations =========================
;;--------------- fmap implementations ---------------
(defn collreduce-fmap
  ([cr g]
     (r/map g cr))
  ([_ _ _]
     (throw (java.lang.UnsupportedOperationException.
             "Fmap for reducibles does not support varargs."))))

(defn reducible-fmap
  ([c g]
     (into (empty c)
           (r/map g c)))
  ([c g cs]
     (into (empty c)
           (apply map g c cs))))

(defn group-entries [k ms]
  (r/map val
         (r/remove nil?
                   (r/map #(find % k) ms))))

(defn apply-key [g maps k]
  [k (apply g (into [] (group-entries k maps)))])

(defn map-fmap-r
  ([m g]
     (r/map (fn [[k v]] [k (g v)]) m))
  ([m g ms]
     (let [source (cons m ms)
           keys (distinct (into [] (r/flatten (r/map keys source))))]
       (r/map (partial apply-key g source) keys))))

(defn map-fmap
  ([m g]
     (into (empty m) (map-fmap-r m g)))
  ([m g ms]
     (into (empty m) (map-fmap-r m g ms))))

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
;;-------------- fapply implementations ----------------
(defn collreduce-fapply [crv crg]
  (r/mapcat #(r/map % crv) crg))

(defn collreduce-pure [_ v]
  (r/map identity  [v]))

(defn reducible-fapply
  ([cv sg]
     (into (empty cv)
           (r/mapcat #(r/map % cv) sg)))
  ([cv sg svs]
     (into (empty cv)
           (r/mapcat #(apply map % cv svs) sg))))

(defn- apply-universal-f [mf m]
  (if-let [f (mf nil)]
    (map-fmap m f)
    m))

(defn map-fapply
  ([mv mg]
     (into
      (if-let [f (mg nil)]
        (map-fmap mv f)
        mv)
      (r/remove
       nil?
       (r/map (fn [[kg vg]]
                (if-let [[kv vv] (find mv kg)]
                  [kv (vg vv)]))
              mg))))
  ([mv mg mvs]
     (into
      (if-let [f (mg nil)]
        (map-fmap mv f mvs)
        (apply merge mv mvs))
      (r/remove
       nil?
       (r/map (fn [[kg vg]]
                (if-let [vs (seq (into [] (group-entries kg (cons mv mvs))))]
                  [kg (apply vg vs)]))
              mg)))))

(defn list-fapply
  ([cv sg]
     (apply list (mapcat #(map % cv) sg)))
  ([cv sg svs]
     (apply list (mapcat #(apply map % cv svs) sg))))

(defn seq-fapply
  ([cv sg]
     (mapcat #(map % cv) sg))
  ([cv sg svs]
     (mapcat #(apply map % cv svs) sg)))

(defn coll-fapply
  ([cv sg]
     (into (empty cv)
           (mapcat #(map % cv) sg)))
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
(defn collreduce-join [cr]
  (r/flatten cr))

(defn collreduce-bind
  ([cr g]
     (r/mapcat g cr))
  ([cr g ss]
     (throw (java.lang.UnsupportedOperationException.
             "Bind for reducibles does not support varargs."))))

(defn default-bind
  ([m g]
     (join (fmap m g)))
  ([m g ms]
     (join (fmap m g ms))))

(defn reducible-join [c]
  (into (empty c) (r/flatten c)))

(defn reducible-bind
  ([c g]
     (into (empty c)
           (r/mapcat g c)))
  ([c g ss]
     (into (empty c)
           (apply mapcat g c ss))))

(defn map-join-r [m]
  (r/mapcat (fn [[k x :as e]]
              (if (map? x)
                (r/map (fn [[kx vx]]
                         [(if (and k kx)
                            (join [k kx])
                            (or k kx))
                          vx])
                       x)
                [e]))
            m))

(defn map-join [m]
  (into (empty m) (map-join-r m)))

(defn map-bind
  ([m g]
     (into (empty m)
           (map-join-r
            (map-fmap-r m g))))
  ([m g ms]
     (into (empty m)
           (map-join-r
            (map-fmap-r m g ms)))))

(defn coll-join [c]
  (into (empty c) (flatten c)))

(defn coll-bind
  ([c g]
     (into (empty c)
           (mapcat g c)))
  ([c g ss]
     (into (empty c)
           (apply mapcat g c ss))))

(defn list-join [c]
  (apply list (flatten c)))

(defn list-bind
  ([c g]
     (apply list (mapcat g c)))
  ([c g ss]
     (apply list (apply mapcat g c ss))))

(defn seq-bind
  ([c g]
     (mapcat g c))
  ([c g ss]
     (apply mapcat g c ss)))

;;================== Collections Extensions =====================
(extend clojure.lang.IPersistentCollection
  Functor
  {:fmap coll-fmap}
  Applicative
  {:pure coll-pure
   :fapply coll-fapply}
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
   :fapply reducible-fapply}
  Monad
  {:join reducible-join
   :bind reducible-bind})

(extend clojure.lang.PersistentList
  Functor
  {:fmap list-fmap}
  Applicative
  {:pure coll-pure
   :fapply list-fapply}
  Monad
  {:join list-join
   :bind list-bind})

(extend clojure.lang.ASeq
  Functor
  {:fmap seq-fmap}
  Applicative
  {:pure coll-pure
   :fapply seq-fapply}
  Monad
  {:join flatten
   :bind seq-bind})

(extend clojure.lang.LazySeq
  Functor
  {:fmap seq-fmap}
  Applicative
  {:pure lazyseq-pure
   :fapply seq-fapply}
  Monad
  {:join flatten
   :bind seq-bind})

(extend clojure.lang.APersistentSet
  Functor
  {:fmap reducible-fmap}
  Applicative
  {:pure coll-pure
   :fapply reducible-fapply}
  Monad
  {:join reducible-join
   :bind reducible-bind})

(extend clojure.lang.APersistentMap
  Functor
  {:fmap map-fmap}
  Applicative
  {:pure map-pure
   :fapply map-fapply}
  Monad
  {:join map-join
   :bind map-bind})

(extend clojure.core.protocols.CollReduce
  Functor
  {:fmap collreduce-fmap}
  Applicative
  {:pure collreduce-pure
   :fapply collreduce-fapply}
  Monad
  {:join collreduce-join
   :bind collreduce-bind})

(defn create-mapentry [k v]
  (clojure.lang.MapEntry. k v))

(defn mapentry-fmap
  ([[ke ve] g]
     (create-mapentry ke (g ve)))
  ([[ke ve] g es]
     (create-mapentry
      ke
      (apply g ve (vals es)))))

(defn mapentry-pure [e v]
  (create-mapentry nil v))

(defn mapentry-fapply
  ([[ke ve :as e] [kg vg]]
     (if (or (nil? kg) (= ke kg))
       (create-mapentry ke (vg ve))
       e))
  ([[ke ve :as e] [kg vg] es]
     (if (or (nil? kg)
             (not (some (fn [[k _]]
                          (not= k kg))
                        (cons e es))))
       (create-mapentry ke (apply vg ve (map val es)))
       e)));;TODO e should be represented with Nothing once Maybe is implemented

(defn mapentry-join [[k x :as e]]
  (if (vector? x)
    (let [[kx vx] x]
      (create-mapentry (if (and k kx)
                                (join [k kx])
                                (or k kx))
                              vx))
    e))

(extend clojure.lang.MapEntry
  Functor
  {:fmap mapentry-fmap}
  Applicative
  {:pure mapentry-pure
   :fapply mapentry-fapply}
  Monad
  {:join mapentry-join
   :bind default-bind})

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
  ;;TODO maybe lambda in a string would be appropriate as applicative
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
;;----------------- Universal ------------------
(defn reference-fapply
  ([rv rg]
     (fmap rv (deref rg)))
  ([rv rg rvs]
     (fmap rv (deref rg) rvs)))

(defn reference-join [r]
    (fmap r deref))

;;----------------- Agent -----------------------
(defn agent-fmap
  ([a g]
     (do (swap! a g) a))
  ([a g as]
     (do (apply swap! a g (map deref as)) a)))

(defn agent-pure [a v]
  (atom v))

;;------------------- Ref --------------------------
(defn ref-fmap
  ([r g]
     (do (alter r g) r))
  ([r g rs]
     (do (apply alter r g (map deref rs)) r)))

(defn ref-pure [a v]
  (ref v))

(extend clojure.lang.Atom
  Functor
  {:fmap agent-fmap}
  Applicative
  {:pure agent-pure
   :fapply reference-fapply}
  Monad
  {:join reference-join
   :bind default-bind})

(extend clojure.lang.Ref
  Functor
  {:fmap ref-fmap}
  Applicative
  {:pure ref-pure
   :fapply reference-fapply}
  Monad
  {:join reference-join
   :bind default-bind})
