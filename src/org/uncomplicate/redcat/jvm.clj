(ns org.uncomplicate.redcat.jvm
  (:use org.uncomplicate.redcat.protocols)
  (:require [clojure.core.reducers :as r]))

(set! *warn-on-reflection* true)

(defn deref? [x]
  (instance? clojure.lang.IDeref x))

(defn reducible? [x]
  (instance? clojure.core.protocols.CollReduce x))

;; TODO see whether it is necessary
(defn arg-counts [f]
  (map alength (map (fn [^java.lang.reflect.Method m]
                      ( .getParameterTypes m))
                    (.getDeclaredMethods
                     ^java.lang.Class (class f)))))

(extend-type nil
  Functor
  (fmap
    ([_ _] nil)
    ([_ _ args] nil))
  Applicative
  (pure [_ _] nil)
  (fapply
    ([_ _] nil)
    ([_ _ _] nil))
  Monad
  (bind
    ([_ _] nil)
    ([_ _ _] nil))
  (join [_] nil)
  Magma
  (op
    ([_ _] nil)
    ([_ _ _] nil))
  Monoid
  (id [_] nil)
  Semigroup)

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
;;================== Semigroup implementations ==================
(defn coll-op
  ([x y]
     (into x y))
  ([x y ys]
     (reduce #(into %1 %2) (into x y) ys)))

(defn seq-op
  ([x y]
     (concat x y))
  ([x y ys]
     (apply concat x y ys)))

(defn list-op
  ([x y]
     (apply list (seq-op x y)))
  ([x y ys]
     (apply list (seq-op x y ys))))

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
  Magma
  {:op coll-op}
  Monoid
  {:id empty}
  Semigroup)

(extend clojure.lang.APersistentVector
  Functor
  {:fmap reducible-fmap}
  Applicative
  {:pure coll-pure
   :fapply reducible-fapply}
  Monad
  {:join reducible-join
   :bind reducible-bind})

(extend clojure.lang.IPersistentList
  Functor
  {:fmap list-fmap}
  Applicative
  {:pure coll-pure
   :fapply list-fapply}
  Monad
  {:join list-join
   :bind list-bind}
  Magma
  {:op list-op})

(extend clojure.lang.PersistentList
  Functor
  {:fmap list-fmap}
  Applicative
  {:pure coll-pure
   :fapply list-fapply}
  Monad
  {:join list-join
   :bind list-bind}
  Magma
  {:op list-op})

(extend clojure.lang.ASeq
  Functor
  {:fmap seq-fmap}
  Applicative
  {:pure coll-pure
   :fapply seq-fapply}
  Monad
  {:join flatten
   :bind seq-bind}
  Magma
  {:op seq-op})

(extend clojure.lang.LazySeq
  Functor
  {:fmap seq-fmap}
  Applicative
  {:pure lazyseq-pure
   :fapply seq-fapply}
  Monad
  {:join flatten
   :bind seq-bind}
  Magma
  {:op seq-op})

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

(defn mapentry-op
  ([[kx vx] [ky vy]]
     (create-mapentry (op kx ky) (op vx vy)))
  ([[kx vx] [ky vy] es]
     (create-mapentry (op kx ky (map key es))
                      (op vx vy (map val es)))))

(extend clojure.lang.MapEntry
  Functor
  {:fmap mapentry-fmap}
  Applicative
  {:pure mapentry-pure
   :fapply mapentry-fapply}
  Monad
  {:join mapentry-join
   :bind default-bind}
  Magma
  {:op mapentry-op}
  Semigroup);;TODO Maybe mapentry could be monoid once maybe is implemented?...

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
  Magma
  (op
    ([x y]
       (str x y))
    ([x y ys]
       (apply str x y ys)))
  Monoid
  (id [s] "")
  Semigroup)

(extend-type Number
  Magma
  (op
    ([x y]
       (+ x y))
    ([x y ys]
       (apply + x y ys)))
  Monoid
  (id [x] 0)
  Semigroup)

(extend-type clojure.lang.Keyword
  Functor
  (fmap
    ([k g]
       (keyword (fmap (name k) g)))
    ([k g ks]
       (keyword (fmap (name k) g
                      (map name ks)))))
  Magma
  (op
    ([x y]
       (keyword (str (name x) (name y))))
    ([x y ys]
       (keyword (apply str
                       (name x)
                       (name y)
                       (map name ys)))))
  Monoid
  (id [x] (keyword ""))
  Semigroup)

;;===================== Function ===========================
;;--------------------- Curried ----------------------------
(declare curry*)

(defn ^:private  gen-invoke [^clojure.lang.IFn f arity n]
  (let [args (map #(symbol (str "a" %)) (range arity))]
        `(invoke [~'_ ~@args]
                 (if (> ~n ~arity)
                   (curry* (partial ~f ~@args) (- ~n ~arity))
                   (.invoke ~f ~@args)))))

(defn ^:private  gen-applyto [^clojure.lang.IFn f n]
  `(applyTo [~'_ ~'args]
            (let [as# (- ~n (count ~'args))]
              (if (pos? as#)
                (curry* (apply partial ~f ~'args) as#)
                (.applyTo ~f ~'args)))))

(defmacro ^:private deftype-curried-fn []
  `(deftype ~'CurriedFn ~'[^clojure.lang.IFn f n]
     Curried
     ~'(original [_] f)
     ~'(arity [_] n)
     clojure.lang.IFn
     ~@(map #(gen-invoke 'f % 'n) (range 22))
     ~(gen-applyto 'f 'n)
     java.util.concurrent.Callable
     ~'(call [_]
         (if (pos? n) f (.call f)))
     java.lang.Runnable
     ~'(run [_]
         (if (pos? n) f (.run f)))))

(deftype-curried-fn)

(defn ^:private curry* [f n]
  (CurriedFn. f n))

(def curried? (partial instance? CurriedFn))

(defn curry
  ([f] (curry f (min 2 (apply max (arg-counts f)))))
  ([f n]
     (if (and (fn? f) (pos? n))
       (curry* f n)
       f)))

(def curried (curry identity 1))

;;---------------------------------------------------------
;;-------------------- clojure.lang.IFn -------------------
(defn function-fmap
  ([f g]
     (comp g f))
  ([f g gs]
     (apply comp g f gs)))

;;-------------------- CurriedFn --------------------------
(defn curried-fmap
  ([cf g]
     (curry (comp g (original cf))
            (arity cf)))
  ([cf g cfs]
     (reduce curried-fmap
             (into (list cf g) (seq cfs)))))

(defn curried-pure [cf x]
  (curry* (fn [& _] x) 0))

(defn curried-fapply
  ([cf cg]
     (curry (fn
              ([x]
                 ((cg x) (cf x)))
              ([x & xs]
                 ((apply cg x xs) (apply cf x xs))))
            1))
  ([cf cg hs]
     (reduce #(curried-fapply %2 %1)
             (into [cg cf] hs))))

(defn curried-bind
  ([cf cg]
     (curry (fn
              ([x]
                 ((cg (cf x)) x))
              ([x & xs]
                 (apply (cg (apply cf x xs)) x xs)))
            1))
  ([cf cg hs]
     (reduce #(curried-bind %2 %1)
             (into [cg cf] hs))))

(defn curried-join [cf]
  (bind cf identity))
;;---------------------------------------------------------

(extend clojure.lang.AFunction
  Functor
  {:fmap function-fmap}
  Semigroup
  {:op comp}
  Monoid
  {:id identity})

(extend CurriedFn
  Functor
  {:fmap curried-fmap}
  Applicative
  {:pure curried-pure
   :fapply curried-fapply}
  Monad
  {:join curried-join
   :bind curried-bind}
  Magma
  {:op curried-fmap}
  Monoid
  {:id curried}
  Semigroup)

;;====================== References =======================
;;----------------- Universal ------------------
(defn reference-fapply
  ([rv rg]
     (fmap rv (deref rg)))
  ([rv rg rvs]
     (fmap rv (deref rg) rvs)))

(defn reference-join [r]
  (fmap r #(if (deref? %) (deref %) %)))

(defn reference-op
  ([rx ry]
     (pure rx (op (deref rx) (deref ry))))
  ([rx ry rys]
     (pure rx (op
               (deref rx)
               (deref ry)
               (map deref rys)))))

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
   :bind default-bind}
  Magma
  {:op reference-op})

(extend clojure.lang.Ref
  Functor
  {:fmap ref-fmap}
  Applicative
  {:pure ref-pure
   :fapply reference-fapply}
  Monad
  {:join reference-join
   :bind default-bind}
  Magma
  {:op reference-op})
