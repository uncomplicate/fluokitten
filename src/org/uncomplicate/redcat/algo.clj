(ns org.uncomplicate.redcat.algo
  (:use org.uncomplicate.redcat.protocols)
  (:require [clojure.core.reducers :as r]))

(defn deref? [x]
  (instance? clojure.lang.IDeref x))

(defn reducible? [x]
  (instance? clojure.core.protocols.CollReduce x))

(defn reducible [c]
  (r/map identity c))

(defn realize [c cr]
  (into (empty c) cr))

(defn arg-counts [f]
  (map alength (map (fn [^java.lang.reflect.Method m]
                      ( .getParameterTypes m))
                    (.getDeclaredMethods
                     ^java.lang.Class (class f)))))

(defn monoidf*
  ([ide] ide)
  ([ide e1 e2] (op e1 e2)))

(extend-type nil
  Functor
  (fmap
    ([_ _] nil)
    ([_ _ _] nil))
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
  Semigroup)

(let [nmf (partial monoidf* nil)]
  (extend nil
    Monoid
    {:id (fn [_] nil)
     :monoidf (fn [_] nmf)}))

(extend-type Object
  Functor
  (fmap
    ([o f]
      (f o))
    ([o f os]
       (apply f o os))))

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
  ([l g]
     (with-meta
       (apply list (map g l))
       (meta l)))
  ([l g ss]
     (with-meta
       (apply list (apply map g l ss))
       (meta l))))

(defn seq-fmap
  ([s g]
     (with-meta (map g s) (meta s)))
  ([s g ss]
     (with-meta
       (apply map g s ss)
       (meta s))))

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
                (if-let [vs (seq (into [] (group-entries
                                           kg (cons mv mvs))))]
                  [kg (apply vg vs)]))
              mg)))))

(defn list-fapply
  ([cv sg]
     (with-meta
       (apply list (mapcat #(map % cv) sg))
       (meta cv)))
  ([cv sg svs]
     (with-meta
       (apply list (mapcat #(apply map % cv svs) sg))
       (meta cv))))

(defn seq-fapply
  ([cv sg]
     (with-meta
       (mapcat #(map % cv) sg)
       (meta cv)))
  ([cv sg svs]
     (with-meta
       (mapcat #(apply map % cv svs) sg)
       (meta cv))))

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
     (throw (ex-info "Bind for reducibles does not support varargs."
                     {:exception-type :unsupported-operation}))))

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
  (with-meta
    (apply list (flatten c))
    (meta c)))

(defn list-bind
  ([c g]
     (with-meta
       (apply list (mapcat g c))
       (meta c)))
  ([c g ss]
     (with-meta
       (apply list (apply mapcat g c ss))
       (meta c))))

(defn seq-join [c]
  (with-meta (flatten c) (meta c)))

(defn seq-bind
  ([c g]
     (with-meta
       (mapcat g c)
       (meta c)))
  ([c g ss]
     (with-meta
       (apply mapcat g c ss)
       (meta c))))

;;======== Algebraic structures implementations ==================
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

(let [cmf (partial monoidf* nil)]
  (defn collection-monoidf [_] cmf))

;;================== Foldable ===================================
(defn collection-fold [c]
  (r/fold (monoidf (first c)) c))

(defn collection-foldmap [c g]
  (fold (r/map g c)))

(defn map-fold [m]
  (collection-fold (vals m)))

(defn map-foldmap [m g]
  (collection-foldmap (seq m) g))

(defn collfold-fold [c]
  (r/fold (monoidf
           (first (into [] (r/take 1 c))))
          c))

;;================== Collections Extensions =====================
(extend clojure.core.protocols.CollReduce
  Functor
  {:fmap collreduce-fmap}
  Applicative
  {:pure collreduce-pure
   :fapply collreduce-fapply}
  Monad
  {:join collreduce-join
   :bind collreduce-bind})

(extend clojure.core.reducers.CollFold
  Foldable
  {:fold collfold-fold
   :foldmap collection-foldmap})

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

;;===================== Literals Extensions ================
(extend-type String
  Functor
  (fmap
    ([s g]
      (apply str (g s)))
    ([s g ss]
       (apply str (apply g s ss))))
  Magma
  (op
    ([x y]
       (str x y))
    ([x y ys]
       (apply str x y ys)))
  Semigroup)

(let [smf (partial monoidf* "")]
  (extend String
    Monoid
    {:id (fn [_] "")
     :monoidf (fn [_] smf)}))

(extend-type Number
  Magma
  (op
    ([x y]
       (+ x y))
    ([x y ys]
       (apply + x y ys)))
  Semigroup)

(let [nmf (partial monoidf* 0)]
  (extend Number
    Monoid
    {:id (fn [_] 0)
     :monoidf (fn [_] nmf)}))

;;===================== Function ===========================
;;-------------------- clojure.lang.IFn -------------------
(defn function-fmap
  ([f g]
     (comp g f))
  ([f g gs]
     (apply comp g f gs)))

(defn function-op
  ([x y]
       (if (= identity x)
         y
         (if (= identity y)
           x
           (comp x y))))
  ([x y ys]
     (reduce function-op x (cons y ys))))

(defn function-identity [_]
  identity)
;;---------------------------------------------------------

(let [fmf (partial monoidf* identity)]
  (defn function-monoidf [_] fmf))


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
