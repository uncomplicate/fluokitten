(ns uncomplicate.fluokitten.algo
  (:use [uncomplicate.fluokitten protocols utils])
  (:require [clojure.core.reducers :as r]))

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
  Foldable
  (fold [_] nil)
  (foldmap [_ _] nil)
  Magma
  (op
    ([_ y] y)
    ([_ y ys]
     (reduce op y ys)))
  Monoid
  (id [_] nil))

(extend-type Object
  Functor
  (fmap
    ([o f]
     (f o))
    ([o f os]
     (apply f o os)))
  Foldable
  (fold [o] o)
  (foldmap [o g] (g o)))

;;=============== Functor implementations =========================
;;--------------- fmap implementations ---------------

(defn collreduce-fmap
  ([cr g]
   (r/map g cr))
  ([_ _ _]
   (throw (java.lang.UnsupportedOperationException.
           "fmap for reducibles does not support varargs."))))

(defn reducible-fmap
  ([c g]
   (into (empty c)
         (r/map g c)))
  ([c g cs]
   (into (empty c)
         (apply map g c cs))))

(defn ^:private group-entries-xf [k]
  (comp (map #(find % k))
        (remove nil?)
        (map val)))

(defn ^:private map-fmap-xf
  ([g ms]
   (comp (mapcat keys)
         (dedupe)
         (map (fn [k] [k (apply g (into [] (group-entries-xf k) ms))])))))

(defn map-fmap
  ([m g]
   (into (empty m) (r/map (fn [[k v]] [k (g v)]) m)))
  ([m g ms]
   (let [ms (cons m ms)]
     (into (empty m)
           (map-fmap-xf g ms)
           ms))))

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
   (with-meta
     (map g s)
     (meta s)))
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

(defn collreduce-fapply [crg crv]
  (r/mapcat #(r/map % crv) crg))

(defn collreduce-pure [_ v]
  (r/map identity [v]))

(defn reducible-fapply
  ([cg cv]
   (into (empty cv)
         (collreduce-fapply cg cv)))
  ([cg cv cvs]
   (into (empty cv)
         (r/mapcat #(apply map % cv cvs) cg))))

(defn ^:private apply-universal-f [mf m]
  (if-let [f (mf nil)]
    (map-fmap m f)
    m))

(defn map-fapply
  ([mg mv]
   (if-let [f (mg nil)]
     (into (empty mv)
           (r/map (fn [[kv vv]]
                    (if-let [eg (find mg kv)]
                      [kv ((val eg) vv)]
                      [kv (f vv)]))
                  mv))
     (into mv
           (comp (map (fn [[kg vg]]
                        (if-let [ev (find mv kg)]
                          [kg (vg (val ev))])))
                 (remove nil?))
           mg)))
  ([mg mv mvs]
   (let [ms (cons mv mvs)]
     (if-let [f (mg nil)]
       (into (empty mv)
             (comp (mapcat keys)
                   (dedupe)
                   (map (fn [kv]
                          (let [vs (into [] (group-entries-xf kv) ms)
                                fun (if-let [eg (find mg kv)] (val eg) f)]
                            [kv (apply fun vs)]))))
             ms)
       (into (apply merge mv mvs)
             (comp (map (fn [[kg vg]]
                          (if-let [vs (seq (into [] (group-entries-xf kg) ms))]
                            [kg (apply vg vs)])))
                   (remove nil?))
             mg)))))

(defn list-fapply
  ([cg cv]
   (with-meta
     (apply list (mapcat #(map % cv) cg))
     (meta cv)))
  ([cg cv cvs]
   (with-meta
     (apply list (mapcat #(apply map % cv cvs) cg))
     (meta cv))))

(defn seq-fapply
  ([cg cv]
   (with-meta
     (mapcat #(map % cv) cg)
     (meta cv)))
  ([cg cv cvs]
   (with-meta
     (mapcat #(apply map % cv cvs) cg)
     (meta cv))))

(defn coll-fapply
  ([cg cv]
   (into (empty cv)
         (mapcat #(map % cv) cg)))
  ([cg cv cvs]
   (into (empty cv)
         (mapcat #(apply map % cv cvs) cg))))

(defn coll-pure [cv v]
  (conj (empty cv) v))

(defn lazyseq-pure [cv v]
  (lazy-seq (coll-pure cv v)))

(defn map-pure [m v]
  (coll-pure m [nil v]))

;;================== Monad Implementations ======================

(defn collreduce-bind
  ([cr g]
   (r/mapcat g cr))
  ([cr g ss]
   (throw (ex-info "bind for reducibles does not support varargs."
                   {:exception-type :unsupported-operation}))))

(defn default-bind
  ([m g]
   (join (fmap m g)))
  ([m g ms]
   (join (fmap m g ms))))

(defn reducible-bind
  ([c g]
   (into (empty c)
         (r/mapcat g c)))
  ([c g ss]
   (into (empty c)
         (apply mapcat g c ss))))

(let [flatten-keys (fn [[k x :as e]]
                     (if (map? x)
                       (map (fn [[kx vx]]
                              [(if (and k kx)
                                 (vec (flatten [k kx]))
                                 (or k kx))
                               vx])
                            x)
                       [e]))]

  (defn map-join [m]
    (into (empty m) (mapcat flatten-keys m)))

  (defn map-bind
    ([m g]
     (into (empty m)
           (comp (map (fn [[k v]] [k (g v)]))
                 (mapcat flatten-keys))
           m))
    ([m g ms]
     (let [ms (cons m ms)]
       (into (empty m)
             (comp (map-fmap-xf g ms)
                   (mapcat flatten-keys))
             ms)))))

(defn coll-bind
  ([c g]
   (into (empty c)
         (mapcat g c)))
  ([c g ss]
   (into (empty c)
         (apply mapcat g c ss))))

(defn ^:private join-op [c e]
  (if (coll? e)
    (op c e)
    (conj c e)))

(defn seq-join [c]
  (with-meta
    (reduce join-op [] c)
    (meta c)))

(defn coll-join [c]
  (into (empty c) (seq-join c)))

(defn list-join [c]
  (with-meta
    (apply list (seq-join c))
    (meta c)))

(defn collreduce-join [c]
  (let [combinef (fn [x y]
                   (if (coll? x)
                     (join-op x y)
                     (if (coll? y)
                       (conj y x)
                       [x y])))]
    (r/fold (fn [] []) combinef c)))

(defn reducible-join [c]
  (into (empty c)
        (collreduce-join c)))

(defn list-bind
  ([c g]
   (with-meta
     (apply list (mapcat g c))
     (meta c)))
  ([c g ss]
   (with-meta
     (apply list (apply mapcat g c ss))
     (meta c))))

(defn seq-bind
  ([c g]
   (with-meta
     (mapcat g c)
     (meta c)))
  ([c g ss]
   (with-meta
     (apply mapcat g c ss)
     (meta c))))

(defn lazyseq-bind
  ([c g]
   (seq-bind c g))
  ([c g ss]
   (seq-bind c g ss)))

;;======== Algebraic structures implementations ==================

(defn coll-op
  ([x y]
   (into x y))
  ([x y ys]
   (reduce into (into x y) ys)))

(defn collreduce-op
  ([x y]
   (if (instance? clojure.core.protocols.CollReduce y)
     (r/cat x y)
     (into x y)))
  ([x y ys]
   (r/reduce collreduce-op (collreduce-op x y) ys)))

(defn reducible-op
  ([x y]
   (into (empty x) (collreduce-op x y)))
  ([x y ys]
   (into (empty x) (collreduce-op x y ys))))

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

;;================== Foldable ===================================

(defn collection-fold [c]
  (let [ide (id (first c))]
    (r/fold (fn [] ide) op c)))

(defn collection-foldmap [c g]
  (fold (r/map g c)))

(defn map-fold [m]
  (collection-fold (vals m)))

(defn map-foldmap [m g]
  (collection-foldmap (seq m) g))

(defn collfold-fold [c]
  (let [ide (id (first (into [] (r/take 1 c))))]
    (r/fold (fn [] ide) op c)))

;;================== Collections Extensions =====================

(defmacro extend-collection [t]
  `(extend ~t
     Functor
     {:fmap coll-fmap}
     Applicative
     {:pure coll-pure
      :fapply coll-fapply}
     Monad
     {:join coll-join
      :bind coll-bind}
     Foldable
     {:fold collection-fold
      :foldmap collection-foldmap}
     Magma
     {:op coll-op}
     Monoid
     {:id empty}))

(defmacro extend-vector [t]
  `(extend ~t
     Functor
     {:fmap reducible-fmap}
     Applicative
     {:pure coll-pure
      :fapply reducible-fapply}
     Monad
     {:join reducible-join
      :bind reducible-bind}
     Magma
     {:op reducible-op}))

(defmacro extend-list [t]
  `(extend ~t
     Functor
     {:fmap list-fmap}
     Applicative
     {:pure coll-pure
      :fapply list-fapply}
     Monad
     {:join list-join
      :bind list-bind}
     Magma
     {:op list-op}))

(defmacro extend-seq [t]
  `(extend ~t
     Functor
     {:fmap seq-fmap}
     Applicative
     {:pure coll-pure
      :fapply seq-fapply}
     Monad
     {:join seq-join
      :bind seq-bind}
     Magma
     {:op seq-op}))

(defmacro extend-lazyseq [t]
  `(extend ~t
     Functor
     {:fmap seq-fmap}
     Applicative
     {:pure lazyseq-pure
      :fapply seq-fapply}
     Monad
     {:join seq-join
      :bind lazyseq-bind}
     Magma
     {:op seq-op}))
(defmacro extend-set [t]
  `(extend ~t
     Functor
     {:fmap reducible-fmap}
     Applicative
     {:pure coll-pure
      :fapply reducible-fapply}
     Monad
     {:join coll-join
      :bind reducible-bind}))

(defmacro extend-map [t]
  `(extend ~t
     Functor
     {:fmap map-fmap}
     Applicative
     {:pure map-pure
      :fapply map-fapply}
     Monad
     {:join map-join
      :bind map-bind}
     Foldable
     {:fold map-fold
      :foldmap map-foldmap}))

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

(declare create-mapentry)

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
  ([[kg vg] [ke ve :as e]]
   (if (or (nil? kg) (= ke kg))
     (create-mapentry ke (vg ve))
     e))
  ([[kg vg] [ke ve :as e] es]
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
                         (vec (flatten [k kx]))
                         (or k kx))
                       vx))
    e))

(defn mapentry-id [[kx vx]]
  (create-mapentry (id kx) (id vx)))

(defn mapentry-op
  ([[kx vx] [ky vy]]
   (create-mapentry (op kx ky) (op vx vy)))
  ([[kx vx] [ky vy] es]
   (create-mapentry (op kx ky (map key es))
                    (op vx vy (map val es)))))

(defn mapentry-fold [[_ v]]
  v)

(defmacro extend-mapentry [t]
  `(extend ~t
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
     Monoid
     {:id mapentry-id}
     Foldable
     {:fold mapentry-fold}))

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
  Monoid
  (id [s] ""))

(extend-type Number
  Magma
  (op
    ([x y]
     (+ x y))
    ([x y ys]
     (apply + x y ys)))
  Monoid
  (id [x] 0))

(defn keyword-fmap
  ([k g]
   (keyword (fmap (name k) g)))
  ([k g ks]
   (keyword (fmap (name k) g
                  (map name ks)))))

(let [empty-keyword (keyword "")]
  (defn keyword-id [_]
    empty-keyword))

(defn keyword-op
  ([_] keyword-id)
  ([x y]
   (keyword (str (name x) (name y))))
  ([x y ys]
   (keyword (apply str
                   (name x)
                   (name y)
                   (map name ys)))))

(defmacro extend-keyword [t]
  `(extend ~t
     Functor
     {:fmap keyword-fmap}
     Magma
     {:op keyword-op}
     Monoid
     {:id keyword-id}))

;;===================== Function ===========================
;;-------------------- clojure.lang.IFn -------------------

(defn function-fmap
  ([f g]
   (comp g f))
  ([f g gs]
   (apply comp g f gs)))

(defn function-id [_]
  identity)

(defn function-op
  ([x y]
   (if (= identity x)
     y
     (if (= identity y)
       x
       (comp x y))))
  ([x y ys]
   (reduce function-op x (cons y ys))))

(defmacro extend-function [t]
  `(extend ~t
     Functor
     {:fmap function-fmap}
     Magma
     {:op function-op}
     Monoid
     {:id function-id}))

;;====================== References =======================
;;----------------- Universal ------------------

(defn reference-fapply
  ([rg rv]
   (fmap rv (deref rg)))
  ([rg rv rvs]
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

(defn reference-fold [r] @r)

(defn reference-foldmap [r g] (g @r))

;;----------------- Agent -----------------------

(defn agent-fmap
  ([a g]
   (do (swap! a g) a))
  ([a g as]
   (do (apply swap! a g (map deref as)) a)))

(defn agent-pure [_ v]
  (atom v))

(defn atom-id [a]
  (atom (id @a)))

;;------------------- Ref --------------------------

(defn ref-fmap
  ([r g]
   (do (alter r g) r))
  ([r g rs]
   (do (apply alter r g (map deref rs)) r)))

(defn ref-pure [_ v]
  (ref v))

(defn ref-id [r]
  (ref (id @r)))

;;------------------ Extensions --------------------

(defmacro extend-atom [t]
  `(extend ~t
     Functor
     {:fmap agent-fmap}
     Applicative
     {:pure agent-pure
      :fapply reference-fapply}
     Monad
     {:join reference-join
      :bind default-bind}
     Foldable
     {:fold reference-fold
      :foldmap reference-foldmap}
     Magma
     {:op reference-op}
     Monoid
     {:id atom-id}))

(defmacro extend-ref [t]
  `(extend ~t
     Functor
     {:fmap ref-fmap}
     Applicative
     {:pure ref-pure
      :fapply reference-fapply}
     Monad
     {:join reference-join
      :bind default-bind}
     Foldable
     {:fold reference-fold
      :foldmap reference-foldmap}
     Magma
     {:op reference-op}
     Monoid
     {:id ref-id}))

;;================== Maybe Just ===========================

(deftype Just [v]
  Object
  (hashCode [_]
    (hash v))
  (equals [this that]
    (or (identical? this that)
        (and (instance? Just that)
             (= v (fold that)))))
  Functor
  (fmap [_ g]
    (Just. (g v)))
  (fmap [_ g jvs]
    (if (some nil? jvs)
      nil
      (Just. (apply g v (map fold jvs)))))
  Applicative
  (pure [_ x]
    (Just. x))
  (fapply [_ jv]
    (fmap jv v))
  (fapply [_ jv jvs]
    (fmap jv v jvs))
  Monad
  (bind [_ g]
    (g v))
  (bind [_ g jvs]
    (if (some nil? jvs)
      nil
      (apply g v (map fold jvs))))
  (join [jjv]
    (if (or (nil? v) (instance? Just v)) v jjv))
  Foldable
  (fold [_] v)
  (foldmap [_ g] (g v))
  Magma
  (op [x y]
    (if-not (nil? y)
      (Just. (op v (fold y)))
      x))
  (op [x y ys]
    (if-let [ys* (map fold (remove nil? (cons y ys)))]
      (Just. (op v (first ys*) (rest ys*)))
      x))
  Monoid
  (id [_] nil))
