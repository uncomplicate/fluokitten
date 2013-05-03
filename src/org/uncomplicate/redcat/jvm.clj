(ns org.uncomplicate.redcat.jvm
  (:use [org.uncomplicate.redcat protocols algo]))

(set! *warn-on-reflection* true)

(ns org.uncomplicate.redcat.algo)
(defn create-mapentry [k v]
  (clojure.lang.MapEntry. k v))

(ns org.uncomplicate.redcat.jvm)

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
  Foldable
  {:fold collection-fold
   :foldmap collection-foldmap}
  Magma
  {:op coll-op}
  Monoid
  {:id empty
   :monoidf collection-monoidf}
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
  {:join seq-join
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
  {:join seq-join
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
   :bind map-bind}
  Foldable
  {:fold map-fold
   :foldmap map-foldmap})

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
  Semigroup)

(let [kmf (partial monoidf* (keyword ""))]
  (defn keyword-monoidf [_] kmf))

(extend clojure.lang.Keyword
  Monoid
  {:id (fn [_] (keyword ""))
   :monoidf keyword-monoidf})

;;===================== Function ===========================
;;--------------------- CurriedFn ----------------------------
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
(def cidentity curried)

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

(defn curried-op
  ([x y]
       (if (= identity (original x))
         y
         (if (= identity (original y))
           x
           (curried-fmap x y))))
  ([x y ys]
     (reduce curried-op x (cons y ys))))

(defn curried-identity [_]
  cidentity)

(let [cmf (partial monoidf* cidentity)]
  (defn curried-monoidf [_] cmf))

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
  {:op curried-op}
  Monoid
  {:id curried-identity
   :monoidf curried-monoidf}
  Semigroup)

(extend clojure.lang.AFunction
  Functor
  {:fmap function-fmap}
  Magma
  {:op function-op}
  Monoid
  {:id function-identity
   :monoidf function-monoidf}
  Semigroup)

;;====================== References =======================
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
  {:op reference-op}
  Semigroup)

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
  {:op reference-op}
  Semigroup)
:w
