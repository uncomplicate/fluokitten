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


(extend clojure.lang.Keyword
  Functor
  {:fmap keyword-fmap}
  Magma
  {:op keyword-op}
  Monoid
  {:id (fn [_] (keyword ""))
   :monoidf keyword-monoidf})

;;===================== Function ===========================
;;--------------------- CurriedFn ----------------------------

(defn ^:private  gen-invoke [^clojure.lang.IFn f arity n]
  (let [args (map #(symbol (str "a" %)) (range arity))]
        `(invoke [~'_ ~@args]
                 (if (> ~n ~arity)
                   (CurriedFn. (partial ~f ~@args) (- ~n ~arity))
                   (.invoke ~f ~@args)))))

(defn ^:private  gen-applyto [^clojure.lang.IFn f n]
  `(applyTo [~'_ ~'args]
            (let [as# (- ~n (count ~'args))]
              (if (pos? as#)
                (CurriedFn. (apply partial ~f ~'args) as#)
                (.applyTo ~f ~'args)))))

(declare curriedfn-monoidf
         cidentity)

(defmacro ^:private deftype-curried-fn []
  `(deftype ~'CurriedFn ~'[^clojure.lang.IFn f n]
     clojure.lang.IDeref
     ~'(deref [_] f)
     Curried
     ~'(arity [_] n)
     clojure.lang.IFn
     ~@(map #(gen-invoke 'f % 'n) (range 22))
     ~(gen-applyto 'f 'n)
     java.util.concurrent.Callable
     ~'(call [_]
         (if (pos? n) f (.call f)))
     java.lang.Runnable
     ~'(run [_]
         (if (pos? n) f (.run f)))
     Functor
     ~'(fmap [_ g]
         (CurriedFn. (comp g f) n))
     ~'(fmap [cf g cfs]
         (reduce fmap (into (list cf g) (seq cfs))))
     Applicative
     ~'(pure [_ x]
         (CurriedFn. (fn [& _] x) 0))
     ~'(fapply [cg cf]
         (CurriedFn.
          (fn
            ([x]
               ((cg x) (cf x)))
            ([x & xs]
               ((apply cg x xs) (apply cf x xs))))
          1))
     ~'(fapply [cg cf hs]
         (reduce fapply cg (cons cf hs)))
     Monad
     ~'(bind [cf cg]
         (CurriedFn.
          (fn
            ([x]
               ((cg (cf x)) x))
            ([x & xs]
               (apply (cg (apply cf x xs)) x xs)))
          1))
     ~'(bind [cf cg hs]
         (reduce #(bind %2 %1)
                 (into [cg cf] hs)))
     ~'(join [cf] (bind cf identity))
     Magma
     ~'(op [x y]
         (if (= identity f)
           y
           (if (= identity (deref y))
             x
             (fmap x y))))
     ~'(op [x y ys]
         (reduce op x (cons y ys)))
     Monoid
     ~'(id [_] cidentity)
     ~'(monoidf [_] curriedfn-monoidf)
     Semigroup))

(deftype-curried-fn)

(def curried? (partial instance? CurriedFn))

(defn curry
  ([f] (curry f (min 2 (apply max (arg-counts f)))))
  ([f n]
     (if (and (fn? f) (pos? n))
       (->CurriedFn f n)
       f)))

(def curried (CurriedFn. identity 1))
(def cidentity curried)

(def curriedfn-monoidf
  (partial monoidf* cidentity))


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
