(ns org.uncomplicate.redcat.jvm
  (:use [org.uncomplicate.redcat protocols algo]))

(set! *warn-on-reflection* true)

;;======== Set appropriate platform specific vars in algo. ======
(ns org.uncomplicate.redcat.algo)

(defn deref? [x]
  (instance? clojure.lang.IDeref x))

(defn create-mapentry [k v]
  (clojure.lang.MapEntry. k v))

(ns org.uncomplicate.redcat.jvm)

;;======== Clojure JVM-specific Extensions =====================
(extend-collection clojure.lang.IPersistentCollection)

(extend-seq clojure.lang.ASeq)

(extend-lazyseq clojure.lang.LazySeq)

(extend-vector clojure.lang.APersistentVector)

(extend-list clojure.lang.IPersistentList)

(extend-list clojure.lang.PersistentList)

(extend-set clojure.lang.APersistentSet)

(extend-map clojure.lang.APersistentMap)

(extend-mapentry clojure.lang.MapEntry)

(extend-keyword clojure.lang.Keyword)

(extend-function clojure.lang.AFunction)

(extend-atom clojure.lang.Atom)

(extend-ref clojure.lang.Ref)

;;===================== CurriedFn ===========================
(defn ^:private arg-counts [f]
  (map alength (map (fn [^java.lang.reflect.Method m]
                      ( .getParameterTypes m))
                    (.getDeclaredMethods
                     ^java.lang.Class (class f)))))

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

(def ^:private curriedfn-monoidf
  (partial monoidf* cidentity))
