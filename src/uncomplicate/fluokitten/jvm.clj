(ns ^{:doc "Extends Clojure core with the implementations of
fluokitten protocols. Defines curried functions. Need to be
used or required for enabling Fluokitten on projects that
run on JVM platform."
      :author "Dragan Djuric"}
  uncomplicate.fluokitten.jvm
  (:use [uncomplicate.fluokitten protocols algo utils]))

(set! *warn-on-reflection* true)

;;======== Set appropriate platform specific vars in algo. ======
(ns uncomplicate.fluokitten.utils)

(defn deref?
  "Checks whether x is dereferencible. On JVM it checks if it is
   an instance of clojure.lang.IDeref, on other platforms it may
   be implemented in a similar or a completely different way."
  [x]
  (instance? clojure.lang.IDeref x))

(ns uncomplicate.fluokitten.algo)

(defn create-mapentry
  "Creates a map entry with the supplied key and value. On JVM it
   creates an instance of clojure.lang.MapEntry."
  [k v]
  (clojure.lang.MapEntry. k v))

(ns uncomplicate.fluokitten.jvm)

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
         (CurriedFn.
          (fn
            ([x]
               (apply (cg x) (cf x) (map #(% x) hs)))
            ([x & xs]
               (apply (apply cg x xs) (apply cf x xs)
                      (map #(apply % x xs) hs))))
          1))
     Monad
     ~'(bind [cf cg]
         (CurriedFn.
          (fn
            ([x]
               (with-context cf
                 ((cg (cf x)) x)))
            ([x & xs]
               (with-context cf
                 (apply (cg (apply cf x xs)) x xs))))
          1))
     ~'(bind [cf cg hs]
         (CurriedFn.
          (fn
            ([x]
               (with-context cf
                 ((apply cg (cf x) (map #(% x) hs)) x)))
            ([x & xs]
               (with-context cf
                 (apply (apply cg (apply cf x xs)
                               (map #(apply % x xs) hs))
                        x xs))))
          1))
     ~'(join [cf] (bind cf identity))
     Foldable
     ~'(fold [_] f)
     ~'(foldmap [_ g] (g f))
     Magma
     ~'(op [x y]
         (if (= identity f)
           y
           (if (= identity (fold y))
             x
             (fmap x y))))
     ~'(op [x y ys]
         (reduce op x (cons y ys)))
     Monoid
     ~'(id [_] cidentity)))

(deftype-curried-fn)

(def ^{:doc "Checks whether an object is a curried function."}
  curried? (partial instance? CurriedFn))

(defn curry
  "Creates an automatically curried version of the function f.
   If arity is supplied, the function will be automatically
   curried when called with less arguments. If arity is not
   supplied, the default arity will depend on the arity of f.
   arity defaults to 2 if f can support it, otherwise it is
   1.

   ---- Example: currying +
   (((curry +) 3) 5)
   => 8

   ((((curry + 3) 3) 5) 7)
   => 15

   ((curry +) 3 5 7)
   => 15
  "
  ([f] (curry f (min 2 (apply max (arg-counts f)))))
  ([f arity]
     (if (and (fn? f) (pos? arity))
       (->CurriedFn f arity)
       f)))

(def curried (CurriedFn. identity 1))
(def cidentity curried)
