(ns ^{:doc "Extends Clojure core with the implementations of
fluokitten protocols. Defines curried functions. Need to be
used or required for enabling Fluokitten on projects that
run on JVM platform."
      :author "Dragan Djuric"}
    uncomplicate.fluokitten.jvm
  (:require [uncomplicate.fluokitten
             [protocols :refer :all]
             [algo :refer :all]
             [utils :refer [split-last with-context]]]))

(set! *warn-on-reflection* true)

;;======== Set appropriate platform specific vars in algo. ======

(in-ns 'uncomplicate.fluokitten.utils)

(defn deref?
  "Checks whether x is dereferencible. On JVM it checks if it is
   an instance of clojure.lang.IDeref, on other platforms it may
   be implemented in a similar or a completely different way."
  [x]
  (clojure.core/instance? clojure.lang.IDeref x))

(in-ns 'uncomplicate.fluokitten.algo)

(defn create-mapentry
  "Creates a map entry with the supplied key and value. On JVM it
   creates an instance of clojure.lang.MapEntry."
  [k v]
  (clojure.lang.MapEntry. k v))

(in-ns 'uncomplicate.fluokitten.jvm)

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

(defn ^:private gen-invoke [^clojure.lang.IFn f arity n]
  (let [args (map #(symbol (str "a" %)) (range arity))]
    `(invoke [~'_ ~@args]
             (if (> ~n ~arity)
               (CurriedFn. (partial ~f ~@args) (- ~n ~arity))
               (.invoke ~f ~@args)))))

(defn ^:private gen-applyto [^clojure.lang.IFn f n]
  `(applyTo [~'_ ~'args]
            (let [as# (- ~n (count ~'args))]
              (if (pos? as#)
                (CurriedFn. (apply partial ~f ~'args) as#)
                (.applyTo ~f ~'args)))))

(defmacro ^:private deftype-curried-fn []
  `(deftype ~'CurriedFn ~'[^clojure.lang.IFn f ^long n]
     java.lang.Object
     ~'(hashCode [_]
         (clojure.lang.Util/hashCombine f (Long/hashCode n)))
     ~'(equals [x y]
         (or
          (identical? x y)
          (and (instance? CurriedFn y)
               (= n (.n ^CurriedFn  y)) (identical? f (.f ^CurriedFn y)))))
     ~'(toString [_]
         (format "#curried-function[arity: %d, %s]" n f))
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

(defmethod print-method CurriedFn
  [cf ^java.io.Writer w]
  (.write w (str cf)))

(defn curried-arity [^CurriedFn cf]
  (.n cf))

(defn curried-curry
  ([cf]
   cf)
  ([^CurriedFn cf ^long arity]
   (if (pos? arity)
     (->CurriedFn (.f cf) arity)
     cf)))

(defn curried-uncurry [^CurriedFn cf]
  (.f cf))

(defn curried-op
  ([x y]
   (if (identical? identity y)
     x
     (->CurriedFn (comp x y) (arity y))))
  ([x y z]
   (if (identical? identity z)
     (curried-op x y)
     (curried-op (function-op x y) z)))
  ([x y z w]
   (curried-op (function-op x y z) w))
  ([x y z w ws]
   (let [[f fs] (split-last ws)]
     (curried-op (function-op x y z w fs) f))))

(defn curried-fmap
  ([cf g]
   (curried-op g cf))
  ([cf g chs]
   (->CurriedFn (function-fmap cf g chs) (apply max (arity cf) (map arity chs)))))

(defn curried-fapply
  ([cf cg]
   (->CurriedFn (function-fapply cf cg) 1))
  ([cf cg chs]
   (->CurriedFn (function-fapply cf cg chs) 1)))

(extend CurriedFn
  Curry
  {:arity curried-arity
   :curry curried-curry
   :uncurry curried-uncurry}
  Functor
  {:fmap curried-fmap}
  Applicative
  {:fapply curried-fapply
   :pure function-pure}
  Monad
  {:join function-join
   :bind function-bind}
  Foldable
  {:fold function-fold
   :foldmap default-foldmap}
  Magma
  {:op curried-op}
  Monoid
  {:id (constantly identity)})

;; ====================== Functions as Curry =======================
(defn fn-curry
  ([f]
   (curry f (min 2 (long (apply max (arg-counts f))))))
  ([f ^long arity]
   (if (pos? arity)
     (->CurriedFn f arity)
     f)))

(extend clojure.lang.IFn
  Curry
  {:arity (constantly 0)
   :curry fn-curry
   :uncurry identity})

;; ====================== Maybe Just ==============================

(deftype Just [v]
  Object
  (hashCode [_]
    (hash v))
  (equals [this that]
    (or (identical? this that)
        (and (instance? Just that)
             (= v (value that)))))
  (toString [_]
    (format "#just[%s]" v))
  Maybe
  (value [_]
    v))

(defmethod print-method Just
  [x ^java.io.Writer w]
  (.write w (str x)))

(defn just-pure [x v]
  (Just. v))

(extend-just Just just-pure)

(defn nil-fapply
  ([_ _] nil)
  ([_ _ _] nil))

(extend nil
  Applicative
  {:pure just-pure
   :fapply nil-fapply})
