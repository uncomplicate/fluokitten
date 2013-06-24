(ns uncomplicate.fluokitten.test
  (:use [uncomplicate.fluokitten core algo])
  (:use [midje.sweet :exclude [just]]))

(defn check-eq
  ([expected]
     (fn [actual]
       (check-eq expected actual)))
  ([expected actual]
     (cond
      (nil? expected)
      (nil? actual)
      (and (instance? (type expected) actual)
           (deref? expected) (deref? actual))
      (let [e (deref expected)
            a (deref actual)]
        (check-eq e a))
      (reducible? expected)
      (= (into [] expected) (into [] actual))
      :else (= expected actual))))

;;=============== Functor tests ========================
(defmacro functor-law2
  ([f x] `(functor-law2 ~f ~f ~x))
  ([f1 f2 x & xs]
     `(facts "Second functor law."
             (fmap (comp ~f1 ~f2) ~x ~@xs) =>
             (check-eq (fmap ~f1 (fmap ~f2 ~x ~@xs))))))

(defmacro fmap-keeps-type [f x & xs]
  `(fact "fmap should return data of the same type
            as the functor argument."
         (type (fmap ~f ~x ~@xs)) => #(isa? % (type ~x))))

;;================ Applicative tests ==========================
(defmacro applicative-law1 [f x & xs]
  `(fact "First applicative law."
        (fapply (pure ~x ~f) ~x ~@xs)
        => (check-eq (fmap ~f ~x ~@xs))))

(defmacro applicative-law2-identity [x]
  `(fact "Identity applicative law."
         (fapply (pure ~x identity) ~x)
         => (check-eq ~x)))

(defmacro applicative-law3-composition [u v x & xs]
  `(fact "Composition applicative law."
         (-> (pure ~x #(partial comp %))
             (fapply ~u) (fapply ~v) (fapply ~x ~@xs))
         => (check-eq (fapply ~u (fapply ~v ~x ~@xs)))))

(defmacro applicative-law4-homomorphism [ap f x & xs]
  `(fact "Homomorphism applicative law."
         (apply fapply (pure ~ap ~f) (pure ~ap ~x)
                (map (pure ~ap) '~xs))
         => (check-eq (pure ~ap (~f ~x ~@xs)))))

(defmacro applicative-law5-interchange [ap f x & xs]
  `(fact "Interchange applicative law."
         (apply fapply (pure ~ap ~f) (pure ~ap ~x)
                (map (pure ~ap) '~xs))
         => (check-eq (fapply (pure ~ap #(% ~x ~@xs))
                 (pure ~ap ~f)))))

(defmacro fapply-keeps-type [f x & xs]
  `(fact "fapply should return data of the same type
            as the applicative argument."
         (type (fapply (pure ~x ~f) ~x ~@xs))
         => (type ~x)))

;;=============== Monad tests ============================
(defmacro monad-law1-left-identity [m g x & xs]
  `(fact "Left Identity Monad Law"
         (apply bind (pure ~m ~x)
                (conj (vec (map (pure ~m) '~xs))
                      ~g))
         => (check-eq (~g ~x ~@xs))))

(defmacro monad-law2-right-identity [m]
  `(fact "Right Identity Monad Law"
         (bind ~m (pure ~m))
         => (check-eq ~m)))

(defmacro monad-law3-associativity [f g m & ms]
  `(fact "Associativity Monad Law"
         (bind (bind ~m ~@ms ~f) ~g)
         => (check-eq (bind ~m ~@ms
                            (fn [& xs#]
                              (bind (apply ~f xs#) ~g))))))

;;============= Magmas, Semigroups and Monoids =======================
(defmacro magma-op-keeps-type [x y & ys]
  `(fact "Magma - op should keep the type."
         (type (op ~x ~y ~@ys))
         => #(isa? % (type ~x))))

(defmacro semigroup-op-associativity [x y & ys]
  `(fact "Semigroup - op associativity."
         (op ~x ~y ~@ys)
         => (check-eq
             (reduce #(op %2 %1)
                     (reverse (list ~x ~y ~@ys))))))

(defmacro monoid-identity-law [x & xs]
  `(fact "Monoid law 1."
         (op (id ~x) ~x) => (check-eq ~x)
         (op ~x (id ~x)) => (check-eq ~x)))

;;=============== Metadata ==========================
(defmacro data-structures-should-preserve-metadata
  [f1 f2 builder x y]
  `(facts  "All data structures should preserve metadata."
           (meta (fmap ~f1 (with-meta ~x
                            {:test true})))
           => {:test true}

           (meta (fmap ~f2 (with-meta ~x
                            {:test true}) ~y))
           => {:test true}

           (meta (fmap ~f1 (with-meta (empty ~x)
                            {:test true})))
           => {:test true}

           (meta (fapply (pure ~x ~f1)
                         (with-meta ~x
                           {:test true})))
           => {:test true}

           (meta (fapply (pure ~x ~f2)
                         (with-meta ~x
                           {:test true}) ~y))
           => {:test true}

           (meta (fapply (pure ~x ~f1)
                         (with-meta (empty ~x)
                           {:test true})))
           => {:test true}

           (meta (bind (with-meta ~x {:test true})
                      #(~builder (~f1 %))))
           => {:test true}

           (meta (bind (with-meta ~x
                         {:test true})
                       ~y
                       #(~builder (~f2 %1 %2))))
           => {:test true}

           (meta (bind (with-meta (empty ~x)
                           {:test true})
                         #(~builder (~f1 %))))
           => {:test true}

           (meta (join (with-meta ~x
                         {:test true})))
           => {:test true}

           (meta (join (with-meta (empty ~x)
                         {:test true})))
           => {:test true}))
