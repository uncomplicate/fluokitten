(ns ^{:doc "Macros that generate Midje tests for various laws that
categorical concepts have to satisfy. You should use these tests
to check if your implementations of clojure protocols are valid
beyond what the compiler can assert."
      :author "Dragan Djuric"} uncomplicate.fluokitten.test
  (:use [uncomplicate.fluokitten core algo])
  (:use [midje.sweet :exclude [just]]))

(defn check-eq
  "Midje checker that check for the equality of contents in contexts such
   as references and reducibles."
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
  "Generates a test that checks if the functor x satisfies
   the Second Functor Law:

   (fmap (comp f g)) => (fmap f (fmap g))

   or, when applied to a concrete functor:

   (fmap (comp f g) x) => (fmap f (fmap g x))
  "
  ([f x] `(functor-law2 ~f ~f ~x))
  ([f g x & xs]
     `(facts "Second functor law."
             (fmap (comp ~f ~g) ~x ~@xs) =>
             (check-eq (fmap ~f (fmap ~g ~x ~@xs))))))

(defmacro fmap-keeps-type
  "Generates a test that checks if the functor x's implementation of
   fmap keeps the type of x when f is applied to its content."
  [f x & xs]
  `(fact "fmap should return data of the same type
            as the functor argument."
         (type (fmap ~f ~x ~@xs)) => #(isa? % (type ~x))))

;;================ Applicative tests ==========================
(defmacro applicative-law1
  "Generates a test that checks if the applicative functor x
   satisfies the first applicative law:

   (fapply (pure x f) x) => (fmap f x)
  "
  [f x & xs]
  `(fact "First applicative law."
        (fapply (pure ~x ~f) ~x ~@xs)
        => (check-eq (fmap ~f ~x ~@xs))))

(defmacro applicative-law2-identity
  "Generates a test that checks if the applicative functor x
   satisfies the second applicative law:

   (fapply (pure x identity) x) => x
  "
  [x]
  `(fact "Identity applicative law."
         (fapply (pure ~x identity) ~x)
         => (check-eq ~x)))

(defmacro applicative-law3-composition
  "Generates a test that checks if the applicative functor x
   satisfies the third applicative law:

   (<*> (pure x (curry comp)) u v x)
   => (fapply u (fapply v x))
  "
  [u v x & xs]
  `(fact "Composition applicative law."
         (-> (pure ~x #(partial comp %))
             (fapply ~u) (fapply ~v) (fapply ~x ~@xs))
         => (check-eq (fapply ~u (fapply ~v ~x ~@xs)))))

(defmacro applicative-law4-homomorphism
  "Generates a test that checks if the applicative functor x
   satisfies the fourth applicative law:

   (fapply (pure a f) (pure a x)) => (f x)
  "
  [ap f x & xs]
  `(fact "Homomorphism applicative law."
         (apply fapply (pure ~ap ~f) (pure ~ap ~x)
                (map (pure ~ap) '~xs))
         => (check-eq (pure ~ap (~f ~x ~@xs)))))

(defmacro applicative-law5-interchange
  "Generates a test that checks if the applicative functor x
   satisfies the fifth applicative law:

   (fapply u (pure a y)) => (fapply (pure a fn(% y)) u)
  "
  [ap f x & xs]
  `(fact "Interchange applicative law."
         (apply fapply (pure ~ap ~f) (pure ~ap ~x)
                (map (pure ~ap) '~xs))
         => (check-eq (fapply (pure ~ap #(% ~x ~@xs))
                 (pure ~ap ~f)))))

(defmacro fapply-keeps-type
  "Generates a test that checks if the applicative functor x's
   implementation of fapply keeps the type of x when the function
   inside f is applied to its content."
  [f x & xs]
  `(fact "fapply should return data of the same type
            as the applicative argument."
         (type (fapply (pure ~x ~f) ~x ~@xs))
         => (type ~x)))

;;=============== Monad tests ============================
(defmacro monad-law1-left-identity
  "Generates a test that checks if the monad x
   satisfies the first monad law:

   (bind (pure m x) f) => (g x)
  "
  [m g x & xs]
  `(fact "Left Identity Monad Law"
         (apply bind (pure ~m ~x)
                (conj (vec (map (pure ~m) '~xs))
                      ~g))
         => (check-eq (~g ~x ~@xs))))

(defmacro monad-law2-right-identity
  "Generates a test that checks if the monad x
   satisfies the secondmonad law:

   (bind m (pure m)) => m
  "
  [m]
  `(fact "Right Identity Monad Law"
         (bind ~m (pure ~m))
         => (check-eq ~m)))

(defmacro monad-law3-associativity
  "Generates a test that checks if the monad x
   satisfies the third monad law:

   (bind m (fn [x] (bind (f x) g)
  "
  [f g m & ms]
  `(fact "Associativity Monad Law"
         (bind (bind ~m ~@ms ~f) ~g)
         => (check-eq (bind ~m ~@ms
                            (fn [& xs#]
                              (bind (apply ~f xs#) ~g))))))

;;============= Magmas, Semigroups and Monoids =======================
(defmacro magma-op-keeps-type
  "Generates a test that checks if the operation op is closed on magma x."
  [x y & ys]
  `(fact "Magma - op should keep the type."
         (type (op ~x ~y ~@ys))
         => #(isa? % (type ~x))))

(defmacro semigroup-op-associativity
  "Generates a test that checks if x and op form a semigroup,
   i.e whether op is associative:

   (op (op a b)) => (op a (op b c))
  "
  [x y & ys]
  `(fact "Semigroup - op associativity."
         (op ~x ~y ~@ys)
         => (check-eq
             (reduce #(op %2 %1)
                     (reverse (list ~x ~y ~@ys))))))

(defmacro monoid-identity-law
  "Generates a test that checks whether the Monoid implementation x
   satisfies the monoid identity law, i.e. if identity element for op
   exists:
      (op x (id x)) => x
      (op (id x) x) => x
  "
  [x & xs]
  `(fact "Monoid law 1."
         (op (id ~x) ~x) => (check-eq ~x)
         (op ~x (id ~x)) => (check-eq ~x)))

;;=============== Metadata ==========================
(defmacro data-structures-should-preserve-metadata
  "Generates the tests that check whether the implementations
   of functions defined in Fluokitten protocols that extend
   Clojure core data structures preserve metadata."
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
