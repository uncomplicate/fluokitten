(ns org.uncomplicate.redcat.core-test
  (:use org.uncomplicate.redcat.jvm
        org.uncomplicate.redcat.core
        midje.sweet)
  (:require [clojure.string :as s]))

(defn scaffold [iface]
  (doseq [[iface methods] (->> iface .getMethods
                            (map #(vector (.getName (.getDeclaringClass %))
                                    (symbol (.getName %))
                                    (count (.getParameterTypes %))))
                            (group-by first))]
    (println (str "  " iface))
    (doseq [[_ name argcount] methods]
      (println
        (str "    "
          (list name (into ['this] (take argcount (repeatedly gensym)))))))))

(defn check-eq [expected]
  (cond
   (deref? expected)
   (let [exp (deref expected)]
     #(and (instance? (type expected) %)
           (= exp (deref %))))
   (reducible? expected)
   #(= (into [] expected) (into [] %))
   :else #(= expected %)))

;;=============== Curried tests ========================
(facts
 ((curry + 3) 1 2 3) => 6
 (((curry + 2) 1) 2) => 3)

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

(fact "First functor law."
      (fmap identity) => identity)

;;--------------- nil ---------------
(functor-law2 inc + nil)

(functor-law2 inc + nil 99 0)

;;--------------- literals ---------------
;; Functor operations on any Object, if more particular
;; implementation is not specified.
;; The type of the result does not have to be the same
;; as the type of the input object. The result type
;; depends on the function that is being applied.
;; However, keeping the type is not required by any
;; Functor law, so keeping the type is convenient
;; but not mandatory.

(functor-law2 inc (partial * 100) 101)

(functor-law2 inc (partial * 100) 101 2 3)

(functor-law2 s/capitalize str \a)

(functor-law2 s/capitalize str \a \b \c)

(facts "Objects as functors."
       (fmap identity 1) => 1
       (fmap inc 1) => 2
       (fmap str 1) => "1"
       (fmap str \a) => "a"
       (fmap  + \a) => (throws ClassCastException))

;;--------------- String ---------------
(functor-law2 s/capitalize s/reverse "something")

(functor-law2 s/capitalize str "something" "else")

(fmap-keeps-type s/reverse "something")

(fmap-keeps-type str "something" "else")

;;--------------- Vector ---------------
(functor-law2 inc (partial * 100)
              [1 -199 9])

(functor-law2 inc (partial * 100)
              [1 -199 9]
              [1 -66 9])

(fmap-keeps-type inc [100 0 -999])

(fmap-keeps-type + [100 0 -999]
                 (list 44 0 -54))

;;--------------- List ---------------
(functor-law2 inc (partial * 100)
              (list -3 5 0))

(functor-law2 inc (partial * 100)
              (list -3 5 0)
              (list -3 44 -2))

(fmap-keeps-type inc (list 77 0 -39))

(fmap-keeps-type + (list 77 0 -39)
                 (list 88 -8 8))

;;--------------- Set ---------------
(functor-law2 inc (partial * 100)
              #{-449 9 6})

(functor-law2 inc (partial * 100)
              #{-449 9 6}
              #{-4 88 7})

(fmap-keeps-type inc #{5 89 -7})

(fmap-keeps-type + #{5 89 -7}
                 (list -3 -4 -45))

;;--------------- Seq ---------------
;; Due to Clojure implementation details,
;; the type of the result does not have to be the same
;; as the type of the input seq.
;; However, keeping the type is not mandated by any Functor law.
;; Both seqs implement the seq interface, though.

(functor-law2 inc (partial * 100)
              (seq (list 8 9 10)))

(functor-law2 inc (partial * 100)
              (seq (list 8 9 10))
              (seq (list -5 -4 -5)))

;;--------------- LazySeq ---------------
(functor-law2 inc (partial * 100)
              (lazy-seq (list 78 -3 5)))

(functor-law2 inc (partial * 100)
              (lazy-seq (list 78 -3 5))
              (lazy-seq (list 88 0 -4)))

(fmap-keeps-type inc (lazy-seq (list 8 9 -2)))

(fmap-keeps-type + (lazy-seq (list 8 9 -2))
                 (lazy-seq (list 18 5 -92)))


;;--------------- CollReduce ---------------
(functor-law2 inc (partial * 100)
              (reducible [1 -199 9]))

(fmap-keeps-type inc (reducible [100 0 -999]))

;;--------------- MapEntry ---------------
(functor-law2 inc (partial * 100)
              (first {:a 11}))

(functor-law2 inc (partial * 100)
              (first {:a 11})
              (first {:a 5}))

(fmap-keeps-type inc (first {:c 3}))

(fmap-keeps-type + (first {:c 3})
                 (first {:d 5}))

;;--------------- Map ---------------
(fact
 (fmap (comp inc +) {:a 1} {:a 2} {:a 3})
 => {:a 7})

(fact
 (fmap (fn [& xs]
         (hash-map :sum (apply + xs)))
       {:a 1 :b 2} {:a 4})
 => {:a {:sum 5}, :b {:sum 2}})

(functor-law2 inc (partial * 100)
              {:a 2 :t 5 :h 99})
(functor-law2 inc +
              {:a 2 :t 5 :h 99}
              {:a 7 :t 8 :h 49}
              {:a 8 :t 3 :h 59})

(fmap-keeps-type inc {:a 2 :t 5 :h 99})

(fmap-keeps-type + {:a 2 :t 5 :h 99}
                 {:k 5 :n 4 :dd 5})

;;--------------- Atom ---------------
(functor-law2 inc (partial * 100) (atom 34))

(functor-law2 inc (partial * 100) (atom 35) (atom 5))

(fmap-keeps-type  inc (atom 36))

(fmap-keeps-type  + (atom 37) (atom 5))

;;--------------- Ref ---------------
(dosync
 (functor-law2 inc (partial * 100) (ref 44)))

(dosync
 (functor-law2 inc (partial * 100) (ref 45) (atom 3) (ref 7)))

(dosync
 (fmap-keeps-type inc (ref 46)))

(dosync
 (fmap-keeps-type + (ref 47) (ref 4) (atom 7)))

;;--------------- Function ---------------
(fact "Second functor law."
      ((fmap (comp inc dec) inc) 1)
      => ((fmap inc (fmap dec inc))1))

(fact "Second functor law - varargs."
      ((fmap (comp inc dec) +) 1 2 3)
      => ((fmap inc (fmap dec +)) 1 2 3))

(fact "Fmap keeps type." (fn? (fmap inc dec)))

(fact "Fmap keeps type - varargs." (fn? (fmap inc dec +)))

;============================= Applicative tests ==========================
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
         ;;TODO This could be more idiomatic with vararg fapply
         (-> (pure ~x #(partial comp %))
             (fapply ~u) (fapply ~v) (fapply ~x ~@xs))
         => (check-eq (fapply ~u (fapply ~v ~x ~@xs)))))

(defmacro applicative-law4-homomorphism [ap f x & xs]
  `(fact "Homomorphism applicative law."
         (apply fapply (pure ~ap ~f) (pure ~ap ~x)
                (map (partial pure ~ap) '~xs))
         => (check-eq (pure ~ap (~f ~x ~@xs)))))

(defmacro applicative-law5-interchange [ap f x & xs]
  `(fact "Interchange applicative law."
         (apply fapply (pure ~ap ~f) (pure ~ap ~x)
                (map (partial pure ~ap) '~xs))
         => (check-eq (fapply (pure ~ap #(% ~x ~@xs))
                 (pure ~ap ~f)))))

(defmacro fapply-keeps-type [f x & xs]
  `(fact "fapply should return data of the same type
            as the applicative argument."
         (type (fapply (pure ~x ~f) ~x ~@xs))
         => (type ~x)))

;--------------- Vector ---------------
(applicative-law1 inc [1 -2 5])

(applicative-law1 + [1 -2 5] [8 98 0])

(applicative-law2-identity [1 445 -4])

(applicative-law3-composition [inc]
                              [(partial * 10)]
                              [1 -34343444])

(applicative-law3-composition [inc]
                              [(partial * 10)]
                              [1 -34343444]
                              (list 1 78))

(applicative-law4-homomorphism [] inc 1)
(applicative-law4-homomorphism [] + 1 55 6)

(applicative-law5-interchange [] inc 1)
(applicative-law5-interchange [] + 1 54 -2)

(fapply-keeps-type inc  [1 -4 9])
(fapply-keeps-type + [1 -4 9] [2 -3 -4])

;--------------- CollReduce ---------------
(applicative-law1 inc (reducible [1 -2 5]))

(applicative-law2-identity (reducible [1 445 -4]))

(applicative-law3-composition [inc]
                              [(partial * 10)]
                              (reducible [1 -34343444]))

(applicative-law4-homomorphism (reducible []) inc 1)

(applicative-law5-interchange (reducible []) inc 1)

(fapply-keeps-type inc (reducible [1 -4 9]))

;;--------------- List ---------------
(applicative-law1 inc (list 2 44 -7))

(applicative-law1 + (list 2 44 -7) (list 3 5 -3))

(applicative-law2-identity (list 2 -5 99))

(applicative-law3-composition (list inc)
                              (list (partial * 10))
                              (list 2 8 -33))

(applicative-law3-composition (list inc)
                              (list (partial * 10))
                              (list 2 8 -33)
                              (list -4 -5 -5))

(applicative-law4-homomorphism (list) inc 2)

(applicative-law4-homomorphism (list) + 2 -3 -44)

(applicative-law5-interchange (list) inc 2)

(applicative-law5-interchange (list) + 2 9 -21)

(fapply-keeps-type inc (list 2 -4 9))

(fapply-keeps-type + (list 2 -4 9) (list 9 -8))

;;-------------- Seq ----------------
(applicative-law1 inc (seq (list 3 9 0)))

(applicative-law1 + (seq (list 3 9 0))
                  (seq (list -5 4 6)))

(applicative-law2-identity (seq (list 3 -79 29)))

(applicative-law3-composition (seq (list inc))
                              (seq (list (partial * 10)))
                              (seq (list 3 -1 0)))

(applicative-law3-composition (seq (list inc))
                              (seq (list (partial * 10)))
                              (seq (list 3 -1 0))
                              (seq (list -5 3 4)))

(applicative-law4-homomorphism (seq (list 3)) inc 3)

(applicative-law4-homomorphism (seq (list 3)) + 3 0 5 5)

(applicative-law5-interchange (seq (list 3)) inc 3)

(applicative-law5-interchange (seq (list 3)) + 3 -8 -87)

(fapply-keeps-type inc (seq (list 3 -4 9)))

(fapply-keeps-type + (seq (list 3 -4 9))
                (seq (list -5 -5 -3)))

;;-------------- Lazy Seq ----------------
(applicative-law1 inc (lazy-seq (list 3 9 0)))

(applicative-law1 + (lazy-seq (list 3 9 0))
                  (lazy-seq (list -4 -5)))

(applicative-law2-identity (lazy-seq (list 3 -79 29)))

(applicative-law3-composition (lazy-seq (list inc))
                              (lazy-seq (list (partial * 10)))
                              (lazy-seq (list 3 -1 0)))

(applicative-law3-composition (lazy-seq (list inc))
                              (lazy-seq (list (partial * 10)))
                              (lazy-seq (list 3 -1 0))
                              (lazy-seq (list 9)))

(applicative-law4-homomorphism (lazy-seq (list 3)) inc 3)

(applicative-law4-homomorphism (lazy-seq (list 3)) + 3 -2)

(applicative-law5-interchange (lazy-seq (list 3)) inc 3)

(applicative-law5-interchange (lazy-seq (list 3)) + 3 -2 -3)

(fapply-keeps-type inc (lazy-seq (list 3 -4 9)))

(fapply-keeps-type + (lazy-seq (list 3 -4 9))
                (lazy-seq (list -5 -5 -5)))

;;-------------- Set ----------------
(applicative-law1 inc #{4 2 -44})

(applicative-law1 + #{4 2 -44}
                  #{0 -8})

(applicative-law2-identity #{4 40 -1})

(applicative-law3-composition #{inc}
                              #{(partial * 10)}
                              #{4 -5 -6})

(applicative-law3-composition #{inc}
                              #{(partial * 10)}
                              #{4 -5 -6}
                              #{0 -4})

(applicative-law4-homomorphism #{} inc 4)

(applicative-law4-homomorphism #{} + 4 0 -98)

(applicative-law5-interchange #{} inc 4)

(applicative-law5-interchange #{} + 4 -8)

(fapply-keeps-type inc #{4 -2 5})

(fapply-keeps-type + #{4 -2 5}
                #{-9 -7})

;;-------------- MapEntry -----------
(applicative-law1 inc (first {5 5}))

(applicative-law1 + (first {5 5})
                  (first {5 6}))

(applicative-law2-identity (first {5 5}))

(applicative-law3-composition (first {58 inc})
                              (first {57 (partial * 10)})
                              (first {5 5}))

(applicative-law3-composition (first {58 inc})
                              (first {57 (partial * 10)})
                              (first {5 5})
                              (first {5 6}))

(applicative-law4-homomorphism (first {57 57}) inc 5)

(applicative-law4-homomorphism (first {57 57}) + 5 -4 5)

(applicative-law5-interchange (first {58 58}) inc 5)

(applicative-law5-interchange (first {58 58}) + 5 3 4 5)

;;-------------- Map ----------------
(fact (pure {} 2) => {nil 2})

(fact (fapply {:a inc :b dec nil (partial * 2)} {:a 1 :b 5})
      => {:a 2 :b 4})

(fact (fapply {:a inc :b dec} {:a 1 :c 2})
      => {:a 2 :c 2})

(fact (fapply {:a + :b - nil (partial * 2)}
           {:a 1 :b 1}
           {:a 2 :b 3 :c 44}
           {:b 4})
      => {:a 3 :b -6 :c 88})

(fact (fapply {:a + :b - :d (partial * 2)}
           {:a 1 :b 1}
           {:a 2 :b 3 :c 44}
           {:b 4})
      => {:a 3 :b -6 :c 44})

(fact (fapply {nil +}
              {:a 1 :b 3}
              {:a 2 :b 4}
              {:a 3 :b 5})
      => {:a 6 :b 12})

(applicative-law1 inc {nil 6})

(applicative-law1 inc {6 6})

(applicative-law1 + {nil 6} {2 23})

(applicative-law2-identity {nil 6})

(applicative-law2-identity {6 6})

(applicative-law3-composition {6 inc}
                              {6 (partial * 10)}
                              {6 6 -5 -6})

(applicative-law3-composition {6 inc}
                              {6 (partial * 10)}
                              {6 6 -5 -6}
                              {6 8 -5 -2})

(applicative-law3-composition {nil inc}
                              {nil (partial * 10)}
                              {6 6 -5 -6})

(applicative-law4-homomorphism {} inc 4)

(applicative-law4-homomorphism {} + 4 7 9)

(applicative-law5-interchange {} inc 4)

(applicative-law5-interchange {} + 4 8 9)

(fapply-keeps-type inc {4 -2 5 5})

;;-------------- Atom -----------
(applicative-law1 inc (atom 6))

(applicative-law1 + (atom 6) (atom 9) (atom -77) (atom -1))

(applicative-law2-identity (atom 6))

(applicative-law3-composition (atom inc)
                              (atom (partial * 10))
                              (atom 6))

(applicative-law3-composition (atom inc)
                              (atom (partial * 10))
                              (atom 6)
                              (atom -2))

(applicative-law4-homomorphism (atom 6) inc 5)

(applicative-law4-homomorphism (atom 6) + 5 -4 5)

(applicative-law5-interchange (atom 6) inc 5)

(applicative-law5-interchange (atom 6) + 5 3 4 5)

;;-------------- Ref -----------
(dosync
 (applicative-law1 inc (ref 6)))

(dosync
 (applicative-law1 + (ref 6) (ref 9) (ref -77) (ref -1)))

(dosync
 (applicative-law2-identity (ref 6)))

(dosync
 (applicative-law3-composition (ref inc)
                               (ref (partial * 10))
                               (ref 6)))

(dosync
 (applicative-law3-composition (ref inc)
                               (ref (partial * 10))
                               (ref 6)
                               (ref -2)))

(dosync
   (applicative-law4-homomorphism (ref 6) inc 5))

(dosync
 (applicative-law4-homomorphism (ref 6) + 5 -4 5))

(dosync
   (applicative-law5-interchange (ref 6) inc 5))

(dosync
   (applicative-law5-interchange (ref 6) + 5 3 4 5))

;;------------------- Function ---------------------------
(facts "First applicative law."
       ((fapply (pure identity inc) (pure identity 1)) 7)
       => ((fmap inc (pure identity 1)) 7)
      ;; ((fapply (pure identity +) (pure identity 1)) 7 9 11 13)
      ;; => ((fmap + (pure identity 1)) 7 9 11 13)
       )

(facts "Learn You a Haskell example."
       ((fapply (fmap + (partial + 3)) (partial * 100)) 5) => 508
       ;;((fapply (fmap + (partial + 3)) (partial * 100)) 2 3 4) => 2412
       ((fapply (fapply (pure identity +) (partial + 3)) (partial * 100)) 6) => 609
       ;;((fapply (fmap + (partial + 3)) (partial * 100)) 5 1) => 509
       ;;((fapply (fmap + (partial + 3)) (partial * 100)) 5 1 2) => 1011
       )

;;=============== Monad tests ============================

(defmacro monad-law1-left-identity [m g x & xs]
  `(fact "Left Identity Monad Law"
         (apply bind (pure ~m ~x) ~g
                (map (partial pure ~m) '~xs))
         => (check-eq (~g ~x ~@xs))))

(defmacro monad-law2-right-identity [m]
  `(fact "Right Identity Monad Law"
         (bind ~m (partial pure ~m))
         => (check-eq ~m)))

(defmacro monad-law3-associativity [f g m & ms]
  `(fact "Associativity Monad Law"
         (-> (bind ~m ~f ~@ms) (bind ~g))
         => (check-eq (bind ~m (fn [& xs#]
                                 (bind (apply ~f xs#) ~g))
                            ~@ms))))

;;--------------- Vector ---------------------------------
(monad-law1-left-identity [] (comp vector inc) 1)

(monad-law1-left-identity [] (comp vector +) 1 2 3)

(monad-law2-right-identity [1 2 -33])

(monad-law3-associativity (comp vector inc)
                          (comp vector (partial * 10))
                          [1 -3 -88])

;;--------------- CollReduce ---------------------------------
(monad-law1-left-identity (reducible [])
                          (comp reducible vector inc) 1)

(monad-law2-right-identity (reducible [1 2 -33]))

(monad-law3-associativity (comp reducible vector inc)
                          (comp reducible vector (partial * 10))
                          (reducible [1 -3 -88]))

;;--------------- List -----------------------------------
(monad-law1-left-identity (list) (comp list inc) 2)

(monad-law1-left-identity (list) (comp list +) 2 3 4)

(monad-law2-right-identity (list 2 4 -33))

(monad-law3-associativity (comp list inc)
                          (comp list (partial * 10))
                          (list 2 -3 -88))

;;--------------- LazySeq -----------------------------------
(monad-law1-left-identity (lazy-seq (list))
                          (comp list inc) 3)

(monad-law1-left-identity (lazy-seq (list))
                          (comp list +) 3 49 9)

(monad-law2-right-identity (lazy-seq (list 3 2 -33)))

(monad-law3-associativity (comp list inc)
                          (comp list (partial * 10))
                          (lazy-seq (list 3 -3 -88)))

;;--------------- Seq -----------------------------------
(monad-law1-left-identity (conj (seq [0]) 2)
                          (comp seq vector inc) 4)

(monad-law1-left-identity (conj (seq [0]) 2)
                          (comp seq vector +) 4 8 9)

(monad-law2-right-identity (conj (seq [4 2 -33]) 2))

(monad-law3-associativity (comp seq vector inc)
                          (comp seq vector (partial * 10))
                          (conj (seq [4 -3 -88]) 2))

;;--------------- Set ---------------------------------
(monad-law1-left-identity #{} (comp hash-set inc) 6)

(monad-law1-left-identity #{} (comp hash-set +) 6 7 99)

(monad-law2-right-identity #{5 -3 24})

(monad-law3-associativity (comp hash-set inc)
                          (comp hash-set (partial * 10))
                          #{5 -56 30})

;;--------------- Map ---------------------------------
(facts
 (join {:a 1 :b 2}) => {:a 1 :b 2}
 (join {:a {:a1 1 :a2 5} :b {:b1 2}})
 => {[:a :a1] 1 [:a :a2] 5 [:b :b1] 2})

(monad-law1-left-identity {} #(hash-map :increment (inc %)) 5)

(monad-law1-left-identity {}
                          (fn [& xs]
                            (hash-map :sum (apply + xs)))
                          5 6 88 9)

(monad-law2-right-identity {:a 1 :b 2})

(monad-law3-associativity #(hash-map :increment (inc %))
                          #(hash-map :10times (* 10 %))
                          {:a 1 :b 2})

(monad-law3-associativity (fn [& xs] (hash-map :sum (apply + xs)))
                          #(hash-map :10times (* 10 %))
                          {:a 1 :b 2 :c 3}
                          {:a 4 :b 3 :c 2}
                          {:a 12 :b 23 :c 9})

(fact (let [f #(hash-map :increment (inc %))
            m {:a 1 :b 2}]
        (fapply (pure {} f) m)
        => (bind (pure {} f) #(fmap % m))))

;;--------------- MapEntry ----------------------------
(facts
 (join (first {:a 1})) => (first {:a 1})
 (join (first {:a [:b 1]})) => (first {[:a :b] 1}))

(monad-law1-left-identity (first {:a 1})
                          #(first (hash-map :increment (inc %)))
                          5)

(monad-law1-left-identity (first {:a 1})
                          (fn [& xs] (first (hash-map :sum (apply + xs))))
                          5 77 8)

(monad-law2-right-identity (first {:a 4}))

(monad-law3-associativity #(first (hash-map :increment (inc %)))
                          #(first (hash-map :10times (* 10 %)))
                          (first {:a 1}))

(monad-law3-associativity (fn [& xs]
                            (first (hash-map :sum (apply + xs))))
                          #(first (hash-map :10times (* 10 %)))
                          (first {:a 1})
                          (first {:a 2})
                          (first {:a 77}))

;;--------------- Atom ----------------------------
(monad-law1-left-identity (atom 9) (comp atom inc) 1)

(monad-law1-left-identity (atom 9) (comp atom +) 1 2 3)

(monad-law2-right-identity (atom 9))

(monad-law3-associativity (comp atom inc)
                          (comp atom (partial * 10))
                          (atom 9))

;;--------------- Ref ----------------------------
(dosync
 (monad-law1-left-identity (ref 9) (comp ref inc) 1))

(dosync
 (monad-law1-left-identity (ref 9) (comp ref +) 1 2 3))

(dosync
 (monad-law2-right-identity (ref 9)))

(dosync
 (monad-law3-associativity (comp ref inc)
                           (comp ref (partial * 10))
                           (ref 9)))
