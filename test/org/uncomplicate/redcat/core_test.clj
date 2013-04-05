(ns org.uncomplicate.redcat.core-test
  (:use org.uncomplicate.redcat.jvm
        org.uncomplicate.redcat.core
        midje.sweet)
  (:require [clojure.string :as s]))

;=============== Functor tests ========================
(defmacro functor-law2
  ([f x] `(functor-law2 ~f ~f ~x))
  ([f1 f2 x & xs]
     `(facts "Second functor law."
             (fmap (comp ~f1 ~f2) ~x ~@xs) =>
             (fmap ~f1 (fmap ~f2 ~x ~@xs)))))

(defmacro fmap-keeps-type [f x & xs]
  `(fact "fmap should return data of the same type
            as the functor argument."
         (type (fmap ~f ~x ~@xs)) => (type ~x)))

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
(functor-law2 inc (partial * 100)
              {:a 2 :t 5 :h 99})
(functor-law2 inc (partial * 100)
              {:a 2 :t 5 :h 99}
              {:a 8 :t 3 :h 59})

(fmap-keeps-type inc {:a 2 :t 5 :h 99})
(fmap-keeps-type + {:a 2 :t 5 :h 99}
                 {:k 5 :n 4 :dd 5})

;;--------------- Atom ---------------
(let [a (atom 44)]
  (functor-law2 inc (partial * 100) a)
  (functor-law2 inc (partial * 100) a (atom 5))
  (fmap-keeps-type  inc a)
  (fmap-keeps-type  + a (atom 5)))

;;--------------- Ref ---------------
(let [r (ref 457)]
  (dosync
   (functor-law2 inc (partial * 100) r))
  (dosync
   (functor-law2 inc (partial * 100) r (ref 7)))
  (dosync
   (fmap-keeps-type inc r))
  (dosync
   (fmap-keeps-type + r (ref 4))))

;; TODO functions
;;not a proper test at all!! (functor-law2 (gen-fn (partial * 100) inc) (gen-fn (partial * 44) dec) (gen/int))

;============================= Applicative tests ==========================
(defmacro applicative-law1 [f x & xs]
  `(fact "First applicative law."
         (<*> (pure ~x ~f) ~x ~@xs)
         => (fmap ~f ~x ~@xs)))

(defmacro applicative-law2-identity [x]
  `(fact "Identity applicative law."
         (<*> (pure ~x identity) ~x) => ~x))

(defmacro applicative-law3-composition [u v x & xs]
  `(fact "Composition applicative law."
         ;;TODO This could be more idiomatic with vararg <*>
         (-> (pure ~x #(partial comp %))
             (<*> ~u) (<*> ~v) (<*> ~x ~@xs))
         => (<*> ~u (<*> ~v ~x ~@xs))))

(defmacro applicative-law4-homomorphism [ap f x & xs]
  `(fact "Homomorphism applicative law."
         (apply <*> (pure ~ap ~f) (pure ~ap ~x)
                (map (partial pure ~ap) '~xs))
         => (pure ~ap (~f ~x ~@xs))))

(defmacro applicative-law5-interchange [ap f x & xs]
  `(fact "Interchange applicative law."
         (apply <*> (pure ~ap ~f) (pure ~ap ~x)
                (map (partial pure ~ap) '~xs))
         => (<*> (pure ~ap #(% ~x ~@xs))
                 (pure ~ap ~f))))

(defmacro <*>-keeps-type [f x & xs]
  `(fact "<*> should return data of the same type
            as the applicative argument."
         (type (<*> (pure ~x ~f) ~x ~@xs))
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

(<*>-keeps-type inc (list 1 -4 9))
(<*>-keeps-type + (list 1 -4 9) (list 2 -3 -4))

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

(<*>-keeps-type inc (list 2 -4 9))

(<*>-keeps-type + (list 2 -4 9) (list 9 -8))

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

(<*>-keeps-type inc (seq (list 3 -4 9)))

(<*>-keeps-type + (seq (list 3 -4 9))
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

(<*>-keeps-type inc (lazy-seq (list 3 -4 9)))

(<*>-keeps-type + (lazy-seq (list 3 -4 9))
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

(<*>-keeps-type inc #{4 -2 5})

(<*>-keeps-type + #{4 -2 5}
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

(fact (<*> {:a inc :b dec nil (partial * 2)} {:a 1 :b 5})
      => {:a 4 :b 8})

(fact (<*> {:a + :b - nil (partial * 2)}
           {:a 1 :b 1}
           {:a 2 :b 3 :c 44}
           {:b 4})
      => {:a 6 :b -12 :c 88})

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

(<*>-keeps-type inc {4 -2 5 5})

;;=============== Monad tests ============================

(defmacro monad-law1-left-identity [m g x]
  `(fact "Left Identity Monad Law"
         (>>= (pure ~m ~x) ~g) => (~g ~x)))

(defmacro monad-law2-right-identity [m]
  `(fact "Right Identity Monad Law"
         (>>= ~m (partial pure ~m)) => ~m))

(defmacro monad-law3-associativity [f g m]
  `(fact "Associativity Monad Law"
         (-> (>>= ~m ~f) (>>= ~g))
         => (>>= ~m #(>>= (~f %) ~g))))

;;--------------- Vector ---------------------------------
(monad-law1-left-identity [] (comp vector inc) 1)

(monad-law2-right-identity [1 2 -33])

(monad-law3-associativity (comp vector inc)
                          (comp vector (partial * 10))
                          [1 -3 -88])

;;--------------- List -----------------------------------
(monad-law1-left-identity (list) (comp list inc) 2)

(monad-law2-right-identity (list 2 4 -33))

(monad-law3-associativity (comp list inc)
                          (comp list (partial * 10))
                          (list 2 -3 -88))

;;--------------- LazySeq -----------------------------------
(monad-law1-left-identity (lazy-seq (list))
                          (comp list inc) 3)

(monad-law2-right-identity (lazy-seq (list 3 2 -33)))

(monad-law3-associativity (comp list inc)
                          (comp list (partial * 10))
                          (lazy-seq (list 3 -3 -88)))

;;--------------- Seq -----------------------------------
(monad-law1-left-identity (conj (seq [0]) 2)
                          (comp seq vector inc) 4)

(monad-law2-right-identity (conj (seq [4 2 -33]) 2))

(monad-law3-associativity (comp seq vector inc)
                          (comp seq vector (partial * 10))
                          (conj (seq [4 -3 -88]) 2))

;;--------------- Set ---------------------------------
(monad-law1-left-identity #{} (comp hash-set inc) 6)

(monad-law2-right-identity #{5 -3 24})

(monad-law3-associativity (comp hash-set inc)
                          (comp hash-set (partial * 10))
                          #{5 -56 30})

;;--------------- Map ---------------------------------
(monad-law1-left-identity {} #(hash-map :increment (inc %)) 5)

(monad-law2-right-identity {:a 1 :b 2})

(monad-law3-associativity #(hash-map :increment (inc %))
                          #(hash-map :10times (* 10 %))
                          {:a 1 :b 2})

;;--------------- MapEntry ----------------------------
(monad-law1-left-identity (first {:a 1})
                          #(first (hash-map :increment (inc %)))
                          5)

(monad-law2-right-identity (first {:a 4}))

(monad-law3-associativity #(first (hash-map :increment (inc %)))
                          #(first (hash-map :10times (* 10 %)))
                          (first {:a 1}))
