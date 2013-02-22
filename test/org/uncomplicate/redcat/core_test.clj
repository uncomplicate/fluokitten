(ns org.uncomplicate.redcat.core-test
  (:use org.uncomplicate.redcat.core
        org.uncomplicate.redcat.jvm
        midje.sweet)
  (:require [clojure.data.generators :as gen])
  (:require [clojure.string :as s]))

(defn gen-fn [& fs]
  (first (apply gen/one-of (map vector fs))))

;====================================== Functor tests ==========================================
;TODO test varargs functor!
(defmacro functor-law2
  ([fgen xgen] `(functor-law2 ~fgen ~fgen ~xgen))
  ([fgen1 fgen2 xgen]
    `(formula "Second functor law."
              [f1# ~fgen1
               f2# ~fgen2
               x# ~xgen]
              (= (fmap (comp f1# f2#) x#) (fmap f1# (fmap f2# x#))) => true?)))

(defmacro fmap-keeps-type [f xgen]
  `(formula "fmap should return data of the same type
            as the functor argument."
            [x# ~xgen]
            (type (fmap ~f x#)) => (type x#)))

(fact "First functor law."
      (fmap identity) => identity)
;nil as a functor
(functor-law2 (gen-fn inc dec) nil)

;Functor operations on any Object, if more particular implementation is not specified.
;The type of the result does not have to be the same as the type of the input object.
;The result type depends on the function that is being applied.
;However, keeping the type is not required by any Functor law.
(functor-law2
 (gen-fn inc (partial * 100))
 (gen/int))

(functor-law2 str (gen/char))

(tabular
  (fact "Plain Objects as functors."
        (fmap ?f ?o) ?arrow ?expected)
  ?f ?o ?arrow ?expected
  identity 1 => 1
  inc 1 => 2
  str 1 => "1"
  str \a => "a"
  + \a => (throws ClassCastException))

;Functor functions on a string
(functor-law2
 (gen-fn s/capitalize s/lower-case s/upper-case s/reverse)
 (gen/string))

(fmap-keeps-type s/reverse (gen/string))

;Functor functions on a vector
(functor-law2
 (gen-fn (partial * 100) inc)
 (gen/vec gen/int))

(fmap-keeps-type inc (gen/vec gen/int))

;Functor functions on a list
(functor-law2
 (gen-fn (partial * 100) inc)
 (into (list) (gen/list gen/int)))

(fmap-keeps-type inc (into (list) (gen/list gen/int)))

;Functor functions on a set
(functor-law2
 (gen-fn (partial * 100) inc)
 (gen/set gen/int))

(fmap-keeps-type inc (gen/set gen/int))

;Functor functions on a seq
;Due to Clojure implementation details, the type of the result does not have to be the same
;as the type of the input seq. However, keeping the type is not required by any Functor law.
;Both seqs implement the seq interface, though.
(functor-law2
 (gen-fn (partial * 100) inc)
 (seq (gen/list gen/int)))

;Functor functions on a lazy seq
(functor-law2
 (gen-fn (partial * 100) inc)
 (lazy-seq (gen/list gen/int)))

(fmap-keeps-type inc (lazy-seq (gen/list gen/int)))

;Functor functions on a map entry
(functor-law2
 (gen-fn (partial * 100) inc)
 (clojure.lang.MapEntry. (gen/keyword) (gen/int))) ;#(or (first (gen/hash-map gen/keyword gen/int)) (first {:a 1})))

(fmap-keeps-type inc (clojure.lang.MapEntry. (gen/keyword) (gen/int)));#(or (first (gen/hash-map gen/keyword gen/int)) (first {:a 1})))

;Functor functions on a map (depends on proper behavior of map entries as functors)
(functor-law2
 (gen-fn (partial * 100) inc)
 (gen/hash-map gen/keyword gen/int))

(fmap-keeps-type
 inc
 (gen/hash-map gen/keyword gen/int))

;Functor functions on an atom
(functor-law2
 (gen-fn (partial * 100) inc)
 (atom (gen/int)))

(fmap-keeps-type  inc (atom (gen/int)))

;Functor functions on a ref
(dosync
 (functor-law2
  (gen-fn (partial * 100) inc)
  (ref (gen/int))))

(dosync
 (fmap-keeps-type inc (ref (gen/int))))

;Functor functions on a function
;not a proper test at all!!(functor-law2 (gen-fn (partial * 100) inc) (gen-fn (partial * 44) dec) (gen/int))

;============================= Applicative tests ==========================

(defmacro applicative-law1 [f x]
  `(fact "First applicative law."
         (<*> (pure ~x ~f) ~x) => (fmap ~f ~x)))

(defmacro applicative-law2-identity [x]
  `(fact "Identity applicative law."
         (<*> (pure ~x identity) ~x) => ~x))

(defmacro applicative-law3-composition [u v w]
  `(fact "Composition applicative law."
         (-> (pure ~w #(partial comp %)) (<*> ~u) (<*> ~v) (<*> ~w))
         => (<*> ~u (<*> ~v ~w))))

(defmacro applicative-law4-homomorphism [ap f x]
  `(fact "Homomorphism applicative law."
         (<*> (pure ~ap ~f) (pure ~ap ~x))
         => (pure ~ap (~f ~x))))

(defmacro applicative-law5-interchange [ap f x]
  `(fact "Interchange applicative law."
         (<*> (pure ~ap ~f) (pure ~ap ~x))
         => (<*> (pure ~ap ($ ~x)) (pure ~ap ~f))))

(defmacro <*>-keeps-type [f x]
  `(fact "<*> should return data of the same type
            as the applicative argument."
         (type (<*> (pure ~x ~f) ~x)) => (type ~x)))

;--------------- Vector ---------------
(applicative-law1 inc [1 -2 5])

(applicative-law2-identity [1 445 -4])

(applicative-law3-composition [inc]
                              [(partial * 10)]
                              [1 -34343444])

(applicative-law4-homomorphism [] inc 1)

(applicative-law5-interchange [] inc 1)

(<*>-keeps-type inc (list 1 -4 9))

;;--------------- List ---------------
(applicative-law1 inc (list 2 44 -7))

(applicative-law2-identity (list 2 -5 99))

(applicative-law3-composition (list inc)
                              (list (partial * 10))
                              (list 2 -445 2))

(applicative-law4-homomorphism (list) inc 2)

(applicative-law5-interchange (list) inc 2)

(<*>-keeps-type inc (list 2 -4 9))

;;-------------- Seq ----------------
(applicative-law1 inc (seq (list 3 9 0)))

(applicative-law2-identity (seq (list 3 -79 29)))

(applicative-law3-composition (seq (list inc))
                              (seq (list (partial * 10)))
                              (seq (list 3 -1 0)))

(applicative-law4-homomorphism (seq (list 3)) inc 3)

(applicative-law5-interchange (seq (list 3)) inc 3)

(<*>-keeps-type inc (seq (list 3 -4 9)))

;;-------------- Lazy Seq ----------------
(applicative-law1 inc (lazy-seq (list 3 9 0)))

(applicative-law2-identity (lazy-seq (list 3 -79 29)))

(applicative-law3-composition (lazy-seq (list inc))
                              (lazy-seq (list (partial * 10)))
                              (lazy-seq (list 3 -1 0)))

(applicative-law4-homomorphism (lazy-seq (list 3)) inc 3)

(applicative-law5-interchange (lazy-seq (list 3)) inc 3)

(<*>-keeps-type inc (lazy-seq (list 3 -4 9)))

;;-------------- Set ----------------
(applicative-law1 inc #{4 2 -44})

(applicative-law2-identity #{4 40 -1})

(applicative-law3-composition #{inc}
                              #{(partial * 10)}
                              #{4 -5 -6})

(applicative-law4-homomorphism #{} inc 4)

(applicative-law5-interchange #{} inc 4)

(<*>-keeps-type inc #{4 -2 5})

;;-------------- MapEntry -----------
(applicative-law1 inc (first {5 5}))

(applicative-law2-identity (first {5 5}))

(applicative-law3-composition (first {58 inc})
                              (first {57 (partial * 10)})
                              (first {5 5}))

(applicative-law4-homomorphism (first {57 57}) inc 5)

(applicative-law5-interchange (first {58 58}) inc 5)

;;-------------- Map ----------------
;;figure out how to create pure map. probably it shouid require [k v] pairs
(fact (pure {} 2) => {identity 2})

(fact (<*> {:a inc :b dec} {:a 1 :b 100})
      => {:a 2 :b 99})

(applicative-law1 inc {identity 6})

(applicative-law1 inc {6 6})

(applicative-law2-identity {identity 6})

(applicative-law2-identity {6 6})

(applicative-law3-composition {6 inc}
                              {6 (partial * 10)}
                              {6 6 -5 -6})

(applicative-law4-homomorphism {} inc 4)

(applicative-law5-interchange {} inc 4)

(<*>-keeps-type inc {4 -2 5 5})
