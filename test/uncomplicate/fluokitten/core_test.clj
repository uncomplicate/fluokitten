;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.fluokitten.core-test
  (:use [uncomplicate.fluokitten algo jvm core test utils])
  (:use [midje.sweet :exclude [just]])
  (:require [clojure.string :as s]
            [clojure.core.reducers :as r]))

(def unchecked-math *unchecked-math*)
(set! *unchecked-math* false)

;;=============== Curry tests ========================

(facts
 ((curry + 3) 1 2 3) => 6
 (((curry + 2) 1) 2) => 3)

(fact "First functor law."
      (fmap identity) => identity)

;;=============== Functor ===========================

;;--------------- nil ---------------

(functor-law2 inc + nil)

(functor-law2 inc + nil 99 0)

(facts "nil as functor."
       (fmap identity nil) => nil
       (fmap + nil) => nil
       (fmap + nil 1 2 3) => nil)

;;--------------- literals ---------------
;; Functor operations on any Object, if more particular
;; implementation is not specified.
;; The type of the result does not have to be the same
;; as the type of the input object. The result type
;; depends on the function that is being applied.
;; However, keeping the type is not required by any
;; Functor law (in the sense of clojure types),
;; so keeping the exact type is convenient
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
       (fmap  + \a) => (throws ClassCastException)
       (fmap str \a \b 1) => "ab1")

;;--------------- String ---------------

(functor-law2 s/capitalize s/reverse "something")

(functor-law2 s/capitalize str "something" "else")

(fmap-keeps-type s/reverse "something")

(fmap-keeps-type str "something" "else")

(facts "Strings as functors"
       (fmap (fn [^String s] (.toUpperCase s)) "abc") => "ABC"
       (fmap str "abc" "def") => "abcdef")

;;--------------- Vector ---------------

(functor-law2 inc (partial * 100) [1 -199 9])

(functor-law2 inc (partial * 100) [1 -199 9] [1 -66 9])

(fmap-keeps-type inc [100 0 -999])

(fmap-keeps-type + [100 0 -999] (list 44 0 -54))

(facts "Vectors as functors"
       (fmap inc [1 2 3]) => [2 3 4]
       (fmap + [1 2 3] [4 5 6]) => [5 7 9]
       (fmap + [1 2 3] [4 5 6] [7 8 9]) => [12 15 18])

;; ------- Primitive Java arrays ---------------

(functor-law2 inc (partial * 100) (double-array [1 -199 9]))

(functor-law2 inc (partial * 100) (float-array [1 -199 9]) (float-array [1 -66 9]))

(fmap-keeps-type inc (int-array [77 0 -39]))

(fmap-keeps-type + (short-array [77 0 -39]) (short-array [88 -8 8]))

(facts "Primitive arrays as functors"
       (fmap inc (long-array [1 2 3])) => (check-eq (long-array [2 3 4]))
       (fmap + (float-array [1 2 3]) (float-array [4 5 6])) => (check-eq (float-array [5 7 9]))
       (fmap + (byte-array [1 2 3]) (byte-array [4 5 6]) (byte-array [7 8 9]))
       => (check-eq (byte-array [12 15 18])))

;;--------------- List ---------------

(functor-law2 inc (partial * 100) (list -3 5 0))

(functor-law2 inc (partial * 100) (list -3 5 0) (list -3 44 -2))

(fmap-keeps-type inc (list 77 0 -39))

(fmap-keeps-type + (list 77 0 -39) (list 88 -8 8))

(facts "Lists as functors"
       (fmap inc (list 1 2 3)) => (list 2 3 4)
       (fmap + '(1 2 3) '(4 5 6)) => '(5 7 9)
       (fmap + '(1 2 3) '(4 5 6) '(7 8 9)) => '(12 15 18))

;;--------------- Set ---------------

(functor-law2 inc (partial * 100) #{-449 9 6})

(functor-law2 inc (partial * 100) #{-449 9 6} #{-4 88 7})

(fmap-keeps-type inc #{5 89 -7})

(fmap-keeps-type + #{5 89 -7} (list -3 -4 -45))

;;--------------- Seq ---------------
;; Due to Clojure implementation details,
;; the type of the result does not have to be the same
;; as the type of the input seq.
;; However, keeping the type is not mandated by any Functor law.
;; Both seqs implement the seq interface, though.

(functor-law2 inc (partial * 100) (seq (list 8 9 10)))

(functor-law2 inc (partial * 100) (seq (list 8 9 10)) (seq (list -5 -4 -5)))

;;--------------- LazySeq ---------------

(functor-law2 inc (partial * 100) (lazy-seq (list 78 -3 5)))

(functor-law2 inc (partial * 100) (lazy-seq (list 78 -3 5)) (lazy-seq (list 88 0 -4)))

(fmap-keeps-type inc (lazy-seq (list 8 9 -2)))

(fmap-keeps-type + (lazy-seq (list 8 9 -2)) (lazy-seq (list 18 5 -92)))

;;--------------- Eduction ---------------

(functor-law2 inc (partial * 100) (eduction [78 -3 5]))

(functor-law2 inc (partial * 100) (eduction (list 78 -3 5)) (eduction (list 88 0 -4)))

(fmap-keeps-type inc (eduction (list 8 9 -2)))

(fmap-keeps-type + (eduction (list 8 9 -2)) (eduction (list 18 5 -92)))

;;--------------- CollReduce ---------------

(functor-law2 inc (partial * 100) (reducible [1 -199 9]))

(fmap-keeps-type inc (reducible [100 0 -999]))

;;--------------- MapEntry ---------------

(functor-law2 inc (partial * 100) (first {:a 11}))

(functor-law2 inc (partial * 100) (first {:a 11}) (first {:a 5}))

(fmap-keeps-type inc (first {:c 3}))

(fmap-keeps-type + (first {:c 3}) (first {:d 5}))

;;--------------- Map ---------------

(fact
 (fmap (comp inc +) {:a 1} {:a 2} {:a 3}) => {:a 7})

(fact
 (fmap (fn [& xs]
         (hash-map :sum (apply + xs))) {:a 1 :b 2} {:a 4}) => {:a {:sum 5}, :b {:sum 2}})

(functor-law2 inc (partial * 100) {:a 2 :t 5 :h 99})

(functor-law2 inc + {:a 2 :t 5 :h 99} {:a 7 :t 8 :h 49} {:a 8 :t 3 :h 59})

(fmap-keeps-type inc {:a 2 :t 5 :h 99})

(fmap-keeps-type + {:a 2 :t 5 :h 99} {:k 5 :n 4 :dd 5})

;;--------------- Atom ---------------

(functor-law2 inc (partial * 100) (atom 34))

(functor-law2 inc (partial * 100) (atom 35) (atom 5))

(fmap-keeps-type  inc (atom 36))

(fmap-keeps-type  + (atom 37) (atom 5))


;;--------------- Volatile ---------------

(functor-law2 inc (partial * 100) (volatile! 34))

(functor-law2 inc (partial * 100) (volatile! 35) (volatile! 5))

(fmap-keeps-type inc (volatile! 36))

(fmap-keeps-type + (volatile! 37) (volatile! 5))

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

(fact "First functor law."
      (fmap identity) => identity)

(fact "Second functor law."
      ((fmap (comp inc dec) inc) 1)
      => ((fmap inc (fmap dec inc)) 1))

(fact "Second functor law"
      ((fmap (comp inc dec) +) 1 2 3)
      => ((fmap inc (fmap dec +)) 1 2 3))

(fact "Fmap keeps type." (fn? (fmap inc dec)))

(fact "Fmap keeps type - varargs." (fn? (fmap inc dec +)))

;;--------------- Curried Function ---------------

(facts "How CurriedFn fmap works for fixed args."
       ((((fmap (curry +) (curry +)) 1) 2) 3) => 6

       ((((fmap (comp #(partial % 1) (curry + 2))
                (curry + 2)) 1) 2) 3) => 7

       ((((fmap #(partial % 1)
                (fmap (curry + 2) (curry + 2))) 1) 2) 3)
       => 7)

(facts "Second functor law."
       ((fmap (comp inc dec) (curry inc)) 1)
       => ((fmap inc (fmap dec (curry inc))) 1)

       ((fmap (comp inc dec) (curry +)) 1 2 3)
       => ((fmap inc (fmap dec (curry +))) 1 2 3)

       (((fmap (comp inc dec) (curry + 2)) 1) 2)
       => (((fmap inc (fmap dec (curry + 2))) 1) 2)

       ((((fmap (comp #(partial %)
                      (curry + 2)) (curry + 2)) 1) 2) 3)
       => ((((fmap #(partial %)
                   (fmap (curry + 2) (curry + 2))) 1) 2) 3))

(facts "How CurriedFn fmap works for variable args."
       (((fmap /
               (curry + 4)
               (curry (partial * 10) 3)) 2 3 5) 2)
       => 1/50

       (((fmap /
               (curry + 4)
               (curry (partial * 10) 3)
               (curry (partial / 4) 1)) 2 3 5) 2)
       => 3/10)

(fact "Fmap keeps type."
      (ifn? (fmap inc (curry dec))))

(fact "Fmap keeps type - varargs."
      (ifn? (fmap inc (curry dec) (curry +))))

;; ========== Applicative Functors ==============================

;;--------------- Vector ---------------

(applicative-law1 inc [1 -2 5])

(applicative-law1 + [1 -2 5] [8 98 0])

(applicative-law2-identity [1 445 -4])

(applicative-law3-composition [inc] [(partial * 10)] [1 -34343444])

(applicative-law3-composition [inc] [(partial * 10)] [1 -34343444] (list 1 78))

(applicative-law4-homomorphism [] inc 1)
(applicative-law4-homomorphism [] + 1 55 6)

(applicative-law5-interchange [] inc 1)
(applicative-law5-interchange [] + 1 54 -2)

(fapply-keeps-type inc  [1 -4 9])
(fapply-keeps-type + [1 -4 9] [2 -3 -4])

;;--------------- Java Arrays ---------------

(applicative-law1 inc (double-array [1 -2 5]))

(applicative-law1 + (float-array [1 -2 5]) (float-array [8 98 0]))

(applicative-law2-identity (int-array [1 445 -4]))

(applicative-law3-composition [inc] [(partial * 10)] (long-array [1 -34343444]))

(applicative-law3-composition [inc] [(partial * 10)] (long-array [1 -34343444]) (long-array [1 78]))

(applicative-law4-homomorphism (double-array 0) inc 1)
(applicative-law4-homomorphism (byte-array 0) + 1 55 6)

(applicative-law5-interchange (int-array 0) inc 1)
(applicative-law5-interchange (int-array 0) + 1 54 -2)

(fapply-keeps-type inc  (long-array [1 -4 9]))
(fapply-keeps-type + (double-array [1 -4 9]) (double-array [2 -3 -4]))

;;--------------- CollReduce ---------------

(applicative-law1 inc (reducible [1 -2 5]))

(applicative-law2-identity (reducible [1 445 -4]))

(applicative-law3-composition (reducible [inc]) (reducible [(partial * 10)]) (reducible [1 -34343444]))

(applicative-law4-homomorphism (reducible []) inc 1)

(applicative-law5-interchange (reducible []) inc 1)

(fapply-keeps-type inc (reducible [1 -4 9]))

;;--------------- List ---------------

(applicative-law1 inc (list 2 44 -7))

(applicative-law1 + (list 2 44 -7) (list 3 5 -3))

(applicative-law2-identity (list 2 -5 99))

(applicative-law3-composition (list inc) (list (partial * 10)) (list 2 8 -33))

(applicative-law3-composition (list inc) (list (partial * 10)) (list 2 8 -33) (list -4 -5 -5))

(applicative-law4-homomorphism (list) inc 2)

(applicative-law4-homomorphism (list) + 2 -3 -44)

(applicative-law5-interchange (list) inc 2)

(applicative-law5-interchange (list) + 2 9 -21)

(fapply-keeps-type inc (list 2 -4 9))

(fapply-keeps-type + (list 2 -4 9) (list 9 -8))

;;-------------- Seq ----------------

(applicative-law1 inc (seq (list 3 9 0)))

(applicative-law1 + (seq (list 3 9 0)) (seq (list -5 4 6)))

(applicative-law2-identity (seq (list 3 -79 29)))

(applicative-law3-composition (seq (list inc)) (seq (list (partial * 10))) (seq (list 3 -1 0)))

(applicative-law3-composition (seq (list inc)) (seq (list (partial * 10)))
                              (seq (list 3 -1 0)) (seq (list -5 3 4)))

(applicative-law4-homomorphism (seq (list 3)) inc 3)

(applicative-law4-homomorphism (seq (list 3)) + 3 0 5 5)

(applicative-law5-interchange (seq (list 3)) inc 3)

(applicative-law5-interchange (seq (list 3)) + 3 -8 -87)

(fapply-keeps-type inc (seq (list 3 -4 9)))

(fapply-keeps-type + (seq (list 3 -4 9)) (seq (list -5 -5 -3)))

;;-------------- Lazy Seq ----------------

(applicative-law1 inc (lazy-seq (list 3 9 0)))

(applicative-law1 + (lazy-seq (list 3 9 0)) (lazy-seq (list -4 -5)))

(applicative-law2-identity (lazy-seq (list 3 -79 29)))

(applicative-law3-composition (lazy-seq (list inc)) (lazy-seq (list (partial * 10)))
                              (lazy-seq (list 3 -1 0)))

(applicative-law3-composition (lazy-seq (list inc)) (lazy-seq (list (partial * 10)))
                              (lazy-seq (list 3 -1 0)) (lazy-seq (list 9)))

(applicative-law4-homomorphism (lazy-seq (list 3)) inc 3)

(applicative-law4-homomorphism (lazy-seq (list 3)) + 3 -2)

(applicative-law5-interchange (lazy-seq (list 3)) inc 3)

(applicative-law5-interchange (lazy-seq (list 3)) + 3 -2 -3)

(fapply-keeps-type inc (lazy-seq (list 3 -4 9)))

(fapply-keeps-type + (lazy-seq (list 3 -4 9)) (lazy-seq (list -5 -5 -5)))

;;-------------- Eduction ----------------

(applicative-law1 inc (eduction (list 3 9 0)))

(applicative-law1 + (eduction (list 3 9 0)) (eduction (list -4 -5)))

(applicative-law2-identity (eduction (list 3 -79 29)))

(applicative-law3-composition (eduction (list inc)) (eduction (list (partial * 10)))
                              (eduction (list 3 -1 0)))

(applicative-law3-composition (eduction (list inc)) (eduction (list (partial * 10)))
                              (eduction (list 3 -1 0)) (eduction (list 9)))

(applicative-law4-homomorphism (eduction (list 3)) inc 3)

(applicative-law4-homomorphism (eduction (list 3)) + 3 -2)

(applicative-law5-interchange (eduction (list 3)) inc 3)

(applicative-law5-interchange (eduction (list 3)) + 3 -2 -3)

(fapply-keeps-type inc (eduction (list 3 -4 9)))

(fapply-keeps-type + (lazy-seq (list 3 -4 9)) (lazy-seq (list -5 -5 -5)))

;;-------------- Set ----------------

(applicative-law1 inc #{4 2 -44})

(applicative-law1 + #{4 2 -44} #{0 -8})

(applicative-law2-identity #{4 40 -1})

(applicative-law3-composition #{inc} #{(partial * 10)} #{4 -5 -6})

(applicative-law3-composition #{inc} #{(partial * 10)} #{4 -5 -6} #{0 -4})

(applicative-law4-homomorphism #{} inc 4)

(applicative-law4-homomorphism #{} + 4 0 -98)

(applicative-law5-interchange #{} inc 4)

(applicative-law5-interchange #{} + 4 -8)

(fapply-keeps-type inc #{4 -2 5})

(fapply-keeps-type + #{4 -2 5} #{-9 -7})

;;-------------- MapEntry -----------

(applicative-law1 inc (first {5 5}))

(applicative-law1 + (first {5 5}) (first {5 6}))

(applicative-law2-identity (first {5 5}))

(applicative-law3-composition (first {58 inc}) (first {57 (partial * 10)}) (first {5 5}))

(applicative-law3-composition (first {58 inc}) (first {57 (partial * 10)}) (first {5 5}) (first {5 6}))

(applicative-law4-homomorphism (first {57 57}) inc 5)

(applicative-law4-homomorphism (first {57 57}) + 5 -4 5)

(applicative-law5-interchange (first {58 58}) inc 5)

(applicative-law5-interchange (first {58 58}) + 5 3 4 5)

;;-------------- Map ----------------

(fact (pure {} 2) => {nil 2})

(fact (fapply {:a inc :b dec nil (partial * 2)} {:a 1 :b 5}) => {:a 2 :b 4})

(fact (fapply {:a inc :b dec} {:a 1 :c 2}) => {:a 2 :c 2})

(fact (fapply {:a + :b - nil (partial * 2)} {:a 1 :b 1} {:a 2 :b 3 :c 44} {:b 4})
      => {:a 3 :b -6 :c 88})

(fact (fapply {:a + :b - :d (partial * 2)} {:a 1 :b 1} {:a 2 :b 3 :c 44} {:b 4})
      => {:a 3 :b -6 :c 44})

(fact (fapply {nil +} {:a 1 :b 3} {:a 2 :b 4} {:a 3 :b 5})
      => {:a 6 :b 12})

(applicative-law1 inc {nil 6})

(applicative-law1 inc {6 6})

(applicative-law1 + {nil 6} {2 23})

(applicative-law2-identity {nil 6})

(applicative-law2-identity {6 6})

(applicative-law3-composition {6 inc} {6 (partial * 10)} {6 6 -5 -6})

(applicative-law3-composition {6 inc} {6 (partial * 10)} {6 6 -5 -6} {6 8 -5 -2})

(applicative-law3-composition {nil inc} {nil (partial * 10)} {6 6 -5 -6})

(applicative-law4-homomorphism {} inc 4)

(applicative-law4-homomorphism {} + 4 7 9)

(applicative-law5-interchange {} inc 4)

(applicative-law5-interchange {} + 4 8 9)

(fapply-keeps-type inc {4 -2 5 5})

;;-------------- Atom -----------

(applicative-law1 inc (atom 6))

(applicative-law1 + (atom 6) (atom 9) (atom -77) (atom -1))

(applicative-law2-identity (atom 6))

(applicative-law3-composition (atom inc) (atom (partial * 10)) (atom 6))

(applicative-law3-composition (atom inc) (atom (partial * 10)) (atom 6) (atom -2))

(applicative-law4-homomorphism (atom 6) inc 5)

(applicative-law4-homomorphism (atom 6) + 5 -4 5)

(applicative-law5-interchange (atom 6) inc 5)

(applicative-law5-interchange (atom 6) + 5 3 4 5)

;;-------------- Volatile -----------

(applicative-law1 inc (volatile! 6))

(applicative-law1 + (volatile! 6) (volatile! 9) (volatile! -77) (volatile! -1))

(applicative-law2-identity (volatile! 6))

(applicative-law3-composition (volatile! inc) (volatile! (partial * 10)) (volatile! 6))

(applicative-law3-composition (volatile! inc) (volatile! (partial * 10)) (volatile! 6) (volatile! -2))

(applicative-law4-homomorphism (volatile! 6) inc 5)

(applicative-law4-homomorphism (volatile! 6) + 5 -4 5)

(applicative-law5-interchange (volatile! 6) inc 5)

(applicative-law5-interchange (volatile! 6) + 5 3 4 5)

;;-------------- Ref -----------

(dosync
 (applicative-law1 inc (ref 6)))

(dosync
 (applicative-law1 + (ref 6) (ref 9) (ref -77) (ref -1)))

(dosync
 (applicative-law2-identity (ref 6)))

(dosync
 (applicative-law3-composition (ref inc) (ref (partial * 10)) (ref 6)))

(dosync
 (applicative-law3-composition (ref inc) (ref (partial * 10)) (ref 6) (ref -2)))

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
       ((fapply (pure identity inc) (pure identity 1)) 6)
       => ((fmap inc (pure identity 1)) 7)

       ((fapply (pure identity +) (pure identity 1)) 7 9 11 13)
       => ((fmap + (pure identity 1)) 7 9 11 13))

(facts "Identity applicative law."
       ((fapply (pure identity identity) (curry inc)) 7) => 8
       ((fapply (pure identity identity) +) 7 8 2) => 17)

(facts "How fapply works for functions"

       ((fapply (pure identity +) (pure identity 7)))=> 7

       ((fapply (pure identity +) (pure identity 1)
                (pure identity 2) (pure identity 3)) 11)
       => 6

       ((fapply (pure identity +) (pure identity 1)
                (pure identity 2) (pure identity 3)) 11 33)
       => 6)

;;------------------- Curried Function ---------------------------

(let [c+ (curry +)
      c* (curry *)
      curried (curry identity)]

  (facts
   "Learn You a Haskell Applicative curried function example."
   ((fapply (fmap c+ (c+ 3)) (c* 100)) 5) => 508
   ((fapply (fmap c+ (c+ 3)) (c* 100)) 2 3 4) => 2412

   ((fapply (fapply (pure curried c+) (c+ 3))
            (c* 100)) 6) => 609
   ((<*> (pure curried c+) (c+ 3) (c* 100)) 6) => 609)

  (facts "First applicative law."
         ((fapply (pure curried inc) (pure curried 1)) 6)
         => ((fmap inc (pure curried 1)) 7)

         ((fapply (pure curried +) (pure curried 1)) 7 9 11 13)
         => ((fmap + (pure curried 1)) 7 9 11 13))

  (facts "Identity applicative law."
         ((fapply (pure curried identity) (curry inc)) 7) => 8
         ((fapply (pure curried identity) c+) 7 8 2) => 17)

  (facts "How fapply works for curried functions"
         ((fapply (curry + 4) (<*> (pure curried c+) (c+ 2) (c* 10))) 2 3 4) => 260

         ((-> (pure (curry +) (curry  / 3)) (fapply (curry +))
              (fapply (curry +)) (fapply ((curry *) 10))) 7 1) => 1/70

         ((fapply c+ (pure curried 7)) 9)=> 16

         ((fapply (pure curried +) (pure curried 1) (pure curried 2) (pure curried 3)) 11) => 6

         ((fapply (pure curried +) (pure curried 1) (pure curried 2) (pure curried 3)) 11 33) => 6)

  (facts "Composition applicative law"
         ((-> (pure curried (curry comp)) (fapply c+) (fapply c*) (fapply (c* 10))) 7)
         => ((fapply c+ (fapply c* (c* 10))) 7))

  (facts "Homomorphism applicative law."
         ((fapply (pure curried #(% 7)) (pure curried (c* 10))) 8)
         => ((pure curried (#(% 7) (c* 10))) 8))

  (facts "Interchange applicative law."
         ((fapply c+ (pure curried 7)) 9) => ((fapply (pure curried #(% 7)) c+) 9)))

;; ================ Monad =============================================================

;;--------------- Vector --------------------------------

(monad-law1-left-identity [] (comp vector inc) 1)

(monad-law1-left-identity [] (comp vector +) 1 2 3)

(monad-law2-right-identity [1 2 -33])

(monad-law3-associativity (comp vector inc) (comp vector (partial * 10)) [1 -3 -88])

;;--------------- Primitive Java Arrays --------------------------------

(monad-law1-left-identity (double-array 0) (comp (pure (double-array 0)) inc) 1)

(monad-law1-left-identity (int-array 0) (comp (pure (int-array 0)) +) 1 2 3)

(monad-law2-right-identity (short-array [1 2 -33]))

(monad-law3-associativity (comp (pure (float-array 0)) inc)
                          (comp (pure (float-array 0)) (partial * 10))
                          (float-array [1 -3 -88]))

;;--------------- CollReduce ---------------------------------

(monad-law1-left-identity (reducible []) (comp reducible vector inc) 1)

(monad-law2-right-identity (reducible [1 2 -33]))

(monad-law3-associativity (comp reducible vector inc)
                          (comp reducible vector (partial * 10))
                          (reducible [1 -3 -88]))

;;--------------- List -----------------------------------

(monad-law1-left-identity (list) (comp list inc) 2)

(monad-law1-left-identity (list) (comp list +) 2 3 4)

(monad-law2-right-identity (list 2 4 -33))

(monad-law3-associativity (comp list inc) (comp list (partial * 10)) (list 2 -3 -88))

;;--------------- LazySeq -----------------------------------

(monad-law1-left-identity (lazy-seq (list)) (comp list inc) 3)

(monad-law1-left-identity (lazy-seq (list)) (comp list +) 3 49 9)

(monad-law2-right-identity (lazy-seq (list 3 2 -33)))

(monad-law3-associativity (comp list inc) (comp list (partial * 10)) (lazy-seq (list 3 -3 -88)))

;;--------------- Eduction -----------------------------------

(monad-law1-left-identity (eduction (list)) (comp list inc) 3)

(monad-law1-left-identity (eduction (list)) (comp list +) 3 49 9)

(monad-law2-right-identity (eduction (list 3 2 -33)))

(monad-law3-associativity (comp list inc) (comp list (partial * 10)) (eduction (list 3 -3 -88)))

;;--------------- Seq -----------------------------------

(monad-law1-left-identity (conj (seq [0]) 2) (comp seq vector inc) 4)

(monad-law1-left-identity (conj (seq [0]) 2) (comp seq vector +) 4 8 9)

(monad-law2-right-identity (conj (seq [4 2 -33]) 2))

(monad-law3-associativity (comp seq vector inc)
                          (comp seq vector (partial * 10))
                          (conj (seq [4 -3 -88]) 2))

;;--------------- Set ---------------------------------

(monad-law1-left-identity #{} (comp hash-set inc) 6)

(monad-law1-left-identity #{} (comp hash-set +) 6 7 99)

(monad-law2-right-identity #{5 -3 24})

(monad-law3-associativity (comp hash-set inc) (comp hash-set (partial * 10)) #{5 -56 30})

;;--------------- Map ---------------------------------

(facts
 (join {:a 1 :b 2}) => {:a 1 :b 2}
 (join {:a {:a1 1 :a2 5} :b {:b1 2}})
 => {[:a :a1] 1 [:a :a2] 5 [:b :b1] 2})

(monad-law1-left-identity {} #(hash-map :increment (inc %)) 5)

(monad-law1-left-identity {} (fn [& xs] (hash-map :sum (apply + xs))) 5 6 88 9)

(monad-law2-right-identity {:a 1 :b 2})

(monad-law3-associativity #(hash-map :increment (inc %)) #(hash-map :10times (* 10 %)) {:a 1 :b 2})

(monad-law3-associativity (fn [& xs] (hash-map :sum (apply + xs))) #(hash-map :10times (* 10 %))
                          {:a 1 :b 2 :c 3} {:a 4 :b 3 :c 2} {:a 12 :b 23 :c 9})

(fact
 (let [f #(hash-map :increment (inc %))
       m {:a 1 :b 2}]
   (fapply (pure {} f) m) => (bind (pure {} f) #(fmap % m))))

;;--------------- MapEntry ----------------------------

(facts
 (join (first {:a 1})) => (first {:a 1})
 (join (first {:a [:b 1]})) => (first {[:a :b] 1}))

(monad-law1-left-identity (first {:a 1}) #(first (hash-map :increment (inc %))) 5)

(monad-law1-left-identity (first {:a 1}) (fn [& xs] (first (hash-map :sum (apply + xs)))) 5 77 8)

(monad-law2-right-identity (first {:a 4}))

(monad-law3-associativity #(first (hash-map :increment (inc %)))
                          #(first (hash-map :10times (* 10 %)))
                          (first {:a 1}))

(monad-law3-associativity (fn [& xs]
                            (first (hash-map :sum (apply + xs))))
                          #(first (hash-map :10times (* 10 %)))
                          (first {:a 1}) (first {:a 2}) (first {:a 77}))

;;--------------- Atom ----------------------------

(facts "Join function for atoms."
       (join (atom 1)) => (check-eq (atom 1))
       (join (atom (atom 2))) => (check-eq (atom 2)))

(monad-law1-left-identity (atom 9) (comp atom inc) 1)

(monad-law1-left-identity (atom 9) (comp atom +) 1 2 3)

(monad-law2-right-identity (atom 9))

(monad-law3-associativity (comp atom inc) (comp atom (partial * 10)) (atom 9))

;;--------------- Atom ----------------------------

(facts "Join function for volatiles."
       (join (volatile! 1)) => (check-eq (volatile! 1))
       (join (volatile! (volatile! 2))) => (check-eq (volatile! 2)))

(monad-law1-left-identity (volatile! 9) (comp volatile! inc) 1)

(monad-law1-left-identity (volatile! 9) (comp volatile! +) 1 2 3)

(monad-law2-right-identity (volatile! 9))

(monad-law3-associativity (comp volatile! inc) (comp volatile! (partial * 10)) (volatile! 9))

;;--------------- Ref ----------------------------

(dosync
 (facts "Join function for refs."
        (join (ref 1)) => (check-eq (ref 1))
        (join (ref (ref 2))) => (check-eq (ref 2))))

(dosync
 (monad-law1-left-identity (ref 9) (comp ref inc) 1))

(dosync
 (monad-law1-left-identity (ref 9) (comp ref +) 1 2 3))

(dosync
 (monad-law2-right-identity (ref 9)))

(dosync
 (monad-law3-associativity (comp ref inc) (comp ref (partial * 10)) (ref 9)))

;;------------------- Curried Function ---------------------------

(let [c+ (curry +)
      c* (curry *)
      curried (curry identity)]

  (facts "How bind for curried functions works."
         ((bind + (comp return (partial + 3))) 3 4 5) => 15
         ((bind c+ (comp return inc)) 3 4) => 8
         ((bind (pure identity 5) (comp return inc)) 3 1) => 6
         ((bind (bind (c* 3) (comp return inc)) (comp return (c+ 17))) 7)
         ((bind (bind (c* 3) c*) c+) 7) => 154)

  (facts "How join for curried function works."
         ((bind c* c+) 3 4) => ((join (fmap c+ c*)) 3 4))

  (facts "Left Identity Monad Law"
         ((bind (pure curried 7) c*) 6) => ((c* 7) 6)

         ((bind (pure curried 7) c*) 6 7 8) => ((c* 7) 6 7 8))

  (fact "Right identity Monad Law."
        ((bind c* (pure c*)) 2 3) => (c* 2 3))

  (facts "Associativity Monad Law."
         ((bind (bind (c* 3) c*) c+) 7) => ((bind (c* 3) (fn [x] (bind (c* x) c+))) 7)

         ((bind (bind (c* 3) c*) c+) 2 3 4) => ((bind (c* 3) (fn [x] (bind (c* x) c+))) 2 3 4)))

;; ====================== Comonads =============================================================



;;============= Magmas, Semigroups and Monoids =======================

;;----------------------- Vector --------------------------

(magma-op-keeps-type [1 2] [3 4])

(magma-op-keeps-type [1 2] [3 4] [5 6])

(semigroup-op-associativity [1 2] [3 4])

(semigroup-op-associativity [1 2] [3 4] [5 6])

(monoid-identity-law [1 2])

;;----------------------- Primitive Java Arrays --------------------------

(magma-op-keeps-type (double-array [1 2]) (double-array [3 4]))

(magma-op-keeps-type (float-array [1 2]) (float-array [3 4]) (float-array [5 6]))

(semigroup-op-associativity (int-array [1 2]) (int-array  [3 4]))

(semigroup-op-associativity (short-array [1 2]) (short-array [3 4]) (short-array [5 6]))

(monoid-identity-law (long-array [1 2]))

;;----------------------- List --------------------------

(magma-op-keeps-type (list 1 2) (list 3 4))

(magma-op-keeps-type (list 1 2) (list 3 4) (list 5 6) (list 7 8))

(semigroup-op-associativity (list 1 2) (list 3 4))

(semigroup-op-associativity (list 1 2) (list 3 4) (list 5 6) (list 7 8))

(monoid-identity-law (list 1 2))

;;----------------------- LazySeq --------------------------

(magma-op-keeps-type (lazy-seq (list 1 2)) (lazy-seq (list 3 4)))

(magma-op-keeps-type (lazy-seq (list 1 2)) (lazy-seq (list 3 4))
                     (lazy-seq (list 5 6)) (lazy-seq (list 7 8)))

(semigroup-op-associativity (lazy-seq (list 1 2)) (lazy-seq (list 3 4)))

(semigroup-op-associativity (lazy-seq (list 1 2)) (lazy-seq (list 3 4))
                            (lazy-seq (list 5 6)) (lazy-seq (list 7 8)))

(monoid-identity-law (lazy-seq (list 1 2)))

;;----------------------- Eduction --------------------------

(magma-op-keeps-type (eduction (list 1 2)) (eduction (list 3 4)))

(magma-op-keeps-type (eduction (list 1 2)) (eduction (list 3 4))
                     (eduction (list 5 6)) (eduction (list 7 8)))

(semigroup-op-associativity (eduction (list 1 2)) (eduction (list 3 4)))

(semigroup-op-associativity (eduction (list 1 2)) (eduction (list 3 4))
                            (eduction (list 5 6)) (eduction (list 7 8)))

(monoid-identity-law (eduction (list 1 2)))

;;----------------------- Seq --------------------------

(magma-op-keeps-type (seq (list 1 2)) (seq (list 3 4)))

(magma-op-keeps-type (seq (list 1 2)) (seq (list 3 4)) (seq (list 5 6)) (seq (list 7 8)))

(semigroup-op-associativity (seq (list 1 2)) (seq (list 3 4)))

(semigroup-op-associativity (seq (list 1 2)) (seq (list 3 4)) (seq (list 5 6)) (seq (list 7 8)))

(monoid-identity-law (seq (list 1 2)))

;;----------------------- Set --------------------------

(magma-op-keeps-type #{1 2} #{3 4})

(magma-op-keeps-type #{1 2} #{3 4} #{5 6})

(semigroup-op-associativity #{1 2} #{3 4})

(semigroup-op-associativity #{1 2} #{3 4} #{5 6})

(monoid-identity-law #{1 2})

;;----------------------- Map --------------------------

(magma-op-keeps-type {:a 1} {:b 2})

(magma-op-keeps-type {:a 1} {:b 2 :c 3} {:d 5} {:e 6})

(semigroup-op-associativity {:a 1} {:b 2})

(semigroup-op-associativity {:a 1} {:b 2 :c 3} {:d 5} {:e 6})

(monoid-identity-law {:a 1 :b 2})

;;----------------------- MapEntry --------------------------

(magma-op-keeps-type (first {:a 1}) (first {:b 2}))

(magma-op-keeps-type (first {:a 1}) (first {:b 2 :c 3}) (first {:d 5}) (first {:e 6}))

(semigroup-op-associativity (first {:a 1}) (first {:b 2}))

(semigroup-op-associativity (first {:a 1}) (first {:b 2 :c 3}) (first {:d 5}) (first {:e 6}))

(monoid-identity-law (first {:a 1}))

;;----------------------- String --------------------------

(magma-op-keeps-type "something" "else")

(magma-op-keeps-type "something" "else" "even" "more")

(semigroup-op-associativity "something" "else")

(semigroup-op-associativity "something" "else" "even" "more")

(monoid-identity-law "something")

;;----------------------- Number --------------------------

(magma-op-keeps-type 1 2)

(magma-op-keeps-type 1 2 3 4)

(semigroup-op-associativity 1 2)

(semigroup-op-associativity 1 2 3 4)

(monoid-identity-law 4)

;;----------------------- Keyword --------------------------

(magma-op-keeps-type :something :else)

(magma-op-keeps-type :something :else :even :more)

(semigroup-op-associativity :something :else)

(semigroup-op-associativity :something :else :even :more)

(monoid-identity-law :something)

;;----------------------- Atom --------------------------

(magma-op-keeps-type (atom 1) (atom 2))

(magma-op-keeps-type (atom 2) (atom 3) (atom 4))

(semigroup-op-associativity (atom 5) (atom 6))

(semigroup-op-associativity (atom 7) (atom 8) (atom 9))

(monoid-identity-law (atom 4))

;;----------------------- Volatile --------------------------

(magma-op-keeps-type (volatile! 1) (volatile! 2))

(magma-op-keeps-type (volatile! 2) (volatile! 3) (volatile! 4))

(semigroup-op-associativity (volatile! 5) (volatile! 6))

(semigroup-op-associativity (volatile! 7) (volatile! 8) (volatile! 9))

(monoid-identity-law (volatile! 4))

;;----------------------- Ref --------------------------

(dosync (magma-op-keeps-type (ref 1) (ref 2)))

(dosync (magma-op-keeps-type (ref 2) (ref 3) (ref 4)))

(dosync (semigroup-op-associativity (ref 5) (ref 6)))

(dosync (semigroup-op-associativity (ref 7) (ref 8) (ref 9)))

(dosync (monoid-identity-law (ref 5)))

;;------------------- Functions --------------------------

(facts "Magma op keeps type"
       (op + *) => fn?
       (op + * /) => fn?)

(facts "Function semigroup op associativity."
       ((op (op inc (partial * 10)) +) 7 9) => ((op inc (op (partial * 10) +)) 7 9))

(monoid-identity-law +)

;;-------------------- CurriedFn ----------------------

(let [c+ (curry +)
      c* (curry *)
      cdiv (curry /)]

  (magma-op-keeps-type c+ c*)

  (magma-op-keeps-type c+ c* cdiv)

  (facts "CurriedFn semigroup op associativity."
         ((op (op (c+ 2) (cdiv 3)) (c* 5)) 7 9) => ((op (c+ 2) (op (cdiv 3) (c* 5))) 7 9))

  (monoid-identity-law c+))

;;----------------- Monoid fold -------------------------

(facts "Monoids are used by collection fold."
       (fold [[1] [2]]) => [1 2]
       (fold [(list 1) (list 2)]) => (list 1 2)
       (fold [(seq (list 1)) (seq (list 2))]) => (seq (list 1 2))
       (fold [(lazy-seq (list 1)) (lazy-seq (list 2))]) => (lazy-seq (list 1 2))
       (fold [#{1} #{2}]) => #{1 2}
       (fold [{:a 1} {:b 2}]) => {:a 1 :b 2}
       (fold [:a :b :c]) => :abc
       (fold ["a" "b" "c"]) => "abc"
       (fold [1 2 3]) => 6
       (fold [(just 1) (just 2) (just 3)]) => (just 6)
       (fold [(atom 1) (atom 2) (atom 3)]) => (check-eq (atom 6))
       (fold [(ref 1) (ref 2) (ref 3)]) => (check-eq (ref 6))
       (fold [(first {:a 1}) (first {:a 2}) (first {:b 3})]) => (check-eq (first {:aab 6})))

;;===================== Foldable =====================

(facts "How Foldable collections work."
       (fold [1 2 3 4 5]) => 15
       (fold (vec (range 999))) => 498501
       (fold []) => nil
       (fold (list 1 2 3 4 5))= > 15
       (fold (r/map inc [1 2 3 4 5])) => 20
       (fold (list 1 2 3 4 5)) => 15
       (fold (lazy-seq (list 1 2 3 4 5))) => 15
       (fold (eduction (list 1 2 3 4 5))) => 15
       (fold (seq (list 1 2 3 4 5))) => 15
       (fold {:a 1 :b 2 :c 3}) => 6
       (fold #{1 2 3 4 5}) => 15

       (fold + [1 2 3 4]) => 10
       (fold * [1 2 3 4]) => 24
       (fold + 0 [1 2 3] [4 5 6]) => 21
       (fold * 1 [1 2 3] [4 5 6]) => 315
       (fold * 2 [1 2 3] [4 5 6] [7 8 9] [1 2 1] [3 2 1]) => 12160
       (fold * 2 [1 2 3 4] [4 5 6] [7 8 9] [1 2 1] [3 2 1]) => 12160

       (foldmap inc [1 2 3 4 5]) => 20
       (foldmap inc []) => nil
       (foldmap inc (list 1 2 3 4 5))= > 20
       (foldmap inc (r/map inc [1 2 3 4 5])) => 25
       (foldmap inc (list 1 2 3 4 5)) => 20
       (foldmap inc (lazy-seq (list 1 2 3 4 5))) => 20
       (foldmap inc (eduction (list 1 2 3 4 5))) => 20
       (foldmap inc (seq (list 1 2 3 4 5))) => 20
       (foldmap inc {:a 1 :b 2 :c 3}) => 9
       (foldmap inc #{1 2 3 4 5}) => 20

       (foldmap * 2 + [1 2 3] [4 5 6] [7 8 9] [1 2 1] [3 2 1]) => 12160)

(facts "How Foldable arrays work."
       (fold (double-array [1 2 3 4 5])) => 15.0
       (fold (long-array (range 999))) => 498501
       (fold (short-array [])) => 0
       (fold (int-array [1 2 3 4 5]))= > 15
       (fold (float-array [1 2 3 4 5])) => 15.0

       (fold + (double-array [1 2 3 4])) => 10.0
       (fold * (float-array [1 2 3 4])) => 24.0
       (fold + 0 (int-array [1 2 3]) (int-array [4 5 6])) => 21
       (fold * 1 (short-array [1 2 3]) (short-array [4 5 6])) => 315
       (fold * 2 (long-array [1 2 3]) (long-array [4 5 6]) (long-array [7 8 9])
             (long-array [1 2 1]) (long-array [3 2 1])) => (throws UnsupportedOperationException)
       (fold * 2 (double-array [1 2 3 4]) (double-array [4 5 6]) (double-array [7 8 9])
             (double-array [1 2 1]) (double-array [3 2 1])) => (throws UnsupportedOperationException)

       (foldmap inc (double-array [1 2 3 4 5])) => 20.0
       (foldmap inc (float-array [])) => 0.0
       (foldmap inc (double-array [1 2 3 4 5])) => 20.0

       (foldmap * 2 + (long-array [1 2 3]) (long-array [4 5 6])
                (long-array [7 8 9]) (long-array [1 2 1]) (long-array [3 2 1]))
       => (throws UnsupportedOperationException))

(fact "fold extract the value from the reference context."
      (fold (atom :something)) => :something
      (foldmap inc (atom 5)) => 6
      (fold (ref :something)) => :something
      (foldmap inc (ref 5)) => 6)

;;=============== Metadata ==========================

(data-structures-should-preserve-metadata inc + vector [1 2 3] [4 5 6])

(data-structures-should-preserve-metadata inc + list (list 1 2 3) (list 4 5 6))

(data-structures-should-preserve-metadata inc + (comp seq list) (seq [1 2 3]) (seq [4 5 6]))

(data-structures-should-preserve-metadata inc + #(lazy-seq (list %))
                                          (lazy-seq (list 1 2 3)) (lazy-seq (list 4 5 6)))

(data-structures-should-preserve-metadata inc + hash-set #{1 2 3} #{4 5 6})

(data-structures-should-preserve-metadata inc + #(hash-map nil %) {:a 2 :b 4} {:a 5 :b 7})

(let [a1 (atom 1 :meta {:test true})
      a2 (atom 2 :meta {:test true})]
  (facts  "All atoms should preserve metadata."
          (meta (fmap! inc a1)) => {:test true}
          (meta (fmap! + a1 a2)) => {:test true}

          (meta (fapply! (pure a1 inc) a1)) => {:test true}
          (meta (fapply! (pure a1 +) a1 a2)) => {:test true}

          (meta (bind! a1 #(atom (inc %)))) => {:test true}
          (meta (bind! a1 a2 #(atom (+ %1 %2)))) => {:test true}

          (meta (join! (atom (atom 1) :meta {:test true}))) => {:test true}))

(let [r1 (ref 1 :meta {:test true})
      r2 (ref 2 :meta {:test true})]
  (facts  "All refs should preserve metadata."
          (dosync
           (meta (fmap! inc r1))) => {:test true}
          (dosync
           (meta (fmap! + r1 r2)) => {:test true})

          (dosync
           (meta (fapply! (pure r1 inc) r1))) => {:test true}
          (dosync
           (meta (fapply! (pure r1 +) r1 r2))) => {:test true}

          (dosync
           (meta (bind! r1 #(ref (inc %))))) => {:test true}
          (dosync
           (meta (bind! r1 r2 #(ref (+ %1 %2))))) => {:test true}

          (dosync
           (meta (join! (ref (ref 1) :meta {:test true})))) => {:test true}))

;; Reducers and MapEntry do not support metadata.

;; =========== Additional core functions =============

(let [c+ (curry +)
      c* (curry *)]

  (facts
   "<*> is a left-associative fapply."
   (<*> (pure [] (curry comp)) [(c+ 2)] [(c* 2)] [1 2 3]) => [4 6 8]))

;;=================== Just ===================================

(functor-law2 inc + (just 5))

(functor-law2 inc + (just 6) (just 99) (just 0))

(applicative-law1 inc (just 5))

(applicative-law1 + (just 5) (just 8))

(applicative-law2-identity (just 5))

(applicative-law3-composition (just inc) (just (partial * 10)) (just 5))

(applicative-law3-composition (just inc) (just (partial * 10)) (just 5) (just 9))

(applicative-law4-homomorphism (just nil) inc 1)

(applicative-law4-homomorphism (just nil) + 5 3)

(applicative-law5-interchange (just nil) inc 1)

(applicative-law5-interchange (just nil) + 5 4 3)

(monad-law1-left-identity (just nil) (comp just inc) 1)

(monad-law1-left-identity (just nil) (comp just +) 1 2 3)

(monad-law2-right-identity (just 5))

(monad-law3-associativity (comp just inc) (comp just (partial * 10)) (just 5))

(magma-op-keeps-type (just :something) (just :else))

(magma-op-keeps-type (just :something) (just :else) (just :even) (just :more))

(semigroup-op-associativity (just :something) (just :else))

(semigroup-op-associativity (just :something) (just :else) (just :even) (just :more))

(monoid-identity-law (just :something))

(facts "nil is the id for Maybe's op"
       (op (just 3) nil) => (just 3)
       (op (just 3) nil (just 5)) => (just 8)
       (op (just 4) (just 6) nil nil) => (just 10)
       (op nil (just 6)) => (just 6)
       (op nil (just 7) (just 8) (just 9)) => (just 24))

(fact "Just's fmap and bind should return nil if any of the arguments is nil"
      (fmap + (just 1) (just 2) (just 3) nil) => nil
      (bind (just 2) + nil (just 3)) => nil)

(fact "join should remove nesting contexts."
      (join (just 5)) => (just 5)
      (join (just (just (just 5)))) => (just (just 5)))

(fact "fold extracts the value from the Just context."
      (fold (just :something)) => :something
      (foldmap inc (just 5)) => 6)

(facts
 "Automatic binding of context for return and unit"

 (let [curried (curry identity)
       returning-f (fn ([x]
                        (return (inc x)))
                     ([x & ys]
                      (return (apply + x ys))))]

   (bind (vec (range 1 500)) returning-f) => (range 2 501)

   (bind (vec (range 1 500)) (vec (range 1 500)) (vec (range 1 500)) returning-f) => (range 3 1500 3)

   (bind (apply list (range 1 500)) returning-f) => (range 2 501)

   (bind (apply list (range 1 500)) (apply list (range 1 500)) (apply list (range 1 500)) returning-f)
   => (range 3 1500 3)

   ;; ===== TODO: clojure.lang.Range has been introduced in 1.7.0-beta1
   ;; instead of LazySeq. It seems that it does something multithreaded
   ;; since it fails if range is larger than 32
   #_((bind (range 1 500) returning-f)
      => (range 2 501)

      (bind  (range 1 500)
             (range 1 500)
             (range 1 500)
             returning-f)
      => (range 3 1500 3))

   (bind (vec (range 1 500)) returning-f) => (range 2 501)

   (bind (vec (range 1 500)) (range 1 500) (range 1 500) returning-f)
   => (range 3 1500 3)

   (bind (seq (apply list (range 1 500))) returning-f) => (range 2 501)

   (bind (seq (apply list (range 1 500))) (seq (apply list (range 1 500)))
         (seq (apply list (range 1 500))) returning-f)
   => (range 3 1500 3)

   (bind (apply hash-set (range 1 500)) returning-f) => (apply hash-set (distinct (range 2 501)))

   (bind (apply hash-set (range 1 500)) (apply hash-set (range 1 500))
         (apply hash-set (range 1 500)) returning-f)
   => (apply hash-set (distinct (range 3 1500 3)))

   (bind (apply hash-map (interleave (range 1 500) (range 1 500))) returning-f)
   => (apply hash-map (interleave (range 1 500) (range 2 501)))

   (bind (apply hash-map (interleave (range 1 500) (range 1 500)))
         (apply hash-map (interleave (range 1 500) (range 1 500)))
         (apply hash-map (interleave (range 1 500) (range 1 500))) returning-f)
   => (apply hash-map (interleave (range 1 500) (range 3 1500 3)))

   (realize [] (bind (r/map identity (vec (range 1 500))) returning-f)) => (range 2 501)

   (bind (first {:a 1}) returning-f) => (first {:a 2})

   (bind (atom 1) returning-f) => (check-eq (atom 2))

   (dosync (bind (ref 1) returning-f)) => (check-eq (ref 2))

   ((bind (pure curried 3) returning-f) 7) => 4

   ((bind (pure curried 3) (pure curried 6) (pure curried 9) returning-f) 7) => 18))

(fact
 "Syntactic sugar for the bind function - mdo"

 (mdo [] (+ 1 2)) => 3

 (mdo [a [1 2 3]
       b [4 5 6]
       c [7 8 9]]
      (return (* a b c)))
 => (bind [1 2 3]
          (fn [a]
            (bind [4 5 6]
                  (fn [b]
                    (bind [7 8 9]
                          (fn [c]
                            (pure [7 8 9] (* a b c))))))))

 (mdo [a [1 2 3]
       b [4 5 6]
       c [7 8 9]]
      (unit (* a b c)))
 => (bind [1 2 3]
          (fn [a]
            (bind [4 5 6]
                  (fn [b]
                    (bind [7 8 9]
                          (fn [c]
                            (pure [7 8 9] (* a b c)))))))))

(set! *unchecked-math* unchecked-math)
