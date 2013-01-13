(ns org.uncomplicate.redcat.core-test
  (:use midje.sweet
        org.uncomplicate.redcat.core)
  (:use org.uncomplicate.redcat.jvm)
  (:require [clojure.data.generators :as gen])
  (:require [clojure.string :as s]))
(comment
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
            (fmap (comp f1# f2#) x#) => (fmap f1# (fmap f2# x#)))))

(defmacro fmap-keeps-type [f xgen]
  `(formula "fmap should return data of the same type as the functor argument."
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
(functor-law2 (gen-fn inc (partial * 100)) (gen/int))
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
(functor-law2 (gen-fn s/capitalize s/lower-case s/upper-case s/reverse) (gen/string))
(fmap-keeps-type s/reverse (gen/string))

;Functor functions on a vector
(functor-law2 (gen-fn (partial * 100) inc) (gen/vec (gen/int)))
(fmap-keeps-type inc (gen/vec (gen/int)))

;Functor functions on a list
(functor-law2 (gen-fn (partial * 100) inc) (gen/list (gen/int)))
(fmap-keeps-type inc (gen/list (gen/int)))

;Functor functions on a set
(functor-law2 (gen-fn (partial * 100) inc) (gen/set (gen/int)))
(fmap-keeps-type inc (gen/set (gen/int)))

;Functor functions on a seq
;Due to Clojure implementation details, the type of the result does not have to be the same
;as the type of the input seq. However, keeping the type is not required by any Functor law.
;Both seqs implement the seq interface, though.
(functor-law2 (gen-fn (partial * 100) inc) (seq (gen/list (gen/int))))

;Functor functions on a lazy seq
(functor-law2 (gen-fn (partial * 100) inc) (lazy-seq (gen/list (gen/int))))
(fmap-keeps-type inc (lazy-seq (gen/list (gen/int))))

;Functor functions on a map entry
(functor-law2 (gen-fn (partial * 100) inc) (clojure.lang.MapEntry. (gen/keyword) (gen/int)))
(fmap-keeps-type inc (clojure.lang.MapEntry. (gen/keyword) (gen/int)))

;Functor functions on a map (depends on proper behavior of map entries as functors)
(functor-law2 (fmap (gen-fn (partial * 100) inc)) (gen/hash-map (gen/keyword) (gen/int)))
(fmap-keeps-type (fmap inc) (gen/hash-map (gen/keyword) (gen/int)))

;Functor functions on an atom
(functor-law2 (gen-fn (partial * 100) inc) (atom (gen/int)))
(fmap-keeps-type inc (atom (gen/int)))

;Functor functions on an atom
(dosync (functor-law2 (gen-fn (partial * 100) inc) (ref (gen/int))))
(dosync (fmap-keeps-type inc (ref (gen/int))))

;Functor functions on a function
;not a proper test at all!!(functor-law2 (gen-fn (partial * 100) inc) (gen-fn (partial * 44) dec) (gen/int))

;============================= Applicative tests ================================================

;Applicative functions on an Object
;(facts
 ; (pure 3 1) => 1)

(defmacro applicative-law1 [fgen xgen]
  `(formula "First applicative law."
            [f# ~fgen
             x# ~xgen]
            (<*> (pure x# f#) x#) => (fmap f# x#)))

(defmacro applicative-law2 [xgen]
  `(formula "Second applicative law."
            [x# ~xgen]
            (<*> (pure x# identity) x#) => x#))

;Applicative function on a vector
(applicative-law1 (gen-fn inc (partial * 10)) (gen/vec (gen/int)))
(applicative-law2 (gen/vec (gen/int)))

(fact
  (<*> [+] [1 2 3] [10 20 30] [100 200 300]) => [111 222 333])
)
(fact ((fmap inc dec dec) 1) => 0)