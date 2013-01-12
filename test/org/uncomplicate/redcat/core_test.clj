(ns org.uncomplicate.redcat.core-test
  (:use midje.sweet
        [org.uncomplicate.redcat core jvm])
  (:require [clojure.data.generators :as gen])
  (:require [clojure.string :as s]))

(defn gen-fn [& fs]
  (first (apply gen/one-of (map vector fs))))

;====================================== Functor tests ==========================================
(defmacro functor-law2 [fgen xgen]
  `(formula "Second functor law."
            [f1# ~fgen
             f2# ~fgen
             x# ~xgen]
            (fmap (comp f1# f2#) x#) => (fmap f1# (fmap f2# x#))))

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
;============================= Applicative tests ================================================


