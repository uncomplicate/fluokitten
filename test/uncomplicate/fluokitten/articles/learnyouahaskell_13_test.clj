;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.fluokitten.articles.learnyouahaskell-13-test
  "These expressions are used as examples in the
    Larn You a Haskell for Great Good
    article at the Fluokitten web site."
  (:use [uncomplicate.fluokitten jvm core test])
  (:require [uncomplicate.fluokitten.protocols :as protocols])
  (:use [midje.sweet :exclude [just]]))


(facts
 "Examples from the LYAH book, chapter 13,
  the Writer? I hardly know her! section"


 )

(facts
 "Examples from the LYAH book, chapter 13,
  Some useful monadic functions section"

 ;;TODO The first few functions are obsolete in Clojure.
 ;;Write the examples with fmap and fapply once Writer and
 ;; Reader are implemented.
 )

(deftype Prob [xs]
  Object
  (hashCode [_]
    (hash xs))
  (equals [this that]
    (or (identical? this that)
        (and (instance? Prob that)
             (= xs (.xs ^Prob that)))))
  protocols/Functor
  (fmap [_ f]
    (Prob. (fmap (fn [[x p]]
                   [(f x) p])
                 xs)))
  protocols/Applicative
  (pure [_ v]
    (Prob. [[v, 1]]))
  protocols/Monad
  (join [_]
    (let [multi-all (fn [[innerxs p]]
                      (map (fn [[x r]]
                             [x (* p r)])
                           (.xs innerxs)))]
      (Prob. (apply concat (map multi-all xs)))))
  (bind [p f]
    (join (fmap f p)))
  protocols/Foldable
  (fold [_]
    (fold (fmap first xs))))

(defn prob [& xs]
  (if (= 1 (reduce (fn [sum [x p]] (+ sum p)) 0 xs))
    (Prob. xs)
    nil))

(facts
 "Examples from the LYAH book, chapter 13, Making monads section"
 (fmap - (Prob. [[3 1/2] [5 1/4] [9 1/4]]))
 => (Prob. [[-3 1/2] [-5 1/4] [-9 1/4]])

 (prob [3 1/2] [5 1/4] [9 1/4])
 => (Prob. [[3 1/2] [5 1/4] [9 1/4]])

 (prob [:tails 3/4] [:heads 1/2])
 => nil

 (pure (Prob. []) :something)
 => (prob [:something 1])

 (def this-situation (prob [(prob [:a 1/2] [:b 1/2]) 1/4]
                           [(prob [:c 1/2] [:d 1/2]) 3/4]))

 (join this-situation)
 => (prob [:a 1/8] [:b 1/8] [:c 3/8] [:d 3/8])

 (defn coin [] (prob [:heads 1/2] [:tails 1/2]))

 (defn loaded-coin [] (prob [:heads 1/10] [:tails 9/10]))

 (mdo [a (coin)
       b (coin)
       c (loaded-coin)]
      (return (not (some #(= :heads %) [a b c]))))

 => (prob [false 1/40] [false 9/40] [false 1/40] [false 9/40]
          [false 1/40] [false 9/40] [false 1/40] [true 9/40]))
