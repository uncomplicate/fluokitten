;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.fluokitten.articles.getting-started-test
  (:use [uncomplicate.fluokitten jvm core test])
  (:use [midje.sweet :exclude [just]]))

(facts "These expressions are used as examples in the
        Getting Started guide at the Fluokitten web site."

       (fmap inc [1 2 3]) => [2 3 4]

       (fmap + [1 2 3] [1 2 3 4]) => [2 4 6]

       (fmap + (list 1 2 3) [1 2] (sorted-set  5 3 1 2)) => (list 3 6)

       (fmap + {:a 1 :b 2} {:a 3 :c 4} {:d 5})
       => {:a 4 :b 2 :c 4 :d 5}

       (fmap * (atom 2) (ref 3) (atom 4))
       => (check-eq (atom 24))

       ((fmap inc *) 2 3) => 7

       (fapply [inc dec (partial * 10)] [1 2 3])
       => [2 3 4 0 1 2 10 20 30]

       (fapply [+ -] [1 2] [3 4]) => [4 6 -2 -2]

       (fapply {:a + :b *} {:a 1 :b 2} {:a 3 :b 3 :c 4} {:d 5})
       => {:a 4, :b 6, :c 4, :d 5}

       (pure [] 3) => [3]

       (pure [] 1 2 3) => [1 2 3]

       (pure (atom nil) 5) => (check-eq (atom 5))

       (bind [1 2 3] #(vector (inc %) (dec %)))
       => [2 0 3 1 4 2]

       (bind (atom 1) (comp return inc))
       => (check-eq (atom 2))

       (id 4) => 0

       (op 1 2) => 3

       (id [4 5 6]) => []

       (op [1 2] [3]) => [1 2 3]

       (fold (atom 3)) => 3

       (fold []) => nil

       (fold [1 2 3]) => 6

       (fold [[1 2 3] [3 4 5 4 3] [3 2 1]])
       => [1 2 3 3 4 5 4 3 3 2 1]

       (fold (fold [[1 2 3] [3 4 5 4 3] [3 2 1]]))
       => 31

       (fold op [] [[1] [2] [3 4]] [[4] [5 6] [6 66]] [[22 33]])
       => [1 4 22 33 2 5 6 3 4 6 66]

       (foldmap + 0 count [[1] [2] [3 4]]))
