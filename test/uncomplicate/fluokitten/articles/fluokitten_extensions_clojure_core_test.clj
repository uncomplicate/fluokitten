(ns uncomplicate.fluokitten.articles.fluokitten-extensions-clojure-core-test
   "These expressions are used as examples in the
    Fluokitten Extensions of Clojure Core
    article at the Fluokitten web site."
  (:use [uncomplicate.fluokitten jvm core test utils])
  (:require [clojure.core.reducers :as r])
  (:use [midje.sweet :exclude [just]]))

(facts
 "Data structures: Functor"

 (fmap inc []) => []

 (fmap inc [1 2 3]) => [2 3 4]

 (fmap + [1 2] [3 4 5] [6 7 8]) 
 => [10 11 12 11 12 13 12 13 14 11 12 13 12 13 14 13 14 15]

 (fmap inc (list)) => (list)

 (fmap inc (list 1 2 3)) => (list 2 3 4)
 
 (fmap + (list 1 2) (list 3 4 5) (list 6 7 8))
 => '(10 11 12 11 12 13 12 13 14 11 12 13 12 13 14 13 14 15)

 (fmap inc (empty (seq [1]))) => (empty (seq [2]))

 (fmap inc (seq [1 2 3])) => (seq [2 3 4])

 (fmap + (seq [1 2]) (seq [3 4 5]) (seq [6 7 8]))
 => (seq [10 11 12 11 12 13 12 13 14 11 12 13 12 13 14 13 14 15])

 (fmap inc (lazy-seq [])) => (lazy-seq [])

 (fmap inc (lazy-seq [1 2 3])) => (lazy-seq [2 3 4])

 (fmap + (lazy-seq [1 2])
       (lazy-seq [3 4 5])
       (lazy-seq [6 7 8]))
 => (lazy-seq 
     [10 11 12 11 12 13 12 13 14 11 12 13 12 13 14 13 14 15])

 (fmap inc {}) => {}

 (fmap inc {:a 1 :b 2 :c 3}) => {:a 2 :b 3 :c 4}

 (fmap inc (first {:a 1}))
 => (first {:a 2})

 (into [] (fmap inc (r/map identity  [1 2 3])))
 => [2 3 4])

(facts
 "Data structures: Applicative"

 (pure [4 5] 1) => [1]

 (pure (list) 1) => (list 1)

 (pure (seq [3]) 1) => (seq [1])

 (pure (lazy-seq [4]) 1) => (lazy-seq [1])

 (pure #{} 1) => #{1}

 (with-context []
   (return 1)
   => [1])

 (fapply [] []) => []

 (fapply [] [1 2 3]) => []

 (fapply [inc dec] []) => []

 (fapply [inc dec] [1 2 3]) => [2 3 4 0 1 2]

 (fapply [+ *] [1 2 3] [4 5 6]) => 
 [5 6 7 6 7 8 7 8 9 4 5 6 8 10 12 12 15 18]

 (fapply (list) (list)) => (list)

 (fapply (list) (list 1 2 3)) => (list)

 (fapply (list inc dec) (list)) => (list)

 (fapply (list inc dec) (list 1 2 3))
 => (list 2 3 4 0 1 2)

 (fapply (list + *) (list 1 2 3) (list 4 5 6))
 => (list 5 6 7 6 7 8 7 8 9 4 5 6 8 10 12 12 15 18)

 (fapply (empty (seq [2])) (empty (seq [3])))
 => (empty (seq [1]))

 (fapply (empty (seq [33])) (seq [1 2 3]))
 => (empty (seq [44]))

 (fapply (seq [inc dec]) (empty (seq [1])))
 => (empty (seq [3]))

 (fapply (seq [inc dec]) (seq [1 2 3]))
 => (seq [2 3 4 0 1 2])

 (fapply (seq [+ *]) (seq [1 2 3]) (seq [4 5 6]))
 => (seq [5 6 7 6 7 8 7 8 9 4 5 6 8 10 12 12 15 18])

 (fapply (lazy-seq []) (lazy-seq []))
 => (lazy-seq [])

 (fapply (lazy-seq []) (lazy-seq [1 2 3]))
 => (lazy-seq [])

 (fapply (lazy-seq [inc dec]) (lazy-seq []))
 => (lazy-seq [])

 (fapply (lazy-seq [inc dec]) (lazy-seq [1 2 3]))
 => (lazy-seq [2 3 4 0 1 2])

 (fapply (lazy-seq [+ *])
         (lazy-seq [1 2 3])
         (lazy-seq [4 5 6]))
 => (lazy-seq [5 6 7 6 7 8 7 8 9 4 5 6 8 10 12 12 15 18])

 (fapply #{} #{}) => #{}

 (fapply #{} #{1 2 3}) => #{}

 (fapply #{inc dec} #{}) => #{}

 (fapply #{inc dec} #{1 2 3}) => #{2 3 4 0 1}

 (fapply #{+ *} #{1 2 3} #{4 5 6}) => #{4 5 6 7 8 9 10 12 15 18})

(facts
 "Data structures: Monad"

 (def increment (comp return inc))
 (def add (comp return +))

 (bind [] increment) => []

 (bind [1 2 3] increment) => [2 3 4]

 (bind [1 2 3] [4 5 6] add) => [5 7 9]

 (bind (list) increment) => (list)

 (bind (list 1 2 3) increment) => (list 2 3 4)

 (bind (list 1 2 3) (list 4 5 6) add) => (list 5 7 9)

 (bind (empty (seq [2])) increment)
 => (empty (seq [3]))

 (bind (seq [1 2 3]) increment) => (seq [2 3 4])

 (bind (seq [1 2 3]) (seq [4 5 6]) add)
 => (seq [5 7 9])

 (bind (lazy-seq []) increment)
 => (lazy-seq [])

 (bind (lazy-seq [1 2 3]) increment)
 => (lazy-seq [2 3 4])

 (bind (lazy-seq [1 2 3]) (lazy-seq [4 5 6]) add)
 => (lazy-seq [5 7 9])

 (bind #{} increment) => #{}

 (bind #{1 2 3} increment) => #{2 3 4}

 (bind #{1 2 3} #{4 5 6} add) => #{5 7 9}

 (join [[1 2] [3 [4 5] 6]]) => [1 2 3 [4 5] 6]

 (join (list (list 1 2) (list 3 (list 4 5) 6)))
 => (list 1 2 3 (list 4 5) 6)

 (join (seq (list (list 1 2) (list 3 (list 4 5) 6))))
 => (seq (list 1 2 3 (list 4 5) 6))

 (join (lazy-seq (list (list 1 2) (list 3 (list 4 5) 6))))
 => (lazy-seq (list 1 2 3 (list 4 5) 6))

 (join #{#{1 2} #{3 #{4 5} 6}}) => #{1 2 3 #{4 5} 6})

(facts
 "Data structures: Magma"

 (op [1 2 3] [4 5 6]) => [1 2 3 4 5 6]

 (op [1 2 3] [4 5 6] [7 8 9] [10 11 12])
 => [1 2 3 4 5 6 7 8 9 10 11 12]

 (op (list 1 2 3) (list 4 5 6)) => (list 1 2 3 4 5 6)

 (op (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))
 => (list 1 2 3 4 5 6 7 8 9 10 11 12)

 (op (lazy-seq [1 2 3]) (lazy-seq [4 5 6]))
 => (lazy-seq [1 2 3 4 5 6])

 (op (lazy-seq [1 2 3]) (lazy-seq [4 5 6])
     (lazy-seq [7 8 9]) (lazy-seq [10 11 12]))
 => (lazy-seq [1 2 3 4 5 6 7 8 9 10 11 12])

 (op (seq [1 2 3]) (seq [4 5 6])) => (seq [1 2 3 4 5 6])

 (op (seq [1 2 3]) (seq [4 5 6])
     (seq [7 8 9]) (seq [10 11 12]))
 => (seq [1 2 3 4 5 6 7 8 9 10 11 12])

 (op #{1 2 3 6} #{4 5 6}) => #{1 2 3 4 5 6}

 (op #{1 2 3 6} #{4 5 6} #{7 8 9} #{10 11 12})
 => #{1 2 3 4 5 6 7 8 9 10 11 12}

 (op {:a 1 :b 2} {:a 3 :c 4}) => {:a 3 :b 2 :c 4}

 (op {:a 1 :b 2} {:a 3 :c 4} {:d 5} {:e 6})
 => {:a 3 :b 2 :c 4 :d 5 :e 6}

 (op (first {:a 1}) (first {:b 2})) => (first {:ab 3})

 (op (first {:a 1}) (first {:b 2}) (first {:b 3}))
 => (first {:abb 6}))

(facts
 "Data structures: id."

 (id [2]) => []

 (id (list 4 5 6)) => (list)

 (id (seq [1 2])) => (empty (seq [2]))

 (id (lazy-seq [1 23])) => (lazy-seq [])

 (id #{2 3}) => #{}

 (id {:1 2}) => {}

 (id (first {:a 1})) => [(keyword "") 0])

(facts
 "Data structures: fold."

 (fold [1 2 3 4 5 6]) => 21

 (fold (list "a" "b" "c")) => "abc"

 (fold (seq [:a :b :c])) => :abc

 (fold (lazy-seq [[1] (list 2) (seq [3])])) => [1 2 3]

 (fold #{1 2 3}) => 6

 (fold {:a 1 :b 2 :c 3}) => 6

 (fold (first {:a 1})) => 1)

(facts
 "String: Functor, Magma, Monoid, Foldable."

 (op "some" "thing" " " "else") => "something else"

 (id "something") => "")

(facts
 "Keyword: Functor, Magma, Monoid, Foldable."

 (op :some :thing :else) => :somethingelse

 (id :something) => (keyword ""))

(facts
 "Numbers: Functor, Magma, Monoid, Foldable."

 (op 1 2 3) => 6

 (id 4) => 0)

(facts
 "Functions: Functor."

 ((fmap inc +) 1 2 3) => ((comp inc +) 1 2 3)

 (let [inc+ (fmap inc +)
       cinc+ (fmap inc (curry +))]

   (inc+ 1 2 3) => 7

   (inc+ 1) => 2

   (cinc+ 1 2 3) => 7

   ((cinc+ 1) 2) => 4

   ((inc+ 1) 2) => (throws ClassCastException)))

(facts
 "Functions: Applicative."

 (let [c+ (curry +)
       c* (curry *)]

   (((pure curried inc) 100) 1) => 2

   ((fapply (fapply (pure curried c+) (c+ 3)) (c* 100)) 6) => 609

   ((<*> (pure curried c+) (c+ 3) (c* 100)) 6) => 609))

(facts
 "Functions: Monad."

 ((bind (curry +) (curry *)) 3 4) => 84)

(facts
 "References"

 (fmap inc (atom 3)) => (check-eq (atom 4))

 (fmap inc (ref 5)) => (check-eq (ref 6))

 (fmap + (atom 3) (atom 4) (ref 5)) => (check-eq (ref 12))

 (fmap + (ref 5) (atom 6) (ref 7)) => (check-eq (ref 18))

 (pure (atom 8) 1) => (check-eq (atom 1))

 (pure (ref 8) 2) => (check-eq (ref 2))

 (fapply (atom inc) (atom 1)) => (check-eq (atom 2))

 (fapply (ref inc) (ref 2)) => (check-eq (ref 3))

 (fapply (atom +) (atom 1) (ref 2) (atom 3)) => (check-eq (atom 6))

 (fapply (ref +) (ref 1) (atom 2) (atom 3)) => (check-eq (atom 6))

 (join (atom (ref (atom 33)))) => (check-eq (atom (atom 33)))

 (join (ref (ref (atom 33)))) => (check-eq (ref (atom 33)))

 (bind (atom 8) increment) => (check-eq (atom 9))

 (bind (ref 8) increment) => (check-eq (ref 9))

 (op (atom 3) (atom 4)) => (check-eq (atom 7))

 (op (ref "some") (ref "thing")) => (check-eq (ref "something"))

 (op (atom 1) (ref 2) (atom 3)) => (check-eq (atom 6))

 (id (atom nil)) => (check-eq (atom nil))

 (id (ref 8)) => (check-eq (ref 0))

 (id (ref "something")) => (check-eq (ref ""))

 (fold (atom "something")) => "something"

 (fold (ref 2)) => 2
 )
