(ns uncomplicate.fluokitten.articles.getting-started-test
  (:use [uncomplicate.fluokitten jvm core test])
  (:use [midje.sweet :exclude [just]]))

(facts "These expressions are used as examples in the
        Getting Started guide at the Fluokitten web site."

       (fmap inc [1 2 3]) => [2 3 4]

       (fmap + [1 2 3] [1 2 3 4]) => [2 3 4 5 3 4 5 6 4 5 6 7]

       (fmap + (list 1 2 3) [1 2] (sorted-set 1 2 3 4)) 
       => (list 3 4 5 6 4 5 6 7 4 5 6 7 5 6 7 8 5 6 7 8 6 7 8 9)

       (fmap * (atom 2) (ref 3) (atom 4))
       => (check-eq (atom 24))

       ((fmap inc *) 2 3) => 7

       (fapply [inc dec (partial * 10)] [1 2 3])
       => [2 3 4 0 1 2 10 20 30]

       (fapply [+ -] [1 2] [3 4]) => [4 5 5 6 -2 -3 -1 -2]

       (pure [] 3) => [3]

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
       => 31)
