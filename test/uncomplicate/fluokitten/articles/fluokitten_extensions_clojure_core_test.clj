(ns uncomplicate.fluokitten.articles.fluokitten-extensions-clojure-core-test
   "These expressions are used as examples in the
    Fluokitten Extensions of Clojure Core
    article at the Fluokitten web site."
  (:use [uncomplicate.fluokitten jvm core test])
  (:require [clojure.core.reducers :as r])
  (:use [midje.sweet :exclude [just]]))

(facts
 "Data structures: Functor"

 (fmap inc []) => []

 (fmap inc [1 2 3]) => [2 3 4]

 (fmap + [1 2] [3 4 5] [6 7 8]) => [10 13]

 (fmap inc (list)) => (list)

 (fmap inc (list 1 2 3)) => (list 2 3 4)

 (fmap + (list 1 2) (list 3 4 5) (list 6 7 8))
 => (list 10 13)

 (fmap inc (empty (seq [1]))) => (empty (seq [2]))

 (fmap inc (seq [1 2 3])) => (seq [2 3 4])

 (fmap + (seq [1 2]) (seq [3 4 5]) (seq [6 7 8]))
 => (seq [10 13])

 (fmap inc (lazy-seq [])) => (lazy-seq [])

 (fmap inc (lazy-seq [1 2 3])) => (lazy-seq [2 3 4])

 (fmap + (lazy-seq [1 2])
       (lazy-seq [3 4 5])
       (lazy-seq [6 7 8]))
 => (lazy-seq [10 13])

 (fmap inc {}) => {}

 (fmap inc {:a 1 :b 2 :c 3}) => {:a 2 :b 3 :c 4}

 (fmap + {:a 1 :b 2} {:a 3 :b 4 :c 5} {:a 6 :b 7 :c 8})
 => {:a 10 :b 13 :c 13}

 (fmap inc (first {:a 1}))
 => (first {:a 2})

 (fmap + (first {:a 1}) (first {:a 3}) (first {:b 6}))
 => (first {:a 10})


 (into [] (fmap inc (r/map identity  [1 2 3])))
 => [2 3 4]

 (into [] (fmap + (r/map identity [1 2])
                (r/map identity [3 4 5])
                (r/map identity [6 7 8])))
 => (throws UnsupportedOperationException))

(facts
 "Data structures: Applicative"

 (pure [4 5] 1) => [1]

 (pure (list) 1) => (list 1)

 (pure (seq [3]) 1) => (seq [1])

 (pure (lazy-seq [4]) 1) => (lazy-seq [1])

 (pure #{} 1) => #{1}

 (pure {} 1) => {nil 1}

 (pure (first {1 1}) 1) => (first {nil 1})

 (into (list) (pure (r/map identity [2 3]) 1))
 => (list 1)

 (fapply [] []) => []

 (fapply [] [1 2 3]) => []

 (fapply [inc dec] []) => []

 (fapply [inc dec] [1 2 3]) => [2 3 4 0 1 2]

 (fapply [+ *] [1 2 3] [4 5 6]) => [5 7 9 4 10 18]

 (fapply (list) (list)) => (list)

 (fapply (list) (list 1 2 3)) => (list)

 (fapply (list inc dec) (list)) => (list)

 (fapply (list inc dec) (list 1 2 3))
 => (list 2 3 4 0 1 2)

 (fapply (list + *) (list 1 2 3) (list 4 5 6))
 => (list 5 7 9 4 10 18)

 (fapply (empty (seq [2])) (empty (seq [3])))
 => (empty (seq [1]))

 (fapply (empty (seq [33])) (seq [1 2 3]))
 => (empty (seq [44]))

 (fapply (seq [inc dec]) (empty (seq [1])))
 => (empty (seq [3]))

 (fapply (seq [inc dec]) (seq [1 2 3]))
 => (seq [2 3 4 0 1 2])

 (fapply (seq [+ *]) (seq [1 2 3]) (seq [4 5 6]))
 => (seq [5 7 9 4 10 18])

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
 => (lazy-seq [5 7 9 4 10 18])

 (fapply #{} #{}) => #{}

 (fapply #{} #{1 2 3}) => #{}

 (fapply #{inc dec} #{}) => #{}

 (fapply #{inc dec} #{1 2 3}) => #{2 3 4 0 1}

 (fapply #{+ *} #{1 2 3} #{4 5 6}) => #{5 7 9 4 10 18}

 (fapply {} {}) => {}

 (fapply {} {:a 1 :b 2 :c 3}) => {:a 1 :b 2 :c 3}

 (fapply {:a inc} {}) => {}

 (fapply {:a inc :b dec nil (partial * 10)}
         {:a 1 :b 2 :c 3 :d 4 nil 5})
 => {:a 2 :b 1 :c 30 :d 40 nil 50}

 (fapply {nil /  :a + :b *} {:a 1 :c 2} {:a 3 :b 4} {:c 2 :d 5})
 => {:a 4 :b 4 :c 1 :d 1/5}

 (fapply (first {:a inc}) (first {:b 1}))
 => (first {:b 1})

 (fapply (first {:a inc}) (first {:a 1}))
 => (first {:a 2})

 (fapply (first {nil inc}) (first {:a 1}))
 => (first {:a 2})

 (fapply (first {nil inc}) (first {nil 1}))
 => (first {nil 2})

 (fapply (first {nil inc})))

(facts
 "Data structures: Monad"

 (bind [] (comp vector inc)) => []

 (bind [1 2 3] (comp vector inc)) => [2 3 4]

 (bind [1 2 3] [4 5 6] (comp vector +)) => [5 7 9]

 (bind (list) (comp list inc)) => (list)

 (bind (list 1 2 3) (comp list inc)) => (list 2 3 4)

 (bind (list 1 2 3) (list 4 5 6) (comp list +)) => (list 5 7 9)

 (bind (empty (seq [2])) (comp seq vector inc))
 => (empty (seq [3]))

 (bind (seq [1 2 3]) (comp seq vector inc)) => (seq [2 3 4])

 (bind (seq [1 2 3]) (seq [4 5 6]) (comp seq vector +))
 => (seq [5 7 9])

 (bind (lazy-seq [])
       (fn [& args]
         (lazy-seq (vector (apply inc args)))))
 => (lazy-seq [])

 (bind (lazy-seq [1 2 3])
       (fn [& args]
         (lazy-seq (vector (apply inc args)))))
 => (lazy-seq [2 3 4])

 (bind (lazy-seq [1 2 3]) (lazy-seq [4 5 6])
       (fn [& args]
         (lazy-seq (vector (apply + args)))))
 => (lazy-seq [5 7 9])

 (bind #{} (comp hash-set inc)) => #{}

 (bind #{1 2 3} (comp hash-set inc)) => #{2 3 4}

 (bind #{1 2 3} #{4 5 6} (comp hash-set +)) => #{5 7 9}

 (bind {} #(hash-map :x %)) => {}

 (bind {:a 1} #(hash-map :increment (inc %)))
 => {[:a :increment] 2}

 (bind {:a 1  :b 2 :c 3} #(hash-map :increment (inc %)))
 => {[:a :increment] 2 [:b :increment] 3 [:c :increment] 4}

 (bind {:a 1} {:a 2 :b 3} {:b 4 :c 5}
       (fn [& args] {:sum (apply + args)}))
 => {[:a :sum] 3 [:b :sum] 7 [:c :sum] 5}

 (bind (first {:a 1}) #(first {:increment (inc %)}))
 => (first {[:a :increment] 2})

 (bind (first {:a 1}) (first {:a 2}) (first {:b 4})
       (fn [& args] (first {:sum (apply + args)})))
 => (first {[:a :sum] 7})

 (join [[1 2] [3 [4 5] 6]]) => [1 2 3 4 5 6]

 (join (list (list 1 2) (list 3 (list 4 5) 6)))
 => (list 1 2 3 4 5 6)

 (join (seq (list (list 1 2) (list 3 (list 4 5) 6))))
 => (seq (list 1 2 3 4 5 6))

 (join (lazy-seq (list (list 1 2) (list 3 (list 4 5) 6))))
 => (lazy-seq (list 1 2 3 4 5 6))

 (join #{#{1 2} #{3 #{4 5} 6}}) => #{1 2 3 4 5 6}

 (join {:a 1 :b {:c 2 :d {:e 3}}}) => {:a 1 [:b :c] 2 [:b :d :e] 3}
 )
