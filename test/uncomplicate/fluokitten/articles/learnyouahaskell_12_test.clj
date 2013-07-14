(ns uncomplicate.fluokitten.articles.learnyouahaskell-12-test
  "These expressions are used as examples in the
    Larn You a Haskell for Great Good
    article at the Fluokitten web site."
  (:use [uncomplicate.fluokitten jvm core test])
  (:require [uncomplicate.fluokitten.protocols :as protocols])
  (:use [midje.sweet :exclude [just]]))

(facts
 "Examples from the LYAH book, chapter 12, Walk the line section"

 (defn land-left [n [left right]]
   [(+ left n) right])

 (defn land-right [n [left right]]
   [left (+ right n)])

 (land-left 2 [0 0])
 => [2 0]

 (land-right 1 [1 2])
 => [1 3]

 (land-right -1 [1 2])
 => [1 1]

 (land-left 2 (land-right 1 (land-left 1 [0 0])))
 => [3 1]

 (->> [0 0] (land-left 1) (land-right 1) (land-left 2))
 => [3 1]

 (land-left 10 [0 3])
 => [10 3]

 (->> [0 0] (land-left 1) (land-right 4) (land-left -1) (land-right -2))
 => [0 2]

 (def land-left (curry
                 (fn [n [left right]]
                   (if (< (Math/abs (- (+ left n) right)) 4)
                     (just [(+ left n) right])
                     nil))))


 (def land-right (curry
                  (fn [n [left right]]
                    (if (< (Math/abs (- left (+ right n))) 4)
                      (just [left (+ right n)])
                      nil))))

 (land-left 2 [0 0])
 => (just [2 0])

 (land-left 10 [0 3])
 => nil

 (>>= (land-right 1 [0 0]) (land-left 2))
 => (just [2 1])

 (>>= nil (land-left 2))
 => nil

 (>>= (just [0 0]) (land-right 2) (land-left 2) (land-right 2))
 => (just [2 4])

 (>>= (just [0 0]) (land-left 1) (land-right 4) (land-left -1) (land-right -2))
 => nil

 (defn banana [[_ _]] nil)

 (>>= (just [0 0]) (land-left 1) banana (land-right 1))
 => nil

 (defn >> [m n]
   (bind m (fn [_] n)))

 (>> nil (just 3))
 => nil

 (>> (just 3) (just 4))
 => (just 4)

 (>> (just 3) nil)
 => nil

 (-> (>>= (just [0 0]) (land-left 1)) (>> nil) (>>= (land-right 1)))
 => nil


 (if-let [pole1 (land-left 1 [0 0])]
   (if-let [pole2 (land-right 4 (fold pole1))]
     (if-let [pole3 (land-left 2 (fold pole2))]
       (fold (land-left 1 (fold pole3))))))
 => [4 4])

(facts
 "Examples from the LYAH book, chapter 12, do notation section"

 (>>= (just 3) (fn [x] (just (str x "!"))))
 => (just "3!")

 (>>= (just 3) (fn [x] (>>= (just "!") (fn [y] (just (str x y))))))
 => (just "3!")

 (let [x 3
       y "!"]
   (str x y))
 => "3!"

 (>>= nil (fn [x] (>>= (just "!") (fn [y] (just (str x y)))) ))
 => nil

 (>>= (just 3) (fn [x] (>>= nil (fn [y] (just (str x y)))) ))
 => nil

 (>>= (just 3) (fn [x] (>>= (just "!") (fn [y] nil)) ))
 => nil

 (>>= (just 3) (fn [x]
 (>>= (just "!") (fn [y]
 (just (str x y))))))

 => (mdo [x (just 3)
          y (just "!")]
         (just (str x y)))

 (>>= (just 9) (fn [x] (just (> x 8))))
 => (just true)

 (mdo [x (just 9)]
      (just (> x 8)))
 => (just true)

 (defn routine []
   (mdo [start (just [0 0])
         first (land-left 2 start)
         second (land-right 2 first)]
        (land-left 1 second)))

 (routine) => (just [3 2])

 (mdo [start (just [0 0])
       first (land-left 2 start)
       _ nil
       second (land-right 2 first)]
      (land-left 1 second))
 => nil

 (mdo [[x & _] (just "hello")]
      (return x))
 => (just \h))

(fact
 "Examples from the LYAH book, chapter 12, The list monad section"

 (<*> [(curry *)] [1 2 3] [10 100 1000])
 => [10 100 1000 20 200 2000 30 300 3000]

 (>>= [3 4 5] (fn [x] [x (- x)]))
 => [3 -3 4 -4 5 -5]

 (>>= [] (fn [x] ["bad" "mad" "rad"]))
 => []

 (>>= [1 2 3] (fn [_] []))
 => []

 (>>= [1 2] (fn [n] (>>= [\a \b] (fn [ch] (return [n ch])))))
 => [[1 \a] [1 \b] [2 \a] [2 \b]]

 (mdo [n [1 2]
       ch [\a \b]]
      (return [n ch]))
 => [[1 \a] [1 \b] [2 \a] [2 \b]]

 (for [n [1 2]
       ch [\a \b]]
   [n ch])
 => [[1 \a] [1 \b] [2 \a] [2 \b]]

 (for [x (range 1 50)
       :when (some (partial = \7) (str x))]
   x)
 => [7 17 27 37 47]

 (defn guard [b]
   (if b (return []) (id *pure-context*)))

 (binding [*pure-context* (just nil)]
   (guard (> 5 2))
   => (just [])

   (guard (> 1 2))
   => nil)

 (binding [*pure-context* []]
   (guard (> 5 2))
   => [[]]

   (guard (> 1 2))
   => []

   (>> (guard (> 5 2)) (return "cool"))
   => ["cool"]

   (>> (guard (> 1 2)) (return "cool"))
   => [])

 (>>= (vec (range 1 50)) (fn [x] (>> (guard (some (partial = \7) (str x))) (return x))))
 => [7 17 27 37 47]






 )
