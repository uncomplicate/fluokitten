(ns uncomplicate.fluokitten.articles.learnyouahaskell-12-test
  "These expressions are used as examples in the
    Larn You a Haskell for Great Good
    article at the Fluokitten web site."
  (:use [uncomplicate.fluokitten jvm core test utils])
  (:require [uncomplicate.fluokitten.protocols :as protocols])
  (:use [midje.sweet :exclude [just]]))

(facts
 "Examples from the LYAH book, chapter 12, A fistful of monads section"

 (<*> (fmap * (just 2)) (just 8))
 => (throws ClassCastException)

 (<*> (fmap (curry *) (just 2)) (just 8))
 => (just 16)

 (fmap * (just 2) (just 8))
 => (just 16)

 (<*> (fmap (curry str 2) (just "klingon")) nil)
 => nil

 (<*> (fmap (curry -) [3 4]) [1 2 3])
 => [2 1 0 3 2 1])

(facts
 "Examples from the LYAH book, chapter 12, A fistful of monads section"

 (fmap #(str % "!") (just "wisdom"))
 => (just "wisdom!")

 (fmap (partial str "!") nil)
 => nil

 (<*> (just (partial + 3)) (just 3))
 => (just 6)

 (<*> nil (just "greed"))
 => nil

 (<*> (just +) nil)
 => nil

 (fmap max (just 3) (just 6))
 => (just 6)

 (fmap max (just 3) nil)
 => nil

 ((fn [x] (just (+ x 1))) 1)
 => (just 2)

 ((fn [x] (just (+ x 100))) 1)
 => (just 101)

 (defn apply-maybe [m f]
   (cond (nil? m) nil
         (maybe? m) (f (fold m))))

 (apply-maybe (just 3) (fn [x] (just (+ x 1))))
 => (just 4)

 (apply-maybe (just "smile") (fn [x] (just (str x " :)"))))
 => (just "smile :)")

 (apply-maybe nil (fn [x] (just (+ x 1))))
 => nil

 (apply-maybe nil (fn [x] (just (str x " :)"))))
 => nil

 (apply-maybe (just 3) (fn [x] (if (> x 2) (just x) nil)))
 => (just 3)

 (apply-maybe nil (fn [x] (if (> x 2) (just x) nil)))
 => nil)

(facts
 "Examples from the LYAH book, chapter 12, Walk the line section"

 (with-context (just "")
   (return "WHAT")
   => (just "WHAT"))

 (>>= (just 9) (fn [x] (return (* x 10))))
 => (just 90)

 (>>= nil (fn [x] (return (* x 10))))
 => nil)

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
   (if b (return []) (id (get-context))))

 (with-context (just nil)

   (guard (> 5 2))
   => (just [])

   (guard (> 1 2))
   => nil)

 (with-context []

   (guard (> 5 2))
   => [[]]

   (guard (> 1 2))
   => []

   (>> (guard (> 5 2)) (return "cool"))
   => ["cool"]

   (>> (guard (> 1 2)) (return "cool"))
   => [])

 ;;== TODO: clojure.lang.Range breaks bind and binding when count > 32
 (>>= (vec (range 1 50))
      (fn [x] (>> (guard (some (partial = \7) (str x))) (return x))))
 => [7 17 27 37 47]

 (mdo
  [x (range 1 50)
   _ (guard (some (partial = \7) (str x)))]
  (return x))

 (defn move-knight [[c r]]
   (mdo [[c' r'] [[(+ c 2) (- r 1)] [(+ c 2) (+ r 1)]
                  [(- c 2) (- r 1)] [(- c 2) (+ r 1)]
                  [(+ c 1) (- r 2)] [(+ c 1) (+ r 2)]
                  [(- c 1) (- r 2)] [(- c 1) (+ r 2)]]
         _ (guard (and (contains? #{1 2 3 4 5 6 7 8} c')
                       (contains? #{1 2 3 4 5 6 7 8} r')))]
        (return [c' r'])))

 (defn move-knight2 [[c r]]
   (let [on-board (fn [[c r]]
                    (and (contains? #{1 2 3 4 5 6 7 8} c)
                         (contains? #{1 2 3 4 5 6 7 8} r)))]
     (filter on-board [[(+ c 2) (- r 1)] [(+ c 2) (+ r 1)]
                       [(- c 2) (- r 1)] [(- c 2) (+ r 1)]
                       [(+ c 1) (- r 2)] [(+ c 1) (+ r 2)]
                       [(- c 1) (- r 2)] [(- c 1) (+ r 2)]])))

 (move-knight [6 2])
 => [[8 1] [8 3] [4 1] [4 3] [7 4] [5 4]]

 (move-knight [8 1])
 => [[6 2] [7 3]]

 (move-knight2 [6 2])
 => [[8 1] [8 3] [4 1] [4 3] [7 4] [5 4]]

 (move-knight2 [8 1])
 => [[6 2] [7 3]]

 (defn in3 [start]
   (mdo [first (move-knight start)
         second (move-knight first)]
        (move-knight second)))

 (defn in3' [start]
   (>>= (move-knight start) move-knight move-knight))

 (defn can-reach-in3 [start end]
   (some (partial = end) (in3 start)))

 (defn can-reach-in3' [start end]
   (some (partial = end) (in3' start)))

 (can-reach-in3 [6 2] [6 1])
 => true

 (can-reach-in3' [6 2] [6 1])
 => true

 (can-reach-in3 [6 2] [7 3])
 => falsey

 (can-reach-in3' [6 2] [7 3])
 => falsey)

(facts
 "Examples from the LYAH book, chapter 12, the Monad laws section"

 (with-context (just nil)

   (>>= (return 3) (fn [x] (just (+ x 1000000))))
   => (just 1000003)

   ((fn [x] (just (+ x 1000000))) 3)
   => (just 1000003))

 (with-context []

   (>>= (return "WoM") (fn [x] [x x x]))
   => ["WoM" "WoM" "WoM"]

   ((fn [x] [x x x]) "WoM")
   => ["WoM" "WoM" "WoM"])

 (>>= (just "move on up") (fn [x] (return x)))
 => (just "move on up")

 (>>= [1 2 3 4] (fn [x] (return x)))
 => [1 2 3 4]

 (>>= (just [0 0]) (land-right 2) (land-left 2) (land-right 2))
 => (just [2 4])

 (>>= (>>= (>>= (just [0 0]) (land-right 2)) (land-left 2)) (land-right 2))
 => (just [2 4])

 (>>= (just [0 0]) (fn [x]
 (>>= (land-right 2 x) (fn [y]
 (>>= (land-left 2 y) (fn [z]
 (land-right 2 z)))))))
 => (just [2 4])

 (let [f (fn [x] [x (- x)])
       g (fn [x] [(* x 3) (* x 2)])
       h (<=< f g)]
   (h 3)
   => [9 -9 6 -6]))
