(ns uncomplicate.fluokitten.articles.functors-applicatives-monads-pictures-test
   "These expressions are used as examples in the
        Functors, Applicatives, and Monads in Pictures
        article at the Fluokitten web site."
  (:use [uncomplicate.fluokitten jvm core test])
  (:use [midje.sweet :exclude [just]]))

(facts
 "Introduction"
 2 => 2

 ((partial + 3) 2) => 5

 (just 2) => (just 2)

 ((partial + 3) (just 2)) => (throws ClassCastException)

 (fmap (partial + 3) (just 2)) => (just 5)

 (fmap (partial + 3) nil) => nil)

(def posts {1 {:title "Apples"}})

(defn find-post [post-id]
  (if-let [post (posts post-id)]
    (just post)
    nil))

(defn get-post-title [post]
  "This example is intentionally silly to correspond to the
   intention of the original example. Clojure maps handle
   the nil-map case well, but the real database title fetching
   function might not."
  (post :title))

(facts
 "Database records example with Maybe."

 (fmap get-post-title (find-post 1)) => (just "Apples")

 (fmap get-post-title (find-post 2)) => nil)

(facts
 "Lists and functions as functors."

 (fmap (partial + 3) [2 4 6]) => [5 7 9]

 (let [foo (fmap (partial + 3) (partial + 2))]
   (foo 10) => 15))

(facts
 "Applicatives"

  (let [add3 (partial + 3)]

   (just add3) => (just add3)

   (fapply (just add3) (just 2)) => (just 5)

   (<*> (just add3) (just 2)) => (just 5))

 (<*> [(partial * 2) (partial + 3)] [1 2 3])
 => [2 4 6 4 5 6]

 (fmap (just (partial + 5)) (just 4))
 => (throws ClassCastException)

 (<*> (just (partial + 5)) (just 3))
 => (just 8)

 (fmap * (just 5) (just 3)) => (just 15))

(defn half [x]
  (if (even? x)
    (just (quot x 2))
    nil))

(facts
 "Monads"

 (half 4) => (just 2)

 (half 5) => nil

 (half (just 4)) => (throws IllegalArgumentException)

 (bind (just 3) half) => nil

 (>>= (just 3) half) => nil

 (>>= (just 4) half) => (just 2)

 (>>= nil half) => nil

 (>>= (just 20) half half half) => nil

 (>>= (just 20) half half) => (just 5))
