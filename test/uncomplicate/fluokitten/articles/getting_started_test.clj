(ns uncomplicate.fluokitten.articles.getting-started-test
  (:use [uncomplicate.fluokitten algo jvm core test])
  (:use [midje.sweet :exclude [just]]))

(facts "These expressions are used as examples in the
        Getting Started guide at the Fluokitten web site."

       (fmap inc [1 2 3]) => [2 3 4]

       )
