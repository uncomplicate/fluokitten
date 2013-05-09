(ns org.uncomplicate.redcat.types-test
  (:use [org.uncomplicate.redcat core types test])
  (:use [midje.sweet :exclude [just]]))

;;--------------- nil ---------------
(functor-law2 inc + (just 5))

(functor-law2 inc + (just 6) (just 99) (just 0))
