(ns org.uncomplicate.redcat.types-test
  (:use [org.uncomplicate.redcat core types test])
  (:use [midje.sweet :exclude [just]]))

;;--------------- Just ---------------
(functor-law2 inc + (just 5))

(functor-law2 inc + (just 6) (just 99) (just 0))

(applicative-law1 inc (just 5))

(applicative-law1 + (just 5) (just 8))

(applicative-law2-identity (just 5))

(applicative-law3-composition (just inc)
                              (just (partial * 10))
                              (just 5))

(applicative-law3-composition (just inc)
                              (just (partial * 10))
                              (just 5)
                              (just 9))

(applicative-law4-homomorphism (just nil) inc 1)

(applicative-law4-homomorphism (just nil) + 5 3)

(applicative-law5-interchange (just nil) inc 1)

(applicative-law5-interchange (just nil) + 5 4 3)

(monad-law1-left-identity (just nil) (comp just inc) 1)

(monad-law1-left-identity (just nil) (comp just +) 1 2 3)

(monad-law2-right-identity (just 5))

(monad-law3-associativity (comp just inc)
                          (comp just (partial * 10))
                          (just 5))

(fact "Just's fmap and bind should return nil if any of the arguments is nil"
      (fmap + (just 1) (just 2) (just 3) nil) => nil
      (bind (just 2) + nil (just 3)) => nil)

(fact "Join should remove nesting contexts."
      (join (just 5)) => (just 5)
      (join (just (just (just (just 5))))) => (just 5))

;;---------------- State -------------
