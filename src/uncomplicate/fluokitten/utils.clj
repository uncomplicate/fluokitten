(ns ^{:doc "TODO"
      :author "Dragan Djuric"}
  uncomplicate.fluokitten.utils
  (:require [clojure.core.reducers :as r]))

(def ^{:dynamic true :private true} *pure-context*)

(defn get-context []
  "Fetches the currently valid monadic context, if any.
   If no context is currently bound, returns
   Var$Unbound."
  *pure-context*)

(defmacro with-context
  "Establishes the monadic context that can be accessed
   with the get-context function in the dynamic scope inside
   the body."
  [context & body]
  `(binding [*pure-context* ~context]
     ~@body))

(defn with-current-context
  "Creates a function that applies function f with the supplied arguments,
   while providing the currently valid dynamic scoped monadic context
   inside f's lexical scope."
  [f]
  (let [current-context (get-context)]
    (fn [& args]
      (with-context current-context
        (apply f args)))))

(defn reducible
  "Creates an identity reducible function from the collection c."
  [c]
  (r/map identity c))

(defn realize [c cr]
  "Realizes the reducible function cr into a collection
   that has the same type as c, while enabling monadic context
   to be used inside the lexical scope of cr."
  (let [res (empty c)]
    (with-context res
      (into res cr))))

(defn reducible?
  "Checks whether x is an instance of the CollReduce protocol."
  [x]
  (instance? clojure.core.protocols.CollReduce x))

(declare deref?)
