(ns ^{:doc "TODO"
      :author "Dragan Djuric"}
  uncomplicate.fluokitten.utils
  (:require [clojure.core.reducers :as r]))

(def ^:dynamic *pure-context*)

(defn get-context []
  "TODO"
  *pure-context*)

(defmacro with-context
  "TODO"
  [context & body]
  `(binding [*pure-context* ~context]
     ~@body))

(defn with-current-context
  "TODO"
  [f]
  (let [current-context (get-context)]
    (fn [& args]
      (with-context current-context
        (apply f args)))))

(defn reducible
  "TODO"
  [c]
  (r/map identity c))

(defn realize [c cr]
  "TODO"
  (let [res (empty c)]
    (with-context res
      (into res cr))))

(defn reducible?
  "TODO"
  [x]
  (instance? clojure.core.protocols.CollReduce x))
