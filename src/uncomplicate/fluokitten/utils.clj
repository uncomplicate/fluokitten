;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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

(defn split-last [s]
  (loop [ret [] s s]
    (if (next s)
      (recur (conj ret (first s)) (next s))
      [(first s) ret])))
