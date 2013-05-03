(ns org.uncomplicate.redcat.utils
  (:require [clojure.core.reducers :as r]))

(defn deref? [x]
  (instance? clojure.lang.IDeref x))

(defn reducible? [x]
  (instance? clojure.core.protocols.CollReduce x))

(defn arg-counts [f]
  (map alength (map (fn [^java.lang.reflect.Method m]
                      ( .getParameterTypes m))
                    (.getDeclaredMethods
                     ^java.lang.Class (class f)))))

(defn reducible [c]
  (r/map identity c))

(defn realize [c cr]
  (into (empty c) cr))
