;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.fluokitten.articles.implementing_protocols_test
   "These expressions are used as examples in the
    Implementing Fluokitten Protocols
    article at the Fluokitten web site."
   (:use [uncomplicate.fluokitten jvm core test])
   (:require [clojure.core.reducers :as r])
   (:use [midje.sweet :exclude [just]]))
