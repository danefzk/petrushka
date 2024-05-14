(ns syntheticmusicology.petrushka.core
  (:require [petrushka.api :as api]
            [petrushka.terms.core :as terms.core]
            [petrushka.main :as main]
            [petrushka.protocols :as protocols])
  (:refer-clojure :exclude [+ *]))

(defn bind-fn [clojure-fn constructor-fn]
  (defmethod protocols/rewrite-function clojure-fn [_] constructor-fn)
  (partial api/construct-term clojure-fn constructor-fn))

(def + (bind-fn clojure.core/+ terms.core/->TermPlus))

(def * (bind-fn clojure.core/* terms.core/->TermProduct))

