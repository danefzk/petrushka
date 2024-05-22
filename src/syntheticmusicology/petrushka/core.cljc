(ns syntheticmusicology.petrushka.core
  (:require [petrushka.api :as api]
            [petrushka.terms.core :as terms.core]
            [petrushka.protocols :as protocols]
            [petrushka.utils.symbol :as symbols])
  (:refer-clojure :exclude [+ * when or]))

(defn bind-fn [clojure-fn constructor-fn]
  (defmethod protocols/rewrite-function clojure-fn [_] constructor-fn)
  (partial api/construct-term clojure-fn constructor-fn))

(def + (bind-fn clojure.core/+ terms.core/->TermPlus))

(def * (bind-fn clojure.core/* terms.core/->TermProduct))

(defmacro defmacrobind [macro-symbol constructor-fn]
  `(do
     (defmethod protocols/rewrite-macro
       (symbols/fully-qualify-symbol (symbol (str "clojure.core/" '~macro-symbol)))
       [_#]
       ~constructor-fn)
     (defmacro ~macro-symbol [& args#]
       `(if (some api/cacheing-decisions ~(vec args#))
          (api/cacheing-validate (~~constructor-fn ~(vec args#)))
          ~(apply list (symbol (str "clojure.core/" '~macro-symbol)) args#)))))

(defmacrobind when terms.core/->TermWhen)

(defmacrobind or terms.core/->TermOr)


