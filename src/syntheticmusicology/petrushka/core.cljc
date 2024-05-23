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

(defn if-constructor [& args]
  (terms.core/->TermIf (vec args)))

(defmethod protocols/rewrite-symbol 'if [_] if-constructor)

(defmacro if 
  "note: 'if' is a special form in the clojure compiler.
   Trying to import this macro into another namespace using :refer [if] will not overwrite the speical form.
   Instead, fully qualify the symbol when you intend to use it: 

   (:require [this-namespace :as ns])
   (ns/if ...args)
   "
  [& args#]
  `(if (some api/cacheing-decisions ~(vec args#))
     (api/cacheing-validate (apply if-constructor ~(vec args#)))
     ~(apply list 'if args#)))


