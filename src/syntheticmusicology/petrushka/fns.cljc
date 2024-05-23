(ns syntheticmusicology.petrushka.fns
  (:require [petrushka.terms.introduced :as terms.introduced]
            [syntheticmusicology.petrushka.auto :as auto]
            [syntheticmusicology.petrushka.shared :as api]))

(defn conjunction [& args]
  (loop [expr (first args)
         more (rest args)]
    (if (seq more)
      (recur
       (auto/?>
        (and expr (first more)))
       (rest more))
      expr))
  #_(apply api/conjunction args) ;; todo - though their implementations are the same, calling the impl function makes some tests fail. why?
  )

(defn disjunction [& args]
  (loop [expr (first args)
         more (rest args)]
    (if (seq more)
      (recur
       (auto/?>
        (or expr (first more)))
       (rest more))
      expr)))

(defmacro ^:introduced forall [[bind set-expr] constraint-expr]
  `(let [~bind (api/lexical (api/fresh))]
     (auto/?>
      (terms.introduced/forall
       '~bind
        [~bind ~set-expr ~constraint-expr]))))

(defmacro ^:introduced for-set [[bind set-expr] generator-expr]
  `(let [~bind (api/lexical (api/fresh))]
     (auto/?>
      (terms.introduced/for-set
       '~bind
        [~bind ~set-expr ~generator-expr]))))