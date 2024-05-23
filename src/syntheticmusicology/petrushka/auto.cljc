(ns syntheticmusicology.petrushka.auto
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.protocols :as protocols] 
            [petrushka.solver :as solver]
            [syntheticmusicology.petrushka.core]
            [petrushka.terms.core]
            [petrushka.terms.introduced :as terms.introduced]
            [petrushka.terms.set]
            [petrushka.types :as types]
            [petrushka.utils.string :refer [>>]]
            [petrushka.utils.test :as utils.test]
            [syntheticmusicology.petrushka.shared :as api] 
            :reload))

(comment
  (hyperfiddle.rcf/enable!)
  )

(defmacro satisfy
  ([term]
   `(satisfy ~term {}))
  ([term opts]
   `(solver/solve
     ~opts
     (api/dither ~term)
     nil)))

(tests "satisfy"
 (tests "constraint must be boolean"
        (utils.test/throws? (satisfy (+ (api/fresh) 1))) := true
        (utils.test/throws? (satisfy (= (api/fresh) 1))) := false
        ))

(defmacro
  solve-for
  [sym constraint]
  `(let [~sym (api/fresh)]
     (get
      (satisfy ~constraint)
      ~sym)))

(defmacro maximize
  ([objective constraint]
   `(maximize ~objective ~constraint {}))
  ([objective constraint opts]
   `(solver/solve
     ~opts
     (api/dither ~constraint)
     (api/dither ~objective))))

(tests "maximize"
 (-> (let [a (api/fresh)]
       (maximize a (clojure.core/and (>= a 3000) (= 11 (mod a 12)))))
     first 
     vals
     boolean) 
 := true

  (mod 2147483639 12)

 (tests "objective must be types/Numeric"
        (utils.test/throws? (maximize (= (api/fresh) 1) true)) := true)

 (tests "constraint is required"
        (utils.test/throws? (maximize (api/fresh) nil))
        := true)
 (tests "types are unified across the objective and constraint"
        (let [a (api/fresh)]

          (utils.test/throws? (maximize (+ a 12) (contains? a 12)))
          := true

          (utils.test/throws? (maximize (+ a 12) (contains? #{} a)))
          := false)))

(defmacro ^:introduced ?> 
  "The dither operator.
   dith·er - verb: to be indecisive."
  [form]
  `(api/dither ~form))

;; moves to fns
(defn conjunction [& args]
  (loop [expr (first args)
         more (rest args)]
    (if (seq more)
      (recur
       (?> 
        (and expr (first more)))
       (rest more))
      expr))
  #_(apply api/conjunction args) ;; todo - though their implementations are the same, calling the impl function makes some tests fail. why?
  )

;; moves to fns
(defn disjunction [& args]
  (loop [expr (first args)
         more (rest args)]
    (if (seq more)
      (recur
       (?>
        (or expr (first more)))
       (rest more))
      expr)))

;; moves to fns
(defmacro ^:introduced forall [[bind set-expr] constraint-expr]
  `(let [~bind (api/lexical (api/fresh))]
     (?> (terms.introduced/forall 
          '~bind  
          [~bind ~set-expr ~constraint-expr]))))

;; moves to fns
(defmacro ^:introduced for-set [[bind set-expr] generator-expr]
  `(let [~bind (api/lexical (api/fresh))]
     (?> (terms.introduced/for-set
          '~bind
          [~bind ~set-expr ~generator-expr]))))

;; moves to root
(defn dithered? [x]
  (boolean (api/cacheing-decisions x)))

;; moves to root
(def bind api/bind)

;; moves to root
(defn fresh-set [super]
  (bind super (api/fresh)))

(tests
 (protocols/decisions 1)
 (dithered? (?> (+ (api/fresh) 1))) := true
 (dithered? (?> (+ 1 1))) := false
 )