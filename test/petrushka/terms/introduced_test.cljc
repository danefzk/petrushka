(ns petrushka.terms.introduced-test
  (:require [petrushka.protocols :as protocols]
            [petrushka.types :as types]
            [hyperfiddle.rcf :refer [tests]]
            [syntheticmusicology.petrushka.auto :as main]
            [petrushka.utils.test :refer [throws?]]
            [syntheticmusicology.petrushka.fns :as fns]
            [syntheticmusicology.petrushka.shared :as shared :refer [fresh]]
            [petrushka.utils.symbol :as symbols]))


(tests "forall"

  (tests "internal decision is validated as numeric"
    (throws?
     (fns/forall [a (shared/fresh)] (= a #{})))
    := true)


  (tests "internal decision is validated as numeric"
    (throws?
     (fns/forall [a (shared/fresh)] (contains? a 1)))
    := true)

  (tests "internal decision is hidden from external retrieval"
    (count
     (protocols/decisions
      (fns/forall [a (shared/fresh)] (= a 1))))
    := 1)

  (let [x (shared/fresh)
        res (main/satisfy
             (fns/forall [a (main/bind (range 100) x)]
               (= 5 (mod a 12))))]
    (get res x)) := #{65 77 41 89 29 17 5 53}

  (let [cluster-free (fn [set-decision]
                       (main/?>
                        (fns/forall [a (main/bind (range 12) set-decision)]
                          (when (contains? set-decision (mod (+ a 1) 12))
                            (not (contains? set-decision (mod (+ a 2) 12)))))))
        x (shared/fresh)
        res (main/satisfy
             (cluster-free x))
        validate (fn [s]
                   (every?
                    true?
                    (for [e s]
                      (if (contains? s (mod (+ e 1) 12))
                        (not (contains? s (mod (+ e 2) 12)))
                        true))))]
    (validate (get res x)))
  := true)


(tests "for-set"

  (tests "internal decision is validated as numeric"
    (throws?
     (fns/for-set [a (shared/fresh)] (if (= a #{}) 1 2)))
    := true) 

  (tests "internal decision is hidden from external retrieval"
    (count
     (protocols/decisions
      (fns/for-set [a (shared/fresh)] (+ a 1))))
    := 1)

  (let [x (shared/fresh)
        res (main/satisfy
             (=
              #{1 2 3}
              (fns/for-set [a (main/bind (range 12) x)]
                (+ a 1))))]
       (get res x)) := #{0 1 2})