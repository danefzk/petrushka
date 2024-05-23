(ns petrushka.main-test
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.protocols :as protocols]
            [syntheticmusicology.petrushka.auto :as main :refer [bind ?> satisfy]]
            [petrushka.types :as types]
            [syntheticmusicology.petrushka.shared :as api :refer [fresh]]
            [petrushka.utils.test :refer [throws?]]))


(tests "conjunction does not stack overflow on many clauses"
 false := (throws?
           (let [n 15]
             (->> (for [x (range n)
                        y (range n)
                        :let [a (fresh)
                              b (fresh)]]
                    (?> (and
                         (= a x)
                         (= b y)
                         (= (+ a b) (+ x y)))))
                  (apply main/conjunction)))))