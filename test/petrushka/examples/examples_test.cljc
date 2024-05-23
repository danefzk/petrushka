(ns petrushka.examples.examples-test
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.protocols :as protocols]
            [syntheticmusicology.petrushka.auto :as main :refer [bind ?> satisfy solve-for]]
            [syntheticmusicology.petrushka.shared :refer [fresh]]
            [petrushka.types :as types]
            [syntheticmusicology.petrushka.fns :as fns :refer [conjunction]]
            [petrushka.solver :as solver]
            [petrushka.utils.test :refer [throws?]]
            [hyperfiddle.rcf :as rcf]))


(tests 
 (let [mesos (take 5 (repeatedly fresh))
       cluster-free (fn [set-decision]
                      (?>
                       (fns/forall [a (bind (range 12) set-decision)]
                         (when (contains? set-decision (mod (+ a 1) 12))
                           (not (contains? set-decision (mod (+ a 2) 12)))))))
       constraint (?>
                   (apply
                    conjunction
                    (concat
                     (->> mesos
                          (partition 2 1)
                          (map
                           (fn [[a b]]
                             (and
                              (not= a b)
                              (= (count (clojure.set/intersection a b)) 3)))))
                     (map cluster-free mesos)
                     (map (comp (partial = 4) count) mesos))))
       solution (satisfy constraint) 
       mesos* (map solution mesos)]
   true := 
   (and (every?
         true?
         (map
          (fn [x]
            (= 4 (count x)))
          mesos*))
        (every?
         true?
         (map
          (fn [[a b]]
            (and
             (not= a b)
             (= 3 (count (clojure.set/intersection a b)))))
          (partition 2 1 mesos*)))))

  )
