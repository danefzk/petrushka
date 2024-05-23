(ns syntheticmusicology.interface-parity-test
  "tests that the auto interface and the encapsulated 
   namespaces return identically"
  (:require [hyperfiddle.rcf :refer [tests]]
            [syntheticmusicology.petrushka.shared :as api]
            [syntheticmusicology.petrushka.auto :as main]
            [syntheticmusicology.petrushka.core :as pcore]))

(tests
 "functions"
 true :=
 (let [var (api/fresh)]
   (= (api/dither (+ 1 var))
      (pcore/+ 1 var)))
  
  (pcore/+ 1 (api/fresh))
  )

(tests
 true :=
 (let [var (api/fresh)]
   (= (api/dither (* 1 var))
      (pcore/* 1 var)))

 (pcore/+ 1 (api/fresh)))

(tests 
 "macros"
  true :=
  (let [var (api/fresh)]
    (= (api/dither (or true var))
       (pcore/or true var)))
  
  true :=
  (let [var (api/fresh)
        varb (api/fresh)]
    (= (api/dither (or (= varb 1) var))
       (pcore/or (api/dither (= varb 1)) var)))
  )


(tests
 "special forms"
 true :=
 (let [var (api/fresh)]
   (= (api/dither (if var (when var true) (when (not var) false)))
      (pcore/if var (api/dither (when var true)) (api/dither (when (not var) false))))))