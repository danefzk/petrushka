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
 (let [var (main/fresh)]
   (= (api/dither (+ 1 var))
      (pcore/+ 1 var)))
  
  (pcore/+ 1 (main/fresh))
  )

(tests
 true :=
 (let [var (main/fresh)]
   (= (api/dither (* 1 var))
      (pcore/* 1 var)))

 (pcore/+ 1 (main/fresh)))

(tests 
 "macros"
  true :=
  (let [var (main/fresh)]
    (= (api/dither (or true var))
       (pcore/or true var)))
  
  true :=
  (let [var (main/fresh)
        varb (main/fresh)]
    (= (api/dither (or (= varb 1) var))
       (pcore/or (api/dither (= varb 1)) var)))
  )


(tests
 "special forms"
 true :=
 (let [var (main/fresh)]
   (= (api/dither (if var (when var true) (when (not var) false)))
      (pcore/if var (api/dither (when var true)) (api/dither (when (not var) false))))))