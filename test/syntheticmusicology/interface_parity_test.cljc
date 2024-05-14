(ns syntheticmusicology.interface-parity-test
  "tests that the auto interface and the encapsulated 
   namespaces return identically"
  (:require [hyperfiddle.rcf :refer [tests]]
            [petrushka.api :as api]
            [petrushka.main :as main]
            [syntheticmusicology.petrushka.core :as pcore]))

(tests
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