(ns petrushka.utils.string
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn >> [env string]
  (->> string
       (re-seq #"\{\{[a-z]*\}\}")
       (map (partial drop-last 2))
       (map (partial drop 2))
       (map (partial apply str))
       (map (fn [s] (get env (keyword s) "")))
       (apply format (clojure.string/replace string #"\{\{[a-z]*\}\}" "%s"))))

(tests 
 (>> {:e "E" :g "G"} "a b c d {{e}} f {{g}}") := "a b c d E f G"
 (>> {:e "E" :g [:a :b :c]} "a b c d {{e}} f {{g}}") := "a b c d E f [:a :b :c]"
 )