(ns petrushka.types)

(def Numeric ::numeric)
(def Set ::set)
(def Bool ::boolean)
#_(def Sequential ::sequential)
#_(def Null ::null)

(def all-var-types 
  #{Numeric 
    Set
    Bool
    #_Sequential
    #_Null})