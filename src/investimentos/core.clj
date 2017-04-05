(ns investimentos.core
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))

(defn montante [dados]
  (* (:capitalInicial dados) (math/expt  (+ 1 (/ (:aa dados) 100)) (:tAnos dados))))

(def teste 
  {:aa 10.00 :capitalInicial 100000.00 :tAnos 26})

(comment def teste 
  {:aa 10.00 :capitalInicial 0.00 :aporteMensal 250.00 :tMeses 24})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (montante teste)))
