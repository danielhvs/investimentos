(ns investimentos.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.pprint :refer :all])
  (:gen-class))

(defn montante [dados]
  (let [montante (* (:montante dados) (math/expt  (+ 1 (/ (:taxa dados) 100)) (:tempo dados)))]
    {:montante montante :taxa (:taxa dados) :tempo (:tempo dados)}))

(defn inc-tempo [dados]
  (assoc dados :tempo (inc (:tempo dados))))

(defn soma-montantes [m1 m2]
  (assoc m2 :montante (+ (:montante m1) (:montante m2))))

(defn montante-com-aportes [dados n]
  (let [aplicacoes (take n (iterate inc-tempo dados))]
    (map montante aplicacoes)))

(defn soma-montantes [seq-dados]
  (reduce + (map :montante seq-dados)))

(def aplicacao {:taxa 1.00 :montante 282000.00 :tempo 1})
(def aporte {:taxa (/ 5.00 12.0) :montante 250.00 :tempo 1})

(defn -main
  [& args]
  (let [seq-montantes (montante-com-aportes aporte (* 25 12))]
    (pprint seq-montantes)
    (pprint (soma-montantes seq-montantes))
    (pprint (montante aplicacao))))
