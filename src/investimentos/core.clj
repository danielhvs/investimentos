(ns investimentos.core
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))

(defn montante [dados]
  (let [capitalInicial (* (:capitalInicial dados) (math/expt  (+ 1 (/ (:taxa dados) 100)) (:tempo dados)))]
    {:capitalInicial capitalInicial :taxa (:taxa dados) :tempo (:tempo dados)}))

(defn soma-montantes [m1 m2]
  (assoc m2 :capitalInicial (+ (:capitalInicial m1) (:capitalInicial m2))))

(comment errado ainda. falta somar os montantes e fazer novo montante)
(defn montante-com-aportes [dados n]
  (let [m1 (soma-montantes (montante dados) dados)
        m2 (soma-montantes (montante m1) m1)]
    m2))

(comment montante
 (soma-montantes dados (montante dados)))

(def aplicacao {:taxa 10.00 :capitalInicial 100000.00 :tempo 26})
(def aporte {:taxa (/ 10.00 1) :capitalInicial 100.00 :tempo 1})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (montante-com-aportes aporte 3)))
