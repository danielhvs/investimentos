(ns investimentos.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.pprint :refer :all]
            ;[clojurewerkz.money.format :refer :all]
)
  (:gen-class))

;(import java.util.Locale)
;(defn reais [valor] (amount-of cu/BRL valor) (Locale. "pt" "BR"))

(defn fator [dados]
  (math/expt  (+ 1 (/ (:taxa dados) 100)) (:tempo dados)))

(defn montante [dados]
  (let [montante (* (:montante dados) (fator dados))]
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

(defn ir [t]
  (cond 
   (<= t 180) 0.225
   (<= t 360) 0.200
   (<= t 540) 0.175
   :else 0.150)
)

; tempo deve ser sempre em dias!
(defn rendimento [dados]
  (* (:montante dados) (- (fator dados) 1)))

(defn rendimento-com-ir [dados]
  (* (rendimento dados) (- 1 (ir (:tempo dados)))))

(defn calcula-rendimento [dados]
  (if (:ir dados) (rendimento-com-ir dados) (rendimento dados)))

(defn calcula-melhor [seq-dados]
  (let [rendimentos (map calcula-rendimento seq-dados)
        maior-rendimento (apply max rendimentos)]
    (filter #(= maior-rendimento (calcula-rendimento %)) seq-dados)
    ))

(def cdi 10.0)
(def aplicacao {:taxa 1.00 :montante 282000.00 :tempo 1})
(def aporte {:taxa 0.4167 :montante 500.00 :tempo 1})
(def lca {:taxa (/ (* cdi (/ 80 100)) 365.0) :montante 100000.00 :tempo 361 :ir false :tipo "lca"})
(def cdb {:taxa (/ (* cdi (/ 118 100)) 365.0) :montante 100000.00 :tempo 360 :ir true :tipo "cdb"})

(defn -main
  [& args]
  (let [seq-montantes (montante-com-aportes aporte (* 35 12))
        m (soma-montantes seq-montantes)]
    (pprint seq-montantes)
    (pprint m)
    (pprint (str "rendimento mensal: " (- (:montante (montante {:taxa 0.4167 :montante m :tempo 1})) m)) )))

(calcula-rendimento lca)
(calcula-rendimento cdb)
(calcula-melhor [lca cdb])
