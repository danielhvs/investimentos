(ns investimentos.core
  (:require [clojure.math.numeric-tower :as math]
            [clojure.pprint :refer :all]
            ;[clojurewerkz.money.format :refer :all]
)
  (:gen-class))

;; define abs & power to avoid needing to bring in the clojure Math library
(defn abs [x]
  " Absolute value"
  (if (< x 0) (- x) x))
 
(defn power [x n]
  " x to power n, where n = 0, 1, 2, ... "
  (apply * (repeat n x)))
 
(defn calc-delta [A x n]
  " nth rooth algorithm delta calculation "
  (/ (- (/ A (power x (- n 1))) x) n))
 
(defn nth-root
  " nth root of algorithm: A = numer, n = root"
  ([A n] (nth-root A n 0.5 1.0))  ; Takes only two arguments A, n and calls version which takes A, n, guess-prev, guess-current
  ([A n guess-prev guess-current] ; version take takes in four arguments (A, n, guess-prev, guess-current)
   (if (< (abs (- guess-prev guess-current)) 1e-6)
     guess-current
     (recur A n guess-current (+ guess-current (calc-delta A guess-current n)))))) ; iterate answer using tail recursion


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
(defn percentual-cdi-diario [taxa]
  (/ (* cdi (/ taxa 100)) 365.0))

(def aplicacao {:taxa 1.00 :montante 282000.00 :tempo 1})
(def aporte {:taxa 0.4167 :montante 500.00 :tempo 1})
(def lca {:taxa (percentual-cdi-diario 100) :montante 5000.00 :tempo 720 :ir false :tipo "lca"})
(def cdb {:taxa (percentual-cdi-diario 115) :montante 5000.00 :tempo 720 :ir true :tipo "cdb pine"})
(def lca2 {:taxa (percentual-cdi-diario 100) :montante 5000.00 :tempo 180 :ir false :tipo "lca"})
(def cdb2 {:taxa (percentual-cdi-diario 122.5) :montante 5000.00 :tempo 180 :ir true :tipo "cdb teste"})

(defn -main
  [& args]
  (let [seq-montantes (montante-com-aportes aporte (* 35 12))
        m (soma-montantes seq-montantes)]
    (pprint seq-montantes)
    (pprint m)
    (pprint (str "rendimento mensal: " (- (:montante (montante {:taxa 0.4167 :montante m :tempo 1})) m)) )))

(defn calcula-taxa-equivalente [isento com-ir]
  (let [
        imposto (ir (:tempo com-ir))
        c (:montante com-ir)
        f (fator com-ir)
        x (- (* (math/expt f 2) imposto))
        y (* c f)
        z (- (* c f imposto))
        ]
    (pprint imposto)
    (pprint c)
    (pprint f)
    (- (nth-root 
        (+ x y z) 
        (:tempo com-ir)) 
       1.0)))

(calcula-rendimento lca)
(calcula-rendimento cdb)
(calcula-melhor [lca cdb])

(calcula-rendimento lca2)
(calcula-rendimento cdb2)

(percentual-cdi-diario 85)
(calcula-taxa-equivalente lca cdb)
