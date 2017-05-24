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

(defn fator-ir [t]
  (cond 
   (<= t 180) 0.225
   (<= t 360) 0.200
   (<= t 540) 0.175
   :else 0.150)
)

; tempo deve ser sempre em dias!
(defn rendimento [dados]
  (* (:montante dados) (- (fator dados) 1)))

(defn sem-ir [dados] 0)

(defn ir [dados]
  (let [r (rendimento dados)]
    (* r (fator-ir (:tempo dados)))))

(defn calcula-rendimento [dados]
  (let [r (rendimento dados)
        imposto ((:ir dados) dados)
        taxaMontante (* (:montante (montante dados)) (:tempo dados) (:taxaMontante dados))]
    (- r imposto taxaMontante)))

(defn calcula-melhor [seq-dados]
  (let [rendimentos (map calcula-rendimento seq-dados)
        maior-rendimento (apply max rendimentos)]
    (filter #(= maior-rendimento (calcula-rendimento %)) seq-dados)
    ))

(defn montantes-discretos [dados]
  (loop [montante-discreto dados
         proximo-dado (montante (assoc montante-discreto :tempo 1))
         iteracao 1
         acumulado []]
    (if (= iteracao (:tempo dados))
      acumulado
      (recur 
       proximo-dado
       (montante (assoc proximo-dado :tempo 1))
       (inc iteracao)
       (conj acumulado proximo-dado)))))

(defn consolida-montantes-discretos [dados]
  (let [montantes (montantes-discretos dados)]
    (map-indexed #(vector (inc %1) (:montante %2)) montantes)))

; cdi ~ selic
(def cdi 20.5)
(defn percentual-cdi-diario [taxa]
  (/ (* cdi (/ taxa 100)) 365.0))

; tempo sempre em dias
(def aplicacao {:taxa 1.00 :montante 282000.00 :tempo 1})
(def aporte {:taxa 0.4167 :montante 500.00 :tempo 1})
(def aporte {:taxa 0.7 :montante 100.00 :tempo 1})
(def lca {:taxa (percentual-cdi-diario 91) :montante 30000 :tempo (* 1081) :ir sem-ir :taxaMontante 0 :tipo "lca"})
(def cdb-diario {:taxa (percentual-cdi-diario 101) :montante 50000.00 :tempo (* 60) :ir ir :taxaMontante 0 :tipo "cdb-diario"})
(def cdb2anos {:taxa (percentual-cdi-diario 114) :montante 30000.00 :tempo (* 725) :ir ir :taxaMontante 0 :tipo "cdb2anos"})
(def cdb1ano {:taxa (percentual-cdi-diario 110) :montante 30000.00 :tempo (* 361) :ir ir :taxaMontante 0 :tipo "cdb1ano"})
(def cdb6meses {:taxa (percentual-cdi-diario 110) :montante 5000.00 :tempo (* 180) :ir ir :taxaMontante 0 :tipo "cdb6meses"})
(def cdb5anos {:taxa (percentual-cdi-diario 110) :montante 5000.00 :tempo (* 5 365) :ir ir :taxaMontante 0 :tipo "cdb5anos"})
(def selic2anos {:taxa (percentual-cdi-diario 100) :montante 50000 :tempo (* 2 365) :ir ir :taxaMontante (/ 0.0030 365) :tipo "tesouro selic"})
(defn quase-igual [x y]
  (do
    (if (or (< x 0) (< y 0)) (throw (Exception. "x ou y negativo")))
    (< (abs (- x y)) 0.01)))

(defn -main
  [& args]
  (let [seq-montantes (montante-com-aportes aporte (* 35 12))
        m (soma-montantes seq-montantes)]
    (pprint seq-montantes)
    (pprint m)
    (pprint (str "rendimento mensal: " (- (:montante (montante {:taxa 0.4167 :montante m :tempo 1})) m)) )))

; metodo numerico
(defn calcula-taxa-eq [com-ir]
  (let [percentual-diario (percentual-cdi-diario 100)
        isento (assoc com-ir :ir sem-ir)
        juros (calcula-rendimento com-ir)]
    (loop [chute (assoc isento :taxa percentual-diario :tempo (:tempo com-ir))]
      (if (quase-igual juros (calcula-rendimento chute))
        (let [taxa-eq (* 100 (/ (* 365 (:taxa chute)) cdi))]
          (do (pprint (str (format "%.2f" taxa-eq) "% CDI"))
              taxa-eq))
        (recur (assoc chute :taxa (- (:taxa chute) (* 0.000005 percentual-diario) )))))))

; metodo analitico
(defn calcula-tx-eq [com-ir]
  (let [
        i (fator-ir (:tempo com-ir))
        c (:montante com-ir)
        f (fator com-ir)
        x (+ f 
             (- (* f i)) 
             i
             )
        ]
    (let [taxa-eq-dia
          (- (nth-root x (:tempo com-ir)) 
             1.0)
          taxa-eq (* 10000 (/ (* 365 taxa-eq-dia) cdi))]
      (do (pprint (str (format "%.2f" taxa-eq) "% CDI"))
          taxa-eq))))

  (defn tx-eq-maior-menor "exemplo: menor = 1.6 significa 1.6% = 0.016. n é quantos quantidade de tempo. exemplo: se é em meses, n = 12 significa comparar taxa mensal com anual"
    [maior n]
    (* 100 
       (dec (power (+ 1 (/ maior 100))
                   (/ 1 n)))))

  (defn tx-eq-menor-maior "exemplo: menor = 1.6 significa 1.6% = 0.016. n é quantos quantidade de tempo. exemplo: se é em meses, n = 12 significa comparar taxa mensal com anual"
    [menor n]
    (* 100 
       (dec (power (+ 1 (/ menor 100))
                   n))))


(:tipo (first (calcula-melhor [lca cdb1ano])))
(calcula-rendimento cdb1ano)
(calcula-rendimento lca)
;  (calcula-taxa-eq cdb6meses)
;  (calcula-tx-eq cdb6meses)
;  (tx-eq-menor-maior 6.84 2)

