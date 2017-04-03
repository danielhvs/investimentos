(ns investimentos.core
  (:gen-class))

(defn busca-melhor [arquivo]
  (let [conteudo (slurp arquivo)]
    (println conteudo)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (busca-melhor (first args)))
