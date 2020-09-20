(ns mandel-canvas.coloring.quick)

(def max-iter 15)

(def color-table
  (mapv
    #(if (= max-iter %)
      "black"
      (str "rgb(" (* 16 %)
              "," (* 16 %)
              "," (* 16 %)))
    (range (inc max-iter))))

(def opts
  {:max-iter max-iter
   :color-fn color-table})