(ns mandel-canvas.coloring.pink)

(def max-iter 255)

(def color-table
  (mapv
    #(if (= max-iter %)
      "black"
      (str "rgb(" (- 255 %)
              "," (int (* 16 (mod % 16)))
              "," (- 255 %)))
    (range (inc max-iter))))

(def opts
  {:max-iter max-iter
   :color-fn color-table})
