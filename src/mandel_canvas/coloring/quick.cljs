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
   :iter-steps [max-iter]
   :color-fn color-table})

(def binary
  {:max-iter max-iter
   :color-fn (fn [iter] (if (even? iter) "black" "#ccc"))})

(def slow-binary
  (let [max-iter 1000]
    {:max-iter max-iter
     :iter-steps (range 2 max-iter 2)
     :color-fn (fn [iter] (if (even? iter) "black" "#ccc"))}))
