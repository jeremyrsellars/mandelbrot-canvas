(ns mandel-canvas.coloring.pink)

(def max-iter 60)

(defn- rgb
  [r g b]
  (str "rgb(" (int (min 255 (max 0 r)))
          "," (int (min 255 (max 0 g)))
          "," (int (min 255 (max 0 b)))
          ")"))

(def color-table
    (mapv
      #(rgb (- 255 %)
            (min (- 255 %) (- 255 (+ 16 (* 7 (mod % 16)))))
            (- 255 %))
      (range (inc max-iter))))

(def opts
  {:max-iter (count color-table)
   :color-fn (fn [iter](nth color-table iter "black"))})
