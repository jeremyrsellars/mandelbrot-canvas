(ns mandel-canvas.coloring.pink-orange-blue
  (:require mandel-canvas.coloring.common))

(defn- rgb
  [r g b]
  (str "rgb(" (int (min 255 (max 0 r)))
          "," (int (min 255 (max 0 g)))
          "," (int (min 255 (max 0 b)))
          ")"))

(let [blend-over 200]
  (def color-table
    (-> []
      (into
        (map
          #(rgb  (- 255 %)
                 (min (- 255 %) (* 8 (mod % 16)))
                 (- 255 %))
          (range blend-over)))
      (into
        (map
          #(rgb  (- 255 %)
                 (min (- 255 %) (* 8 (mod % 16)))
                 (min (- 255 %) (* 8 (mod % 16))))
          (range blend-over)))
      (into
        (map
          #(rgb  (min (- 255 %) (* 8 (mod % 16)))
                 (min (- 255 %) (* 8 (mod % 16)))
                 (- 255 %))
          (range blend-over)))))

  (def opts
    (let [max-iter (dec (count color-table))]
      {:max-iter max-iter
       :iter-steps (butlast (range blend-over max-iter blend-over)) ; because black is 1 step away
       :color-fn (fn [iter](nth color-table iter "black"))})))

(defmethod mandel-canvas.coloring.common/color-scheme ::scheme
  [_]
  opts)
