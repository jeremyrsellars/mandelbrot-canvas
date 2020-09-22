(ns mandel-canvas.coloring.pink-orange-blue)

(defn- rgb
  [r g b]
  (str "rgb(" (int (min 255 (max 0 r)))
          "," (int (min 255 (max 0 g)))
          "," (int (min 255 (max 0 b)))
          ")"))

(def color-table
  (let [blend-over 200]
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
          (range blend-over)))
      (conj "black"))))

(def opts
  {:max-iter (dec (count color-table))
   :color-fn color-table})


; (println (map vector (range) color-table))
