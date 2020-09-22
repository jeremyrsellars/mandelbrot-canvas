(ns mandel-canvas.algorithm.color-scheme
  (:require [goog.object :as gobj]
            [mandel-canvas.arithmetic.vector :as av]))

(defn render-async
  [{:keys [max-iter color-fn width height rendering-context]}]
  (let [remaining-chunks
          (->>
            (for [pixel-x (range width)
                  pixel-y (range height)]
              [pixel-x pixel-y])
            shuffle
            (partition-all 100)
            (into [])
            atom)
        render-chunk
          (fn render-chunk
            [coordinates]
            (doseq [[pixel-x pixel-y] coordinates
                    :let [x (/ pixel-x width)
                          y (/ pixel-y height)
                          mag (/ (av/magnitude [x y]) (js/Math.sqrt 2))
                          iter (* mag max-iter)]]
              ;(println [pixel-x pixel-y] [x y] iter)
              (gobj/set rendering-context "fillStyle" (color-fn (int iter)))
              (.fillRect rendering-context pixel-x pixel-y 1 1)))

        next-chunk
          (fn next-chunk
            []
            (let [[old new] (swap-vals! remaining-chunks pop)
                  chunk (peek old)]
              (render-chunk chunk)
              (if (seq new)
                (js/setTimeout next-chunk 0)
                (println "Done with async!"))))]
    (next-chunk)))
