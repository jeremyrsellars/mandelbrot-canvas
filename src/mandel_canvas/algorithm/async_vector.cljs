(ns mandel-canvas.algorithm.async-vector
  (:require [goog.object :as gobj]
            [mandel-canvas.arithmetic.vector :as av]))

(defn render-async
  [{:keys [max-iter color-fn width height rendering-context min-x max-x min-y max-y]}]
  (let [x-width (- max-x min-x)
        y-width (- max-y min-y)
        remaining-chunks
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
                    :let [x (+ min-x (* x-width (/ pixel-x width)))
                          y (+ min-y (* y-width (/ pixel-y height)))
                          c [x y]
                          iter (loop [iter 0
                                      z [0 0]]
                                  (cond (>= iter max-iter)
                                        iter

                                        (>= (av/magnitude z) 4)
                                        iter

                                        :iterate
                                        (recur (inc iter)
                                               (av/add c (av/mul z z)))))]]

              ;(println [pixel-x pixel-y] [x y] iter)
              (gobj/set rendering-context "fillStyle" (color-fn iter))
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
