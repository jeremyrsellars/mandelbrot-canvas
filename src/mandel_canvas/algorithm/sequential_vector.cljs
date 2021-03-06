(ns mandel-canvas.algorithm.sequential-vector
  (:require [goog.object :as gobj]
            [mandel-canvas.arithmetic.vector :as av]))

(defn render
  [{:keys [max-iter color-fn width height rendering-context min-x max-x min-y max-y log]}]
  (let [x-width (- max-x min-x)
        y-width (- max-y min-y)]
    (doseq [pixel-x (range width)
            pixel-y (range height)
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
      ;(log [pixel-x pixel-y] [x y] iter)
      (gobj/set rendering-context "fillStyle" (color-fn iter))
      (.fillRect rendering-context pixel-x pixel-y 1 1))))
