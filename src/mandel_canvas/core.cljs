(ns mandel-canvas.core
  "Z = Z^2 + C"
  (:require [goog.object :as gobj]))

(defonce app-elem (.getElementById js/document "app"))
(defonce canvas-elem
  (doto (.createElement js/document "canvas")
    (gobj/set "height" 100)
    (gobj/set "width" 100)))
(defonce _
  (do
    (.appendChild app-elem canvas-elem)))

(defonce ctx
  (doto (.getContext canvas-elem "2d")
    (gobj/set "mozImageSmoothingEnabled" false)
    (gobj/set "webkitImageSmoothingEnabled" false)
    (gobj/set "fillStyle" "#888")
    (.fillRect 0, 0, (.-width canvas-elem) (.-height canvas-elem))))

(defn add
  [[r1 i1] [r2 i2]]
  [(+ r1 r2) (+ i1 i2)])

(defn mul
  [[r1 i1] [r2 i2]]
  [(- (* r1 r2) (* i1 i2))
   (+ (* r1 i2) (* r2 i1))])

(defn magnitude
  [[r i]]
  (Math/sqrt (+ (* r r) (* i i))))

(def max-iter 255)

(def color-table
  (mapv
    #(if (= max-iter %)
      "black"
      (str "rgb(" (- 255 %) "," (int (* 16 (mod % 16))) "," (- 255 %)))
    (range (inc max-iter))))

(defonce _doit
  (let [min-x -1.0
        max-x -0.5
        min-y -0.5
        max-y  0.0
        x-width (- max-x min-x)
        y-width (- max-y min-y)
        width (.-width canvas-elem)
        height (.-height canvas-elem)]
    (doseq [pixel-x (range width)
            pixel-y (range height)
            :let [x (+ min-x (* x-width (/ pixel-x width)))
                  y (+ min-y (* y-width (/ pixel-y height)))
                  c [x y]
                  iter (loop [iter 0
                              z [0 0]]
                          (cond (>= iter max-iter)
                                iter

                                (>= (magnitude z) 4)
                                iter

                                :iterate
                                (recur (inc iter)
                                       (add c (mul z z)))))]]
      ;(println [pixel-x pixel-y] [x y] iter)
      (gobj/set ctx "fillStyle" (nth color-table iter))
      (.fillRect ctx pixel-x pixel-y 1 1))))

(println "made it!")
