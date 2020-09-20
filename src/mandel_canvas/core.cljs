(ns mandel-canvas.core
  "Z = Z^2 + C"
  (:require [goog.object :as gobj]
            [mandel-canvas.arithmetic.vector :refer [mul add magnitude]]
            mandel-canvas.algorithm.sequential-vector
            mandel-canvas.coloring.pink
            mandel-canvas.coloring.quick))

(defonce app-elem (.getElementById js/document "app"))
(defonce canvas-elem
  (let [canvas-elem
        (doto (.createElement js/document "canvas")
          (gobj/set "height" 100)
          (gobj/set "width" 100))]
    (.appendChild app-elem canvas-elem)
    canvas-elem))

(defn do-render
  [{:keys [width height render-fn color-opts]
    :or {width 50
         height 50
         render-fn mandel-canvas.algorithm.sequential-vector/render
         color-opts mandel-canvas.coloring.pink/opts}}]
  (let [_ (doto canvas-elem
            (gobj/set "height" height)
            (gobj/set "width" width))
        ctx
          (doto (.getContext canvas-elem "2d")
            (gobj/set "mozImageSmoothingEnabled" false)
            (gobj/set "webkitImageSmoothingEnabled" false)
            (gobj/set "fillStyle" "#888")
            (.fillRect 0 0 (.-width canvas-elem) (.-height canvas-elem)))]
    (println "Done" (gobj/get render-fn "name")
      (time
        (render-fn
          (assoc color-opts
            :width  width
            :height height
            :rendering-context ctx))))))

(do-render
 {})
