(ns mandel-canvas.algorithm.onscreen
  (:require [goog.object :as gobj]))

(defn init-onscreen-2d-context
  [canvas-elem opts]
  (doto (.getContext canvas-elem "2d")
    (gobj/set "mozImageSmoothingEnabled" false)
    (gobj/set "webkitImageSmoothingEnabled" false)
    (gobj/set "fillStyle" "#888")
    #_
    (.fillRect 0 0 (.-width canvas-elem) (.-height canvas-elem))))
