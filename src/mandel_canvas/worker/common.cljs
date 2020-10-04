(ns mandel-canvas.worker.common
  (:require [butler.core :as butler]))

(defmulti render-in-worker
  (fn render-in-worker-dispatch [operation-key data]
    operation-key))
