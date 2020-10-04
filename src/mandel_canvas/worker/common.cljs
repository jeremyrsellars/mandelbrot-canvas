(ns mandel-canvas.worker.common
  (:require [butler.core :as butler]))

(def worker-js-url "/cljs-out/dev-main-worker.js")

(defmulti render-in-worker
  (fn render-in-worker-dispatch [operation-key data]
    operation-key))
