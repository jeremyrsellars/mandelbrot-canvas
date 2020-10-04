(ns mandel-canvas.worker
  (:require mandel-canvas.algorithm.worker-vectorized
            [mandel-canvas.worker.common :as worker-common]
            [butler.core :as butler]))

(def handlers
  (reduce-kv
    (fn assoc-handler-wrapper [hs k m-fn]
      (assoc hs k (fn handle [opts] (m-fn k opts))))
    {}
    (methods worker-common/render-in-worker)))

(butler/serve! handlers)

(println :handlers handlers)
