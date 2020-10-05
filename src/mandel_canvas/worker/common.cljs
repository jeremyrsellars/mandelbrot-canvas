(ns mandel-canvas.worker.common
  (:require [butler.core :as butler]))

(def worker-js-url "/cljs-out/dev-main-worker.js")

(def workers-ref (atom #{}))

(def abort-ref (atom false))

(defmulti render-in-worker
  (fn render-in-worker-dispatch [operation-key data]
    operation-key))

(defn abortable-butler
  "Like butler.core/butler, but saves the resultant worker for potential abort later."
  [& args]
  (let [{:keys [worker] :as result} (apply butler/butler args)]
    (swap! workers-ref conj worker)
    result))

(defmethod mandel-canvas.worker.common/render-in-worker :abort
  [_ _]
  (reset! abort-ref true)
  (let [[workers-to-terminate _] (swap-vals! workers-ref empty)]
    (doseq [^js/WebWorker worker workers-to-terminate]
      (when worker
        (.terminate worker)))))
