(ns mandel-canvas.algorithm.worker-vectorized
  (:require [goog.events :as gevt]
            [goog.events.EventType :as EventType]
            [goog.object :as gobj]
            [butler.core :as butler]
            mandel-canvas.worker.common
            mandel-canvas.coloring.pink-orange-blue
            [mandel-canvas.algorithm.progressive-vectorized :as pv]))

(defn init-offscreen-2d-context
  [canvas-elem
   {:keys [log]
    :or {log println}}]
  (let [offscreen (.transferControlToOffscreen canvas-elem)
        {:keys [worker] :as rendering-butler}
        (butler/butler "/cljs-out/dev-main-worker.js"
            {:display-message
              (fn display-message
                [msg]
                (println :butler-display-message msg))
             :done
              (fn done
                [result]
                (println :butler-done result))})]
    (gevt/listen worker "onerror" #(.log js/console (str ::butler-error-string %)))
    (gevt/listen worker EventType/ERROR #(.log js/console (str ::butler-error %)))
    {:offscreen     offscreen
     :rendering-butler rendering-butler}))


(defn render-async
  [{:keys [max-iter iter-steps color-fn width height rendering-context min-x max-x min-y max-y log] :as opts}
   done]
  (let [{:keys [offscreen rendering-butler]} rendering-context]
    (println :starting-rendering-butler ::render-async opts)
    (butler/work! rendering-butler ::worker-vectorized
      {:offscreen offscreen
       :opts
       {:max-iter max-iter
        :iter-steps iter-steps
        :width  width
        :height height
        :min-x min-x
        :max-x max-x
        :min-y min-y
        :max-y max-y}}
      [[:offscreen]])))

(defmethod mandel-canvas.worker.common/render-in-worker ::worker-vectorized
  [_ {:keys [offscreen opts]}]
  (butler/bring! :display-message (str  "butler: Starting " ::worker-vectorized))
  (pv/render-async
    (-> (merge mandel-canvas.coloring.pink-orange-blue/opts opts)
        (assoc :rendering-context (.getContext offscreen "2d")
               :log #(butler/bring! :display-message %&)))
    #(butler/bring! :done %)))
