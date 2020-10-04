(ns mandel-canvas.algorithm.worker-vectorized
  (:require [goog.events :as gevt]
            [goog.events.EventType :as EventType]
            [goog.object :as gobj]
            [butler.core :as butler]
            mandel-canvas.worker.common
            mandel-canvas.coloring.common
            mandel-canvas.coloring.pink-orange-blue
            [mandel-canvas.algorithm.progressive-vectorized :as pv]))

(defn init-offscreen-2d-context
  [canvas-elem
   {:keys [log]
    :or {log println}}]
  (let [offscreen (.transferControlToOffscreen canvas-elem)
        {:keys [worker] :as rendering-butler}
        (butler/butler mandel-canvas.worker.common/worker-js-url
            {:display-message
              (fn display-message
                [msg]
                (println :butler-display-message msg))
             :downstream-error
              (fn downstream-error
                [context error]
                (.error js/console "Downstream error" context error))
             :done
              (fn done
                [result]
                (println :butler-done result))})]
    (gevt/listen worker EventType/ERROR #(.error js/console "Primary worker error" (str ::butler-error) %))
    {:offscreen     offscreen
     :rendering-butler rendering-butler}))


(defn render-async
  [{:keys [max-iter iter-steps scheme-key width height rendering-context min-x max-x min-y max-y log] :as opts}
   done]
  (let [{:keys [offscreen rendering-butler]} rendering-context]
    (println :starting-rendering-butler ::render-async opts)
    (butler/work! rendering-butler ::render
      {:offscreen offscreen
       :opts
       {:scheme-key scheme-key
        :max-iter max-iter
        :iter-steps iter-steps
        :width  width
        :height height
        :min-x min-x
        :max-x max-x
        :min-y min-y
        :max-y max-y}}
      [[:offscreen]])))

(defmethod mandel-canvas.worker.common/render-in-worker ::render
  [_ {:keys [offscreen opts]}]
  (butler/bring! :display-message (str "butler: Starting " ::render))
  (let [{:keys [scheme-key width height min-x max-x min-y max-y]} opts
        _ (println :scheme-key scheme-key :and (keys opts))
        {:keys [color-fn max-iter iter-steps]} (mandel-canvas.coloring.common/color-scheme scheme-key)
        rendering-context (.getContext offscreen "2d")
        _ (println :color-fn color-fn)
        {:keys [worker] :as partition-butler}
        (butler/butler mandel-canvas.worker.common/worker-js-url
            {:display-message  (partial butler/bring! :display-message)
             :downstream-error (partial butler/bring! :downstream-error)
             :paint            #(apply pv/paint-to-context rendering-context color-fn %)
             :paint-chunk      (fn paint-chunk [{chunk-buffer :chunk}]
                                (let [^js/Float64Array chunk (js/Float64Array. chunk-buffer)]
                                  (butler/bring! :display-message (str "Painting chunk count=" (.-length chunk)))
                                  (loop [idx 0]
                                    (when (< idx (.-length chunk))
                                      (let [color-iter (aget chunk (+ idx 0))
                                            pixel-r (aget chunk (+ idx 1))
                                            pixel-i (aget chunk (+ idx 2))
                                            pixel-r-width (aget chunk (+ idx 3))
                                            pixel-i-width (aget chunk (+ idx 4))]
                                        ;(butler/bring! :display-message (str {:color-iter color-iter, :pixel-r pixel-r, :pixel-i pixel-i}))
                                        (pv/paint-to-context rendering-context color-fn color-iter pixel-r pixel-i pixel-r-width pixel-i-width)
                                        (recur (+ 5 idx)))))))
             :done             (partial butler/bring! :done)})]
    (gevt/listen worker EventType/ERROR (partial butler/bring! :downstream-error))
    (butler/bring! :display-message
     (str "Get to work!"
      {:opts
       {:scheme-key scheme-key
        :max-iter max-iter
        :iter-steps iter-steps
        :width  width
        :height height
        :min-x min-x
        :max-x max-x
        :min-y min-y
        :max-y max-y}}))
    (butler/work! partition-butler ::partition
      {:opts
       {:scheme-key scheme-key
        :max-iter max-iter
        :iter-steps iter-steps
        :width  width
        :height height
        :min-x min-x
        :max-x max-x
        :min-y min-y
        :max-y max-y}})))

(defmethod mandel-canvas.worker.common/render-in-worker ::partition
  [_ {:keys [opts]}]
  (butler/bring! :display-message (str "butler: Starting " ::partition))
  (let [chunk-ref (atom [])]
    (pv/render-async
      (assoc opts :rendering-context nil
                  :log #(butler/bring! :display-message %&)
                  :paint-fn (fn pixel-complete [rendering-context color-fn color-iter pixel-r pixel-i pixel-r-width pixel-i-width]
                              ;(butler/bring! :paint [color-iter pixel-r pixel-i pixel-r-width pixel-i-width])
                              (swap! chunk-ref conj color-iter pixel-r pixel-i pixel-r-width pixel-i-width)))
      #(butler/bring! :done %)
      #(let [[chunk _] (swap-vals! chunk-ref empty)]
        (butler/bring! :paint-chunk
         {:chunk (.-buffer (js/Float64Array. chunk))}
         [[:chunk]])))))

;   (butler/bring! :display-message (str "butler: Starting " ::partition))
;   (pv/render-async
;     (-> (merge mandel-canvas.coloring.pink-orange-blue/opts opts)
;         (assoc :rendering-context (.getContext offscreen "2d")
;                :log #(butler/bring! :display-message %&)))
;     #(butler/bring! :done %)))
; :paint-fn
