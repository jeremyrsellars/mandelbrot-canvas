(ns mandel-canvas.algorithm.worker-vectorized
  (:require [goog.events :as gevt]
            [goog.events.EventType :as EventType]
            [goog.object :as gobj]
            [butler.core :as butler]
            [mandel-canvas.worker.common :as butler-ex]
            mandel-canvas.coloring.common
            mandel-canvas.coloring.pink-orange-blue
            [mandel-canvas.algorithm.progressive-vectorized :as pv]))

(defn quadrants
  "Divides the pixels into approximate quadrants, when an even number of pixels is divided the side is n/2, but when odd, instead of trying to divide a pixel in half, it shifts the middle pixel to the second half."
  [{:keys [pixel-x-offset pixel-y-offset width height min-x max-x min-y max-y]
    :or {pixel-x-offset 0, pixel-y-offset 0}
    :as whole}]
  (if (or (< width 2) (< width 2))
    [whole]
    (let [x-width (- max-x min-x)
          fwidth (int (/ width 2))              ; first half width
          swidth (- width fwidth)               ; second half width
          fx-width (/ (* x-width fwidth) width)
          sx-width (- x-width fx-width)

          y-width (- max-y min-y)
          fheight (int (/ height 2))
          sheight (- height fheight)
          fy-width (/ (* y-width fheight) height)
          sy-width (- y-width fy-width)]
      (for [x [{:pixel-x-offset    pixel-x-offset          :width fwidth   :min-x min-x                                :max-x (+ min-x fx-width)}
               {:pixel-x-offset (+ pixel-x-offset fwidth)  :width swidth   :min-x (+ min-x fx-width #_(/ x-width width)) :max-x max-x}]
            y [{:pixel-y-offset    pixel-y-offset          :height fheight :min-y min-y                                :max-y (+ min-y fy-width)}
               {:pixel-y-offset (+ pixel-y-offset fheight) :height sheight :min-y (+ min-y fy-width #_(/ y-width width)) :max-y max-y}]]
        (merge x y)))))

(defn split-into-cells
 ([whole](split-into-cells whole (.-hardwareConcurrency js/navigator)))
 ([whole max-split]
  (loop [cells [whole]]
    (if (>= (count cells) max-split)
      cells
      (recur (mapcat quadrants cells))))))


(defn init-offscreen-2d-context
  [canvas-elem
   {:keys [log]
    :or {log println}}]
  (let [offscreen (.transferControlToOffscreen canvas-elem)
        {:keys [worker] :as rendering-butler}
        (butler-ex/abortable-butler mandel-canvas.worker.common/worker-js-url
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
      [[:offscreen]])
    (fn cancel []
      (butler/work! rendering-butler :abort)
      (js/setTimeout #() 10000))))

(defmethod mandel-canvas.worker.common/render-in-worker ::render
  [_ {:keys [offscreen opts]}]
  (butler/bring! :display-message (str "butler: Starting " ::render))
  (let [{:keys [scheme-key width height min-x max-x min-y max-y]} opts
        _ (println :scheme-key scheme-key :and (keys opts))
        {:keys [color-fn max-iter iter-steps]} (mandel-canvas.coloring.common/color-scheme scheme-key)
        rendering-context (.getContext offscreen "2d")
        _ (println :color-fn color-fn)]
    (doseq [{:keys [pixel-x-offset pixel-y-offset]
             :or {pixel-x-offset 0, pixel-y-offset 0}
             :as section}
            (split-into-cells
              {:width  width
               :height height
               :min-x min-x
               :max-x max-x
               :min-y min-y
               :max-y max-y})]
      (let [{:keys [worker] :as partition-butler}
            (butler-ex/abortable-butler mandel-canvas.worker.common/worker-js-url
              {:display-message  (partial butler/bring! :display-message)
               :downstream-error (partial butler/bring! :downstream-error)
               :done             (partial butler/bring! :done)
               :paint            #(apply pv/paint-to-context rendering-context color-fn %)
               :paint-chunk
                  (fn paint-chunk
                    [{chunk-buffer :chunk, chunk-key :chunk-key}]
                    (let [^js/Float64Array chunk (js/Float64Array. chunk-buffer)
                          {:keys [pixel-x-offset pixel-y-offset]} chunk-key]
                      (butler/bring! :display-message (str "Painting chunk count=" (.-length chunk)))
                      (loop [idx 0]
                        (when (< idx (.-length chunk))
                          (let [color-iter (aget chunk (+ idx 0))
                                pixel-r (+ pixel-x-offset (aget chunk (+ idx 1)))
                                pixel-i (+ pixel-y-offset (aget chunk (+ idx 2)))
                                pixel-r-width (aget chunk (+ idx 3))
                                pixel-i-width (aget chunk (+ idx 4))]
                            ;(butler/bring! :display-message (str {:color-iter color-iter, :pixel-r pixel-r, :pixel-i pixel-i}))
                            (pv/paint-to-context rendering-context color-fn color-iter pixel-r pixel-i pixel-r-width pixel-i-width)
                            (recur (+ 5 idx)))))))})]
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
           (assoc section
            :chunk-key {:pixel-x-offset pixel-x-offset, :pixel-y-offset pixel-y-offset}
            :scheme-key scheme-key
            :max-iter max-iter
            :iter-steps iter-steps)})))))

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
         {:chunk (.-buffer (js/Float64Array. chunk))
          :chunk-key (get opts :chunk-key)}
         [[:chunk]])))))

;   (butler/bring! :display-message (str "butler: Starting " ::partition))
;   (pv/render-async
;     (-> (merge mandel-canvas.coloring.pink-orange-blue/opts opts)
;         (assoc :rendering-context (.getContext offscreen "2d")
;                :log #(butler/bring! :display-message %&)))
;     #(butler/bring! :done %)))
; :paint-fn
