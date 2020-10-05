(ns mandel-canvas.core
  "Z = Z^2 + C"
  (:require [goog.events :as gevt]
            [goog.events.EventType :as EventType]
            [goog.object :as gobj]
            [butler.core :as butler]
            mandel-canvas.worker ; to see if this is required to make the worker load correctly
            [mandel-canvas.arithmetic.vector :refer [mul add magnitude]]
            mandel-canvas.algorithm.onscreen
            mandel-canvas.algorithm.sequential-vector
            mandel-canvas.algorithm.async-vector
            mandel-canvas.algorithm.color-scheme
            mandel-canvas.algorithm.progressive-vectorized
            mandel-canvas.algorithm.worker-vectorized
            mandel-canvas.coloring.schemes))

(defonce cancel-ref (atom (constantly false)))

(defn cancel!
  []
  (reset! cancel-ref (constantly false)))


(defonce state-ref
  (atom
    (hash-map
      :scheme-key :mandel-canvas.coloring.pink-orange-blue/scheme
      :render-fn mandel-canvas.algorithm.progressive-vectorized/render-async
      ;:context-init-fn mandel-canvas.algorithm.onscreen/init-onscreen-2d-context
      :log println
      :width  500
      :height 500
      :min-x -2
      :max-x 2
      :min-y -2
      :max-y 2)))

(swap! state-ref assoc
  :render-fn mandel-canvas.algorithm.worker-vectorized/render-async
  :context-init-fn mandel-canvas.algorithm.worker-vectorized/init-offscreen-2d-context)

(defonce cursor-ref
  (atom {:x nil :y nil}))

(defn pixel->point
  [{:keys [width height min-x max-x min-y max-y width height]} [pixel-x pixel-y]]
  (let [new-x (+ min-x (* (/ pixel-x width) (- max-x min-x)))
        new-y (+ min-y (* (/ pixel-y height) (- max-y min-y)))]
    [new-x new-y]))

(def mutate #(swap! state-ref merge %))

(defn on-wheel
  [evt]
  (let [state @state-ref
        {:keys [min-x min-y max-x max-y]}  state
        [new-center-x new-center-y] (pixel->point state [(.-offsetX evt) (.-offsetY evt)])
        span-factor (if (neg? (.. evt -event_ -deltaY))  (/ 1 4) 1.5)
        new-span-width-x (* span-factor (- max-x min-x))
        new-span-width-y (* span-factor (- max-y min-y))
        mutation
         {:min-x (- new-center-x new-span-width-x)
          :min-y (- new-center-y new-span-width-y)
          :max-x (+ new-center-x new-span-width-x)
          :max-y (+ new-center-y new-span-width-y)}]
    (js/console.log evt)
    ;(println evt :pp [new-center-x new-center-y] :mutation mutation)
    (mutate mutation)
    (.preventDefault evt)))

(defn on-mousemove
  [evt]
  (reset! cursor-ref
    {:x (.-offsetX evt)
     :y (.-offsetY evt)}))

(defonce app-elem (.getElementById js/document "app"))
(defonce canvas-elem-ref (atom nil))
(defn replace-canvas-elem
  []
  (let [canvas-elem
        (doto (.createElement js/document "canvas")
          (gevt/listen EventType/WHEEL on-wheel)
          (gevt/listen EventType/MOUSEMOVE on-mousemove)
          (gobj/set "height" 100)
          (gobj/set "width" 100))]
    (let [[old-canvas-elem _] (swap-vals! canvas-elem-ref (fn [_] canvas-elem))]
      (when old-canvas-elem
        (.removeChild app-elem old-canvas-elem))
      (.appendChild app-elem canvas-elem)
      canvas-elem)))

(defonce cursor-elem
  (let [cursor-elem (.createElement js/document "div")]
    (.appendChild app-elem cursor-elem)
    cursor-elem))

(defonce caption-elem
  (let [caption-elem (.createElement js/document "div")]
    (.appendChild app-elem caption-elem)
    caption-elem))

(defn do-render
  [{:keys [width height render-fn scheme-key min-x max-x min-y max-y context-init-fn]
    :or {width  500
         height 500
         min-x -1.0
         max-x -0.5
         min-y -0.5
         max-y  0.0
         render-fn mandel-canvas.algorithm.async-vector/render-progressive-async
         context-init-fn mandel-canvas.algorithm.onscreen/init-onscreen-2d-context}
    :as opts}]
  (let [started (js/Date.)
        color-opts (mandel-canvas.coloring.common/color-scheme scheme-key)
        canvas-elem (replace-canvas-elem)
        _ (doto canvas-elem
            (gobj/set "height" height)
            (gobj/set "width" width))

        #_#_
        _ (doto caption-elem
            (gobj/set "height" height)
            (gobj/set "width" width))
        opts (assoc opts
                :width      width
                :height     height
                :min-x      min-x
                :max-x      max-x
                :min-y      min-y
                :max-y      max-y
                :render-fn  render-fn
                :color-opts color-opts)
        opts (assoc opts :rendering-context (context-init-fn canvas-elem))]

    (gobj/set caption-elem "innerHTML"
      (str "<table>"
           "<thead><th></th><th>" "Min" "</th><th>" "Max" "</th><th>" "Difference"    "</th></thead>"
           "<tr><th>X</th><td>" min-x "</td><td>" max-x "</td><td>" (- max-x min-x) "</td></tr>"
           "<tr><th>Y</th><td>" min-y "</td><td>" max-y "</td><td>" (- max-y min-y) "</td></tr>"
           "</table>")
      #_
      (str "<em>X:</em> "    "<span>" min-x "</span> to "
                             "<span>" max-x "</span> (" (- max-x min-x) "), <br>"
           "<em>Y:</em> "    "<span>" min-y "</span> to "
                             "<span>" max-y "</span> (" (- max-y min-y) ")"))
    #_
    (gobj/set caption-elem "innerHTML"
      (str "<em>X:</em> "    "<span>" min-x "</span> to "
                             "<span>" max-x "</span> (" (- max-x min-x) "), <br>"
           "<em>Y:</em> "    "<span>" min-y "</span> to "
                             "<span>" max-y "</span> (" (- max-y min-y) ")"))
    (let [result
          (render-fn
            opts
            (fn done []
              (println "Started:" started ". Completed:" (js/Date.)
                "Elapsed:" (/ (- (js/Date.) started) 1000) "seconds")))]
      (if (fn? result)
        (reset! cancel-ref result)
        (cancel!))
      (println "Done" (gobj/get render-fn "name")))))

(defn do-render-cursor
  [{pixel-x :x, pixel-y :y}]
  (let [{:keys [width height min-x max-x min-y max-y]} @state-ref
        x (+ (* (/ pixel-x width) (- max-x min-x)) min-x)
        y (+ (* (/ pixel-y height) (- max-y min-y)) min-y)]
    (gobj/set cursor-elem "innerHTML"
      (str "<table>"
           "<thead><th></th><th>" "Mouse</th></thead>"
           "<tr><th>X</th><td>" x "</td></tr>"
           "<tr><th>Y</th><td>" y "</td></tr>"
           "</table>"))))

(defonce _startup_render
 (do
  (add-watch cancel-ref :re-render (fn [_key _ref old-value new-value] (old-value)))
  (add-watch state-ref :re-render (fn [_key _ref old-value new-value] (do-render new-value)))
  (add-watch cursor-ref :re-render (fn [_key _ref old-value new-value] (do-render-cursor new-value)))
  (do-render @state-ref)))
