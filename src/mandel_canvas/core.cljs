(ns mandel-canvas.core
  "Z = Z^2 + C"
  (:require [goog.events :as gevt]
            [goog.events.EventType :as EventType]
            [goog.object :as gobj]
            [mandel-canvas.arithmetic.vector :refer [mul add magnitude]]
            mandel-canvas.algorithm.sequential-vector
            mandel-canvas.algorithm.async-vector
            mandel-canvas.algorithm.color-scheme
            mandel-canvas.coloring.pink
            mandel-canvas.coloring.pink-green
            mandel-canvas.coloring.quick))

(defonce state-ref
  (atom
    (assoc mandel-canvas.coloring.pink/opts
      :width  500
      :height 500
      :min-x -2
      :max-x 2
      :min-y -2
      :max-y 2)))
#_
(reset! state-ref
  (assoc mandel-canvas.coloring.pink/opts
    :render-fn mandel-canvas.algorithm.color-scheme/render-async
    :width  500
    :height 500
    :min-x 0
    :max-x 500
    :min-y 0
    :max-y 500))


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
(defonce canvas-elem
  (let [canvas-elem
        (doto (.createElement js/document "canvas")
          (gevt/listen EventType/WHEEL on-wheel)
          (gevt/listen EventType/MOUSEMOVE on-mousemove)
          (gobj/set "height" 100)
          (gobj/set "width" 100))]
    (.appendChild app-elem canvas-elem)
    canvas-elem))

(defonce cursor-elem
  (let [cursor-elem (.createElement js/document "div")]
    (.appendChild app-elem cursor-elem)
    cursor-elem))

(defonce caption-elem
  (let [caption-elem (.createElement js/document "div")]
    (.appendChild app-elem caption-elem)
    caption-elem))

(defn do-render
  [{:keys [width height render-fn color-opts min-x max-x min-y max-y]
    :or {width  500
         height 500
         min-x -1.0
         max-x -0.5
         min-y -0.5
         max-y  0.0
         render-fn mandel-canvas.algorithm.async-vector/render-progressive-async
         color-opts mandel-canvas.coloring.pink/opts}
    :as opts}]
  (let [started (js/Date.)
        _ (doto canvas-elem
            (gobj/set "height" height)
            (gobj/set "width" width))
        _ (doto caption-elem
            (gobj/set "height" height)
            (gobj/set "width" width))
        ctx
          (doto (.getContext canvas-elem "2d")
            (gobj/set "mozImageSmoothingEnabled" false)
            (gobj/set "webkitImageSmoothingEnabled" false)
            (gobj/set "fillStyle" "#888")
            #_
            (.fillRect 0 0 (.-width canvas-elem) (.-height canvas-elem)))
        opts (assoc opts
                :width      width
                :height     height
                :min-x      min-x
                :max-x      max-x
                :min-y      min-y
                :max-y      max-y
                :render-fn  render-fn
                :color-opts color-opts
                :rendering-context ctx)]

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
    (println "Done" (gobj/get render-fn "name")
        (render-fn
          opts
          (fn done []
            (println "Started:" started ". Completed:" (js/Date.)
              "Elapsed:" (/ (- (js/Date.) started) 1000) "seconds"))))))

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
  (add-watch state-ref :re-render (fn [_key _ref old-value new-value] (do-render new-value)))
  (add-watch cursor-ref :re-render (fn [_key _ref old-value new-value] (do-render-cursor new-value)))
  (do-render @state-ref)))

