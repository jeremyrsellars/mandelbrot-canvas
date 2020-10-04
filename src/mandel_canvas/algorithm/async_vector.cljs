(ns mandel-canvas.algorithm.async-vector
  (:require [goog.object :as gobj]
            [mandel-canvas.arithmetic.vector :as av]))

(defn render-async
  [{:keys [max-iter color-fn width height rendering-context min-x max-x min-y max-y log]}]
  (let [x-width (- max-x min-x)
        y-width (- max-y min-y)
        remaining-chunks
          (->>
            (for [pixel-x (range width)
                  pixel-y (range height)]
              [pixel-x pixel-y])
            shuffle
            (partition-all 100)
            (into [])
            atom)
        render-chunk
          (fn render-chunk
            [coordinates]
            (doseq [[pixel-x pixel-y] coordinates
                    :let [x (+ min-x (* x-width (/ pixel-x width)))
                          y (+ min-y (* y-width (/ pixel-y height)))
                          c [x y]
                          iter (loop [iter 0
                                      z [0 0]]
                                  (cond (>= iter max-iter)
                                        iter

                                        (>= (av/magnitude z) 4)
                                        iter

                                        :iterate
                                        (recur (inc iter)
                                               (av/add c (av/mul z z)))))]]

              ;(log [pixel-x pixel-y] [x y] iter)
              (gobj/set rendering-context "fillStyle" (color-fn iter))
              (.fillRect rendering-context pixel-x pixel-y 1 1)))

        next-chunk
          (fn next-chunk
            []
            (let [[old new] (swap-vals! remaining-chunks pop)
                  chunk (peek old)]
              (render-chunk chunk)
              (if (seq new)
                (js/setTimeout next-chunk 0)
                (log "Done with async!"))))]
    (next-chunk)))

(defn halvesish
  [n]
  (cond (= 1 n)               [n]
        (even? n)             (let [h (/ n 2)] [h h])
        :odd                  (let [h (js/Math.floor (/ n 2))] [h 1 h])))

(defn quarter
  [[pixel-x pixel-y pixel-width pixel-height :as block]]
  (if (= 1 pixel-width pixel-height)
    [block]
    (let [width-span-widths  (halvesish pixel-width)
          height-span-widths (halvesish pixel-height)
          x-starts (reductions + pixel-x width-span-widths)
          y-starts (reductions + pixel-y height-span-widths)
          width-spans  (map vector x-starts width-span-widths)
          height-spans (map vector y-starts height-span-widths)]
      (for [[x width] width-spans
            [y height] height-spans]
        [x y width height]))))


(defn quarter-all
  [blocks]
  (mapcat quarter blocks))

(def rendering-job
  (atom nil))

(defn render-progressive-async
  [{:keys [max-iter color-fn width height rendering-context min-x max-x min-y max-y log]} done]
  (let [whole-block [0 0 width height]
        job (js/Object.)
        _ (reset! rendering-job job)
        x-width (- max-x min-x)
        y-width (- max-y min-y)
        remaining-blocks
          (atom (into [] (quarter-all (quarter-all [whole-block]))))
        render-chunk
          (fn render-chunk
            [coordinates]
            (doseq [[pixel-x pixel-y pixel-width pixel-height] coordinates
                    :let [x (+ min-x (* x-width (/ pixel-x width)))
                          y (+ min-y (* y-width (/ pixel-y height)))
                          c [x y]
                          iter (loop [iter 0
                                      z [0 0]]
                                  (cond (>= iter max-iter)
                                        iter

                                        (>= (av/magnitude z) 4)
                                        iter

                                        :iterate
                                        (recur (inc iter)
                                               (av/add c (av/mul z z)))))]]

              (gobj/set rendering-context "fillStyle" (color-fn iter))
              (.fillRect rendering-context pixel-x pixel-y pixel-width pixel-height)))

        next-chunk
          (fn next-chunk
            []
            (let [[chunk remaining] (split-at 100 @remaining-blocks)
                  _ (render-chunk chunk)
                  new (reset! remaining-blocks
                        (reduce
                          (fn conj-quarters-when-not-unit-pixel [acc [_ _ w h :as block]]
                            (if (= 1 w h)
                              acc
                              (into acc (quarter block))))
                          (into [] remaining)
                          chunk))]
              (cond (not (identical? job @rendering-job))
                    (log "Aborting old job")

                    (seq new)
                    (js/setTimeout next-chunk 0)

                    :done!
                    (do (log "Done with async!")
                        (when done (done))))))]
    (next-chunk)))
