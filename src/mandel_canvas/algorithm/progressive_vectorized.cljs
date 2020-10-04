(ns mandel-canvas.algorithm.progressive-vectorized
  (:require [goog.object :as gobj]
            [mandel-canvas.arithmetic.vectorized :as av]))

; 2d mapped onto 1d array like this:
; [x0,y0 .. (width-1),y0, x0,y1 .. (width-1),y1, x0,(height-1) .. (width-1),(height-1) ]

(defn iterate-exploded?
  "Iterates the Mandelbrot function Z = Z² + C
   for the complex point at idx in place until the Ziter[idx] >= iteration-count,
   or the Z value explodes where magnitude(Z) > 2.
   Inputs:
     Zr Zi and Cr Ci are parallel arrays representing real and imaginary components of Z and C
     Ziter is a parallel array of the number of iterations computed so far.
     iteration-count is where to stop iterating (starting at Ziter[iter]).
   Mutations:
    Zr[idx] and Zc[idx] are set to the their values at the latest computed iteration
    idx[idx] is set to the latest computed iteration.
   Return:
    true when the Z value exploded for the point
    meaning no further iteration is necessary"
  [idx Zr Zi Cr Ci Ziter iteration-count]
  (let [cr         (aget Cr idx)
        ci         (aget Ci idx)]
    (loop [zr   (aget Zr    idx)
           zi   (aget Zi    idx)
           iter (aget Ziter idx)]
      (let [magnitude-squared (+ (* zr zr) (* zi zi))
            exploded? (> magnitude-squared 4.0)]
        (if (or exploded? (>= iter iteration-count))
          ; done
          (do (aset Zr    idx zr)
              (aset Zi    idx zi)
              (aset Ziter idx iter)
            exploded?)

          ; do another iteration
          (let [next-zr (+ (- (* zr zr) (* zi zi))
                           cr)
                next-zi (+ (* zr zi 2)
                           ci)]
            (recur next-zr next-zi (inc iter))))))))


(def rendering-job
  (atom nil))

(defn paint-to-context
  [rendering-context color-fn color-iter pixel-r pixel-i pixel-r-width pixel-i-width]
  (gobj/set rendering-context "fillStyle" (color-fn color-iter))
  (.fillRect rendering-context pixel-r pixel-i (or pixel-r-width 1) (or pixel-i-width 1)))

(defn render-async
  [{:keys [max-iter iter-steps rendering-context color-fn width height min-x max-x min-y max-y log paint-fn]
    :or {:paint-fn paint-to-context}}
   done
   chunk-complete]
  (let [done (or done (fn [& args]))
        chunk-complete (or chunk-complete (fn [& args]))
        job (js/Object.)
        _ (reset! rendering-job job)
        continue? (fn continue? [] (identical? job @rendering-job))

        max-iter (or max-iter 13)
        iter-steps (or iter-steps
                       (let [chunk-iterations (int (max 1 (/ max-iter 3.4)))]
                         (->> (cons max-iter (range chunk-iterations max-iter chunk-iterations))
                              distinct
                              (sort-by identity)
                              (into []))))

        min-r min-x
        min-i min-y
        max-r max-x
        max-i max-y
        length (* width height)
        Zr (js/Float64Array. length)
        Zi (js/Float64Array. length)
        Cr (js/Float64Array. length)
        Ci (js/Float64Array. length)
        Ziter (js/Int16Array. length)
        #_#_
        _ (do (assert (= length (.-length Zr)))
              (assert (= length (.-length Zi)))
              (assert (= length (.-length Cr)))
              (assert (= length (.-length Ci)))
              (assert (= length (.-length iter))))

        unexploded-indexes (atom (into #{} (shuffle (range length))))
        chunks-in-process (atom (list))

        width-r (- max-r min-r)
        width-i (- max-i min-i)]

    ; initialize arrays
    (doseq [pixel-r (range width)
            pixel-i (range height)
            :let [cr (+ min-r (* width-r (/ pixel-r width)))
                  ci (+ min-i (* width-i (/ pixel-i height)))
                  idx (+ (* pixel-i width) pixel-r)]]
      (when (pos? max-iter)
        ; first iteration is always Z=c
        (aset Zr    idx cr)
        (aset Zi    idx ci)
        (aset Ziter idx 1))
      (aset Cr    idx cr)
      (aset Ci    idx ci))

    (let [render-chunk
          (fn render-chunk
            [indexes iteration-count]
            (doseq [idx indexes]
             (let [pixel-r (mod idx width)
                   pixel-i (int (/ idx width))]
                (when (iterate-exploded? idx Zr Zi Cr Ci Ziter iteration-count)
                  (swap! unexploded-indexes disj idx))

              (let [iter-so-far (aget Ziter idx)
                    color-iter (if (= iter-so-far iteration-count)
                                  max-iter ; imagine it will not explode
                                  iter-so-far)]
                #_
                (when (> (- iteration-count 10) iter-so-far)
                  (log idx [pixel-r pixel-i] iter-so-far max-iter color-iter))
                (paint-fn rendering-context color-fn color-iter pixel-r pixel-i 1 1)))))
          next-chunk
            (fn next-chunk
              [[iteration-count :as iter-steps] max-iter]
              (let [iteration-count (min (or iteration-count max-iter) max-iter)]
                (cond (not (continue?))
                      (log "Aborting old job")

                      :more-to-do
                      (let [[old new-chunks] (swap-vals! chunks-in-process rest)
                            chunk (first old)]

                        ; do chunk work
                        (when (seq chunk)
                          ;(log "rendering chunk" (count chunk) "pixels" "up-to" iteration-count "iterations")
                          (render-chunk chunk iteration-count)
                          ;(log "done rendering chunk"))
                          (chunk-complete))

                        ; schedule more work
                        (cond (seq new-chunks)
                              (js/setTimeout #(next-chunk iter-steps max-iter) 0)

                              (<= max-iter iteration-count)
                              (log "Done with async" iteration-count "iterations"
                                (done))

                              (seq @unexploded-indexes)
                              (let [next-iter-steps (or (next iter-steps)
                                                        (log "ran out of next-iter-steps")
                                                        [max-iter])]
                                #_(log "Going deeper" next-iter-steps "then" (count next-iter-steps) "steps remaining")
                                (reset! chunks-in-process (partition-all 1000 (shuffle (seq @unexploded-indexes))))
                                (js/setTimeout #(next-chunk next-iter-steps max-iter) 0))

                              :all-exploded
                              (log "Done with async -- everything exploded" iteration-count "iterations"
                                (done)))))))]


      (next-chunk (cons 0 iter-steps) max-iter)))) ; first one is dropped
