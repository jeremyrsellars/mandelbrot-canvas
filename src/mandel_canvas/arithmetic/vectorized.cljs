(ns mandel-canvas.arithmetic.vectorized)

; 2d mapped onto 1d array like this:
; [x0,y0 .. (width-1),y0, x0,y1 .. (width-1),y1, x0,(height-1) .. (width-1),(height-1)]

(defn add
  [idx arr-1 arr-2]
  (+ (nth arr-1 idx) (nth arr-2 idx)))

(defn mul
  [idx arr-1 arr-2]
  (* (nth arr-1 idx) (nth arr-2 idx)))

(defn magnitude-squared
  [idx arr-r arr-i]
  (let [r (nth arr-r idx)
        i (nth arr-i idx)]
    (+ (* r r) (* i i))))
