(ns mandel-canvas.arithmetic.vector)

(defn add
  [[r1 i1] [r2 i2]]
  [(+ r1 r2) (+ i1 i2)])

(defn mul
  [[r1 i1] [r2 i2]]
  [(- (* r1 r2) (* i1 i2))
   (+ (* r1 i2) (* r2 i1))])

(defn magnitude
  [[r i]]
  (Math/sqrt (+ (* r r) (* i i))))
