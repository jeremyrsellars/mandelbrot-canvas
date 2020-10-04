(ns mandel-canvas.coloring.common)

(defmulti color-scheme
  (fn color-scheme-dispatch [scheme-key]
    scheme-key)
  :default :default-scheme)
