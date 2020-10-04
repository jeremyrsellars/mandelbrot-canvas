(ns mandel-canvas.coloring.schemes
  (:require mandel-canvas.coloring.common
            mandel-canvas.coloring.pink
            mandel-canvas.coloring.pink-green
            mandel-canvas.coloring.pink-orange-blue
            mandel-canvas.coloring.quick))

(def scheme-keys
  (keys (methods mandel-canvas.coloring.common/color-scheme)))
