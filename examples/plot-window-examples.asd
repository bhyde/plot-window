; -*- mode:common-lisp -*-

(defsystem plot-window-examples
  :licence "Apache 2.0"
  :serial t
  :depends-on (plot-window-mapstraction 
               plot-window-syntax-highlighter 
               plot-window-nicedit
               ;; plot-window-timeline
               plot-window-j-query-color-box
               plot-window-d3js
               plot-window-flot)
  :components ((:file "demo")))
