; -*- mode:common-lisp -*-

(defsystem plot-window-timeline
  :licence "Apache 2.0"
  :serial t
  :depends-on (#:plot-window)
  :components ((:file "timeline")))
