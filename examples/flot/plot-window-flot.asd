; -*- mode:common-lisp -*-

(defsystem plot-window-flot
  :licence "Apache 2.0"
  :serial t
  :depends-on (#:plot-window)
  :components ((:file "flot")))
