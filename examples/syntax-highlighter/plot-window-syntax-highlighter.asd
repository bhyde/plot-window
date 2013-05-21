; -*- mode:common-lisp -*-

(defsystem plot-window-syntax-highlighter
  :licence "Apache 2.0"
  :serial t
  :depends-on (#:plot-window)
  :components ((:file "syntax-highlighter")))
