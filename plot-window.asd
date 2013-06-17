; -*- mode:common-lisp -*-

#-asdf3 (error "Plot-window requires ASDF3.")

(defsystem plot-window
  :licence "Apache 2.0"
  :serial t
  :depends-on (#:uiop
               #:alexandria
               #:log4cl
               #:optima
               #:hunchentoot      ; web server
               #:cl-who           ; html generation
               #:cl-interpol      ; also for html generation
               #:parenscript      ; javascript via sexprs
               #:css-lite
               #:clws             ; web sockets
               #:cl-json)
  :components ((:file "packages")
               (:file "ps-utilities")
               (:file "ps-modules")
               (:file "dw")
               (:file "utilities")
               (:file "log4cl-memory-appender")
               (:file "servers")
               (:file "main")))
