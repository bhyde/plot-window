; -*- mode:common-lisp -*-

(defsystem plot-window
  :licence "Apache 2.0"
  :serial t
  :depends-on (#:alexandria             ; only for once-only :(
               #:log4cl
               #:optima
               #:hunchentoot            ; web server
               #:cl-who                 ; html generation
               #:cl-interpol            ; also for html generation
               #:parenscript            ; javascript via sexprs
               #:css-lite
               #:cl-json
               #:clws                   ; web sockets
               )
  :components ((:file "packages")
               (:file "utilities")
               (:file "log4cl-memory-appender")
               (:file "servers")
               (:file "main")))
