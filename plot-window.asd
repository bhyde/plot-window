; -*- mode:common-lisp -*-

(defsystem plot-window
  :licence "Apache 2.0"
  :serial t
  :depends-on (#:alexandria             ; only for once-only :(
               #:hunchentoot            ; web server
               #:cl-who                 ; html generation
               #:parenscript            ; javascript via sexprs
               #:css-lite
               #:cl-json
               #:clws                   ; web sockets
               )
  :components ((:file "packages")
               (:file "utilities")
               (:file "servers")
               (:file "main")))
