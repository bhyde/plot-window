(in-package #:cl-user)

(defpackage #:plot-window
  (:use 
   ;; Basics :)
   #:common-lisp
   ;; Output
   #:hunchentoot #:cl-who #:parenscript)
  (:import-from #:alexandria #:once-only)
  (:shadowing-import-from #:css-lite 
                          ; not  #:%  which conflicst with parenscript
                          #:*css-stream* #:*indent-css*
                          #:comment  #:css  #:css-id-name
                          #:css-string  #:inline-css  #:make-css-func
                          #:make-css-var #:pt #:px)
  (:nicknames "PW")
  (:export 
   ;; also cl-user:initalize-application
   #:plot))
