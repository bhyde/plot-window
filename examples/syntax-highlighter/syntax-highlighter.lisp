(in-package #:plot-window)

(declare-javascript-library syntax-highlighter-core () 
  :url "http://agorbatchev.typepad.com/pub/sh/3_0_83/scripts/shCore.js"
  :loaded-p (boundp -syntax-highlighter))

(declare-javascript-library syntax-javascript-highlighter (syntax-highlighter-core) 
  :url "http://agorbatchev.typepad.com/pub/sh/3_0_83/scripts/shBrushJScript.js"
  :loaded-p (boundp (@ -syntax-highlighter brushes -j-script)))

;(define-css-library syntax-highlighter-default-core ()
;  "http://agorbatchev.typepad.com/pub/sh/3_0_83/styles/shCore.css")
;(define-css-library syntax-highlighter-default (syntax-highlighter-core)
;  "http://agorbatchev.typepad.com/pub/sh/3_0_83/styles/shThemeDefault.css")

(defun add-css-loader (s)
  (unless (page-property :css-loader)
    (setf (page-property :css-loader) t)
    (with-header-fragment (s)
      (defun load-some-css (name continuation)
       (chain ($ :head)
              (append (who-ps-html
                       (:link
                        :rel :stylesheet
                        :type "text/css"
                        :href url
                        :onload continuation))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)    
  (defpsmacro with-css ((url) &body body)
    `(flet ((continuation () 
              (chain console (log (concatenate'string "loaded: " ,url)))
              ,@body))
       (chain ($ "<link>")
              (attr (create :rel "stylesheet"
                            :type "text/css"
                            :href ,url
                            :onload #'continuation))
              (append-to "head")))))

(defun syntax-highlighter-example-1 ()
  (let* ((example
         '(flet ((f ()
                  (chain ($ :body)
                         (append
                          (who-ps-html 
                           (:pre :style "height:30em" :class "brush: js"
                                 (lisp (ps* '(defun f (x) (+ x 12))))))))
                  (chain -syntax-highlighter (highlight (create :light t) (aref ($ 'pre) 0)))))
           (if (equal (typeof -syntax-highlighter) "object")
               (f)
               (with-css ("http://agorbatchev.typepad.com/pub/sh/3_0_83/styles/shCore.css")
                 (with-css ("http://agorbatchev.typepad.com/pub/sh/3_0_83/styles/shThemeDefault.css")
                   (with-javascript-modules (syntax-javascript-highlighter)
                     (f))))))))
    (ps-eval-in-client
      (chain ($ :body) (empty))
      (flet ((f ()
               (chain ($ :body)
                      (append
                       (who-ps-html 
                        (:pre :style "height:30em" :class "brush: js"
                              (lisp (ps* example))))))
               (chain -syntax-highlighter (highlight (create :light t) (aref ($ 'pre) 0)))))
        (if (equal (typeof -syntax-highlighter) "object")
            (f)
            (with-css ("http://agorbatchev.typepad.com/pub/sh/3_0_83/styles/shCore.css")
              (with-css ("http://agorbatchev.typepad.com/pub/sh/3_0_83/styles/shThemeDefault.css")
                (with-javascript-modules (syntax-javascript-highlighter)
                  (f)))))))))



