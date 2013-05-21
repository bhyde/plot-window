(in-package #:plot-window)

(define-javascript-library syntax-highlighter-core () 
  "http://agorbatchev.typepad.com/pub/sh/3_0_83/scripts/shCore.js")
(define-javascript-library syntax-javascript-highlighter (syntax-highlighter-core) 
  "http://agorbatchev.typepad.com/pub/sh/3_0_83/scripts/shBrushJScript.js") 

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
    
(defpsmacro with-css ((url) &body body)
  `(flet ((continuation () 
            (chain console (log (concatenate'string "loaded: " ,url)))
            ,@body))
     (chain ($ "<link>")
          (attr (create :rel "stylesheet"
                        :type "text/css"
                        :href ,url
                        :onload #'continuation))
          (append-to "head"))))

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
                   (with-js-libraries (syntax-javascript-highlighter)
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
                (with-js-libraries (syntax-javascript-highlighter)
                  (f)))))))))


#+nil
(define-easy-handler (SyntaxHighlighter-foo :uri "/sh") ()
  (with-my-page (s)
    (add-javascript-libraries 'jquery 'syntax-javascript-highlighter)
    (with-header-fragment (s)
      (:link
       :rel "stylesheet"
       :type "text/css"
       :href "http://agorbatchev.typepad.com/pub/sh/3_0_83/styles/shCore.css")
      (:link
       :rel "stylesheet"
       :type "text/css"
       :onload "javascript:alert(123)"
       :href "http://agorbatchev.typepad.com/pub/sh/3_0_83/styles/shThemeDefault.css"))
    (ps-onready (s)
      (chain -syntax-highlighter (all)))
    (:body
     (:pre
      :class "brush: js"
      (str
       (lisp
        (ps (defun f (x) (+ 1 x)))))
      ))))

