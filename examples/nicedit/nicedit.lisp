(in-package #:plot-window)

(define-javascript-library nicedit () 
  "http://js.nicedit.com/nicEdit-latest.js"
  (boundp nic-editor))

(defun nicedit-example1 ()
  (ps-eval-in-client
    (chain ($ :body) (empty))
    (chain ($ :body) (append "<div id='ta' style='height:30em'/>"))
    (with-js-libraries (nicedit)
      (chain (new (nic-Editor (create 'full-panel t)))
             (panel-instance :ta (create 'has-panel t))))))

