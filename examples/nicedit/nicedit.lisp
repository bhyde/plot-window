(in-package #:plot-window)

(declare-javascript-library nicedit ()
  :url "http://js.nicedit.com/nicEdit-latest.js"
  :loaded-p (boundp nic-editor))

(defun nicedit-example1 ()
  (ps-eval-in-client
    (chain ($ :body) (empty))
    (chain ($ :body) (append "<div id='ta' style='height:30em'/>"))
    (with-javascript-modules (nicedit)
      (chain (new (nic-Editor (create 'full-panel t)))
             (panel-instance :ta (create 'has-panel t))))))

