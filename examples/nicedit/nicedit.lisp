(in-package #:plot-window)

(declare-javascript-library nicedit ()
  :url "http://js.nicedit.com/nicEdit-latest.js"
  :loaded-p (boundp nic-editor))

(defun nicedit-example1 ()
  (ps-eval-in-client
    (let ((new-element ($ "<div/>" (create :height "250px" :width "600px"))))
      (with-javascript-modules (nicedit)
        (chain dw
               (insert-element
                new-element
                (lambda ()
                  (chain (new (nic-Editor (create 'full-panel t)))
                         (panel-instance (aref new-element 0) (create 'has-panel t))))))))))

