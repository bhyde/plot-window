(in-package #:plot-window)

(defun demo ()
  (loop for (title fnc pause) in '(("jquery color animation" j-query-color-box-example 10)
                             ("Nicedit, and editor" nicedit-example1 20)
                             ("Maps" mapabstraction-example 10)
                             ("Syntax Highlighting" syntax-highlighter-example-1 0))

       do
       (format t "~&~A" title)
       (funcall fnc)
       (sleep pause)))
