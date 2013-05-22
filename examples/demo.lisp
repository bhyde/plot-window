(in-package #:plot-window)

(defun demo ()
  (loop for (title fnc pause) in '(("jquery color animation" j-query-color-box-example 12)
                                   ("Nicedit, and editor" nicedit-example1 30)
                                   ("Maps" mapabstraction-example 20)
                                   ("Syntax Highlighting" syntax-highlighter-example-1 8)
                                   ("D3JS" d3js-example-1 0))

       do
       (format t "~&~A" title)
       (funcall fnc)
       (sleep pause)))
