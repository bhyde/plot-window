(in-package #:plot-window)

(defun demo (&optional fast)
  (loop 
     for (title fnc pause-slow pause-fast?)
     in '(("jquery color animation" j-query-color-box-example 12)
          ("Nicedit, and editor" nicedit-example1 30)
          ("Maps" mapabstraction-example 20 6)
          ("Syntax Highlighting" syntax-highlighter-example-1 8)
          ("D3JS" d3js-example-1 7)
          ("Plot" flot-example-1 0))

       do
       (format t "~&~A" title)
       (funcall fnc)
       (sleep
        (cond
          (fast
           (or pause-fast? (min 3 pause-slow)))
          (t pause-slow)))))

