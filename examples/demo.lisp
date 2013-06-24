(in-package #:plot-window)
(named-readtables:in-readtable :cl-interpol)

(defun demo (&optional fast)
  (clear-display-window)
  (loop 
     for (title fnc pause-slow pause-fast?)
     in '(("Example Animation (via jQuery Color Box)" j-query-color-box-example 12)
          ("Richtext editing (via Nicedit)" nicedit-example1 30)
          ("Maps (via mapabstration)" mapabstraction-example 20 6)
          ("Syntax Highlighting" syntax-highlighter-example-1 8)
          ("D3JS is cool" d3js-example-1 7)
          ("Plots (using flot)" flot-example-1 0))
       do
       (format t "~&~A" title)
       (funcall fnc)
       (ps-eval-in-client* `(chain dw (insert-element ($ ,#?"<h2>${title}</h2>"))))
       (sleep
        (cond
          (fast
           (or pause-fast? (min 3 pause-slow)))
          (t pause-slow)))))

