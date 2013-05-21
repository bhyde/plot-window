(in-package #:plot-window)

(define-javascript-library jquery-color (jquery) 
  "http://code.jquery.com/color/jquery.color-2.1.0.min.js"
  ; "https://raw.github.com/jquery/jquery-color/master/jquery.color.js"
  )

(defun j-query-color-box-example ()
  (ps-eval-in-client
    (flet ((colorful ()
             (chain ($ ".block")
                    (animate (create :background-color "rgb(255,180,180)") 1000)
                    (delay 500)
                    (animate (create :background-color "olive") 1000)
                    (delay 500)
                    (animate (create :background-color "#00f") 1000)
                    (delay 500)
                    (animate (create :background-color "snow") 1000)))
           (setup ()
             (chain ($ :body) (empty))
             (chain ($ :body) (append "<button id='go'>Go</button>"))
             (chain ($ :body)
                    (append "<div class='block' style='margin:10px;width:200px;height:100px;border:1px dotted;background-color:snow'>"))))
      (setup)
      (with-js-libraries (jquery-color)
        (chain ($ "#go")
               (click colorful))))))

