(in-package #:plot-window)

(declare-javascript-library jquery-color (jquery)
  :url "http://code.jquery.com/color/jquery.color-2.1.0.min.js"
  ; "https://raw.github.com/jquery/jquery-color/master/jquery.color.js"
  :loaded-p (boundp (@ j-query -color)))

(defun j-query-color-box-example ()
  (ps-eval-in-client
    (macrolet ((ps-css (&rest static-rules) 
                 (apply #'inline-css static-rules)))
      (let ((element
             ($
              (who-ps-html
               (:div
                (:button "go")
                (:div :style (ps-css :margin "10px"
                                     :width "200px"
                                     :height "100px"
                                     :border "1px dotted"
                                     :background-color "snow")))))))
        (flet ((color-animation ()
                 (chain ($ :div element)
                        (animate (create :background-color "rgb(255,180,180)") 1000)
                        (delay 500)
                        (animate (create :background-color "olive") 1000)
                        (delay 500)
                        (animate (create :background-color "#00f") 1000)
                        (delay 500)
                        (animate (create :background-color "snow") 1000))))
          (with-javascript-modules (jquery-color)
            (chain ($ :button element) (click color-animation))
            (chain dw (insert-element element))))))))

