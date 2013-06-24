(in-package #:plot-window)

(declare-javascript-library d3js ()
  :url "http://d3js.org/d3.v3.min.js"
  :loaded-p (boundp d3))

(defun d3js-example-1 ()
  (ps-eval-in-client
    (let ((new-element ($ "<div><svg style='width:300px;height:200px;'/></div>")))
      (flet ((sin-wave (amplitude)
               (loop for i from 1 to 40
                  collect (create :x i
                                  :y (* amplitude (sin (/ i 2))))))
             (dx (d)
               (let ((f (chain d3 scale (linear) (domain '(1 40)) (range '(10 290)))))
                 (funcall f (@ d x))))
             (dy (d)
               (let ((f (chain d3 scale (linear) (domain '(-10 10)) (range '(10 190)))))
                 (funcall f (@ d y))))
             (animation ()
               (with-javascript-modules (d3js)
                 (let ((x (chain d3 ;; A selection of N circles
                                 (select (aref ($ :svg new-element) 0))
                                 (select-all :circle)
                                 (data (sin-wave 10)))))
                   (flet ((f (d) (random 150)))
                     (chain x
                            (enter)
                            (append :circle)
                            (attr :r 3)
                            (attr :fill :lightgray)
                            (attr :cx #'f)
                            (attr :cy #'f)))
                   (chain x ;; sweep into the 1st sin wave
                          (transition)
                          (delay 300)
                          (attr :r 3)
                          (attr :cx #'dx)
                          (attr :cy #'dy)
                          (attr :fill :cornsilk)
                          (attr :stroke :cornflowerblue))
                   (chain x
                          (data (sin-wave -10)) ;; sweep into a 2nd sin wave
                          (transition)
                          (delay 1250)
                          (attr :fill :darkgray)
                          (attr :cx #'dx)
                          (attr :cy #'dy))))))
        (chain dw (insert-element new-element animation))))))





