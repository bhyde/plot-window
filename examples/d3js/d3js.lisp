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


(defun d3js-graph-of-asdf-system (system-designator)
  (let ((system-ids (make-hash-table :test #'eq)))
    (macrolet ((system-id (s) `(gethash ,s system-ids))
               (depends-on (s) `(slot-value ,s 'asdf/component::sibling-dependencies))
               (system-name (s) `(slot-value ,s 'asdf/component::name)))
      (let ((c -1))
        (labels ((recure (system)
                   (unless (system-id system)
                     (setf (system-id system) (incf c))
                     (loop 
                        for requirement in (depends-on system)
                        do (recure (asdf:find-system requirement))))))
          (recure (asdf:find-system system-designator))))
      (flet ((graph-node-of-system (s) `((:name ,@(system-name s)) (:group . 1)))
             (graph-links-of-system (s)
               (loop 
                  with id = (system-id s)
                  for requirement in (depends-on s)
                  as rid = (system-id (asdf:find-system requirement))
                  collect `((:target ,@id) (:source ,@rid ) (:wieght . 1)))))
        `((:nodes ,@(loop
                       with all-systems = (sort (alexandria:hash-table-keys system-ids)
                                                #'< :key #'(lambda (s) (system-id s)))
                       for sys in all-systems
                       collect (graph-node-of-system sys)))
          (:links ,@(loop
                       for sys being each hash-key in system-ids
                       nconc (graph-links-of-system sys))))))))

