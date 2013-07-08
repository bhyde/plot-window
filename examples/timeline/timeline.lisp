(in-package #:plot-window)


(define-javascript-library timeline ()
  "http://api.simile-widgets.org/timeline/2.3.1/timeline-api.js")

(defun timeline-example-1 ()
  (ps-eval-in-client
    (let (t1)
      (flet ((lg (x) (chain console (log x)))
             (init-timeline ()
               (let ((band-infos
                      (list
                       (chain -timeline 
                              (createBandInfo 
                               (create
                                :width "70%"
                                interval-unit (chain -timeline (@ -date-time -m-o-n-t-h))
                                interval-pixels 100)))
                       (chain -timeline 
                              (createBandInfo 
                               (create
                                :width "30%"
                                interval-unit (chain -timeline (@ -date-time -y-e-a-r))
                                interval-pixels 200))))))
                 (chain -timeline
                        (create (chain document (getElementById "timeline"))
                                band-infos))))
             (setup ()
               (chain ($ :body) (empty))
               (chain ($ :body)
                      (append 
                       "<div id='timeline' style='height:250px;border:1px gray;background-color:snow'/>"))))
        (setup)
;        (setf t1 (init-timeline))
;        (lg t1)
        ))))

