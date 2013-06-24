(in-package #:plot-window)

(declare-javascript-library flot (jquery) 
  :url "/flot/jquery.flot.js"
  :loaded-p (boundp (@ j-query plot)))

(define-javascript-code-module my-flot ()
  :where dw
  :loaded-p (boundp (@ dw my-flot))
  :requires (flot))

(defun-javascript (my-flot init-scatter-plot) (jq)
  (flet ((setup-plot (instructions)
           (chain jq
                  (plot (@ instructions series)
                        (@ instructions details))))
         (d1 ()
           (create :label "up"
                   :data (loop for i from 1 to 10
                            collect (list i i))))
         (d2 ()
           (create :label "random"
                   :data (loop for i from 1 to 10
                            collect (list i (1+ (random 10)))))))
    (setup-plot (create
                 :series (list (d1) (d2))
                 :details 
                 (create :series
                         (create :lines (create :show nil)
                                 :points (create show t)))))))

(defun-javascript (my-flot initialize) ()
  (def-jquery-plugin revise-plot (plotting-instructions)
    (let ((series (@ plotting-instructions series))
          (details (@ plotting-instructions details)))
      (with-each-of-jquery-object (i x this)
        (chain ($ x) (plot series details))))))

(defun flot-example-1 ()
  (ps-eval-in-client
    (let ((new-plot
           ($ "<div/>"
              (create :id "ex1"
                      :width "500px" :height "500px"))))
      (with-javascript-modules (my-flot)
        (chain dw (insert-element new-plot
                                  (lambda ()
                                    ((@ dw my-flot init-scatter-plot) new-plot))
                                  ))))))

(defun plot (&optional (data-points (flet ((f (i n)
                                             (/ (* i (sin i)) n)))
                                      (loop
                                         for i below 100 by .4
                                         collect (list i (f i 100))))))
  (flet ((f (alist)
           ;; we need this since cl-json's heuristic for deciding what's an alist
           ;; and hence should become an object doesn't work for some of our needs.
           (alexandria:alist-hash-table alist)))
    (send-json-message
     `((:target . "#ex1")
       (:event . ,(symbol-to-js-string 'revise-plot))
       (:series . (,(f `((:data . ,data-points)
                        ))))
       (:details . (#+nil (:points . ,(f '((:show . t) (:radius . 5) (:fill-color . 3))))
                    #+nil (:lines . ,(f '((:show . t) (:line-width . 1) (:fill-color . 4))))
                    (:xaxes . (,(f '((:ticks . 4)))))
                    (:yaxes . (,(f '((:ticks . 4))))))))
     *last-websocket-client*))
  nil)
