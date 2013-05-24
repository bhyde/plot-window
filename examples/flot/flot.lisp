
(define-javascript-library flot (jquery) 
  "/flot/jquery.flot.js"
  (boundp (@ j-query plot)))


(defun flot-example-1 ()
  (ps-eval-in-client
    (with-js-libraries (flot)
      (def-jquery-plugin revise-plot (plotting-instructions)
        (let ((series (@ plotting-instructions series))
              (details (@ plotting-instructions details)))
          (with-each-of-jquery-object (i x this)
            (chain ($ x) (plot series details)))))
      (defun-bah setup-plot (instructions)
        (chain ($ "#ex1")
               (plot (@ instructions series)
                     (@ instructions details))))
      (defun-bah init-scatter-plot ()
        (flet ((d1 ()
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
      (chain ($ :body) (empty))
      (chain ($ :body) (append "<div>") (attr :id "ex1"))
      (funcall-bah init-scatter-plot))))


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
       (:series . (,data-points))
       (:details . ((:points . ,(f '((:show . t) (:radius . 2) (:fill-color . 3))))
                    (:lines . ,(f '((:show . t) (:line-width . 1) (:fill-color . 4)))))))
     *last-websocket-client*))
  nil)
