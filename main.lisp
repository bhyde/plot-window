(in-package "PLOT-WINDOW")

;;;; Startup...

(defun cl-user::initialize-application (&key (port 8765))
  (start-web-server :port port)
  (start-clws-server)
  (start-and-register-json-rpc-resource))

;;;; Incomming Message Handlers

(define-json-message-handler page-log (message)
  (lg-message* 2 "WS/~S: ~S" *current-websocket-client* message))

;;;; Our Web Application, one main page.

(define-javascript-library jquery-json (jquery) "/jquery.json-2.4.js")

(define-javascript-library graceful-web-socket (jquery) "/jquery.gracefulWebSocket.js")

(define-javascript-library flot (jquery) "/flot/jquery.flot.js")

(define-easy-handler (flot-ws :uri "/") ()
  (with-my-page (s :title "Flot Examples")
    (add-javascript-libraries 'flot 'jquery-json 'graceful-web-socket)
    (with-script-in-header (s)
      (def-jquery-plugin revise-plot (plotting-instructions)
        (let ((series (@ plotting-instructions series))
              (details (@ plotting-instructions details)))
          (with-each-of-jquery-object (i x this)
            (chain ($ x) (plot series details))))))
    (ps-onready (s)
      ;; Connect to the websocket, establish handlers, init plot.
      (let ((ws (chain $ (graceful-web-socket (websocket-url)))))
        (labels ((lg (msg)
                   (send-ws-msg (create :type "page-log" :message msg))
                   (chain console(log msg)))
                 (send-ws-msg (data)
                   (chain ws (send (chain $ (to-J-S-O-N data)))))
                 (on-message (e)
                   (let* ((result (chain $ (parse-j-s-o-n (@ e data))))
                          (targets (j-query (@ result target)))
                          (event (@ result event)))
                     (lg (concatenate 'string "Got: " (chain -J-S-O-N (stringify result))))
                     (funcall (aref targets event) result)))
                 (setup-plot (instructions)
                   (chain ($ "#ex1") (plot (@ instructions series) (@ instructions details))))
                 (init-scatter-plot ()
                   (let ((scatter-plot (create :lines (create :show nil) :points (create show t))))
                     (setup-plot (create :series
                                         (list (create :label "up" :data (loop for i from 1 to 10 collect (list i i)))
                                               (create :label "random" :data (loop for i from 1 to 10 collect (list i (1+ (random 10))))))
                                         :details
                                         (create :series scatter-plot))))
                   (lg "Ex1 is setup."))
                 (init-phase-2 (e)
                   (lg "Hello")
                   (setf (chain ws onmessage) #'on-message)
                   (init-scatter-plot)))
          (setf (chain ws onopen) #'init-phase-2))))
    (:div :id "ex1" :style (css-lite:inline-css `((width "600px") (height "350px"))))))


;;;; Finally, the whole point our plot function.

(defun plot (&optional (data-points '((1 1) (2 2) (1 4))))
  (send-json-message
   `((:target . "#ex1")
     (:event . "revisePlot")
     (:series . (,data-points))
     (:details . ((:points . ((:show . t) (:radius . 2) (:fill-color . 3)))
                  (:lines . ((:show . t) (:line-width . 1) (:fill-color . 4))))))
   *last-websocket-client*)
  nil)
