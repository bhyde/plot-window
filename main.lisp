(in-package #:plot-window)

;;;; Startup...

(defun cl-user::initialize-application (&key (port 8765))
  (start-web-server :port port)
  (start-clws-server)
  (start-and-register-json-rpc-resource))

;;;; Incomming Message Handlers

(define-json-message-handler page-log (message)
  (log:info '(websocket page-log) "~S" message))

;;;; Our Web Application, one main page.

(define-javascript-library jquery-json (jquery) 
  "/jquery.json-2.4.js"
  (boundp (@ j-query to-j-s-o-n)))


(define-javascript-library graceful-web-socket (jquery)
  "/jquery.gracefulWebSocket.js"
  (boundp (@ j-query graceful-web-socket)))

#+nil
(define-javascript-library flot (jquery) 
  "/flot/jquery.flot.js"
  (@ j-query plot))

(define-easy-handler (flot-ws :uri "/") ()
  (with-my-page (s :title "Display Window")
    (add-javascript-libraries 'jquery-json 'graceful-web-socket)
    (with-script-in-header (s)
      #+nil
      (def-jquery-plugin revise-plot (plotting-instructions)
        (let ((series (@ plotting-instructions series))
              (details (@ plotting-instructions details)))
          (with-each-of-jquery-object (i x this)
            (chain ($ x) (plot series details)))))
      (let (ws)
        (labels ((send-ws-msg (data)
                   (chain ws (send (chain $ (to-J-S-O-N data)))))
                 (lg (msg)
                   (send-ws-msg (create :type "page-log" :message msg))
                   (chain console (log msg)))
                 (on-message (e)
                   (let* ((msg (chain $ (parse-j-s-o-n (@ e data))))
                          (target (@ msg target))
                          (selection (if target
                                         (j-query (@ msg target))
                                         j-query))
                          (event (@ msg event))
                          (argument (if (@ msg argument)
                                        (@ msg argument)
                                        msg)))
                     (lg (concatenate 'string "Got: " (chain -J-S-O-N (stringify msg))))
                     (funcall (aref selection event) argument)))
                 #+nil
                 (setup-plot (instructions)
                   (chain ($ "#ex1") (plot (@ instructions series) (@ instructions details))))
                 #+nil
                 (init-scatter-plot ()
                   (let ((scatter-plot (create :lines (create :show nil) :points (create show t))))
                     (setup-plot (create :series
                                         (list (create :label "up" :data (loop for i from 1 to 10 collect (list i i)))
                                               (create :label "random" :data (loop for i from 1 to 10 collect (list i (1+ (random 10))))))
                                         :details
                                         (create :series scatter-plot))))
                   (lg "Ex1 is setup."))
                 (init-phase-2 (e)
                   (setf (@ window bah ws) ws)
                   (setf (@ ws onmessage) #'on-message)
                   (setf (@ ws onerror) (lambda () (lg "ws error")))
                   (lg "Hello")
                   #+nil
                   (init-scatter-plot))
                 (start-up ()
                   (setf ws (chain $ (graceful-web-socket (websocket-url))))
                   (setf (chain ws onopen) #'init-phase-2)))
          (setf (@ window bah)
                (create 'lg lg
                        'send-ws-messge send-ws-msg
                        'start-up start-up)))))
    (ps-onready (s) (funcall (@ window bah start-up)))

    (:div :id "ex1" :style (css-lite:inline-css `((width "600px") (height "350px")))
          "Sir!  Your humble window awakes your command.")))


;;;; Finally, the whole point our plot function.

;;; Note that you can tinker with this to make different styles.

#+nil
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


;;;; Sending commands to the window.

(defvar *last-global-eval* nil)

(defun send-global-eval (javascript-text &optional (client *last-websocket-client*))
  (log:debug '(websocket send global-eval) "~S" javascript-text)
  (setf *last-global-eval* javascript-text)
  (send-json-message 
   `((:event . ,(symbol-to-js-string 'global-eval))
     (:argument . ,javascript-text))
   client))

(defmacro ps-eval-in-client* (&rest parenscript-forms)
  `(send-global-eval (ps* ,@parenscript-forms)))

(defmacro ps-eval-in-client (&body parenscript-forms)
  `(send-global-eval
    (ps
      (setf (@ bah last-result)
            (try (progn ,@parenscript-forms)
                 (:catch (e)
                   (funcall-bah lg
                                (concatenate 'string "Error: " (@ e message)))
                   e))))))


