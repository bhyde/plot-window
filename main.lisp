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

(define-javascript-library jquery-json (jquery) "/jquery.json-2.4.js")

(define-javascript-library graceful-web-socket (jquery) "/jquery.gracefulWebSocket.js")

(define-javascript-library flot (jquery) "/flot/jquery.flot.js")

(define-easy-handler (flot-ws :uri "/") ()
  (with-my-page (s :title "Display Window")
    (add-javascript-libraries 'flot 'jquery-json 'graceful-web-socket)
    (with-script-in-header (s)
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
                   (setf (@ window bah ws) ws)
                   (setf (@ ws onmessage) #'on-message)
                   (setf (@ ws onerror) (lambda () (lg "ws error")))
                   (lg "Hello")
                   (init-scatter-plot))
                 (start-up ()
                   (setf ws (chain $ (graceful-web-socket (websocket-url))))
                   (setf (chain ws onopen) #'init-phase-2)))
          (setf (@ window bah)
                (create 'lg lg
                        'send-ws-messge send-ws-msg
                        'start-up start-up)))))
    (ps-onready (s) (funcall (@ window bah start-up)))
    (:div :id "ex1" :style (css-lite:inline-css `((width "600px") (height "350px"))))))


;;;; Finally, the whole point our plot function.

;;; Note that you can tinker with this to make different styles.

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


;;;; Part of an experiment...

(defun send-global-eval (javascript-text &optional (client *last-websocket-client*))
  (log:debug '(websocket send global-eval) "~S" javascript-text)
  (send-json-message 
   `((:event . ,(symbol-to-js-string 'global-eval))
     (:argument . ,javascript-text))
   client))

(defmacro ps-eval-in-client* (&rest parenscript-forms)
  `(send-global-eval (ps* ,@parenscript-forms)))

(defmacro ps-eval-in-client (&body parenscript-forms)
  `(send-global-eval (ps ,@parenscript-forms)))


;;; Logging...

(in-package #:log4cl)

(defclass memory-appender (serialized-appender)
  ((max :initform 500 :initarg :max-elements )
   (count :initform 0)
   (transcript :initform ()))
  (:documentation "collects recent log messages"))

(unless (boundp '.ndc-is-unbound.)
  (defconstant .ndc-is-unbound. (make-symbol "NDC-IS-UNBOUND")))

(defmethod appender-do-append ((this memory-appender)
                               logger
			       level
                               log-func)
  (with-slots (max count transcript layout) this
    (when (<= max count)
      (decf count 20)
      (setf transcript (subseq transcript 0 (- max 21))))
    (incf count)
    (push
     (list logger level
           (if (boundp 'log4cl::*ndc-context*)
               'log4cl::*ndc-context*
               .ndc-is-unbound.)
           (with-output-to-string (s)
             (layout-to-stream layout s logger level log-func)))
     transcript)))

(defmethod clear-me ((this memory-appender))
  (with-slots (count transcript) this
    (setf count 0
          transcript ())))

(defmethod show ((this memory-appender) &key (classes t) (level :debug) (pattern "."))
  (declare (ignore classes))
  (flet ((id (logger)
           (if (eq logger *root-logger*)
               "+<root>"
               (logger-category logger))))
    (with-slots (transcript) this
      (loop
         with level-n = (make-log-level level)
         for (logger level ndc text) in (reverse (subseq transcript 0 (min 50 (length transcript))))
         when (<= level level-n)
           do (let ((msg (format nil "~(~a~) ~d  ~A~:[~; ~S~]"
                          (id logger) level text (eq ndc .ndc-is-unbound.) ndc)))
                (when (cl-ppcre:scan pattern msg)
                  (format t "~&~A" msg)))))))


(defparameter *my-log* nil)

(defun establish-my-logger ()
  (assert (not *my-log*) () "Logger already established")
  (setf *my-log* (make-instance 'memory-appender :max-elements 40))
  (log4cl::add-appender-internal
   (log4cl::get-logger-internal '() nil nil)
   *my-log* nil))

(defun show-log (&key (level :info) (classes t) (pattern "."))
  (show *my-log* :level level :classes classes :pattern pattern))

#+nil
(defun outline-all-loggers ()
  (let ((cnt 0))
    (labels ((id (logger)
               (if (eq logger *root-logger*)
                   "+<ROOT>"
                   (logger-category logger)))
             (recure (logger d)
               (incf cnt)
               (format t "~&~VT~S" d (id logger))
               (let ((kids (slot-value logger 'child-hash)))
                 (when kids
                   (loop 
                      for c being each hash-value of kids
                      do (recure c (+ 2 d)))))))
      (recure *root-logger* 0)
      cnt)))
