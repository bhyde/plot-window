(in-package "PLOT-WINDOW")

(defvar cl-user::*root*
  (if (string= (ccl:getenv "HOME") "/app")
      "/app/"
      (let ((dirs (pathname-directory 
                   (asdf:COMPONENT-RELATIVE-PATHNAME (asdf:find-system "plot-window")))))
        (setf (cdr dirs) (nreverse (cdr (reverse (cdr dirs)))))
        (namestring
         (make-pathname :directory dirs)))))

;;;; Web Server

(defvar *my-acceptor* nil)

(defun start-web-server (&key port)
    (setf *dispatch-table*
     (list
      'dispatch-easy-handlers
      (flet ((json-rpc-handler ()
               (let ((json-data (raw-post-data :force-text t)))
                 (json-rpc:invoke-rpc json-data))))
        (create-prefix-dispatcher "/js" #'json-rpc-handler))
      (create-folder-dispatcher-and-handler "/" (concatenate 'string cl-user::*root* "static/"))))
  (when *my-acceptor*
    (stop *my-acceptor*))
  (let ((*print-case* :upcase))
    (setf *my-acceptor*
          (start
           (make-instance 'easy-acceptor :port port)))))

;;;; Web Sockets Server

(defun lg-message* (level format-string &rest args)
  (let ((*acceptor* *my-acceptor*))
    (apply #'log-message* level format-string args)))

(defvar *my-clws-server-process* nil)
(defvar *my-clws-server-process-port* 8766)

(defpsmacro websocket-url ()
  (format nil "ws://127.0.0.1:~D/jm" *my-clws-server-process-port*))

(defun start-clws-server ()
  (when (and *my-clws-server-process*
             (bordeaux-threads:thread-alive-p *my-clws-server-process*))
    (lg-message* 2 "destroying websocket server")
    (bordeaux-threads:destroy-thread *my-clws-server-process*))
  (lg-message* 2 "starting websocket server")
  (setf *my-clws-server-process*
        (bordeaux-threads:make-thread 
         #'(lambda () (clws:run-server *my-clws-server-process-port*))
         :name "websockets server")))

;;;; JSON messaging over that web socket.

(defvar *current-websocket-client*)
(defvar *last-websocket-client*)
(defvar *json-rcp-handlers* (make-hash-table :test #'equalp))

(defmacro define-json-message-handler (name (&rest parameters) &body body)
  "Define a function that will be called with the alist that decode-jason creates."
  `(progn 
     (defun ,name (json-alist)
       (with-alist-bind ,parameters json-alist
         ,@body))
     (setf (gethash ',(symbol-name name) *json-rcp-handlers*) ',name)))

(defun dispatch-json-msg (json-message)
  (let* ((json-alist (json:decode-json-from-string json-message))
         (type (cdr (assoc :type json-alist)))
         (func (gethash type *json-rcp-handlers*)))
    (cond
      (func (funcall func json-alist))
      (t (error "Unknown type of websocket message ~S" type)))))

(defclass json-rcp-resource (clws:ws-resource)
  ())

(defmethod ws:resource-client-connected ((r json-rcp-resource) (*current-websocket-client* ws::client))
  (lg-message* "Websocket/jm connect from ~s:~s~" 
               (clws:client-host *current-websocket-client*)
               (clws:client-port *current-websocket-client*))
  t)

(defmethod ws:resource-client-disconnected ((r json-rcp-resource) (*current-websocket-client* ws::client))
  (declare (ignore r))
  (lg-message* "Websocket/jm disconnected from ~s:~s~"
               (clws:client-host *current-websocket-client*)
               (clws:client-port *current-websocket-client*)))


(defmethod ws:resource-received-text ((res json-rcp-resource) (*current-websocket-client* ws::client) message)
  (setf *last-websocket-client* *current-websocket-client*)
  (lg-message* "websocket/jm msg: ~s" message)
  (dispatch-json-msg message))

(defun send-json-message (message-json-alist &optional (client *current-websocket-client*))
  (ws:write-to-client-text client (json:encode-json-to-string message-json-alist)))

#+nil
(defmethod resource-received-binary ((res json-rcp-resource) client message)
  ...)

(defun start-and-register-json-rpc-resource ()
  (ws:register-global-resource 
   "/jm"
   (make-instance 'json-rcp-resource)
   (ws:origin-prefix "http://127.0.0.1" "http://localhost"))
  (bordeaux-threads:make-thread
   #'(lambda ()
     (clws:run-resource-listener (clws:find-global-resource "/jm")))
   :name "resource listener for /jm"))

;;;; Incomming message...

(define-json-message-handler page-log (message)
  (lg-message* 2 "WS/~S: ~S" *current-websocket-client* message))

;;;; The main page, i.e. the plot window

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


;;;; Now a routine to plot a set of points into our display.

(defun plot (&optional (data-points '((1 1) (2 2) (1 4))))
  (send-json-message
   `((:target . "#ex1")
     (:event . "revisePlot")
     (:series . (,data-points))
     (:details . ((:points . ((:show . t) (:radius . 2) (:fill-color . 3)))
                  (:lines . ((:show . t) (:line-width . 1) (:fill-color . 4))))))
   *last-websocket-client*)
  nil)

;;;; Startup...

(defun cl-user::initialize-application (&key (port 8765))
  (start-web-server :port port)
  (start-clws-server)
  (start-and-register-json-rpc-resource))
