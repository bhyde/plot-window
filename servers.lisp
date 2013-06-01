(in-package "PLOT-WINDOW")

(defvar cl-user::*root*
  (if (string= (uiop:getenv "HOME") "/app")
      "/app/"
      (namestring
       (make-pathname :directory
                      (pathname-directory 
                       (asdf:component-relative-pathname
                        (asdf:find-system "plot-window")))))))

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
