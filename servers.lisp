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

(defclass my-easy-acceptor (easy-acceptor)
  ())

(defmethod acceptor-log-message ((acceptor my-easy-acceptor) log-level format-string &rest format-arguments)
  (let ((str (format nil "~?~%"  format-string format-arguments)))
    (ecase log-level
      (:ERROR    (log:error str))
      (:INFO     (log:info str))
      (:WARNING  (log:warn str)))))

(defmethod acceptor-log-access ((acceptor my-easy-acceptor) &key return-code)
  (log:info "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                    ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\""
            (remote-addr*)
            (header-in* :x-forwarded-for)
            (authorization)
            (hunchentoot::iso-time)
            (request-method*)
            (script-name*)
            (query-string*)
            (server-protocol*)
            return-code
            (content-length*)
            (referer)
            (user-agent)))

(defun start-web-server (&key port)
  (when *my-acceptor*
    (log:info "stopping old web server")
    (stop *my-acceptor*))
  (setf *dispatch-table*
        (list
         'dispatch-easy-handlers
         (flet ((json-rpc-handler ()
                  (let ((json-data (raw-post-data :force-text t)))
                    (json-rpc:invoke-rpc json-data))))
           (create-prefix-dispatcher "/js" #'json-rpc-handler))
         (create-folder-dispatcher-and-handler "/" (concatenate 'string cl-user::*root* "static/"))))
  (log:info "Starting web server")
  (let ((*print-case* :upcase))
    (setf *my-acceptor*
          (start
           (make-instance 'my-easy-acceptor :port port)))))




;;;; Web Sockets Server

(defvar *my-clws-server-process* nil)
(defvar *my-clws-server-process-port* 8766)

(defpsmacro websocket-url ()
  (format nil "ws://127.0.0.1:~D/jm" *my-clws-server-process-port*))

(defun start-clws-server ()
  (when (and *my-clws-server-process*
             (bordeaux-threads:thread-alive-p *my-clws-server-process*))
    (log:info  "destroying websocket server")
    (bordeaux-threads:destroy-thread *my-clws-server-process*))
  (log:info "starting websocket server")
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
  (log:debug "connect from ~s:~s" 
             (clws:client-host *current-websocket-client*)
             (clws:client-port *current-websocket-client*))
  t)

(defmethod ws:resource-client-disconnected ((r json-rcp-resource) (*current-websocket-client* ws::client))
  (declare (ignore r))
  (log:debug "disconnected from ~s:~s"
               (clws:client-host *current-websocket-client*)
               (clws:client-port *current-websocket-client*)))


(defmethod ws:resource-received-text ((res json-rcp-resource) (*current-websocket-client* ws::client) message)
  (setf *last-websocket-client* *current-websocket-client*)
  (log:debug "msg: ~s" message)
  (dispatch-json-msg message))

(defun send-json-message (message-json-alist &optional (client *current-websocket-client*))
  (assert (not (clws:client-connection-rejected client)))
  (log:debug  "sending msg ~S" message-json-alist)
  (ws:write-to-client-text client (json:encode-json-to-string message-json-alist)))

#+nil
(defmethod resource-received-binary ((res json-rcp-resource) client message)
  ...)

(let (thread)
  (defun start-and-register-json-rpc-resource ()
    (when thread
      (log:info "kill old msg listener resource")
      (bordeaux-threads:destroy-thread thread))
    (ws:register-global-resource 
     "/jm"
     (make-instance 'json-rcp-resource)
     (ws:origin-prefix "http://127.0.0.1" "http://localhost"))
      (log:info  "start msg listener resource")
    (setf thread
          (bordeaux-threads:make-thread
           #'(lambda ()
               (clws:run-resource-listener (clws:find-global-resource "/jm")))
           :name "resource listener for /jm"))))
