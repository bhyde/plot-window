(in-package #:plot-window)

;;;; Starting our Servers

(defun cl-user::initialize-application (&key (port 8765))
  (start-web-server :port port)
  (start-clws-server)
  (start-and-register-json-rpc-resource))

;;;; Our one and only web page.

(define-easy-handler (dw-window :uri "/") ()
  (with-my-page (s :title "Display Window")
    (with-header-fragment (s)
      (emit-script-tags-for-javascript-libraries s '(dw)))
    (:div :id "ex1"
          :style (css-lite:inline-css `((width "600px") (height "350px")))
          "Sir!  Your humble window awakes your command.")))

;;;; Inbound messages

(define-json-message-handler page-log (message)
  (log:info '(websocket page-log) "~S" message))


;;;; Outbound messages

(defvar *last-global-eval* nil
  "For debugging, this retains the last javascript source send to the a browser.")

(defun send-global-eval (javascript-text &optional (client *last-websocket-client*))
  "Ask the websocket client to run the javascript source code given
For example: (send-global-eval \"alert('hi')\")
Note that this sets *last-global-eval*."
  (log:debug '(websocket send global-eval) "~S" javascript-text)
  (setf *last-global-eval* javascript-text)
  (send-json-message 
   `((:event . ,(symbol-to-js-string 'global-eval))
     (:argument . ,javascript-text))
   client))

(defmacro ps-eval-in-client* (&rest parenscript-forms)
  "Compiles the given parenscript into Javascript and sends it to *last-websocket-client*.
For example: (ps-eval-in-client* '(alert \"hi\"))"
  `(send-global-eval (ps* ,@parenscript-forms)))

(defparameter *catch-eval-in-client-errors* nil)

(defmacro ps-eval-in-client (&body parenscript-forms)
  (let ((body `(progn ,@parenscript-forms)))
    (when *catch-eval-in-client-errors*
      (setf body `(try ,body
                       (:catch (e)
                         ((@ dw lg)
                          (interpolate "[Error: ${(@ e message)}]"))
                         e))))
    `(send-global-eval
      (ps (setf (@ dw last-result) ,body)))))

