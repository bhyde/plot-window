(in-package #:plot-window)

;;;; Our Javascript code module DW

(declare-javascript-library jquery () 
  :url "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
  :loaded-p (boundp j-query))

(declare-javascript-library jquery-json (jquery)
  :url  "/jquery.json-2.4.js"
  :loaded-p (boundp (@ j-query to-j-s-o-n)))

(declare-javascript-library graceful-web-socket (jquery)
  :url "/jquery.gracefulWebSocket.js"
  :loaded-p (boundp (@ j-query graceful-web-socket)))

(define-javascript-code-module dw ()
  :requires (graceful-web-socket jquery-json))

(defun-javascript (dw lg) (msg)
  (send-ws-message (create :type "page-log" :message msg))
  (chain console (log msg)))

(defun-javascript (dw make-element-inserter) (how location)
  (create 'how how 'location location))

(defvar-javascript (dw *inserter*) (make-element-inserter :prepend "body"))

(defun-javascript (dw insert-element) (new-element continuation &optional (element-inserter *inserter*))
  (let* ((loc (@ element-inserter location)))
    (unless (= 1 (length loc))
      (throw
          (new (-error 
                (interpolate
                 "{Selector \"$[place]\" found $[(length loc)] elements, must result in exactly one.}")))))
    (case (@ element-inserter how)
      (:before
       (chain new-element (hide))
       (chain loc (before new-element))
       (chain new-element (slide-down 1000 continuation)))
      (:after
       (chain new-element (hide))
       (chain loc (after new-element))
       (chain new-element (slide-down 1000 continuation)))
      (:append
       (chain new-element (hide) (fade-in 1000 continuation))
       (chain loc (append new-element)))
      (:prepend
       (chain new-element (hide))
       (chain loc (prepend new-element))
       (chain new-element (slide-down 1000 continuation)))
      (:replace-content
       (cprogn (continuation)
               (progn
                 (chain ($ (chain loc (children))) (wrap-all "<div/>"))
                 (chain loc (children) (fade-out 400 next)))
               (progn 
                 (chain loc (empty))
                 (chain new-element (hide) (fade-in 400 next))
                 (chain loc (prepend new-element))))))))

(defvar-javascript (dw ws) 1)

(defun-javascript (dw on-message) (e)
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

(defun-javascript (dw initialize) ()

;;; establish dw.ws
  (let  ((x (chain $ (graceful-web-socket (websocket-url)))))
    (flet ((ws-error (e)
             (chain console (log "ws hd an error.")))
           (ws-open (e)
             ;; This next line sets *last-websocket-client* over
             ;; on the lisp side.  This should be done in a more
             ;; fastidious manner.
             (lg "websocket has opened")))
      (setf (@ dw ws) x)
      (setf (@ x onerror)   #'ws-error)
      (setf (@ x onopen)    #'ws-open)
      (setf (@ x onmessage) #'on-message)))

  (chain console (log "dw module has initialized")))

(defun-javascript (dw send-ws-message) (data)
  (cond
    ((and (boundp (@ this dw))
          (boundp (@ dw ws))
          (eql (typeof (@ dw ws)) :object)
          (not (eql null (@ dw ws))))
     (cond
       ((eql (@ dw ws ready-state) 1)
        (chain dw ws (send (chain $ (to-J-S-O-N data)))))
       (t
        (chain console (log "dw.ws is not open")))))
    (t
     (chain console (log "dw.ws is unavailable")))))


