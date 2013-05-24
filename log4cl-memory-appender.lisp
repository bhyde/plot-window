(in-package #:plot-window)

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
