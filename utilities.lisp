(in-package #:plot-window)

(defun build-symbol (&rest bits)
  (intern (format nil "~@:(~{~A~}~)" bits)))

(defun build-keyword (x)
  (intern (format nil "~@:(~S~)" x) :keyword))

(defmacro with-alist-bind ((&rest vars) alist &body body)
  (once-only (alist)
    `(let ,(loop
              for var in vars
              collect `(,var (cdr (assoc ,(build-keyword var) ,alist))))
       ,@body)))

;;;; Some CSS defaults

(defparameter *std-margin* "0.5em")
(defparameter *double-std-margin* "1em")

(macrolet ((define-css-var (name definition)
             `(progn
                (defvar ,name)
                (make-css-var ,name ,definition)
                ',name)))
  (define-css-var *usual-margin* '(:margin "0.5em"))
  (define-css-var *background-color* '(:background-color "#EEF7E3 !important"))
  (define-css-var *foreground-color* '(:color "#062E3B"))
  (define-css-var *border* '(:border "1px #86B295 dotted"))
  (define-css-var *default-font* '(:font-family "Helvetica, Arial, sans-serif"
                                   :font-size "+120%")))

(defun func-of-with-std-body (stream lambda)
  (with-html-output (stream)
    (:body 
     :style (css-lite:inline-css `(*background-color* 
                                   *foreground-color*
                                   (padding ,*std-margin*)
                                   *border*
                                   (margin ,*std-margin*)))
     (funcall lambda stream)
     (:img :src "/lisp-logo120x80.png"
           :style (css-lite:inline-css
                   `((width 45) (hieght 30) (position absolute)
                     (bottom ,*double-std-margin*)
                     (right ,*double-std-margin*)))))))

(defmacro with-std-body ((var-of-stream) &body body)
  `(func-of-with-std-body ,var-of-stream
                          #'(lambda (,var-of-stream)
                              (with-html-output (,var-of-stream ,var-of-stream 
                                                                :indent *indent-default*)
                                ,@body))))

(defvar *indent-default* t)

(defvar *header-fragments*)

(defvar *page-plist*)

(defmacro page-property (name)
  `(getf *page-plist* ,name))
  

(defmacro with-header-fragment ((s) &body body)
  `(progn
     (push
      (with-html-output-to-string  (,s nil :indent *indent-default*) ,@body)
      *header-fragments*)
     (values)))

(defmacro with-my-page ((stream &key title) &body body)
  `(let* ((*header-fragments* ())
          (*page-plist* ())
          (*read-default-float-format* 'single-float)
          (body (with-html-output-to-string (,stream nil :indent *indent-default*)
                  (with-std-body (,stream) ,@body))))
     (with-header-fragment (s)
       (:title ,title))
     (with-html-output-to-string (,stream nil :indent *indent-default*)
       (:html
        (:head (loop for str in (nreverse *header-fragments*) do (htm (str str))))
        (str body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *javascript-library-info* (make-hash-table))

  (defun info-of-javascript-library (javascript-library)
    (or (gethash javascript-library *javascript-library-info*)
        (error "Unknown Javascript library ~A" javascript-library)))

  (defmacro define-javascript-library (name (&rest preconditions) url)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *javascript-library-info*) '(:url ,url :preconditions ,preconditions)))))

(define-javascript-library jquery () "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js")

(defun add-javascript-libraries (&rest java-libraries)
  (loop
     for java-library in java-libraries
     as library-info = (info-of-javascript-library java-library)
     do
       (apply #'add-javascript-libraries (getf library-info :preconditions))
       (unless (page-property java-library)
         (setf (page-property java-library) t)
         (with-header-fragment (stream)
           (:script :type "text/javascript"
                    :src (getf library-info :url))))))

(defmacro with-script-in-header ((stream) &body parenscript-body)
  `(with-header-fragment (,stream)
     (:script :type "text/javascript"
              (str (ps ,@parenscript-body)))))

(defmacro ps-onready ((stream) &body parenscript)
  "A cl-who macro that arranges for it's body to happen at startup"
  `(progn
     (add-javascript-libraries 'jquery)
     (with-script-in-header (,stream)
       ($ (lambda () ,@parenscript)))))

(defpsmacro lg (obj)
  `(chain console (log ,obj)))

(defpsmacro with-delay ((milliseconds) &body body)
  `(set-timeout (lambda () ,@body) ,milliseconds))

(defun get-needed-files (libraries)
  (let (needed-libraries)
    (labels ((collect-needed-libraries (libraries)
               (loop 
                  for library in libraries
                  do (destructuring-bind (&key url preconditions)
                         (info-of-javascript-library library)
                       (pushnew (list library url)
                                needed-libraries :key #'first)
                       (collect-needed-libraries preconditions)))))
      (collect-needed-libraries libraries))
    needed-libraries))

(defpsmacro with-js-libraries ((&rest libraries) &body body)
  (let ((needed-libraries (get-needed-files libraries)))
    `(labels 
         ((get-libraries-and-do-it ()
            (,(build-symbol "get-" (caar needed-libraries))))
          ,@(nreverse
             (loop
                finally (print library-fetcher)
                for next = 'do-it then library-fetcher
                for (name url) in (nreverse needed-libraries)
                as library-fetcher = (build-symbol "get-" name)
                collect `(,library-fetcher ()
                                           (lg ,url)
                                           (chain $ (get-script ,url ,next)))))
          (do-it () ,@body))
       (get-libraries-and-do-it))))

(defpsmacro def-jquery-plugin (name (&rest args) &body body)
  ;; see http://docs.jquery.com/Plugins/Authoring#Getting_Started
  `(flet ((setup-function ($)
            (flet ((,name (,@args) 
                     ;; Lucky Mr. Body will find "this" bound to
                     ;; the jquery object he want's to chew on.
                     ,@body))
              (setf (@ $ fn ,name) #',name))))
     (setup-function j-query)))

(defpsmacro with-each-of-jquery-object ((index element jquery-object) &body body)
  `(chain ,jquery-object (each (lambda (,index ,element) ,@body))))
