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

(defparameter *random-texts*
  #("apple" "carrot" "tomato" "dog" "fish" "piano" "trombone" "fire" "water"))

(defun random-elt (seq)
  (elt seq (random (length seq))))

(defpsmacro random-paragraph ()
  (let ((text (random-elt *random-texts*)))
    (interpolate "{<p>$(text)</p>}")))

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




