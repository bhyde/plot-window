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

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *javascript-library-info* (make-hash-table))

  (defun info-of-javascript-library (javascript-library)
    (or (gethash javascript-library *javascript-library-info*)
        (error "Unknown Javascript library ~A" javascript-library)))

  (defmacro define-javascript-library (name (&rest preconditions) url present-p)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *javascript-library-info*) '(:url ,url :preconditions ,preconditions :present-p ,present-p)))))

(define-javascript-library jquery () 
  "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
  (boundp j-query))


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

(defpsmacro boundp (sym)
  `(not (string= (typeof ,sym) "undefined")))

(defpsmacro lg (obj)
  `(chain console (log ,obj)))

(defpsmacro with-delay ((milliseconds) &body body)
  `(set-timeout (lambda () ,@body) ,milliseconds))

(defun get-needed-files (libraries)
  (let (needed-libraries)
    (labels ((collect-needed-libraries (libraries)
               (loop 
                  for library in libraries
                  do (destructuring-bind (&key url preconditions present-p)
                         (info-of-javascript-library library)
                       (pushnew (list library url present-p)
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
                ; finally (print library-fetcher)
                for next = 'do-it then library-fetcher
                for (name url present-p) in (nreverse needed-libraries)
                as library-fetcher = (build-symbol "get-" name)
                collect `(,library-fetcher ()
                                           (cond
                                             (,present-p (funcall ,next))
                                             (t
                                              (lg ,url)
                                              (chain $ (get-script ,url ,next)))))))
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

(defpsmacro defun-bah (name args &body body)
  `(flet ((,name ,args ,@body))
     (setf (@ window bah ,name) ,name)))

(defpsmacro funcall-bah (name &rest args)
  `(funcall (@ window bah ,name) ,@args))

(defun interp% (str)
  "Avoid the need to use a unique read table."
  (assert (stringp str))
  (with-input-from-string (s str)
    (cl-interpol::interpol-reader s #\? nil)))

(defmacro interpolate (str)
  (interp% str))

(defpsmacro interpolate (str)
  (interp% str))

(defpsmacro with-output-to-string ((stream) &body body)
  "Converts some of what cl-interpol generates into javascript."
  (flet ((transform (form)
           (optima:ematch form
             ((optima:guard (list 'write-string x s) (eq s stream))
              `,x)
             ((optima:guard (list 'princ x s) (eq s stream))
              `(-String ,x)))))
    `(chain (array ,@(mapcar #'transform body))
            (join ""))))

(defpsmacro string-upcase (x)
  `(chain ,x (to-upper-case)))

(defpsmacro string-downcase (x)
  `(chain ,x (to-lower-case)))

(defpsmacro plusp (x)
  `(< 0 ,x))

(defpsmacro char-downcase (x) `(chain ,x (to-lower-case)))

(defpsmacro char-upcase (x) `(chain ,x (to-upper-case)))

(defpsmacro char (str x) `(aref ,str ,x))

(defpsmacro cprogn ((finally) &body stms)
  (flet ((replace-continuation (continuation stm)
           (let ((got-one nil))
             (labels ((recure (form)
                        (cond
                          ((eq 'next form) 
                           (setf got-one t)
                           continuation)
                          ((listp form)
                           (mapcar #'recure form))
                          (t form))))
               (prog1
                   (recure stm)
                 (unless got-one (error "Did not find NEXT in ~S" stm)))))))
    (let* ((base (gensym))
           (frst (build-symbol base "-0")))
      `(labels (,@(loop
                     as nm = frst then next-nm
                     for i from 1
                     as (stm . more) on stms
                     as next-nm = (if more (build-symbol base i) finally)
                     collect `(,nm () ,(replace-continuation next-nm stm) undefined)))
         (,frst)))))

(defpsmacro new-element ((how where &optional continuation) &body element-maker)
  `(flet ((continuation () ,(if continuation `(funcall ,continuation) nil)))
     (let* ((new-element ($ (progn ,@element-maker)))
            (place ,where)
            (loc ($ place)))
       (unless (= 1 (length loc))
         (throw
             (new (-error 
                   (interpolate
                    "{Selector \"$[place]\" found $[(length loc)] elements, must result in exactly one.}")))))
       (case ,how
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
                    (chain loc (prepend new-element)))))))))

(defun test-new-element ()
  (ps-eval-in-client
    (cprogn ((lambda ()))
      (new-element (:replace-content :body next) "<p id='xx'>Yeah</p>")
      (new-element (:append :body next)          "<p>Append 1</p>")
      (new-element (:append :body next)          "<p>Append 2</p>")
      (new-element (:prepend :body next)         "<p>Prepend 1</p>")
      (new-element (:prepend :body next)         "<p>Prepend 2</p>")
      (new-element (:before "#xx" next)          "<p id='z'>before 1</p>")
      (new-element (:after "#xx" next)           "<p>after 1</p>")
      (new-element (:before "#xx" next)          "<p>before 2</p>")
      (new-element (:after "#xx" next)           "<p>after 2</p>")
      )))
