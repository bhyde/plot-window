(in-package #:plot-window)




;;;; A few standard Lisp functions.

(defpsmacro boundp (sym)
  `(not (string= (typeof ,sym) "undefined")))

(defpsmacro plusp (x)
  `(< 0 ,x))

(defpsmacro char (str x) `(aref ,str ,x))

(defpsmacro char-downcase (x) `(chain ,x (to-lower-case)))

(defpsmacro char-upcase (x) `(chain ,x (to-upper-case)))

(defpsmacro string-upcase (x)
  `(chain ,x (to-upper-case)))

(defpsmacro string-downcase (x)
  `(chain ,x (to-lower-case)))




;;;; CProgn 

(defpsmacro cprogn ((finally) &body stms)
  "Run a cascade of continuations.  Each of the statements is converted
into a function.  Each statement should include one or occurances of the
token next.  That will be replaced with the function created for the
following statement.  Each statement should invoke next when it completes.
and final form will run the finally form."
  (flet ((build-symbol (&rest bits)
           (intern (format nil "~@:(~{~A~}~)" bits)))
         (replace-continuation (continuation stm)
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
    (let* ((base (ps-gensym "_c"))
           (frst (build-symbol base "-0"))
           (lst (if (symbolp finally) 
                    finally
                    (build-symbol base "-lst"))))
      `(labels (,@(unless (symbolp finally) `((,lst () ,@(cddr finally))))
                ,@(loop
                     as nm = frst then next-nm
                     for i from 1
                     as (stm . more) on stms
                     as next-nm = (if more (build-symbol base i) lst)
                     collect `(,nm () ,(replace-continuation next-nm stm) undefined)))
         (,frst)))))

;;;; Misc.

(defpsmacro with-this ((this-form) &body body)
  "Execute the body with `this' bound to the value of the form given."
  `(chain
    (lambda () ,@body)
    (call ,this-form)))

(defpsmacro with-delay ((milliseconds) &body body)
  "While you may prefer to do this with cprogn this form arranges to run the
body after the given number of milliseconds."
  `(set-timeout (lambda () ,@body) ,milliseconds))


;;;; minimal cl-interp for parenscript

(defun interp% (str)
  "Avoid the need to use a unique read table."
  (assert (stringp str))
  (with-input-from-string (s str)
    (cl-interpol:interpol-reader s #\? nil :recursive-p nil)))

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




;;;; JQuery sugar...

(defmacro ps-onready ((stream) &body parenscript)
  "A cl-who macro that arranges for it's body to happen at startup"
  `(progn ;; ought to assert/assure jquery's loaded.
     (with-script-in-header (,stream)
       ($ (lambda () ,@parenscript)))))

(defpsmacro with-each-of-jquery-object ((index element jquery-object) &body body)
  `(chain ,jquery-object (each (lambda (,index ,element) ,@body))))

(defpsmacro def-jquery-plugin (name (&rest args) &body body)
  ;; see http://docs.jquery.com/Plugins/Authoring#Getting_Started
  `(flet ((setup-function ($)
            (flet ((,name (,@args) 
                     ;; Lucky Mr. Body will find "this" bound to
                     ;; the jquery object he want's to chew on.
                     ,@body))
              (setf (@ $ fn ,name) #',name))))
     (setup-function j-query)))


