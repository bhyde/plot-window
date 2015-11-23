(in-package #:plot-window)

;;; Modules are named objects that contian a number of named code
;;; fragments (functions, variables, arbitrary code) in parenscript.
;;; At runtime modules reside in a javascript object, that object
;;; is stored someplace; so each module also has a path that outlines
;;; where it is stored.

;;; Man of the names used for fragments are one-to-one with the
;;; properties of the module object.  A parenscript macro, with-module,
;;; provides a convience wrapper which will establish a symbol-macrolet
;;; with one symbol-macro for each of these.  Each code fragment is
;;; implicitly wrapped in a with-module for the module in question.


(defclass javascript-named-thing ()
  ((name :type (and symbol (not null) (not keyword))
         :initarg :name 
         :reader name)))

(defmethod print-object ((x javascript-named-thing) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~S" (name x))))

(defvar *javascript-modules* (make-hash-table))

(defmacro module-info (name)
  `(gethash ,name *javascript-modules*))

(defun show-javascript-modules () 
  (loop 
     for m being each hash-value in *javascript-modules*
     do (show-javascript-module m)))

(defun clear-javascript-modules ()
  (clrhash *javascript-modules*))

(defun clear-javascript-module (name-or-module)
  (let ((name (if (symbolp name-or-module)
                  name-or-module
                  (name name-or-module))))
    (remhash name *javascript-modules*)))

(defclass javascript-module (javascript-named-thing)
  ((path :type list :initarg path)
   (requires :type list :initform nil :initarg :requires :reader requires)
   (loaded-p :type t :initarg :loaded-p :reader loaded-p
             :documentation "A parenscript expression which is true if the module has been loaded.")))

(defmethod initialize-instance :after ((x javascript-module) &key &allow-other-keys)
  (setf (module-info (name x)) x))

(defclass javascript-library (javascript-module)
  ((url :type string :initarg :url :reader url)))

(defmethod show-javascript-module ((m javascript-library))
  (with-slots (name requires loaded-p) m
    (format t "~&~s ~s ~a ~s" name requires (url m) loaded-p)))

(defmacro declare-javascript-library (name (&rest requires) &key url loaded-p)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (make-instance 'javascript-library
                    :name ',name
                    :requires ',requires
                    :url ',url
                    :loaded-p ',loaded-p)
     ',name))

(defclass javascript-code-module (javascript-module)
  ((where :type lisp :initform '(@ window) :initarg :where :reader where)
   ;(revision :type fixnum :initform 0 :reader generation)
   ;(generation-last-written :type fixnum :initform 0 :reader generation-last-written)
   (code-fragments :type hash-table ;; name -> javascript-code-fragment
                   :initform (make-hash-table)
                   :reader code-fragments)))

(defmethod show-javascript-module ((m javascript-code-module))
  (with-slots (name requires code-fragments) m
    (format t "~&~S ~s ~d fragments" 
            name requires (hash-table-count code-fragments))))

(defmacro define-javascript-code-module (name () &key where requires loaded-p)
  (assert (symbolp name))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (module-info ',name)
           (make-instance 'javascript-code-module
                          :name ',name
                          ,@(when where `(:where ',where))
                          :loaded-p ',loaded-p
                          :requires ',requires))
     ',name))

(defvar *sequence-number-generator* most-negative-fixnum)

(defclass javascript-code-fragment (javascript-named-thing)
  ((module :type javascript-code-module :initarg :module :reader module)
   (sequence-number :reader sequence-number)
   (parenscript :type t :initarg :parenscript :reader parenscript)))

(defun java-code-fragment-info (module-name fragment-name)
  (gethash fragment-name (code-fragments (module-info module-name))))

(defmethod print-object ((x javascript-code-fragment) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~S:~S" (name (module x)) (name x))))

(defmethod initialize-instance :after ((x javascript-code-fragment) &key &allow-other-keys)
  (with-slots (name module sequence-number) x
    (with-slots (code-fragments) module
      (let ((prior-instance? (gethash name code-fragments)))
        (setf sequence-number (if prior-instance?
                                  (sequence-number prior-instance?)
                                  (incf *sequence-number-generator*))))
    (setf (gethash name (code-fragments module)) x))))

(defclass javascript-function (javascript-code-fragment)
  ((args :initarg :args :reader args)))

(defmacro defun-javascript ((module-name function-name) (&rest args) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (make-instance 'javascript-function
                    :name ',function-name
                    :module (module-info ',module-name)
                    :args ',args
                    :parenscript ',body)
     ',function-name))

(defclass javascript-variable (javascript-code-fragment)
  ())

(defmacro defvar-javascript ((module-name variable-name) inital-value)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (make-instance 'javascript-variable
                    :name ',variable-name
                    :module (module-info ',module-name)
                    :parenscript ',inital-value)
     ',variable-name))

#+nil ;; replaced by defun-javascript
(defmacro defunjs ((module-name function-name) (&rest args) &body body)
  "Define a javascript function, in parenscript, inside of the given module."
  `(store-a-bit-of-javascript-module
    ',module-name 'main-forms ',function-name 
    '(flet ((,function-name (,@args) ,@body))
       ,function-name)))

#+nil
(defmacro define-javascript ((module-name init-name) () &body body)
  `(store-a-bit-of-javascript-module ',module-name 'init-forms ',init-name
                             '(progn ,@body)))

(defmethod parenscript-of ((m javascript-code-module))
  (with-slots (where name code-fragments) m
    (loop
       with variables = ()
       with functions = ()
       with exports = ()
       with var-inits = ()
       with initializer? = nil
       finally (return `(setf (@ ,where ,name)
                              (with-this ((create ,@(nreverse variables)))
                                (let ((,name this))
                                  (flet ,(nreverse functions)
                                    ,@(nreverse exports)
                                    ,@(nreverse var-inits)
                                    ,@initializer?))
                                this)))
       for code-fragment in (sort (alexandria:hash-table-values code-fragments) #'< :key #'sequence-number)
       do (typecase code-fragment
            (javascript-variable
             (with-slots ((var-name name) (var-init parenscript)) code-fragment
               (push var-name variables)
               (push 'null variables)
               (push `(setf (@ this ,var-name) ,var-init) var-inits)))
            (javascript-function
             (with-slots ((func-name name) (func-args args) (func-body parenscript)) code-fragment
               (cond
                 ((eq 'initialize func-name)
                  (setf initializer? func-body))
                 (t
                  (push `(,func-name ,func-args ,@func-body) functions)
                  (push `(setf (@ this ,func-name) ,func-name) exports)))))))))
               
;;;;

(defparameter *where-to-store-js-module-files* (namestring (asdf:system-relative-pathname 'plot-window "static/")))

(defgeneric update-js-module-file-if-necessary (m)
  (:documentation "Compile time hook which provides an oportunity to regenerate the javascript file."))

(defmethod update-js-module-file-if-necessary ((m javascript-library))
  (declare (ignore m)))
 
(defmethod update-js-module-file-if-necessary ((m javascript-code-module))
  ;; the if-necessary is tbd
  (with-open-file (*parenscript-stream* (pathname-of-javascript-code-module m)
                                        :direction :output
                                        :if-exists :rename-and-delete)
    (format *parenscript-stream* "// Do not edit, generated from parenscript.~%")
    ;; binding gensym counter here is ok because the whole module is inside
    ;; it's own lexical scope.  It also makes the repeat compiles are more similar
    (let ((*ps-gensym-counter* 1))
      (ps* (parenscript-of m)))
    (format *parenscript-stream* "~&// Do not edit, generated from parenscript.~%"))
  t)

(defmethod pathname-of-javascript-code-module ((m javascript-code-module))
    (make-pathname :defaults *where-to-store-js-module-files*
                   :type "js"
                   :name (string-downcase (name m))))

(defmethod url ((m javascript-code-module))
  (format nil "/~a.js" (string-downcase (name m))))







;;;; Generating script tags, i.e. children of page's head.

(defvar *libraries-loaded-on-current-page*)

(defmethod html-elements-of ((m javascript-module) (s stream))
  (with-slots (name requires) m
    (unless (member name *libraries-loaded-on-current-page*)
      (push name *libraries-loaded-on-current-page*)
      (flet ((nl () (format s "~%")))
        (loop
           finally (nl) (html-element-of m s)
           for required-module-name in requires
           do (html-elements-of
               (module-info required-module-name) s))))))

(defmethod html-element-of ((m javascript-library) (s stream))
  (format s "<script type=\"text/javascript\" src=~S></script>" (url m)))

(defmethod html-element-of ((m javascript-code-module) (s stream))
  (update-js-module-file-if-necessary m)
  (format s "<script type=\"text/javascript\" src=~S></script>" (url m)))

(defun emit-script-tags-for-javascript-libraries (stream libraries)
  "Emit script tags to stream, including those for libraries requred by
the libraries given.  Code libraries this will recompile the library from
the parenscript is necessary.  This output is typcially inserted into the
head element.  It is an error to call this more than once per page."
  (loop
     with *libraries-loaded-on-current-page* = nil
     for lib in libraries
     do (html-elements-of (module-info lib) stream)))




;;;; Dynamically loading libraries.


(defun collect-required-libaries (libraries)
  (let ((required nil))
    (labels ((recure (library)
               (let ((info (or (module-info library)
                               (error "Unknown library ~S" library))))
                 (unless (member info required)
                   (mapcar #'recure (requires info))
                   (push info required)))))
      (map nil #'recure libraries)
      required)))







;;;; Parenscript code to dynamically load java script libraries.

(defpsmacro with-javascript-modules ((&rest modules-needed) &body body)
  (let ((all-modules ()))
    (labels ((collect-modules (module-name)
               (let ((info (or (module-info module-name)
                               (error "module ~S is unknown" module-name) )))
                 (map nil #'collect-modules (requires info))
                 (pushnew info all-modules))))
      (map nil #'collect-modules modules-needed)
      (loop for m in all-modules 
         do (update-js-module-file-if-necessary m))
      `(cprogn ((lambda () ,@body))
               ,@(loop for info in (nreverse all-modules)
                    collect `(if ,(loaded-p info)
                                 (next)
                                 (chain $ (get-script ,(url info) next))))))))



