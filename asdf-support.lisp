;; GOALS
;; Is to provide an easy upgrade path for users familiar with asdf.
;; It is NOT to provide full asdf support.
;; It is only to provide idiomatic asdf support.


;; This means that specializing any methods, other than perform, on your system will not have the effect you desire.
;; we will interrogate .asd files for systems (and only once at startup as this is for support only).
;; we will provide messages (and in a (asdf-failures) function) about what asdf systems where not defined.

;; Remember: This is only a partial asdf support. Think of it as more support for ASDF syntax.

;; TODO

(defpackage :sysdef.asdf (:use :cl :sysdef :alexandria)
  (:export #:defsystem #:defsystem-connection #:cl-source-file #:static-file #:find-system
   #:enable-asdf-support #:disable-asdf-support #:register-asdf-systems #:*signal-error* #:*central-registry*
   #:operate #:perform #:oos #:operate-on-system #:system #:module #:component
   
   ;;class aliases
   #:operation #:load-op #:load-source-op #:compile-op #:test-op
   )
  (:import-from #+lispworks :clos #+sbcl :sb-mop #+clisp :clos #+cmu :pcl #+allegro :mop #+(or ccl openmcl) :ccl

   #:generic-function-method-class #:funcallable-standard-class
   #:method-qualifiers #:method-specializers #:generic-function-lambda-list #:method-function)
   
  (:shadow #:perform)
  (:documentation "The sysdef.asdf package offers a tiny piece of ASDF compatibility in order to load
systems defined using ASDF syntax. No attempt has been made to fully support specialization on ASDF methods
and these sorts of custom systems will not work in this support package.
The goal here is to support the most basic of ASDF system definitions.
   
It is worth noting that specialization of perform methods IS supported however calling call-next-method
in these specialized methods is undefined."))


(in-package :sysdef.asdf)


;;; CONDITIONS
(define-condition asdf-definition-failed (warning)
  ((cause :accessor cause-of :initarg :cause))
  (:report (lambda (c s)
             (print-unreadable-object (c s :type t :identity t)
               (princ (cause-of c) s)))))


;; SPECIAL VARIABLES
(defvar *central-registry* ())

(defparameter *keywords-mappings*
  '((:description . (simple-converted-mapping :documentation))
    (:name . ignored-mapping)
    (:class . silent-mapping)
    (:author . identity-mapping)
    (:maintainer . identity-mapping)
    (:licence . identity-mapping)
    (:license . identity-mapping)
    (:serial . identity-mapping)
    (:pathname  . pathname-mapping) 
    (:default-component-class . identity-mapping)
    (:long-description . (simple-converted-mapping :documentation))
    (:depends-on . depends-on-mapping)
    (:version . version-mapping)
    (:components . convert-components)
    (:in-order-to . convert-in-order-to)))



;;; ENABLING AND DISABLING SYSDEF.ASDF
(defun add-package-nickname (package nickname)
  (rename-package package package (adjoin nickname (package-nicknames package)
                                          :test #'string-equal)))

(defun remove-package-nickname (package nickname)
  (rename-package package package (remove nickname (package-nicknames package)
                                          :test #'string-equal)))

(defun enabledp ()
  (and (find-package :asdf)
       (eql (find-package :asdf)
            (find-package :sysdef.asdf))))

(defun enable-asdf-support ()
  "Enables ASDF support for mudballs. This adds the package nickname :ASDF to the sysdef.asdf package. If
ASDF is already loaded then the ASDF package is renamed to asdf.original. This function will register-asdf-systems."
  (unless (enabledp)
    (backup-asdf-package)
    (add-package-nickname :sysdef.asdf :asdf)
    (register-asdf-systems)))

(defun disable-asdf-support ()
  (when (enabledp)
    (remove-package-nickname :sysdef.asdf :asdf)
    (restore-asdf-package)))

(defun backup-asdf-package ()
  (when (and (find-package :asdf)
             (not (eql (find-package :asdf)
                       (find-package :sysdef.asdf))))
    (rename-package :asdf :asdf.original)))

(defun restore-asdf-package ()
  (when (find-package :asdf.original)
    (rename-package :asdf.original :asdf)))



;;; CLASS ALIASES
(defun alias-class (name alias)
  (setf (find-class alias) (find-class name)))

(alias-class 'lisp-source-file 'cl-source-file)
(alias-class 'action 'operation)
(alias-class 'load-action 'load-op)
(alias-class 'load-source-action 'load-source-op)
(alias-class 'compile-action 'compile-op)

;; Required classes
(defclass test-op (operation) ())



;;; DEFSYSTEM SUPPORT
(defmacro defsystem (name &body options)
  "Defines a system using ASDF syntax. This is converted into appropriate an mudballs system definition."
  `(handler-bind ((error 'asdf-definition-failed))
     (define-system ,name (,@(superclass options))
       ,@(convert-options options))))

(defvar *signal-error* t "When bound to NIL errors signalled during the creation of asdf systems will
not signal an error but will print a warning instead.")

(defun asdf-definition-failed (error)
  (unless *signal-error*
    (when-let (restart (find-restart 'skip error))
      (warn 'asdf-definition-failed :cause error)
      (invoke-restart restart))))

(defun superclass (options)
  (let ((class (cdr (member :class options))))
    (if class
        (list (first class))
        nil)))

(defun convert-options (options)
  (loop :for (key value . -) :on options by #'cddr
        :when (map-value key value)
        :collect :it))

(defun map-value (key value)
  (let ((mapper (or (cdr (assoc key *keywords-mappings*))
                    'identity-mapping)))
    (call-mapper mapper key value)))

(defgeneric call-mapper (mapper key value)
  (:method ((mapper symbol) key value)
   (funcall mapper key value))
  (:method ((mapper cons) key value)
   (funcall (apply 'alexandria:curry mapper)
          key value)))


;; Mapping Functions
(defvar *in-module-parse* nil)

(defun pathname-mapping (key value)
  "Pathnames require some special handling. On files the pathname option specifies the exact location
of the file, while on modules the pathname specifies the module directory."
  (if *in-module-parse*
      (list :directory value)
      (list key value)))
  
(defun version-dependency (thing)
  (and (consp thing)
       (= (length thing) 3)
       (eql (first thing) :version)))

(deftype asdf-dependency ()
  `(or string symbol (satisfies version-dependency)))

(defun convert-dependency (dependency)
  (check-type dependency asdf-dependency)
  (if (version-dependency dependency)
      (destructuring-bind (- name version) dependency
        (list name :version version))
      dependency))
  
(defun depends-on-mapping (key value)
  "Converts the ASDF :depends-on mapping to a mudballs system :needs option."
  (declare (ignore key))
  (cons :needs (mapcar 'convert-dependency value)))
        
(defun simple-converted-mapping (new-mapping key value)
  (declare (ignore key))
  (list new-mapping value))

(defun ignored-mapping (key value)
  (declare (ignore value))
  (warn "Ignoring ASDF option ~S." key))

(defun silent-mapping (key value)
  (declare (ignore key value)))

(defun identity-mapping (key value)
  (list key value))

(defun convert-components (key components)
  (let ((*in-module-parse* nil))
    (cons key
          (loop for (type name . options) in components
                collect (convert-component type name options)))))

(defun default-component-p (type)
  (eql type :file))

(defun convert-type (type)
  (find-symbol (symbol-name type)
               (load-time-value
                (package-name :sysdef-user))))

(defun version-mapping (key version)
  (cons key (sysdef::coerce-to-version version)))

(defun convert-component (type name options)
  (cond ((and (default-component-p type) (null options)) name)
        ((default-component-p type)
         (cons name (convert-options options)))
        (t (list* name (convert-type type)
                  (let ((*in-module-parse* (subtypep (convert-type type) 'module)))
                    (convert-options options))))))

(defun convert-in-order-to (key needs)
  (declare (ignore key))
  (cons :needs (loop :for (dependent-op . requirements) :in needs
                     :append (convert-requirement dependent-op requirements))))

(defun convert-requirement (op requirements)
  (loop :for (required-op . required-components) :in requirements
        :append (loop :for component :in required-components
                      :collect (list op component required-op))))


;; Borrowed from ASDF.
(defun resolve-symlinks (path)
  #-allegro (truename path)
  #+allegro (excl:pathname-resolve-symbolic-links path))

(defun register-asdf-systems (&key (errorp nil))
  "Registers all ASDF systems that can be found in the paths on *central-registry*. Mudballs will NOT automatically
reload these files if they are changed, you will have to call register-asdf-systems manually to in these cases."
  (let ((*signal-error* errorp))
    (dolist (file *central-registry*)
      (let ((definitions (directory (merge-pathnames (make-pathname :name :wild :type "asd" :version :newest)
                                                     file))))
        (dolist (definition definitions)
          (let ((truename (resolve-symlinks definition)))
            (let ((*default-development-mode* t))
              (load truename))))))))




(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass wrap-execute-gf (standard-generic-function)
    ()
    (:metaclass funcallable-standard-class))

  (defgeneric perform (operation system)
    (:generic-function-class wrap-execute-gf)
    (:documentation "The perform method is the ASDF equivalent to execute. Defining a method on perform will
add an appropriate method to execute.")))
  

;; we can't use (eql (ensure-generic-function 'perform)) as it breaks under ACL.
(defmethod add-method ((perform-gf wrap-execute-gf) (method method))
  (let ((gf (ensure-generic-function 'execute)))
    (add-method gf (make-instance (generic-function-method-class gf)
                                  :qualifiers (method-qualifiers method)
                                  :specializers (reverse (method-specializers method))
                                  :lambda-list (generic-function-lambda-list gf)
                                  :function (create-argument-rotater method)))))

(defun create-argument-rotater (method)
  (let ((method-function (method-function method)))
    #'(lambda  (x #+cmucl &optional y)
        #+(or lispworks allegro ccl openmcl)
        (funcall method-function y x)
        #+(or sbcl clisp cmucl)
        (funcall method-function (reverse x) y))))



(defun operate-on-system (class-name system &rest args &key &allow-other-keys)
  (apply 'sysdef:perform system class-name args))

(defun oos (class-name system &rest args &key &allow-other-keys)
  (apply 'operate-on-system class-name system args))


;; EOF
