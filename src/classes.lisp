(in-package :cl-argparser)


(define-condition cmd-arg-error (simple-error) ())


(defclass flag ()
  ((short :initarg :short
          :reader short)
   (long :initarg :long
         :initform nil
         :reader long)
   (var :initarg :var
        :reader var)
   (help-message :initarg :help-message
                 :reader help-message)))


(defmethod print-object ((flag flag) stream)
  (with-slots (short long var help-message)
      flag
    (format stream "[~a] ~a/~a ~a" var short long help-message)))


(defclass optional ()
  ((short :initarg :short
          :reader short)
   (long :initarg :long
         :initform nil
         :reader long)
   (var :initarg :var
        :reader var)
   (default :initarg :default
             :reader default)
   (help-message :initarg :help-message
                 :reader help-message)))


(defmethod print-object ((optional optional) stream)
  (with-slots (short long var help-message default)
      optional
    (format stream "[~a] ~a/~a ~a (default:~a)" var short long help-message (if default default "none"))))


(defclass positional ()
  ((name :initarg :name
         :reader name)
   (help-message :initarg :help-message
                 :reader help-message)))


(defmethod print-object ((positional positional) stream)
  (with-slots (name help-message)
      positional
    (format stream "[~a] ~a" name help-message)))


(defclass parser ()
  ((name :initform ""
         :initarg :name
         :reader name)
   (flags :initform (list)
          :reader flags)
   (optionals :initform (list)
              :reader optionals)
   (positionals :initform (list)
                :reader positionals)
   (subparsers :initform (list)
               :reader subparsers)
   (table :initform (make-hash-table :test 'equalp)
          :reader table)))


(defmethod print-object ((parser parser) stream)
  (with-slots (name flags optionals positionals subparsers table)
      parser
    (format stream "parser:~a~%flags:~%~{   ~a~%~}optionals:~%~{   ~a~%~}positionals:~%~{   ~a~%~}subparser:~%~{   ~a~%~}"
            name flags optionals positionals subparsers)
    (format stream "parsed values:~%")
    (maphash #'(lambda (key value)
                 (format stream "~a:~a~%" key value))
             table)))
