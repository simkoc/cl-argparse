(in-package :de.halcony.argparse)


(define-condition cmd-arg-error (simple-error)
  ((format-control :initarg :format-control
                   :reader format-control
                   :initform "")
   (format-arguments :initarg :format-arguments
                     :reader format-arguments
                     :initform (list))))

(define-condition help-flag-condition (simple-error)
  ())


(defmethod print-object ((cmd-arg-error cmd-arg-error) stream)
  (with-slots (format-control format-arguments)
      cmd-arg-error
    (apply #'format stream format-control format-arguments)))


(define-condition cancel-parsing-error (simple-error)
  ((format-control :initarg :format-control
                   :reader format-control
                   :initform "")
   (format-arguments :initarg :format-arguments
                     :reader format-arguments
                     :initform (list))))


(defmethod print-object ((cancel-parsing-error cancel-parsing-error) stream)
  (with-slots (format-control format-arguments)
      cancel-parsing-error
    (apply #'format stream format-control format-arguments)))


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
    (format stream "   ~a ~a" (if long
                                  (format nil "-~a/--~a" short long)
                                  (format nil "-~a" short))
            help-message)))


(defclass help-flag (flag)
  ((short :initform "h")
   (long :initform "help")
   (help-message :initform "displays this help message")))


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
    (format stream "   ~a ~a" (if long
                                  (format nil "-~a/--~a" short long)
                                  (format nil "-~a" short))
            help-message)))


(defclass positional ()
  ((name :initarg :name
         :reader name)
   (help-message :initarg :help-message
                 :reader help-message)))


(defmethod print-object ((positional positional) stream)
  (with-slots (name help-message)
      positional
    (format stream "   [~a] ~a" name help-message)))


(defclass parser ()
  ((description :initarg :description
                :reader description
                :initform "no description")
   (program-name :initform "program.lisp"
                 :initarg :program-name
                 :reader program-name)
   (previous-parsers :initform (list)
                    :reader previous-parsers)
   (name :initform ""
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
   (defaults :initform (make-hash-table :test 'equalp)
             :reader defaults)
   (table :initform (make-hash-table :test 'equalp)
          :reader table)))

(defmethod print-object ((parser parser) stream)
  (with-slots (table)
      parser
    (format stream "<PARSER~%")
    (maphash #'(lambda (key value)
                 (format stream "~a -> ~a~%" key value))
             table)
    (format stream ">")))
