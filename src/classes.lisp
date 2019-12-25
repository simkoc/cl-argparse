(in-package :de.halcony.argparse)


(define-condition cmd-arg-error (simple-error)
  "Condition that signals that a cmd argument was unexpected/wrong.
   Only relevant for internal use"
  ((format-control :initarg :format-control
                   :reader format-control
                   :initform "")
   (format-arguments :initarg :format-arguments
                     :reader format-arguments
                     :initform (list))))

(define-condition help-flag-condition (simple-error)
  "Condition that singals that a help flag was encountered during parsing"
  ())


(defmethod print-object ((cmd-arg-error cmd-arg-error) stream)
  (with-slots (format-control format-arguments)
      cmd-arg-error
    (apply #'format stream format-control format-arguments)))


(define-condition cancel-parsing-error (simple-error)
  "Condition that is singaled if for one reason or the other the
   parsing of the arg string is canceld. Contains the reason for
   it as values and can be pretty printed. Is exposed via the API.
   In case it was triggered by a help flag the text is empty"
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
