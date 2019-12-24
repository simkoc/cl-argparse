(in-package :de.halcony.argparse)


(defmethod parse-optionals ((parser parser) argv)
  ;(format t "parsing optionals on ~a~%" argv)
  (with-slots (flags optionals table)
      parser
    (do ((argv argv)
         (had-match nil nil)
         (stop nil))
        (stop argv)
      (dolist (flag flags)
        (multiple-value-bind (rem-argv match-p)
            (parse-flag flag argv table)
          (setf argv rem-argv)
          (setf had-match (or had-match match-p))))
      (dolist (optional optionals)
        (multiple-value-bind (rem-argv match-p)
            (parse-optional optional argv table)
          (setf argv rem-argv)
          (setf had-match (or had-match match-p))))
      (if had-match
          (setf stop nil)
          (setf stop T)))))


(defmethod parse-positionals ((parser parser) initial-argv)
  ;(format t "parsing positionals on ~a~%" initial-argv)
  (with-slots (positionals table)
      parser
    (do ((parsers positionals
                  (cdr parsers))
         (remaining-argv initial-argv))
        ((not parsers) remaining-argv)
      (multiple-value-bind (rem-argv)
          (parse-positional (car parsers) remaining-argv table)
        (setf remaining-argv rem-argv)))))


(defmethod parse-subparsers ((parser parser) argv)
  (with-slots (subparsers)
      parser
    (if (and subparsers argv)
        (progn
          (dolist (subparser subparsers)
            (with-slots (name)
                subparser
              (if (string= (car argv) name)
                  (return-from parse-subparsers (multiple-value-bind (parser argv)
                                                    (parse subparser (cdr argv))
                                                  (values parser argv))))))
          (error 'cmd-arg-error
                 :format-control "there is no matching action command"))
        (if (and subparsers
                 (not argv))
            (error 'cmd-arg-error
                   :format-control "there is a missing action command")
            argv))))


(defmethod parse ((parser parser) argv)
  (with-slots (defaults table)
      parser
    (maphash #'(lambda (key value)
                 (setf (gethash key table) value))
             defaults))
  (handler-case
      (multiple-value-bind (argv)
          (parse-optionals parser argv)
        (multiple-value-bind (argv)
            (parse-positionals parser argv)
          (multiple-value-bind (ret-parser argv)
              (parse-subparsers parser argv)
            (declare (ignore ret-parser))
            (if argv
                (error 'cmd-arg-error
                       :format-control "to many arguments provided. Leftover ~a"
                       :format-arguments (list argv))
                parser))))
    (cmd-arg-error (e)
      (print-help parser)
      (error 'cancel-parsing-error
             :format-control "The provided arguments are not satisfactory"))
    (help-flag-condition (e)
      (print-help parser)
      (error 'cancel-parsing-error))))


(defmethod parse-flag ((flag flag) argv table)
  (with-slots (short long var)
      flag
    (let ((short (format nil "-~a" short))
          (long (if long (format nil "--~a" long) nil)))
      (if (or (string= short (car argv))
              (and long
                   (string= long (car argv))))
          (values (cdr argv) (setf (gethash var table) t))
          (values argv nil)))))


(defmethod add-flag ((parser parser)
                     &key
                       (short (error "a short flag has to be provided")) long help
                       (var (error "the variable name has to be provided")))
  (with-slots (table flags)
      parser
    (push
     (make-instance 'flag
                    :help-message help
                    :short short
                    :long long
                    :var var)
     flags)
    (setf (gethash var table) nil)
    parser))


(defmethod parse-flag ((help-flag help-flag) argv table)
  (with-slots (short long)
      help-flag
    (let ((short (format nil "-~a" short))
          (long (if long (format nil "--~a" long) nil)))
      (if (or (string= short (car argv))
              (and long
                   (string= long (car argv))))
          (error 'help-flag-condition)
          (values argv nil)))))


(defmethod add-help ((parser parser))
  (with-slots (flags)
      parser
    (push (make-instance 'help-flag) flags)))


(defmethod parse-optional ((optional optional) argv table)
  (with-slots (short long var)
      optional
    (let ((short (format nil "-~a" short))
          (long (if long (format nil "--~a" long) nil)))
      (if (or (string= short (car argv))
              (and long
                   (string= long (car argv))))
          (values (cddr argv) (setf (gethash var table) (cadr argv)))
          (values argv nil)))))


(defmethod add-optional ((parser parser)
                         &key
                           (short (error "a short value has to be provided")) long default help
                           (var (error "the variable name has to be provided")))
  (with-slots (table optionals)
      parser
    (push
     (make-instance 'optional
                    :short short
                    :long long
                    :default default
                    :help-message help
                    :var var
                    :default default)
     optionals)
    (if default
        (setf (gethash var table) default))))


(defmethod parse-positional ((positional positional) argv table)
  (with-slots (name)
      positional
    (if argv
        (progn
          (setf (gethash name table) (car argv))
          (cdr argv))
        (error 'cmd-arg-error
               :format-control "missing positional argument for ~a"
               :format-arguments (list name)))))


(defmethod add-positional ((parser parser)
                           &key
                             (name (error "the variable name has to be provided"))
                             (help ""))
  (with-slots (positionals)
      parser
    (setf positionals (append positionals
                              (list (make-instance 'positional
                                                   :name name
                                                   :help-message help))))))


(defmethod add-subparser ((parser parser) (subparser parser))
  (with-slots (subparsers name previous-parsers)
      parser
    (push subparser subparsers)
    (sync-parser parser)))


(defmethod sync-parser ((parser parser))
  (with-slots (name table previous-parsers subparsers)
      parser
    (dolist (subparser subparsers)
      (setf (slot-value subparser 'previous-parsers) (append previous-parsers (list name)))
      (setf (slot-value subparser 'table) table)
      (sync-parser subparser))))


(defmethod add-generic-parser ((parser parser) (gen parser))
  (with-slots (flags optionals positionals)
      parser
    (let ((pflags flags)
          (poptionals optionals)
          (ppositionals positionals))
      (with-slots (flags optionals positionals subparsers)
          gen
        (setf pflags (append pflags flags))
        (setf poptionals (append poptionals optionals))
        (setf ppositionals (append ppositionals positionals))))))


(defmethod add-default ((parser parser)
                        &key
                          (var (error "a variable name has to be provided"))
                          (default (error "the default value has to be provided")))
  (with-slots (defaults)
      parser
    (setf (gethash var defaults) default)))


(defmacro create-sub-parser ((name &optional (description "no description")) &body elements)
  `(let ((,name (make-instance 'parser :name (string-downcase (format nil "~a" ',name)) :description ,description)))
     (cl-argparse:add-help ,name)
     ,@elements
     ,name))

(defmacro create-main-parser ((var &optional (description "no description")) &body elements)
  `(let ((,var (make-instance 'parser :name "main-parser" :description ,description)))
     (cl-argparse:add-help ,var)
     ,@elements
     ,var))


(defmethod get-value (name (parser parser))
  (gethash name (slot-value parser 'table)))


(defmacro aif (condition if then)
  `(let ((it ,condition))
     (if it
         ,if
         ,then)))


(defmethod print-help ((parser parser))
  (with-slots (previous-parsers name flags optionals positionals subparsers description)
      parser
    (format t "./program.lisp~a~a~a~a~%~%~a~%~%"
            (aif (append previous-parsers (list name))
                 (format nil " ~{... ~a~^ ~}" (remove-if #'(lambda(elem)
                                                             (string= elem "main-parser"))
                                                         it))
                 "")
            (aif (append (mapcar #'short flags)
                         (mapcar #'short optionals))
                 (format nil " (~{-~a~^,~})" it)
                 "")
            (aif (mapcar #'name positionals)
                 (format nil "~{ [~a]~}" it)
                 "")
            (aif (mapcar #'name subparsers)
                 (format nil " {~{~a~^,~}}" it)
                 "")
            description)
    (format t "~{~a~%~}" flags)
    (format t "~{~a~%~}" optionals)
    (format t "~{~a~%~}" positionals)))
