(in-package :de.halcony.argparse)


(defmethod parse-optionals ((parser parser) argv)
  (format t "parsing optionals~%")
  (with-slots (flags optionals table)
      parser
    (do ((argv argv)
         (had-match nil nil)
         (stop nil))
        (stop argv)
      (dolist (flag flags)
        (format t "parsing: ~a~%" flag)
        (multiple-value-bind (rem-argv match-p)
            (parse-flag flag argv table)
          (format t "match-p ~a~%" match-p)
          (setf argv rem-argv)
          (setf had-match (or had-match match-p))))
      (dolist (optional optionals)
        (format t "parsing: ~a~%" optional)
        (multiple-value-bind (rem-argv match-p)
            (parse-optional optional argv table)
          (format t "match-p ~a~%" match-p)
          (setf argv rem-argv)
          (setf had-match (or had-match match-p))))
      (if had-match
          (setf stop nil)
          (setf stop T)))))


(defmethod parse-positionals ((parser parser) initial-argv)
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
    (dolist (subparser subparsers)
      (with-slots (name)
          subparser
        (if (string= (car argv) name)
            (return (parse parser (cdr argv))))))
    argv))


(defmethod parse ((parser parser) argv)
  (with-slots (defaults table)
      parser
    (maphash #'(lambda (key value)
                 (setf (gethash key table) value))
             defaults))
  (multiple-value-bind (argv)
      (parse-optionals parser argv)
    (multiple-value-bind (argv)
        (parse-positionals parser argv)
      (multiple-value-bind (argv)
          (parse-subparsers parser argv)
        (if argv
            (error 'cmd-arg-error
                   :format-control "to many arguments provided. Leftover ~a"
                   :format-arguments (list argv))
            parser)))))


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
    (push (make-instance 'positional
                         :name name
                         :help-message help)
          positionals)))


(defmethod add-subparser ((parser parser) (subparser parser))
  (setf (slot-value subparser 'table) (slot-value parser 'table))
  (with-slots (subparsers)
      parser
    (push subparser subparsers)))


(defmethod add-generic-parser ((parser parser) (gen parser))
  (with-slots (pflags poptionals ppositionals psubparsers)
      parser
    (with-slots (flags optionals positionals subparsers)
        gen
      (setf pflags (append pflags flags))
      (setf poptionals (append poptionals optionals))
      (setf ppositionals (append ppositionals positionals))
      (setf psubparsers (append psubparsers subparsers)))))


(defmethod add-default ((parser parser)
                        &key
                          (var (error "a variable name has to be provided"))
                          (default (error "the default value has to be provided")))
  (with-slots (defaults)
      parser
    (setf (gethash var defaults) default)))


(defmacro create-parser ((name) &body elements)
  `(let ((,name (make-instance 'parser :name (string-downcase (format nil "~a" ',name)))))
     ,@elements
     ,name))


(defmethod get-value (name (parser parser))
  (gethash name (slot-value parse 'table)))
