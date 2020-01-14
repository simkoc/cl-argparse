(in-package :de.halcony.argparse)


(defmethod parse-optionals ((parser parser) argv)
  "Given a parser and a list of strings this method extracts the provided
   flags and optional parameters and enters them into the parser table.
   It returns the remaining list. The parsing either stops if all optionals
   or flags of the parser a filled or if the currently viewed list element does
   not match either an optional or flag."
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
  "Given a parser and a list of strings this method extracts the
   provided positional parameters and enters them into the parser table.
   It returns the remaining list"
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
  "Given a parser and a list of strings this method extracts the
   subparser from which the parsing continues and continues the
   parsing with this parser"
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
  "This is the main parsing method and providing a parser and a string of
   arguments it extracts and enters all provided parameters into the parser"
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
  "This method tries to match the given (car argv) with the flag. If this
   succeeds the flag is entered into the parser value table and (cdr argv) and
   T is returned. If there is no match argv and nil is returned"
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
  "Adds a flag to the given parser using short as the short value of the flag (has to
   be lead with a single - in the argument list. The var determines the name under
   which it is stored in the parser value table. long determines the long version of the
   flag (lead with two -) and help is the help string used to generate the help message"
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
  "Tries to parse (car argv) using the passed flag (i.e., help flag).
   If the flag is successfully parsed the help-flag-condition is
   signaled, else argv and nil is returned."
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
  "Adds the help flag to the parser"
  (with-slots (flags)
      parser
    (push (make-instance 'help-flag) flags)))


(defmethod parse-optional ((optional optional) argv table)
  "Tries to parse (car argv) using the given optional. If the optional
   matches the defined table value is set to (cadr argv) and (cddr argv)
   and T are returned. Else argv and nil are returned"
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
  "Adds an optional value to the given parser. short is the to be used parameter name (with a single
   leading -). Var is the name under which to store the value in the parser table. long is the long
   version of the name (lead with two --). Default determines the default value and help is the
   help message string to be used in the auto generated help message"
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
  "Parses a positional argument, i.e., (car argv) and stores it in the
   given table. Throws an error in case no value is contained in argv.
   Returns (cdr argv) as it either always works or has to throw an error."
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
  "Adds a positional argument to the given parser. Name is both the name of the positional
   used in the help message as well as the name under which the value is stored. Help defines
   the string to be used in the auto generated help message"
  (with-slots (positionals)
      parser
    (setf positionals (append positionals
                              (list (make-instance 'positional
                                                   :name name
                                                   :help-message help))))))


(defmethod add-subparser ((parser parser) (subparser parser))
  "Adds the given sub parser to the given parser."
  (with-slots (subparsers name previous-parsers)
      parser
    (push subparser subparsers)
    (sync-parser parser)))


(defmethod sync-parser ((parser parser))
  "Syncs the current parser, i.e., merges the value table (for defaults) as well as
   append the list of previous parsers to the sub parser for generating the help
   message"
  (with-slots (name table previous-parsers subparsers)
      parser
    (dolist (subparser subparsers)
      (setf (slot-value subparser 'previous-parsers) (append previous-parsers (list name)))
      (setf (slot-value subparser 'table) table)
      (sync-parser subparser))))


(defmethod add-generic-parser ((parser parser) (gen parser))
  "Adds a generic parser to the given parser. Basically merges the
   generic parser into the given parser, adding all the flags, optional,
   and positional utilities. Subparsers are ignored."
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
  "Adds a default value to the parser value table. I.e., allows to define sub parser
   specific default values that cannot be set via the cmd"
  (with-slots (defaults)
      parser
    (setf (gethash var defaults) default)))


(defmacro create-sub-parser ((name &optional (description "no description")) &body elements)
  "Macro for creating a sub parser, provides name as the subparser name to the help message, adds a
   help flag and allows to add further parameters to the parser. Returns the parser at the end."
  `(let ((,name (make-instance 'parser :name (string-downcase (format nil "~a" ',name)) :description ,description)))
     (cl-argparse:add-help ,name)
     ,@elements
     ,name))


(defmacro create-main-parser ((var &optional (description "no description")) &body elements)
  "Macro for creating the main parser. Always names the parser main-parser (required for parsing
   to work) and adds the help flag. Allows to add further parameters to the parser. Returns the parser
   at the end"
  `(let ((,var (make-instance 'parser :name "main-parser" :description ,description)))
     (cl-argparse:add-help ,var)
     ,@elements
     ,var))


(defmethod get-value (name (parser parser))
  "Get the stored value for name from the given parser."
  (gethash name (slot-value parser 'table)))


(defmacro aif (condition if then)
  `(let ((it ,condition))
     (if it
         ,if
         ,then)))


(defmethod print-help ((parser parser))
  "Functionality to print the correct help message for the given parser"
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


(defmacro create-mockup-parser (&rest key-value-list)
  "allows for the creation of a mockup parser to enable creating a parser without actually parsing
   command line parameters"
  (assert (= (mod (length key-value-list) 2) 0) () "an even amount of key value pairs has to be provided")
  (let ((pair-up (do ((l key-value-list
                         (cddr l))
                      (pairs (list)))
                     ((not l) pairs)
                   (push (cons (car l) (cadr l)) pairs))))
    `(cl-argparse:parse
      (cl-argparse:create-main-parser (mockup "mockup parser")
        ,@(mapcar #'(lambda (key-value)
                      `(cl-argparse:add-default mockup
                                                :var ,(car key-value)
                                                :default ,(cdr key-value)))
                  pair-up))
      (list))))
