#|
This file is supposed to give a quick example on how cl-argparse could be used to parse the command
line parameters for your awesome program.

The underlying idea is a program that manipulates strings. It provides two different possible actions: count and replace.
Count counts the occurences of substrings in the given string
Replace replaces certain substrings with others
The program expects a string as input, is able to lowercase the whole string before performing the given actions and is also able to skip the first n elements of the string.
|#

(ql:quickload :cl-ppcre)


;; here the sub parser for the replace action is created
(defun create-replace-parser ()
  (cl-argparse:create-sub-parser (replace "find and replace substrings")
    (cl-argparse:add-positional replace :name "find"
                                :help "the string to be replaced")
    (cl-argparse:add-positional replace :name "replace"
                                :help "the string to replace with")
    (cl-argparse:add-default replace
                             :var "func"
                             :default #'replace-main)))


;; here the sub parser for the count action is created
(defun create-count-parser ()
  (cl-argparse:create-sub-parser (count "count string element occurences")
    (cl-argparse:add-positional count :name "count"
                                :help "the string element to be counted")
    (cl-argparse:add-default count
                             :var "func"
                             :default #'count-main)))


;;; here the main parser is created
(defun create-parser ()
  (cl-argparse:create-main-parser (main-parser "an example program able to manipulate strings given via cmd")
    (cl-argparse:add-flag main-parser
                          :short "i"
                          :long "insensitive"
                          :help "if set lower case every character"
                          :var "insensitive")
    (cl-argparse:add-optional main-parser
                  :short "s"
                  :long "skip"
                  :help "skips the first n elements of the given strings"
                  :default "0"
                  :var "skip")
    (cl-argparse:add-positional main-parser
                    :name "input"
                    :help "the string to work on")
    (cl-argparse:add-subparser main-parser (create-replace-parser))
    (cl-argparse:add-subparser main-parser (create-count-parser))))


(defun main (argvs)
  (handler-case
      (let ((pargvs (cl-argparse:parse (create-parser) argvs)))
        (let ((workstring (if (cl-argparse:get-value "insensitive" pargvs)
                              (string-downcase (cl-argparse:get-value "input" pargvs))
                              (cl-argparse:get-value "input" pargvs))))
          (funcall (cl-argparse:get-value "func" pargvs)
                   (subseq workstring
                           (parse-integer (cl-argparse:get-value "skip" pargvs)))
                   pargvs)))
    (cl-argparse:cancel-parsing-error (e)
      (format t "~a~%" e))))


(defun replace-main (input-string pargvs)
  (format t "~a~%" pargvs)
  (cl-ppcre:regex-replace-all
   (cl-argparse:get-value "find" pargvs)
   input-string
   (cl-argparse:get-value "replace" pargvs)))


(defun count-main (input-string pargvs)
  (length (cl-ppcre:all-matches-as-strings
           (cl-argparse:get-value "count" pargvs)
           input-string)))



#|
Below a couple call options witht the command line arguments as a list of strings.
Remember, that depending on the OS the first element of, e.g., sb-ext:*argv*, is the
call path and thus a cdr is in order before passing that variable to the parser.
|#


;;; print the auto generated help message for the main parser
(main (list "-h"))


;;; print the auto generated help message for the sub parser sub
(main (list "input string value" "replace" "-h"))


;;; print the auto generated help message for the sub parser sub-2
(main (list "input string value" "count" "-h"))


;;; using the flags and arguments leading to the count functionality
(main (list "-s" "0" "input string value" "count" "in"))
;;; output: 2
(main (list "-s" "5" "input string value" "count" "in"))
;;; output: 1


;;; using the flags and arguments leading the replace functionality
(main (list "-i" "INPUT STRING VALUE" "replace" "in" "out"))
;;; output: output stroutg value
(main (list "INPUT STRING VALUE" "replace" "in" "out"))
;;; output: INPUT STRING VALUE
