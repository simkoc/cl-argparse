(defpackage :de.halcony.argparse
  (:nicknames :cl-argparse)
  (:use :cl-user :cl)
  (:export create-main-parser
           create-sub-parser
           cancel-parsing-error
           parser
           parse
           add-flag
           add-optional
           add-positional
           add-subparser
           add-default
           add-help
           add-generic-parser
           get-value
           cancel-parsing-error
           get-key-value-pairs))
