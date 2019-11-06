(defpackage :de.halcony.argparse
  (:nicknames :cl-argparse)
  (:use :cl-user :cl)
  (:export create-main-parser
           create-sub-parser
           parser
           parse
           add-flag
           add-optional
           add-positional
           add-subparser
           add-default
           add-help
           get-value))
