(defpackage :de.halcony.argparse
  (:nicknames :cl-argparse)
  (:use :cl-user :cl)
  (:export create-parser
           parser
           parse
           add-flag
           add-optional
           add-positional
           add-subparser
           add-default
           get-value))
