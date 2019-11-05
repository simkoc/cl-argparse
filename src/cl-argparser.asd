(asdf:defsystem :cl-argparse
  :name "cl-argparse"
  :author "Simon Koch <projects@halcony.de>"
  :maintainer "Simon Koch <projects@halcony.de>"
  :components ((:file "package")
               (:file "classes"
                :depends-on ("package"))
               (:file "methods"
                :depends-on ("package"
                             "classes"))))
