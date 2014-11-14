
(asdf:defsystem #:cl-cvd
  :serial t
  :description "Chinese Vocabulary Drill"
  :author "Riley E."
  :license "GPLv3 or Later"
  :depends-on (:iterate
                :cl-ppcre
                :cl-csv
                :external-program
                :bordeaux-threads
                :cl-cffi-gtk)
  :components ((:file "package")
               (:file "cl-cvd")
               (:file "cl-cvd-gui")
               (:file "csv-import")))
