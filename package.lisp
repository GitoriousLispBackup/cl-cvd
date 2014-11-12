
(defpackage #:chinese-vocab-drill
  (:nicknames #:cl-cvd)
  (:use :common-lisp
        :iterate
        :bordeaux-threads
        :cl-ppcre
        :cl-csv
        :external-program))
