
(defpackage #:chinese-vocab-drill
  (:nicknames #:cl-cvd)
  (:use :iterate :bordeaux-threads :cl-ppcre
        :cl-csv :gtk :gdk :gobject :glib :pango
        :cairo :cffi :external-program :cl))
