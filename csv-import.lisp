
(defun preprocess-english (desc-string)
  (car (read-csv desc-string :separator #\SEMICOLON)))

(defun collect-measures (l)
  (iterate (for s in l)
    (when (and (stringp s)
               (< 4 (length s))
               (string= (subseq s 0 3) "CL:"))
      (collect s))))

(defun clean-measures (s)
  (regex-replace "CL:" s ""))

(defun mapcaf (f list)
  (apply #'append (mapcar f list)))

(defun flatten (tree)
  (declare (inline mapcaf))
  (when tree
    (if (atom tree)
        (list tree)
        (mapcaf #'flatten tree))))

(defun finalize-measures (l)
  (let ((objet-petit-a (collect-measures l)))
    (unless (zerop (length objet-petit-a))
      (flatten
       (mapcar #'cl-csv:read-csv
               (mapcar #'clean-measures objet-petit-a))))))

(defun clean-english (l)
  (remove-if (lambda (s)
               (or
                (and (< 8 (length s))
                     (or (string= (subseq s 0 8) "see also")
                         (string= (subseq s 0 9) "(see also")))
                (and (< 4 (length s))
                     (string= (subseq s 0 3) "CL:"))))
             l))

(defun collect-see-also (l)
  (iterate (for s in l)
    (when (and (stringp s)
               (< 8 (length s))
               (string= (subseq s 0 8) "see also"))
      (collect s))))

(defun eleml-to-struct (l)
  (destructuring-bind (hsk hanzi pinyin description) l
    (let* ((pre-english (preprocess-english description))
           (units       (finalize-measures  pre-english))
           (see-also    (collect-see-also   pre-english))
           (english     (clean-english      pre-english)))
      (make-vocab-entry :hsk     (read-from-string hsk)
                        :hanzi   hanzi
                        :pinyin  pinyin
                        :english english
                        :units   units
                        :seealso see-also))))

(defun batch-add-table (l)
  (dolist (lx l)
    (puthash (gen-ht-key 'zh-index)
             *zh-hash-table*
             (eleml-to-struct lx))))
