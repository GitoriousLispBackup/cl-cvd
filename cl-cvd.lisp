
;; Copyright (C) 2014 Riley E.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

(in-package :cl-cvd)

(declaim (optimize (speed 3) (debug 3) (safety 3)))

  ;;; The constant PHI is used for spaced-repetition timing
(defconstant phi 1.618033988749895d0
  "The constant PHI, The golden ratio.")

(defvar *zh-hash-table* (make-hash-table)
  "Hash table for storing data of, and
    metadata about the vocabulary listing")

(defvar *mp3dir* nil)

(defvar *mp3-alist* nil)

(defvar *vocab-key-count* 0
  "Counter for the list of vocabulary entries,
    is incremented on addition of elements, or set
    when a data-set is loaded into the hash-table
    by the `import-vocab' function.")

(defvar *test-pool* nil
  "Words that have been seen during a practice session")

(defvar *current-hsk-level* 1)

(defvar *punctuation* (list #\.     #\,
                            #\;     #\:
                            #\/     #\\
                            #\|     #\!
                            #\-     #\_
                            #\(     #\) 
                            #\{     #\}
                            #\[     #\]
                            #\~     #\`
                            #\<     #\>
                            #\?     #\&
                            #\"     #\+
                            #\=))

(defvar *whitespace* (list #\SPACE   #\Tab
                           #\NEWLINE #\BACKSPACE))

(defstruct vocab-entry
  (hsk     1                    :type integer)
  (hanzi   ""                   :type string)
  (pinyin  ""                   :type string)
  (english (list "")            :type list)
  (seealso (list "")            :type list)
  (units   (list "")            :type list)
  (score   (complex 0.0 0.0)    :type complex)
  (date    (get-universal-time) :type integer)
  (reps    1                    :type integer))

(defun element-of-truth (l)
  (iterate (for i in l)
    (when i (return i))))

(defun gen-ht-key (prefix)
  (let ((the-sym-name (format nil "~D-~D" prefix (incf *vocab-key-count*))))
    (intern the-sym-name :cl-cvd)))

(defun key-exists-p (key table)
  (let ((target (gethash key table)))
    (when target key)))

(defun puthash (key table object)
  (setf (gethash key table) object))

(defun hsk-apropos (level)
  (declare (fixnum level))
  (loop :for key :being the hash-keys :of *zh-hash-table*
        :for val :being the hash-value :of *zh-hash-table*
        :when (= level (the fixnum (vocab-entry-hsk val)))
          :collect key))

(defun zh-apropos (zh-string)
  (declare (string zh-string))
  (loop :for key :being the hash-keys :of *zh-hash-table*
        :for val :being the hash-value :of *zh-hash-table*
        :when (scan zh-string (vocab-entry-hanzi val))
          :collect (list key val)))

(defun zh-apropos-key (zh-string)
  (declare (string zh-string))
  (loop :for key :being the hash-keys  :of *zh-hash-table*
        :for val :being the hash-value :of *zh-hash-table*
        :when (scan zh-string (vocab-entry-hanzi val))
          :collect key))

(defun en-apropos (en-string)
  (declare (string en-string))
  (loop :for key :being the hash-keys :of *zh-hash-table*
        :for val :being the hash-value :of *zh-hash-table*
        :when (element-of-truth
               (mapcar (lambda (s)
                         (scan en-string s))
                       (vocab-entry-english val)))
          :collect (list key val)))

(defun en-apropos-word (en-word)
  (declare (string en-word))
  (loop :for key :being the hash-keys :of *zh-hash-table*
        :for val :being the hash-value :of *zh-hash-table*
        :when (element-of-truth
               (mapcar (lambda (s)
                         (find-word-in-string en-word s))
                       (vocab-entry-english val)))
          :collect (list key val)))

(defun find-word-in-string (word target-string)
  (declare (string word target-string))
  (multiple-value-bind (word-begin word-end) (scan word target-string)
    (when (and word-begin word-end)
      (cond ((string= word target-string) word)
            ((and (or (zerop word-begin)
                      (delimiter-p (char target-string (- word-begin 1))))
                  (or (= (length target-string) word-end)
                      (delimiter-p (char target-string word-end))))
             word)))))

(defun delimiter-p (chr)
  (or (member chr *punctuations*)
      (member chr *whitespace*)))

(defun add-entry (&key hanzi pinyin english (hsk 0) (hash-table *zh-hash-table*))
  (puthash (gen-ht-key 'zh-index)
           hash-table
           (make-vocab-entry :hanzi   hanzi
                             :pinyin  pinyin
                             :english english
                             :hsk     hsk)))

(defun revise-entry (&key key field new-data (hash-table *zh-hash-table*))
  (let ((the-object (gethash key hash-table)))
    (case field
      ((hanzi)   (setf (vocab-entry-hanzi   the-object) new-data))
      ((pinyin)  (setf (vocab-entry-pinyin  the-object) new-data))
      ((english) (setf (vocab-entry-english the-object) new-data)))))

(defun append-english (english-strings &key key (hash-table *zh-hash-table*))
  (let ((the-object (gethash key hash-table)))
    (revise-entry (append (vocab-entry-english the-object) english-strings)
                  :key key
                  :field 'english)))

(defun update-score (answer hash-key test-type &key (hash-table *zh-hash-table*))
  (let ((vocab-entry (gethash hash-key hash-table)))
    (setf (vocab-entry-score vocab-entry)
          (+ (vocab-entry-score vocab-entry)
             (score-result (check-answer answer vocab-entry test-type))))
    (setf (vocab-entry-date vocab-entry)
          (get-universal-time))
    (incf (vocab-entry-reps vocab-entry))))

(defun export-vocab (&key (vocab-table *zh-hash-table*) (filename "zh-portable.raw"))    
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (pprint (maphash (lambda (a b)
                         (list a b))
                       vocab-table) out))))

(defun import-vocab (&key (vocab-table *zh-hash-table*) (filename "zh-portable.raw"))
  (labels ((structure-vocab (l)
             (puthash (car l) vocab-table (cadr l))))
    (with-open-file (in filename)
      (declare (dynamic-extent in))
      (with-standard-io-syntax
        (mapcar #'structure-vocab (read in))))
    (setf *vocab-key-count* (hash-table-count *zh-hash-table*))))

(defun fill-mp3-paths ()
  (setf *mp3dir* (directory #P"~/chinese/hsk_mp3/*.mp3"))
  nil)

(defun find-mp3-path (match-name)
  (iterate (for elt in *mp3dir*)
    (finding elt such-that (scan match-name (namestring elt)))))

(defun find-matching-mp3 (vocab-key)
  (let* ((vocab-entry (gethash vocab-key *zh-hash-table*))
         (pinyin (vocab-entry-pinyin vocab-entry))
         (nospace (regex-replace " " pinyin ""))
         (match-name (concatenate 'string "-" nospace "-"))
         (mp3-path (find-mp3-path match-name)))
    (when mp3-path
      (push (list vocab-key
                  (namestring mp3-path))
            *mp3-alist*))))

(defun find-active-vocab-mp3s (&optional (source-list *mp3dir*))
  (mapcar (lambda (key)
            (unless (assoc key *mp3-alist*)
              (find-matching-mp3 key)))
          source-list))

(defun play-mp3 (key)
  (bordeaux-threads:make-thread (lambda ()
                                  (run "/usr/bin/mpg123"
                                       (cdr (assoc key *mp3-alist*))))
                                :name "mp3 playback thread"))

(defun is-subset-p (set-x set-y)
  (labels ((subset-p (set-x set-y)
             (not (set-difference set-x set-y))))
    (and (subset-p set-x set-y)
         (subset-p set-y set-x))))

(defun load-from-hsk (hsk-val &optional (n 10))
  (let ((base (length *test-pool*)))
    (setf *test-pool*
          (nconc *test-pool*
                 (subseq (reverse (hsk-apropos hsk-val))
                         base
                         (+ n base))))))

(defun enumerate-qualified-elements ()
  (length (remove-if-not #'qualified-p *test-pool*)))

(defun refil-testing-pool (hsk upper-bound)
  (load-from-hsk hsk (- upper-bound (enumerate-qualified-elements))))

(defun hsk-spillover ()
  (if (and (hsk-apropos (+ *current-hsk-level* 1))
           (set-equal-p *test-pool* (hsk-apropos *current-hsk-level*)))
      (incf *current-hsk-level*)
      (format nil "Takeshi: ``Amazing!''")))

(defun count-spaces (str)
  (let ((space-count 0))
    (iterate (for chr in-string str)
      (when (char= chr #\SPACE)
        (incf space-count))
      (finally (return space-count)))))

(defun english-sensible-p (vocab-entry)
  (element-of-truth (mapcar (lambda (s)
                              (< (count-spaces s) 2))
                            (vocab-entry-english vocab-entry))))

(defun sensible-tests (vocab-element)
  (if (english-qualified-p vocab-element)
      (list 'english 'hanzi 'pinyin)
      (list 'hanzi 'pinyin)))

(defun qualified-p (vocab-struct)
  (and (> 10 (vocab-entry-reps vocab-struct))
       (> (get-universal-time)
          (vocab-entry-date vocab-struct))))

(defun set-next-test (vocab-struct)
  (setf (vocab-entry-date vocab-struct)
        (schedule-next-test vocab-struct)))

(defun show-challenge (&key field key (hash-table *zh-hash-table*))
  (let ((the-object (gethash key hash-table)))
    (case field
      ((english)     (nth (random (length (vocab-entry-english the-object)))
                          (vocab-entry-english the-object)))
      ((english-all) (format nil "~{~A~^, ~}." (vocab-entry-english the-object)))
      ((pinyin)      (vocab-entry-pinyin the-object))
      ((hanzi)       (vocab-entry-hanzi the-object)))))

(defun take-answer (&key test)
  (format t "~D> " test)
  (read-line))

(defun construct-test-list (length &key (test-pool *test-pool*) (vocab *zh-hash-table*))
  "Construct a test list of LENGTH members"
  (let ((repeat 0)
        (result))
    (iterate (for key in test-pool)
      (if (>= repeat length)
          result
          (when (qualified-p (gethash key vocab))
            (incf repeat)
            (collect key into result at beginning))))))

(defun reconstruct-test-pool ()
  (maphash (lambda (key val)
             (when (< 1 (vocab-entry-reps val))
               (push key *test-pool*)))
           *zh-hash-table*))

(defun string-in-list-p (string l)
  (iterate (for s in l)
    (when (string= s string)
      l)))

(defun check-answer (answer vocab-entry test-type)
  (cond ((not (member test-type '(english hanzi pinyin)))
         (error "Unknown test-type"))
        ((and (equalp test-type 'english)
              (string-in-list-p answer (vocab-entry-english vocab-entry))))
        ((and (equalp test-type 'hanzi)
              (string= answer (vocab-entry-hanzi vocab-entry))))
        ((and (equalp test-type 'pinyin)
              (string= answer (vocab-entry-pinyin vocab-entry))))))

(defun score-result (result)
  (if result
      1
      #C(0 1)))

(defun determine-offset (c)
    (let ((ratio (/ (realpart c) (imagpart c))))
      (cond ((<= ratio 1)  3600)
            ((<= ratio 2)  7200)
            ((<= ratio 5)  10800)
            ((<= ratio 10) 28000))))

(defun schedule-next-test (reps score)
  (round
   (+ (get-universal-time)
      (* (+ 7200
            (determine-offset score) 
            (expt phi (/ reps 2)))))))

(defun display-and-play (&key key from for)
  (let ((goal      (show-challenge :field for :key key))
        (challenge (show-challenge :field from :key key)))
    (play-mp3 key)
    (format t "~D~%~D> " challenge for)
    (let* ((vocab-entry (gethash key *zh-hash-table*))
           (results     (check-answer (get-answer) vocab-entry for))
           (reps        (vocab-entry-reps vocab-entry)))
      (setf (vocab-entry-score vocab-entry)
            (+ (score-result results)
               (vocab-entry-score vocab-entry)))
      (if results
          (progn (setf (vocab-entry-date vocab-entry)
                       (schedule-next-test reps
                                           (vocab-entry-score vocab-entry)))
                 (incf reps))
          (progn (setf *test-pool*
                       (reverse (cons key (reverse *test-pool*))))
                 goal)))))

(defun get-answer ()
  (read-line))

(defun test-loop (&optional (n 10) (type 'random))
  (let ((test-list (construct-test-list n)))
    (iterate (for elt in test-list)
      (for i from n downto 0)
      
      (case type
        ((random) (random-test elt))
        ((t) (display-and-play :key elt :from 'pinyin :for 'hanzi))))))

(defun random-test (key)
  (let* ((test-list (list 'hanzi 'pinyin 'english))
         (crazy-english (list 'hanzi 'pinyin))
         (sane-for (if (english-sensible-p (gethash key *zh-hash-table*))
                       (nth (random (length test-list)) test-list)
                       (nth (random (length crazy-english)) crazy-english)))
         (rest-tests (delete sane-for test-list))
         (from (nth (random (length rest-tests)) rest-tests)))
    (display-and-play :key key :from from :for sane-for)))
