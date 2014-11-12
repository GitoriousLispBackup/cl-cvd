
(defun count-spaces (str)
  (let ((space-count 0))
    (iterate (for chr in-string str)
      (when (char= chr #\SPACE)
        (incf space-count))
      (finally (return space-count)))))
