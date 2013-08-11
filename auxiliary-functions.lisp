;;; Auxillary Function Used Throughout Other Files


(setf (symbol-function 'find-all-if) #'remove-if-not)

;; Section 3.19 pg. 101
(defun find-all (item sequence &rest keyword-args
                      &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequenc that match item,
  according to the keywords. Doesn't alter seuqence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
    (apply #'remove item sequence
           :test (complement test) keyword-args)))

(defun starts-with (list x)
  "Is this a list whole first element is x?"
  (and (consp list) (eql (first list) x)))
