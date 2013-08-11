;;; Chapter 1
;; 1.1 [m]
(defparameter *endings*
  '(Jr MD PhD)
  "List of titles that can appear at the end of a name.")

(defun last-name (name)
  (let ((lastname (first (last name))))
    (if (member lastname *endings*)
        (last-name (butlast name))
      lastname)))


;; 1.2 [m]
(defun power (b e)
  "Raise base B to the exponent E."
  (cond
   ((= e 0) 1)
   ((evenp e) (expt (power b (/ e 2)) 2))
   (t (* b (power b (1- e))))))

;; 1.3 [m]
(defun atom-count (exp)
  "Counts the number of atoms in a list."
  (cond
   ((null exp) 0)
   ((atom exp) 1)
   (t (+ (atom-count (first exp)) (atom-count (rest exp))))))

;; 1.4 [m]
(defun count-anywhere (item tree)
  "Count the number of times an expression appears in another expression."
  (cond
   ((eql item tree) 1)
   ((atom tree) 0)
   (t (+ (count-anywhere item (first tree))
         (count-anywhere item (rest tree))))))


;; 1.5 [m]
(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (apply #'+ (mapcar #'* a b)))


;;; Chapter 2
