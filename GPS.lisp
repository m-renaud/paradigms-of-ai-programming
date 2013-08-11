;;; GeneralProblem Solver
;;; Chapter 4


;; Section 4.11 pg. 129
;; Originally member-equal
;; Modified as per suggestion later on the page.
(defun member-condition (item list &optional (test #'equal))
  (member item list :test test))


(defvar *ops* nil
  "A list of available operators for solving a problem.")

(defun use (oplist)
  "Use oplist as the default list of operators."
  (length (setf *ops* oplist)))

(defstruct op
  "An operation"
  (action nil)
  (preconds nil)
  (add-list nil)
  (del-list nil))

(defun executing-p (x)
  "Is x of the form: (executing ...) ?"
  (starts-with x 'executing))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
   (make-op :action action :preconds preconds
            :add-list add-list :del-list del-list)))


(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: achieve all goals using *ops*."
  ;; To (somewhat) solve "Leaping Before You Look", remove all goals
  ;; from the start state. Unfortunately a problem such as
  ;; (gps '(a) '(a)) is not achievable with this modification.
  ;; Therefore, the problem is not solved with this approarch.
  ;; (let ((state (set-difference state goals)))

  (find-all-if #'action-p (achieve-all (cons '(start) state) goals nil)))

(defun action-p (x)
  "Is x something that is (start) or (executing ...)?"
  (or (equal x '(start)) (executing-p x)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds, or if there is
  an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-condition goal state) state)
        ((member-condition goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))


(defun achieve-all (state goals goal-stack)
  "Try to achieve each goal, then make sure they still hold."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member-condition goal (op-add-list op)))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      ;; Return an updated state.
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x) (member-condition x (op-del-list op)))
                         state2)
              (op-add-list op)))))



;; Section 4.10 - The Lack of Intermediate Information Problem
;;   Addition of debugging facilities to the code.
(defvar *dbg-ids* nil
  "Identifier used by dbg.")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented dbg info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (format *debug-io* "~&~V@T~?" (* 2 indent) format-string args)))

(defun dbg-enable (&rest ids)
  "Enable dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun dbg-disable (&rest ids)
  "Disable dbg output on the given ids. With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                    (set-difference *dbg-ids* ids))))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Section 4.4 - Test
(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
            :preconds '(car-needs-battery shop-knows-problem shop-has-money)
            :add-list '(car-works))
   (make-op :action 'tell-shop-problem
            :preconds '(in-communication-with-shop)
            :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop
            :preconds '(know-phone-number)
            :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
            :preconds '(have-phone-book)
            :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
            :preconds '(have-money)
            :add-list '(shop-has-money)
            :del-list '(have-money))
   (make-op :action 'ask-phone-number
            :preconds '(in-communication-with-shop)
            :add-list '(know-phone-number))))

(mapc #'convert-op *school-ops*)

;; Section 4.4 - Stage 4: Test
;; (gps '(son-at-home car-needs-battery have-money have-phone-book)
;;      '(son-at-school)
;;      *school-ops*)


;; Section 4.7 - The Clobbered Sibling Goal Problem
;;  With the original version of GPS without achieve all, have-money
;;  is achieved and then son-at-school is achieved, but in the process,
;;  undoing the goal of have-money.
;; (gps '(son-at-home car-needs-battery have-money have-phone-book)
;;      '(have-money son-at-school)
;;      *school-ops*)


;; Section 4.8 - Leaping Before You Look Problem
;;   Could be solved by doing a (set-differenc *state* goals)
;;   before running GPS. See GPS definition for explatation.
;; (gps '(son-at-home car-needs-battery have-money have-phone-book)
;;      '(son-at-school have-money)
;;      *school-ops*)


;; Section 4.9 - The Recursive Subgoal Problem
;; (gps '(son-at-home car-needs-battery have-money)
;;      '(son-at-school)
;;      *school-ops*)



;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Section 4.12 - The New Domain Problem: Monkey and Bananas
(defparameter *banana-ops*
  (list
    (op 'climb-on-chair
        :preconds '(chair-at-middle-room at-middle-room on-floor)
        :add-list '(at-bananas on-chair)
        :del-list '(at-middle-room on-floor))
    (op 'push-chair-from-door-to-middle-room
        :preconds '(chair-at-door at-door)
        :add-list '(chair-at-middle-room at-middle-room)
        :del-list '(chair-at-door at-door))
    (op 'walk-from-door-to-middle-room
        :preconds '(at-door on-floor)
        :add-list '(at-middle-room)
        :del-list '(at-door))
    (op 'grasp-bananas
        :preconds '(at-bananas empty-handed)
        :add-list '(has-bananas)
        :del-list '(empty-handed))
    (op 'drop-ball
        :preconds '(has-ball)
        :add-list '(empty-handed)
        :del-list '(has-ball))
    (op 'eat-bananas
        :preconds '(has-bananas)
        :add-list '(empty-handed not-hungry)
        :del-list '(has-bananas hungry))))

(mapc #'convert-op *banana-ops*)


;; Pg 133
;; (gps '(at-door on-floor has-ball hungry chair-at-door) '(not-hungry))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Section 4.13 - The Maze Searching Domain
(defun make-maze-ops (pair)
  "Make maze ops in both directions."
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Make an operator to move between two places in a maze."
  (op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

(defparameter *maze-ops*
  (mappend #'make-maze-ops
     '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
       (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
       (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))


;; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Section 4.14 - The Blocks World Domain
(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (move-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops))))
    ops))

(defun move-op (a b c)
  "Make an operator to move A from B to C."
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
    `((,a on ,c) (space on ,b))))
    
;; (gps '((a on b) (b on c) (c on table) (space on a) (space on table))
;;	      '((b on a) (c on b)))


