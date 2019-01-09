;; -*- coding: utf-8 -*-
;;; pl.lisp Time-stamp: <2019-01-09 21:25:23 user> (incremental autotitle)


;; コンパイル必須！
;; 関数search-rulesは、末尾再帰最適化によるループ構造を必要とする



;;; -*- Mode: LISP; Syntax: Common-lisp; Package: gambol; Lowercase: Yes -*-
;;;
;;; See README for copyright information about this modification of FROLIC.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         prolog.l
; Description:  prolog-style search procedure with continuations
; Author:       Jed Krohnfeldt / Craig Steury, University of Utah
; Created:      5-Aug-86
; Language:     Lisp
; Package:      FROLIC
;
; (c) Copyright 1986, 1987, University of Utah, all rights reserved.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This program combines propositional search and unification to achieve a
; predicate-calculus search capability as in Prolog.
; This version of the program implements backtracking and the cut operator
; using a tail-recursive algorithm, implemented with continuations.
; Continuations capture the current state of a computation in such a form
; that it can be restarted at a later time, such as for backtracking to 
; Prolog choice points.
;
; Prolog rules are created with:  (*- head body body ...) - body may be empty
; Prolog queries are posed with:  (?- goal goal ...)
;
; * logical variables are preceeded with a question mark (?)
; * a lisp predicate in the body of a rule is wrapped in (lop (pred))
;   a lisp predicate succeeds if it returns non-nil, and fails otherwise
; * logical variables may be used in lisp predicates provided they are bound
;   by a prolog clause prior to their use - 
;          example (?- (foo ?x) (lop (equal ?x 2)))
; * use pl-solve-one, pl-solve-next, pl-solve-rest and pl-solve-all to
;   return prolog bindings to lisp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Gambol

(oleo.base:oleo-base-header)

(declaim (optimize (speed 3)
                   (compilation-speed 0)))


(defpackage :oleo.prolog.core
  (:use :cl :oleo.base :cl-cont)
  ;(:nicknames :prolog-core)
  ;(:use :common-lisp :common :cl-cont :anaphora :alexandria :j-hash-table :j-lazylist)
  (:export 

   #:pl-filter

   #:*- #:*--
   #:pl-clear
   #:pl-assert
   #:pl-asserta
   #:pl-retract

   #:pl-circulate

   #:define-pl-macro
   #:pl-genvar

   #:!- #:!--

   #:pl-exec-1 #:pl-exec 
   #:pl-query-1 #:pl-query #:&pl-query 
   #:exec-prolog #:exec-prolog-1
   #:prolog #:prolog-1 #:do-prolog #:do-prolog-1 #:do-prolog-collect
   #:parallel-prolog #:parallel-prolog-1 
   #:do-parallel-prolog #:do-parallel-prolog-1 #:do-parallel-prolog-collect
   
   #:&prolog #:&do-prolog-collect
   #:&parallel-prolog #:&do-parallel-prolog-collect
   ))

(in-package :oleo.prolog.core)

;(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
 
(defun pl-filter (x)
  (if x x '*IMPOSSIBLE*))

;(include :name-hash-table :lazy-list)

(defparameter *print* nil)

;@eval-when-compile
(defmacro show (x) `(if *print* (print ,x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *the-root-continuation* nil)
(defvar *max-tail-call* 2)
(defvar *max-tail-count* nil)
(defmacro do-root ((&key (max *max-tail-call*))  &body body)
  `(let ((*the-root-continuation* nil)
         (*max-tail-call* ,max)
         (*max-tail-count* ,max))
     (let ((r ,@body))
       (show 'entered)
       (loop (if *the-root-continuation*
               (let ((f *the-root-continuation*))
                 (show 'return-to-root!)                 
                 (setq *the-root-continuation* nil
                       *max-tail-count* ,max
                       r (funcall f)))
               (return r))))))

;@eval-when-compile
(defmacro root-tail (&body body)
  `(cond 
     ((eql 0 (decf *max-tail-count*))
       (setq *the-root-continuation* (lambda () ,@body)))
     (t ,@body)))

;@eval-when-compile
(defmacro root-tail-forced (&body body)
  `(setq *the-root-continuation* (lambda () ,@body)))

;@eval-when-compile
(defmacro root-tail-counted (count &body body)
  `(cond 
     ((>= 0 (setq *max-tail-count* (- *max-tail-count* ,count)))
       (setq *the-root-continuation* (lambda () ,@body)))
     (t ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;@eval-when-compile
(defmacro dbg-chk (msg code)
  `(prog2 (show ,msg)
    ,code
    (show "DONE!")))

(setf *print-circle* t)
(proclaim '(optimize (speed 3)))

;;; Constants, really.
(define-symbol-macro **IMPOSSIBLE** 'no)  
(define-symbol-macro **SOLVED** 'yes)

;(define-symbol-macro **INTERNAL-LEVEL** 'jpl-internal-level)
;(define-symbol-macro **CUT-INTERNAL-LEVEL** 'jpl-internal-cut-level)

;(defvar *impossible*          'no "make impossible look nice")
;(defvar *solved*             'yes "make solved look nice")


;;; Controllable/accessible by the user.
(defvar *tracing*             nil "if t, tracing is turned on")
; (defvar *lips*                  0 "logical inferences per second")

;;; Altered in the course of finding solutions.
(defvar *auto-backtrack* nil "return all solutions if true")

(defvar *last-continuation*   );nil "saved state of the system")
(defvar *trail*               );nil "the trail, for backtracking")
(defvar *x-env*               );nil "env for goals")
(defvar *y-env*               );nil "env for rules")
(defvar *top-level-envs*      );nil "saves top-level environments")
(defvar *top-level-vars*      );nil "saves top-level variable names")

(defvar *num-slots*           ); -1 "number of logical variables in a query")
(defvar *prolog-rules*  (make-name-hash-table) "hash table for prolog rule heads")
(defvar *prolog-strict-rules*  (make-hash-table :test 'eq) "hash table for strict prolog rule heads")
(defvar *prolog-local-rules*  (make-hash-table :test 'eq) "hash table for local prolog rule heads")
(defvar *prolog-general-rules*  (make-hash-table :test 'equalp) "hash table for general prolog rule heads")

;; 以下は規定値（定数扱い）
(defvar *returns-binding-list* t)
(defvar *show-bindings* t)
(defvar *retract-mode-p* nil)


;@eval-when-compile
(defmacro with-empty-prolog-environment (&body body)
  `(let (*last-continuation*
         *trail*
         *x-env*
         *y-env*
         *top-level-envs*
         *top-level-vars*
         *prolog-local-rules*)
    ,@body))

;; HACK
(defun reset-rulebase ()
  (setq *last-continuation* nil
        *trail* nil
        *x-env* nil 
        *y-env* nil 
        *top-level-envs* nil
        *top-level-vars* nil
        *prolog-local-rules* nil))

(defun make-rulebase ()
  (list nil nil nil nil nil nil nil))

(defun current-rulebase ()
  (list *last-continuation* *trail*
        *x-env* *y-env* *top-level-envs* *top-level-vars*
        *prolog-local-rules*))

;@eval-when-compile
(defmacro with-rulebase (rulebase &body body)
  `(destructuring-bind (*last-continuation*
                        *trail* *x-env* *y-env*
                        *top-level-envs* *top-level-vars*
                        *prolog-local-rules*) ,rulebase
     ,@body))


;; rule selector functions
;@eval-when-compile
(defmacro head (rule)
  `(car ,rule))

;@eval-when-compile
(defmacro body (rule)
  `(cdr ,rule))

;; HACK [2017-10-11]
;; (defmacro functor (term)
;;   `(cond ((consp ,term) (car ,term))
;;          ((vectorp ,term) (svref ,term 1))
;;          (t ,term)))
;@eval-when-compile
(defmacro functor (term)
  (let ((tmp (gensym)))
    `(let ((,tmp ,term))
       (cond ((consp ,tmp) (car ,tmp))
             ((vectorp ,tmp) (svref ,tmp 1))
             (t ,tmp)))))

;; HACK [2017-10-11]
;@eval-when-compile
(defmacro set-functor! (term val)
  (let ((tmp (gensym)))
    `(let ((,tmp ,term))
       (cond ((consp ,tmp) (setf (car ,tmp) ,val))
             ((vectorp ,tmp) (setf (svref ,tmp 1) ,val))
             (t (setf ,term ,val))))))

;@eval-when-compile
(defmacro principal-functor (rule)
  `(functor (head ,rule)))


;@eval-when-compile
(defmacro mol-skel (molecule)
  `(svref ,molecule 1))

;@eval-when-compile
(defmacro mol-env (molecule)
  `(svref ,molecule 2))

;@eval-when-compile
(defmacro rule-head (molecule)
  `(head (mol-skel ,molecule)))

;@eval-when-compile
(defmacro rule-body (molecule)
  `(body (mol-skel ,molecule)))

;@eval-when-compile
(defmacro rule-env (molecule)
  `(mol-env ,molecule))


;@eval-when-compile
(defmacro goal-p (goal) `(consp ,goal))
;@eval-when-compile
(defmacro make-goal (body env) `(cons ,body ,env))
;@eval-when-compile
(defmacro copy-goal (goal) `(let ((src ,goal)) (cons (car src) (cdr src))))
;@eval-when-compile
(defmacro goal-env (goal)
  `(cdr ,goal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Contunuations (vector version - faster than defstructs).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;@eval-when-compile
(defmacro make-cont (goals rules level back trail garbage envtag)
  `(vector ,goals ,rules ,level ,back ,trail ,garbage ,envtag))

;@eval-when-compile
(defmacro cont-goals (cont)
  `(svref ,cont 0))

;@eval-when-compile
(defmacro cont-rules (cont)
  `(svref ,cont 1))

;@eval-when-compile
(defmacro cont-level (cont)
  `(svref ,cont 2))

;@eval-when-compile
(defmacro cont-back (cont)
  `(svref ,cont 3))

;@eval-when-compile
(defmacro cont-trail (cont)
  `(svref ,cont 4))

;@eval-when-compile
(defmacro cont-garbage (cont)
  `(svref ,cont 5))

;@eval-when-compile
(defmacro cont-envtag (cont)
  `(svref ,cont 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Molecules - a molecule consists of a skeleton and an environment
; occurences of logical variables in the skeleton point to the environment
; (vector version - faster than defstructs).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;@eval-when-compile
(defmacro make-molecule (skel env)
  `(progn
    (show 'mol)
    (vector 'molecule ,skel ,env)))

;@eval-when-compile
(defmacro <new-goals> (env rule-body)
  `(do ((goals ,rule-body (cdr goals))
       (env ,env)
       (result nil))
      ((null goals) 
       '(when nil result 
         (show result)
         (show (list 'new-goals (length result))))
       ;(show (list 'new-goals molecule result)) 
       (nreverse result))
    ;;(show 'new-goals)
    (push (make-goal (first goals)
                     env)
          result)))

(defun new-goals (molecule)
  (<new-goals> (rule-env molecule) (rule-body molecule)))


;; todo

;@eval-when-compile
(defmacro molecule-p (x)
  `(let ((x ,x))
    (and (simple-vector-p x)
         (eq (svref x 0) 'molecule))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Random macros.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Take bindings apart.
;@eval-when-compile
(defmacro lhs (binding)
  `(car ,binding))

;@eval-when-compile
(defmacro rhs (binding)
  `(cdr ,binding))

;; Predicates for variables, atoms, and failure conditions.
;@eval-when-compile
(defmacro var-name? (x)
  `(char= #\? (char (symbol-name ,x) 0)))

;; Test for anonymous logical variable - assumes logical variable

;@eval-when-compile
(defmacro anon-var-name? (x)
  `(string= '? ,x))


;; A var looks like (:*var* name index env) where name is the logical variable
;; name, and index is the index of the variable's value in the environment
;; (vars are contained in skeletons).

;;;;@eval-when-compile
(defmacro var? (x)
  `(let ((x ,x))
    (and (consp x) (eq (car x) ':*var*))))

;;;;@eval-when-compile
(defmacro var-name (x)
  `(cadr ,x))

;;;;@eval-when-compile
(defmacro var-index (x)
  `(caddr ,x))

;;;;@eval-when-compile
(defmacro var-env (x)
  `(cadddr ,x))


;;;;@eval-when-compile
(defmacro anon-var? (x)
  `(let ((x (var-name ,x)))
    (anon-var-name? x)))

;;;;@eval-when-compile
(defmacro lookup-var (var env)
  `(svref ,env (var-index ,var)))

;;;;@eval-when-compile
(defmacro make-empty-environment (size)
  `(make-array ,size :initial-element '*undefined*))

;;;;@eval-when-compile
(defmacro impossible? (env)
  `(eq ,env **IMPOSSIBLE**))

;(defmacro cut? (goal)
;  `(and (molecule-p ,goal) (string= (functor (mol-skel ,goal)) 'cut)))

;(defmacro unify? (goal)
;  `(and (molecule-p ,goal) (string= (functor (mol-skel ,goal)) '=)))


;@eval-when-compile
(defmacro pl-bound? (x)
  `(not (eq ,x '*undefined*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Binding and the trail.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 頭部マッチングの際に、その環境が入る
;; 頭部マッチは基本的にuntrailの必要がないため、その識別に使う
(defvar *header-environment* nil)

;; Bind logical variable x to y and push it on the trail.
;@eval-when-compile
(defmacro pl-bind (x x-env y y-env)
  `(let ((x ,x)
         (x-env ,x-env)
         (y ,y)
         (y-env ,y-env))
    (or (anon-var? x)
        (setf (lookup-var x x-env)
                (cond ((molecule-p y)
                        (error "pl-bind ~D , ~D" y y-env))
                      ((or (consp y) 
                           (simple-vector-p y))
                        (push (cons x-env (var-index x)) *trail*)
                        (make-molecule-from-pool y y-env))
                      (t
                        (unless (eq *header-environment*  x-env)
                          (push (cons x-env (var-index x)) *trail*))
                        y))))))


;; Undo the trail bindings back to the last choice point (mark).
;@eval-when-compile
(defmacro unbind-var (binding)
  `(let ((binding ,binding))
    (let ((x (svref (car binding) (cdr binding))))
      (when (molecule-p x)
        (unget-molecule x)))
    (setf (svref (car binding) (cdr binding)) '*undefined*)))

;@eval-when-compile
(defmacro untrail (mark)
  `(let ((mark ,mark))
    (loop
      (if (eq *trail* mark)
        (return)
        (unbind-var (pop *trail*))))))


;@eval-when-compile
(defmacro goal-body (goal)
  `(car ,goal))


;@eval-when-compile
(defmacro goal-functor (goal)
  `(let ((goal ,goal))
    (if goal 
      (if (consp goal) 
        (functor (goal-body goal))
        (functor (mol-skel goal))))))

;@eval-when-compile
(defmacro set-goal-functor! (goal f)
  `(let ((goal ,goal))
    (if goal 
      (if (consp goal) 
;        (setf (functor (goal-body goal)) ,f)
;        (setf (functor (mol-skel goal)) ,f)))))
        (set-functor! (goal-body goal) ,f)
        (set-functor (mol-skel goal) ,f)))))
        
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Hooks to common lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;@eval-when-compile
(defmacro lisp-query? (goal)
  `(let ((goal ,goal))
    (and (consp goal) (eql (car goal) :lop))))


;; Scan the form, replacing all logical variables with their values in the
;; given environment.  The optional variable "query" is true if we are
;; expanding in order to print the top level query solution.  In this case,
;; we don't want to print the internal representation of logical variables.
(defun expand-logical-vars (exp env &optional (query nil))
  (cond ((null exp) nil)
 ((var? exp)
  (if (anon-var? exp)
    exp ;'?
    ; deref in goal environment
    (let ((val (x-view-var exp env)))
      (if (eq val exp)
        (if query
   ; pretty-print logical vars
   (cadr exp)
   exp)
        ; use new environment
        (expand-logical-vars val *x-env* query)))))
 ((molecule-p exp) (expand-logical-vars (mol-skel exp) (mol-env exp)))
 ((stringp exp) exp)
 ((vectorp exp)
         ;; Added because in debugging mode we get different answers
         ;; than in non debugging mode.
          ;(if query (setq exp (copy-seq exp)))
          (if t (setq exp (copy-seq exp)))
          (dotimes (i (length exp) exp)
            (setf (svref exp i)
                    (expand-logical-vars (svref exp i) env query))))
 ((atom exp) exp)

        ;
        ((eq :quote (car exp))
          (second exp))

        ((eq :lisp (car exp))
          (do-lisp-hook (second exp) env))

 (t (cons (expand-logical-vars (car exp) env query)
   (expand-logical-vars (cdr exp) env query))))

)

;; Execute a lisp hook form and return the environment handles multiple
;; values returned from the Lisp expression.
(defun do-lisp-hook (ARGS env)
  ;(show (mol-skel molecule))
  #{macrolet ((call (n)
                 `(funcall F ,@(mapcar #/`(expand-logical-vars (nth ,_ ARGS) env)
                                       (iota n :start 1)))))
  #{let ((F (ensure-function (expand-logical-vars (car args) env))))
  (case (length args)
    (1 (funcall f))
    (2 (call 1))     (3 (call 2))    (4 (call 3))
    (5 (call 4))     (6 (call 5))    (7 (call 6))
    (8 (call 7))     (9 (call 8))   (10 (call 9))
    (11 (call 10))  (12 (call 11))  (13 (call 12))
    (t (apply f (expand-logical-vars (cdr args) env)))))

;(defun do-lisp-hook (goal)
;  ;(show (mol-skel molecule))
;  (let* ((goal-body (goal-body goal))
;  (env (goal-env goal))
;  (expanded-form (expand-logical-vars (cadr goal-body) env)))
;    (apply (symbol-function (first expanded-form))
;           (rest expanded-form))))




;; The IS clause - unification on variables returned from calls to Lisp.
;; The general form is (is ?v1 ... ?vn (lop (lisp-hook))).
;; Binds the ?vi variables to the values returned from (lisp-hook).
;(defmacro is? (goal)
;  `($ and (molecule-p ,goal) (string= (functor (mol-skel ,goal)) 'is)))


(defun do-is (goal)
  ;(print (last (goal-body goal)))
  #{let* ((goal-body (goal-body goal))
          (env (goal-env goal)) 
          (hook (car (last goal-body)))
          (retvals (multiple-value-list 
                     (do-lisp-hook hook env))))
  (if (eq **IMPOSSIBLE** (first retvals))
    **IMPOSSIBLE**
    (dolist (var (cdr goal-body))
      (if (eq hook var) ;; terminator
        (return t)
        (if (eq **IMPOSSIBLE** (unify var env (car retvals) env))
          (return **IMPOSSIBLE**)
          (setq retvals (cdr retvals)))))))


(defun do-is/2 (goal &aux (goal-body (goal-body goal)))
  (when (/= 2 (length (cdr goal-body)))
    (error "@is/2: number of argument must be 2. but ~D" goal-body))
  (let* ((env (goal-env goal))
         (hook (third goal-body))
         (r (do-lisp-hook hook env)))    
    (if (eq **IMPOSSIBLE** r)
      **IMPOSSIBLE**
      (unify (second goal-body) env r env))))

(defun do-is/3 (goal &aux (goal-body (goal-body goal)))
  (when (/= 3 (length (cdr goal-body)))
    (error "@is/3: number of argument must be 3. but ~D" goal-body))
  (let ((env (goal-env goal))
        (hook (fourth goal-body)))

    (multiple-value-bind (r r2) (do-lisp-hook hook env)
      (if (eq **IMPOSSIBLE** r)
        **IMPOSSIBLE**
        (if (eq **IMPOSSIBLE** (unify (second goal-body) env r env))
          **IMPOSSIBLE**
          (unify (third goal-body) env r2 env))))))

(defun do-is/4 (goal &aux (goal-body (goal-body goal)))
  (when (/= 4 (length (cdr goal-body)))
    (error "@is/4: number of argument must be 4. but ~D" goal-body))
  (let ((env (goal-env goal))
        (hook (fifth goal-body)))

    (multiple-value-bind (r r2 r3) (do-lisp-hook hook env)
      (if (eq **IMPOSSIBLE** r)
        **IMPOSSIBLE**
        (if (eq **IMPOSSIBLE** (unify (second goal-body) env r env))
          **IMPOSSIBLE**
          (if (eq **IMPOSSIBLE** (unify (third goal-body) env r2 env))
            **IMPOSSIBLE**
            (unify (fourth goal-body) env r3 env)))))))


;; (defun <do-foreach> (goal)
;;   #{let* ((goal-body (goal-body goal))
;;          (env (goal-env goal))         
;;          (clause (cdr goal-body)))
;;   (when (/= 2 (length clause))
;;     (error "length of FOREACH clause must be 2. but ~D" clause)) 
;;   #{let ((itor  (x-view-var (cadr clause) env)))
;;   (if (itor-end-p itor)
;;     **IMPOSSIBLE**
;;     (unify (car clause) env (itor-get itor) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Continuations - a continuation captures the state of prolog, saving the
; binding environment (env), goal list (goals), rule list (rules),
; unification level (level), the current continuation or choice point (back)
; and the goal trace (trace) for debugging purposes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Predicates for dealing with functors.

(EVAL-WHEN (:COMPILE-TOPLEVEL)
(defmacro ruleroot-p (x) (list 'vectorp x))
(defmacro make-ruleroot (functor) (list 'vector functor nil t nil nil))
(defmacro ruleroot-functor (x) `(svref ,x 0))
(defmacro ruleroot-skels (x) `(svref ,x 1))
(defmacro ruleroot-fixed? (x) `(svref ,x 2))
(defmacro ruleroot-obsolete? (x) `(svref ,x 3))
(defmacro ruleroot-circular? (x) `(svref ,x 4))
)

;; TODO OBSOLETE?
;(defun regular-rule-symbol? (s)
;  (char/= #\$ (char (symbol-name s) 0)))
(defun regular-rule-symbol? (s)
  nil) ;; HACK

;; local-rule-symbol?は実行順序に注意
;; regular-rule-symbol?が否定された後のコンテクストで使用すること(_で始まっていることが前提なので)
;; すなわち、local-rule-symbol?が否定されれば、Strictなルール名ということである
(defun local-rule-symbol? (s)
  (let ((name (symbol-name s)))
    (and (> (length name) 1)
         (char= #\$ (char name 1)))))

;@eval-when-compile
(defmacro gethash-if-possible (key table)
  `(if ,table
    (gethash ,key ,table)
    (values nil nil)))
(defun <get-prolog-rules> (functor)
  (if (symbolp functor) 
    ;; 各述語の実行順には意味があることに注意
    (cond ((regular-rule-symbol? functor) (get-name-hash functor *prolog-rules*))
          ((local-rule-symbol? functor) 
            (gethash-if-possible functor *prolog-local-rules*))
          (t (gethash-if-possible functor *prolog-strict-rules*)))
    (if (consp functor)
      (cond ((eq :local (first functor))
              (gethash-if-possible (second functor) *prolog-local-rules*))
            ((eq :strict (first functor))
              (gethash-if-possible (second functor) *prolog-strict-rules*))
            ((eq :general (first functor))
              (gethash-if-possible (second functor) *prolog-general-rules*))))))

(defun get-prolog-rules (functor)
  (awhen (<get-prolog-rules> functor)
    (ruleroot-skels it)))

;@eval-when-compile
(defmacro puthash-with-checking (tester key table value)
  `(progn 
    (unless ,table (setf ,table (make-hash-table :test ',tester)))
    (setf (gethash ,key ,table) ,value)))

(defun <put-prolog-rules> (functor rules)
  (if (symbolp functor)     
    (cond ((regular-rule-symbol? functor) (set-name-hash functor rules *prolog-rules*))
          ((local-rule-symbol? functor) 
            (puthash-with-checking eq functor *prolog-local-rules* rules))
          (t (puthash-with-checking eq functor *prolog-strict-rules* rules)))
    (if (consp functor)
      (cond ((eq :local (first functor))
              (puthash-with-checking eq (second functor) *prolog-local-rules* rules))
            ((eq :strict (first functor))
              (puthash-with-checking eq (second functor) *prolog-strict-rules* rules))
            ((eq :general (first functor))
              (puthash-with-checking equalp (second functor) *prolog-general-rules* rules))))))

(defun put-prolog-rules (functor rules)
  (let ((ruleroot (aif (<get-prolog-rules> functor)
                       it
                       (make-ruleroot functor))))
    (setf (ruleroot-skels ruleroot) rules)
    (<put-prolog-rules> functor ruleroot)))


;@eval-when-compile
(defmacro remhash-if-possible (key table)
  `(if ,table
    (remhash ,key ,table)
    (values nil nil)))
(defun <remove-prolog-rules> (functor)
  (if (symbolp functor)     
    (cond ((regular-rule-symbol? functor) (rem-name-hash functor *prolog-rules*))
          ((local-rule-symbol? functor) 
            (remhash-if-possible functor *prolog-local-rules*))
          (t (remhash-if-possible functor *prolog-strict-rules*)))    
    (if (consp functor)
      (cond ((eq :local (first functor))
              (remhash-if-possible (second functor) *prolog-local-rules*))
            ((eq :strict (first functor))
              (remhash-if-possible (second functor) *prolog-strict-rules*))
            ((eq :general (first functor))
              (remhash-if-possible (second functor) *prolog-general-rules*))))))

(defun remove-prolog-rules (functor)
  (awhen (<get-prolog-rules> functor)
    (setf (ruleroot-obsolete? it) t)
    (<remove-prolog-rules> functor)))



;@eval-when-compile
(defmacro all-prolog-rules ()           ; nothing calls this! WSA 2009dec2
  `(let ((result nil))
    (map-name-hash #'(lambda (key val)
                       (declare (ignore key))
                       (setf result (append val result)))
     *prolog-rules*)
     result))

(defun <make-ruleroot-obsoleted> (key ruleroot)
  (declare (ignore key))
  (setf (ruleroot-obsolete? ruleroot) t))

;@eval-when-compile
(defmacro clear-standard-hash (table)
  `(maphash #'<make-ruleroot-obsoleted> ,table))

(defun remove-all-prolog-rules ()
  (progn
    (map-name-hash #'<make-ruleroot-obsoleted> *prolog-rules*)
    (when *prolog-strict-rules*   (clear-standard-hash *prolog-strict-rules*))
    (when *prolog-local-rules*    (clear-standard-hash *prolog-local-rules*))
    (when *prolog-general-rules*  (clear-standard-hash *prolog-general-rules*))))



(defun pl-compile-rule (functor &key (recursive t))
  (dolist (skel (get-prolog-rules functor))
    (when (and (numberp (cdr skel))
               (or (not recursive)
                   (zerop (cdr skel))))
      (let* ((mol (build-molecule skel))
             (new-goals-prototype (new-goals mol)))
        (setf (cdr skel) mol)
        (setf (car skel) (cons (mol-env mol) new-goals-prototype))))))

(defun pl-uncompile-rule (functor)
  (dolist (skel (get-prolog-rules functor))
    (when (molecule-p (cdr skel))
      (setf (car skel) (mol-skel (cdr skel)))
      (setf (cdr skel) (length (mol-env (cdr skel)))))))
      


;;

(defstruct pl-operator)

;; Rule indexing.
(defun index-rule (skeleton num-vars)
  (let ((func (principal-functor skeleton)   ))
    (if t; (symbolp func)
      (add-rule-to-index skeleton func num-vars))))

(defun push-rule (skeleton num-vars)
  (let ((func (principal-functor skeleton)))
    (if t; (symbolp func)
      (push-rule-to-index skeleton func num-vars))))

;
(defun dup-rule (skel)
  (let ((copied (copy-list skel)))
    (setf (car copied) (copy-list (car copied)))
    (setf (caar copied) (make-pl-operator))
      copied))

;; Add a rule to prolog.  Each skeleton is paired with the number of
;; variables in its environment, so new environments can be built easily.
(defun add-rule-to-index (skeleton functor num-vars)
  "add a rule to the end of the database for the functor"
  ;(show skeleton)
  (put-prolog-rules functor (append (get-prolog-rules functor)
        (list (cons (dup-rule skeleton) num-vars)))) 
  skeleton)

(defun push-rule-to-index (skeleton functor num-vars)
  "add a rule to the beginning of the database for the functor"
  (put-prolog-rules functor (cons (cons (dup-rule skeleton) num-vars)
                                  (get-prolog-rules functor)))
  skeleton)

(defun rule-part (rule-pair) (car rule-pair))
(defun num-vars (rule) (cdr rule))

;@eval-when-compile
(defmacro is-var-ignored? (x)
  `(and 
    (not *retract-mode-p*)
    (let ((name (symbol-name ,x)))
      (and (/= 1 (length name))
           (char=  #\? (char name 1))))))

;; Construct environments for top-level goals.
(defun build-molecule (skeleton &aux (n (num-vars skeleton)))
  (make-molecule (rule-part skeleton)
                 (make-empty-environment n)))

(defun make-goals (goals &key require-environment-information)
  #{let ((*num-slots* -1))
  (do ((goal-list goals (cdr goal-list))
       (acc-env nil)
       (result nil))
      ((null goal-list)
       (progn
  (setf *top-level-vars* acc-env)
  (let* ((g-env (make-empty-environment (1+ *num-slots*)))
                (ret  (nreverse
                       (mapcar #'(lambda (x) (cons x g-env))
                               result))))
    (setf *top-level-envs* g-env)
           (if require-environment-information
             (values 
              ret
              g-env
              (mapcan (lambda (x) (unless (is-var-ignored? (car x))
                                    ;(list  (cons (first x) (fourth x)))
                                    (list x)
                                    ))
                      acc-env))
             ret))))
      ;; make goal skeleton
      (let* ((env (list acc-env))
      (skel (calcify (first goal-list) env)))
 (setf acc-env (car env))
 (push skel result))))


;;;;@eval-when-compile
(defmacro pl-search->search-rules (goals back)
  `(let* ((goals ,goals)
         (rule-mol (<rule-mol> goals)))
    (search-rules
     goals
     rule-mol 
     ,back)))

;; Attempt to solve a list of goals with respect to rule-base.
;(defun pl-solve (goals)
;  (let ((*top-level-vars* nil)
;        (*top-level-envs* nil)
;        (*trail* nil))
;    (pl-search->search-rules (make-goals goals) nil)))
(defun pl-solve (goals)
  (pl-search->search-rules (make-goals goals) nil))

(defun <rule-mol> (goals)
  (let* ((functor (goal-functor (first goals)))
         (ruleroot
           (if (ruleroot-p functor)
             functor
             (when functor 
               (if (var? functor) 
                 ;; ルール名が変数の場合（動的に解決する必要がある）
                 ;; ex) (?op ...) 
                 (<get-prolog-rules> 
                  (expand-logical-vars functor (goal-env (first goals))))
                 ;; 通常のルール名が与えられた場合
                 (awhen (<get-prolog-rules> functor)
                   (set-goal-functor! (first goals) it)))))))
    (cond ((null ruleroot) nil)
          ((ruleroot-obsolete? ruleroot)
            ;; ルールがOBSOLETEになったので読みなおす
            ;; ※　ここには、ルール名が変数である場合には到達しない　※
            (show 'OBSOLETE)
            (set-goal-functor! (first goals) (ruleroot-functor ruleroot))
            (<rule-mol> goals))
          ((ruleroot-fixed? ruleroot) (ruleroot-skels ruleroot))
          (t (copy-list (ruleroot-skels ruleroot))))))

;hack

;@eval-when-compile
(defmacro succeed-continue->search-rules (goals back)
  `(let ((goals ,goals))
    (pl-search->search-rules goals ,back)))





(defun <helper/continue-on> (cont)
  (if cont
    (if (cont-goals cont)
      (prog1 
        cont
        ;(show 'opklok)
        ;(when (vectorp (cont-trail cont)) (show 'huuuu))
        (untrail (cont-trail cont)))
      (let ((back (cont-back cont)))
        (unget-cont-to-pool cont)
        (<helper/continue-on> back)))
    (progn 
      (untrail nil) 
      nil)))


;;;;@eval-when-compile
(defmacro continue-on->search-rules (cont &optional (filter 'progn))
  `(let ((cont (<helper/continue-on> ,cont)))
    (if cont
       (let ((goals (cont-goals cont))
             (rules (cont-rules cont))
             (back (cont-back cont)))
         (unget-cont-to-pool cont)
         (,filter
          (search-rules goals ;(without-call/cc (cont-goals cont))
                        rules ;(without-call/cc (cont-rules cont))
                        back  ;(without-call/cc (cont-back cont))
                        )))
      **IMPOSSIBLE**)))

;;;;@eval-when-compile
(defmacro fail-continue->search-rules (back)
  `(continue-on->search-rules ,back))
    
;; Success - report solutions to user.

;;;;@eval-when-compile
(defmacro succeed->search-rules (back)
  `(let ((back ,back))
    (setf *last-continuation* back)
    (when (and *auto-backtrack*
               *show-bindings*)
      (show-bindings *top-level-vars*
                     *top-level-envs*))

    (if *auto-backtrack*
      (continue-on->search-rules back)
      (if *returns-binding-list*
        (build-binding-list *top-level-vars*
                            *top-level-envs*)
        t))))

;@eval-when-compile
(defmacro check (command argnum)
  `(if (= ,argnum (length (cdr (goal-body goal))))
    t
    (error "~D : number of arguments must be ~D" ,command ,argnum)))



(define-symbol-macro VECLEN 4)
(let ((pool (make-array 128 :fill-pointer 0 :adjustable t)))
  (defun get-cont-from-pool ()
    (cond ((eql 0 (length pool)) 
            (show 'new-cont-from-pool)
            (make-array 7))
          (t 
            ;(show 'pooled-cont!!)
            (vector-pop pool))))

  ;; OBSOLETE
  (defun unget-all-conts-to-pool-recursively (cont)
    (let ((back (cont-back cont)))
      (fill cont nil) ;; プールする前に内容をクリア
      (vector-push cont pool)
      (when back (unget-cont-to-pool back))))

  (defun unget-cont-to-pool (cont 
                             &optional 
                             (allow-garbage-collect t))
    ;(show 'unget-cont)
    (let ((garbage (cont-garbage cont)))
      (when (and garbage
                 allow-garbage-collect)
        (unget-environment garbage)))
    (fill cont nil) ;; プールする前に内容をクリア
    (vector-push cont pool))

  (defun make-cont-from-pool (goals rules level back trail garbage envtag)
    (cond  ((eql 0 (length pool))
             (show 'new-cont)
             (make-cont goals rules level back trail garbage envtag))
           (t
             ;(show 'pooled-cont)
             (let ((cont (vector-pop pool)))
               (setf (svref cont 0) goals
                     (svref cont 1) rules
                     (svref cont 2) level
                     (svref cont 3) back
                     (svref cont 4) trail
                     (svref cont 5) garbage
                     (svref cont 6) envtag
                     )
               cont))))
  )


(let ((pool (make-array 64 :fill-pointer 0 :adjustable t)))
  (defun unget-molecule (mol)
    '(fill mol nil :start 1)
    (vector-push mol pool))
  (defun make-molecule-from-pool (skel env)
    (if (eql 0 (length pool))
      (let ((mol (make-molecule skel env)))
        ;(setf (svref mol 0) 'pooled)
        mol)
      (let ((mol (vector-pop pool)))
        ;(show 'mol-from-pool)
        ;(show mol)
        ;(setf (svref mol 0) molecule)
        (setf (svref mol 1) skel
              (svref mol 2) env)
        mol))))

(let* ((len 16)
       (arrs (make-array len)))
  (flet ((new (n) (make-array n :fill-pointer 0 :adjustable t)))
    (setf (svref arrs 0) (new 32))
    (setf (svref arrs 1) (new 16))
    (setf (svref arrs 2) (new 8))
    (setf (svref arrs 3) (new 8))
    (setf (svref arrs 4) (new 4))
    (do ((i 5 (1+ i)))
        ((>= i len) arrs)
      (setf (svref arrs i) (new 2))))
  
  (defun get-environment (i)
    (declare (fixnum i))
    (cond ((eql 0 (length (svref arrs i)))
            (show 'new-env)
            (make-empty-environment (* (1+ i) VECLEN)))
          (t (vector-pop (svref arrs i)))))
  (defun unget-environment (env)
    (when env
      ;(show 'unget)
      (vector-push (fill env '*undefined*)
                   (svref arrs
                          (floor (/ (1- (length env)) VECLEN))))))
  (defun cur-arrs () arrs))


;;環境が無い場合に、インクリメントしながらタグとして使われるダミーの環境
(defvar *tagenv-tmp-counter* 0) 

;; goalは必ずシンボルで与えられること
;@eval-when-compile
(defmacro match-rule-head->search-rules (goal goals pending-rules back)

  (flet ((final (garbage? the-rule environment new-goals)
               `(unless (impossible? (let ((*header-environment* ,environment)) 
                                       (unify goal-body ; (goal-body goal) 
                                              goal-env ;(goal-env goal)
                                              (car ,the-rule)
                                              ,environment)))
                 ;(show ,garbage?)

                 (let*  (;(matched-rule-mol ,matched)
                        ;(new-goals (new-goals matched-rule-mol))
                        ;(new-goals ,matched)
                        (new-goals ,new-goals)
                        ;(goals (nconc ,new-goals (cons '*level-tag* (rest goals))))
                        ;(arg-2 matched-rule-mol)
                        (back (make-cont-from-pool 
                               goals      ; goals
                               (rest rules)     ; rules
                               nil ;**INTERNAL-LEVEL** ;level      ; level
                               back      ; back
                               old-trail     ; trail
                               ,(when garbage? 
                                      `(when (vectorp ,environment) ,environment))           ; garbage
                               ,environment

                               )))  
                   
                   ;(if new-goals (show 'empty) (show 'not-empty))
                   (return 
                     (if new-goals
                       (pl-search->search-rules (nconc new-goals (rest goals)) 
                                                back)
                       (pl-search->search-rules (rest goals) 
                                                back)))
                   
                   ))))

  `(let (;(goal ,goal)
         (goal-body (goal-body ,goal))
         (goal-env (goal-env ,goal))
        (goals ,goals)
        (pending-rules ,pending-rules)
        (back ,back))


  (do ((rules pending-rules (rest rules))
       (old-trail *trail*)
       (env nil)
       (env-len 0))
      ((null rules) 
       (when env
         ;; 環境をプールに格納
         (unget-environment env))
       (fail-continue->search-rules back))
    
    ;(show goal)
    (let* ((cur (first rules))
           (car-part (car cur)) 
           (cdr-part (cdr cur)))
      (if (integerp cdr-part)
        (let ((num-vars cdr-part)
              (rule-part car-part))
          ;; 必要なら変数ベクタを新たに確保
          ;; 足りているなら必要な要素だけ初期化
          (if (> num-vars env-len)
            ;; 変数ベクタはVECLENの整数倍のサイズになるように確保
            (let ((d (floor (/ (1- num-vars) VECLEN))))
              (when env
                ;; 以前の環境があればプールに格納
                ;(show (list 'alloc num-vars env-len))
                (unget-environment env))
              (setq env-len (* (1+ d) VECLEN))
              ;;(setq env (make-array env-len :initial-element '*undefined*))
              (setq env (get-environment d))
              )
            (fill env '*undefined* :end num-vars))
          
          (let ((the-env (if (eql 0 num-vars) 
                           (incf *tagenv-tmp-counter*) 
                           env)))
            ,(final t
                    'rule-part 
                    'the-env
                    '(<new-goals> the-env (body rule-part))))
          ;;'(<new-goals> env (body rule-part) make-molecule-from-pool)))
          )
        ;; コンパイルされたルールの場合
        (let ((static-env (car car-part))
              (new-goals-prototype (cdr car-part)))
          (when static-env (fill static-env '*undefined*))
          ,(final nil 
                 '(mol-skel cdr-part) 
                 'static-env 
                 '(copy-list new-goals-prototype)))))))))


(defun do-mark (back user-defined-target-mark)
  (make-cont-from-pool nil 
                       nil 
                       user-defined-target-mark 
                       back 
                       (when back (cont-trail back))
                       nil 
                       nil))

;; TODO WARP機能　あとで実装
;; Perform a cut operation.
;; ユーザが明示的にマークしたポイントへのFailの準備
;; 引数なし(:fail)の場合には呼ばれない
(defun prepare-failing (cont user-defined-target-mark)
  (if cont
    (if (eql (cont-level cont) user-defined-target-mark)
      cont
      (let ((back (cont-back cont)))
                                        ;(show 'johjo)
        (unget-cont-to-pool cont)
        (prepare-failing back user-defined-target-mark)))
    (error "prolog: could not (:fail ~D)" user-defined-target-mark)))

(defun do-chop (cont user-defined-target-mark)
  (if cont
    (if (eql (cont-level cont) user-defined-target-mark)
      (prog1 
        (cont-back cont)
        ;(show 'do-chop)
        (unget-cont-to-pool cont nil))
      (let ((back (cont-back cont)))
        (unget-cont-to-pool cont nil)
        (do-chop back user-defined-target-mark)))
    (error "prolog: could not (:chop ~D)" user-defined-target-mark)))


(defun do-cut-by-mark (cont target-mark)
  (if cont
       (let ((cur-mark (cont-level cont)))
         (setf (cont-rules cont) nil)
         (unless (eql cur-mark target-mark)
           (do-cut-by-mark (cont-back cont) target-mark)))
    (error "prolog: could not (:cut ~D)" target-mark)))

;; ターゲットを補足できない場合もあり得る？が、その場合はエラーとする
(defun do-cut-by-envtag (cont target-envtag)
  (when cont
    (setf (cont-rules cont) nil)
    (unless (eq (cont-envtag cont) 
                target-envtag)
      (do-cut-by-envtag (cont-back cont) target-envtag))))

;; ターゲットを補足できない場合もあり得る
;; (ユーザによって意図的に継続が削除された場合など)
(defun do-cut-at-root (cont target-envtag)
  (when cont
    (if (eq (cont-envtag cont) 
            target-envtag)
         (setf (cont-rules cont) nil))
      (do-cut-at-root (cont-back cont) target-envtag)))

(defun do-cut-surface (cont target-envtag)
  (when cont
    (unless (eq (cont-envtag cont) 
                target-envtag)
      (setf (cont-rules cont) nil))
      (do-cut-surface (cont-back cont) target-envtag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Unification functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Explicit call to unify (= lhs rhs) - unify lhs with rhs.
(defmacro exec-unification (goal)
  `(let* ((goal ,goal)
          (goal-body (goal-body goal))
          (env (goal-env goal))
          (lhs (cadr goal-body))
          (rhs (caddr goal-body)))
    (unify (expand-lisp-hooks lhs env) env (expand-lisp-hooks rhs env) env)
    ;(unify lhs env rhs env)
    ))

(defmacro exec-unification-with-expansion (goal)
  `(let* ((goal ,goal)
          (goal-body (goal-body goal))
          (env (goal-env goal))
          (lhs (cadr goal-body))
          (rhs (caddr goal-body)))
    (unify (expand-lisp-hooks lhs env) env (expand-logical-vars rhs env) env)
    ;(unify lhs env rhs env)
    ))
(defmacro exec-unification-with-snapshot (goal)
  `(let* ((goal ,goal)
          (goal-body (goal-body goal))
          (env (goal-env goal))
          (lhs (cadr goal-body))
          (rhs (caddr goal-body)))
    (unify (expand-lisp-hooks lhs env) env (filter-vars! (expand-logical-vars rhs env)) env)
    ;(unify lhs env rhs env)
    ))

;; If term is a lisp-query, it is evaluated and its result returned; if not,
;; it is simply returned.
(defun expand-lisp-hooks (term env)
  (if (lisp-query? term)
      (let ((expanded-form (expand-logical-vars (cadr term) env)))
        (apply (ensure-function (first expanded-form)) (rest expanded-form)))
      term))

;; Dereference to find ultimate binding of a logical variable in the goal
;; environment.
(defun x-view-var (x env)
  (cond ((var? x)
  (if (anon-var? x)
             x
             (let ((val (lookup-var x env)))
               (if (pl-bound? val)
                   (x-view-var val env)
                   x))))
 ((molecule-p x)
  (x-view-var (mol-skel x) (setq *x-env* (mol-env x))))
 (t x)))

(defun y-view-var (y env)
  (cond ((var? y)
  (if (anon-var? y)
             y
             (let ((val (lookup-var y env)))
               (if (pl-bound? val)
                   (y-view-var val env)
                   y))))
 ((molecule-p y)
  (y-view-var (mol-skel y) (setq *y-env* (mol-env y))))
 (t y)))

;; Unify - unification, returns environment in which x and y are unified.
;; Unify sets up environments and trail, and cleans up on failure.
(defun unify (x x-env y y-env)
  (let ((save-trail *trail*) (ans nil))
    (setf *x-env* x-env)     ;goal environment
    (setf *y-env* y-env)     ;rule environment
    (if (impossible? (setf ans (unify1 x y))) (untrail save-trail))
    ans))

;; Unify1 dereferences variables in their environments.
(defun unify1 (x y)
  (unify2 (x-view-var x *x-env*) (y-view-var y *y-env*)))

;; Unify2 is the main unification routine.
(defun unify2 (x y)
  ;#-PCLS
  ;(declare (inline pl-bind))
  (cond ((var? x) (pl-bind x *x-env* y *y-env*))
        ; bind variables if distinct
        ((var? y) (pl-bind y *y-env* x *x-env*))

        
        ;((and (symbolp x) (symbolp y) (equalp (symbol-name x) (symbol-name y)) (show (list x y))) t)

        ((pl-operator-p y) t)
        
        ((stringp x) (if (equalp x y) t **IMPOSSIBLE**))
        ((stringp y) **IMPOSSIBLE**)
        ((vectorp x) (if (not (and (vectorp y)
                                   (eql (length x) (length y))))
                       **IMPOSSIBLE**
                       (dotimes (i (length x) t)
                         (let ((x-env *x-env*)
                               (y-env *y-env*))
                           (when (impossible? (unify1 (svref x i) (svref y i)))
                             (return **IMPOSSIBLE**))
                           (setf *x-env* x-env)
                           (setf *y-env* y-env)))))

        ; unify atoms 
        ((atom x) (if (equalp x y) t **IMPOSSIBLE**))
        ((atom y) **IMPOSSIBLE**)
        ; both terms complex
        ((let ((x-env *x-env*)
        (y-env *y-env*))
    (if (impossible? (unify1 (&car x) (&car y)))
               **IMPOSSIBLE**
               (progn
                 (setf *x-env* x-env)
                 (setf *y-env* y-env)
                 (unify1 (&cdr x) (&cdr y))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Success and failure display functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Build a list of the bindings.
(defun build-binding-list (vars env)
  ;(print (list 'build vars env))
  (let ((result nil))
    (mapc #'(lambda (x)
              ;; ??で始まる変数は無視する
              (unless (is-var-ignored? (car x))
                (push (cons (car x) (expand-logical-vars (cdr x) env t)) result)))
          vars)
    (if result result t)))


;; Show result bindings (bindings of goal variables).
(defun show-bindings (vars envs)
  (let ((bindings (build-binding-list vars envs)))
    (terpri)
    (if (atom bindings)
      (format t "~s~%" **SOLVED**)
      (mapc #'show-one-binding bindings))))

(defun show-one-binding (binding)
  (format t "~s = ~s~%" (lhs binding) (rhs binding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Molecule and skeleton building forms - for rules.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;@eval-when-compile
(defmacro add-new-var (var vars)
  `(append ,vars (list (cons ,var (incf *num-slots*)))))


(defvar *macros* nil)

(defmacro define-pl-macro (name (&rest ll) &body body)
  `(let ((fn (lambda (,@ll) ,@body))
         (slot (assoc ',name *macros*)))
    (if slot
      (setf (cdr slot) fn)
      (push `(,',name . ,fn)  *macros*))
    ',name))

(defun pl-genvar ()
  `(:*var* ,(gensym "?") ,(incf *num-slots*)))

(defun mapform (f xs)
  (cond
    ((null xs) nil)
    ((consp xs)
          (cons (funcall f (car xs)) (mapform f (cdr xs))))
    (t (funcall f xs))))

;;; Calcify returns form with all logical variables replaced with a vectorized
;;; representation.  env is destructively modified.  It is expected to be input
;;; in the form ((env-a-list)).  Note that the input environment is not
;;; necessarily nil, as in the case when a series of input goals are calcified
;;; and must share an environment.


(defun calcify (form alist)
  (cond 
    ((null form) nil)
    ((symbolp form)
      (if (var-name? form)
        (if (anon-var-name? form)
          '(:*var* ? -1)
          (let ((slot (assoc form (car alist))))
            (if (not slot)
              (let ((nv `(,form . (:*var* ,form ,(incf *num-slots*)))))
                                        ; destructively modify alist
                (push nv (car alist))
                (setf slot nv)))
                                        ; return new rep for var
            (cdr slot)))
        form))
    ((stringp form) form)
    ((vectorp form)
      ;;(dotimes (i (length form) form)
      ;;  (setf (svref form i) (calcify (svref form i) alist)))
      (map 'vector #/(calcify _ alist) form))
    ((atom form) form)
    ((eql ':*var* (car form)) form)
                                        ;((eql 'quote (car form)) (second form))
    ((and (symbolp (car form))
          (assoc (car form) *macros*))
      (calcify (apply (cdr (assoc (car form) *macros*)) 
                      (cdr form))
               alist))
    ;;(t (cons (calcify (car form) alist)
    ;;  (calcify (cdr form) alist)))
    
                                        ;((progn (show form) nil) )
    
    ;; ((PROGN (when (symbolp (car form)) (setf (car form) (intern (symbol-name (car form))))) nil))
    
    (t (mapform (lambda (subform) (calcify subform alist))
                form))
    ;;         (t (let ((result (mapcar (lambda (subform) (calcify subform alist))
    ;;                                  form)))
    ;;              (when (cdr (last form))  ;; (... . ?x) 
    ;;                (setf (cdr (last form))
    ;;                        (calcify (cdr (last (form))) alist)))
    ;;              result))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Assert and solve - lisp calls to prolog.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add a rule to the database.

;; Destructively modify a rule to produce a skeleton rule.  Each logical
;; variable is converted to a pointer into the environment.
;; Calcify returns the environment structure created for the rule.


(defun pl-assert (rule)
  "add a rule to the end of the database for this functor"
  (let ((env (list nil)))
    (let ((*num-slots* -1))
     (index-rule   (calcify rule env)  (1+ *num-slots*))
      rule)))

(defun pl-asserta (rule)
  "add a rule to the beginning of the database for this functor"
  (let ((env (list nil)))
    (let ((*num-slots* -1))
      (push-rule (calcify rule env) (1+ *num-slots*))
      rule)))
  

;; Makes sure lisp gets nil on failure instead of **IMPOSSIBLE**.
;;;;@eval-when-compile
(defmacro filter-no (value)
  `(let ((retval ,value))
     (if (impossible? retval) nil retval)))

;;;;@eval-when-compile
(defun continue-on (cont)
  (continue-on->search-rules cont filter-no))

;; Return the first solution to the query, setting *last-continuation* so
;; that subsequent calls to solve-next can get other solutions - the return
;; value is an environment, an alist with (var . binding) pairs.
(defun pl-solve-one (goals)
  (let ((*auto-backtrack* nil))
    (filter-no (pl-solve goals))))

;; Return the next solution, using *last-continuation* (the continuation
;; from the most recent pl-solve-one or pl-solve-cc) or the optional
;; continuation provided.
(defun pl-solve-next (&optional (cont *last-continuation*))
  (let ((*auto-backtrack* nil))
    (continue-on cont)))

;; Return the rest of the solutions, using *last-continuation* (the
;; continuation from the most recent pl-solve-one or pl-solve-cc) or the
;; optional continuation provided.
(defun pl-solve-rest (&optional (cont *last-continuation*))
  (let ((*auto-backtrack* t))
    (continue-on cont)))

;; Return all solutions to the query - the return value is a list of
;; environments (env env ...) where each environment is a
;; ((var . binding)...) alist.
(defun pl-solve-all (goals)
  (let ((*auto-backtrack* t))
    (filter-no (pl-solve goals))))

;;; (do-solve-all (?who) '((mortal ?who))
;;;   (show ?who))
(defmacro do-solve-all ((&rest vars) goals &body body)
  (let* ((env (gensym "ENV"))
         (binding-list (loop for var in vars
                             collecting (list var `(cdr (assoc ',var ,env))))))
    `(dolist (,env (pl-solve-all ,goals))
       (let ,binding-list
         ,@body))))

;;; "I didn't mean that."  The next three functions handle the delicate
;;; matter of rule retraction.  It is at least as robust as some commercial
;;; prologs, in that this doesn't fiddle with any facts currently in the
;;; backtracking chain.  It's not smart enough to retract rules, only facts.

;;; Take the results of PL-SOLVE-ONE and produce a version of the query
;;; filling in the logical variables with the association list.
(defun <subst-alist> (alist x)
  (cond ((consp x)
          (mapcar #/(<subst-alist> alist _) x))
        ((simple-vector-p x)
          (map 'vector #/(<subst-alist> alist _) x))
        ((and (symbolp x)
              (var-name? x))
          #{let ((pair (assoc x alist)))
          (cond ((null pair) x)
                ((anon-var-name? x) ;; ワイルドカード[?]の場合
                  (setf (car pair) nil) ;; 一回現れる毎に用済みにする
                  (cdr pair))
                (t (cdr pair))))
        (t x)))

;; Change (:*var* ?x 0) to ?x for rule printing.
;; 注意！ 破壊的である
(defun filter-vars (exp)
  (cond ((null exp) nil)
 ((var? exp) (cadr exp))
 ((stringp exp) exp)
 ((vectorp exp)
         (setf exp (copy-seq exp))
  (dotimes (i (length exp) exp)
    (setf (svref exp i) (filter-vars (svref exp i)))))
 ((atom exp) exp)
 (t (cons (filter-vars (car exp))
   (filter-vars (cdr exp))))))

;; Change (:*var* ?x 0) to ?x for rule printing.
;; 破壊版
(defun filter-vars! (exp)
  (cond ((null exp) nil)
 ((var? exp) (cadr exp))
 ((stringp exp) exp)
 ((vectorp exp)
  (dotimes (i (length exp) exp)
    (setf (svref exp i) (filter-vars (svref exp i)))))
 ((atom exp) exp)
 (t (setf (car exp) (filter-vars (car exp))
          (cdr exp) (filter-vars (cdr exp)))
   exp)))


(defun <retract-fact> (functor fact-literal)
  ;; make variables look good and yank out arity data to help REMOVE-IF
  #{labels ((rule-filter (rule)
               (first (filter-vars rule))))
  #{let* ((prev-ruleroot (<get-prolog-rules> functor))
          (functor-rules (ruleroot-skels prev-ruleroot)))
  (when functor-rules
    (setf (ruleroot-obsolete? prev-ruleroot) t)
    (<remove-prolog-rules> functor)
    (put-prolog-rules functor 
                      (remove-if #'(lambda (r) (and (null (cdr r))
                                                    (equalp (cdar r) fact-literal)))
                                 functor-rules
                                 :key #'rule-filter))))

(defun <retract-facts> (functor fact-literals)
  ;; make variables look good and yank out arity data to help REMOVE-IF
  #{labels ((rule-filter (rule)
               (first (filter-vars rule))))
  #{let* ((prev-ruleroot (<get-prolog-rules> functor))
          (functor-rules (ruleroot-skels prev-ruleroot)))
  (when functor-rules
    (setf (ruleroot-obsolete? prev-ruleroot) t)
    (<remove-prolog-rules> functor)
    (put-prolog-rules functor 
                      (remove-if #'(lambda (r) (and (null (cdr r))
                                                    (member (cdar r) fact-literals :test 'equalp)))
                                 functor-rules
                                 :key #'rule-filter))))

(defun <convert-head-for-anon-var-name> (x)
  (cond ((consp x) (mapcar #'<convert-head-for-anon-var-name> x))
        ((stringp x) x)
        ((vectorp x) (map 'vector #'<convert-head-for-anon-var-name> x))
        ((and (symbolp x) (anon-var-name? x))
          (gensym "?-"))
        (t x)))

(defun <retract-check> (functor)
  (awhen (<get-prolog-rules> functor)
    (when (ruleroot-circular? it)
      (error "cannot RETRACT: ~D is circular" functor))))

;; 事実(fact)の削除 ワイルドカードも使える
;; 複数の事実が削除対象となる際は、まず全ての削除候補を収集し、最後に一括で削除する
(defun pl-retract (goals)
  ;; Paranoia - since retraction will run during on-going solution searcheck mark)))ck mark)))ck mark)))ck mark)))ck mark)))ck mark)))ck mark)))s,
  ;; we have to protect all the specials involved in that search while se do
  ;; a new search for the rule to retract.
  (let* ((*auto-backtrack* nil)
         (*retract-mode-p* t)
         (*last-continuation* nil)
         (*trail* nil)
         (*x-env* nil)
         (*y-env* nil)
         (*top-level-envs* nil)
         (*top-level-vars* nil)
         (new-goals (cons (<convert-head-for-anon-var-name> (car goals))
                          (cdr goals)))
         (head (car new-goals))
         (clause (filter-no (pl-solve new-goals))))
    
    (<retract-check> (first head))

    (cond ((null clause) nil)  ; no match
          ((eq clause t)       ; literal match
           (<retract-fact> (car head) (cdr head))
            t)
          ((listp clause)      ; unified match
            #{let (tmp
                   (head-cdr (cdr head)))
            ;; 削除対象となる事実を収集し…
            (push (<subst-alist> clause head-cdr) tmp)
            (do ((clause (continue-on *last-continuation*) 
                         (continue-on *last-continuation*)))  
                ((null clause)
                 ;; 最後に一括して削除する
                 ;(print tmp)
                 (<retract-facts> (car head) (nreverse tmp)))
              (push (<subst-alist> clause head-cdr) tmp))
            ;(retract-fact (subst-alist clause goals))
            t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; User interaction.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interactive version of assert
;; (used to be called :- but common lisp thinks :- is a keyword).
(defmacro *- (&rest rule)
  `(pl-assert ',rule))

(defmacro *-- (&rest heads)
  `(progn ,@(mapcar #/`(pl-assert '(,_)) heads)))

;; Interactive version of pl-solve-one.

(defmacro ?- (&rest goals)
  `(let((*auto-backtrack* nil))
     (pl-solve ',goals)))

;; Interactive version of pl-solve-all.
(defmacro ??- (&rest goals)
  `(let ((*auto-backtrack* t))
     (pl-solve ',goals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rule and database manipulation and printing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove all rules from the database.
(defun clear-rules (&optional (functors nil))
  (if functors
      (dolist (functor functors)
        (remove-prolog-rules functor))
      ; clear rule index
      (remove-all-prolog-rules))
  t)

(defun pl-clear (functor)
  (remove-prolog-rules functor)
  functor)

(defun pl-circulate (functor)
  (awhen (<get-prolog-rules> functor)
    (unless (ruleroot-circular? it)
      (setf (ruleroot-circular? it) t)
      #{let ((skels (ruleroot-skels it)))
      (setf (cdr (last skels)) skels))
    functor))

(defun depth (cont &optional (acc 0))
  (if cont
    (depth (cont-back cont) (1+ acc))
    acc))
  

;; Attempt to match a goal against rules in the database.
;; hack
;; ここがPrologエンジンの心臓部
;; 末尾再帰によるループ形成が必須であるため、要コンパイル

(defun <empty-var?> (skel env)
  (when (var? skel)
    (let ((x (lookup-var skel env)))
      (or (eq x '*undefined*)
          (and (molecule-p x)
               (<empty-var?> (mol-skel x) (mol-env x)))))))

;(defun <deep-lookup> (skel env)
;  (if (var? skel)
;    (let ((x (lookup-var skel env)))
;      (cond ((molecule-p x)
;              (<deep-lookup> (mol-skel x) (mol-env x)))
;            (t x)))
;    skel))

(defun search-rules (goals rules back &aux (goal (first goals)))
  ;(show (list 'depth (depth back)))
  (symbol-macrolet 
      ((<<fc>> (fail-continue->search-rules back))
       (<<sc>>
           (succeed-continue->search-rules (rest goals) back)
         )
       
       (<<pl-search>> (pl-search->search-rules goals back))
       ;; 以下は終端処理
       (<<succeed>>  (succeed->search-rules back))
       (<<other>> (match-rule-head->search-rules goal goals rules back)))

    
    (if goal ;(vectorp goal) ;(molecule-p goal)
      (let* (
            
            (goal (if (consp goal) 
                    goal
                    (progn 
                      (show 'uhhhhhhhhhhhhhhhhhhhhhhhhhhhhh)
                      (cons (mol-skel goal) (mol-env goal)))))
                    
            
            (x (functor (goal-body goal)))
            ;(_ (print (keywordp x)))
            )

        (if (keywordp x)
          (case x                       
            (:=    (if (impossible? (exec-unification goal)) <<fc>> <<sc>>))
            (:is   (if (impossible? (do-is goal))    <<fc>> <<sc>>))
            (:is/2 (if (impossible? (do-is/2 goal))  <<fc>> <<sc>>))
            (:is/3 (if (impossible? (do-is/3 goal))  <<fc>> <<sc>>))
            (:is/4 (if (impossible? (do-is/4 goal))  <<fc>> <<sc>>))
            (:call
              (setf goals 
                      (cons (make-goal (expand-logical-vars (cdr (goal-body goal)) 
                                                            (goal-env goal))
                                       (goal-env goal))
                            (cdr goals)))
              <<pl-search>>)


            (:cut
              (if (and (cdr (goal-body goal)) 
                       (check x 1))

                ;; (cut lev) 引数ありの場合
                (let ((mark (x-view-var (cadr (goal-body goal)) (goal-env goal))))
                  (do-cut-by-mark back mark))
                  ;; (cut) 引数なしの場合（これが通常）
                  ;;(setq back (do-cut back level)))
                (do-cut-by-envtag back (goal-env goal)))
              <<sc>>)

            (:+
              (let ((sub (<new-goals> (goal-env goal) (cdr (goal-body goal)))))
                (pl-search->search-rules (nconc sub (cdr goals)) back))
              )

            (:cut-root
              (do-cut-at-root back (goal-env goal))
              <<sc>>
              )
            (:cut-surface
              (do-cut-surface back (goal-env goal))
              <<sc>>
              )

            (:fail
              (when (and (cdr (goal-body goal)) 
                       (check x 1))

                ;; (cut lev) 引数ありの場合
                (let ((mark (x-view-var (cadr (goal-body goal)) (goal-env goal))))
                  (setq back (prepare-failing back mark))))
              <<fc>>)

            (:chop
              (when (and (cdr (goal-body goal)) 
                       (check x 1))

                ;; (cut lev) 引数ありの場合
                (let ((mark (x-view-var (cadr (goal-body goal)) (goal-env goal))))
                  (setq back (do-chop back mark))))
              <<sc>>)

            (:mark
              (check x 1)
              (let ((mark (x-view-var (cadr (goal-body goal)) (goal-env goal))))
                (setq back (do-mark back mark)))
              <<sc>>)
            (:remark
              (check x 1)
              (let ((mark (x-view-var (cadr (goal-body goal)) (goal-env goal))))
                (setf (cont-level back) mark))
              <<sc>>)


;;             (:level
;;               (check x 1)
;;               (let ((env (goal-env goal)))
;;                 (if (eq **IMPOSSIBLE**
;;                         (unify level env (x-view-var (cadr (goal-body goal)) env) env))
;;                   <<fc>>
;;                   <<sc>>))
;;               )

            (:read-symbol-value
              (check x 2)
              (let* ((env (goal-env goal))
                     (sym (x-view-var (second (goal-body goal)) env))
                     (result (if (boundp sym) (symbol-value sym) **IMPOSSIBLE**)))
                (if (eq **IMPOSSIBLE**
                        (unify result env (x-view-var (third (goal-body goal)) env) env))
                  <<fc>>
                  <<sc>>))
              )

            (:write-symbol-value
              (check x 2)
              (let* ((env (goal-env goal))
                     (sym (x-view-var (second (goal-body goal)) env)))
                (setf (symbol-value sym) 
                        (x-view-var (third (goal-body goal)) env)))
              <<sc>>)
                                        ; 
            (:asserta
              (check x 1)
              (pl-asserta (list (filter-vars! (expand-logical-vars
                                               (second (goal-body goal))
                                               (goal-env goal)))))
              <<sc>> 
              )
            (:assertz
              (check x 1)
              (pl-assert (list (filter-vars! (expand-logical-vars
                                              (second (goal-body goal))
                                              (goal-env goal)))))
              <<sc>> 
              )
            ;; 
            (:retract
              (check x 1)
              ;; FILTER-VARS because you can retract facts with logical
              ;; variables, though not ??.
              (if (not (pl-retract (list (filter-vars!
                                          (expand-logical-vars
                                           (second (goal-body goal))
                                           (goal-env goal))))))
                <<fc>>                  ; 
                <<sc>> 
                ))
              
            (:lisp (if (do ((env (goal-env goal))
                            (exps (cdr (goal-body goal)) (cdr exps)))
                           ((null exps) t)
                         (when (eq '*IMPOSSIBLE*
                                   (do-lisp-hook (car exps) env))
                           (return nil)))
                     <<sc>>
                     <<fc>>))

            (:lop (if (do ((env (goal-env goal))
                          (exps (cdr (goal-body goal)) (cdr exps)))
                          ((null exps) t)
                        #{let ((result (do-lisp-hook (car exps) env)))
                        (when (or (null result) 
                                  (eq '*IMPOSSIBLE* result))
                          (return nil)))
                    <<sc>>
                    <<fc>>))

            (:undef
              (let* ((skel (goal-body goal))
                     (env (goal-env goal))
                     (lhs (cadr skel)))
                                        
                (setf (lookup-var lhs env) '*undefined*)
                      
                )
              <<sc>>)
            (:nonvar
              (let* ((env (goal-env goal)))
                (if (dolist (mol-skel (cdr (goal-body goal)) t)
                      (when (<empty-var?> mol-skel env)
                        (return nil)))                  
                  <<sc>>
                  <<fc>>)))
            (:any-nonvar
              (let* ((env (goal-env goal)))
                (if (dolist (mol-skel (cdr (goal-body goal)) nil)
                      (when (<empty-var?> mol-skel env)
                        (return t)))                  
                  <<sc>>
                  <<fc>>)))
            (:var
              (let* ((env (goal-env goal)))
                (if (dolist (mol-skel (cdr (goal-body goal)) t)
                      (unless (<empty-var?> mol-skel env)
                        (return nil)))                  
                  <<sc>>
                  <<fc>>)))
            (:any-var
              (let* ((env (goal-env goal)))
                (if (dolist (mol-skel (cdr (goal-body goal)) nil)
                      (unless (<empty-var?> mol-skel env)
                        (return t)))                  
                  <<sc>>
                  <<fc>>)))


            (:*var* 
              ;(show 'yeaaaaaa)
              (let ((var-val (lookup-var (goal-body goal) (goal-env goal))))
                (if (molecule-p var-val)
                  (progn
                    ;; 変数の連鎖がある場合は、効率化のために、ここで解決する
                    (while (var? (mol-skel var-val))
                      (setq var-val (lookup-var (mol-skel var-val) (mol-env var-val)))
                      )
                    (if (molecule-p var-val)
                      (pl-search->search-rules 
                       (cons (make-goal (mol-skel var-val) (mol-env var-val)) 
                             (cdr goals)) 
                       back)
                      <<fc>>
                      ))

                  <<fc>>                
                  )))

            (:expand=
              (check x 2)
              (if (impossible? (exec-unification-with-expansion goal))
                <<fc>>
                <<sc>>)
              )
            (:snapshot=
              (check x 2)
              (if (impossible? (exec-unification-with-snapshot goal))
                <<fc>>
                <<sc>>)
              )

            ;; デバッグプリント機能
            (:debug
              (let* ((env (goal-env goal)))
                (format t "DEBUG:");
                (dolist (mol-skel (cdr (goal-body goal)))
                      (format t "~D ==> ~D " mol-skel (expand-logical-vars mol-skel env)))
                (format t "~%")
                )
              <<sc>>)
                            

            (t
              ;; 未定義のシステム述語
              (error "prolog: unknown system predicate ~D" x))
            )

          <<other>>))
      (progn
        ;(show (list 'test==================================================== goal))
        ;(unless (null goals) (error "ahahman"))
      (if (null goals)
        <<succeed>>
        ;; これはないハズ
        (error "korehanai")
        ;<<other>>
        )
      )
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 以下、ユーティリティ ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pl-exec-1 (goals)
  (let ((*returns-binding-list* nil)
        (*retract-mode-p* nil)
        (*last-continuation* nil)
        (*trail* nil) 
        (*x-env* nil) 
        (*y-env* nil)
        (*top-level-envs* nil) 
        (*prolog-local-rules* nil))
    (filter-no (pl-solve-one goals))))

;; 具体解を必要としないバックトラック実行
(defun pl-exec (goals &optional max)
  (when max (assert (and 'pl-exec (non-negative-integer-p max))))
  #{unless (eq max 0)
  #{let ((*retract-mode-p* nil)
         (*last-continuation* nil)
         (*trail* nil) 
         (*x-env* nil) 
         (*y-env* nil)
         (*top-level-envs* nil) 
         (*prolog-local-rules* nil)
         (cnt (when max 1)))
  (awhen (filter-no (pl-solve-one goals))
    (when (and cnt (>= cnt max))
      (return-from pl-exec t))
    (do ((result (continue-on *last-continuation*)
                 (continue-on *last-continuation*)))
        ((or (null (filter-no result))
             (and cnt (>= (incf cnt) max)))
         t))))

(defmacro !- (&rest goals)
  `(pl-exec ',goals))

(defmacro !-- (&rest heads)
  #{let ((flag (gensym)))
  `(let ((,flag t))
    ,@(mapcar #/`(setq ,flag (and (pl-exec '(,_)) ,flag)) heads)
    ,flag))

(defun pl-query-1 (goals)
  #{let ((*retract-mode-p* nil)
         (*last-continuation* nil)
         (*trail* nil) 
         (*x-env* nil) 
         (*y-env* nil)
         (*top-level-envs* nil) 
         (*prolog-local-rules* nil))
  (filter-no (pl-solve-one goals)))

(defun pl-query (goals &optional max)
  (when max (assert (and 'pl-query (non-negative-integer-p max))))
  #{unless (eq max 0)
  #{let ((*retract-mode-p* nil)
         (*last-continuation* nil)
         (*trail* nil) 
         (*x-env* nil) 
         (*y-env* nil)
         (*top-level-envs* nil) 
         (*prolog-local-rules* nil)
         (cnt (when max 1)))
  (awhen (filter-no (pl-solve-one goals))
    (when (and cnt (>= cnt max))
      (return-from pl-query (list it)))
    (do (tmp
         (result (continue-on *last-continuation*)
                 (continue-on *last-continuation*)))
        ((or (null (filter-no result))
             (progn (push result tmp) 
                    (and cnt (>= (incf cnt) max))))
         (cons it (nreverse tmp))))))

(defun &pl-query (goals &optional max)
  (when max (assert (and '&pl-query (non-negative-integer-p max))))
  #{unless (eq max 0)
  #{let ((*retract-mode-p* nil)
         (*last-continuation* nil)
         (*trail* nil) 
         (*x-env* nil) 
         (*y-env* nil)
         (*top-level-envs* nil) 
         (*prolog-local-rules* nil)
         (cnt (when max 1)))
  (awhen (filter-no (pl-solve-one goals))
    #{let* ((save/last-continuation *last-continuation*)
            (save/trail *trail*)
            (save/x-env *x-env*)
            (save/y-env *y-env*)
            (top-level-envs *top-level-envs*)
            (save/prolog-local-rules *prolog-local-rules*)
            (f (lambda (f)
                 #{when (or (null cnt)
                            (<= (incf cnt) max))
                 #{let ((*retract-mode-p* nil)
                        (*last-continuation* save/last-continuation)
                        (*trail* save/trail)
                        (*x-env* save/x-env)
                        (*y-env* save/y-env)
                        (*top-level-envs* top-level-envs)
                        (*prolog-local-rules* save/prolog-local-rules))
                 (awhen (filter-no (continue-on *last-continuation*))
                   (setq save/last-continuation *last-continuation*
                         save/trail *trail*
                         save/x-env *x-env*
                         save/y-env *y-env*
                         save/prolog-local-rules *prolog-local-rules*)
                   (&cons! it (funcall f f))))))
    (&cons! it (funcall f f))))

;; 残った継続をプールで再利用できるように開放する
;; 途中で計算をやめた場合（残りの計算が残っている場合）にのみ利用する
(defun release-last-continuation-resources ()
  (loop (if *trail*
          (unbind-var (pop *trail*))
          (return)))
  (do ((cont *last-continuation* (prog1 (cont-back cont)
                                   (unget-cont-to-pool cont))))
      ((null cont))))

(defun <is-external-var?> (sym &aux (name (symbol-name sym)))
  (and (/= 1 (length name))
       (char= #\_ (char name 1))))
(defun <is-internal-var?> (sym) (not (<is-external-var?> sym)))
(defun <to-lisp-var> (sym)
  (intern (subseq (symbol-name sym) (if (<is-external-var?> sym) 2 1))
          (symbol-package sym)))
    
;;;;@eval-when-compile
(defmacro <cell-pop> (place)
  (let ((v (gensym)))
  `(let ((,v ,place))
    (setf ,place (cdr ,place))
    (setf (cdr ,v) nil)
    ,v)))

;;;;@eval-when-compile
(defmacro <cell-push> (cell place)
  `(setf 
    (cdr ,cell) ,place
    ,place ,cell))

(defun <clone-goals> (env-len goals)
  (show 'clone-goals)
  (let ((env (make-array env-len)))
    (cons env
          (mapcar (lambda (g)
                    (let ((clone (copy-goal g)))
                      (setf (goal-env clone) env)
                      clone))
                  goals))))

;; unsafe-fast?が真の場合には、リエントラントに呼び出されないことを要求するかわりに
;; 若干効率的になる。ただし、実行速度は殆ど変わらないのでOBSOLETEにする予定である
(defmacro <solve> ((&rest def) f)
  (multiple-value-bind (initial-goals initial-env alist) 
      (make-goals def :require-environment-information t)
    (let* ((first (gensym))
           (result (gensym))
           (working (gensym))
           ;(top-level-envs *top-level-envs*)
           (env (gensym))
           (cell (gensym))
           (pair (gensym))
           (body (funcall f (remove-if #'<is-external-var?> alist :key #'car)))
           (pool (list nil (cons initial-env initial-goals)))
           (initial-bind 
             (mapcar (lambda (x) 
                       (let ((var (<to-lisp-var> (car x))))
                         `(setf (svref ,env ,(fourth x)) ,var)))
                     (remove-if #'<is-internal-var?> alist :key #'car))))
      
      `(let* ((,first t)
              (,working t)
              (|pool| (copy-list ',pool))
              (,cell (if (cdr |pool|) 
                       (<cell-pop> (cdr |pool|)) 
                       (list (<clone-goals> ,(length initial-env) ',initial-goals))))
              (,pair (car ,cell))
              (,env (car ,pair))
              (*returns-binding-list* nil)
              (*retract-mode-p* nil)
              (*last-continuation* nil)
              (*trail* nil) 
              (*x-env* nil) 
              (*y-env* nil)
              (*top-level-envs* ,env) 
              (*prolog-local-rules* nil))
        (fill ,env '*undefined*)
        ,@initial-bind
          (do ()
              ((let ((,result (if ,first 
                                (progn (setq ,first nil)
                                       (pl-search->search-rules (cdr ,pair) nil))
                                (continue-on *last-continuation*))))
                 (not (filter-no ,result)))
               ;;
               (<cell-push> ,cell (cdr |pool|)) 
               nil
               )
            (unwind-protect 
              (progn ,body
                     (setq ,working nil))
              (if ,working 
                (progn (<cell-push> ,cell (cdr |pool|))
                       (release-last-continuation-resources))
                (setq ,working t))))))))

(defmacro exec-prolog (&rest def)
  #{let ((flag (gensym)))
  (flet ((f (alist) (declare (ignore alist)) 
            `(setq ,flag t)))
    `(let (,flag)
      ,(macroexpand-1 `(<solve> ,def ,#'f))
      ,flag)))

(defmacro exec-prolog-1 (&rest def)
  (macroexpand-1 `(<solve> ,def ,(lambda (alist) (declare (ignore alist))
                                           `(return t)))))

(defmacro do-prolog ((&rest def) &body body)
  (flet ((f (alist) 
           `(let ,(mapcar 
                   (lambda (x) 
                     `(,(<to-lisp-var> (car x))
                       (expand-logical-vars ',(cdr x) *top-level-envs* t)))
                   alist)
             ,@body)))
    (macroexpand-1 `(<solve> ,def ,#'f))))

(defmacro do-prolog-1 ((&rest def) &body body)
  `(do-prolog (,@def) (return (progn ,@body))))

(defmacro prolog (&rest def)
  `(let (tmp)
      ,(macroexpand-1 
        `(<solve> ,def
          , (lambda (alist)
              `(push (build-binding-list ',alist *top-level-envs*) tmp))))
    (nreverse tmp)))

(defmacro prolog-1 (&rest def)
  (macroexpand-1 
   `(<solve> ,def
     , (lambda (alist)
         `(return (build-binding-list ',alist *top-level-envs*))))))
       

(defmacro do-prolog-collect ((&rest goal) &body body)
  (let ((tmp (gensym)))
    `(let (,tmp)
      (do-prolog ,goal 
        (push (progn ,@body) ,tmp))
      (nreverse ,tmp))))


(defmacro <&solve> ((&rest def) f)
  (multiple-value-bind (initial-goals initial-env alist) 
      (make-goals def :require-environment-information t)
    #{let* ((first (gensym))
            (working (gensym))
            (result (gensym))
            ;(top-level-envs *top-level-envs*)
            (env (gensym))
            (cell (gensym))
            (pair (gensym))
            (body (funcall f (remove-if #'<is-external-var?> alist :key #'car)))
            (pool (list nil (cons initial-env initial-goals)))
            (initial-bind 
             (mapcar (lambda (x) 
                       (let ((var (<to-lisp-var> (car x))))
                         `(setf (svref ,env ,(fourth x)) ,var)))
                     (remove-if #'<is-internal-var?> alist :key #'car))))
      
    `(lambda ()
      #{let* ((|SAVE/*last-continuation*| nil)
              (|SAVE/*trail*| nil) 
              (|SAVE/*x-env*| nil) 
              (|SAVE/*y-env*| nil)
              (|SAVE/*prolog-local-rules*| nil)
              (,first t)
              (,working t)
              (|pool| (copy-list ',pool))
              (,cell (if (cdr |pool|) 
                       (<cell-pop> (cdr |pool|)) 
                       (list (<clone-goals> ,(length initial-env) ',initial-goals))))
              (,pair (car ,cell))
              (,env (car ,pair)))
      (fill ,env '*undefined*)
      ,@initial-bind
      #{let ((g (lambda (g)
                  #{let ((*returns-binding-list* nil)
                         (*retract-mode-p* nil)
                         (*last-continuation* |SAVE/*last-continuation*|)
                         (*trail* |SAVE/*trail*|)
                         (*x-env* |SAVE/*x-env*|)
                         (*y-env* |SAVE/*y-env*|)                          
                         (*top-level-envs* ,env);',top-level-envs)
                         (*prolog-local-rules* |SAVE/*prolog-local-rules*|))
                  #{let ((,result (if ,first 
                                    (progn (setq ,first nil)
                                           (pl-search->search-rules (cdr ,pair) nil))
                                    (continue-on *last-continuation*))))

                  (setq |SAVE/*trail*| *trail*
                        |SAVE/*last-continuation*| *last-continuation*
                        |SAVE/*x-env*| *x-env*
                        |SAVE/*y-env*| *y-env*
                        |SAVE/*prolog-local-rules*| *prolog-local-rules*)
                  (if (filter-no ,result)
                     (&cons! (unwind-protect 
                               (prog1 ,body
                                 (setq ,working nil))
                               (if ,working 
                                 (progn (<cell-push> ,cell (cdr |pool|))
                                        (release-last-continuation-resources))
                                 (setq ,working t)))
                             (funcall g g))
                     (progn
                       (<cell-push> ,cell (cdr |pool|))
                       nil)))))
      (funcall g g))))


(defmacro &prolog (&rest def)
  `(funcall 
    ,(macroexpand-1 
      `(<&solve> ,def ,(lambda (alist)
                               `(build-binding-list ',alist *top-level-envs*))))))


(defmacro &do-prolog-collect ((&rest def) &body body)
  (flet ((f (alist) 
           `(let ,(mapcar 
                   (lambda (x) 
                     `(,(<to-lisp-var> (car x))
                       (expand-logical-vars ',(cdr x) *top-level-envs* t)))
                   alist)
             ,@body)))
    `(funcall ,(macroexpand-1 `(<&solve> ,def ,#'f)))))


(defmacro &parallel-prolog (&rest defs)
  `(&mapcar #'nconc ,@(mapcar #/`(&prolog ,@_) defs)))

(defmacro parallel-prolog (&rest defs)
  `(&strict (&parallel-prolog ,@defs)))

(defmacro parallel-prolog-1 (&rest defs)
  `(&car (&parallel-prolog ,@defs)))


(defmacro <&do-parallel-prolog> (op (&rest defs) &body body)
  #{let (lisp-vars)
  #{flet ((f (alist) #{case (length alist)
             (0 (push #() lisp-vars) 
                nil)
             (1 (push (<to-lisp-var> (caar alist)) lisp-vars) 
                `(expand-logical-vars ',(cdar alist) *top-level-envs* t))
             (t (push (map 'vector #/(<to-lisp-var> (car _)) alist)
                      lisp-vars) 
                `(vector ,@(mapcar #/`(expand-logical-vars ',(cdr _) *top-level-envs* t)
                                   alist)))))
  #{let ((args (freplicate (length defs) #'gensym))
         (main-codes (mapcar #/`(funcall ,(macroexpand-1 `(<&solve> ,_ ,#'f)))
                             defs)))
  (case op
    (|&Collect|
      ` (&mapcar (lambda (,@args) 
                   (declare (ignorable ,@args))
                   (bind ,(mapcar #'list (nreverse lisp-vars) args)
                     ,@body))
                 ,@main-codes))
    (|Do|
      ` (&dolists ,(mapcan #'list args main-codes) 
          (declare (ignorable ,@args))
          (bind ,(mapcar #'list (nreverse lisp-vars) args)
            ,@body)))))
     

(defmacro &do-parallel-prolog-collect ((&rest defs) &body body)
  `(<&do-parallel-prolog> |&Collect| ,defs ,@body))

(defmacro do-parallel-prolog-collect ((&rest defs) &body body)
  `(&strict (&<do-parallel-prolog> |&Collect| ,defs ,@body)))

(defmacro do-parallel-prolog ((&rest defs) &body body)
  `(<&do-parallel-prolog> |Do| ,defs ,@body))

(defmacro do-parallel-prolog-1 ((&rest defs) &body body)
  `(<&do-parallel-prolog> |Do| ,defs (return (progn ,@body))))


;; end of pl.lisp


;)
