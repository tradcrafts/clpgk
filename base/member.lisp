;; -*- coding: utf-8 -*-
(oleo.core:oleo-core-header)

(oleo.core:define-package :oleo.base.member* (:oleo.base.member)
  (:use :cl)
  (:import/export :oleo.base.attr*)
  (:export 
   #:call-member
   #:symbol-member
   #:funcall-member #:apply-member
   #:member-boundp #:member-makunbound
   #:define-member #:define-generic-member
   #:define-member-slot
   #:define-member-accessor))

(in-package :oleo.base.member)


(defparameter *selector-symbol-table* (make-hash-table))

(defun <fetch-selector-symbol> (name)
  (let ((selector-symbol (gethash name *selector-symbol-table*)))
    (unless selector-symbol
      (setq selector-symbol (gensym (symbol-name name)))
      (eval `(defgeneric ,selector-symbol (x)))
      (setf (gethash name *selector-symbol-table*) selector-symbol))
    selector-symbol))

(defparameter *function-var-table* (make-hash-table :test 'equal))

(defun <fetch-function-var> (name class-name &key (force t))
  #{let* ((key (cons name class-name))
          (function-var (gethash key *function-var-table*)))
  (when (and #!function-var force)
    (let ((sym-1 (gensym (format nil "~D-~D" name class-name)))
          (sym-2 (gensym (format nil "(SETF ~D-~D)" name class-name))))
      (setq function-var (cons sym-1 (cons sym-2 nil)))
      (setf (gethash key *function-var-table*) function-var)))
  function-var)

(defun |symbol-member| (name object)
  #{let ((selector-symbol (gethash name *selector-symbol-table*)))
  (if selector-symbol
    (first (funcall selector-symbol object))
    (error "SYMBOL-MEMBER: member ~D is not defined." name)))

(setf (symbol-function 'symbol-member) #'|symbol-member|)

(defun <get-selector-symbol> (name)
  #{let ((selector-symbol (gethash name *selector-symbol-table*)))
  (unless selector-symbol
    (error "SYMBOL-MEMBER: member ~D is not defined." name))
  selector-symbol)
  
(define-compiler-macro symbol-member (name object)
  (if (and (consp name) 
           (eq 'quote (car name)))
    `(let ((selector-symbol (memoized (<get-selector-symbol> ,name))))
      (first (funcall selector-symbol ,object)))
    `(|symbol-member| ,name ,object)))


(defun funcall-member (name object &rest args)
  (apply (symbol-member name object) object args))

(define-compiler-macro funcall-member (name object &rest args)
  #{let ((x (gensym)))
  `(let ((,x ,object))
    (funcall (symbol-member ,name ,x) ,x ,@args)))

(defun apply-member (name object arg1 &rest rest-args)
  (if rest-args
    (apply #'apply (symbol-member name object) object arg1 rest-args)
    (apply (symbol-member name object) object arg1)))

(define-compiler-macro apply-member (name object arg1 &rest args)
  #{let ((x (gensym)))
  `(let ((,x ,object))
    (apply (symbol-member ,name ,x) ,x ,arg1 ,@args)))


(defun <valid-member-name?> (name)
  (or (symbolp name) 
      (and (consp name)
           (eq (length name) 2) 
           (eq (car name) 'SETF))))


(defun member-boundp (name class)
  (unless (<valid-member-name?> name)
    (error "MEMBER-BOUNDP: invalid member name ~D" name))
  #{let* ((for-setf? (consp name))
          (name (if (consp name) (second name) name))
          (function-var (<fetch-function-var> name class :force nil))
          (symbol (if for-setf? (second function-var) (first function-var))))

  (and symbol
       (fboundp symbol)))
    

(defun member-makunbound (name class)
  (unless (<valid-member-name?> name)
    (error "MEMBER-MAKUNBOUND: invalid member name ~D" name))
  #{let* ((for-setf? (consp name))
          (name (if (consp name) (second name) name))
          (function-var (<fetch-function-var> name class :force nil))
          (symbol (if for-setf? (second function-var) (first function-var))))
  
  (when (and symbol (fboundp symbol))
    (fmakunbound symbol))
  name)

(defun <gencode-defmember> (macroname defn name object class args body)
  (unless (<valid-member-name?> name)
    (error "~D: invalid member name ~D" macroname name))
  #{let* ((for-setf? (consp name))
          (name (if (consp name) (second name) name))
          (function-var (<fetch-function-var> name class))
          (symbol (if for-setf? (second function-var) (first function-var)))
          (definition ` (,defn ,symbol (,object ,@args) 
                          (declare (ignorable ,object))
                          ,@body)))
  (if (cddr function-var)
    definition
    (let ((selector-symbol (<fetch-selector-symbol> name)))
      `(progn
        (unless (cddr ',function-var)
          (defmethod ,selector-symbol ((_ ,class))
            (declare (ignore _))
            ',function-var)
          (setf (cddr ',function-var) t))
        ,definition))))

(defmacro define-member (name ((object class) &rest args) &body body)
  (<gencode-defmember> 'define-member 'defun name object class args body))

(defmacro define-generic-member (name ((object class) &rest args) &body body)
  (<gencode-defmember> 'define-generic-member 'defmethod name object class args body))


(defmacro call-member (name obj &rest args)
  (let ((selector-symbol (<fetch-selector-symbol> name))
        (fn-name (case (length args)
                   (0 '<call-member/0>) (1 '<call-member/1>) (2 '<call-member/2>)
                   (3 '<call-member/3>) (4 '<call-member/4>) (5 '<call-member/5>)
                   (6 '<call-member/6>) (7 '<call-member/7>) (8 '<call-member/8>)
                   (9 '<call-member/9>) (10 '<call-member/10>) (11 '<call-member/11>)
                   (12 '<call-member/12>) (13 '<call-member/13>) (14 '<call-member/14>)
                   (15 '<call-member/15>) (16 '<call-member/16>) (t '<call-member>))))
    `(,fn-name ',selector-symbol ,obj ,@args)))


(defsetf <call-member> <setf-wrapper>)
(declaim (inline <call-member> <setf-wrapper>))
(defun <call-member> (selector-symbol obj a b c d e f g h i j k l m n o p q &rest args)
  (apply (first (funcall selector-symbol obj)) 
         obj a b c d e f g h i j k l m n o p q args))

(defun <setf-wrapper> (selector-symbol obj a b c d e f g h i j k l m n o p q &rest args)
  (let ((value (car (last args))))
    (apply (second (funcall selector-symbol obj))
           obj
           value
           a b c d e f g h i j k l m n o p q
           (nbutlast args))))


(defmacro macrodef (n) ;; n = 0..16
  #{let* ((<call-member/n> (intern (format nil "<CALL-MEMBER/~D>" n)))
          (<setf-wrapper/n> (intern (format nil "<SETF-WRAPPER/~D>" n)))
          (syms '(a b c d e f g h i j k l m n o p q))
          (args (nthcdr (- (length syms) n) syms)))
  `(eval-when (:load-toplevel :execute)
    (defsetf ,<call-member/n> ,<setf-wrapper/n>)
    (declaim (inline ,<call-member/n> ,<setf-wrapper/n>))
    (defun ,<call-member/n> (selector-symbol obj ,@args)
      (funcall (first (funcall selector-symbol obj)) obj ,@args))
    (defun ,<setf-wrapper/n> (selector-symbol obj ,@args value)
      (funcall (second (funcall selector-symbol obj))
               obj
               value
               ,@args))))

(macrodef 0) (macrodef 1) (macrodef 2) (macrodef 3) (macrodef 4) (macrodef 5)
(macrodef 6) (macrodef 7) (macrodef 8) (macrodef 9) (macrodef 10) (macrodef 11) 
(macrodef 12) (macrodef 13) (macrodef 14) (macrodef 15) (macrodef 16)
(fmakunbound 'macrodef)

;; slotsはsymbolまたは(symbol...)。 symbolと(symbol)は等価
(defmacro define-member-slot (slots class) 
  `(eval-when (:load-toplevel :execute)
    ,@ (with-collect (c)
         (dolist (slot (ensure-cons slots))
           (c `(define-member ,slot ((x ,class)) 
                (slot-value x ',slot)))
           (c `(define-member (SETF ,slot) ((x ,class) val) 
                (setf (slot-value x ',slot) val)))))))


;; namesはsymbolまたは(symbol...)。 symbolと(symbol)は等価
(defmacro define-member-accessor (name ((obj class) &rest args) access-form)
  `(eval-when  (:load-toplevel :execute)
    (define-member ,name ((,obj ,class) ,@args)
          ,access-form)
    (c (define-member (SETF ,name) ((,obj ,class) |(val)| ,@args)
          (setf ,access-form |(val)|)))))


;; 入力マクロ [ ... ]
;; メンバ関数コールに展開される[obj foo a b // 'bar x // baz y]
;; ==> (call-member baz (bar (call-member foo obj a b) x) y)
;; // はデリミタとして機能する
;; 展開は後段*j-header-brace-expander*に指定された関数(j-member.lisp内)に任せる

;; 入力マクロ[]の展開部
(defun brack-expander (src)
  #{let ((xss (split-sequence '// (cdr src)))
         (head (car src)))
  #{dolist (xs xss head)
  (unless xs
    (error "reader macro {}: invalid arguments: ~D" src))
  #{let ((cmd (car xs)))
  (cond ((symbolp cmd)
          (setq head `(call-member ,cmd ,head ,@(cdr xs))))              
        ((and (consp cmd)
              (eq 'quote (car cmd)))
          (setq head `(,(second cmd) ,head ,@(cdr xs))))
        (t (error "reader macro{}: invalid arguments ~D" cmd))))


(defun <register-readers> ()
  @select-reader oleo.core.reader::%internal%
  (set-macro-character #\] (get-macro-character #\)))
  @select-reader oleo.core.reader::%internal%
  (set-macro-character 
   #\[
   (lambda (stream char)
     @ignore (char)
     (let ((elems (read-delimited-list #\] stream t)))
       (brack-expander elems)))))

(register-reader-registerer '|member| '<register-readers>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-member foo ((a integer) b) b)
(define-generic-member foo ((a integer) (b symbol)) a)
(define-generic-member foo ((a integer) (b integer)) (list 'int-int a b))
(define-generic-member foo ((a integer) (b t)) (list 'int-t a b))
(member-makunbound 'foo 'integer)
(call-member foo 2 "foo")

(defun test (n) (dotimes (_ n) (call-member foo n 'a)))
(time (test 1000000))
(defun b (a b))
(time (dotimes (_ 10000000) (b _ _)))










(let* ((a '(1 2)) (b `(,a ,a))) (eq (first b) (second b)))



(-> '(x t // 'b c d e // f g // 'h // foo))
(CALL-MEMBER FOO (H (CALL-MEMBER F (B (CALL-MEMBER T X) C D E) G)))

{obj foo a // (if (f _) (- _) _)}

(-> '(a 'car // 'nbutlast // 'sort #'< // foo 1 2 3))
(CALL-MEMBER FOO (SORT (NBUTLAST (CAR A)) #'<) 1 2 3)
(SORT (NBUTLAST (CAR A)) #'<)
(CALL-MEMBER FOO (H (CALL-MEMBER F (B (CALL-MEMBER T X) C D E) G)))
(CALL-MEMBER FOO (H (CALL-MEMBER F (CALL-MEMBER A (CALL-MEMBER T X) 'B C D E) G)))
(sort '(1 2 3) '<)
(1 2 3)
{a'car // 'nbutlast !}
{obj foo a b }
(define-member-slots struc a b c d e)
(setq s (make-struc))
(setf (call-member e s) 'bar)
(define-member-accessor car ((obj list)) (car obj))
(define-member-accessor cdr ((_ list)))
(setf (call-member car '(1 2 3)) 100)
(call-member cdr '(1 2 3))
(set (car nil) 3)

(split-sequence '// '(a b // c))
{obj draw-rect x y x2 y2 // }
'(#>a)



(defmethod met (a b))
(defmethod met ((a integer)  x &key f) 3)







(defstruct struc a b c d e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(declaim (inline <call-member>))
'(defmacro <call-member> (selector obj &rest args)
  (apply (symbol-value (funcall selector obj)) obj args))
  
'(defun foo () (<call-member> 'a 1 2 3 4))




(incf (call-member a b c d e))

(defmacro generic-call/memoized (name obj &rest args)
  (let* ((selector-symbol (<fetch-selector-symbol> name))
         (tmp (gensym)))
    `(let* ((,tmp ,obj))
      (funcall (symbol-value (memoized (,selector-symbol ,tmp))) ,tmp ,@args))))

(define-member foo ((x integer) &rest a)
  (list 'integer x a))
(define-member foo ((x cons))
  (list 'cons x))
(define-member foo ((x list) a b)
  (list 'list x a b))

(setf (call-member foo nil 3 '(1)) 'val)

(define-member set ((xs cons) i j &rest args)
  (svref (nth i xs) j))
(define-member (setf set) ((xs cons) y i j &rest args)
  (setf (svref (nth i xs) j) (cons y args)))

(defvar a '(#(1 2 3) #(4 5)))
(setf (call-member set a 1 0 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6) 110)


(defmethod met ((a integer) b) (list a b))
(defmethod met ((a real) b) (list b a))
(funcall 'met 100 2)
