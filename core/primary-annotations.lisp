;; -*- coding: utf-8 -*-

(oleo.core.init:define-package :oleo.core.primary-annotations ()
  (:use :cl :oleo.core.init :alexandria :anaphora :cl-annot :metabang-bind)

  (:export
   #:define-package ;; import from oleo.core.init
   #:export* ;; import from oleo.core.init


   ;; `CL-ANNOT' annotation utility

   #:define-annotation
   #:define-annotation*
   #:define-binary-annotation
   #:define-unary-annotation
   #:define-proxy-annotation
   #:define-forms-as-annotation

   #:annot ;; @foo ... の等価表現を提供する --> (annot foo ...)
   #:annotation-macro-name
   
   ;;;;  Annotations ;;;;
   ;; 最適化関係
   #:package-optimize-level
   #:optimized #:fully-optimized
   ;; 最適化
   #:O0 #:O1 #:O2 #:O3
   ;; 最適化 (SAFETYとDEBUGを常に３に固定した版)
   #:S0 #:S1 #:S2 #:S3
   ;; その他
   #:inline
   #:later #:later!

   )
  )

(in-package :oleo.core.primary-annotations)
;;;;;;;;;;;;;;;;
;; Annotation
;;;;;;;;;;;;;;;;

;; 定義されたアノテーションの情報が格納されるalist
;; 要素は(シンボル . 引数の数) の形式
(defvar *<annotations>* nil)

(defun <register-annotation-info> (name arity internal-name &aux (info (cons arity internal-name)))
  (aif (assoc name *<annotations>*)
       (setf (cdr it) info)
       (setf *<annotations>* (acons name info *<annotations>*)))
  name)

(defun annotation-macro-name (annot-name)
  (aif (assoc annot-name *<annotations>*)
       (bind ((info (cdr it))
              ((_ . internal-name) info))
         ;(declare (ignore _))
         internal-name)
       annot-name))

(defannotation annot ((annot-name &rest args))
    (:arity 1 :inline t)
  `(<annot> ,annot-name ,args))

(defannotation || ((annot-name &rest args))
    (error "||")
    (:arity 1 :inline t)
  `(<annot> ,annot-name ,args))

(defmacro <annot> (annot-name args)
  (cl-annot.expand:expand-annotation (annotation-macro-name annot-name) args))

;#h[2015-08-21]; `defann'を`define-annotation'に変更
;;  アノテーションの定義
(defmacro define-annotation (name lambdalist &rest body)
  (let ((internal-name (intern (concatenate 'string "annot " (symbol-name name))))
        (arity (length lambdalist)))
    `(progn
      (defannotation ,internal-name ,lambdalist 
          (:arity ,arity :inline t :alias ,name)
        ,@body)
      (<register-annotation-info> ',name ,arity ',internal-name)
      ',name)))

;; アノテーションの定義 (同名のマクロ定義も同時に行う版)
(defmacro define-annotation* (name lambdalist &rest body)
  (let ((arity (length lambdalist)))
    `(defannotation ,name ,lambdalist (:arity ,arity :inline t) ,@body)))

;(define-annotation* myann (a b) (list 'the a b))
;'(@myann 1 2)


;#h[2015-08-21]; `defann/binary'を`define-binary-annotation'に変更
(defmacro define-binary-annotation (name &optional (op name))
  `(define-annotation ,name (a b) (list ',op a b)))

;#h[2015-08-21]; `defann/unary'を`define-unary-annotation'に変更
(defmacro define-unary-annotation (name &optional (op name))
  `(define-annotation ,name (a) (list ',op a)))

(defmacro define-proxy-annotation (name arity &optional (op name))
  (unless (alexandria:non-negative-integer-p arity)
    (error "(DEFINE-PROXY-ANNOTATION ~A ~A ...): ~A is not a non-negative integer" name arity arity))
  (let* ((static-tmp-syms (memoized (list (list (gensym)))))
         (n-syms (length (car static-tmp-syms))))
    (when (< n-syms arity)
      (dotimes (i (- arity n-syms))
        (push (gensym) (car static-tmp-syms)))
      (setq n-syms arity))
    (let ((static-vars (nthcdr (- n-syms arity) (car static-tmp-syms))))
      `(define-annotation ,name ,static-vars (list ',op ,@static-vars)))))

;(define-proxy-annotation foo 20)

(defmacro define-forms-as-annotation (arity &rest form-names)
  (unless (alexandria:non-negative-integer-p arity)
    (error "(DEFINE-FORMS-AS-ANNOTATION ~A ...): ~A is not a non-negative integer" arity arity))
  `(progn
     ,@(mapcar (lambda (name) `(define-proxy-annotation ,name ,arity))
               form-names)))

;(register-forms-as-annotation 10 a b c)

(define-annotation inline (src) 
  (if (consp src) 
    `(progn 
      (proclaim '(inline ,(second src)))
      ,src)
    `(declare (inline ,src))))


;; @fully-optimized exp =>
;; @fo (defun name ll body) -> (defun name ll DECL body)
;; @fo (let V body) -> (let DECL body)
;; @fo (other-form body) -> (other-form DECL body)
;; -O3 -O2 -O1 -O
;;@-o0

(defparameter *<optimize-levels>* (make-hash-table))

(define-annotation package-optimize-level (level)
  (unless (and (integerp level) (>= level 0) (<= level 3))
    (error "@PACKAGE-OPTIMIZE-LEVEL ~A" level))
  (setf (gethash *package* *<optimize-levels>*) level))

(defun <get-opt-level> () (gethash *package* *<optimize-levels>* 1))
(defun <optimize> (&optional (level (<get-opt-level>)) safe?)
  (if safe?
    (case level
      (0 '(DECLARE (OPTIMIZE (SPEED 0) (SPACE 0) (SAFETY 3) (DEBUG 3) (COMPILATION-SPEED 3))))
      (1 '(DECLARE (OPTIMIZE (SPEED 2) (SPACE 1) (SAFETY 3) (DEBUG 3) (COMPILATION-SPEED 0))))
      (2 '(DECLARE (OPTIMIZE (SPEED 3) (SPACE 2) (SAFETY 3) (DEBUG 3) (COMPILATION-SPEED 0))))
      (3 '(DECLARE (OPTIMIZE (SPEED 3) (SPACE 3) (SAFETY 3) (DEBUG 3) (COMPILATION-SPEED 0)))))
    (case level
      (0 '(DECLARE (OPTIMIZE (SPEED 0) (SPACE 0) (SAFETY 3) (DEBUG 3) (COMPILATION-SPEED 3))))
      (1 '(DECLARE (OPTIMIZE (SPEED 2) (SPACE 0) (SAFETY 3) (DEBUG 3) (COMPILATION-SPEED 2))))
      (2 '(DECLARE (OPTIMIZE (SPEED 3) (SPACE 1) (SAFETY 1) (DEBUG 2) (COMPILATION-SPEED 1))))
      (3 '(DECLARE (OPTIMIZE (SPEED 3) (SPACE 3) (SAFETY 0) (DEBUG 0) (COMPILATION-SPEED 0)))))))

(defun <insert-optimaizer> (form
                            &optional
                            (level (<get-opt-level>))
                            safe?)
  (if (atom form)
    (list 'locally (<optimize>) form)
    (let ((op (car form))
          (optdecl (<optimize> level)))
      (cond ((eq op 'lambda)
              (list* op (second form) optdecl (cddr form)))
            ((or (eq op 'defun) (eq op 'defmacro))
              (list* op (second form) (third form) optdecl (cdddr form)))
            ((or (eq op 'let) (eq op 'let*))
              (list* op (second form) optdecl (cddr form)))
            ((or (eq op 'destructuring-bind) (eq op 'multiple-value-bind))
              (list* op (second form) (third form) optdecl (cdddr form)))
            (T (list 'locally optdecl form))
            ))))

;(<insert-optimaizer> '(multiple-value-bind (x) a b))

(define-annotation optimized (exp)
  (<insert-optimaizer> exp))

;; 最適化アノテーション: Cコンパイラの最適化フラグ風味 `@O0' から  `@O3'
(define-annotation O0 (exp)
  (<insert-optimaizer> exp 0))
(define-annotation O1 (exp)
  (<insert-optimaizer> exp 1))
(define-annotation O2 (exp)
  (<insert-optimaizer> exp 2))
(define-annotation O3 (exp)
  (<insert-optimaizer> exp 3))

;; 最適化アノテーション: (SAFETYが常に３の版) `@S0' から  `@S3'
(define-annotation S0 (exp)
  (<insert-optimaizer> exp 0 t))
(define-annotation S1 (exp)
  (<insert-optimaizer> exp 1 t))
(define-annotation S2 (exp)
  (<insert-optimaizer> exp 2 t))
(define-annotation S3 (exp)
  (<insert-optimaizer> exp 3 t))

(define-annotation fully-optimized (exp)
  (<insert-optimaizer> exp 3))

(define-annotation later (form)
  `(implement-later ,form))

;; 警告付き　かつ　定義フォームを強制
(define-annotation later! (form)
  (assert (and '@later! (alexandria:proper-list-p form) (>= (length form) 2)))
  `(implement-later (console-message "@LATER! (~A ~A ...) : Pkg=~A"
                                     ',(first form) ',(second form) ,(package-name *package*))
                    ,form))





