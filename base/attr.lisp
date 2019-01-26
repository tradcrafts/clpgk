;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.core:clpgk-core-header)

(clpgk.core:define-package :clpgk.base.attr* (:clpgk.base.attr)
  (:use :cl)
  (:import/export :clpgk.base.text*)
  (:export
   #:data-attr
   #:type-attr
   #:define-type-attr
   #:attr
   ))

(in-package :clpgk.base.attr)


@eval-always
(defun <get-symbol-constant> (x)
  (cond ((keywordp x) x)
        ((and (consp x) 
              (eq 'quote (first x)) 
              (symbolp (second x)))
          (second x))
        (t 0) ;; 非シンボル値
        ))

;; 値属性 ;;

(defun <get-data-attr-table> (attr)
  (aif (get attr '|data-attr|) 
       it
       (setf (get attr '|data-attr|) (make-hash-table :weak :key))))


(defun <data-attr> (object attr default)
  #{let ((table (get attr '|data-attr|)))
  (if table
    (multiple-value-bind (val exists) (gethash object table )
      (values (if exists val default)  
              exists))
    (values default nil)))

;@inline 
(defun data-attr (object attr &optional default)
  (assert (and 'data-attr (symbolp attr)))
  (<data-attr> object attr default))

#-clisp
(define-compiler-macro data-attr (object attr &optional default)
  #{let* ((sym (<get-symbol-constant> attr)))
  (cond ((symbolp sym)
          #{let* ((table (<get-data-attr-table> sym)))
          `(let ((default ,default))
            (multiple-value-bind (val exists) (gethash ,object ,table )
             (values (if exists val default) exists))))
        (t `(<data-attr> ,object ,attr ,default))))

(defun (setf data-attr) (data object attr)
  #{let ((table (<get-data-attr-table> attr)))
  (setf (gethash object table) data))

;;;; 型属性 ;;;

(defun <get-type-attr-ident> (attr)
  (acond ((get attr '|type-attr|) 
          it)
         (t #{let ((ident (gensym (symbol-name attr))))
            (eval `(defmethod ,ident (_)
                    (declare (ignore _))
                    (if (boundp ',ident)
                      (symbol-data ',ident)
                      '|undefined|)))
            (setf (get attr '|type-attr|) ident))))

(defmacro define-type-attr (class attr data-form)
  (assert (and 'define-type-attr 
               (symbolp class)
               (symbolp attr)))
  #{let* ((ident (<get-type-attr-ident> attr))
          (code (if (eq attr T)
                  `(setf (symbol-data ',ident ,data-form))
                  `(let ((v ,data-form)) 
                    (defmethod ,ident ((_ ,class)) 
                      (declare (ignore _))
                      v)))))
    `(progn ,code nil))

(defun <type-attr> (object attr default)
  (acond ((get attr '|type-attr|)
          #{let ((result (funcall it object)))
          (if (eq result '|undefined|)
            (values default nil)
            (values result t)))
         (t (values default nil))))

;@inline 
(defun type-attr (object attr &optional default)
  (assert (and 'type-attr (symbolp attr)))
  (<type-attr> object attr default))

#-clisp
(define-compiler-macro type-attr (object attr &optional default)
  #{let* ((sym (<get-symbol-constant> attr)))
  (cond ((symbolp sym)
          #{let* ((ident (<get-type-attr-ident> sym)))
          `(let ((default ,default)
                 (result (,ident ,object)))
            (if (eq result '|undefined|)
              (values default nil)
              (values result t))))
        (t `(<type-attr> ,object ,attr ,default))))



;;;

;@inline
(defun <attr> (object attr default)
  (assert (and 'attr (symbolp attr)))
  (multiple-value-bind (result exists) (data-attr object attr)
    (if exists 
      (values result t)
      (type-attr object attr default))))

(defun attr (object attr &optional default)
  (<attr> object attr default))

#-clisp
(define-compiler-macro attr (object attr &optional default)
  (if (symbolp (<get-symbol-constant> attr))
    `(let ((def ,default)
           (obj ,object))
      (multiple-value-bind (result exists)
          (data-attr obj ,attr def)
        (if exists
          (values result t)
          (type-attr obj ,attr def))))
    `(<attr> ,object ,attr ,default)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(テスト)

(funcall (compiler-macro-function 'attr) '(attr a 'b c) nil)

(define-type-attr number baz! -1)
(define-type-attr integer baz! -100)

(setf (data-attr 1 'baz!) 'chin)

(attr '1 'baz!)
(attr 'a3 'baz! 20)

(defun foo (a) 
  (attr a 'baz!))


(funcall #'type-attr 3 'baz! 3)

(defun foo (x)
  (type-attr x 'baz! 100.3))
(foo 3)

(define-type-attr integer unk :foobar)

(type-attr (expt 2 100) 'unk 'bak)

(attr 5 'foo 'xx)

(setf (data-attr 3 'foo) 20)
(funcall 'data-attr 3 'foo)

(defun foo (x) (data-attr x 'foo :ng))
(foo 3)

20
T
NIL
NIL

(ext:gc)

