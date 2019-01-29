;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(in-package :clpgk.core.reader)

;(setf *readtable* (copy-readtable))
;(enable-annot-syntax)

(defvar *<readtable/case>* (let ((rt (copy-readtable *readtable*)))
                             (setf (readtable-case rt) :preserve)
                             rt))

(defvar *<collect-reader-idents>* nil)
(defvar *<avoid-reader-idents>* nil)
(defvar *<selected-reader-idents>* nil)

(define-annotation select-reader (ident-or-idents reader-setting)
  (let ((idents (ensure-list ident-or-idents)))
    `(flet ((execute () ,reader-setting))
       (cond (*<collect-reader-idents>*
               ,@(mapcar (lambda (id) `(pushnew ',id *<reader-idents>*)) idents))
           (*<avoid-reader-idents>*
             (unless (or ,@(mapcar (lambda (id) `(member ',id *<avoid-reader-idents>*)) idents))
               (execute)))
           (*<selected-reader-idents>*
             (when (or ,@(mapcar (lambda (id) `(member ',id *<selected-reader-idents>*)) idents))
               (execute)))
           (t (execute))))))


(defvar *extra-procedures-alist* nil)

(defun register-optional-reader (ident function-designator)
  ;(format t "~%READER REGISTERED: ~A~%" ident)
  (let ((pair (assoc ident *extra-procedures-alist*)))
    (if pair
      (setf (cdr pair) function-designator)
      (push (cons ident function-designator) *extra-procedures-alist*)))
  ident)

(defun register-reader-registerer (ident function-designator)
  (register-optional-reader ident function-designator))


(defun <j-header> (limitations)
  ;(print limitations)
  (let ((old-readtable *readtable*))
    (setf *readtable* (copy-readtable nil))
    (when (or (null limitations) (member :annot limitations))
      (enable-annot-syntax))
    (dolist* ((ident . f) *extra-procedures-alist*)
      ;; DEBUG
      (when (or (null limitations) (member ident limitations))
        (funcall f)))
    old-readtable))

(defun enable-reader(&optional limitations)
  (<j-header> limitations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ANNOTATION UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;

;(j-header)



(define-annotation where () 'where)



(define-unary-annotation quote)
(define-unary-annotation todo)

(define-annotation err (form) `(has-errors ,form))
(define-annotation noerr (form) `(has-no-errors ,form))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END OF J-HEADER.LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


