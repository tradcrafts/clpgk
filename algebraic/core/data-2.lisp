;; -*- coding: utf-8 -*-

(oleo.base:oleo-base-header)
(in-package :oleo.algebraic.core)

;; Maybe

(define-global-data maybe nothing (just t))
;(define-internal-data _maybe _nothing (_just t))

;; Either

(define-global-data either (left t) (right t))
;(define-internal-data _either (_left t) (_right t))


;; Tuples P3 ... P20
(eval-when (:compile-toplevel :load-toplevel)
  (defvar *tuple-max* 20))

(eval-when (:compile-toplevel)
  (defmacro <define-tuples> ()
    (let (data-definitions
          conversion-list)
      (do ((i 3 (1+ i))
           (ts (list t t t) (cons t ts)))
          ((> i *tuple-max*) `(eval-when (:load-toplevel)
                                (defvar %tuple-conversion-vector% #(nil nil nil ,@(nreverse conversion-list)))
                                ,@data-definitions))
        (let ((ident (intern (format nil "P~A" i))))
          (push ident conversion-list)
          (push `(define-global-data ,ident (,ident ,@ts))
                data-definitions))))))

(<define-tuples>)

#Comment


(define-data mydat (myd t t t))
(define-internal-data imydat (imyd t t t))

(defclass xcons () ((<car> :initarg <car> :initform nil)
                    (<cdr> :initarg <cdr> :initform nil)))

(defun xcons (xcar xcdr)
  (make-instance 'xcons '<car> xcar '<cdr> xcdr))

(defun xcar (xcons)
  (slot-value xcons '<car>))
(defun xcdr (xcons)
  (slot-value xcons '<cdr>))

(defun xlist (&rest elems)
  (when elems
    (let (last)
      (dolist (e (nreverse elems) last)
        (setf last (xcons e last))))))

(defun xdelist (xcons)
  (let (delist)
    (while xcons
      (push (xcar xcons) delist)
      (setf xcons (xcdr xcons)))
    (nreverse delist)))


(defun copy-xlist (xcons)
  (let (new)
    (while xcons
      (setf cur (xcons (xccar xcons) cur))
      
  
(defun xmap (xcons f)
  (let (mapped)
    (while xcons
      (funcall f (xcar xcons))
      (setf xcons (xcdr xcons)))
    mapped))


(define-internal-data foobar fb-1 (fb-2 t t t))

(defclass tup ()
  ((fst :accessor t-fst :initform nil :allocation :class)
   (snd :accessor t-snd)))

(defclass tup2 (tup)
  ((fst :accessor t2-fst :initform 'ok)))
          
(t-fst (make-instance 'tup))
(setf (t-fst (make-instance 'tup)) 200)

(t2-fst (make-instance 'tup2))
(xcdr (xcons 1 2))



(slot-value (make-instance 'xcons :car 0) 'cdr)

(defun mk () (make-instance 'xcons))
(disassemble 'mk)


(myd 'a nil 'c :source (myd 1 2 3) :mask nil :update t)
(myd 'a nil 'c :mask nil :update t)
(myd 'a nil 'c :mask nil :updPate t)


(just# 3 :update t)

(== (myd 3 2 1) (myd 3 2 1))
(== 3 3)

(typep (just 3) 'maybe)


(symbol-package '==)
