;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(in-package :clpgk.algebraic.xdata)


(DEFCLASS XTUPLE ()
  (fst
   snd))

(DEFCLASS P2 (XTUPLE) ())

(DEFMETHOD PRINT-OBJECT ((x P2) stream)
  (FORMAT stream "(@p ~W ~W)" (SLOT-VALUE x 'fst) (SLOT-VALUE x 'snd)))

(DEFUN XTUPLE-P (x) (TYPEP x 'XTUPLE))
(DEFINE-COMPILER-MACRO XTUPLE-P (x) `(TYPEP ,x 'XTUPLE))
            
(DEFUN XTUPLE (fst snd &OPTIONAL (class 'XTUPLE))
  (LET ((xtuple (MAKE-INSTANCE class)))
    (SETF (SLOT-VALUE xtuple 'fst) fst
          (SLOT-VALUE xtuple 'snd) snd)
    xtuple))

; XTUPLE compiler macro版に深刻な不具合？
'(DEFINE-COMPILER-MACRO XTUPLE (fst snd &OPTIONAL (class ''XTUPLE))
  `(LET ((xtuple (MAKE-INSTANCE ,class)))
    (SETF (SLOT-VALUE xtuple 'fst) ,fst
          (SLOT-VALUE xtuple 'snd) ,snd)
    xtuple))

(defun xtuple* (a b)
  (IF (SYMBOLP a)
    (LET ((lisp-ident (GET a '|%classdata%|)))
      (IF lisp-ident
        (XTUPLE a b lisp-ident)
        (XTUPLE a b 'P2)))
    (XTUPLE a b)))

; (@p 'just# 3)
;(just# 3) ==> (xtuple* '|just#| 3)
;(string-downcase "")
;(just# 

(DEFUN XTUPLE-FST (xtuple)  (SLOT-VALUE xtuple 'fst))
(DEFUN XTUPLE-SND (xtuple)  (SLOT-VALUE xtuple 'snd))

(DEFINE-COMPILER-MACRO XTUPLE-FST (xtuple)  `(SLOT-VALUE ,xtuple 'fst))
(DEFINE-COMPILER-MACRO XTUPLE-SND (xtuple)  `(SLOT-VALUE ,xtuple 'snd))

(DEFUN (SETF XTUPLE-FST) (val x)  (SETF (SLOT-VALUE x 'fst) val))
(DEFUN (SETF XTUPLE-SND) (val x)  (SETF (SLOT-VALUE x 'snd) val))
(DEFINE-COMPILER-MACRO (SETF XTUPLE-FST) (val x)  `(SETF (SLOT-VALUE ,x 'fst) ,val))
(DEFINE-COMPILER-MACRO (SETF XTUPLE-SND) (val x)  `(SETF (SLOT-VALUE ,x 'snd) ,val))

