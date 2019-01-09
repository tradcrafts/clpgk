;; -*- coding: utf-8 -*-

; Beginning of Licence
;
; This software is licensed only for personal and educational use and
; not for the production of commercial software.  Modifications to this
; program are allowed but the resulting source must be annotated to
; indicate the nature of and the author of these changes.  
;
; Any modified source is bound by this licence and must remain available 
; as open source under the same conditions it was supplied and with this 
; licence at the top.

; This software is supplied AS IS without any warranty.  In no way shall 
; Mark Tarver or Lambda Associates be held liable for any damages resulting 
; from the use of this program.

; The terms of these conditions remain binding unless the individual 
; holds a valid license to use Qi commercially.  This license is found 
; in the final page of 'Functional Programming in Qi'.  In that event 
; the terms of that license apply to the license holder. 
;
; (c) copyright Mark Tarver, 2008
; End of Licence

(IN-PACKAGE :CL-USER)
                                        ;(setq custom:*load-compiling* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (SETF *READTABLE* (COPY-READTABLE *READTABLE*))
  (SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
  )

(DEFPACKAGE :OLEO.EMBED.CORE
  ;; make-package in sbcl will not use the common-lisp
  ;; package by default, so I changed it to defpackage
  (:EXPORT 
   #:TUPLE #:TUPLE-P #:MAKE-TUPLE #:TUPLE-FST #:TUPLE-SND
   #:QILOAD
   #:*QI-READTABLE*
   
   )
  (:USE :CL :OLEO.BASE :OLEO.ALGEBRAIC.CORE :OLEO.ALGEBRAIC.XDATA :OLEO.PROLOG)
  (:SHADOW CL:DEFUN)
  )

(DEFVAR OLEO.EMBED.CORE:*QI-READTABLE* *READTABLE*)

(IN-PACKAGE :OLEO.EMBED.CORE)


(DEFPARAMETER *<DEFTEST>* NIL)
(DEFPARAMETER *<DEFREMS>* NIL)
(DEFMACRO DEFUN (NAME LL &REST BODY)
  (MULTIPLE-VALUE-BIND (body decls doc-string) (PARSE-BODY BODY :DOCUMENTATION T) 
  `(PROGN (PUSHNEW ',NAME *<DEFTEST>*)
          (CL:DEFUN ,NAME ,LL
            ,@(WHEN doc-string (LIST doc-string))
            ,@decls
            (SETQ *<DEFTEST>* (REMOVE ',NAME *<DEFTEST>*))
            (PUSHNEW ',NAME *<DEFREMS>*)
            ,@body)
          )))



;(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)

'(DEFVAR *V-STACK* NIL)

  ;; vs xs は共に長さ２で、(LIST *)の形式であることが保証されている
'(DEFMACRO PROGV* ((list1 v) (list2 x) code)
  (DECLARE (IGNORE list1 list2))
  '(UNLESS (AND (MEMBER (CAR vs) '(LIST))
                (MEMBER (CAR xs) '(LIST))
                (= 2 (LENGTH vs))
                (= 2 (LENGTH xs)))
    (ERROR "PROGV* ER"))
  `(PROG2
     (PUSH (CONS ,v ,x) *V-STACK*)
     ,code
                                        ;(PROGN ,body)
     (POP *V-STACK*)))
  


'(DEFUN BOUNDP* (v)
  (ASSOC v *V-STACK*))

'(DEFINE-COMPILER-MACRO BOUNDP* (v)
  `(ASSOC ,v *V-STACK*))

'(DEFUN SYMBOL-VALUE* (v)
  (CDR (ASSOC v *V-STACK*)))

'(DEFINE-COMPILER-MACRO SYMBOL-VALUE* (v)
  `(CDR (ASSOC ,v *V-STACK*)))


  
  ;; (DEFMACRO PROGV* (vs xs &BODY body)
  ;;   `(PROGV ,vs ,xs ,@body))

  ;; (DEFUN BOUNDP* (v)
  ;;   (BOUNDP v))

  ;; (DEFUN SYMBOL-VALUE* (v)
  ;;   (SYMBOL-VALUE v))

                                        ;)



