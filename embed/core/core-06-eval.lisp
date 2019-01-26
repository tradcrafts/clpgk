;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory
;;
;; -------------------------------------------------
;; --------------ORIGINAL Qi LICENSE ---------------
;; -------------------------------------------------
;;
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (SETF *READTABLE* (COPY-READTABLE *READTABLE*))
  (SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
  )

(IN-PACKAGE :CLPGK.EMBED.CORE)

(DEFUN eval (V6) (EVAL (quote V6)))

(DEFUN quote (X) (lisp-form NIL X))


(DEFPARAMETER *DEBUG-PRINT* NIL)

(DEFUN <count-nested-/.> (form &OPTIONAL (cnt 0))
  (IF (AND (CONSP form) (EQ (FIRST form) '/.))
    (<count-nested-/.> (THIRD form) (1+ cnt))
    (VALUES cnt form)))
(DEFUN <collect-nested-/.-vars> (form)
  (WHEN (AND (CONSP form) (EQ (FIRST form) '/.))
    (CONS (SECOND form) (<collect-nested-/.-vars> (THIRD form)))))
(DEFUN <exist-same-vars?> (vars)
  (WHEN vars
    (OR (MEMBER (CAR vars) (CDR vars))
        (<exist-same-vars?> (CDR vars)))))
(DEFUN <convert-/.> (xi-lambda-expression inherits)
  (MULTIPLE-VALUE-BIND (cnt exp) (<count-nested-/.> xi-lambda-expression)
    (IF (<= cnt 1)
      (LET ((var (SECOND xi-lambda-expression)))
        `(LAMBDA (,var) ,(lisp-form (cons var inherits) exp)))
      (LET* ((vars (<collect-nested-/.-vars> xi-lambda-expression))
             (main-exp (lisp-form (append vars inherits) exp)))
        (LIST 'APPLICABLE
              (IF (<exist-same-vars?> vars)
                (LET ((proxy-vars (FREPLICATE (LIST-LENGTH vars) (LAMBDA () (GENSYM "P")))))
                  `(LAMBDA ,proxy-vars
                     ,(REDUCE (LAMBDA (A B) `(LET (,A) ,B))
                              (ZIP vars proxy-vars)
                              :FROM-END T
                              :INITIAL-VALUE main-exp)))
                (LIST 'LAMBDA vars main-exp)))))))

(DEFUN lisp-form (V93 V94)
  (WHEN *DEBUG-PRINT*
    (PRINT (LIST 'lisp-form V93 V94)))
  ;;(WHEN V93 (PRINT (LIST V93 V94)))
  
  (COND
    ((NULL V94) NIL)
    ((wrapper (element? V94 V93)) V94) ;; <-- これはもはや不要（おそらく）

    ;;((EQ '_ V94) (LIST 'GENSYM "X"))
    ((EQ '_ V94) '_)
    
    ((EQ T V94) T)

  ;; HACK JUN COMENNTED OUT
  ;((wrapper (variable? V94)) (LIST 'QUOTE V94))

    ((wrapper (symbol? V94))
      (FLET ((error/misplaced (c) (ERROR "misplaced token `~A'" c)))
        (CASE V94
          (|;|  (error/misplaced #\.))
          (bar# (error/misplaced #\:))
          (|<,>| (error/misplaced #\,))
          (T (LIST 'QUOTE V94)))))
  ((MEMBER V94 '(true false)) (LIST 'QUOTE V94))
  ((NUMBERP V94) V94)
  ((CHARACTERP V94) V94)
  ((STRINGP V94) V94)
  ;; ((AND (CONSP V94) (EQ '/. (CAR V94)) (CONSP (CDR V94))
  ;;   (CONSP (CDR (CDR V94))) (NULL (CDR (CDR (CDR V94)))))
  ;;  (LET* ((V95 (CDR V94)) (V96 (CAR V95)))
  ;;    (LIST 'LAMBDA (LIST V96) (lisp-form (CONS V96 V93) (CAR (CDR V95))))
  ;;    ))

  ((AND (CONSP V94) (EQ '/. (CAR V94)) (CONSP (CDR V94))
        (CONSP (CDR (CDR V94))) (NULL (CDR (CDR (CDR V94)))))
    (<convert-/.> V94 V93))

  ;; HACK
  ;; ((AND NIL (CONSP V94) (EQ 'let (CAR V94)) (CONSP (CDR V94))
  ;;   (CONSP (CDR (CDR V94))) (CONSP (CDR (CDR (CDR V94))))
  ;;   (NULL (CDR (CDR (CDR (CDR V94))))))
  ;;   (LET* ((V97 (CDR V94)) 
  ;;          (V98 (CAR V97)) 
  ;;          (V99 (CDR V97)))
  ;;     '(LIST 'LET (LIST (LIST V98 (lisp-form V93 (CAR V99))))
  ;;      (lisp-form (CONS V98 V93) (CAR (CDR V99))))
      
  ;;     `(let ,V98 
  ;;       ,(lisp-form V93 (CAR V99)) 
  ;;       ,(lisp-form (CONS V98 V93) (CAR (CDR V99))))
  ;;     )    
  ;;   )

  ((AND (CONSP V94) (EQ 'let (CAR V94)))
    (IF V93
      `(<<let>> (<<inherit>> ,@V93) ,@(CDR V94))
      (CONS '<<let>> (CDR V94))))


  ;; ([:] ...) or ([::] ...) リスト内包表記
  ((AND (CONSP V94) (MEMBER (CAR V94) '(<genlist> <&genlist> <genparlist> <&genparlist> <subparlist>)))
    (LIST* (CASE (CAR V94)
             (<genlist> '<<genlist>>)
             (<&genlist> '<<&genlist>>)
             (<genparlist> '<<genparlist>>)
             (<&genparlist> '<<&genparlist>>)
             (<subparlist> '<<subparlist>>))
           V93 (CDR V94)))
  
  ;; ((AND (CONSP V94) (EQ '<genlist> (CAR V94))) ;; ([:] ...)
  ;;   `(<<genlist>> ,V93 ,@(CDR V94)))
  ;; ((AND (CONSP V94) (EQ '<&genlist> (CAR V94))) ;; ([::] ...)
  ;;   `(<<&genlist>> ,V93 ,@(CDR V94)))

  ((AND (CONSP V94) (MEMBER (CAR V94) '(<<case>>)) (EQL 2 (LIST-LENGTH V94)))
    (LIST (CAR V94) (SECOND V94) V93))
  
  ((AND (CONSP V94) (EQ 'rule (CAR V94)))
   (LET* ((V100 (CDR V94)))
    (LET ((Intersection (intersection V93 (flatten V100))))
     (bld-assoc Intersection (process-rule 'single Intersection V100)))))
  ((AND (CONSP V94) (EQ 'multi (CAR V94)))
   (LET* ((V101 (CDR V94)))
    (LET ((Intersection (intersection V93 (flatten V101))))
      (bld-assoc Intersection (process-rule 'multi Intersection V101)))))

  ;; 構文的マクロの場合は何もしない (マクロ側に全てを任せる)
  ((AND (CONSP V94) (wrapper (macro? (CAR V94)))) V94)

  ((AND (CONSP V94) (CONSP (CDR V94)) (wrapper (element? (CAR V94) V93)))
   (LET* ((V102 (CDR V94)))
    (lisp-form V93 (apcons (LIST 'apply (CAR V94) (CAR V102)) (CDR V102)))))
  ((AND (CONSP V94) (CONSP (CDR V94)) (CONSP (CAR V94)))
   (LET* ((V103 (CDR V94)))
    (lisp-form V93 (apcons (LIST 'apply (CAR V94) (CAR V103)) (CDR V103)))))

  ;; HACK
  ;((AND (CONSP V94) (wrapper (partial-application? (CAR V94) (CDR V94))))
  ; (lisp-form V93 (partial-application V94)))
  ((AND (CONSP V94) (wrapper (partial-application? (CAR V94) (CDR V94))))
    (partial-application (CAR V94)
                         (MAPCAR (LAMBDA (exp) (lisp-form V93 exp))
                                 (CDR V94))))

  ((CONSP V94)
   (CONS (CAR V94)
    (THE LIST (MAPCAR #'(LAMBDA (Y) (lisp-form V93 Y)) (CDR V94)))))
  ((TUPLE-P V94) (|@p| (lisp-form V93 (fst V94)) (lisp-form V93 (snd V94))))
  (T V94)))

(DEFUN bld-assoc (V110 V111)
 (COND ((NULL V110) V111)
  (T (CONS 'PROGN (CONS (LIST 'SETQ '*alist* NIL) (ba-help V110 V111))))))

(DEFUN ba-help (V112 V113)
 (COND
  ((CONSP V112)
   (LET* ((V114 (CAR V112)))
    (CONS (LIST 'PUSH (LIST 'CONS (LIST 'QUOTE V114) V114) '*alist*)
     (ba-help (CDR V112) V113))))
  ((NULL V112) (LIST V113)) (T (f_error 'ba-help))))

(DEFUN intersection (V117 V118)
 (COND ((NULL V117) NIL)
  ((CONSP V117)
   (LET* ((V119 (CAR V117)) (V120 (CDR V117)))
    (if (THE SYMBOL (element? V119 V118)) (CONS V119 (intersection V120 V118))
     (intersection V120 V118))))
  (T (implementation_error 'intersection))))

(DEFUN apcons (V69 V70) (COND ((NULL V70) V69) (T (CONS V69 V70))))

(DEFUN macro? (V71)
 (IF (AND (SYMBOLP V71) (MACRO-FUNCTION V71) (NOT (exempted-macro? V71)))
  'true 'false))

(DEFUN opaque (F)
  (SETQ *exempted-macro* (REMOVE F *exempted-macro*)) 
   F)

(DEFUN transparent (F)
  (SETQ *exempted-macro* (CONS F *exempted-macro*)) 
   F)

(DEFUN exempted-macro? (F) (MEMBER F *exempted-macro*))

;(SETQ *exempted-macro* '(if and or time freeze do prolog? list))

'(DEFUN partial-application? (V72 V73)
 (arity-F-check V72 (arity V72) (LIST-LENGTH V73)))

(DEFUN partial-application? (V72 V73)
  (MULTIPLE-VALUE-BIND (functional-arity max-arity) (arity V72)
    (arity-F-check V72 functional-arity max-arity (LIST-LENGTH V73))))

'(DEFUN arity-F-check (V93 V94 V95)
 (COND ((NULL V94) 'false) 
       ((ABSEQUAL V94 V95) 'false)
       ((EQL V94 -1) 'false)
       ((> V95 V94)
        (output "warning: ~A may not like ~A arguments.~%" V93 V95) 'false)
       (T 'true)))

(DEFUN arity-F-check (V93 functional-arity max-arity V95)
 (COND ((EQL functional-arity V95) 'false)
       ((EQL functional-arity -1) 'false)
       ((< V95 functional-arity) 'true)
       ((EQL max-arity -1) 'false)
       ((> V95 max-arity)
        (output "warning: ~A may not like ~A arguments.~%" V93 V95) 'false)
       (T 'false)))

'(DEFUN partial-application (V96)
 (COND
  ((CONSP V96)
   (CONS 'FUNCALL (CONS (partial-application (CAR V96)) (CDR V96))))
  ((fbound? V96) (closure V96)) 
  (T V96)))

(DEFUN partial-application (fn-name args)
  (LET* ((n-given (LIST-LENGTH args))
         (arity-of-new-function (- (arity fn-name) n-given))
         (tmpvars (FREPLICATE arity-of-new-function #'GENSYM))
         (lambda-exp `(LAMBDA ,tmpvars (,fn-name ,@args ,@tmpvars))))
    (IF (EQL 1 arity-of-new-function)
      lambda-exp
      (LIST 'APPLICABLE lambda-exp))))

(DEFUN fbound? (V26) (NOT (EQL (arity V26) -1)))

(DEFUN closure (V28) (LIST 'FUNCTION (nest-lambdas NIL (arity V28) V28)))

(DEFUN nest-lambdas (V29 V30 V31)
 (COND ((EQL 0 V30) (CONS V31 (REVERSE V29)))
  (T
   (LET ((V (gensym "X")))
    (LIST 'LAMBDA (LIST V) (nest-lambdas (CONS V V29) (1- V30) V31))))))

(DEFUN apply (X Y)
  (IF (> (arity X) 1) 
      (FUNCALL (EVAL (closure X)) Y)   
      (FUNCALL X Y)))
