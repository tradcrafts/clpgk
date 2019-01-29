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

'(DEFUN arity (FUNC) (GETHASH FUNC *arity* -1))

(DEFUN arity (FUNC)
  (COND ((SYMBOLP FUNC) 
          (QUERY-ARITY FUNC))
        (T
          ;(PRINT FUNC)
          -1)))

'(DEFVAR *arity*
 (MAKE-HASH-TABLE :SIZE 300 :REHASH-SIZE 2 :REHASH-THRESHOLD 0.8))

'(DEFUN store_arities (V5883)
 (COND ((NULL V5883) NIL)
  ((AND (CONSP V5883) (CONSP (CDR V5883)))
   (LET* ((V5884 (CDR V5883)))
    (store_arity (CAR V5883) (CAR V5884)) 
    (store_arities (CDR V5884))))
  (T (implementation_error 'store_arities))))

'(DEFUN store_arity (FUNC N)
 (LET ((Arity (arity FUNC)))
  (WHEN (AND (NOT (= -1 Arity)) (NOT (= Arity N)))
    ;;(warn (FORMAT NIL "Changing the arity of '~A' may cause errors~%" FUNC))
    )
  (SETF (GETHASH FUNC *arity*) N)))

(DEFUN store_arity (FUNC N)
  (REGISTER-ARITY FUNC N))

;; (DEFUN warn (String)
;;   (COND ((EQ *strong-warning* 'true) (error String))
;;          (T (FORMAT T "======> Warning: ~A~%" String)))
;;   String)

;; 2015-8-29 JUN hacked
(DEFUN warn (String)
  (COND ((EQ *strong-warning* 'true) (error String))
         (T (WARN "======> Warning: ~A~%" String)))
  String)

(DEFUN strong-warning (Flag) 
  (COND ((EQ Flag '+) (SETQ *strong-warning* 'true))
        ((EQ Flag '-) (SETQ *strong-warning* 'false))
        (T (ERROR "strong-warning expects either + or -~%"))))

(strong-warning '-)         


(REGISTER-ARITY 'and -1) (REGISTER-ARITY 'append 2 -1) (REGISTER-ARITY 'apply 2)
(REGISTER-ARITY 'arity 1) (REGISTER-ARITY 'assoc 2)
(REGISTER-ARITY 'assoc-type 2) (REGISTER-ARITY 'boolean? 1)
;(REGISTER-ARITY 'cd 1)
(REGISTER-ARITY 'character? 1)
;(REGISTER-ARITY 'compile 2)
(REGISTER-ARITY 'complex? 1)
(REGISTER-ARITY 'concat -1) (REGISTER-ARITY 'congruent? 2)
(REGISTER-ARITY 'cons 2) (REGISTER-ARITY 'cons? 1) (REGISTER-ARITY 'declare 2)
(REGISTER-ARITY 'destroy 1)
;(REGISTER-ARITY 'delete-file 1)
(REGISTER-ARITY 'difference 2)
;(REGISTER-ARITY 'dump 1) (REGISTER-ARITY 'echo 1)
(REGISTER-ARITY 'element? 2)
(REGISTER-ARITY 'empty? 1) (REGISTER-ARITY 'eval 1)
(REGISTER-ARITY 'explode 1) (REGISTER-ARITY 'fail-if 2)
(REGISTER-ARITY 'fix 2) (REGISTER-ARITY 'float? 1) (REGISTER-ARITY 'freeze 1)
(REGISTER-ARITY 'fst 1) (REGISTER-ARITY 'gensym 1)
(REGISTER-ARITY 'get-array 3) (REGISTER-ARITY 'get-prop 3)
(REGISTER-ARITY 'qi_> 2) (REGISTER-ARITY 'qi_>= 2) (REGISTER-ARITY 'qi_= 2)
(REGISTER-ARITY 'head 1) (REGISTER-ARITY 'if 3)
(REGISTER-ARITY 'if-with-checking 1) (REGISTER-ARITY 'if-without-checking 1)
(REGISTER-ARITY 'integer? 1) (REGISTER-ARITY 'inferences 1)
(REGISTER-ARITY 'intersection 2) (REGISTER-ARITY 'length 1)
(REGISTER-ARITY 'lineread 0) (REGISTER-ARITY 'load 1) (REGISTER-ARITY 'qi_< 2)
(REGISTER-ARITY 'qi_<= 2) (REGISTER-ARITY 'm-prolog 1)
(REGISTER-ARITY 'make-array 1) (REGISTER-ARITY 'map 2)
(REGISTER-ARITY 'mapcan 2) (REGISTER-ARITY 'maxinferences 1)
(REGISTER-ARITY 'not 1)
(REGISTER-ARITY 'nth 2) (REGISTER-ARITY 'number? 1)
(REGISTER-ARITY 'occurs-check 1) (REGISTER-ARITY 'occurrences 2)
(REGISTER-ARITY 'occurs-check 1) (REGISTER-ARITY 'or -1)
(REGISTER-ARITY 'opaque 1) (REGISTER-ARITY 'print 1)
(REGISTER-ARITY 'profile 1) (REGISTER-ARITY 'profile-results 1)
(REGISTER-ARITY 'put-array 3)
(REGISTER-ARITY 'put-prop 3) (REGISTER-ARITY 'random 1)
(REGISTER-ARITY 'quit 0) (REGISTER-ARITY 'read-char 1)
(REGISTER-ARITY 'read-file-as-charlist 1) (REGISTER-ARITY 'read-file 1)
(REGISTER-ARITY 'read-chars-as-stringlist 2) (REGISTER-ARITY 'rational? 1)
(REGISTER-ARITY 'real? 1) (REGISTER-ARITY 'remove 2)
(REGISTER-ARITY 'reverse 1) (REGISTER-ARITY 'round 1) (REGISTER-ARITY 'save 0)
(REGISTER-ARITY 'snd 1) (REGISTER-ARITY 's-prolog 1)
(REGISTER-ARITY 'specialise 1) (REGISTER-ARITY 'spy 1)
(REGISTER-ARITY 'speed 1) (REGISTER-ARITY 'sqrt 1) (REGISTER-ARITY 'step 1)
(REGISTER-ARITY 'string? 1) (REGISTER-ARITY 'strong-warning 1)
(REGISTER-ARITY 'subst 3) (REGISTER-ARITY 'sugar 3)
(REGISTER-ARITY 'sugarlist 1) (REGISTER-ARITY 'symbol? 1)
(REGISTER-ARITY 'tail 1) (REGISTER-ARITY 'tc 1) (REGISTER-ARITY 'thaw 1)
(REGISTER-ARITY 'time 1) (REGISTER-ARITY 'track 1)
(REGISTER-ARITY 'transparent 1) (REGISTER-ARITY 'tuple? 1)
(REGISTER-ARITY 'tuple 2) (REGISTER-ARITY 'type 1)
(REGISTER-ARITY 'typecheck 3) (REGISTER-ARITY 'unassoc-type 1)
(REGISTER-ARITY 'unprofile 1) (REGISTER-ARITY 'unsugar 1)
(REGISTER-ARITY 'union 2)
(REGISTER-ARITY 'untrack 1) (REGISTER-ARITY 'unspecialise 1)
(REGISTER-ARITY 'value 1) (REGISTER-ARITY 'variable? 1)
(REGISTER-ARITY 'warn 1)
(REGISTER-ARITY 'write-to-file 2) (REGISTER-ARITY 'y-or-n? 1)
(REGISTER-ARITY '+ 2 -1) (REGISTER-ARITY '* 2 -1) (REGISTER-ARITY '/ 2 -1)
(REGISTER-ARITY '- 2 -1)
(REGISTER-ARITY '|@p| 2) (REGISTER-ARITY '|@sv| -1)
(REGISTER-ARITY 'svlen 1) (REGISTER-ARITY 'svref 2)
(REGISTER-ARITY 'preclude 1) (REGISTER-ARITY 'include 1)
(REGISTER-ARITY 'preclude-all-but 1) (REGISTER-ARITY 'include-all-but 1)
(REGISTER-ARITY 'where 2) (REGISTER-ARITY 'closure? 1)
(REGISTER-ARITY 'function? 1) (REGISTER-ARITY 'callable? 1)
(REGISTER-ARITY 'list? 1) (REGISTER-ARITY 'none -1) (REGISTER-ARITY 'only -1)
(REGISTER-ARITY 'qi_/= 2) (REGISTER-ARITY 'unsafeCast 1)
(REGISTER-ARITY '<<xi-simple-fail>> 1) (REGISTER-ARITY 'unsafeFail 1)
(REGISTER-ARITY 'failed? 1) (REGISTER-ARITY 'fork 3) (REGISTER-ARITY 'delay 1)
(REGISTER-ARITY 'force 1) (REGISTER-ARITY '& 1) (REGISTER-ARITY '! 1)
(REGISTER-ARITY 'promise? 1) (REGISTER-ARITY '&cons 2)
(REGISTER-ARITY '&cons! 2) (REGISTER-ARITY 'regex -1) (REGISTER-ARITY '$ 2)
(REGISTER-ARITY '$? 2)
(REGISTER-ARITY 'zip -1) (REGISTER-ARITY 'unzip 1)


