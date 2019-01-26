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

(DEFVAR *teststack*)
(DEFVAR *<proxy-vars>*)
(DEFUN <new-proxy-var> (&OPTIONAL (tag "TV"))
  (FLET ((new () (CONS NIL (GENSYM tag))))
    (AIF (FIND T *<proxy-vars>* :KEY #'CAR)
         (PROGN (SETF (CAR IT) NIL)
                (CDR IT))
         (PROGN (SETQ *<proxy-vars>* (CONS (new) *<proxy-vars>*))
                (CDAR *<proxy-vars>*)))))
(DEFUN <reset-proxy-vars> ()
  (MAPC (LAMBDA (C) (SETF (CAR C) T))
        *<proxy-vars>*))
(DEFUN <get-proxy-vars-list> ()
  ;(IF *<proxy-vars>* (FORMAT T "PROXY-VARS: LEN: ~A~%" (LENGTH *<proxy-vars>*)))
  (MAPCAR #'CDR *<proxy-vars>*))

(DEFUN <allocate-ignoreVar> ()
  (LET ((var (GENSYM "I")))
    (SETF (GET var 'ignore?) T)
    ;(PUSH var *<ignoreVars>*)
    var))
(DEFUN <is-ignoreVar?> (x)
  ;;(MEMBER x *<ignoreVars>*)
  (GET x 'ignore?)
  )

(DEFMACRO <<delayed>> (exp) (LIST 'FORCE exp))

(DEFUN <as-variable?> (x)
  (OR (wrapper (variable? x))
      (AND (CONSP x) (EQ '<<delayed>> (FIRST x)))))


(DEFMACRO define (F &REST Def)   
  ;; HACK
  ;;(WARN "define:")
  `(compile_qi (QUOTE ,F) (QUOTE ,Def)))

(DEFUN compile_qi (F Def)
  (LET ((ErrString (FORMAT NIL "syntax error in ~A" F)))
       (compile '<define> (CONS F Def) ErrString)))

(DEFMACRO fun (&REST Rules) `(compile_fun (QUOTE ,Rules)))

(DEFUN compile_fun (V88)
 (LET ((ErrString (FORMAT NIL "syntax error in ~{~S ~}" V88)))
  (LET ((Anon (gensym "anon")))
   (LET ((Func (compile '<define> (CONS Anon V88) ErrString)))
    Anon))))

(DEFUN <define> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<name> (<name> Stream)))
         ;; HACK
         ;;(WARN "name=~W" <name>)
    (IF (NOT (failure? <name>))
     (LET ((<signature> (<signature> <name>)))
         ;; HACK 型情報無しの場合は <signature> == NIL
         ;;(FORMAT T "sig=~W~%" <signature>)
      (IF (NOT (failure? <signature>))
       (LET ((<rules> (<rules> <signature>)))
         ;; HACK
         ;;(FORMAT T "rules=~W~%" <rules>)
        (IF (NOT (failure? <rules>))
         (LIST (FIRST <rules>)
          (compile_to_machine_code (SECOND <name>) (SECOND <rules>)))
         NIL))
       NIL))
     NIL)))
  (BLOCK localfailure
   (LET ((<name> (<name> Stream)))
     ;;(FORMAT T "name=~W~%" <name>)
    (IF (NOT (failure? <name>))
     (LET ((<rules> (<rules> <name>)))
       ;;(FORMAT T "rules=~W~%" <rules>)
      (IF (NOT (failure? <rules>))
       (LIST (FIRST <rules>)
        (compile_to_machine_code (SECOND <name>) (SECOND <rules>)))
       NIL))
     NIL)))))

(DEFUN <name> (Stream)
 (OR
  (BLOCK localfailure
   (IF (CONSP (FIRST Stream))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
     (if (and (symbol? (CAAR Stream)) (not (sysfunc? (CAAR Stream))))
      (SETQ *currfunc* (CAAR Stream))
      (ERROR "~A is not a legitimate functor." (CAAR Stream))))
    NIL))))

(DEFUN sysfunc? (F) (element? F *sysfuncs*))

(DEFUN <signature> (Stream)
 (OR
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQ (FIRST (FIRST Stream)) '{))
    (LET
     ((<signature-help>
       (<signature-help> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <signature-help>))
      (IF
       (AND (CONSP (FIRST <signature-help>))
        (EQ (FIRST (FIRST <signature-help>)) '}))
       (LIST
        (FIRST
         (LIST (REST (FIRST <signature-help>)) (SECOND <signature-help>)))
        (SECOND <signature-help>))
       NIL)
      NIL))
    NIL))))

(DEFUN <signature-help> (Stream)
 (OR
  (BLOCK localfailure
   (IF (CONSP (FIRST Stream))
    (LET
     ((<signature-help>
       (<signature-help> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <signature-help>))
      (LIST (FIRST <signature-help>)
       (if (element? (CAAR Stream) (CONS '{ (CONS '} NIL)))
        (RETURN-FROM localfailure NIL)
        (CONS (CAAR Stream) (SECOND <signature-help>))))
      NIL))
    NIL))
  (BLOCK localfailure
   (LET ((<e> (<e> Stream)))
    (IF (NOT (failure? <e>)) (LIST (FIRST <e>) NIL) NIL)))))

(DEFUN <rules> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<rule> (<rule> Stream)))
    (IF (NOT (failure? <rule>))
     (LET ((<rules> (<rules> <rule>)))
      (IF (NOT (failure? <rules>))
       (LIST (FIRST <rules>) (CONS (SECOND <rule>) (SECOND <rules>))) NIL))
     NIL)))
  (BLOCK localfailure
   (LET ((<rule> (<rule> Stream)))
    (IF (NOT (failure? <rule>))
     (LIST (FIRST <rule>) (CONS (SECOND <rule>) NIL)) NIL)))))

(DEFUN <rule> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<patterns> (<patterns> Stream)))
    (IF (NOT (failure? <patterns>))
     (IF (AND (CONSP (FIRST <patterns>)) (EQ (FIRST (FIRST <patterns>)) '->))
      (LET
       ((<action>
         (<action> (LIST (REST (FIRST <patterns>)) (SECOND <patterns>)))))
       (IF (NOT (failure? <action>))
        (IF (AND (CONSP (FIRST <action>)) (EQ (FIRST (FIRST <action>)) 'where))
         (LET
          ((<guard>
            (<guard> (LIST (REST (FIRST <action>)) (SECOND <action>)))))
          (IF (NOT (failure? <guard>))
           (LIST (FIRST <guard>)
            (CONS (SECOND <patterns>)
             (CONS
              (CONS 'where
               (CONS (SECOND <guard>) (CONS (SECOND <action>) NIL)))
              NIL)))
           NIL))
         NIL)
        NIL))
      NIL)
     NIL)))
  (BLOCK localfailure
   (LET ((<patterns> (<patterns> Stream)))
    (IF (NOT (failure? <patterns>))
     (IF (AND (CONSP (FIRST <patterns>)) (EQ (FIRST (FIRST <patterns>)) '->))
      (LET
       ((<action>
         (<action> (LIST (REST (FIRST <patterns>)) (SECOND <patterns>)))))
       (IF (NOT (failure? <action>))
        (LIST (FIRST <action>)
         (CONS (SECOND <patterns>) (CONS (SECOND <action>) NIL)))
        NIL))
      NIL)
     NIL)))
  (BLOCK localfailure
   (LET ((<patterns> (<patterns> Stream)))
    (IF (NOT (failure? <patterns>))
     (IF (AND (CONSP (FIRST <patterns>)) (EQ (FIRST (FIRST <patterns>)) '<-))
      (LET
       ((<action>
         (<action> (LIST (REST (FIRST <patterns>)) (SECOND <patterns>)))))
       (IF (NOT (failure? <action>))
        (IF (AND (CONSP (FIRST <action>)) (EQ (FIRST (FIRST <action>)) 'where))
         (LET
          ((<guard>
            (<guard> (LIST (REST (FIRST <action>)) (SECOND <action>)))))
          (IF (NOT (failure? <guard>))
           (LIST (FIRST <guard>)
            (CONS (SECOND <patterns>)
             (CONS
              (CONS 'where
               (CONS (SECOND <guard>) (CONS (bld_back (SECOND <action>)) NIL)))
              NIL)))
           NIL))
         NIL)
        NIL))
      NIL)
     NIL)))
  (BLOCK localfailure
   (LET ((<patterns> (<patterns> Stream)))
    (IF (NOT (failure? <patterns>))
     (IF (AND (CONSP (FIRST <patterns>)) (EQ (FIRST (FIRST <patterns>)) '<-))
      (LET
       ((<action>
         (<action> (LIST (REST (FIRST <patterns>)) (SECOND <patterns>)))))
       (IF (NOT (failure? <action>))
        (LIST (FIRST <action>)
         (CONS (SECOND <patterns>) (CONS (bld_back (SECOND <action>)) NIL)))
        NIL))
      NIL)
     NIL)))))

(DEFUN bld_back (V1)
 (LET ((Guard (LIST 'succeeds? (LIST 'set '*backtrack* V1))))
  (LET ((NewAction (LIST 'value '*backtrack*)))
   (LIST 'where Guard NewAction))))

(DEFUN succeeds? (X)
   (IF (EQL X #\Escape)
       'false
       'true))

(DEFUN fail-if (F X) (IF (EQ (FUNCALL F X) 'true)
                         #\Escape
                         X))

;; Added [2018-06-24]
(DEFUN appliable? (x)
  (OR (SYMBOLP x)
      (is_xi_internal_lambda? x)))
(DEFUN get-appliable (x &KEY raw)
  (COND ((SYMBOLP x) x)
        ((is_xi_internal_lambda? x) (IF raw x (get_xi_internal_lambda_def x)))
        (T (ERROR "get-appliable: internal error"))))

(DEFUN <patterns> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<pattern> (<pattern> Stream)))
    (IF (NOT (failure? <pattern>))
     (LET ((<patterns> (<patterns> <pattern>)))
      (IF (NOT (failure? <patterns>))
       (LIST (FIRST <patterns>)
        (CONS (ch-esc (SECOND <pattern>)) (SECOND <patterns>)))
       NIL))
     NIL)))
  (BLOCK localfailure
   (LET ((<e> (<e> Stream)))
    (IF (NOT (failure? <e>)) (LIST (FIRST <e>) NIL) NIL)))))

(DEFUN <pattern> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<start_of_list> (<start_of_list> Stream)))
    (IF (NOT (failure? <start_of_list>))
     (IF
      (AND (CONSP (FIRST <start_of_list>))
       (EQ (FIRST (FIRST <start_of_list>)) '|@p|))
      (LET
       ((<pattern1>
         (<pattern1>
          (LIST (REST (FIRST <start_of_list>)) (SECOND <start_of_list>)))))
       (IF (NOT (failure? <pattern1>))
        (LET ((<pattern2> (<pattern2> <pattern1>)))
         (IF (NOT (failure? <pattern2>))
          (LET ((<end_of_list> (<end_of_list> <pattern2>)))
           (IF (NOT (failure? <end_of_list>))
            (LIST (FIRST <end_of_list>)
             (CONS '|@p|
              (CONS (SECOND <pattern1>) (CONS (SECOND <pattern2>) NIL))))
            NIL))
          NIL))
        NIL))
      NIL)
     NIL)))
  (BLOCK localfailure
   (LET ((<start_of_list> (<start_of_list> Stream)))
    (IF (NOT (failure? <start_of_list>))
     (IF
      (AND (CONSP (FIRST <start_of_list>))
       (EQ (FIRST (FIRST <start_of_list>)) 'cons))
      (LET
       ((<pattern1>
         (<pattern1>
          (LIST (REST (FIRST <start_of_list>)) (SECOND <start_of_list>)))))
       (IF (NOT (failure? <pattern1>))
        (LET ((<pattern2> (<pattern2> <pattern1>)))
         (IF (NOT (failure? <pattern2>))
          (LET ((<end_of_list> (<end_of_list> <pattern2>)))
           (IF (NOT (failure? <end_of_list>))
            (LIST (FIRST <end_of_list>)
             (CONS 'cons
              (CONS (SECOND <pattern1>) (CONS (SECOND <pattern2>) NIL))))
            NIL))
          NIL))
        NIL))
      NIL)
     NIL)))

  ;;; TEST HACK [2018-06-18] JUN[2018-06-13]
  (BLOCK localfailure
    (LET ((<start_of_list> (<start_of_list> Stream)))
      (WHEN (AND (NOT (failure? <start_of_list>))
                 (CONSP (FIRST <start_of_list>))
                 (MEMBER (FIRST (FIRST <start_of_list>))
                         '(|@sv| |@sv*| |@| fork => type -> view when unless WHEN UNLESS
                           ! force & delay == != === !== QUOTE
                           and or none not qi_< qi_<= qi_> qi_>= /.)))
        (LET ((operator (FIRST (FIRST <start_of_list>))))
                                        ;(PRINT <start_of_list>)
          (AWHEN (POSITION '|-end-of-list-| (CAR <start_of_list>))
            (LET* ((arity (1- IT))
                   (args (SUBSEQ (CAR <start_of_list>) 1 IT))
                   (ptns
                    (CASE operator
                      ((=> type) (UNLESS (AND (<= 1 arity) (SYMBOLP (FIRST args)))
                                   (ERROR "required (type <TypeName> <Pattern>)"))
                        
                        ;; 型名はベクタとしてエスケープする
                        (SETF (FIRST args) (VECTOR (FIRST args)))
                        (CDR args))
                      ((-> view) (UNLESS (AND (<= 2 arity) (appliable? (FIRST args)))
                                   (ERROR "required (view <FunctionName> <Pattern>)"))
                        ;; 関数名はベクタとしてエスケープする
                        (SETF (FIRST args) (VECTOR (get-appliable (FIRST args))))
                        (CDR args))
                      ((|@| fork) (UNLESS (<= 2 arity)
                                  (ERROR "required (fork <Pattern/1> <Pattern/2> ...)"))
                        args)
                      ((when unless WHEN UNLESS)
                        (UNLESS (AND (<= 1 arity) (appliable? (FIRST args)))
                          (ERROR "required (~A <PredicateName>)" (FIRST args)))
                        ;; 述語はベクタとしてエスケープする
                        (SETF (FIRST args) (VECTOR (get-appliable (FIRST args))))
                        (CDR args)
                        )
                      (QUOTE
                        (UNLESS (EQL 1 arity)
                          (ERROR "Xi: illegal QUOTE form ~A" args))
                        ;; クオートされた値はベクタとしてエスケープする
                        (SETF (FIRST args) (VECTOR (FIRST args)))
                        (CDR args)
                        )
                      ((|@sv| |@sv*|) args)
                      ((! force) (UNLESS (<= 1 arity)
                                   (ERROR "required (force <Pattern/1> ...)"))
                        (SETQ operator 'view
                              args (CONS #(force) args))
                        (CDR args))
                      ((& delay) (UNLESS (AND (= 1 arity) (wrapper (variable? (FIRST args))))
                                   (ERROR "required (delay <Pattern> <Variable>)"))
                        args)
                      ((== != === !== and or none not < <= > >= /. QUOTE)
                        (LET ((whole-form (CONS operator args)))
                          (UNLESS (<valid_complex_pattern?> whole-form)
                            (ERROR "required (~A ~A)" operator args))
                          (SETQ args (LIST (VECTOR whole-form))
                                operator '<complex>)
                          args
                        ))
                      )))
              ;;(FORMAT T "dbg0: ptns = ~A~%" ptns)
              (LET* ((m (LIST-LENGTH ptns))
                    (scaned
                      (WHEN ptns
                        ;;(FORMAT T "dbg1: ~A~%" ptns)
                        (LET ((m (LIST-LENGTH ptns))
                              (<pat> (<pattern1> (LIST ptns (SECOND <start_of_list>))))
                              tmp)
                          ;;(FORMAT T "dbg2-1: ~A~%" <pat>)
                          ;;(FORMAT T "dbg2: ~A~%" (SECOND <pat>))
                          (PUSH (SECOND <pat>) tmp)
                          (DOTIMES (i (1- m) (NREVERSE tmp))
                            (SETQ <pat> (<pattern1> <pat>))
                            ;;(FORMAT T "dbg3: ~A~%" (SECOND <pat>))
                            (PUSH (SECOND <pat>) tmp)))))
                    (new-args (NCONC (NBUTLAST args m) scaned)))
                    
                      
                      (LIST (NTHCDR (1+ IT) (CAR <start_of_list>))
                            (CONS operator new-args)))))))))
  ;;; TEST END HACK

  ;;; HACK JUN[2018-06-13]
  ;; (BLOCK localfailure
  ;;   (LET ((<start_of_list> (<start_of_list> Stream)))
  ;;     (WHEN (AND (NOT (failure? <start_of_list>))
  ;;                (CONSP (FIRST <start_of_list>))
  ;;                (MEMBER (FIRST (FIRST <start_of_list>))
  ;;                        '(@sv @sv* & fork => type -> view when unless WHEN UNLESS)))
  ;;       (LET ((operator (FIRST (FIRST <start_of_list>))))
  ;;                                       ;(PRINT <start_of_list>)
  ;;         (AWHEN (POSITION '|-end-of-list-| (CAR <start_of_list>))
  ;;           (LET ((arity (1- IT))
  ;;                 (args (SUBSEQ (CAR <start_of_list>) 1 IT)))
  ;;             (CASE operator
  ;;               ((=> type) (UNLESS (AND (<= 1 arity) (SYMBOLP (FIRST args)))
  ;;                       (ERROR "required (type <TypeName> <Pattern>)")))
  ;;               ((-> view) (UNLESS (AND (<= 2 arity) (SYMBOLP (FIRST args)))
  ;;                       (ERROR "required (view <FunctionName> <Pattern>)")))
  ;;               ((& fork) (UNLESS (<= 2 arity)
  ;;                       (ERROR "required (fork <Pattern/1> <Pattern/2> ...)")))
  ;;               ((when unless WHEN UNLESS)
  ;;                 (UNLESS (AND (<= 1 arity) (SYMBOLP (FIRST args)))
  ;;                   (ERROR "required (~A <PredicateName>)" (FIRST args)))
  ;;                 ;; 述語はベクタとしてエスケープする
  ;;                 (SETF (FIRST args) (VECTOR (FIRST args)))
  ;;                 )
  ;;               ((@sv @sv*))
  ;;               )
  ;;             (LIST (NTHCDR (1+ IT) (CAR <start_of_list>))
  ;;                   (CONS operator args))))))))
  ;;; END HACK

  (BLOCK localfailure
   (IF (CONSP (FIRST Stream))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
     (if (cons? (CAAR Stream))
      (ERROR "~A is not a legitimate constructor~%" (CAAR Stream))
      (RETURN-FROM localfailure NIL)))
    NIL))
  (BLOCK localfailure
   (LET ((<simple_pattern> (<simple_pattern> Stream)))
    (IF (NOT (failure? <simple_pattern>)) <simple_pattern> NIL)))))

(DEFUN <simple_pattern> (Stream)
  (OR
    ;; HACK [2018-06-16]
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQ (FIRST (FIRST Stream)) '_))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (<allocate-ignoreVar>))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQ (FIRST (FIRST Stream)) '_))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (gensym "X"))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\Escape))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (LIST 'esc))
    NIL))
  (BLOCK localfailure
   (IF (CONSP (FIRST Stream))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
     (IF (MEMBER (CAAR Stream) '(-> <- _))
      (RETURN-FROM localfailure NIL) (CAAR Stream)))
    NIL))))

(DEFUN <pattern1> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<pattern> (<pattern> Stream)))
    (IF (NOT (failure? <pattern>)) <pattern> NIL)))))

(DEFUN <pattern2> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<pattern> (<pattern> Stream)))
    (IF (NOT (failure? <pattern>)) <pattern> NIL)))))

(DEFUN <action> (Stream)
 (OR
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\Escape))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (LIST 'esc))
    NIL))
  (BLOCK localfailure
   (IF (CONSP (FIRST Stream))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (CAAR Stream))
    NIL))))

(DEFUN ch-esc (V83)
 (COND ((AND (CONSP V83) (EQ 'esc (CAR V83)) (NULL (CDR V83))) (esc)) (T V83)))

;; hack [2018-09-17] 便宜上defconstantからdefvarに変更。
;; 定数であることには変わりないが、SBCLで再コンパイルする際にエラーが頻発するため
(DEFVAR *failure-object* #\Escape)

(DEFUN <guard> (Stream)
 (OR
  (BLOCK localfailure
   (IF (CONSP (FIRST Stream))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (CAAR Stream))
    NIL))))

(DEFVAR *<definition-only-p>* NIL)

;; V85=関数名 V86=定義のリスト
(DEFUN compile_to_machine_code (V85 V86)
  ;(PRINT (LIST V85 V86))
  (LET ((Lambda+ (compile-to-lambda+ V85 V86)))
    ;; HACK
    ;(FORMAT T "DEBUG: Lambda+=~W~%" Lambda+)
    (LET ((Lisp (compile-to-lisp V85 Lambda+)))
      ;; HACK
      ;(FORMAT T "DEBUG: Lisp=~W~%" Lisp)
      ;;(LET ((Store (record_source V85 Lisp)))

      ;; HACK Jun
      (LET ((Store (WHEN (EQL 'DEFUN (FIRST Lisp))
                     (record_source V85 Lisp))))
        (IF *<definition-only-p>*
          (SETQ *<definition-only-p>* Lisp)
          (COMPILE (EVAL Lisp)))
        ))))

;; Hacked JUN キーパラメータによる動作の抑制
(DEFUN compile-to-lambda+ (V1 V2 &KEY arity (chkFreeVar NIL))
 ;;(LET ((Arity (aritycheck V1 V2)))
 ;; (LET ((Free (MAPCAR #'(LAMBDA (Rule) (free-variable-check V1 Rule)) V2)))
 (LET ((Arity (IF arity arity (aritycheck V1 V2))))
  (LET ((Free (WHEN chkFreeVar (MAPCAR #'(LAMBDA (Rule) (free-variable-check V1 Rule)) V2))))
   (LET ((Variables (parameters Arity)))
     (LET ((Linear (MAPCAR 'linearise V2)))
     (LET ((Abstractions (MAPCAR 'abstract-rule Linear)))
       ;(PRINT Abstractions)
      (LET
       ((Applications
          (MAPCAR #'(LAMBDA (X) (application_build Variables X)) Abstractions)))
        ;(FORMAT T "LAMBDA+ = ~W~%" (LIST Variables Applications))
       (LIST Variables Applications))))))))

         
(DEFUN free-variable-check (V3 V4)
 (COND
  ((AND (CONSP V4) (CONSP (CDR V4)) (NULL (CDR (CDR V4))))
   (LET ((Bound (extract-vars (CAR V4))))
    (LET ((Free (extract-free-vars Bound (CAR (CDR V4)))))
     (free-variable-warnings V3 Free))))
  (T (implementation_error 'free-variable-check))))

(DEFUN extract-vars (V5)
 (COND ((wrapper (variable? V5)) (LIST V5))
  ((CONSP V5)
   (THE LIST (union (extract-vars (CAR V5)) (extract-vars (CDR V5)))))
  (T NIL)))

(DEFUN extract-free-vars (V7 V8)
 (COND
  ((AND (wrapper (variable? V8)) (NOT (wrapper (element? V8 V7))))
   (LIST V8))
  ((AND (CONSP V8) (EQ (CAR V8) 'rule)) NIL)
  ((AND (CONSP V8) (EQ '/. (CAR V8)) (CONSP (CDR V8)) (CONSP (CDR (CDR V8)))
    (NULL (CDR (CDR (CDR V8)))))
   (LET* ((V9 (CDR V8)))
    (extract-free-vars (CONS (CAR V9) V7) (CAR (CDR V9)))))
  ((AND (CONSP V8) (EQ 'let (CAR V8)) (CONSP (CDR V8)) (CONSP (CDR (CDR V8)))
    (CONSP (CDR (CDR (CDR V8)))) (NULL (CDR (CDR (CDR (CDR V8))))))
   (LET* ((V10 (CDR V8)) (V11 (CDR V10)))
    (THE LIST
     (union (extract-free-vars V7 (CAR V11))
      (extract-free-vars (CONS (CAR V10) V7) (CAR (CDR V11)))))))
   ((CONSP V8) (union (extract-free-vars V7 (CAR V8)) (extract-free-vars V7 (CDR V8))))
  (T NIL)))

(DEFUN free-variable-warnings (V15 V16)
 (IF (NULL V16) '_
     (warn (FORMAT NIL "The following variables are free in ~A: ~{~A, ~}~%" V15 V16)))) 

(DEFUN linearise (V2)
 (COND
  ((AND (CONSP V2) (CONSP (CDR V2)) (NULL (CDR (CDR V2))))
   (LET* ((V3 (CAR V2))) (linearise-help (flatten V3) V3 (CAR (CDR V2)))))
  (T (implementation_error 'linearise))))

(DEFUN flatten (V4)
 (COND ((NULL V4) NIL)
       ((CONSP V4) (APPEND (flatten (CAR V4)) (flatten (CDR V4))))
       (T (LIST V4)))) 
  
(DEFUN linearise-help (V5 V6 V7)
 (COND ((NULL V5) (LIST V6 V7))
  ((CONSP V5)
   (LET* ((V8 (CAR V5)) (V9 (CDR V5)))
    (if
     (THE SYMBOL
      (and (THE SYMBOL (variable? V8)) (THE SYMBOL (element? V8 V9))))
     (LET ((Var (gensym "X")))
      (LET ((NewAction (LIST 'where (LIST 'qi_= V8 Var) V7)))
       (LET ((NewPatts (linearise-X V8 Var V6)))
        (linearise-help V9 NewPatts NewAction))))
     (linearise-help V9 V6 V7))))
  (T (implementation_error 'linearise-help))))

(DEFUN linearise-X (V23 V24 V25)
 (COND ((ABSEQUAL V23 V25) V24)
  ((CONSP V25)
   (LET* ((V26 (CAR V25)) (V27 (CDR V25)))
    (LET ((L (linearise-X V23 V24 V26)))
     (if (qi_= L V26) (CONS V26 (linearise-X V23 V24 V27)) (CONS L V27)))))
  (T V25)))

(DEFUN aritycheck (V40 V41)
 (COND
  ((AND (CONSP V41) (CONSP (CAR V41)) (NULL (CDR V41)))
   (LIST-LENGTH (CAR (CAR V41))))
  ((AND (CONSP V41) (CONSP (CAR V41)) (CONSP (CDR V41))
    (CONSP (CAR (CDR V41))))
   (LET* ((V42 (CDR V41)) (V43 (CAR V42)) (V44 (CAR V43)))
    (if (qi_= (LIST-LENGTH (CAR (CAR V41))) (LIST-LENGTH V44))
     (aritycheck V40 (CONS (CONS V44 '_) (CDR V42)))
     (ERROR "arity error in ~A~%" V40))))
  (T (implementation_error 'aritycheck))))

(DEFUN abstract-rule (V12)
 (COND
   ((AND (CONSP V12) (CONSP (CDR V12)) (NULL (CDR (CDR V12))))
     (abstraction_build (CAR V12) (CAR (CDR V12))))
   (T (implementation_error 'abstract-rule))))

(DEFUN abstraction_build (V50 V51)
 (COND ((NULL V50) V51)
       ((CONSP V50) (LIST '/. (CAR V50) (abstraction_build (CDR V50) V51)))
       (T (implementation_error 'abstraction_build))))

(DEFUN parameters (V52)
 (COND ((EQL 0 V52) NIL)
       (T (CONS (THE SYMBOL (gensym "V")) (parameters (1- V52))))))

(DEFUN application_build (V53 V54)
 (COND ((NULL V53) V54)
       ((CONSP V53) (application_build (CDR V53) (LIST V54 (CAR V53))))
       (T (implementation_error 'application_build))))


;; TEST
(DEFMACRO <<case>> (&WHOLE whole fake-string &OPTIONAL inherits  &REST err)
  ;;(WHEN (EQ hack 'not-passed) (PRINT (fake-string-info fake-string)))
  ;(FORMAT T "<DEBUG:case-whole>=~W~%" whole)
  (LET* ((raw (fake-string-info fake-string))
         (source-exps (FIRST raw))
         (arity (LIST-LENGTH source-exps))
         (rule-body (REST raw))
         (<name> `(,rule-body dummy-name))
         (<rules> (<rules> <name>))
         (input (SECOND <rules>))
         (lambda+ (compile-to-lambda+ NIL input :arity arity :chkFreeVar NIL))
         (args (FIRST lambda+))
         (*teststack* NIL) ;; <-- added [2018-06-16]
         (*<proxy-vars>* NIL) ;; <-- added [2018-06-16]
         (defbody (MAPCAR 'reduce (SECOND lambda+)))
         (CondExpression (cond-expression ''functionname! (APPEND args inherits) defbody))
         (source-forms (MAPCAR (LAMBDA (exp) (lisp-form inherits exp)) source-exps))
         (lisp-code `(LET ,(MAPCAR #'LIST args source-forms)
                       (DECLARE (IGNORABLE ,@args))
                       ,CondExpression)))

    ;; 注意 core-06.lisp のlisp-form関数でクオート処理が行われる
    ;; 変数をクオートする箇所をコメントアウトすることで、とりあえず動くようになった
    
    ;;(FORMAT T "<DEBUG:rules>=~W~%" defbody)
    ;;(FORMAT T "<DEBUG:args>=~W~%" args)
    ;;(FORMAT T "<DEBUG:cmp>=~W~%" CondExpression)
    ;;(FORMAT T "<DEBUG:CODE>=~W~%" lisp-code )

    lisp-code
  
  )
    ;'''(NIL NIL)
  )


(DEFPARAMETER *<<test-index>>* NIL)

(DEFMACRO <<case/where>> (&WHOLE whole fake-string &OPTIONAL inherits &REST err)
  ;;(FORMAT T "~%<DEBUG:case/where-whole>=~W~%" whole)
  `(LET ((<<caseIndex>> *<<test-index>>*))
     (<<case>> ,fake-string ,inherits)))


(DEFVAR *<LOCALLY-LET-OP>* NIL)
;;(DEFVAR *<NEED-PKG-DEFUN-P>* NIL)
(DEFVAR *<DEF-OP>* 'DEFUN)

(DEFUN to_pkg_symbol (sym)
  (INTERN (STRING-UPCASE (SYMBOL-NAME sym))))

; compile-to-lisp
;; (DEFUN 関数名...)形式の生成
(DEFUN compile-to-lisp (V290 V291 &AUX (normal? (NOT (EQ 'xi_lambda V290))))
  ;; V290 : 関数名
  ;; V291 = (変数リスト (各定義のXiラムダ表現...))
  ;;(FORMAT T "DEBUG: compile-to-lisp: ~W~%" V291)
  (LET (restore-arity)
    (UNWIND-PROTECT
      (COND
        ((AND (CONSP V291) (CONSP (CDR V291)) (NULL (CDR (CDR V291))))
          (LET* ((V292 (CAR V291)))
            (LET ((Arity (LIST-LENGTH V292)))
              ;; JUN Hack
              (WHEN normal?
                (WHEN *<LOCALLY-LET-OP>* (SETQ restore-arity (arity V290)))
                (store_arity V290 Arity))
              (LET* ((*teststack* NIL) ;; <-- added [2018-06-16]
                     (*<proxy-vars>* NIL) ;; <-- added [2018-06-16]
                     (Reduce (MAPCAR 'reduce (CAR (CDR V291)))))
                                        ;(FORMAT T "DEBUG: compile-to-lisp - Reduce=~W~%" Reduce)
                (LET ((CondExpression (cond-expression V290 V292 Reduce)))
                  (LET* ((DefName (IF *<LOCALLY-LET-OP>*
                                           (to_pkg_symbol V290)
                                           V290))
                         (DefOperator *<DEF-OP>*)
                         (Lisp (LIST 'DEFUN DefName V292 CondExpression))
                         (optLisp (optimise-lisp *speed* Lisp))
                         (defbody (CDDR optLisp)))


                    
                    (WHEN *<LOCALLY-LET-OP>* ;; FLET or LABELS
                      (SETQ defbody `(,V292
                                       (,*<LOCALLY-LET-OP>* ((,V290 ,@defbody))
                                         (,V290 ,@V292)))))
                                                       
                                              
                                       
                    (IF normal?
                      (IF DefOperator
                        (LIST* DefOperator DefName defbody)
                        (LIST* DefName defbody))
                      (LIST* 'LAMBDA defbody))
                                        ;(FORMAT T "DEBUG: compile-to-lisp - Lisp=~W~%" Lisp)
                    ))))))
        (T (implementation_error 'compile-to-lisp)))

      ;; ローカル定義の場合にはarity情報を巻き戻す
      (WHEN restore-arity
        (store_arity V290 restore-arity)
        )
      )))

;; Jun Added NOW TESTING...


(DEFUN <escape-special-clause> (xs)
  (COND
    ((AND (CONSP xs) (MEMBER (FIRST xs) '(<<case>>
                                          <<case/where>>
                                          ;<<tests/case/where>>
                                          ;<<check/case/where>>
                                          ) ))
      (SETF (CDR xs) (LIST (make-fake-string (CDR xs)))))
    ((CONSP xs) (MAPC #'<escape-special-clause> xs)))
  xs)


(DEFUN reduce (V293)
  (SETQ *teststack* NIL) ;; これはこのままでないと動作がおかしくなる
  (<reset-proxy-vars>)

  
  (LET ((Result (reduce-help V293)))
    ;;(FORMAT T "DEBUG: reduce=~W~%" Result)
    ;(<escape-case-clause> Result)
    (<escape-special-clause> (LIST (CONS ':tests (REVERSE *teststack*)) Result))))

(DEFUN <error/checkBoolean> (x exp)  (ERROR "Xi: ~A is not a boolean. ~A" x exp))
(DEFMACRO <checkBoolean> (exp)
  `(LET ((result ,exp))
     (IF (OR (EQ 'true result) (EQ 'false result))
       result
       (<error/checkBoolean> result ',exp))))
(DEFMACRO <cl-to-xi-boolean> (cl-exp)  `(IF ,cl-exp 'true 'false))

(DEFUN reduce-help (V294)
  ;(FORMAT T "DEBUG: reduce-help=~W~%" V294)
  (COND
    ((AND (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
          (CONSP (CDR (CAR V294))) (CONSP (CAR (CDR (CAR V294))))
          (EQ 'cons (CAR (CAR (CDR (CAR V294)))))
          (CONSP (CDR (CAR (CDR (CAR V294)))))
          (CONSP (CDR (CDR (CAR (CDR (CAR V294))))))
          (NULL (CDR (CDR (CDR (CAR (CDR (CAR V294)))))))
          (CONSP (CDR (CDR (CAR V294)))) (NULL (CDR (CDR (CDR (CAR V294)))))
          (CONSP (CDR V294)) (NULL (CDR (CDR V294))))
      (LET*
          ((V295 (CDR V294)) (V296 (CAR V294)) (V297 (CDR V296)) (V298 (CAR V297))
           (V299 (CDR V298)))
        (add-test (CONS 'CONSP V295))
        (LET
            ((Abstraction
               (LIST '/. (CAR V299)
                     (LIST '/. (CAR (CDR V299)) (ebr (CAR V295) V298 (CAR (CDR V297)))))))
          (LET
              ((Application
                 (LIST (LIST Abstraction (CONS '&CAR V295)) (CONS '&CDR V295))))
            (reduce-help Application)))))
    ((AND (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
          (CONSP (CDR (CAR V294))) (CONSP (CAR (CDR (CAR V294))))
          (EQ '|@p| (CAR (CAR (CDR (CAR V294))))) (CONSP (CDR (CAR (CDR (CAR V294)))))
          (CONSP (CDR (CDR (CAR (CDR (CAR V294))))))
          (NULL (CDR (CDR (CDR (CAR (CDR (CAR V294)))))))
          (CONSP (CDR (CDR (CAR V294)))) (NULL (CDR (CDR (CDR (CAR V294)))))
          (CONSP (CDR V294)) (NULL (CDR (CDR V294))))
      ;; タプル
      ;(PRINT V294)

      (LET*
          ((V300 (CDR V294)) (V301 (CAR V294)) (V302 (CDR V301)) (V303 (CAR V302))
           (V304 (CDR V303)))
        (add-test (CONS 'TUPLE-P V300))
        (LET
            ((Abstraction
               (LIST '/. (CAR V304)
                     (LIST '/. (CAR (CDR V304)) (ebr (CAR V300) V303 (CAR (CDR V302)))))))
          (LET
              ((Application
                 (LIST (LIST Abstraction (CONS 'fst V300)) (CONS 'snd V300))))
            (reduce-help Application)))))

    ;; JUN HACK
    ((AND  (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
           (CONSP (CDR (CAR V294))) (CONSP (CAR (CDR (CAR V294))))
          (MEMBER (CAR (CAR (CDR (CAR V294)))) '(|@sv| |@sv*|)))
      ;; ベクタ (SIMPLE-VECTOR)
      (LET* ((V300 (CDR V294)) (V301 (CAR V294)) (V302 (CDR V301)) (V303 (CAR V302))
             (V304 (CDR V303))
             (strict? (EQ '|@sv| (CAR (CAR (CDR (CAR V294))))))
             (Src (CAR V300))
             (n (LIST-LENGTH V304))
             )
        
        (UNLESS (OR (ZEROP n) (wrapper (variable? Src)))
          (LET ((TmpVar (<new-proxy-var> "TV/VEC")))
            (add-test (LIST 'SETQ TmpVar Src))
            (SETQ Src TmpVar)))

        (add-test `(AND (SIMPLE-VECTOR-P ,Src)
                        (,(IF strict? 'EQL '<=) ,n (LENGTH ,Src))))

        (LET ((Abstraction (REDUCE (LAMBDA (a b) (LIST '/. a b)) V304
                                   :FROM-END T :INITIAL-VALUE (ebr Src V303 (CAR (CDR V302)))))
              Args)
          (DOTIMES (i n) (PUSH (LIST 'SVREF Src i) Args))
          (LET ((Application (REDUCE #'LIST (NREVERSE Args) :INITIAL-VALUE Abstraction)))
            (reduce-help Application)))))

    ;; JUN HACK
    ((AND  (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
           (CONSP (CDR (CAR V294))) (CONSP (CAR (CDR (CAR V294))))
          (MEMBER (CAR (CAR (CDR (CAR V294)))) '(when unless WHEN UNLESS)))
      ;; when/unless/WHEN/UNLWSSパタン
      (LET* ((V300 (CDR V294)) (V301 (CAR V294)) (V302 (CDR V301)) (V303 (CAR V302))
             (V304 (CDR V303))
             (op (CAR (CAR (CDR (CAR V294)))))
             (native? (OR (EQ 'WHEN op) (EQ 'UNLESS op)))
             (Src (CAR V300))
             (predicate (SVREF (FIRST V304) 0))
             (Patterns (CONS (IF (OR (EQ 'when op) (EQ 'WHEN op)) 'true 'false)
                             (CDR V304))))

        ;(PRINT V294)
        (UNLESS (OR (NULL (CDR Patterns)) (wrapper (variable? Src)))
          (LET ((TmpVar (<new-proxy-var> "TV/TEST")))
            (add-test `(PROGN (SETQ ,TmpVar ,Src) T))
            (SETQ Src TmpVar)))

      (LET* ((testExp (LIST predicate Src))             
             (Args (CONS (LIST (IF native? '<cl-to-xi-boolean> '<checkBoolean>) testExp)
                         (AWHEN (CDR Patterns) (REPLICATE (LIST-LENGTH IT) Src))))
             (Abstraction (REDUCE (LAMBDA (a b) (LIST '/. a b)) Patterns
                                   :FROM-END T :INITIAL-VALUE (ebr Src V303 (CAR (CDR V302)))))
             (Application (REDUCE #'LIST Args :INITIAL-VALUE Abstraction)))
        ;;(PRINT Args)
        (reduce-help Application))))


    ;; JUN HACK TEST [2018-06-20] TODO
    ((AND  (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
           (CONSP (CDR (CAR V294))) (CONSP (CAR (CDR (CAR V294))))
          (MEMBER (CAR (CAR (CDR (CAR V294)))) '(if)))
      ;; ifパタン
      (LET* ((V300 (CDR V294)) (V301 (CAR V294)) (V302 (CDR V301)) (V303 (CAR V302))
             (V304 (CDR V303))
             (op (CAR (CAR (CDR (CAR V294)))))
             (native? (OR (EQ 'WHEN op) (EQ 'UNLESS op)))
             (Src (CAR V300))
             (predicate (SVREF (FIRST V304) 0))
             (Patterns (CONS (IF (OR (EQ 'when op) (EQ 'WHEN op)) 'true 'false)
                             (CDR V304))))

        ;(PRINT V294)
        (UNLESS (OR (NULL (CDR Patterns)) (wrapper (variable? Src)))
          (LET ((TmpVar (<new-proxy-var> "TV/IF")))
            (add-test `(PROGN (SETQ ,TmpVar ,Src) T))
            (SETQ Src TmpVar)))

      (LET* ((testExp (LIST predicate Src))             
             (Args (CONS (LIST (IF native? '<cl-to-xi-boolean> '<checkBoolean>) testExp)
                         (AWHEN (CDR Patterns) (REPLICATE (LIST-LENGTH IT) Src))))
             (Abstraction (REDUCE (LAMBDA (a b) (LIST '/. a b)) Patterns
                                   :FROM-END T :INITIAL-VALUE (ebr Src V303 (CAR (CDR V302)))))
             (Application (REDUCE #'LIST Args :INITIAL-VALUE Abstraction)))
        ;;(PRINT Args)
        (reduce-help Application))))

    ;; JUN HACK
    ((AND  (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
           (CONSP (CDR (CAR V294))) (CONSP (CAR (CDR (CAR V294))))
          (MEMBER (CAR (CAR (CDR (CAR V294)))) '(=> type)))
      ;; typeパタン
      (LET* ((V300 (CDR V294)) (V301 (CAR V294)) (V302 (CDR V301)) (V303 (CAR V302))
             (V304 (CDR V303))
             ;(op (CAR (CAR (CDR (CAR V294)))))
             (typeName (SVREF (FIRST V304) 0))
             (Patterns (CDR V304))
             (Src (CAR V300)))

        (UNLESS (OR (NULL Patterns) (wrapper (variable? Src)))
          (LET ((TmpVar (<new-proxy-var> "TV/TYP")))
            (add-test `(PROGN (SETQ ,TmpVar ,Src) T))
            (SETQ Src TmpVar)))

        (add-test `(TYPEP ,Src ',typeName))
        (LET ((Abstraction (REDUCE (LAMBDA (a b) (LIST '/. a b)) Patterns
                                   :FROM-END T :INITIAL-VALUE (ebr Src V303 (CAR (CDR V302))))))
          (LET* ((Args (WHEN Patterns (REPLICATE (LIST-LENGTH Patterns) Src)))
                 (Application (REDUCE #'LIST Args :INITIAL-VALUE Abstraction)))
            (reduce-help Application)))))


        ;; JUN HACK
    ((AND  (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
           (CONSP (CDR (CAR V294))) (CONSP (CAR (CDR (CAR V294))))
          (MEMBER (CAR (CAR (CDR (CAR V294)))) '(-> view)))
      ;; viewパタン
      (LET* ((V300 (CDR V294)) (V301 (CAR V294)) (V302 (CDR V301)) (V303 (CAR V302))
             (V304 (CDR V303))
             ;(op (CAR (CAR (CDR (CAR V294)))))
             (filterName (SVREF (FIRST V304) 0))
             (Patterns (CDR V304))
             (Src (CAR V300))
             (filtered (LIST filterName Src))
             (TmpVar (<new-proxy-var> "TV/VIEW"))
             )
        (add-test `(PROGN (SETQ ,TmpVar ,filtered) T))
        (LET ((Abstraction (REDUCE (LAMBDA (a b) (LIST '/. a b)) Patterns
                                   :FROM-END T :INITIAL-VALUE (ebr TmpVar V303 (CAR (CDR V302))))))
          (LET* ((Args (WHEN Patterns (REPLICATE (LIST-LENGTH Patterns) TmpVar)))
                 (Application (REDUCE #'LIST Args :INITIAL-VALUE Abstraction)))
            (reduce-help Application)))))

    ;; JUN HACK
    ((AND  (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
           (CONSP (CDR (CAR V294))) (CONSP (CAR (CDR (CAR V294))))
          (MEMBER (CAR (CAR (CDR (CAR V294)))) '(|@| fork)))
      ;; forkパタン
      (LET* ((V300 (CDR V294)) (V301 (CAR V294)) (V302 (CDR V301)) (V303 (CAR V302))
             (V304 (CDR V303))
             (Src (CAR V300))
             (n (LIST-LENGTH V304)))

        (UNLESS (<as-variable?> Src)
          (LET ((TmpVar (<new-proxy-var> "TV/FORK")))
            (add-test `(PROGN (SETQ ,TmpVar ,Src) T))
            (SETQ Src TmpVar)))

        (LET* ((Abstraction (REDUCE (LAMBDA (a b) (LIST '/. a b)) V304
                                   :FROM-END T :INITIAL-VALUE (ebr Src V303 (CAR (CDR V302)))))
               (Application (REDUCE #'LIST (REPLICATE n Src) :INITIAL-VALUE Abstraction)))
          (reduce-help Application))))

    ;; JUN HACK
    ((AND  (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
           (CONSP (CDR (CAR V294))) (CONSP (CAR (CDR (CAR V294))))
           (EQ '<complex> (CAR (CAR (CDR (CAR V294)))) ) )
      ;; 複合 パタン
      (LET* ((V300 (CDR V294)) (V301 (CAR V294)) (V302 (CDR V301)) (V303 (CAR V302))
             (V304 (CDR V303))
             (Src (CAR V300))
             (ComplexPattern (SVREF (FIRST V304) 0))
             (Test `(LET ((Tmp ,Src))
                      ,(<complex_pattern_to_lisp> ComplexPattern 'Tmp))))
        (add-test Test)
        (reduce-help (ebr Src V303 (CAR (CDR V302))))))

    ;; JUN HACK
    ((AND  (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
           (CONSP (CDR (CAR V294))) (CONSP (CAR (CDR (CAR V294))))
           (EQ 'QUOTE (CAR (CAR (CDR (CAR V294)))) ) )
      ;; QUOTE パタン
      (LET* ((V300 (CDR V294)) (V301 (CAR V294)) (V302 (CDR V301)) (V303 (CAR V302))
             (V304 (CDR V303))
             (Src (CAR V300))
             (Test (LIST 'EQUAL Src (LIST 'QUOTE (SVREF (FIRST V304) 0)))))
        (add-test Test)
        (reduce-help (ebr Src V303 (CAR (CDR V302))))))

    ;; JUN HACK
    ((AND  (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
           (CONSP (CDR (CAR V294))) (CONSP (CAR (CDR (CAR V294))))
          (MEMBER (CAR (CAR (CDR (CAR V294)))) '(& delay)))
      ;; 遅延パタン
      (LET* ((V300 (CDR V294)) (V301 (CAR V294)) (V302 (CDR V301)) (V303 (CAR V302))
             (V304 (CDR V303))
             (Src (LIST '<<delayed>> (CAR V300)))
             (dstVar (CAR V304)))

        (reduce-help `((/. ,dstVar ,(ebr Src V303 (CAR (CDR V302)))) ,Src))))

    ((AND (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
          (CONSP (CDR (CAR V294))) (CONSP (CDR (CDR (CAR V294))))
          (NULL (CDR (CDR (CDR (CAR V294))))) (CONSP (CDR V294))
          (NULL (CDR (CDR V294)))
          (NOT (wrapper (variable? (CAR (CDR (CAR V294)))))))
      ;; 変数以外の単一の比較対象
      (LET* ((V305 (CAR V294)) (V306 (CDR V305)))
        ;; JUN Hacked [2018-06-17]
        (UNLESS (EQ '_ (CAR V306))
          (add-test (equality-test (CAR V306) (CAR (CDR V294))))
          )
        (reduce-help (CAR (CDR V306)))))
    
    ((AND (CONSP V294) (CONSP (CAR V294)) (EQ '/. (CAR (CAR V294)))
          (CONSP (CDR (CAR V294))) (CONSP (CDR (CDR (CAR V294))))
          (NULL (CDR (CDR (CDR (CAR V294))))) (CONSP (CDR V294))
          (NULL (CDR (CDR V294))))
      ;; 変数
      (LET* ((V307 (CAR V294)) (V308 (CDR V307)) (Src (CAR (CDR V294))))
        ;; 対象の値が式(遅延パターンによる(<<delayed>>..)を除く)で表されている場合
        (UNLESS (<as-variable?> Src)
            
          ;;ただし、ワイルドカードパタン(_)に引っかかる場合は除く
          (UNLESS (<is-ignoreVar?> (CAR V308))
            ;;(FORMAT T "debug:~A~%"  V308)
            (LET ((TmpVar (<new-proxy-var> "TV/V")))
              (add-test `(PROGN (SETQ ,TmpVar ,(CAR (CDR V294))) T))
              (SETQ Src TmpVar)))
          )
        ;(reduce-help (ebr (CAR (CDR V294)) (CAR V308) (CAR (CDR V308))))
        (reduce-help (ebr Src (CAR V308) (CAR (CDR V308))))
        ))
    ((AND (CONSP V294) (EQ 'where (CAR V294)) (CONSP (CDR V294))
          (CONSP (CDR (CDR V294))) (NULL (CDR (CDR (CDR V294)))))
      (LET* ((V309 (CDR V294)))
        (add-test (LIST 'wrapper (CAR V309)))
        (reduce-help (CAR (CDR V309)))))
    ((AND (CONSP V294) (CONSP (CDR V294)) (NULL (CDR (CDR V294))))
      (LET* ((V310 (CAR V294)))
        (LET ((Z (reduce-help V310)))
          (IF (EQUAL V310 Z) V294 (reduce-help (CONS Z (CDR V294)))))))
    (T V294)))

(DEFUN ebr (V321 V322 V323)
  (COND
    ((ABSEQUAL V322 V323) V321)
    ((AND (CONSP V323) (EQ '/. (CAR V323)) (CONSP (CDR V323))
          (CONSP (CDR (CDR V323))) (NULL (CDR (CDR (CDR V323))))
          (> (occurrences V322 (CAR (CDR V323))) 0))
      V323)
    ((AND (CONSP V323) (EQ 'let (CAR V323)) (CONSP (CDR V323))
          (CONSP (CDR (CDR V323))) (NULL (CDR (CDR (CDR V323))))
          (ABSEQUAL V322 (CAR (CDR V323))))
      V323)
    ((CONSP V323) (CONS (ebr V321 V322 (CAR V323)) (ebr V321 V322 (CDR V323))))
    (T V323)))

(DEFUN add-test (V324) (SETQ *teststack* (CONS V324 *teststack*)))

;; パターンマッチの比較定数は、
;; NIL・数値・文字リテラル・文字列リテラル・シンボル・真偽値シンボルである

;; Added Jun [2018-06-20]
(DEFUN <testableAtomic?> (x)
  (OR (NULL x)
      (NUMBERP x)
      (CHARACTERP x)
      (OR (MEMBER x '(true false))
          (wrapper (symbol? x)))
      (STRINGP x)))
       

(DEFUN equality-test (V325 V326)
  (COND ((NULL V325)
          (LIST 'NULL V326))
        ((OR (NUMBERP V325)
             (CHARACTERP V325))
          (LIST 'EQL V325 V326))
        ((OR (MEMBER V325 '(true false))
             (wrapper (symbol? V325)))
          (LIST 'EQ V325 V326))
        ((STRINGP V325) (LIST 'EQUAL V325 V326))
        ;;((<quoted?> V325) (LIST 'EQUAL V325 V326))
        (T (implementation_error 'equality-test))))

(DEFUN <quoted?> (x)
  (AND (CONSP x) (CONSP (CDR x)) (EQ 'QUOTE (CAR x)) (NULL (CDDR x))))

;; Added Jun [2018-06-21]
(DEFUN <GenericLiteral?> (x)
  (OR (NUMBERP x)
      (CHARACTERP x)
      (SYMBOLP x) ;; NILもシンボルなので
      (STRINGP x)
      (<quoted?> x)
      ))

(DEFUN <GenericLiteralEq> (x dst ext?)
  (COND ((NUMBERP x)
          (LIST (IF ext? 'EQUALP 'EQL) x dst))
        ((CHARACTERP x)
          (LIST 'EQL x dst))
        ((SYMBOLP x)
          (LIST 'EQ (LIST 'QUOTE x) dst))
        ((STRINGP x)
          (LIST (IF ext? 'EQUALP 'EQUAL) x dst))
        ((CONSP x) ;; (QUOTE VAL)
          (LIST (IF ext? 'EQUALP 'EQUAL) x dst))
          
        ))
        

(DEFUN <valid_complex_pattern?> (X)
  (COND ((appliable? X) T)
        ((EQ '_ X) T)
        ((ATOM X) NIL)
        ((<quoted?> X) T)
        ((AND (<= 2 (LENGTH X))
              (MEMBER (FIRST X) '(== != === !==
                                  and or none not when unless WHEN UNLESS
                                  /.
                                  qi_< qi_<= qi_> qi_>=)))
          (CASE (FIRST X)
            ((== != === !==) (EVERY #'<GenericLiteral?> (REST X)))
            ((and or none) (EVERY #'<valid_complex_pattern?> (REST X)))
            ((when unless WHEN UNLESS)
              (AND (EQL 2 (LENGTH X)) (appliable? (SECOND X))))
            ((qi_< qi_<= qi_> qi_>=)
              (AND (EQL 2 (LENGTH X)) (NUMBERP (SECOND X))))
            (not (AND (EQL 2 (LENGTH X))
                      (<valid_complex_pattern?> (SECOND X))))
            (/. (is_xi_internal_lambda? X))
            ))
        (T NIL)))


(DEFUN <complex_pattern_to_lisp> (P &OPTIONAL (V 'x))
  (COND ((appliable? P)
          (LET ((P (get-appliable P :raw T)))
            (COND ((SYMBOLP P) `(wrapper (,P ,V)))
                  (T (LET ((type-required (get_type_of_xi_internal_lambda P))
                           (def (get_xi_internal_lambda_def P)))
                       `(AND (TYPEP ,V ',type-required)
                             (wrapper (,def ,V))))))))
        ((EQ '_ P) T)
        ((<quoted?> P) (<GenericLiteralEq> P V))
        (T (LET ((OP (FIRST P)))
             (CASE OP
               ((== != === !==)
                 (LET ((lisp-op (IF (OR (EQ '== OP) (EQ '=== OP)) 'OR 'NONE))
                       (ext? (OR (EQ '=== OP) (EQ '!== OP))))
                   (CONS lisp-op
                         (MAPCAR (LAMBDA (X) (<GenericLiteralEq> X V ext?)) (REST P)))))
               ((and or none) (CONS (CASE OP (and 'AND) (or 'OR) (none 'NONE))
                                    (MAPCAR (LAMBDA (X) (<complex_pattern_to_lisp> X V)) (REST P))))
               (not (LIST 'NOT (<complex_pattern_to_lisp> (SECOND P))))
               (WHEN (LIST (get-appliable (SECOND P)) V))
               (when (LIST 'wrapper (LIST (get-appliable (SECOND P)) V)))
               (UNLESS (LIST 'NOT (LIST (get-appliable (SECOND P)) V)))
               (unless (LIST 'NOT (LIST 'wrapper (LIST (get-appliable (SECOND P)) V))))
               ((qi_< qi_<= qi_> qi_>=) (LIST 'AND
                                              (LIST 'NUMBERP V)
                                              (LIST (CASE OP (qi_< '<) (qi_> '>) (qi_<= '<=) (qi_>= '>=))
                                                    V (SECOND P))))
               
               (/. (LET ((type-required (get_type_of_xi_internal_lambda P))
                         (def (get_xi_internal_lambda_def P)))
                     (ERROR "XI-INTERNAL-ERROR: <complex_pattern_to_lisp> ~W" P)
                     `(AND (TYPEP ,V ',type-required)
                           (wrapper (,def ,V))))))))))




(DEFUN cond-expression (V327 V328 V329)
  ;(FORMAT T "DEBUG: cond-expression=~W~%" (LIST V327 V328 V329))
  (LET* ((Err (insert-error-condition V327 V329))
         (Cases (MAPCAR 'make-case Err))
         (CondForm (cond-form V328 Cases))
         (ProxyVars (<get-proxy-vars-list>)))
    (IF ProxyVars
      `(LET ,ProxyVars
         (DECLARE (IGNORABLE ,@ProxyVars))
         ,CondForm)
      CondForm)))

(DEFUN cond-form (V42 V43)
 (COND
  ((AND (CONSP V43) (CONSP (CAR V43)) (CONSP (CDR (CAR V43)))
    (NULL (CDR (CDR (CAR V43)))) (EQ T (CAR (CAR V43))))
   (lisp-form V42 (CAR (CDR (CAR V43)))))
  (T (CONS 'COND (make-lispform V42 V43)))))

(DEFUN make-lispform (V334 V335)
 (COND ((NULL V335) NIL)
  ((AND (CONSP V335) (CONSP (CAR V335)) (CONSP (CAR (CAR V335)))
    (CONSP (CDR (CAR V335))) (NULL (CDR (CDR (CAR V335))))
    (EQ 'AND (CAR (CAR (CAR V335)))))
   (LET* ((V336 (CAR V335)) (V337 (CAR V336)))
    (LET ((NewTests (MAPCAR #'(LAMBDA (Test) (lisp-form V334 Test)) (CDR V337))))
     (LET ((NewResult (lisp-form V334 (CAR (CDR V336)))))
      (CONS (LIST (CONS (CAR V337) NewTests) NewResult)
       (make-lispform V334 (CDR V335)))))))
  ((AND (CONSP V335) (CONSP (CAR V335)) (CONSP (CDR (CAR V335)))
    (NULL (CDR (CDR (CAR V335)))))
   (LET* ((V338 (CAR V335)))
    (LET ((NewTest (lisp-form V334 (CAR V338))))
     (LET ((NewResult (lisp-form V334 (CAR (CDR V338)))))
      (CONS (LIST NewTest NewResult) (make-lispform V334 (CDR V335)))))))
  (T (implementation_error 'make-lispform))))

(DEFUN insert-error-condition (V57 V58)
 (COND
  ((AND (CONSP V58) (CONSP (CAR V58)) (CONSP (CAR (CAR V58)))
    (EQ ':tests (CAR (CAR (CAR V58)))) (NULL (CDR (CAR (CAR V58))))
    (CONSP (CDR (CAR V58))) (NULL (CDR (CDR (CAR V58)))))
   V58)
  ((AND (CONSP V58) (CONSP (CAR V58)) (CONSP (CAR (CAR V58)))
    (EQ ':tests (CAR (CAR (CAR V58)))) (CONSP (CDR (CAR V58)))
    (NULL (CDR (CDR (CAR V58)))) (NULL (CDR V58)))
   (LIST (CAR V58) (error-condition V57)))
  ((CONSP V58) (cons (CAR V58) (insert-error-condition V57 (CDR V58))))
  (T V58)))

(DEFUN error-condition (V345) (LIST (LIST ':tests T) (LIST 'f_error V345)))

(DEFUN make-case (V346)
 (COND
  ((AND (CONSP V346) (CONSP (CAR V346)) (EQ ':tests (CAR (CAR V346)))
    (NULL (CDR (CAR V346))) (CONSP (CDR V346)) (NULL (CDR (CDR V346))))
   (CONS T (CDR V346)))
  ((AND (CONSP V346) (CONSP (CAR V346)) (EQ ':tests (CAR (CAR V346)))
    (CONSP (CDR (CAR V346))) (NULL (CDR (CDR (CAR V346)))) (CONSP (CDR V346))
    (NULL (CDR (CDR V346))))
   (CONS (CAR (CDR (CAR V346))) (CDR V346)))
  ((AND (CONSP V346) (CONSP (CAR V346)) (EQ ':tests (CAR (CAR V346)))
    (CONSP (CDR V346)) (NULL (CDR (CDR V346))))
   (CONS (CONS 'AND (CDR (CAR V346))) (CDR V346)))
  (T V346)))

(SETQ *speed* 1)
