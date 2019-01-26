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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 組み込み関数を追加する手順
;; -------------------------------------------------------------------------------
;; (1) 本モジュールにて関数を定義
;; (2) core-01-features_load1.lispの *sysfuncs*にシンボルを追加
;; (3) core-05-arity_load5.lispのinitialise_arity_tableにシンボルとアリティの組を追加
;; (4) core-16-signatures_load17.lispに関数のシグネチャを追加
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (DEFUN print_tuple (Tuple Stream)
;;  (FORMAT Stream "(@p ~S ~S)" (fst Tuple) (snd Tuple)))

;; (DEFSTRUCT
;;  (TUPLE
;;   (:PRINT-FUNCTION
;;    (LAMBDA (Struct Stream Depth) (DECLARE (IGNORE Depth))
;;     (print_tuple Struct Stream)))
;;   (:CONC-NAME NIL) (:CONSTRUCTOR @p (fst snd)))
;;  fst snd)


;; 構文的でないマクロは、このスペシャル変数に含める
;; そうしておくと、展開時に全ての引数が自動的にLISPコードに変換される
(SETQ *exempted-macro* '(if and or none only list @sv time freeze delay & do prolog? list &cons &cons!))

;; そうでない構文的なマクロを定義する際には、式の部分は適宜lispFormを適用してLISPコードに変換する必要がある
;; たとえば、非大文字シンボルのクオート処理など
(DEFUN lispForm (X &OPTIONAL inherits) (FUNCALL 'lisp-form inherits X))


(DEFUN |@p| (a b)
  (XTUPLE* a b))

(DEFUN TUPLE-P (x) (XTUPLE-P x))
(DEFINE-COMPILER-MACRO TUPLE-P (x) `(XTUPLE-P ,x))

(DEFUN fst (tuple)  (XTUPLE-FST tuple))
(DEFUN snd (tuple)  (XTUPLE-SND tuple))
(DEFINE-COMPILER-MACRO fst (tuple)  `(XTUPLE-FST ,tuple))
(DEFINE-COMPILER-MACRO snd (tuple)  `(XTUPLE-SND ,tuple))


(DEFUN (SETF fst) (val x)  (SETF (XTUPLE-FST x) val))
(DEFUN (SETF snd) (val x)  (SETF (XTUPLE-SND) val))
(DEFINE-COMPILER-MACRO (SETF fst) (val x)  `(SETF (XTUPLE-FST ,x) ,val))
(DEFINE-COMPILER-MACRO (SETF snd) (val x)  `(SETF (XTUPLE-SND ,x) ,val))

(DEFMACRO |@sv| (&REST elems)
  (CONS 'VECTOR elems))

(DEFUN sv? (x) (SIMPLE-VECTOR-P x))
(DEFUN svlen (x)
  (ASSERT (AND 'svlen (SIMPLE-VECTOR-P x)))
  (LENGTH x))
(DEFUN svref (x index)
  (ASSERT (AND 'svref (SIMPLE-VECTOR-P x) (NON-NEGATIVE-INTEGER-P index)))
  (SVREF x index))
(DEFUN (SETF svref) (val x index)  (SETF (SVREF x index) val))
(DEFINE-COMPILER-MACRO (SETF svref) (val x index)  `(SETF (SVREF ,x ,index) ,val))

(DEFUN <error/if> (X)
  (error "~S is not a boolean~%" X))

(DEFMACRO if (X Y Z)
 `(LET ((C ,X))
   (COND ((EQ C 'true) ,Y) ((EQ C 'false) ,Z)
    (T (<error/if> C)))))

;;(DEFMACRO and (X Y) `(if ,X (if ,Y 'true 'false) 'false))
;;(DEFMACRO or (X Y) `(if ,X 'true (if ,Y 'true 'false)))
;;(DEFMACRO none (&rest XS)
;;  (REDUCE (LAMBDA (a b) `(if ,a 'false ,b)) XS  :FROM-END T :INITIAL-VALUE ''true))

(DEFMACRO and  (&REST XS) `(IF (AND ,@(MAPCAR (LAMBDA (x) (LIST 'wrapper x)) XS)) 'true 'false))
(DEFMACRO or   (&REST XS) `(IF (OR  ,@(MAPCAR (LAMBDA (x) (LIST 'wrapper x)) XS)) 'true 'false))
(DEFMACRO none (&REST XS) `(IF (OR  ,@(MAPCAR (LAMBDA (x) (LIST 'wrapper x)) XS)) 'false 'true))
(DEFMACRO only(&REST XS) `(IF (ONLY ,@(MAPCAR (LAMBDA (x) (LIST 'wrapper x)) XS)) 'true 'false))

(DEFUN not (X)
 (COND ((EQ X 'true) 'false) 
       ((EQ X 'false) 'true)
       (T (error "~S is not a boolean~%" X))))

(DEFUN element? (x y) (IF (MEMBER x y :TEST 'ABSEQUAL) 'true 'false))

(DEFUN subst (X Y Z) (SUBST X Y Z :TEST 'ABSEQUAL))

(DEFUN subst (X Y Z) 
  (COND ((ABSEQUAL Y Z) X)
        ((TUPLE-P Z) (|@p| (subst X Y (fst Z)) (subst X Y (snd Z))))
        ((CONSP Z) (CONS (subst X Y (CAR Z)) (subst X Y (CDR Z))))
        (T Z)))        

(DEFUN remove (x y) (REMOVE x y :TEST 'ABSEQUAL))

(DEFUN difference (x y) (SET-DIFFERENCE x y :TEST 'ABSEQUAL))

(DEFUN union (x y) 
  (IF (NULL x) y
      (IF (MEMBER (CAR x) y :TEST 'ABSEQUAL)
          (union (CDR x) y)
          (CONS (CAR x) (union (CDR x) y)))))

(DEFUN assoc (x y) (ASSOC x y :TEST 'ABSEQUAL))

;(DEFMACRO let (VAR VAL EXPR) (LIST 'LET (LIST (LIST VAR VAL)) EXPR))

;; OBSOLETE
(DEFUN <expand-let-syntax-family> (xi-op lisp-op clauses)
  (LET ((n (LIST-LENGTH clauses)))
    (COND ((EVENP n)
            (ERROR "~W: illegal clauses ~W" xi-op clauses))
          (T `(,lisp-op
               (,@(DO ((p (BUTLAST clauses) (CDDR p))
                       tmp)
                      ((NULL p) (NREVERSE tmp))
                    (PUSH (LIST (FIRST p) (SECOND p)) tmp)))
               ,(CAR (LAST clauses)))))))


(DEFMACRO <<let>> (&REST clauses
                   &AUX (inherits (WHEN (AND (CONSP (CAR clauses)) (EQ '<<inherit>> (CAAR clauses)))
                                    (CDAR clauses))))
  (WHEN inherits
    ;(PRINT inherits)
    (SETQ clauses (CDR clauses)))

  (LET ((n (LIST-LENGTH clauses)))
    (COND ((EVENP n)
            (ERROR "let: illegal clauses ~W" clauses))
          ((EQL 1 n) ;; (let exp) --> exp
            (FIRST clauses))

          ;; [2018-07-12] OBSOLETE
          (NIL (EQL 3 n) ;; (let A B E)の形式の場合、拡張構文の可能性がある (OBSOLETE)
            (LET ((dst-exp (FIRST clauses))
                  (src-exp (SECOND clauses))
                  (main-exp (THIRD clauses)))
              (COND ((var? dst-exp)
                      `(LET ((,(FIRST clauses) ,(SECOND clauses))) ,(THIRD clauses)))
                    (T (ERROR "let: impl error ~W" dst-exp)))))

          ;; [2018-07-12] こっちに一本化
          (T (LET (new-vars)
            ;; (let A B C D ... E)の形式である場合、完全なLispのLETでなければならない
            ;; つまり、式から変数の単純なバインドでなければ違法である
            `(LET
                 (,@(DO ((p (BUTLAST clauses) (CDDR p))
                         tmp)
                        ((NULL p) (NREVERSE tmp))
                      (LET ((var-exp (FIRST p))
                            (value-exp (lispForm (SECOND p) inherits)))
                        (IF (var? var-exp)
                          (PUSH var-exp new-vars)
                          (ERROR "let: ~W is not a variable" var-exp)
                          )
                        (PUSH (LIST var-exp value-exp) tmp))))
               ,(lispForm (CAR (LAST clauses))
                          (NCONC new-vars inherits))))))))



(DEFUN <<repeat>> (f)
  (LET ((result (FUNCALL f)))
    (UNLESS (EQ '<<done>> result)
      (&CONS! result (<<repeat>> f)))))

(DEFUN <error/genlist> (lazy? clauses)
  (ERROR "(~A ...) : wrong syntax ~A"
         (IF lazy? "[::]" "[:]")
         clauses))

;; これは大まかなチェックであることに注意。体裁が整っているかのみ検証する。
(DEFUN <check-genlist-form> (lazy? clauses)
  (UNLESS clauses (<error/genlist> lazy? clauses))
  (DO ((c clauses (CASE (FIRST c) ((for) (CDDDDR c)) ((let) (CDDDR c)) ((loop) (CDR c)) (T (CDDR c))))
       (cnt 0))
      ((OR (NULL c) (NULL (CDR c)))
       (UNLESS c (<error/genlist> lazy? clauses)))
    ;;TODO オペランド数の検証が必要
    (LET ((v (FIRST c)))
      (UNLESS (OR (MEMBER v '(if IF when WHEN unless UNLESS while WHILE until UNTIL break BREAK do))
                  (AND (EQ 'let v) (var? (SECOND c)))
                  (AND (EQ 'for v) (var? (SECOND c)))
                  (AND (EQ 'count v) (var? (SECOND c)))
                  (EQ 'loop v)
                  (var? v)
                  (AWHEN (<complex-expressions-p> v)
                    (AND (EQL IT (<complex-expressions-p> (SECOND c)))
                         (EVERY 'var? (<list-complex-expressions> v)))))
        (<error/genlist> lazy? clauses)))))

;(<check-genlist-form> '(if b let c d e f g))
      
          
(DEFUN <manipulate-2nd> (2nd inherits)
  (IF (<complex-expressions-p> 2nd)
    (DESTRUCTURING-BIND (fn &REST inits)
        (MAPCAR (LAMBDA (exp) (lispForm exp inherits))
                (<list-complex-expressions> 2nd))
      `(LIST ,@inits))
    (lispForm 2nd inherits)))
      


(DEFUN <make-genlist-code> (inherits clauses lazy?
                                     &AUX (n (LIST-LENGTH clauses)))
  ;; おおまかに検証
  (<check-genlist-form> lazy? clauses)

  ;; この時点で、少なくとも構文の体裁は整っている
  (LET ((main-exp (LASTCAR clauses))
        (bindings (NBUTLAST clauses))
        pairs)
    (DO ((b bindings (CASE (FIRST b) ((for) (CDDDDR b)) ((let) (CDDDR b)) ((loop) (CDR b)) (T (CDDR b)))))
        ((NULL b))
      (LET ((1st (FIRST b))
            (2nd (SECOND b)))
        (COND ((var? 1st)
                (PUSH (LIST 1st (<manipulate-2nd> 2nd inherits)) pairs)
                (SETQ inherits (CONS 1st inherits)))
              ((MEMBER 1st '(if IF when WHEN unless UNLESS while WHILE until UNTIL break BREAK))                
                (PUSH (LIST 1st (lispForm 2nd inherits)) pairs))
              ((EQ 'let 1st)
                (PUSH (LIST 'let 2nd (lispForm (THIRD b) inherits)) pairs)
                (SETQ inherits (CONS 2nd inherits)))
              ((EQ 'for 1st)
                (PUSH (LIST 'for 2nd (lispForm (THIRD b) inherits) (lispForm (FOURTH b) inherits)) pairs)
                (SETQ inherits (CONS 2nd inherits)))
              ((EQ 'count 1st)
                (PUSH (LIST 'count 2nd) pairs)
                (SETQ inherits (CONS 2nd inherits)))
              ((EQ 'loop 1st)
                (PUSH '(loop NIL) pairs))
              ((EQ 'do 1st)
                (PUSH (LIST 'do (lispForm 2nd inherits)) pairs))
              (T ;; complex
                (LET ((vars (<list-complex-expressions> 1st)))
                  (PUSH (LIST vars
                              (MAPCAR (LAMBDA (e) (<manipulate-2nd> e inherits)) (<list-complex-expressions> 2nd)))
                        pairs)
                  (SETQ inherits (APPEND vars inherits)))))))
  
    (LET* ((tmpvar (UNLESS lazy? (GENSYM)))
           (main-form (lispForm main-exp inherits))
           (result-form (LIST 'BLOCK NIL (IF lazy?
                                           `(YIELD (WITHOUT-CALL/CC ,main-form))
                                           (LIST 'PUSH main-form tmpvar))))
           (need-toplevel-block? NIL)
           (body (REDUCE (LAMBDA/BIND ((1st 2nd &OPTIONAL 3rd 4th) exp)
                           (COND ((MEMBER 1st '(if when)) `(WHEN (WITHOUT-CALL/CC (wrapper ,2nd)) ,exp))
                                 ((MEMBER 1st '(IF WHEN)) `(WHEN (WITHOUT-CALL/CC ,2nd) ,exp))
                                 ((EQ 1st 'unless) `(UNLESS (WITHOUT-CALL/CC (wrapper ,2nd)) ,exp))
                                 ((EQ 1st 'UNLESS) `(UNLESS (WITHOUT-CALL/CC ,2nd) ,exp))
                                 ((EQ 'let 1st) `(LET ((,2nd (WITHOUT-CALL/CC ,3rd))) ,exp))
                                 ((MEMBER 1st '(break BREAK))
                                   (CASE 1st
                                     ((break) `(IF (WITHOUT-CALL/CC (wrapper ,2nd)) (RETURN) ,exp))
                                     ((BREAK) `(IF (WITHOUT-CALL/CC ,2nd) (RETURN) ,exp))))
                                 ((MEMBER 1st '(while WHILE until UNTIL))
                                   (SETQ need-toplevel-block? T)
                                   (CASE 1st
                                     ((while) `(IF (WITHOUT-CALL/CC (wrapper ,2nd)) ,exp (RETURN-FROM <<gl_break>>)))
                                     ((WHILE) `(IF (WITHOUT-CALL/CC ,2nd) ,exp (RETURN-FROM <<gl_break>>)))
                                     ((until) `(IF (WITHOUT-CALL/CC (wrapper ,2nd)) (RETURN-FROM <<gl_break>>) ,exp))
                                     ((UNTIL) `(IF (WITHOUT-CALL/CC ,2nd) (RETURN-FROM <<gl_break>>) ,exp))))
                                 ((EQ 1st 'loop) (LIST 'LOOP exp))
                                 ((EQ 1st 'for) (LET ((cntvar (GENSYM))
                                                      (endvar (GENSYM)))
                                                  `(DO ((,cntvar (WITHOUT-CALL/CC ,3rd) (WITHOUT-CALL/CC (1+ ,cntvar)))
                                                        (,endvar (WITHOUT-CALL/CC ,4th)))
                                                       ((WITHOUT-CALL/CC (> ,cntvar ,endvar)))
                                                     (LET ((,2nd ,cntvar))
                                                       ,exp))))
                                 ((EQ 1st 'count)
                                   (LET ((tmp (GENSYM)))
                                   `(LET ((,tmp 0))
                                      (LOOP
                                         (LET ((,2nd ,tmp))
                                           ,exp)
                                         (WITHOUT-CALL/CC (INCF ,tmp))))))
                                 ((EQ 1st 'do) `(PROGN (WITHOUT-CALL/CC ,2nd) ,exp))
                                 ((var? 1st) (LET ((tmp (GENSYM)))
                                               `(&DOLIST (,tmp (WITHOUT-CALL/CC ,2nd))
                                                  (LET ((,1st (WITHOUT-CALL/CC ,tmp))) ,exp))))
                                 (T  (LET ((tmps (FREPLICATE (LENGTH 1st) #'GENSYM)))
                                       `(&DOLISTS ,(MAPCAN #'LIST tmps 2nd)
                                          (LET ,(MAPCAR (LAMBDA (v x) `(,v (WITHOUT-CALL/CC ,x))) 1st tmps)
                                            ,exp))))))
                         (NREVERSE pairs)
                         :FROM-END T :INITIAL-VALUE result-form)))

      ;; これは、break/BREAK構文が X<-L/provide X ...構文より上位の文脈で使われることを許容するものである
      (SETQ body (LIST 'BLOCK 'NIL body))

      (WHEN need-toplevel-block?
        (SETQ body (LIST 'BLOCK '<<gl_break>> body)))
      
      (IF lazy?
        `(MAKE-GENERATOR ,body '<<done>>)
        `(LET (,tmpvar) ,body (NREVERSE ,tmpvar))))))

;; リスト生成　STRICT版 ([:] ...)
(DEFMACRO <<genlist>> (inherits &REST clauses)
  (<make-genlist-code> inherits clauses NIL))
;; リスト生成 LAZY版 ([::] ...)
(DEFMACRO <<&genlist>> (inherits &REST clauses)
  (LIST '<<repeat>> (<make-genlist-code> inherits clauses T)))

(DEFMACRO <<subparlist>> (inherits &REST clauses)
  (<make-genlist-code> inherits clauses T))

(DEFUN <make-genparlist-code> (inherits main-exp subs lazy?
                                     &AUX (n (LIST-LENGTH subs)))
  ;; おおまかに検証
  (DOLIST (s subs)
    (<check-genlist-form> lazy? (CDDR s)))
  
  (LET* ((chkvar (GENSYM "CHK"))
         (tmpvar (UNLESS lazy? (GENSYM "TMP")))
         (varss (MAPCAR #'CAR subs))
         (generator-vars (FREPLICATE n #'GENSYM))
         (generator-bindings (MAPCAR (LAMBDA (s v) `(,v (WITHOUT-CALL/CC ,(lispForm (CDR s) inherits))))
                                     subs generator-vars))
         (generator-pairs (MAPCAR #'CONS varss generator-vars))
         (main-form (lispForm main-exp (APPEND inherits (REDUCE #'APPEND varss))))
         (result-form (LIST 'BLOCK NIL (IF lazy?
                                         `(YIELD (WITHOUT-CALL/CC ,main-form))
                                         (LIST 'PUSH main-form tmpvar))))
         (body `(LET ,generator-bindings
                  (LOOP ,(REDUCE (LAMBDA/BIND ((vars . genvar) rest)
                                   `(MULTIPLE-VALUE-BIND (,chkvar ,@vars) (FUNCALL ,genvar)
                                      (IF (WITHOUT-CALL/CC (EQ ,chkvar T)) ,rest (RETURN))))
                                 generator-pairs :FROM-END T :INITIAL-VALUE result-form)))))

    (IF lazy?
      `(MAKE-GENERATOR ,body '<<done>>)
      `(LET (,tmpvar) ,body (NREVERSE ,tmpvar)))))
    
        
        

;; リスト生成(パラレル)　STRICT版 ([:] ...)
(DEFMACRO <<genparlist>> (inherits main &REST subs)
  (<make-genparlist-code> inherits main subs NIL))
;; リスト生成(パラレル) LAZY版 ([::] ...)
(DEFMACRO <<&genparlist>> (inherits main &REST subs)
  (LIST '<<repeat>> (<make-genparlist-code> inherits main subs T)))

;;; TEST HACK
;; (DEFPARAMETER *<hist>* NIL)
;; (DEFMACRO push-hist (x) (PUSH x *<hist>*) (WARN "~W" *<hist>*) '(PROGN))
;; (DEFMACRO pop-hist() (POP *<hist>*) '(PROGN))
;; (DEFMACRO |let| (v x e) 
;;   `(PROGN
;;     (push-hist (,v ,x))
;;     (LET ((,v ,x)) (PROG1 ,e (pop-hist)))))



(DEFMACRO list (&REST X) (CONS 'LIST X))

(DEFMACRO do (&REST X) (CONS 'PROGN X))

(DEFUN y-or-n? (X) (IF (Y-OR-N-P X) 'true 'false))

(DEFUN empty? (X) (IF (NULL X) 'true 'false))

(DEFUN value (X) (SYMBOL-VALUE X))

(DEFUN length (X) (LIST-LENGTH X))

;; ORIGINAL
;; (DEFUN nth (N L) 
;;   (error "nth err")
;;   (IF (= N 1) (IF (NULL L) (error "nth expects a longer list.~%") (CAR L))
;;       (nth (1- N) (CDR L))))

;; JUN HACKED (optimize)
(DEFUN |nth| (n xs)
  (LET ((c (NTHCDR n xs)))
    (IF c
      (CAR c)
      (error "nth expects a longer list.~%"))))

(DEFUN concat (X Y) (READ-FROM-STRING (FORMAT NIL "~A~A" X Y)))

(DEFUN append (X Y) (APPEND X Y))

(DEFUN reverse (X) (REVERSE X))

(DEFUN set (X Y) (SET X Y))

(DEFUN cons (X Y) (CONS X Y))

(DEFUN |@c| (X Y) (CONS X Y))

(DEFUN cons? (X) (IF (CONSP X) 'true 'false))
(DEFUN list? (X) (IF (LISTP X) 'true 'false))

(DEFUN closure? (X) (IF (FUNCTIONP X) 'true 'false))
(DEFUN function? (X) (IF (OR (FUNCTIONP X) (AND (SYMBOLP X) (FBOUNDP X))) 'true 'false))
(DEFUN callable? (X) (IF (OR (FUNCTIONP X) (SYMBOLP X)) 'true 'false))

(DEFMACRO time (X) (LIST 'TIME X))

;;(DEFUN implementation_error (Func)
;; (ERROR "Qi implementation error in ~A: report to dr.mtarver@ukonline.co.uk~%" Func))

(DEFUN implementation_error (Func)
 (ERROR "XI: IMPLEMENTATION ERROR: ~A" Func))

(DEFUN explode (X) (COERCE (FORMAT NIL "~S" X) 'LIST))

(DEFUN head (X)
 (IF (CONSP X) (CAR X) (ERROR "head expects a non-empty list.~% ")))

(DEFUN tail (X)
 (IF (CONSP X) (CDR X) (ERROR "tail expects a non-empty list.~% ")))

(DEFUN tuple? (X) (IF (TUPLE-P X) 'true 'false))

;; 遅延リストの比較はしないことに注意
(DEFUN ABSEQUAL (X Y)
 (COND ((AND (CONSP X) (CONSP Y)) 
        (AND (ABSEQUAL (CAR X) (CAR Y))
             (ABSEQUAL (CDR X) (CDR Y))))
       ((AND (TUPLE-P X) (TUPLE-P Y))
         (AND (ABSEQUAL (fst X) (fst Y)) (ABSEQUAL (snd X) (snd Y))))
       ((AND (SIMPLE-VECTOR-P X) (SIMPLE-VECTOR-P Y))
         (LET ((LEN (LENGTH X)))
           (AND (EQL LEN (LENGTH Y))
                (DOTIMES (i LEN T)
                  (UNLESS (ABSEQUAL (SVREF X i) (SVREF Y i))
                    (RETURN))))))
       (T (EQUAL X Y))))

(DEFUN variable? (X)
 (IF (var? X)
     'true 
     'false))

(DEFUN var? (X) (AND (SYMBOLP X) (NOT (NULL X)) (UPPER-CASE-P (CHAR (SYMBOL-NAME X) 0))))

(DEFUN symbol? (X)
  (IF (AND (SYMBOLP X) 
	       (NOT (MEMBER X '(true false NIL)))
               ;;JUN HACKED (Comment Out)
               ;;(NOT (place_holder? X))
           (NOT (UPPER-CASE-P (CHAR (SYMBOL-NAME X) 0))))
      'true
      'false))

;; JUN HACKED (Comment out)
;; (DEFUN place_holder? (X)
;;   (LET ((NAMESTRING (SYMBOL-NAME X))) 
;;          (AND (> (LENGTH (THE STRING NAMESTRING)) 2)
;; 	    (CHAR-EQUAL #\& (CHAR NAMESTRING 0))
;;                 (CHAR-EQUAL #\& (CHAR NAMESTRING 1)))))  

(DEFUN number? (X) (IF (NUMBERP X) 'true 'false))

;(DEFUN string? (X) (IF (STRINGP X) 'true 'false))

;; HACK Jun
;; feke-string構造体が与えられたとき、これを文字列とみなして真を返す
;; xiコンパイラを騙すためのギミックであり、巧妙であるので注意

(DEFUN string? (X) (IF (OR (STRINGP X)
                           (fake-string? X))
                     'true
                     'false))

(DEFUN character? (X) (IF (CHARACTERP X) 'true 'false))

(DEFUN boolean? (X) (IF (MEMBER X '(true false)) 'true 'false))

(DEFUN integer? (X) (IF (INTEGERP X) 'true 'false))

(DEFUN complex? (X) (IF (COMPLEXP X) 'true 'false))

(DEFUN float? (X) (IF (FLOATP X) 'true 'false))

(DEFUN real? (X) (IF (REALP X) 'true 'false))

(DEFUN rational? (X) (IF (RATIONALP X) 'true 'false))

(DEFUN sqrt (X) (SQRT X))

(DEFUN random (X) (RANDOM X))

(DEFUN round (X) (ROUND X))

(DEFUN congruent? (X Y) (IF (EQUALP X Y) 'true 'false))

(DEFUN qi_= (X Y) (IF (ABSEQUAL X Y) 'true 'false))
;; Added by JUN
(DEFUN qi_/= (X Y) (IF (ABSEQUAL X Y) 'false 'true))

(DEFUN eq? (X Y) (qi_= X Y))

(DEFUN qi_> (X Y) (IF (> X Y) 'true 'false))

(DEFUN qi_< (X Y) (IF (< X Y) 'true 'false))

(DEFUN qi_>= (X Y) (IF (>= X Y) 'true 'false))

(DEFUN qi_<= (X Y) (IF (<= X Y) 'true 'false))

(DEFUN gensym (X) (GENTEMP X))

(DEFUN newvar (X) (GENTEMP (FORMAT NIL "~A" X)))
(DEFUN newsym (X) (GENTEMP (FORMAT NIL "~A" X)))

(DEFUN source_code (F) (get-prop F 'source NIL))

(DEFUN map (V32 V33)
 (COND ((NULL V33) NIL)
  ((CONSP V33) (CONS (apply V32 (CAR V33)) (map V32 (CDR V33))))
  (T (ERROR "map requires a list; not ~S~%" V33))))

(DEFUN read-file-as-charlist (File)
 (LET ((AbsFile (FORMAT NIL "~A~A" *qi_home_directory* File)))
  (IF (NOT (PROBE-FILE AbsFile)) (ERROR "~%~A does not exist~%" AbsFile))
  (WITH-OPEN-FILE (In AbsFile :DIRECTION :INPUT)
   (DO ((Letter T) (Letters NIL)) ((NULL Letter) (NREVERSE (CDR Letters)))
    (SETQ Letter (READ-CHAR In NIL NIL)) (PUSH Letter Letters)))))

(DEFVAR *qi_home_directory* "")

(DEFUN cd (String)
 (IF (EQUAL String "") (SETQ *qi_home_directory* String)
  (SETQ *qi_home_directory* (FORMAT NIL "~A/" String))))

(DEFVAR *tc* 'false)

(DEFUN write-to-file (Filename Output)
 (LET ((AbsFilename (FORMAT NIL "~A~A" *qi_home_directory* Filename)))
  (WITH-OPEN-FILE (OUTSTREAM AbsFilename
                               :DIRECTION :OUTPUT 
                               :IF-EXISTS :APPEND 
                               :IF-DOES-NOT-EXIST :CREATE)
    (FORMAT OUTSTREAM "~%")
    (COND ((STRINGP Output) (WRITE-STRING Output OUTSTREAM)) 
          (T (PPRINT Output OUTSTREAM)))  )
  AbsFilename))

;; hack [2018-06-18] freeze/thawをPROMISEで再実装
(DEFMACRO freeze (X) (LIST 'DELAY X))
(DEFUN thaw (X) (FORCE X))
;; こちらは新たに追加
(DEFUN promise? (X) (IF (PROMISE-P X) 'true 'false))
(DEFMACRO delay (X) (LIST 'DELAY X))
(DEFUN force (X) (FORCE X))
(DEFMACRO & (X) (LIST 'DELAY X))
(DEFUN ! (X) (FORCE X))

(DEFMACRO &cons (A B) (LIST '&CONS A B))
(DEFMACRO &cons! (A B) (LIST '&CONS! A B))


(DEFUN ps (X) (PPRINT (source_code X)))

(DEFUN abort () (ERROR ""))

(DEFUN read-char (X) (DECLARE (IGNORE X)) (READ-CHAR))

(DEFUN input () (eval (CAR (lineread))))

(DEFMACRO input+ (Colon Type) `(input+help (curry-type (QUOTE ,Type))))

(DEFUN input+help (Type)
  (IF (NOT (monomorphic? Type)) (ERROR "error: ~A should be a monotype.~%" Type))
  (LET ((I (CAR (lineread))))       
       (COND  ((EQ (statictypecheck NIL I Type) 'false)
              (FORMAT T "this is not a ~A, please re-enter: " Type) 
              (input+help Type))
             (T (eval I)))))

(DEFUN monomorphic? (Type)
  (IF (CONSP Type)
      (AND (monomorphic? (CAR Type)) (monomorphic? (CDR Type)))
      (NOT (var? Type))))

(DEFUN if-without-checking (String) 
   (IF (EQ *tc* 'false) (ERROR String)))

(DEFUN if-with-checking (String) 
   (IF (EQ *tc* 'true) (ERROR String)))

(DEFUN make-array (dims) (MAKE-ARRAY dims :INITIAL-ELEMENT #\Escape))

(DEFUN get-array (array dims default)
 (LET ((array_element (APPLY #'AREF (CONS array dims))))
  (IF (EQ array_element #\Escape) default array_element)))

(DEFUN put-array (array dims value)
 (SETF (APPLY #'AREF (CONS array dims)) value))

(DEFUN debug (X)
  (DECLARE (IGNORE X)) 
  (IF (PROBE-FILE "debug.txt") (DELETE-FILE "debug.txt")) 
  (DRIBBLE (FORMAT NIL "~A~A" *qi_home_directory* "debug.txt"))
  "done")

(DEFUN undebug (X) (DECLARE (IGNORE X)) (DRIBBLE) "done")

(DEFUN version (X) (SETQ *version* X))

(DEFUN type (X) (typecheck NIL X 'A))

(DEFUN typecheck (Hyps X Type) 
   (statictypecheck (MAPCAR 'cons_form_hyp Hyps) (cons_form X) Type))

(DEFUN cons_form_hyp (Hyp)
  (IF (typing? Hyp)
      (CONS (cons_form (CAR Hyp)) (CDR Hyp))
      Hyp))

(DEFUN typing? (Hyp) (AND (LISTP Hyp) (= (LENGTH Hyp) 3) (EQ (CADR Hyp) '|:|))) 


'(DEFUN %valid-regex-arguments? (args &AUX (n (LIST-LENGTH args)))
    (OR (AND (EQL 2 n) (STRINGP (FIRST args)) (STRINGP (SECOND args)))
        (AND (EQL 1 n) (STRINGP (FIRST args)))))

  

'(DEFMACRO regex (src &OPTIONAL (opt ""))
  (UNLESS (AND (STRINGP src) (STRINGP opt))
    (ERROR "xi: invalid form (regex ~A ~A)" src opt))
  `(REGEX ,src
          ,@(WHEN (POSITION #\x opt) '(:EXTENDED-MODE T))
          ,@(WHEN (POSITION #\s opt) '(:SINGLE-LINE-MODE T))
          ,@(WHEN (POSITION #\m opt) '(:MULTI-LINE-MODE T))
          ,@(WHEN (POSITION #\i opt) '(:CASE-INSENSITIVE-MODE T))))

'(DEFUN $ (regex target)
  (text-matches regex target))

'(DEFUN $? (regex target)
  (IF (text-scan regex target :output :position) 'true 'false))


(DEFPARAMETER *internal-param-alist* '((XParamString . STRING)))


(DEFUN make_xi_internal_lambda (var expr type)
  (LET ((internal-param-info (RASSOC type *internal-param-alist*)))
    (UNLESS internal-param-info
      (ERROR "XI INTERNAL ERROR!!!! make_xi_internal_lambda"))
    (LET ((def `(LAMBDA (,var) ,expr))
          (param (CAR internal-param-info)))
      `(/. ,param
           (<<xi_internal_lambda_call>> ,(VECTOR def) ,param)))))

(DEFUN is_xi_internal_lambda? (x)
  (AND (PROPER-LIST-P x)
       (EQ '/. (FIRST x))
       (ASSOC (SECOND x) *internal-param-alist*)))

;; パラメタが要求する型名を返す
;; ただし、(is_xi_internal_lambda? form)が真であることを前提とする
(DEFUN get_type_of_xi_internal_lambda (form)
  (CDR (ASSOC (SECOND form) *internal-param-alist*)))
  


(DEFUN get_xi_internal_lambda_def (form)
  (SVREF (SECOND (THIRD form)) 0))

(DEFMACRO <<xi_internal_lambda_call>> (vec arg)
  (LIST (SVREF vec 0) arg))

;;;;;;;;;;; OBSOLETE

(DEFUN <regex> (src &OPTIONAL (opt ""))
  (UNLESS (AND (STRINGP src) (STRINGP opt))
    (ERROR "xi: invalid form (regex ~A ~A)" src opt))
  `(REGEX ,src
          ,@(WHEN (POSITION #\x opt) '(:EXTENDED-MODE T))
          ,@(WHEN (POSITION #\s opt) '(:SINGLE-LINE-MODE T))
          ,@(WHEN (POSITION #\m opt) '(:MULTI-LINE-MODE T))
          ,@(WHEN (POSITION #\i opt) '(:CASE-INSENSITIVE-MODE T))))

(DEFUN %MAKE-REGEX-LAMBDA% (ptn opt)
  (LET* ((Regex (<regex> ptn opt))
         (output-string (POSITION #\S opt))
         (output-position (POSITION #\P opt))
         (all-matches (POSITION #\+ opt))
         (as-predicate (NONE all-matches output-string output-position)))

    (LET ((output (COND (output-string '(:OUTPUT :STRING))
                        (output-position '(:OUTPUT :POSITION)))))
         
      (COND (as-predicate
              (make_xi_internal_lambda 'x `(IF (AND (STRINGP x)
                                                    (TEXT-SCAN ,Regex x :OUTPUT :POSITION))
                                             'true 'false)))
            (all-matches
              (make_xi_internal_lambda 'x `(TEXT-MATCHES ,Regex x ,@output)))
            (T (make_xi_internal_lambda 'x `(TEXT-SCAN ,Regex x ,@output)))))))
          
                            
; (make-regex-lambda "foo" "i+")
       
