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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (SETF *READTABLE* (COPY-READTABLE *READTABLE*))
  (SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
  )

(IN-PACKAGE :OLEO.EMBED.CORE)

(DEFVAR *<CALLER-PACKAGE>*)

(DEFUN lineread NIL (lineread_loop (READ-CHAR) NIL))

(DEFUN lineread_loop (V3 V4)
 (COND ((EQL #\^ V3) (error "line read aborted"))
  ((MEMBER V3 (LIST #\Newline #\Return) :TEST 'CHAR-EQUAL)
   (LET ((Line (compile '<st_input> V4)))
    (IF (OR (EQ Line 'fail!) (NULL Line))
        (lineread_loop (READ-CHAR) (APPEND V4 (LIST V3)))
        Line)))
  (T (lineread_loop (READ-CHAR) (APPEND V4 (LIST V3))))))

(DEFUN read-file (V5)
 (LET
  ((ErrorString
    (FORMAT NIL "parse failure of file ~A here: ~~%~~%~~{~~C~~} ..." V5)))
  (LET ((Chars (read-file-as-charlist V5)))
       (compile '<st_input> Chars ErrorString))))


;; xiのデータ表現のリストからcons表現への変換 (表層をなぞる)
;; (a (b c) d) --> (cons a (cons (cons b (cons c NIL)) (cons d NIL)))
(DEFUN <to-consing> (xs)
  (COND ((NULL xs) NIL)
        (T `(cons (<<id>> ,(CAR xs)) ,(<to-consing> (CDR xs))))))

;; cons表現の表層をなぞり、原型のリストを得る
;; (cons A (cons B (cons C NIL))) --> (A B C)
(DEFUN <from-consing> (consed)
  (WHEN (CONSP consed)
    (CONS (SECOND (SECOND consed)) ;; (|cons| (<<id>> Value) rest) --> take the Value
          (<from-consing> (THIRD consed)))))

  

(DEFUN <st_input> (Stream)
  (OR
    ;; Added by Jun
    ;; ベクタ要素が現れた場合、それは文字列かネイティブLispコードの何れかである
    ;; #(種類 値)
    (BLOCK localfailure
      (IF (AND (CONSP (FIRST Stream)) (VECTORP (FIRST (FIRST Stream))))
        (LET* ((info (FIRST (FIRST Stream)))
               (elem (CASE (SVREF info 0)
                       (<string> (SVREF info 1))
                       ;; added [2018-06-23]
                       (<lifted> (SVREF info 1))
                       (<lisp>   (LET* ;((info (MAKE-compiler-data-as-fake-string :data (SVREF info 1))))
                                     ((lisp-code (SVREF info 1))
                                      (op (WHEN (CONSP lisp-code) (FIRST lisp-code)))
                                      (info (make-fake-string lisp-code)))
                                   (COND ((AND op (EQL :WITH op))
                                           (UNLESS (AND (>= (LIST-LENGTH lisp-code) 2)
                                                        (OR (SYMBOLP (SECOND lisp-code))
                                                            (AND (LISTP (SECOND lisp-code))
                                                                 (EVERY (LAMBDA (c) (OR (SYMBOLP c)
                                                                                        (AND (LISTP c)
                                                                                             (EQL 2 (LIST-LENGTH c))
                                                                                             (SYMBOLP (FIRST c))
                                                                                             (var? (SECOND c)))))
                                                                        (SECOND lisp-code)))))
                                             (ERROR "Xi: :WITH Syntax Error"))

                                           (UNLESS (LISTP (SECOND lisp-code))
                                             (SETF (SECOND lisp-code) (ENSURE-LIST (SECOND lisp-code))))
                                           
                                           `(<<lisp-code-with-xi-vars>>
                                             ,info
                                             ,(<to-consing> (MAPCAR (LAMBDA (c) (IF (LISTP c) (SECOND c) c))
                                                                    (SECOND lisp-code)))))
                                         
                                         ((AND op (MEMBER op '(:MATCH :UNIFY)))  `(<<external-bind>> ,info))
                                         ((AND op (MEMBER op '(:BIND)))  `(<<external-bind>> ,info))
                                         (T   `(<<lisp-code>> ,info)))))
                       (t info)))
               (<st_input> (<st_input> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
          (IF (NOT (failure? <st_input>))
            (LIST (FIRST <st_input>) (CONS elem (SECOND <st_input>))) NIL))
        NIL))

    
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\[))
    (LET
     ((<st_input1> (<st_input1> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <st_input1>))
      (IF
       (AND (CONSP (FIRST <st_input1>)) (EQL (FIRST (FIRST <st_input1>)) #\]))
       (LET
        ((<st_input2>
          (<st_input2>
           (LIST (REST (FIRST <st_input1>)) (SECOND <st_input1>)))))
        (IF (NOT (failure? <st_input2>))
         (LIST (FIRST <st_input2>)
          (CONS (user-syntax-in (cons_form (SECOND <st_input1>))) (SECOND <st_input2>)))
         NIL))
       NIL)
      NIL))
    NIL))
   (BLOCK localfailure
  (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\())
   (LET
    ((<st_input1> (<st_input1> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
    (IF (NOT (failure? <st_input1>))
     (IF
      (AND (CONSP (FIRST <st_input1>)) (EQL (FIRST (FIRST <st_input1>)) #\)))
      (LET
       ((<st_input2>
         (<st_input2>
          (LIST (REST (FIRST <st_input1>)) (SECOND <st_input1>)))))
       (IF (NOT (failure? <st_input2>))
        (LIST (FIRST <st_input2>)
         (CONS (user-syntax-in (proc_specialforms (SECOND <st_input1>))) (SECOND <st_input2>)))
        NIL))
      NIL)
     NIL))
   NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\|))
    (LET
     ((<st_input> (<st_input> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <st_input>))
      (LIST (FIRST <st_input>) (CONS 'bar# (SECOND <st_input>))) NIL))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\{))
    (LET
     ((<st_input> (<st_input> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <st_input>))
      (LIST (FIRST <st_input>) (CONS '{ (SECOND <st_input>))) NIL))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\}))
    (LET
     ((<st_input> (<st_input> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <st_input>))
      (LIST (FIRST <st_input>) (CONS '} (SECOND <st_input>))) NIL))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\;))
    (LET
     ((<st_input> (<st_input> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <st_input>))
      (LIST (FIRST <st_input>) (CONS (semi-colon) (SECOND <st_input>))) NIL))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\,))
    (LET
     ((<st_input> (<st_input> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <st_input>))
      (LIST (FIRST <st_input>) (CONS (comma) (SECOND <st_input>))) NIL))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\"))
    (LET ((<string> (<string> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <string>))
      (IF (AND (CONSP (FIRST <string>)) (EQL (FIRST (FIRST <string>)) #\"))
       (LET
        ((<st_input>
          (<st_input> (LIST (REST (FIRST <string>)) (SECOND <string>)))))
        (IF (NOT (failure? <st_input>))
         (LIST (FIRST <st_input>)
          (CONS (FORMAT NIL "~{~A~}" (SECOND <string>)) (SECOND <st_input>)))
         NIL))
       NIL)
      NIL))
    NIL))
  (BLOCK localfailure
   (LET ((<character> (<character> Stream)))
    (IF (NOT (failure? <character>))
     (LET ((<st_input> (<st_input> <character>)))
      (IF (NOT (failure? <st_input>))
       (LIST (FIRST <st_input>)
        (CONS (user-syntax-in (tokenise (SECOND <character>))) (SECOND <st_input>)))
       NIL))
     NIL)))
  (BLOCK localfailure
   (LET ((<comment> (<comment> Stream)))
    (IF (NOT (failure? <comment>))
     (LET ((<st_input> (<st_input> <comment>)))
      (IF (NOT (failure? <st_input>)) <st_input> NIL))
     NIL)))
  (BLOCK localfailure
   (LET ((<token> (<token> Stream)))
    (IF (NOT (failure? <token>))
     (LET ((<st_input> (<st_input> <token>)))
      (IF (NOT (failure? <st_input>))
       (LIST (FIRST <st_input>)
        (CONS (user-syntax-in (tokenise (SECOND <token>))) (SECOND <st_input>)))
       NIL))
     NIL)))
  (BLOCK localfailure
   (LET ((<whitespaces> (<whitespaces> Stream)))
    (IF (NOT (failure? <whitespaces>))
     (LET ((<st_input> (<st_input> <whitespaces>)))
      (IF (NOT (failure? <st_input>)) <st_input> NIL))
     NIL)))
  (BLOCK localfailure
   (LET ((<e> (<e> Stream)))
    (IF (NOT (failure? <e>)) (LIST (FIRST <e>) NIL) NIL)))))

(DEFUN comma () '|,|)

(DEFUN esc () #\Escape)



;; Added by JUN
;; シグネチャ中の->を-->に置き換える
;; エスケープされたコンマを*に置き換える(タプルの型表現)
(DEFUN <fix-signature> (XS)
  (IF (CONSP XS)
    (MAPCAR #'(LAMBDA (X)
                (COND ((CONSP X) (<fix-signature> X))
                      ((EQL X '->) '-->)
                      ((EQL X esc_comma) '*)
                      (T X)
                  ))
            XS)
    XS))


(DEFCONSTANT esc_comma '|<,>|)
(DEFUN <as-a-comma?> (x)
  (OR (EQL esc_comma x)
      (EQL '|,| x)))
       
;; コンマ入りの(exp,exp,...)形式の複合式であるかのチェック
;; 通常の式であればNIL、複合式であれば要素数を返す
(DEFUN <complex-expressions-p> (x &KEY
                                raise-error
                                (error-string "~W is not a valid complex expressions"))
  (WHEN (AND (CONSP x)
             (NOT (EQL '|,| (FIRST x)))
             (MEMBER-IF #'<as-a-comma?> x))
    ;; 複合式の要件を満たすので、形式に問題がないか検証
    (LET* ((m (COUNT-IF #'<as-a-comma?> x))
           (n (LIST-LENGTH x))
           (result (WHEN (AND (ODDP n)
                              (EQL m (/ (1- n) 2)))
                     (DO ((p (CDR x) (CDDR p)))
                         ((NULL p) (- n m))
                       (UNLESS (AND (<as-a-comma?> (FIRST p))
                                    (NOT (<as-a-comma?> (SECOND p))))
                         (RETURN NIL))))))
      (IF result
        result
        (WHEN raise-error
          (ERROR "not a complex expression"))))))
           
;; 複合式をリスト化
;; 入力元の複合式は、<complex-expressions-p>が非NILを返すことを前提とする
(DEFUN <list-complex-expressions> (complex-exps)
  (DO ((p complex-exps (CDDR p))
       tmp)
      ((NULL p) (NREVERSE tmp))
    (PUSH (FIRST p) tmp)))
             
(DEFUN <ensure-complex-expressions> (x)
  (IF (<complex-expressions-p> x :raise-error T)
    (<list-complex-expressions> x)
    (LIST x)))

      
;; [2018-07-14] Added
(DEFUN <arrow?> (x) (OR (EQ '-> x) (EQ '<- x)))
(DEFUN <not-arrow?> (x) (NOT (<arrow?> x)))
(DEFUN <transform/case-where> (xs &AUX (len (LIST-LENGTH xs)))
  (WHEN (OR (MEMBER-IF (LAMBDA (x) (MEMBER x '(|,|)))
                       xs)
            (NOT (EQL 0 (MOD len 3))))
    (ERROR "(case where ... SYNTAX ERROR: ~A" xs))
  

  (DO ((H xs (CDDDR H))
       tmp)
      ((NULL H) (APPLY #'NCONC (NREVERSE tmp)))
    (LET ((test-exp (FIRST H))
          (arrow (SECOND H))
          (main-exp (THIRD H)))
      (UNLESS (AND (<not-arrow?> test-exp) (<arrow?> arrow) (<not-arrow?> main-exp))
        (ERROR "(case where... SYNTAX ERROR: ... ~A ~A ~A ..." test-exp arrow main-exp))
      (PUSH (LIST '_ '|,| test-exp arrow main-exp) tmp))))
       
;; Added by JUN
(DEFUN <rule/body/transform> (xs)
  (WHEN (MEMBER-IF (LAMBDA (x) (MEMBER x '(<- |,|)))
                   xs)
    (SETQ xs (COPY-LIST xs))
    ;; 1) 前処理
    ;; <-と@satisfies節の変換処理
    (DO ((H (MEMBER '<- xs) (MEMBER '<- (CDDR H))))
        ((NULL H))
      (COND ((EQL (THIRD H) '|,|)
              (SETF (SECOND H) `(fail-if (/. V (not (,(FOURTH H) V))) ,(SECOND H)))
              (SETF (CDDR H) (CDDDDR H)))
            (T
              (SETF (SECOND H) `(fail-if failed? ,(SECOND H))))))
    ;; 2) 後処理
    ;; @where節の処理
    (DO ((H (MEMBER '|,| xs) (MEMBER '|,| (CDDDDR H))))
        ((NULL H))
      ;; [,] test-exp [<-|->] exp から [<-|->] exp where test-expに変換
      ;; 使用するセル数は変わらないので、置換で済ませる
      (LET ((test-exp (SECOND H))
            (arrow    (THIRD H))
            (main-exp (FOURTH H)))
        (SETF (FIRST H)   arrow
              (SECOND H)  main-exp
              (THIRD H)   'where
              (FOURTH H)  test-exp))))
  xs)
  
(DEFUN <def/transform> (V1)
  (COND ((AND (CONSP (CDR V1))
              (CONSP (SECOND V1)) (EQL '|:| (SECOND (SECOND V1))))
          (LET ((C (SECOND V1))
                (BODY (CDDR V1)))
            (IF (AND (= 3 (LENGTH C)) (EQL 'datatype (THIRD C)))
              ;; (datatype ...)に展開
              (proc_specialforms `(datatype ,(FIRST C) ,@BODY))
              ;; (define f {signature...} body...)に展開
              (proc_specialforms `(define ,(FIRST C) { ,@(CDDR C) } ,@(<rule/body/transform> BODY))))))
        (T
          ;; (define ...)に展開
          (proc_specialforms `(define ,(SECOND V1) ,@(<rule/body/transform> (CDDR V1)))))))


;; OBSOLETE
;; xiのデータ表現のリストからcons表現への変換 (再帰的にツリーを構成)
;; (a (b c) d) --> (cons a (cons (cons b (cons c NIL)) (cons d NIL)))
;; (DEFUN <to-consing-deeply> (xs)
;;   (COND ((ATOM xs) xs)
;;         (T `(cons ,(<to-consing-deeply> (CAR xs)) ,(<to-consing-deeply> (CDR xs))))))

(DEFUN <decl/transform> (V1 &AUX tmp)
  (LABELS ((deep-consing (xs)
             (COND ((ATOM xs) xs)
                   (T `(cons ,(deep-consing (CAR xs)) ,(deep-consing (CDR xs)))))))
    
    (DOLIST (x (CDR V1) (LIST* '<xi/codes> (NREVERSE tmp)))
      (COND ((AND (CONSP x) (EQL 'type (FIRST x)) (EQL '|:| (THIRD x)) (>= (LENGTH x) 4))
              (LET* ((fixed (<fix-signature> (CDDDR x)))
                     (signature (IF (= 1 (LENGTH fixed)) (FIRST fixed) fixed)))
                (PUSH `(synonyms ,(SECOND x) ,signature)
                      tmp)))
            ((AND (CONSP x) (EQL '|:| (SECOND x)) (>= (LENGTH x) 3))
              (LET* ((fixed (<fix-signature> (CDDR x)))
                     (signature fixed))
                (PUSH `(declare ,(FIRST x) ,(deep-consing signature))
                      tmp)))
            (T (ERROR "Xi: decl: illegal clause ~D" x))))))



;; Added by JUN
(DEFUN <fix-where-pattern> (ptn)
  ;; ワイルドカード _ を一時変数に置換 (自動でなされないため)
  (COND ((CONSP ptn)
          (DO ((xs ptn (CDR xs)))
              ((NULL xs) ptn)
            (IF (EQL (CAR xs) '_)
              (SETF (CAR xs) (GENSYM "X"))
              (<fix-where-pattern> (CAR xs)))))
        ((EQL '_ ptn) (GENSYM "X"))
        (T ptn)))

(DEFUN <expand-where-bind-syntax> (V)
  ;; (where A E1 B E2 C E3 MAIN) --> ((((/. A (/. B (/. C MAIN))) E1) E2) E3)
  (LET ((main-exp (CAR (LAST V)))
        (pair-stream (BUTLAST V))
        params
        patterns)
    (DO ((xs pair-stream (CDDR xs)))
        ((NULL xs))
      (PUSH (<fix-where-pattern> (FIRST xs)) patterns)
      (PUSH (SECOND xs) params))

    (LET ((nested-lambdas
            (REDUCE (LAMBDA (ptn exp) `(|/.| ,ptn ,exp))
                    (<fix-where-pattern> (NREVERSE patterns))
                    :INITIAL-VALUE main-exp :FROM-END T)))
      ;(PRINT (compile-to-lisp 'unk (compile-to-lambda+ 'unk `((,params ,nested-lambdas)))))
      (REDUCE #'LIST
              (NREVERSE params)
              :INITIAL-VALUE nested-lambdas))))

(DEFUN <expand-where*-bind-syntax> (V)
  ;; (where* A E1 B E2 MAIN) --> (where A E1 (where B E2 MAIN))の等価表現
  ;; (where* A E1 B E2 C E3 MAIN) --> ((/. A ((/. B ((/. C MAIN) E3)) E2) E1))
  
  (LET ((main-exp (CAR (LAST V)))
        (pair-stream (BUTLAST V))
        pattern-and-param-pairs
        params
        patterns)
    (DO ((xs pair-stream (CDDR xs)))
        ((NULL xs))
      (PUSH (CONS (<fix-where-pattern> (FIRST xs))
                  (SECOND xs))
            pattern-and-param-pairs))

    (REDUCE (LAMBDA (exp pair) `((/. ,(CAR pair) ,exp) ,(CDR pair)))
            pattern-and-param-pairs
            :INITIAL-VALUE main-exp)))

;; DUMMY FORMS
;;(DEFMACRO <<dummy-form-1>> (x) (DECLARE (IGNORE x)))

(DEFCONSTANT true 'true)
(DEFCONSTANT false 'false)




(DEFUN proc_specialforms (V1)
  ;;(PRINT (LIST V1))
  (COND

    ;; Added NOW TESTING...
    ((AND (CONSP V1) (EQ 'case (CAR V1)))
      (IF (EQ 'where (SECOND V1))
        `(<<case>> (NIL)
                   ,@(proc_specialforms (<rule/body/transform> (<transform/case-where> (CDDR V1)))))
        `(<<case>> ,(proc_specialforms (<ensure-complex-expressions> (SECOND V1)))
                   ,@(proc_specialforms (<rule/body/transform> (CDDR V1))))))

    ;; HACK JUN
    ;; Tupleの型表現 (A,B,C,...) --> (A * B * C * ...)
    ((AND (CONSP V1)
          (NOT (EQL '|,| (CAR V1)))
          (MEMBER '|,| V1)
          (ODDP (LENGTH V1))
          (= (1+ (* 2 (COUNT '|,| V1)))
             (LENGTH V1)))
      ;;(PRINT V1)
      
      ;(proc_specialforms (LIST '|@p| (REMOVE '|,| V1)))
      (proc_specialforms (MAPCAR (LAMBDA (X) (IF (EQL '|,| X) esc_comma X))
                                 V1))
      )

    ;; HACK JUN (, ...) --> (@p ...)
    ;; HACK JUN (# ...) --> (; ...) --> (@sv ...)
    ((AND (CONSP V1) (MEMBER (CAR V1) '(|,| |;|)))
      (LET ((op (IF (EQ '|,| (CAR V1)) '|@p| '|@sv|)))
        (UNLESS (IF (AND (CDR V1) (CDR (CDR V1)) (EQ '|,| (THIRD V1)))
                  ;; 第二要素がコンマの場合
                  ;; コンマと式が交互に並んでいるかを検証
                  (AND (ODDP (LIST-LENGTH (CDR V1)))
                       (LET ((idx 0))
                         (DOLIST (elem (CDR V1) T)
                           (WHEN (IF (EVENP idx) (EQ '|,| elem) (NOT (EQ '|,| elem)))
                             (RETURN NIL))
                           (INCF idx))))
                  ;; 第二要素がコンマである場合を除き、コンマが現れることは許されない
                  (NULL (MEMBER '|,| (CDR V1))))
          ;; 不正なコンマ位置の場合はエラー
          (ERROR "xi: misplaced commma: ~A" (CDR V1)))
        (proc_specialforms (LIST* op (REMOVE '|,| (CDR V1))))))


    ;; HACK JUN ({cl|cl*} form-name args...) --> (<<native-lisp-form>> ...)
    ((AND (CONSP V1) (MEMBER (CAR V1) '(cl cl*)))
      (LET* ((arg-exps (CDDR V1))
             (tmp-vars (MAPCAR (LAMBDA (X) (DECLARE (IGNORE X)) (GENSYM "V")) arg-exps))
             (bindings (MAPCAN #'LIST tmp-vars arg-exps))
             (operator (CASE (CAR V1)
                         (cl  '<<native-lisp-call>>)
                         (cl* '<<native-lisp-apply>>))))
        (proc_specialforms `(let
                                ,@bindings
                              (,operator
                               ,(make-fake-string (SECOND V1))
                               ,(<to-consing> tmp-vars))))))


    
    ;; HACK JUN
    ;; (/. -> exp) --> expへの変換
   ;n;((AND (CONSP V1) (EQL '/. (CAR V1)) (EQL '-> (SECOND V1)) (= 3 (LENGTH V1)))
   ;;  (proc_specialforms (THIRD V1)))
    
   ;; LISPにそのまま渡せる多引数ラムダ式
   ;; (/. A1 .... -> Exp) --> lambdaとcaseとのコンビネーションに変換
    ((AND (CONSP V1) (OR (EQ '/. (CAR V1)) (EQ 'lambda (CAR V1)))
          (CONSP (CDR V1)) (CONSP (CDR (CDR V1)))
          (CONSP (CDR (CDR (CDR V1))))
          (MEMBER '-> (CDR V1))
          )
      (LET* ((curry (EQ '/. (CAR V1)))
             (arity (POSITION-IF (LAMBDA (x) (MEMBER x '(-> |,|))) (CDR V1)))
             ;; arityは、最初に->もしくは,が現れる場所によって定める
             ;; もしも|,| が->に先んじて現れる場合、それはガード以外にはない
             (params (FREPLICATE arity (LAMBDA () (GENSYM "C"))))
             (case-body `(<<case>> ,params ,@(proc_specialforms (<rule/body/transform> (CDR V1)))))
             )
        (IF curry
          (REDUCE (LAMBDA (a b) `(/. ,a ,b)) params :FROM-END T :INITIAL-VALUE case-body)
          `(LAMBDA ,params ,case-body))))


   ;; 多引数ラムダ式の展開 (カリー化された関数)
   ;; (/. A1 A2 .... An Exp) ==> (/. A1 (/. A2  ... (/. An Exp)))
   ((AND (CONSP V1) (EQ '/. (CAR V1)) (CONSP (CDR V1)) (CONSP (CDR (CDR V1)))
         (CONSP (CDR (CDR (CDR V1)))))
     (LET* ((V2 (CDR V1)))
       (LIST '/. (CAR V2) (proc_specialforms (CONS '/. (CDR V2))))))

   ;; 任意引数ラムダ式の展開 (カリー化されない関数)
   ;; (lambda A1 A2 .... An Exp) ==> (LAMBDA (A1..An) Exp)
   ;; 無引数も合法である (lambda Exp) ==> (LAMBDA () Exp)
   ((AND (CONSP V1) (EQ 'lambda (CAR V1)) (CONSP (CDR V1)))
     (LET* ((params (BUTLAST (CDR V1)))
            (exp    (proc_specialforms (LASTCAR V1))))
       `(LAMBDA ,params ,exp)))

   ;; Jun HACKED -- OBSOLETE-- 
   ;; 多束縛Letフォームの展開 
   ;;(let* A1 e1 A2 e2 ... exp) ==> (let A1 e1 (let A2 e2 (let An en exp)))
   ;; ((AND (CONSP V1) (EQ 'let* (CAR V1)) (CONSP (CDR V1)) (CONSP (CDR (CDR V1)))
   ;;       (CONSP (CDR (CDR (CDR V1)))) (CONSP (CDR (CDR (CDR (CDR V1))))))
   ;;   (LET* ((V3 (CDR V1)) (V4 (CDR V3)))
   ;;     (LIST 'let (CAR V3) (CAR V4) (proc_specialforms (CONS 'let* (CDR V4))))))

   ;; let* を (let A B (let ... Exp))形式に変換 (この形式でなければ型システムが正常に働かないため)
   ;; (let ...)で書き下されている場合は何もしない(型システムを通さないことを前提としたコーディングである)
   ;; TODO Errorを書き換えよ
   ((AND (CONSP V1) (EQ 'let* (CAR V1)))
     (COND ((NULL (CDR V1)) (ERROR "jun let error"))
           ((NULL (CDDR V1)) (proc_specialforms (SECOND V1)))
           ((<= 3 (LIST-LENGTH (CDR V1)))
             (LET ((dst (SECOND V1))
                   (src (proc_specialforms (THIRD V1)))
                   (exp (proc_specialforms (CONS 'let* (CDDDR V1)))))
               (COND ((var? dst) (LIST 'let dst src exp))
                     ((AND (CONSP dst) (EQL '<<external-bind>> (FIRST dst)))
                       (LET* ((info (SECOND dst))
                              (lisp (SVREF info 1))
                              (external-bind-op (FIRST lisp))
                              (binder (CASE external-bind-op
                                        (:BIND  '<<lisp/bind>>)
                                        (:MATCH '<<lisp/match>>)
                                        (:UNIFY '<<lisp/unify>>))))
                         (LIST binder info src exp)))
                     (T (proc_specialforms (LIST 'case src dst '-> exp))))))
           (T (ERROR "jun let error!"))))

   ;; Added by JUN
   ;; where bindフォームの展開 
   ((AND (CONSP V1) (OR (EQ 'where (CAR V1)) (EQ 'where* (CAR V1))))
     (proc_specialforms (IF (EQ 'where (CAR V1))
                          (<expand-where-bind-syntax> (CDR V1))
                          (<expand-where*-bind-syntax> (CDR V1)))))

   ;; 特殊コンストラクタの変換 ただし、(op arg1 arg2) or (op arg1) or (op)の形の場合のみ
   ((AND (CONSP V1) (MEMBER (CAR V1) '(|@c|)) (NULL (CDDDR V1)))
     (LET ((V5 (CDR V1))
           (dst-op (CASE (CAR V1) (|@c| 'cons))))
       (proc_specialforms (LIST* dst-op (CAR V5) (CDR V5)))))
     

   ;; OBSOLETE [2018-09-17]
   ;; HACK [2018-09-14]
   ;; ３値以上のタプルの展開
   ;; (@p a b c) ==> (@p a (@p b c))
   ;; ((AND (CONSP V1) (EQ (CAR V1) '|@p|) (CONSP (CDR V1)) (CONSP (CDDR V1))
   ;;       (CONSP (CDDDR V1)))
   ;;   ;(PRINT (LIST 'DEB (LIST '|@p| 'tuple3over (proc_specialforms (CONS '|@sv| (CDR V1))))))
   ;;   (LIST '|@p| 'tuple3over (proc_specialforms (CONS '|@sv| (CDR V1))))
   ;;   )

   ;; RE-HACK [2018-09-17]
   ;; ３値以上のタプル表現の変換
   ;; それぞれのクラスに属するようにコンストラクタを取得
   ((AND (CONSP V1) (EQ (CAR V1) '|@p|) (CONSP (CDR V1)) (CONSP (CDDR V1))
         (CONSP (CDDDR V1)))
     (LET ((arity (LIST-LENGTH (CDR V1))))
       (WHEN (> arity *TUPLE-MAX*) 
         (ERROR "(xi: in (@p ...): too many elements: MAX=~A" *TUPLE-MAX*))
       (proc_specialforms (CONS (SVREF OLEO.ALGEBRAIC.CORE::%TUPLE-CONVERSION-VECTOR% arity) (CDR V1)))))

   ;; HACK [2018-09-14] 効率のよい実装にすべき
   ;; ３値以上の@cの展開
   ;; (@c a b c) ==> (@c a (cons b c))
   ((AND (CONSP V1) (MEMBER (CAR V1) '(|@c|)) (CONSP (CDR V1)) (CONSP (CDDR V1))
         (CONSP (CDDDR V1)))
     (LET* ((V5 (CDR V1))
            (src-op (CAR V1))
            (dst-op (CASE src-op (|@c| 'cons))))
       (LIST dst-op
             (proc_specialforms (CAR V5))
             (proc_specialforms (CONS src-op (CDR V5))))))

   ;;; TEST HACK JUN
   ;;; (symbol#) -> (@p symbol# [])
   ;;; (symbol# x) -> (@p symbol# x)
   ;;; (symbol# x y...) -> (@p symbol# (# x y...))
   ((AND (CONSP V1) (SYMBOLP (CAR V1))
         (GET (CAR V1) 'OLEO.ALGEBRAIC.XDATA::|%data_tag%|)
         ;; (NOT (MEMBER (CAR V1) '(|cons| |@p|))) ;; <<- この判定は単なる高速化のため 
         ;; (LET ((symName (SYMBOL-NAME (CAR V1))))  
         ;;   (EQ #\# (CHAR symName (1- (LENGTH symName)))))
         )

     ;;(PRINT V1)
     ;;(UNLESS (GET (CAR V1) 'OLEO.ALGEBRAIC.XDATA::|%data_tag%|) (WARN "xi: (~A ...) is unknown type constructor" (CAR V1)))

     (LET* ((ctor (CAR V1))
            (data-tag (GET ctor 'OLEO.ALGEBRAIC.XDATA::|%data_tag%|))
            (data-arity (GET ctor 'OLEO.ALGEBRAIC.XDATA::|%data_arity%|))
            (args (CDR V1))
            (n (LENGTH args)))
       (proc_specialforms
        (COND ((EQL data-arity n)
                (CASE n
                 (0 (LIST '|@p| data-tag NIL))
                 (1 (LIST '|@p| data-tag (CAR args)))
                 (T `(|@p| ,data-tag (|@sv| ,@args)))))
              ((> n data-arity) (ERROR "too many arguments for (~A ...): ~A" ctor V1))
              ((EQL 0 n) (ERROR "illegal data construction: (~A)" ctor))
              (T (LET* ((m (- data-arity n))
                        (tmpvars (FREPLICATE m (LAMBDA () (GENSYM "DC")))))
                   `(/. ,@tmpvars (|@p| ,data-tag (|@sv| ,@args ,@tmpvars))))))
          
        )))

        

   ;; JUN HACK new syntax `def'
   ;; (def (f : signature...) body...) --> (define f { signature...} body...)
   ;; (def (ident : datatype) body...) --> (datatype ident body...)
   ;; (def f body...) --> (define f body...)
   ((AND (CONSP V1) (EQ 'def (CAR V1)))
     (<def/transform> V1))

   ;; JUN HACK new syntax `decl'
   ;; (decl ...)
   ;; (decl (f:signature)) --> (declare f [...])
   ;; (decl (type a : b) --> (synonyms a b)
   ((AND (CONSP V1) (EQ 'decl (CAR V1)))
     (<decl/transform> V1))
                
   ;; define構文の展開
   ((AND (CONSP V1) (EQ 'define (CAR V1)) (CONSP (CDR V1))
         (CONSP (CDR (CDR V1))) (EQ '|{| (CAR (CDR (CDR V1))))
         (MEMBER '|}| (CDR (CDR (CDR V1)))))
     (LET* ((V8 (CDR V1)) (V9 (CDR V8)) (V10 (CDR V9)))
       (LET ((Signature (normalise-type (curry-type (collect-signature V10)))))
         (LET ((Rules (collect-rules V10)))
           ;; HACK JUN
           ;; 型シグネチャの変換 (内部の->を-->に変換)
           (SETQ Signature (<fix-signature> Signature))
           (CONS 'define
                 (CONS (CAR V8)
                       (CONS '|{| (THE LIST (APPEND Signature (LIST '|}|) Rules)))))))))
   
   
   ;; ユーザ定義のQi構文 define-qi-syntaxで定義されたものの適用
   ((AND (CONSP V1) (SYMBOLP (CAR V1)) (GET (CAR V1) '%qi-syntax%))
     (LET ((fn (GET (CAR V1) '%qi-syntax%)))
       (proc_specialforms (APPLY fn (CDR V1)))))
   
   ;; それ以外のフォーム（無展開）
   (T V1)))

(DEFUN semi-colon () '|;|)

(DEFUN user-syntax-in (X) (apply-user-syntax X *syntax-in*))

(DEFUN collect-rules (V557)
 (COND ((AND (CONSP V557) (EQ '} (CAR V557))) (CDR V557))
  ((CONSP V557) (collect-rules (CDR V557))) 
  (T (implementation_error 'collect-rules))))

(DEFUN collect-signature (V552)
 (COND ((AND (CONSP V552) (EQ '} (CAR V552))) NIL)
  ((CONSP V552) (CONS (CAR V552) (collect-signature (CDR V552))))
  (T (implementation_error 'collect-signature))))


(SETQ *syntax-in* NIL)

(DEFUN sugar (DIRECTION F N)
  (IF (NOT (SYMBOLP F)) (ERROR "~A must be a symbol~%" F))
  (COND ((EQ DIRECTION 'in) (SETQ *syntax-in* (set-user-syntax F N *syntax-in*)))
        ((EQ DIRECTION 'out) (SETQ *syntax-out* (set-user-syntax F N *syntax-out*)))
        (T (ERROR "direction must be in or out~%")))
  F)

(DEFUN sugar-list (DIRECTION)
  (COND ((EQ DIRECTION 'in) *syntax-in*)
        ((EQ DIRECTION 'out) *syntax-out*)
        (T (ERROR "direction must be in or out~%"))))
 
(DEFUN unsugar (F)
 (SETQ *syntax-in* (remove F *syntax-in*)) 
 (SETQ *syntax-out* (remove F *syntax-out*))
 F)   

(DEFUN apply-user-syntax (X Fs)
   (IF (NULL Fs)
       X
       (apply-user-syntax (FUNCALL (CAR Fs) X) (CDR Fs))))

(DEFUN syntax-in (F N)
  (SETQ *syntax-in* (set-user-syntax F N *syntax-in*)))

(DEFUN set-user-syntax (V5 V6 V7)
 (COND ((EQL -1 V6) (THE LIST (REMOVE V5 V7))) 
  ((EQL 1 V6) (CONS V5 V7))
  ((NULL V7) (LIST V5))
  ((CONSP V7) (CONS (CAR V7) (set-user-syntax V5 (1- V6) (CDR V7))))
  (T (implementation_error 'set-user-syntax))))
   
(DEFUN <character> (Stream)
 (OR
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\#))
    (IF
     (AND (CONSP (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
      (EQL (FIRST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))) #\\))
     (LET
      ((<token>
        (<token>
         (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
          (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream)))))))
      (IF (NOT (failure? <token>))
       (LIST (FIRST <token>) (CONS #\# (CONS #\\ (SECOND <token>)))) NIL))
     NIL)
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\#))
    (IF
     (AND (CONSP (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
      (EQL (FIRST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))) #\\))
     (LET
      ((<it>
        (<it>
         (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
          (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream)))))))
      (IF (NOT (failure? <it>))
       (LIST (FIRST <it>) (CONS #\# (CONS #\\ (SECOND <it>)))) NIL))
     NIL)
    NIL))))

(DEFUN <it> (Stream)
 (OR
  (BLOCK localfailure
   (IF (CONSP (FIRST Stream))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
     (CONS (CAAR Stream) NIL))
    NIL))))

(DEFUN <st_input1> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<st_input> (<st_input> Stream)))
    (IF (NOT (failure? <st_input>)) <st_input> NIL)))))

(DEFUN <st_input2> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<st_input> (<st_input> Stream)))
    (IF (NOT (failure? <st_input>)) <st_input> NIL)))))

(DEFUN <comment> (Stream)
 (OR
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\\))
    (LET ((<any> (<any> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <any>))
      (IF (AND (CONSP (FIRST <any>)) (EQL (FIRST (FIRST <any>)) #\\))
       (LIST (FIRST (LIST (REST (FIRST <any>)) (SECOND <any>))) NIL) NIL)
      NIL))
    NIL))))

(DEFUN <any> (Stream)
 (OR
  (BLOCK localfailure
   (IF
    (AND (CONSP (FIRST Stream))
     (wrapper (not-slash? (FIRST (FIRST Stream)))))
    (LET ((<any> (<any> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <any>)) (LIST (FIRST <any>) NIL) NIL))
    NIL))
  (BLOCK localfailure
   (LET ((<e> (<e> Stream)))
    (IF (NOT (failure? <e>)) (LIST (FIRST <e>) NIL) NIL)))))

(DEFUN not-slash? (V10) (COND ((EQL #\\ V10) 'false) (T 'true)))

(DEFUN <string> (Stream)
 (OR
  (BLOCK localfailure
   (IF
    (AND (CONSP (FIRST Stream))
     (wrapper (not-double-quote? (FIRST (FIRST Stream)))))
    (LET ((<string> (<string> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <string>))
      (LIST (FIRST <string>) (CONS (CAAR Stream) (SECOND <string>))) NIL))
    NIL))
  (BLOCK localfailure
   (LET ((<e> (<e> Stream)))
    (IF (NOT (failure? <e>)) (LIST (FIRST <e>) NIL) NIL)))))

(DEFUN not-double-quote? (V15) (COND ((EQL #\" V15) 'false) (T 'true)))

(DEFUN <token> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<alphanum> (<alphanum> Stream)))
    (IF (NOT (failure? <alphanum>))
     (LET ((<token> (<token> <alphanum>)))
      (IF (NOT (failure? <token>))
       (LIST (FIRST <token>) (CONS (SECOND <alphanum>) (SECOND <token>))) NIL))
     NIL)))
  (BLOCK localfailure
   (LET ((<alphanum> (<alphanum> Stream)))
    (IF (NOT (failure? <alphanum>))
     (LIST (FIRST <alphanum>) (CONS (SECOND <alphanum>) NIL)) NIL)))))


(DEFUN tokenise (V121)
 (COND ((AND (CONSP V121) (EQL #\> (CAR V121)) (NULL (CDR V121))) 'qi_>)
  ((AND (CONSP V121) (EQL #\> (CAR V121)) (NULL (CDR V121))) 'qi_>)
  ((AND (CONSP V121) (EQL #\< (CAR V121)) (NULL (CDR V121))) 'qi_<)
  ((AND (CONSP V121) (EQL #\= (CAR V121)) (NULL (CDR V121))) 'qi_=)
  ((AND (CONSP V121) (EQL #\: (CAR V121)) (NULL (CDR V121))) (colon))
  ((AND (CONSP V121) (EQL #\> (CAR V121)) (CONSP (CDR V121))
    (EQL #\= (CAR (CDR V121))) (NULL (CDR (CDR V121))))
   'qi_>=)
  ((AND (CONSP V121) (EQL #\< (CAR V121)) (CONSP (CDR V121))
    (EQL #\= (CAR (CDR V121))) (NULL (CDR (CDR V121))))
    'qi_<=)
  ;; Added by JUN
  ((AND (CONSP V121) (EQL #\/ (CAR V121)) (CONSP (CDR V121))
    (EQL #\= (CAR (CDR V121))) (NULL (CDR (CDR V121))))
    'qi_/=)
  ;; シンボルとしての扱い
  (T (LET ((symbol-name (FORMAT NIL "~{~C~}" V121)))
       (IF (%LOCAL-INTERN-NEEDED-P% symbol-name)
         ;; 大文字で始まるシンボル、<や~で始まるシンボル等は呼び出し元パッケージでREADさせる
         ;; ~で始まるシンボルなどは、かなり特殊な扱いになる
         (%LOCAL-INTERN% symbol-name *<CALLER-PACKAGE>*)
         ;; そうでないシンボルは、QSPACEでREADされる
         (READ-FROM-STRING symbol-name))))))
    

(DEFUN colon () '|:|)

(DEFUN <alphanum> (Stream)
 (OR
  (BLOCK localfailure
   (IF
    (AND (CONSP (FIRST Stream)) (wrapper (alpha? (FIRST (FIRST Stream)))))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (CAAR Stream))
    NIL))))

(DEFUN alpha? (V17)
 (IF
  (MEMBER V17
   '(#\; #\, #\\ #\[ #\] #\( #\) #\} #\{ #\" #\Space #\Return #\Newline #\Tab #\|)
   :TEST 'CHAR-EQUAL)
  'false 'true))

(DEFUN <whitespaces> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<whitespace> (<whitespace> Stream)))
    (IF (NOT (failure? <whitespace>))
     (LET ((<whitespaces> (<whitespaces> <whitespace>)))
      (IF (NOT (failure? <whitespaces>)) (LIST (FIRST <whitespaces>) NIL) NIL))
     NIL)))
  (BLOCK localfailure
   (LET ((<whitespace> (<whitespace> Stream)))
    (IF (NOT (failure? <whitespace>)) (LIST (FIRST <whitespace>) NIL) NIL)))))

(DEFUN <whitespace> (Stream)
 (OR
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\Space))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
     (CONS #\Space NIL))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\Return))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
     (CONS #\Return NIL))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\Newline))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
     (CONS #\Newline NIL))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\Tab))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
     (CONS #\Tab NIL))
    NIL))))

;; (DEFUN cons_form (V18)
;;   (COND
;;     ((NULL V18) NIL) 
;;     ((AND (CONSP V18) (CONSP (CDR V18)) (EQ 'bar# (CAR (CDR V18)))
;;           (CONSP (CDR (CDR V18))) (NULL (CDR (CDR (CDR V18)))))
;;       (CONS 'cons (CONS (CAR V18) (CDR (CDR V18)))))
;;     ((CONSP V18) (LIST 'cons (CAR V18) (cons_form (CDR V18))))
;;     (T (ERROR "cons_form")V18)))


;; リスト内包表記か否か
(DEFUN <is-list-comprehension?> (src)
  (LET (tmp
        par
        lazy?)
    (DO-UNIFY-WHEN
        (src (:AND (:OR (?main bar# bar# . ??r)
                        (:AND (?main bar#  . ??r)
                              (:DO (SETQ lazy? T)))
                        )
                   (:FOR (SPLIT-SEQUENCE 'bar# r)
                       (:EACH+ (:AND ??p
                                     (:HERE (NOT (NULL p)))
                                     (:FOR (SPLIT-SEQUENCE '|,| p)
                                         (:EACH+ (:AND (:OR ((:OR if IF
                                                                  when WHEN unless UNLESS
                                                                  while WHILE until UNTIL
                                                                  break BREAK count)
                                                             ?)
                                                            (do ? . ?)
                                                            (:AND ((:OR let let* provide provide*) . ??let-clauses)
                                                                  (:FOR let-clauses (:EACH+ ? ?)))
                                                            (:EACH+ ? <- ?)
                                                            (for ? ? ?)
                                                            )
                                                       ?term
                                                       (:DO (PUSH term tmp)))
                                                 ))
                                     (:DO (PUSH (NREVERSE tmp) par) (SETQ tmp NIL)))))))
      (IF (NULL (CDR par))
        ;; SINGLE LIST COMPREHENSION
        (LIST* NIL lazy? main (FIRST par))
        ;; PARALLEL LIST COMPREHENSION
        (LIST* T lazy? main  (NREVERSE par))
        ))))


    
(DEFUN <compile-single-list-comprehension> (parallel? lazy? parsed-clauses main-exp)
  (WHEN parallel?
    (SETQ main-exp (LIST 'VALUES T)))
  (LET* (par-vars
         (code
           (CONS (IF parallel? '<subparlist> (IF lazy? '<&genlist> '<genlist>))
                 (REDUCE (LAMBDA (c rest)
                           (ACASE (FIRST c)
                             ((if IF when WHEN unless UNLESS while WHILE until UNTIL break BREAK count for)
                              (CASE IT
                                ((count for) (PUSH (SECOND c) par-vars)))
                              (NCONC c rest))
                                        ;((infinite) (LIST 'LOOP rest))
                             ((let provide)                              
                              (NCONC (WHEN (EQ IT 'provide)
                                       (SETF (CAR c) 'let)
                                       (LIST 'loop))
                                     (LET* ((n (LIST-LENGTH (CDR c)))
                                            (m (/ (- n 2) 2)))
                                       (IF (ZEROP m)
                                         (PROG1 c
                                           (WHEN parallel? (PUSH (SECOND c) par-vars)))
                                         (DO ((c (CDR c) (CDDR c))
                                              vars
                                              pre-bindings
                                              post-bindings)
                                             ((NULL c)
                                              (WHEN parallel?
                                                (SETQ par-vars (NCONC (REVERSE vars) par-vars)))

                                              (REDUCE #'NCONC
                                                      (NCONC (NREVERSE pre-bindings)
                                                             (NREVERSE post-bindings))
                                                      :FROM-END T))
                                           (WHEN parallel? (PUSH (FIRST c) vars))
                                           (LET ((tmpvar (GENSYM "TMP")))
                                             (PUSH (LIST 'let tmpvar (SECOND c)) pre-bindings)
                                             (PUSH (LIST 'let (FIRST c) tmpvar) post-bindings)))))
                                     rest))
                             
                             ((let* provide*)
                              (NCONC (WHEN (EQ IT 'provide*) (LIST 'loop))
                                     (PROG1 c
                                       (SETF (CAR c) 'let)
                                       (LET* ((vars (WHEN parallel? (LIST (SECOND c))))
                                              (n (LIST-LENGTH (CDR c)))
                                              (m (/ (- n 2) 2)))
                                         (DO ((c (CDDR c) (CDDDR c))
                                              (i 0 (1+ i)))
                                             ((EQL m i)
                                              (WHEN parallel? (SETQ par-vars (NCONC (NREVERSE vars) par-vars)))
                                              )
                                           
                                           (WHEN parallel? (PUSH (SECOND c) vars))
                                           (LET ((esc (CDR c)))
                                             (SETF (CDR c) (CONS 'let esc))))))
                                     rest))
                             ((do) (NCONC (MAPCAN (LAMBDA (x) (LIST 'do x))
                                                  (CDR c))
                                          rest))
                             (T ;; V -> E ....
                               (IF (EQL 3 (LIST-LENGTH c))
                                 (PROG1 (NCONC c rest)
                                   (WHEN parallel? (PUSH (FIRST c) par-vars))
                                   (SETF (CDR c) (CDDR c)))
                                 (NCONC (DO ((c c (CDDDR c))
                                             vars
                                             list-exps)
                                            ((NULL c)
                                             (WHEN parallel?
                                               (SETQ par-vars (NCONC (REVERSE vars) par-vars)))
                                             (LIST (INTERSPERSE '|,| (NREVERSE vars))
                                                   (INTERSPERSE '|,| (NREVERSE list-exps))))
                                             ;(WHEN parallel? (PUSH (FIRST c) par-vars))
                                          (PUSH (FIRST c) vars)
                                          (PUSH (THIRD c) list-exps))
                                        rest)))))
                         parsed-clauses :FROM-END T :INITIAL-VALUE (LIST main-exp)))))

    (COND (parallel?
            (NCONC main-exp par-vars)
            (VALUES code par-vars))
          (T code))
    ))
    
(DEFUN <compile-list-comprehension-to-xi> (parsed)
  (DESTRUCTURING-BIND (parallel? lazy? main-exp &REST parsed-clauses) parsed
    (COND ;(parallel? (<compile-single-list-comprehension> T T (CAR parsed-clauses) NIL))
          (parallel?
            (LET (dolist-bindings
                  infos)
              (DOLIST (parsed-par-clauses parsed-clauses)
                (MULTIPLE-VALUE-BIND (code par-vars)
                    (<compile-single-list-comprehension> T T parsed-par-clauses NIL)
                  (PUSH (CONS par-vars code) infos)
                  ))
              (LIST* (IF lazy? '<&genparlist> '<genparlist>)
                     main-exp
                     (NREVERSE infos))))
                
          (T (<compile-single-list-comprehension> NIL lazy? parsed-clauses main-exp )))))

;;;;;;;;;;;;;;;;;;;;;;; IOTA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN <is-iota-form?> (x)
  (DO-UNIFY x
    (:OR (? |;| |;|)
         (? |;| |;| ?)
         (? |;| |;| |;| ?) ;; strict
         (? ? |;| |;|)
         (? ? |;| |;| ?)
         (? ? |;| |;| |;| ?) ;; strict
         )))

(DEFUN <chk/xi/iota> (opname &REST vars)
  `(UNLESS (AND (MAPCAR (LAMBDA (v) (LIST 'NUMBERP v))
                        vars))
     (ERROR "~A: operand is in not a number ~A" opname vars)))

(DEFUN <<<xi/+iota/strict>>> (cur step end)
  (IF (ZEROP step)
    (COND ((= end cur) (LIST cur))
          ((> end cur) (REPEAT cur)))
    (LET (result)
      (WHILE (>= end cur)
        (PUSH cur result)
        (SETQ cur (+ cur step)))
      (NREVERSE result))))
(DEFUN <<<xi/-iota/strict>>> (cur step end &AUX result)
  (WHILE (<= end cur)
    (PUSH cur result)
    (SETQ cur (+ cur step)))
  (NREVERSE result))
(DEFUN <<<xi/+iota/lazy>>> (cur step end)
  (WHEN (>= end cur)
    (LET ((next (+ cur step)))
      (&CONS! cur (<<<xi/+iota/lazy>>> next step end)))))
(DEFUN <<<xi/-iota/lazy>>> (cur step end)
  (WHEN (<= end cur)
    (LET ((next (+ cur step)))
      (&CONS! cur (<<<xi/-iota/lazy>>> next step end)))))

(DEFUN <<xi/iota>> (start step end strict?)
  (IF (>= step 0)
    (IF strict?
      (<<<xi/+iota/strict>>> start step end)
      (<<<xi/+iota/lazy>>> start step end))
    (IF strict?
      (<<<xi/-iota/strict>>> start step end)
      (<<<xi/-iota/lazy>>> start step end))))


(DEFUN <<<series>>> (x stepper predicate)
  (WHEN (wrapper (apply predicate x)) (&CONS! x (<<<series>>> (WITHOUT-CALL/CC (apply stepper x)) stepper predicate))))

(DEFUN <<<series/infinite>>> (x stepper)
  (&CONS! x (<<<series/infinite>>> (WITHOUT-CALL/CC (apply stepper x)) stepper)))

(DEFUN <<xi/series>> (start stepper predicate strict?)
  (COND (strict?
          (LET ((cur start)
                tmp)
            (WHILE (wrapper (apply predicate cur))
              (PUSH cur tmp)
              (SETQ cur (apply stepper cur)))
            (NREVERSE tmp)))
        (predicate
          (WHEN (wrapper (apply predicate start))
            (&CONS! start (<<<series>>> (WITHOUT-CALL/CC (apply stepper start)) stepper predicate))))
        (T ;; 終了条件なしの場合 strict?==NILは必ず満たされている
            (&CONS! start (<<<series/infinite>>> (WITHOUT-CALL/CC (apply stepper start)) stepper)))))
  
(DEFUN <iota/start> (start)
  (IF (NUMBERP start)
    (&IOTA -1 :START start)
    (ERROR "[~A ..] wrong argument" start start)))

(DEFUN <iota/start_next> (start next)
  (COND ((AND (NUMBERP start) (NUMBERP next))
          (&IOTA -1 :START start :STEP (- next start)))
        ((wrapper (function? next))
          (<<xi/series>> start next NIL NIL))
        (T (ERROR "[~A ~A ..] wrong arguments" start next))))

(DEFUN <iota/start_end> (start end strict?)
  (COND ((AND (NUMBERP end) (NUMBERP start))
          (<<xi/iota>> start 1 end strict?))
        ((AND (wrapper (function? end)) (NUMBERP start))
          (<<xi/series>> start #'1+ end strict?))
        (T (ERROR "[~A ~A ~A] wrong arguments" start (IF strict? "..." "..") end))))

(DEFUN <iota/start_next_end> (start next end strict?)
  (COND ((AND (NUMBERP end) (NUMBERP start) (NUMBERP next))
          (LET ((step (- next start)))
            (<<xi/iota>> start step end strict?)))
        ((AND (wrapper (function? end)) (NUMBERP start) (NUMBERP next))
          (LET* ((step (- next start))
                 (stepper (LAMBDA (num) (+ num step))))
            (<<xi/series>> start stepper end strict?)))
        ((AND (wrapper (function? next)) (wrapper (function? end)))
          (<<xi/series>> start next end strict?))
        (T (ERROR "[~A ~A ~A] wrong arguments" start (IF strict? "..." "..") end))))


;; これは実質ダミー定義である
(DEFUN <circulate> (&REST exps) (APPLY #'REPEAT exps))
;; 実際にはこちらのコンパイラマクロ版が呼ばれるだろう
(DEFINE-COMPILER-MACRO <circulate> (&REST exps)
  (CONS 'REPEAT exps))

;; Jun Hacked [2018-06-20] 
;; [A :: B] --> (&cons! A B)
;; 遅延リストへの対応

(DEFUN <cons_form> (xs)
  (COND ((NULL xs) NIL)
        ((AND (CONSP xs) (CONSP (CDR xs)) (EQ 'bar# (CADR xs)))
          (LET ((elem (CADDR xs)))
            (IF (EQ 'bar# elem)
                    (LIST 'cons (CAR xs) (CADDDR xs))
                    (LIST '&cons! (CAR xs) (CADDR xs)))))
        (T (LIST 'cons (CAR xs) (<cons_form> (CDR xs))))))

(DEFUN cons_form (xs)
  (ACOND ((EQUAL '(bar#) xs) '<&genlist>)       ;; [:]
         ((EQUAL '(bar# bar#) xs) '<genlist>)  ;; [::]
         ((DO-UNIFY xs (:APPEND (?) ? (|;| |;| |;| |;|)))
          (CONS '<circulate> (SUBSEQ xs 0 (- (LENGTH xs) 4))))
         ((<is-iota-form?> xs)
          (CASE (LIST-LENGTH xs)
            (3 (LIST '<iota/start> (FIRST xs)))
            (4 (IF (EQ '|;| (SECOND xs))
                 (LIST '<iota/start_end> (FIRST xs) (FOURTH xs) NIL)
                 (LIST '<iota/start_next> (FIRST xs) (SECOND xs))))
            (5 (IF (EQ '|;| (SECOND xs))
                 (LIST '<iota/start_end> (FIRST xs) (FIFTH xs) T)
                 (LIST '<iota/start_next_end> (FIRST xs) (SECOND xs) (FIFTH xs) NIL)))
            (6 (LIST '<iota/start_next_end> (FIRST xs) (SECOND xs) (SIXTH xs) T))
            )
           )
         ((<is-list-comprehension?> xs)
          (<compile-list-comprehension-to-xi> IT))
         (T
          (FLET ((check-tail (XS)
                   (IF (NULL XS)
                     T
                     (WHEN (CDR XS)
                       (IF (CDDR XS)
                         (AND (EQ 'bar# (CADR XS)) (NOT (EQ 'bar# (CADDR XS))) (NULL (CDDDR XS)))
                         (AND (NOT (EQ 'bar# (CADR XS))) (NULL (CDDR XS))))))))
            
            (UNLESS (check-tail (MEMBER 'bar# xs))
              (ERROR "misplaced `:' in [...] syntax: ~A " xs)))
          (<cons_form> xs))))
         
