;; -*- coding: utf-8 -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (oleo.core.reader:enable-reader))

(oleo.core.init:define-package :oleo.core.pre ()
  (:import/export :oleo.core.test)
  (:use :cl :split-sequence :alexandria :anaphora)
  (:export

   #:none #:only

   #:>> #:<< #:foldr-expression #:foldl-expression
   #:! #:with-read-slots #:with-flush-slots

   #:define-predicate
   #:predicate
   #:apply-predicate

   #:with- 

   #:unexport-and-escape

   #:modifyf
   #:while #:awhile #:swhile :until
   #:do-while :do-until
   #:for 
   #:memoized-if
   #:once

   #:second-value-aif #:second-value-awhen #:second-value-aunless
   #:second-value-awhile #:second-value-auntil
   #:second-value-acase #:second-value-aecase
   
   #:it
   
   #:value/1 #:value/2 #:value/3 #:value/4 #:value/5 #:value/6 #:value/7 #:value/8

   ;; Symbols for Parameter
   #:a #:b #:c #:f #:g #:h #:i #:j #:k #:p #:q #:r #:x #:y #:z
   #:\a #:\b #:\c #:\f #:\g #:\h #:\i #:\j #:\k #:\p #:\q #:\r #:\x #:\y #:\z
   #:\ab #:\abc #:\fg #:\fgh #:\pq #:\pqr #:\xy #:\xyz
   
   #:modify #:++ #:-- #:proc #:is
   #:set #:inc #:dec #:swap #:?
   #:div #:%

   
   ))


(in-package :oleo.core.pre)

;; すべて偽なら真となる。
(defmacro none (&rest exps)
  `(not (or ,@exps)))


#Testing NONE
(check-assert (none)
              (none nil)
              (none nil nil)
              #!(none t)
              #!(none nil t)
              #!(none t nil))

;; １つだけ真の場合のみ真となる。
(defmacro only (&rest exps)
  (case (length exps)
    (0 nil)
    (1 (first exps))
    (t #{let ((cnt (gensym)))
      `(let ((,cnt 0))
        (block nil
          ,@(mapcar #/`(when ,_ 
                        (unless (zerop ,cnt) (return nil))
                        (setq ,cnt (1+ ,cnt)))
                    exps)
          (not (zerop ,cnt)))))))

#Testing
(check-assert
  #!(only)
  #!(only nil)
  (only nil t)
  #!(only nil nil)
  (only nil t nil)
  #!(only t t)
  #!(only nil t t))

(defun <fold> (x f)
  (cond ((or (atom f) 
             (eq 'LAMBDA (car f)))
          (list f x))
        ((member (car f) '(QUOTE FUNCTION))
          `(funcall ,f ,x))
        (t `(,@f ,x))))

(defun foldr-expression (src)
  (reduce #'<fold> src))
(defun foldl-expression (src)
  (reduce (lambda (a b) (<fold> b a)) 
          src 
          :from-end t))

(defmacro >> (&rest args) (foldr-expression args))
(defmacro << (&rest args) (foldl-expression args))


#Testing
(check-unit (macroexpand-1 '_)
  ((<< a b c) '(a (b c)))
  ((<< a) 'a)
  ((<< a b) '(a b))
  ((<< 'a #'b c) '(funcall 'a (funcall #'b c)))
  ((>> a b c) '(c (b a)))
  ((>> a) 'a)
  ((>> a b) '(b a))
  ((>> 'a #'b c) '(c (funcall #'b 'a))))


;; （ディープな）スロット参照
(defmacro ! (exp slot &rest rest-slots)
  (reduce (lambda (e s) `(slot-value ,e ',s))
          rest-slots
          :initial-value `(slot-value ,exp ',slot)))

(defun <check-slots-clause> (macroname slots)
  (unless (every #/(or (symbolp _) (and (consp _) 
                                        (eq (length _) 2) 
                                        (symbolp (first _))))
                 slots)
    (error "~D: illegal clause ~D" macroname slots)))

(defmacro with-read-slots ((&rest slots) form &body body)
  (<check-slots-clause> 'with-read-slots slots)
  (with-gensyms (tmp)
    `(let* ((,tmp ,form)
            ,@(mapcar #/(if (symbolp _)
                          `(,_ (slot-value ,tmp ',_))
                          `(,(first _) (slot-value ,tmp ',(second _))))
                      slots))
      ,@body)))

;; 多値は返せない
(defmacro with-flush-slots ((&rest slots) form &body body)
  (<check-slots-clause> 'with-flush-slots slots)
  (with-gensyms (tmp)
    `(let* ((,tmp ,form)
            ,@(mapcar #/(if (symbolp _)
                          `(,_ (slot-value ,tmp ',_))
                          `(,(first _) (slot-value ,tmp ',(second _))))
                      slots))
      (prog1 (progn ,@body)
        (setf ,@(mapcan #/(if (symbolp _)
                            `((slot-value ,tmp ',_) ,_)
                            `((slot-value ,tmp ',(second _)) ,(first _)))
                  slots))))))
        

(defun <pred-code> (x var)
  (cond  ((or (atom x) 
              (eq 'LAMBDA (car x)))
           (list x var))
         (t (cons (car x)
                  (mapcar #/(<pred-code> _ var) (cdr x))))))

(defun <pred-main> (preds var)
  `(and ,@(mapcar #/(<pred-code> _ var)
                  preds)))

@todo
(defmacro predicate (&rest preds)
  (with-gensyms (var)
    `(lambda (,var) ,(<pred-main> preds var))))

(defmacro apply-predicate (form &body preds)
  (with-gensyms (var)
    `(let ((,var ,form))
      ,(<pred-main> preds var))))

;(enable-testing nil)
#Testing PREDICATE,APPLY-PREDICATE
(check-assert
  (funcall (predicate) nil)
  (funcall (predicate symbolp) 'foo)
  (funcall (predicate integerp oddp) 3)
  #!(funcall (predicate integerp evenp) 3)
  (apply-predicate nil)
  (apply-predicate 'foo symbolp)
  (apply-predicate 3 integerp oddp)
  #!(apply-predicate 3 integerp evenp))

(defmacro define-predicate (name &body preds)
  (with-gensyms (var)
    `(defun ,name (,var) ,(<pred-main> preds var))))


#Testing DEFINE-PREDICATE
(progn
  (define-predicate <testpred-0>)
  (define-predicate <testpred-1> integerp)
  (define-predicate <testpred-2> integerp oddp)
  (define-predicate <testpred-3> integerp evenp)
  (check-assert (<testpred-0> 3) (<testpred-1> 3) (<testpred-2> 3) #!(<testpred-3> 3)))



(defmacro with- ((&rest headers) &body body)
  (unless (and headers 
               (or (every #'listp headers)
                   (and (member '+  headers)
                        (setq headers (split-sequence '+ headers)))))
    (error "WITH: ~D" headers))
  #{let ((init `(,@(lastcar headers) ,@body)))
  (reduce (lambda (header form)
            `(,@header ,form))
          (nbutlast headers)
          :from-end t :initial-value init))

(defmacro memoized-if (form &optional (test 'identity))
  #{with-gensyms (mem val)
  `(let ((,mem (memoized (cons nil nil)))
         (,val ,form))
     (cond ((,test ,val)
             (setf (car ,mem) ,val
                   (cdr ,mem) t)
             (values ,val t))
           (t  (values (car ,mem) (cdr ,mem))))))

#Testing MOMOIZED-IF
(flet ((f (x update?) (memoized-if x (lambda (_) (declare (ignore _)) update?))))
  (check-unit (quote _)
    ((nil nil) (multiple-value-list (f 0 nil)))
    ((1 t) (multiple-value-list (f 1 t)))
    (1 (f 2 nil))
    (2 (f 2 t))
    ))
  

;;@doc #; `UNEXPORT-AND-ESCAPE'
;; 競合する関数もしくはマクロの排除と退避。返り値は、退避先のシンボル。
;; ALEXANDRIA:LENGTH=を渡した場合、ALEXANDRIAからLENGTH=をunexportする
;; そして、ALEXANDRIA_LENGTH=というシンボルを同パッケージにインターンし、
;; 新たにエクスポートする
;; ここで、新たなシンボルの先頭には、パッケージのフルネームとニックネームのうち、
;; 最も短い名前が付加される。
;; 例: ALEXANDRIAの場合、フルネームALEXANDRIA.0.DEVとニックネームALEXANDRIAであるから、
;; 短い方のALEXANDRIAが付加され、ALEXANDRIA:ALENXANDRIA_LENTH=となる
(defun unexport-and-escape (symbol)

  (let* ((pkg (symbol-package symbol))
         (pkgname (first (sort (cons (package-name pkg) 
                                     (package-nicknames pkg))
                               #'< :key #'length)))
         (new-escaped-symbol 
           (intern (string-concat pkgname "_" (symbol-name symbol)) pkg)))
    ;; 新しいシンボルへ内容を退避し、エクスポート
    (setf (symbol-function new-escaped-symbol) (symbol-function symbol))
    (export new-escaped-symbol pkg)
    ;; 元のパッケージからアンエクスポート
    (unexport symbol pkg)
    new-escaped-symbol))
  

(defmacro modifyf (place f &optional need-old-value-p)
  (multiple-value-bind (bindings src tmp store-form value-form)
      (get-setf-expansion place)
    (let ((core-form `(let ((,(car tmp) ,value-form))
                       (let ((,(car tmp) (,f ,(car tmp))))
                         ,store-form)
                       ,@(when need-old-value-p (list (car tmp))))))
      (if bindings
        `(let ,(mapcar 'list bindings src)
          ,core-form)
        core-form))))

#Testing MODIFYF
(let ((x 0))
  (check-unit _
    ((modifyf x 1+) 1)
    ((modifyf x 1+ t) 1)
    (x 2)
    ((modifyf x list) '(2))
    ((modifyf x (lambda (a) (cons 'foo a))) '(foo 2))))
  
  

(defmacro while (test &body body)
  `(do () ((not ,test)) ,@body))

#Testing While
(check-assert (equal 6 (let ((n 3)
                             (cnt 0))
                         (while (> n 0) (setq cnt (+ cnt n)
                                              n (1- n)))
                         cnt))

              (equal 'ok (while t (return 'ok))))


(defmacro awhile (test &body body)
  `(loop  (aif ,test (progn ,@body) (return))))


#Testing AWHILE
(check-assert (equal '(true ok) (awhile 'true (return (list it 'ok))))
              (equal 55 (let ((cnt 10)
                              (sum 0))
                          (awhile cnt
                            (setq sum (+ sum it)
                                  cnt (when (> cnt 0) (1- cnt))))
                          sum)))


(defmacro swhile (test &body body)
  `(symbol-macrolet ((it ,test))
     (do () ((not it)) ,@body)))


#Testing SWHILE
(check-assert (equal '(true ok) (swhile 'true (return (list it 'ok))))
              (eql 55 (let ((cnt 10) (sum 0))
                          (swhile cnt (setq sum (+ sum it)
                                            it (when (> it 0) (1- it))))
                          sum))
              )

(defmacro until (test &body body)
  `(do () (,test) ,@body))

#Testing UNTIL
(check-assert (eql 6 (let ((cnt 4) (sum 0)) (until (zerop (decf cnt)) (incf sum cnt)) sum)))

(defmacro do-while (test &body body)
  `(do () ((progn ,@body (not ,test)))))

#Testing DO-WHILE
(check-assert (eql 10 (let ((cnt 4) (sum 0)) (do-while (> (decf cnt) 0) (incf sum cnt)) sum)))

(defmacro do-until (test &body body)
  `(do () ((progn ,@body ,test))))

#Testing DO-UNTIL
(check-assert (eql 10 (let ((cnt 4) (sum 0)) (do-until (zerop (decf cnt)) (incf sum cnt)) sum)))

(defmacro for (def-clause (test-clause &rest return-body) &body body)
  `(do ,def-clause
    ((not ,test-clause) ,@return-body)
    ,@body))

#Testing FOR
(check-assert (eql 10 (for ((cnt 4) (sum 0))
                          ((> cnt 0) sum)
                        (incf sum cnt)
                        (decf cnt))))
          

;;@doc #; `ONCE'
;; bodyを１度だけ評価し、メモ化する
(defmacro once (&body body)
  `(memoized (progn ,@body)))

#Verify ONCE
(flet ((test/once (x) (once x)))
  (dotimes (i 10 T) (unless (zerop (test/once i)) (return))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; VALUE/1 ... VALUE/8 多値からの特定値の取り出しマクロ
;; (value/i [exp]...)
(eval-when (:compile-toplevel)
  (defmacro <def-value/i> (macroname i)
    (let* ((preserved '(a b c d e f g h))
           (n (length preserved))
           (vars (nthcdr (- n i) preserved)))
      `(defmacro ,macroname (&body body)
         `(multiple-value-bind ,',vars (progn ,@body)
            (declare (ignore ,@',(butlast vars)))
            ,',(car (last vars)))))))
          
(<def-value/i> value/1 1) (<def-value/i> value/2 2) (<def-value/i> value/3 3) (<def-value/i> value/4 4)
(<def-value/i> value/5 5) (<def-value/i> value/6 6) (<def-value/i> value/7 7) (<def-value/i> value/8 8)

#Verify VALUE/1 ... VALUE/8
(flet ((f () (values 0 1 2 3 4 5 6 7)))
  (and (eql 0 (value/1 (f))) (eql 1 (value/2 (f))) (eql 2 (value/3 (f))) (eql 3 (value/4 (f)))
       (eql 4 (value/5 (f))) (eql 5 (value/6 (f))) (eql 6 (value/7 (f))) (eql 7 (value/8 (f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 多値の第２値の真偽による分岐
(defmacro second-value-aif (test-form then-form &optional else-form)
  (let ((var1 (gensym))
        (var2 (gensym)))
    `(multiple-value-bind (,var1 ,var2) ,test-form
       (if ,var2
         (let ((it ,var1))
           ,then-form)
         ,else-form))))

#Testing SECOND-VALUE-AIF
(check-assert (eq 'A (second-value-aif (values 'A T) it))
              (equal '(nil) (second-value-aif (values NIL T) (list it)))
              (eq 'B (second-value-aif 'A it 'B)))

(defmacro second-value-awhen (test-form &body body)
  `(second-value-aif ,test-form (progn ,@body)))

#Testing SECOND-VALUE-AWHEN
(check-assert (equal '(A) (second-value-awhen (values 'A T) (setq it (list it it)) (cdr it)))
              (equal '(nil) (second-value-awhen (values NIL T) (setq it (list it it)) (cdr it)))
              (eq NIL (second-value-awhen 'A (setq it (list it it)) (cdr it))))

(defmacro second-value-aunless (test-form &body body)
  (let ((tmp (gensym)))
    `(multiple-value-bind (it ,tmp) ,test-form
       (unless ,tmp
         ,@body))))

#Testing SECOND-VALUE-AUNLESS
(check-assert (equal '(A) (second-value-aunless (values 'A NIL) (setq it (list it it)) (cdr it)))
              (equal NIL (second-value-aunless (values 'A T) (setq it (list it it)) (cdr it)))
              (eq 'A (second-value-aunless 'A it)))


(defmacro second-value-awhile (test-form &body body)
  `(loop (second-value-aif ,test-form
                             (progn ,@body)
                             (return))))

#Testing SECOND-VALUE-AWHILE
(check-assert (equal NIL (second-value-awhile 'true (return (list it 'ok))))
              (equal '(true ok) (second-value-awhile (values 'true T) (return (list it 'ok))))
              (equal '(nil ok) (second-value-awhile (values nil T) (return (list it 'ok))))
              (equal 55 (let ((cnt 10)
                              (sum 0))
                          (second-value-awhile (values cnt (> cnt 0))
                            (setq sum (+ sum it)
                                  cnt (1- cnt)))
                          sum)))

(defmacro second-value-auntil (test-form &body body)
  (let ((tmp (gensym)))
    `(loop
        (multiple-value-bind (it ,tmp) ,test-form
          (if ,tmp
            (return)
            (progn ,@body))))))

#Testing SECOND-VALUE-AUNTIL
(check-assert (equal '(true ok) (second-value-auntil 'true (return (list it 'ok))))
              (equal NIL (second-value-auntil (values 'true T) (return (list it 'ok))))
              (equal '(nil ok) (second-value-auntil (values nil nil) (return (list it 'ok))))
              (equal 45 (let ((cnt 10)
                              (sum 0))
                          (second-value-auntil (values cnt (< cnt 5))
                            (setq sum (+ sum it)
                                  cnt (1- cnt)))
                          sum)))


(defun <second-value-case> (case-op value-key-form clauses)
  (let ((key (gensym)))
    `(multiple-value-bind (it ,key) ,value-key-form
       (,case-op ,key ,@clauses))))

(defmacro second-value-acase (value-key-form &body clauses)
  (<second-value-case> 'CASE value-key-form clauses))

(defmacro second-value-aecase (value-key-form &body clauses)
  (<second-value-case> 'ECASE value-key-form clauses))

#Testing SECOND-VALUE-ACASE SECOND-VALUE-AECASE
(check-unit _
  ('a (second-value-acase (values 'a 'foo) (bar 1) (foo it)))
  ('(1 a) (second-value-aecase (values 'a 'bar) (bar (list 1 it)) (foo it)))
  (T (has-errors (second-value-aecase nil (foo) (bar)))))



;;; ANNOTATIONS

(define-annotation assert (tag test-form)
  `(progn (assert (and ,@(when tag `(',tag))
                       ,test-form)) T))
#Testing ASSERT
(check-assert @has-no-errors @annot (assert tag t)
              @has-errors @annot (assert tag nil)
              @has-no-errors @annot (assert nil t)
              @has-errors @annot (assert nil nil))


(defun <define> (op name lambda-list exp)
  (unless (listp lambda-list)
    (setq lambda-list (list lambda-list)))
  `(,op ,name ,lambda-list ,exp))

(define-annotation defun (name lambda-list exp) (<define> 'DEFUN name lambda-list exp))
(define-annotation defmacro (name lambda-list exp) (<define> 'DEFMACRO name lambda-list exp))

#Verify @DEFUN
(and (eq '|<testfunc/foo1>| @annot (defun |<testfunc/foo1>| (x) (1+ x)))
     (eq '|<testfunc/foo2>| @annot (defun |<testfunc/foo2>| x (1- x)))
     (eq 2 (funcall '|<testfunc/foo1>| 1))
     (eq 0 (funcall '|<testfunc/foo2>| 1)))
#Verify @DEFMACRO
(and (eq '|<testmacro/foo1>| @annot (defmacro |<testmacro/foo1>| (x) (list 'foo x)))
     (eq '|<testmacro/foo2>| @annot (defmacro |<testmacro/foo2>| x (list 'bar x)))
     (equal '(foo baz) (macroexpand-1 '(|<testmacro/foo1>| baz)))
     (equal '(bar boz) (macroexpand-1 '(|<testmacro/foo2>| boz))))
#Testing // Delete Test Functions
(mapc #'fmakunbound '(|<testfunc/foo1>| |<testfunc/foo2>| |<testmacro/foo1>| |<testmacro/foo2>|))

(define-annotation f (lambda-list exp) 
  (unless (listp lambda-list)
    (error "@f invalid lambda-list ~D" lambda-list))
  (list 'lambda lambda-list exp))


(define-annotation \a (exp) `(lambda (a) ,exp))
(define-annotation \b (exp) `(lambda (b) ,exp))
(define-annotation \c (exp) `(lambda (c) ,exp))
(define-annotation \ab (exp) `(lambda (a b) ,exp))
(define-annotation \abc (exp) `(lambda (a b c) ,exp))
(define-annotation \f (exp) `(lambda (f) ,exp))
(define-annotation \g (exp) `(lambda (g) ,exp))
(define-annotation \h (exp) `(lambda (h) ,exp))
(define-annotation \fg (exp) `(lambda (f g) ,exp))
(define-annotation \fgh (exp) `(lambda (f g h) ,exp))
(define-annotation \i (exp) `(lambda (i) ,exp))
(define-annotation \j (exp) `(lambda (j) ,exp))
(define-annotation \k (exp) `(lambda (k) ,exp))
(define-annotation \ij (exp) `(lambda (i j) ,exp))
(define-annotation \ijk (exp) `(lambda (i j k) ,exp))
(define-annotation \p (exp) `(lambda (p) ,exp))
(define-annotation \q (exp) `(lambda (q) ,exp))
(define-annotation \r (exp) `(lambda (r) ,exp))
(define-annotation \pq (exp) `(lambda (p q) ,exp))
(define-annotation \pqr (exp) `(lambda (p q r) ,exp))
(define-annotation \x (exp) `(lambda (x) ,exp))
(define-annotation \y (exp) `(lambda (y) ,exp))
(define-annotation \z (exp) `(lambda (z) ,exp))
(define-annotation \xy (exp) `(lambda (x y) ,exp))
(define-annotation \xyz (exp) `(lambda (x y z) ,exp))

#Testing
(check-unit* (:test nil :predicate (lambda (x) (equal x '(|chk1|)))) (funcall _ '|chk1|)
  (@annot (f (v) (list v)))
  (@annot (|a| (list a))) (@annot (|b| (list b))) (@annot (|c| (list c)))
  (@annot (|f| (list f))) (@annot (|g| (list g))) (@annot (|h| (list h)))
  (@annot (|i| (list i))) (@annot (|j| (list j))) (@annot (|k| (list k)))
  (@annot (|p| (list p))) (@annot (|q| (list q))) (@annot (|r| (list r)))
  (@annot (|x| (list x))) (@annot (|y| (list y))) (@annot (|z| (list z))))


#Testing
(check-unit* (:test nil :predicate (lambda (x) (equal x '(|chk1| |chk2|)))) (funcall _ '|chk1| '|chk2|)
  (@annot (f (v w) (list v w)))
  (@annot (|aB| (list a b))) (@annot (|fG| (list f g)))
  (@annot (|iJ| (list i j))) (@annot (|pQ| (list p q))) (@annot (|xY| (list x y))))

#Testing
(check-unit* (:test nil :predicate (lambda (x) (equal x '(|chk1| |chk2| |chk3|)))) (funcall _ '|chk1| '|chk2| '|chk3|)
  (@annot (f (v w x) (list v w x)))
  (@annot (|aBC| (list a b c))) (@annot (|fGH| (list f g h))) (@annot (|iJK| (list i j k))) (@annot (|pQR| (list p q r))) (@annot (|xYZ| (list x y z))))


(define-annotation dolist (exp body) `(dolist (_ ,exp) ,body))
#Verify @DOLIST
(let (tmp) @annot (dolist '(1 2 3) (push _ tmp))
     (equal tmp '(3 2 1)))

(define-annotation dotimes (exp body) `(dotimes (_ ,exp) ,body))
#Verify @DOTIMES
(let ((cnt 0)) @annot (dotimes 10 (incf cnt (1+ _))) (eq 55 cnt))

(define-binary-annotation push)

(define-annotation ++ (place) (list 'modifyf place '1+ t))
(define-annotation -- (place) (list 'modifyf place '1- t))
#Verify @++ and @--
(let ((x 0)) (and (eq 0 @annot (++ x)) (eq 1 x)
                  (eq 1 @annot (-- x)) (eq 0 x)))

(define-annotation _ (exp body)  `(let ((_ ,exp)) ,body))
(define-annotation let (var exp body)  `(let ((,var ,exp)) ,body))
#Verify @_ and @LET
(and (eq 1 @annot (_ 1 _)) (eq 2 @annot (let v 2 v)))

(define-annotation proc (body) `(lambda () ,body))
(define-annotation is (var exp) `(let ((,var ,exp))))
(define-binary-annotation cons)
(define-binary-annotation modify modifyf)
(define-binary-annotation set setf)
(define-binary-annotation inc incf)
(define-binary-annotation dec decf)
(define-binary-annotation swap rotatef)

(define-annotation ? (test then else) (list 'if test then else))
#Verify @?
(and (eq 1 @annot (? t 1 2)) (eq 0 @annot (? nil 1 0)))

(define-binary-annotation when)
(define-binary-annotation unless)
(define-binary-annotation and)
(define-binary-annotation or)

(define-annotation bind (clause form result-form) `(bind ((,clause ,form)) ,result-form))
#Verify @BIND
@annot (bind ((a b) . c) '((1 2) . 3) (and (eq a 1) (eq b 2) (eq c 3)))

(define-binary-annotation =)
(define-binary-annotation /=)
(define-binary-annotation >)
(define-binary-annotation <)
(define-binary-annotation >=)
(define-binary-annotation <=)
(define-binary-annotation +)
(define-binary-annotation -)
(define-binary-annotation *)
(define-binary-annotation /)
(define-binary-annotation % mod)
(define-binary-annotation eq)
(define-binary-annotation eql)
(define-binary-annotation equal)
(define-binary-annotation equalp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END OF J-INIT.LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                        ;@export


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

comment...

(in-package :oleo.base.ext)
(string-join '())


;; ALIST-BIND ALIST-EBIND

(let ((a '((a . b))))
  (equal (car a) (car (copy-alist a))))
 
(defun <alist-ebind-error> (sym alist)
  (error "ALIST-EBIND: could not find symbol ~A: alist=~A" sym alist))

(declaim (inline <alist-update>))
(defun <alist-update> (key val alist)
  (aif (assoc key alist)
       (progn (setf (cdr it) val) alist)
       (push (cons key val) alist)))

;(reduce #'list '(1 2 3) :from-end t :initial-value :init) 
  
(defun <alist-bind> (error? rebuild? update? vars alist-form body)
  (let* ((alist (gensym))
         (need-modify (and (not rebuild?) update?))
         (update-form (when update?
                        (reduce (lambda (key alist) `(<alist-update> ',key ,key ,alist))
                                vars
                                :initial-value alist :from-end t))))
    `(let ((,alist ,alist-form))
       (let ,(mapcar (lambda (v) `(,v ,(if error?
                                         `(aif (assoc ',v ,alist)
                                               (cdr it)
                                               (<alist-ebind-error> ',v ,alist))
                                         `(cdr (assoc ',v ,alist)))))
                     vars)
         (,(if need-modify 'UNWIND-PROTECT 'PROGN)
           (progn
             ,@body
             ,@(when rebuild? `((setq ,alist (copy-alist ,alist))))
             ,@(when (and rebuild? update?)
                 (list update-form))
             ;,@(when rebuild? (list alist))
             )
           ,@(when need-modify (list update-form))
           )))))


(defmacro alist-bind (vars alist-form &body body)
  (<alist-bind> nil nil nil vars alist-form body))
(defmacro alist-ebind (vars alist-form &body body)
  (<alist-bind> T nil nil vars alist-form body))

(defmacro alist-rebuild-bind (vars alist-form &body body)
  (<alist-bind> nil t t vars alist-form body))
(defmacro alist-rebuild-ebind (vars alist-form &body body)
  (<alist-bind> t t t vars alist-form body))

(defmacro alist-update-ebind (vars alist-form &body body)
  (<alist-bind> t nil t vars alist-form body))

(alist-rebuild-bind (a b) '((a . 0) (b . 1)) (setq a 'aaa))
(let ((alist '((a . 0) (b . 1))))
  (alist-update-ebind (a b) alist (setq a 'aaa))
  alist)
(alist-rebuild-bind (a b) nil (setq a 1 b 100))

(defun <make-alist-let> (* bindings body)
  `(,(if * 'LET* 'LET)
     ,bindings
     ,@body
     (list ,@(mapcar (lambda (x &aux (v (if (consp x) (car x) x)))
                       `(cons ',v ,v))
                     bindings))))

(defmacro make-alist-let (bindings &body body)
  (<make-alist-let> nil bindings body))
(defmacro make-alist-let* (bindings &body body)
  (<make-alist-let> T bindings body))

(make-alist-let ((a 0) b (c 1)) (setq b (vector a c)))

(alist-bind (a b) '((a . 0) (b . 1)) (setq a 'aaa))

(alist-bind (a b) '((a . 0) (c . 1)) (list a b))
(get 'a '(a b) :none)

(defun foobar (x) (unwind-protect (list x)))

(disassemble 'foobar)
;; SEQUAL -- Simple SEQuence EQUAL



(

(vectorp #(1))

(sequal '(3 #(2 _) _) '(3 #(2 3) 1))
(sequal 3 3)

(sequal '(3 #(?a ?a) ?x . ?x) '(3 #(3 3) (12) 12))



(equalp #*01 #(0 1))
(bit-vector-p #*01)

'
(equalize '(3 #(2 _) _) '(3 #(2 3) 1))
(eql 0.0 0.0)
'(a b _ ?x) 

(\ #x233b 50.0)
(\ 600000 180 365)
(defmacro destructuring-setq (bind-form value-form))

(destructuring-setq (a b) 
    
(in-package :alexandria)
(destructuring-case '(b foo)
  ((a b) (list b))
  ((b x) (list 'x x)))

(alist-hash-table '((1 . 2) (3 . 4)))

(in-package :kmrcl)

  


hello

