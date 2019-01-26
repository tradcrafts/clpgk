;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.core:clpgk-core-header)

(clpgk.core:define-package :clpgk.base.match* (:clpgk.base.match)
  (:use :cl)
  (:import/export :clpgk.base.unify*)
  (:export

     #:define-match-slots      #:register-match-slots
     #:define-match-accessors  #:register-match-accessors

     #:define-match-macro

     #:do-match
     #:do-match-if #:do-match-when #:do-match-unless
     #:do-match-gif #:do-match-gwhen #:do-match-gunless
     
     ;; 以下のオペレータは J-MORE-UNIFY-MATCH-DEFS.LISPで定義される
     #:m-case #:m-acase       #:m-gcase #:m-gacase
     #:m-case/w #:m-acase/w   #:m-gcase/w #:m-gacase/w
     
     #:m-cond #:m-gcond

     #:m-bind #:m-bind*

     ;; global symbols
     #:->
     #:proper-list* #:circular-list* 


     ))

(in-package :clpgk.base.match)

(defun <register-readers> ()
  @select-reader :match
  (set-dispatch-macro-character 
   #\# #\M
   (lambda (stream char1 char2 &aux (tmp (gensym)))
     @ignore (char1 char2)
     `(lambda (,tmp) (do-match ,tmp ,(read stream t nil t))))))

(register-reader-registerer '|match-1| '<register-readers>)


;(defmacro ab (body) `(lambda (a b) ,body))
  
;; j-unify-1.lisp内で参照される関数の実体を定義
(defun clpgk.base.unify::%make-match-code% (src ptns)
  `(do-match ,src ,(if (eq 1 (length ptns))
                       (first ptns)
                       `(:and ,@ptns))))


(defun register-match-slots (class-name &rest slots)
  (setf (get class-name '|match|) (cons 'slot slots)))
(defun register-match-accessors (class-name &rest accessor-names)
  (setf (get class-name '|match|) (cons 'accessor accessor-names)))

(defmacro define-match-slots (class-name &body slots)
  `(register-match-slots ',class-name ,@(mapcar #/`',_ slots)))

(defmacro define-match-accessors (class-name &body accessor-names)
  `(register-match-accessors ',class-name ,@(mapcar #/`',_ accessor-names)))

     
;(define-match-accessors CONS car cdr)
;(define-match-slots POINT x y)

@inline
(defun <constant?> (x)
  (or (numberp x) (keywordp x) (stringp x) (characterp x)
      (null x) (eq t x)))

@inline
(defun <variable?> (x)
  (and (symbolp x)
       (not (or (member x '(nil t _))
                (keywordp x)))))

(defun <special-match-prefix?> (kwd)
  (member kwd 
          '(:verify :verify-not
            :type-attr :data-attr :attr
            :set :multiset)))

(defun <reserved-match-kwd?> (kwd)
  (or (member kwd
              '(:and :or :if :do :here :with :unify :match :comp :comp-not
                :given :ungiven
                :always :never))
      (<special-match-prefix?> kwd)))
  

(defvar *match-macro-table*)

(defun <get-macro-symbol> (sym)
  (aif (get sym '|match-macro|)
       it
       #{let ((new (gensym (string-concat "MATCH-MACRO-" (symbol-name sym) ))))
       (setf (get sym '|match-macro|) new)))

(defmacro define-match-macro (name (&rest lambda-list) &body body)
  (unless (symbolp name)
    (error "DEFINE-MATCH-MACRO: ~D is not a symbol" name))
  (when (<reserved-match-kwd?> name)
    (error "DEFINE-MATCH-MACRO: ~D is a reserved keyword" name))

  #{let ((sym (<get-macro-symbol> name)))
  (when (fboundp sym) 
    (warn "DEFINE-MATCH-MACRO: redefining ~D" name))

  `(progn 
    (defun ,sym ,lambda-list ,@body)
    ',name))

(defun <expand-match-macro> (repl)
  (warn "~D" repl)
  (unless *match-macro-table*
    (setq *match-macro-table* (make-hash-table)))
  #{multiple-value-bind (expanded exists) (gethash repl *match-macro-table*)
  (if exists
    expanded
    #{let* ((name (first repl))
            (sym (<get-macro-symbol> name)))
    (unless (fboundp sym)
      (error "MATCH-MACRO: undefined macro :~D" name))
    (unless (proper-list-p repl)
      (error "MATCH-MACRO: invalid macro argument: ~D" repl))
    (setf (gethash repl *match-macro-table*) 
            (apply sym (cdr repl)))))

(defun <check> (ptn &key enumerated)
  (do-unify ptn
    (:OR (:AND (:HERE enumerated)
               (:EACH (:CALL term)))
         (:AND (:HERE #!enumerated)
               (:CALL term)))
    :let (vars) :import (vars)
    :define 
    ((term     
      (:OR (:CALL object)
           (:APPEND (:CALL header) :MAXIMIZE 
                    (:AND (:-> :LENGTH>= 2)
                          (:OR nil (-> (:CALL object))))
                    (:OR (:ACCUM (:CALL term))
                         (:ACCUM #((:CALL slot) (:CALL term)))))
           (:CALL non-object)))
     
     (typename (:HERE (and #>symbolp  #!#>keywordp)))
     (prefix (:HERE (and #>keywordp #><special-match-prefix?>)))
     (var (:AND (:HERE #><variable?>)
                (:-> :PUSHNEW vars)))
     (non-var (:OR (:CALL constant)
                   _ ;; wild card
                   '?                           
                   (:HERE (and (<variable?> _) (member _ vars)))
                   (:CALL function)
                   (:CALL evaluation)))
     
     (object (:OR (:CALL var)
                  (:CALL non-var)))
     (non-object 
      (:OR (:LIST* :and (:ACCUM (:CALL term)))
           (:LIST* :or (:ACCUM (:CALL term)))
           (:LIST* (:-> :eq :here :do :unify :match)
                   (:-> :type proper-list))
           (:LIST* :with 
                   (:here (and #>consp #>proper-list-p)) 
                   (:accum (:call term)))
           (:LIST* :-> (:-> :type keyword) (:-> :type proper-list))
           (:LIST* :-> (:-> :type symbol) :whole (:-> :type proper-list))
           (:LIST* :if
                   (:AND (:-> :length<=<= 2 3)
                         (:ACCUM (:CALL term))))
           ((:-> :EQ :always :never))
           ((:-> :EQ :given :ungiven) (:CALL var))
           ((:-> :EQ :not :comp :comp-not) ?)
           (:LIST* :macro (:TO #'<expand-match-macro> (:CALL term)))
           ;; マクロ(とみなされる)の場合
           (:AND (:LIST* (:AND (:-> :type keyword) 
                               (:HERE #!#><reserved-match-kwd?>)) 
                         ?)
                 (:TO #'<expand-match-macro> (:CALL term)))))

     (header (:AND (:-> :LENGTH>= 2)
                   (:OR (:PREFIX (:CALL prefix) ((:CALL typename)))
                        ((:LIST* -> (:PREFIX (:CALL prefix)
                                             (:LIST* (:CALL typename)
                                                     (:OR nil
                                                          ((:CALL test))
                                                          ((:CALL test) (:CALL trans) (:CALL term))))))))))
     
     (test (:OR nil (:CALL function)))
     (trans (:CALL function))
     (function (:OR (LAMBDA . ?) 
                    #'?))
     (constant (:-> #'<constant?>))
     (evaluation (#`:HERE . ?))
     (slot (:OR (:CALL constant)
                (:CALL evaluation)
                {symbol}
                '?))
     
     )
    :on-failure (error "MATCH: wrong pattern ~D" ptn)
    :success vars
    ))



(defvar *cur-vars*)
(defvar *match-in-verify* nil)
(defvar *match-cur-type*)

(defun <error> (format-string &rest args) 
  (error "MATCH ERROR in ~D: ~D" 
         *match-cur-type* 
         (apply #'format nil format-string args)))


(defun make-general-matcher (type elems src)
  #{let* ((prop (get type '|match|))
          (prop-len (if prop 
                      (1- (length prop))
                      0))
          (not-strict? (find-if #'simple-vector-p elems)))

  (when (zerop prop-len)
    (<error> "この型にはプロパティが存在しない"))

  #{let ((prop-type (first prop))
         (prop-body (rest prop)))
 
  #{with-gensyms (tmp)
  `(progn
    ,@(cond (not-strict?
             #{with-collect (c)
             (dolist* (#(f dst) elems)
               (unless (member f prop-body)
                 (<error> "unknown property ~D" type f))
               #{let ((value-form (if (eq 'accessor prop-type)
                                    `(,f ,src)
                                    `(slot-value ,src ',f))))
               (c `(let ((,tmp ,value-form))
                    ,(funcall 'make-matcher dst tmp)))))
            (t 
             (unless (eq (length elems) prop-len)
               (<error> "引数の不整合"))
             (mapcar @\ab (let ((form (if (eq 'accessor prop-type)
                                       `(,a ,src)
                                       `(slot-value ,src ',a))))
                           `(let ((,tmp ,form))
                             ,(funcall 'make-matcher b tmp)))
                     prop-body
                     elems)))))

(defun <serialize-error> ()
  (<error> "列挙には対応していない"))
       


(defun make-hash-table-matcher (elems src)
  #{with-gensyms (tmp chk)
  (if (find-if #'simple-vector-p elems)
    `(progn
      ,@(mapcar #/`(multiple-value-bind (,tmp ,chk) (gethash ,(svref _ 0) ,src) 
                    (if ,chk 
                      ,(funcall 'make-matcher (svref _ 1) tmp)
                      (return-from |test| nil)))                  
                elems))
    (<serialize-error>)))


(defun make-hash-table-as-set-matcher (elems src)
  #{with-gensyms (tmp)
  (if (find-if #'simple-vector-p elems)
    `(progn
      ,@(mapcar #/`(let ((,tmp #!(eq 
                                  '|not-exist|
                                  (gethash ,(svref _ 0) ,src '|not-exist|))))
                    ,(funcall 'make-matcher (svref _ 1) tmp))
                elems))
    (<serialize-error>)))

(defun make-alist-matcher (elems src)
  #{with-gensyms (tmp)
  (if (find-if #'simple-vector-p elems)
    `(progn
      ,@(mapcar #/`(let ((,tmp (assoc ,(svref _ 0) ,src)))
                    (if ,tmp
                      (let ((,tmp (cdr ,tmp)))
                        ,(funcall 'make-matcher (svref _ 1) tmp))
                      (return-from |test| nil))) 
                elems))
    (<serialize-error>)))


(defun make-sequence-as-set-matcher (elems src &key for-list)
  #{with-gensyms (tmp)
  (if (find-if #'simple-vector-p elems)
    `(progn
      ,@(unless for-list `((when (and (listp ,src)
                                      #!(proper-list-p ,src))
                             (return-from |test| nil))))
      ,@(mapcar #/`(let ((,tmp (when (,(if for-list 'member 'position)
                                       ,(svref _ 0) 
                                       ,src) 
                                 t)))
                    ,(funcall 'make-matcher (svref _ 1) tmp))
                elems))
    (<serialize-error>)))

(defun make-sequence-as-multiset-matcher (elems src &key for-list)
  #{with-gensyms (tmp)
  (if (find-if #'simple-vector-p elems)
    `(progn
      ,@(unless for-list `((when (and (listp ,src)
                                      #!(proper-list-p ,src))
                             (return-from |test| nil))))
      ,@(mapcar #/`(let ((,tmp (count ,(svref _ 0) ,src)))
                    ,(funcall 'make-matcher (svref _ 1) tmp))
                elems))
    (<serialize-error>)))

(defun make-list-matcher (elems src)
  #{with-gensyms (cur tmp)
  (if (find-if #'simple-vector-p elems)
    (let ((required-length (1+ (<check-index> elems))))
      `(cond 
        ((sublist-is-proper-p ,src ,required-length)
         ,@(mapcar #/`(let ((,tmp (nth ,(svref _ 0) ,src )))
                       ,(funcall 'make-matcher (svref _ 1) tmp))
                   elems))
        (t (return-from |test| nil))))
      `(cond 
      ((and (proper-list-p ,src)
            (length= ,(length elems) ,src))
       (let ((,cur ,src))
         ,@ (nbutlast (with-collect (c)
                        (dolist (e elems)
                          (c `(let ((,tmp (car ,cur)))
                               ,(funcall 'make-matcher e tmp)))
                          (c `(setq ,cur (cdr ,cur))))))))
      (t (return-from |test| nil)))))

(defun make-list*-matcher (elems src)
  (when (find-if #'simple-vector-p elems)
    (error "MATCH: LIST"))
  #{with-gensyms (cur tmp)
  `(cond 
    ((sublist-is-proper-p ,src ,(1- (length elems)))
     (let ((,cur ,src))
       ,@ (with-collect (c)
            (do ((e elems (cdr e))
                 (last (last elems)))
                ((null e))
              (if (eq last e)
                (c (funcall 'make-matcher (car e) cur))
                (c `(let ((,tmp (car ,cur)))
                     ,(funcall 'make-matcher (car e) tmp)
                     (setq ,cur (cdr ,cur)))))))))
    (t (return-from |test| nil))))
       

;; 非負の整数がインデックスに使われているかを調べ、その最大値を返す
(defun <check-index> (xs)
  (unless (every #/(non-negative-integer-p (svref _ 0)) 
                 xs)
    (error "illegal index: ~D" xs))
  (maximum xs :key #/(svref _ 0)))


(defun <make-v-matcher> (elems src predicate aref-op)
  #{with-gensyms (tmp)
  (if (find-if #'simple-vector-p elems)
    (let ((required-length (1+ (<check-index> elems))))
      `(cond 
        ((and (,predicate ,src)
              (<= ,required-length (length ,src)))
         ,@(mapcar #/`(let ((,tmp (,aref-op ,src ,(svref _ 0))))
                       ,(funcall 'make-matcher (svref _ 1) tmp))
                   elems))
        (t (return-from |test| nil))))
    `(cond 
      ((and (,predicate ,src)
            (= ,(length elems) (length ,src)))
       ,@ (with-collect (c)
            (do ((e elems (cdr e))
                 (i 0 (1+ i)))
                ((null e))
              (c `(let ((,tmp (,aref-op ,src ,i)))
                   ,(funcall 'make-matcher (car e) tmp))))))
      (t (return-from |test| nil)))))
       

(defun make-vector-matcher (elems src)
  (<make-v-matcher> elems src 'vectorp 'aref))
(defun make-simple-vector-matcher (elems src)
  (<make-v-matcher> elems src 'simple-vector-p 'svref))


(defun make-and-matcher (elems src)
  (when (find-if #'simple-vector-p elems)
    (<verify-error> elems))
  `(progn ,@(mapcar #/(funcall 'make-matcher _ src)
                    elems)))

(defun <verify-error> (elems)
  (<error> "間違ったVERIFY構文: ~D" elems))

(defun make-verify-matcher (elems src)
  (when (find-if #'simple-vector-p elems)
    (<verify-error> elems))
  (let ((*match-in-verify* *match-cur-type*))
    `(or ,@(mapcar #/`(block |test|
                       ,(funcall 'make-matcher _ src)
                       t)                            
                   elems)
      (return-from |test| nil))))

(defun make-list-verify-matcher (elems src)
  (when (find-if #'simple-vector-p elems)
    (<verify-error> elems))
  #{with-gensyms (tmp)
  (let ((*match-in-verify* *match-cur-type*))
    `(dolist (,tmp ,src)
      (or ,@(mapcar #/`(block |test|
                        ,(funcall 'make-matcher _ tmp)
                        t)                            
                    elems)
          (return-from |test| nil)))))

(defun make-vector-verify-matcher (elems src)
  (when (find-if #'simple-vector-p elems)
    (<verify-error> elems))
  #{with-gensyms (tmp i n)
  (let ((*match-in-verify* *match-cur-type*))
    `(do ((,i 0 (1+ ,i))
          (,n (length ,src)))
      ((= ,i ,n))
      (let ((,tmp (aref ,src ,i)))
        (or ,@(mapcar #/`(block |test|
                          ,(funcall 'make-matcher _ tmp)
                          t)                            
                      elems)
            (return-from |test| nil))))))

(defun make-sequence-verify-matcher (elems src)
  `(cond 
    ((proper-list-p ,src)
     ,(make-list-verify-matcher elems src))
    ((vectorp ,src)
     ,(make-vector-verify-matcher elems src))
    (t (return-from |test| nil))))

(defun make-attribute-matcher (attr-op elems src)
  #{with-gensyms (tmp chk)
  (if (find-if #'simple-vector-p elems)
    `(progn
      ,@(mapcar #/`(multiple-value-bind (,tmp ,chk) (,attr-op ,src ,(svref _ 0))
                    (if ,chk 
                      ,(funcall 'make-matcher (svref _ 1) tmp)
                      (return-from |test| nil)))                  
                elems))
    (<serialize-error>)))


(defun <not> (code)
  `(and 
    (block |test| (progn ,code t))
    (return-from |test| nil)))
    

(defmacro <call> (f x)
  (cond ((eq 'lambda (first f))
          (list f x))
        ;; #'symbolもしくは#'(lambda ...)の形式
        ((or (symbolp (second f))
             (eq 'lambda (caadr f)))
          (list 'funcall f x))
        ;; カリー化表現 #'(g ...)
        (t `(,@(second f) ,x))))

(defun <fix-slots-if-needed> (elems)
  (prog1 elems
    (when (find-if #'simple-vector-p elems)
      (dolist (v elems)
        (when (symbolp (svref v 0))
          (setf (svref v 0) (list 'QUOTE (svref v 0))))))))

(defun make-matcher (ptn src)
  (cond 

    ((<constant?> ptn)
      `(unless (equal ,ptn ,src) (return-from |test| nil)))
    ((symbolp ptn)
      (cond ((eq '_ ptn) '(progn)) ;; ワイルドカード
            ((member ptn *cur-vars*)
              `(unless (equal ,ptn ,src)
                (return-from |test| nil)))
            (*match-in-verify* 
              (error "MATCHER: ~D: 代入は禁止されています" *match-in-verify*))
            (t 
              (pushnew ptn *cur-vars*)
              `(setq ,ptn ,src))))
    ((eq 'quote (first ptn))
      `(unless (equal ,ptn ,src) (return-from |test| nil)))
    ((or (eq 'lambda (first ptn))
         (and (eq 'function (first ptn))
              (eq 2 (length ptn))))
      `(unless (<call> ,ptn ,src) (return-from |test| nil)))
    ((every #/(and #>consp (eq 'quote #>first)) 
            ptn)
      `(unless (or ,@(mapcar #/`(equal ,src ,_)
                             ptn))
        (return-from |test| nil)))


    ((eq 'PROGN (first ptn))
      `(unless (equal ,ptn ,src) (return-from |test| nil)))

    ((eq :OR (first ptn))
      `(unless (or ,@(let ((vars *cur-vars*)
                           acc)
                          (prog1 
                            (mapcar #/(let ((*cur-vars* vars))
                                        (prog1
                                          (if (and #>consp (eq :do #>first)) 
                                            `(progn ,(make-matcher _ src) nil)
                                            `(block |test| ,(make-matcher _ src) t))
                                          (setq acc (union *cur-vars* acc))))
                                    (cdr ptn))
                            (setq *cur-vars* acc))))
        (return-from |test| nil)))

    ((eq :AND (first ptn))
      (case (length (cdr ptn))
        (0 t)
        (1 (make-matcher (second ptn) src))
        (t `(progn ,@(mapcar #/(make-matcher _ src)
                             (cdr ptn))))))

    ((eq :if (first ptn))
      #{let* ((len (length (cdr ptn)))
              (test-clause (second ptn))
              (then-clause (third ptn))
              (else-clause (if (eq len 3) 
                             (fourth ptn)
                             '(:NEVER))))
      (let* ((vars *cur-vars*)
             (test (make-matcher test-clause src))
             (then (make-matcher then-clause src))
             (else (let ((*cur-vars* vars))
                     (prog1
                       (make-matcher else-clause src)
                       (setq vars (union *cur-vars* vars))))))
        (setq *cur-vars* (union vars *cur-vars*))
        `(if (block |test| ,test t) ,then ,else)))

    ((eq :GIVEN (first ptn))
      (pushnew (second ptn) *cur-vars*)
      `(unless (equal ,src ,(second ptn))
        (return-from |test| nil)))

    ((eq :UNGIVEN (first ptn))
      (pushnew (second ptn) *cur-vars*)
      `(setq ,(second ptn) ,src))
          
    ((eq :HERE (first ptn))
      `(unless (let ((_ ,src)) (declare (ignorable _)) ,@(cdr ptn))
        (return-from |test| nil)))

    ((eq :DO (first ptn))
      `(let ((_ ,src)) 
        (declare (ignorable _)) 
        ,@(cdr ptn)
        t))

    ((eq :NOT (first ptn))
      `(when (block |test| ,(make-matcher (second ptn) src) t)
        (return-from |test| nil)))

    ((eq :with (first ptn))
      (append (second ptn) 
              `((progn ,@(mapcar #/(make-matcher _ src) 
                                (cddr ptn))))))

    ((member (first ptn) '(:comp :comp-not))
      #{let ((op (if (eq :comp (first ptn)) 'unless 'when)))
      `(,op (equal ,src ,(second ptn))
        (return-from |test| nil)))

    ((eq :match (first ptn))
      `(unless ,(clpgk.base.unify::%make-match-code% src (cdr ptn))
        (return-from |test| nil)))
        
    ((eq :unify (first ptn))
      `(unless ,(clpgk.base.unify::%make-unify-code% src (cdr ptn))
        (return-from |test| nil)))

    ((eq :-> (first ptn))
      `(unless (do-unify ,src ,ptn)
        (return-from |test| nil)))

    ((eq :always (first ptn))
      t)
    ((eq :never (first ptn))
      '(return-from |test| nil))


    ;; 明示的なマクロ
    ((eq :macro (first ptn))
      (make-matcher (<expand-match-macro> (cdr ptn)) src))

    ;; 暗黙的なマクロ
    ((and (keywordp (first ptn))
          #!(<special-match-prefix?> (first ptn)))
      (make-matcher (<expand-match-macro> ptn) src))

    (t #{let ((prefix (when (keywordp (first ptn)) 
                        (prog1 (first ptn)
                          (setq ptn (cdr ptn))))))
       #{let* ((header (first ptn))
               (type (cond ((consp header)
                             (when (keywordp (second header))
                               (setq prefix (second header)
                                     header (cdr header)))                               
                             (second header) )
                           (t header)))
               (tester (when (consp header)
                         (third header)))                         
               (rest (rest ptn))
               (dst (when (eq '-> (first rest))
                      (prog1 
                        (second rest)
                        (setq rest (cddr rest)) )))
               (has-trans (and (consp header)
                               (eq 5 (length header))))
               (internal-type (case type 
                                ((VERIFY VERIFY-NOT) t)
                                ((DATA-ATTR TYPE-ATTR ATTR) t)
                                (LIST* 'list)
                                ((PROPER-LIST 
                                  PROPER-LIST* 
                                  LIST-VERIFY LIST-VERIFY-NOT) 'proper-list)
                                (CIRCULAR-LIST* 'circular-list)
                                ((LIST-AS-SET LIST-AS-MULTISET) 'proper-list)
                                ((SIMPLE-VECTOR-VERIFY
                                  SIMPLE-VECTOR-VERIFY-NOT
                                  SIMPLE-VECTOR-AS-SET SIMPLE-VECTOR-AS-MULTISET)
                                  'simple-vector)
                                ((VECTOR-VERIFY 
                                  VECTOR-VERIFY-NOT 
                                  VECTOR-AS-SET VECTOR-AS-MULTISET) 'vector)
                                ((SEQUENCE-VERIFY SEQUENCE-VERIFY-NOT
                                  SEQUENCE-AS-SET SEQUENCE-AS-MULTISET) 'sequence)
                                (HASH-TABLE-AS-SET 'hash-table)
                                (t type))))

      `(cond 
        ((typep ,src ',internal-type)
         ,@(when dst (list (make-matcher dst src)))
         ,@(when tester `((unless (<call> ,tester ,src)
                            (return-from |test| nil))))
         ,@(when has-trans #{let ((trans (fourth header))
                                  (term (fifth header)))
                 (if trans
                   `((let ((,src (<call> ,trans ,src)))
                       ,(make-matcher term src)))
                   (list (make-matcher term src))))

                                         
         ,@(when rest
                 (setq rest (<fix-slots-if-needed> rest))
                 #{let ((*match-cur-type* type))
                 (list (if prefix
                         (acase prefix
                           ((:VERIFY :VERIFY-NOT) 
                            #{let ((code (case type 
                                           ((T) (make-verify-matcher rest src))
                                           ((SIMPLE-VECTOR VECTOR STRING)(make-vector-verify-matcher rest src))
                                           (PROPER-LIST (make-list-verify-matcher rest src))
                                           ((CONS LIST) `(if (not (proper-list-p ,src))
                                                          (return-from |test| nil)
                                                          ,(make-list-verify-matcher rest src)))
                              
                                           (SEQUENCE (make-sequence-verify-matcher rest src))
                                           )
                                         ))
                            (if (eq it :VERIFY-NOT)
                              (<not> code)
                              code))
                                                        
                                                        
                           (:SET
                            (case type 
                              ((SIMPLE-VECTOR VECTOR STRING SEQUENCE) (make-sequence-as-set-matcher rest src))
                              (HASH-TABLE (make-hash-table-as-set-matcher rest src))
                              ((LIST CONS PROPER-LIST) (make-sequence-as-set-matcher rest src :for-list t))

                              ))

                           (:MULTISET
                            (case type 
                              ((SIMPLE-VECTOR VECTOR STRING SEQUENCE) (make-sequence-as-multiset-matcher rest src))
                              ((LIST CONS PROPER-LIST) (make-sequence-as-multiset-matcher rest src :for-list t))

                              ))
                           )
                         (acase type
                         ((LIST PROPER-LIST)  (make-list-matcher rest src))
                         ((LIST* PROPER-LIST* CIRCULAR-LIST*)  
                           (make-list*-matcher rest src))
                         (SIMPLE-VECTOR (make-simple-vector-matcher rest src))
                         ((VECTOR STRING) (make-vector-matcher rest src))
                         (HASH-TABLE (make-hash-table-matcher rest src))


                         (ALIST (make-alist-matcher rest src))

                         ((T) (make-and-matcher rest src))
                         ((DATA-ATTR TYPE-ATTR ATTR) 
                           (make-attribute-matcher it rest src))
                         (t (make-general-matcher type rest src)))))))
        (t (return-from |test| nil))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro do-match (src ptn &rest options)
  `(do-match-if (,src ,ptn ,@options) t nil))
(defmacro do-match-when ((src ptn &rest options) &body body)
  `(do-match-if (,src ,ptn ,@options) (progn ,@body) nil))
(defmacro do-match-unless ((src ptn &rest options) &body body)
  `(do-match-if (,src ,ptn ,@options) nil (progn ,@body)))
(defmacro do-match-gwhen ((src ptn &rest options) &body body)
  `(do-match-gif (,src ,ptn ,@options) (progn ,@body) nil))
(defmacro do-match-gunless ((src ptn &rest options) &body body)
  `(do-match-gif (,src ,ptn ,@options) nil (progn ,@body)))

(defmacro do-match-if ((src ptn &rest options) then &optional else)
  `(|do-match-if| (,src ,ptn ,@options) ,then ,else))
(defmacro do-match-gif ((src ptn &rest options) then &optional else)
  `(|do-match-if| (,src ,ptn ,@options) ,then ,else :guarded t))

(defmacro |do-match-if| ((src ptn &key enumerated) then else &key guarded)
  #{let* (*match-macro-table*
          (all-vars (<check> ptn :enumerated enumerated)))
  (unless enumerated
    (setq src (list src)
          ptn (list ptn)))
  (unless (= (length src) (length ptn))
    (error "DO-MATCH-IF: ソースとパターンの数の不整合"))

  #{let* ((n (length src))
          (tmpvars (freplicate n #'gensym))
          (main `(block |test|
                  (let ,(mapcar @\ab (list a b) tmpvars src)
                    (let ,all-vars
                      ,@(let (*cur-vars*)
                             (mapcar @\ab (make-matcher a b)
                                     ptn tmpvars))
                      (return-from |blk| ,then))))))
  (when guarded
    (setq main `(guard-block ,main)))
  `(block |blk|
      ,main
    ,else))



(defun <define> (macroname *-if-op def-header clauses &optional n args)
  #{when clauses
  #{let (pairs)
  (do-unify clauses
    (:EACH (:AND (:APPEND :MINIMIZE ?head (=) ?body)
                 (:HERE (push (cons head body) pairs)
                        (if n 
                          (= n (length head)) 
                          (setq n (length head))))))
    :failure (error "~D: 引数の数(正しくは~D)の不整合: ~D" macroname n clauses))

  (unless args 
    (setq args (freplicate n #'gensym)))
  
  #{let ((main 
          (reduce @\ab `(,*-if-op (,args ,(first a) :enumerated t) 
                        (progn ,@(rest a)) ,b)
                  (nreverse pairs)
                  :from-end t 
                  :initial-value `(error "~D: ~D failed" ',macroname ',clauses))))

  (if def-header
    `(,@def-header ,args ,main)
    main))

(defmacro lambda* (&body clauses)
  (<define> 'lambda* 'do-match-gif '(LAMBDA) clauses))

(defmacro defun* (fn-name &body clauses)
  (<define> 'defun* 'do-match-gif `(DEFUN ,fn-name) clauses))

(defmacro match ((&rest args) &body clauses)
  (<define> 'match 'do-match-gif nil clauses (length args) args))

(defmacro unify ((&rest args) &body clauses)
  (<define> 'match 'do-unify-gif nil clauses (length args) args))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(do-match-if ("foo"
               (STRING #(2 y) #(0 x)))
  (list x y))




(j-data:define-data foo (bar t) (baz t t))
(match ((baz 2 3))
  ((bar x) = x)
  ((baz a b) = (list a b))
  ((foo -> x) = (list 'foo x)))

(j-header)
(defun* oddlist
  ((CONS x xs) = #{cond
   ((oddp x) (cons x (oddlist xs)))
   (t (oddlist xs)))
  ((NULL) = nil))

(oddlist '(1 1 2 3 5 8 13 21 34 3))



(match (1 4)
  (x 2 = (list x 'in))
  (_ y = y))

(unify (1 2)
  (?x 2 = (list x 'in))
  (? ?y = y))



(defmethod* foo (t t t position)
  (f e)
  (a b c d =

(match (a b c)
  (T (CONS T x) _ = 
  
       
  

(funcall (lambda* (0 y = (list 'first y))
                  (1 (CONS y nil) = (list 'second y))
                  (x y = (list x y)))

         1 '(2))
(SECOND 2)
(SECOND 2)
(SECOND QUOTE)

((-> LIST #/#>cdr) a (INTEGER -> a) (NUMBER -> x))

(( LIST xs) a b)

(progn @let 3 #>1-)

(defun* fact 
  (0 = 1)
  (n = (* n (fact (1- n)))))

(fact 10)

(defun* foo 
  (0 = 'ok)
  (:hello = 'world)
  (_ = 'ng))

(foo 30)


(m-bind* (((INTEGER -> a) 3)
         ((CONS b c) '(2 3 4 5))
         ((LIST d e f) c :enumerated nil))
  (list a b c d e f))





                                                                                
(do-match-if x 
  (CONS #(car p) #(cdr q) #(car p))
  t)           
(do-match-if x 
  (CONS (POINT #(y a)) (CONS a (CONS)))
  t)           
        
(do-match-gif ('(1 '(2) 3)
            (LIST a (CONS) c))
  (progn (print (list a c)) (gfail) (print 'ok))
  'fail)



(do-match (1 '(1 2))
  (a (LIST a b))
  :enumerated t)
(do-match 3 nil)

(do-match-when (3 x) (print x) 'ok)








  



    
(do-match-if 3
  ((NUMBER nil #'#/#>numberp #'1+ x))
  x)

(do-match-if #'+
  (FUNCTION)
  t)

(do-match-if 3
  #/#>numberp
  t)

T
T

(defun foo (x) (funcall #'numberp x))


(do-match-if 3
  ((T ('2 '3) #/t #/#>- ((INTEGER x))))
  x)

(defun foo (x) (do-match-if x
                 ((T ('2 '3) #/t #/#>sqrt ((NUMBER x))))
                 x))
1.7320508f0



(list :-> :TO)






(LIST* (SYMBOL) ((SYMBOL s)) ((T r)))


(<check> '((foo bar) #(s '2) #(a f) #(d (LIST c bar a b c))))
(B A C F BAR)




(define-data point (p t point) unk)
(p 1 (p 1 (p 2 unk)))


(LIST a b . #(c))

(do-match-if '(1 (1 100) a)
  (LIST* a (LIST a ((INTEGER b))) ((PROPER-LIST c)))
  (list a b c ))


(do-match-if '(1 2 3 . 4)
  (LIST* a b c)
  (list a b c))

(foo)

(do-match-if #(1 2 1)
  (SIMPLE-VECTOR a b a)
  (list a b))

(do-match-if #(1 2)
  ((VECTOR nil nil))
  t)

(do-match-if 3
  #/#>numberp
  t)




(point 1 2)


(do-match-if ('(1 2 3 4 5 6)
               (LIST -> xs #(2 x) #(0 y) #(5 z)))
  (list x y z))
(3 1 6)

(3 1)
(3)
(find nil '(1 nil 3 4))

(do-match-if ((make-hash-table :initial-contents '((1 . 2) (3 . 4)))
              (HASH-TABLE #(3 x) #(1 y)))
  (list x y))

(do-match-if ('((1 . 2) (3 . 4))
               (ALIST #(1 x) #(3 4)))
  x)

(do-match-if ('(1 3 5 8)
               (LIST-AS-SET #(1 x) #(4 y)))
  (list x y))

2
1
               

(typep '((1 . 2) 3) 'alist)
(get a b)
(


(3 4)
(get '+ 'a)



(closure (foo bar) (list foo bar))

(defun foo (foo bar)
  (if foo 
    (closure (foo bar -> a b) (list a b foo bar))
    bar))

(time (foo 4 3))
#<COMPILED-FUNCTION FOO-F-1>
3
#<COMPILED-FUNCTION FOO-F-1>
#<COMPILED-FUNCTION FOO-F-1>
#<COMPILED-FUNCTION FOO-F-1>
#<COMPILED-FUNCTION FOO-F-1>


(do-match-if ('(1 2  3)
               (LIST-VERIFY -> xs (NUMBER)))
  xs)


(do-match-if (#(1 2 4 3)
               (SIMPLE-VECTOR-VERIFY -> xs (NUMBER)))
  xs)
#(1 2 4 3)
#(1 2 4 3)
"fpo"
#(1 2 4 3)

(do-match-if (3
              (foobar a))
  a)


(do-match-if ('(1 2 '(3))
               (SEQUENCE-VERIFY -> x (FIXNUM) (CONS)))
  x)

