;; J-FORM  Time-stamp: <2018-10-05 16:06:53 USER> {incremental autotitle}

(oleo.core:oleo-core-header)
(in-package :oleo.base.form)
;; let-if:
;; let-if (dst src) then else
;; ここで、dstは
;; シンボル{変数名,T,NIL,:T,:NIL} or (:IF var) (:NOT var) (:AND var..) (:OR var..) (:NONE var..)
;; (:BIND bind-ptns..) (:UNIFY unify-ptns..)
;; の何れか
;; ここで、var := {変数名,_} ワイルドカードの場合は変数束縛が行われないため、then節からも見えない
;; var.. := (:COMPLEX var_1 var_2 ... var_n) ここで、変数の要素数は２以上が要件となる
;; dstが複数要素の場合、srcは:COMPLEXを使った複合表現でなければならない
;; 重要：ELSE節からはLETで導入された変数は見えない
;; 裸の変数の場合はletに等しい (let-if V exp then else) --> (let ((V exp)) then)
;; 裸の変数がワイルドカード_の場合は、(let-if _ exp then else) --> (progn exp then)
;; :T,:NILの場合:
;; (let-if (:T exp) then else) --> (let-if ((:IF _) exp) then else)と等価
;; (let-if (:T (:COMPLEX exp..)) then else) --> (let-if ((:AND _ _ ...) (:COMPLEX exp..)) then else)と等価
;; (let-if (:NIL exp) then else) --> (let-if (:NOT _) exp then else)と等価
;; (let-if (:NIL (:COMPLEX exp..)) then else) --> (let-if (:NONE _ _ ...) (:COMPLEX exp..) then else)と等価
;; Tの場合、srcが何であれ無条件で真となりthen節が選択される
;; NILの場合、無条件で偽となりelse節が選択される
;; (let-if ((:IF V) exp) then else)
;; (let-if ((:NOT V) exp) then else)
;; (let-if ((:AND V1) exp) then else) , (let-if ((:AND V1 V2) (:COMPLEX exp1 exp2)) then else)
;;重要：どのような場合でも、expの式は(複合式の場合は含まれる全ての式が左から順に）必ず評価される


(defun <verify-dst> (x)
  (do-unify x
    (:OR (:CALL var)
         (:-> :EQ T NIL :T :NIL)
         ((:OR :NOT :IF) (:CALL var))
         (:AND ((:-> :EQ :AND :OR :NONE) . ?r)
               (:FOR r (:EACH+ (:CALL var))))
         (:AND ((:-> :EQ :unify :match) ? . ?r)
               (:HERE (proper-list-p r))))
    :define
    ((var (:-> :view (x) (and (symbolp x)
                              (not (keywordp x))
                              (not (null x))
                              (not (eq T x))))))))

(defun <complex-src?> (x)
  (and (consp x) (eq :complex (first x))))

(defun <verify-src> (x)
  (cond ((<complex-src?> x)
          (and (proper-list-p x)
               (<= 3 (list-length x))))
        (t t)))

(defun <wildcard?> (x) (eql '_ x))

(defun <strict-logic> (op exps)
  (let ((tmpvars (freplicate (list-length exps) #'gensym)))
    `(let ,(mapcar #'list tmpvars exps)
       (,op ,@tmpvars))))

                       
(defmacro let-if (&whole definition
                  (dst src) &body then-and-else)
  (unless (and (<verify-dst> dst)
               (<verify-src> src)
               then-and-else
               (<= (list-length then-and-else) 2))
    (error "LET-IF: syntax error: ~W" definition))

  
  (let* ((then (first then-and-else))
         (else (second then-and-else))
         (xs (if (<complex-src?> src)
               (cdr src)
               (list src)))
         (src-arity (list-length xs)))
      
          
    (if (symbolp dst)
      (cond ((eq :T dst) `(if ,(<strict-logic> 'and xs) ,then ,else))
            ((eq :NIL dst) `(if ,(<strict-logic> 'none xs) ,then ,else))
            ((eq T dst) `(progn ,@xs ,then))
            ((eq NIL dst) `(progn ,@xs ,else))
            ((< 1 src-arity) (error "too many sources"))
            ((<wildcard?> dst) then)
            (t  `(let ((,dst ,src)) ,then)))
      ;; 
      (let* ((dst-arity (list-length (cdr dst)))
             (cmd (first dst))
             (operator (case cmd
                         (:unify 'do-unify-if) (:match 'do-match-if)
                         (:and 'and) (:or 'or) (:none 'none) (:not 'not) (:if 'identity))))
        
        (unless (eql src-arity dst-arity)
          (error "arity err"))
        (case cmd
          ((:unify :match)
            (if (eql 1 src-arity)
              `(,operator (,src ,(second dst)) ,then ,else)
              `(,operator (,xs ,(cdr dst) :enumerated t) ,then ,else)))
          
          ((:and :or :none)
            (let* ((vars (cdr dst))
                   (tmps (freplicate (list-length vars) #'gensym))
                   (test (cons operator tmps))
                   (tmp-bindings (mapcar #'list tmps xs))
                   (main-bindings (mapcan (lambda (var tmp) (unless (<wildcard?> var) `((,var ,tmp))))
                                          vars tmps)))
                                          
              (if main-bindings
                `(let ,tmp-bindings
                   (if ,test
                     (let ,main-bindings ,then)
                     ,else))
                `(let ,tmp-bindings
                   (if ,test ,then ,else)))))
          
          ((:not :if)
            (cond ((<wildcard?> (second dst))
                    `(if (,operator ,src) ,then ,else))
                  (t (let ((tmp (gensym)))
                       `(let ((,tmp ,src))
                          (if (,operator ,tmp)
                            (let ((,(second dst) ,tmp)) ,then)
                            ,else)))))))))))

         
(defmacro let-when ((dst src) &body body)
  `(let-if (,dst ,src) (progn ,@body)))

(defmacro let-unless ((dst src) &body body)
  `(let-if (,dst ,src) NIL (progn ,@body)))

(defmacro let-case (&whole definition
                    src &body dst-and-body-clauses)
  (flet ((verify-clauses (cs)
           (do-unify cs
             (:EACH (:AND ((:-> :view (x) (<verify-dst> x)) . ?r)
                          (:HERE (proper-list-p r)))))))
    (unless (and (<verify-src> src)
                 (verify-clauses dst-and-body-clauses))
      (error "LET-CASE: syntax error: ~W" definition))

    (let* ((tmp-src (if (<complex-src?> src)
                      (cons :complex (freplicate (list-length (cdr src)) #'gensym))
                      (gensym)))
           (bindings (if (atom tmp-src)
                       `((,tmp-src ,src))
                       (mapcar #'list (cdr tmp-src) (cdr src))))
           )

      `(let ,bindings
         ,(reduce (lambda (clause else) `(let-if (,(first clause) ,tmp-src)
                                           (progn ,@(rest clause))  ,else))
                  dst-and-body-clauses
                  :initial-value nil :from-end t)))
    ))

(defmacro let-cond (&whole definition
                    &body dst-src-and-body-clauses)
  (flet ((verify-clauses (cs)
           (do-unify cs
             (:EACH (:AND ((:-> :view (x) (<verify-dst> x)) (:-> :view (x) (<verify-src> x)) . ?r)
                          (:HERE (proper-list-p r)))))))
    (unless (verify-clauses dst-src-and-body-clauses)
      (error "LET-COND: syntax error: ~W" definition))

    (reduce (lambda (clause else) `(let-if (,(first clause) ,(second clause))
                                     (progn ,@(cddr clause))  ,else))
            dst-src-and-body-clauses
            :initial-value nil :from-end t)))


(defmacro let-while (dst src &body body)
  `(while t (let-if (,dst ,src)
              (progn ,@body)
              (return))))

(defmacro let-until (dst src &body body)
  `(while t (let-if (,dst ,src)
              (return)
              (progn ,@body))))


(defun <split-lambdalist> (params)
  (let ((pos (position-if (lambda (x) (member x '(&OPTIONAL &REST &KEY &AUX)))
                          params)))
    (if pos
      (list (subseq params 0 pos) (subseq params pos))
      (list params nil))))


(defun <let-lambda> (params body &key named)
  (bind (all-vars
         srcs
         ((dsts rest-params) (<split-lambdalist> params)))
    (dolist (d dsts)
      (unless (<verify-dst> d)
        (error ""))
      (let* ((arity (if (consp d)  (list-length (cdr d))  1))
             (vars (freplicate arity #'gensym)))
        (setf all-vars (nconc all-vars vars))
        (if (eql 1 arity)
          (setf srcs (nconc srcs (list (first vars))))
          (setf srcs (nconc srcs `((:complex ,@(copy-list vars))))))))

    `(,@(if named (list 'defun named) '(lambda)) (,@all-vars ,@rest-params)
       (declare (ignorable ,@all-vars))
       ,(reduce (lambda (dst-src rest)
                  `(let-if (,(first dst-src) ,(second dst-src)) ,rest NIL))
                (mapcar #'list dsts srcs)
                :from-end t :initial-value `(progn ,@body)))))

(defmacro lambda/let ((&rest params) &body body)
  (<let-lambda> params body))
(defmacro defun/let (ident (&rest params) &body body)
  (<let-lambda> params body :named ident))


(defmacro let*-if ((&rest dst-and-src-pairs) then &optional else)
  (let ((main-code (reduce (lambda (dst-and-ptn code)
                               (list 'let-if dst-and-ptn code))
                             dst-and-src-pairs
                             :initial-value (if else
                                              `(return-from |let*-if-block| ,then)
                                              then)
                             :from-end t)))
    (if else
      `(block |let*-if-block|
         ,main-code
         ,else)
      main-code)))

; (let*-if (((:unify ?a) 1) ((:unify (?b ?)) '(2))) (list a b) 'ng)


(defmacro let*-when ((&rest dst-and-src-pairs) &body body)
  `(let*-if ,dst-and-src-pairs (progn ,@body)))

(defmacro let*-unless ((&rest dst-and-src-pairs) &body body)
  `(let*-if ,dst-and-src-pairs NIL (progn ,@body)))

(defmacro let*-while (dst src &body body)
  `(while t (let*-if (,dst ,src)
                     (progn ,@body)
                     (return))))

(defmacro let*-until (dst src &body body)
  `(while t (let*-if (,dst ,src)
                     (return)
                     (progn ,@body))))

;;(let*/let (((:unify ?a) '(1)) ((:unify (?b)) a)) (list a b))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

