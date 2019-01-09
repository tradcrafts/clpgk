;; -*- coding: utf-8 -*-

(defpackage :oleo.mspace
  (:use :cl) ;; <<- これをしないとCCLでエラーがでる。シンボル|@|について競合するとかなんとか…
  (:nicknames :oleo.core.monolithic-name-space))

(oleo.core.init:define-package :oleo.core.basic-definitions ()
  (:use :cl :oleo.core.init :anaphora :cl-annot)

  (:import/export :oleo.core.primary-annotations)
  (:import/export-from metabang-bind #:bind)
  (:import/export-from split-sequence #:split-sequence #:split-sequence-if #:split-sequence-if-not)


  (:export

   #:monolize
   #:define-function #:define-function*

   #:mspaced

   #:productive #:productive*
   
   #:register-arity #:register-arity*
   #:query-arity
   
   #:ap #:ap*
   
   #:partial-funcall #:partial-funcall* #:partial-apply #:partial-apply*
   )
  )


(in-package :oleo.core.basic-definitions)

(eval-when (:compile-toplevel :load-toplevel)
  (setq *readtable* (copy-readtable))
  (cl-annot:enable-annot-syntax))

@package-optimize-level 1


(defparameter *<pkg/mspace>* (find-package :oleo.mspace))

(defun <has-lower-case?> (str)
  (find-if (lambda (c) (and (alpha-char-p c) (lower-case-p c)))
           (the string str)))

(defun <transform-symbol-name-if-needed> (name)
  (if (<has-lower-case?> name)
    (if (upper-case-p (char name 0))
      (let ((copied (copy-seq name)))
        (setf (char copied 0) (char-downcase (char name 0)))
        copied)
      name)
    (string-downcase name)))

;; (<transform-symbol-name-if-needed> 'a)

(defun <intern-to-mspace> (sym)
  (intern (<transform-symbol-name-if-needed> (symbol-name sym))
          *<pkg/mspace>*))

(defun <monolize> (idents &aux return-value)
  (dolist (ident idents)
    (assert (and 'monolize (or (symbolp ident) (stringp ident)))))
  (dolist (ident idents return-value)
    (let* ((name (if (symbolp ident) (symbol-name ident) ident))
           (transformed (<transform-symbol-name-if-needed> name))
           (result (intern transformed *<pkg/mspace>*)))
      (unless return-value
        (setq return-value result)))))

;; 単純なインターン （シンボルもしくは文字列から）
(defun intern-to-mspace (ident)
  (monolize ident nil))

(defun monolize (ident &rest more-idents)
  (<monolize> (cons ident more-idents)))

;; マクロ版 （ただし、裸のシンボルしか受け付けない）
(defmacro monodecl (symbol &rest more-symbols)
  `(<monolize> '(,symbol ,@more-symbols)))

;(monodecl a b c)


;; 変数関連付け　関数関連付け　色々考えられるが…
;(defun monolink )

;(monolize 'foobar 'bar)


;;; arity

(defun <get-arities-by-lambda-list> (lambda-list)
  (multiple-value-bind (normvars optvars restvar keyvars)
      (alexandria:parse-ordinary-lambda-list lambda-list :allow-specializers t)
    (let ((functional-arity (length (the list normvars))))
    (values functional-arity
            (if (or restvar keyvars)
              -1
              (+ functional-arity (length (the list optvars))))))))


(defun query-arity (fn-sym)
  (let ((arity-info (the cons (get fn-sym '<arity> '(-1 . -1)))))
    (values (car arity-info) (cdr arity-info))))

(defun <register-arity> (fn-sym functional-arity max-arity mspaced?)
  (unless (and (integerp functional-arity) (integerp max-arity)
               (>= functional-arity -1) (>= max-arity -1)
               (or (eql -1 max-arity) (>= max-arity functional-arity)))
    (error "wrong arguments: (REGISTER-ARITY ~A ~A)" functional-arity max-arity))
  (let ((info (cons functional-arity max-arity)))
    (setf (get fn-sym '<arity>) info)
    (when mspaced?
      (let ((mspaced-sym (monolize fn-sym)))
        (when (not (eq fn-sym mspaced-sym))
          (setf (symbol-function mspaced-sym) (symbol-function fn-sym)
                (get mspaced-sym '<arity>) info)))))
  fn-sym)

(defun register-arity (fn-sym functional-arity &optional (max-arity functional-arity))
  (<register-arity> fn-sym functional-arity max-arity nil))
(defun register-arity* (fn-sym functional-arity &optional (max-arity functional-arity))
  (<register-arity> fn-sym functional-arity max-arity t))
  


(defun <ap> (op args allow-rest-parameters?)
  (let ((n (length args)))
    (multiple-value-bind (f-arity max-arity) (query-arity op)
      (cond ((= n f-arity) (cons op args))               
            ((< n f-arity)
              (let ((m (- f-arity n))
                    params)
                (dotimes (i m) (push (gensym) params))
                (if (and (eql max-arity -1) allow-rest-parameters?)
                  `(lambda (,@params &rest |ap*_rest|) (apply ',op ,@args ,@params |ap*_rest|))
                  `(lambda ,params (,op ,@args ,@params)))
                ))
            ((or (eql f-arity -1) (eql max-arity -1) (< n max-arity))
              (cons op args))
            (t (error "(ap ~A ...) too many arguments" op))))))
  
(defmacro ap (op &rest args)
  (assert (and 'ap (symbolp op)))
  (<ap> op args nil))
(defmacro ap* (op &rest args)
  (assert (and 'ap* (symbolp op)))
  (<ap> op args t))

(register-arity '+ 2 -1)
(register-arity '- 2 -1)
(register-arity '* 2 -1)
(register-arity '/ 2 -1)

;(register-arity* 'cons 2)

(defun <definition/defun> (name lambda-list body)
  `(defun ,name ,lambda-list ,@body))


;(<get-functional-arity-by-lambda-list> '(a b c &optional x y &rest xs))
;(alexandria:parse-ordinary-lambda-list '((x integer) b c &optional x &key f) :allow-specializers t)

(defun <define-function> (name lambda-list body &optional mspaced?)
  (multiple-value-bind (functional-arity max-arity)
      (<get-arities-by-lambda-list> lambda-list)
    `(progn
       ,(<definition/defun> name lambda-list body)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (register-arity ',name ,functional-arity ,max-arity))
       ,@(when mspaced?
           (let ((m-name (monolize name)))
             `((setf (symbol-function ',m-name) (function ,name))
               (eval-when (:compile-toplevel :load-toplevel :execute)
                 (register-arity ',m-name ,functional-arity ,max-arity))
               )))
       ',name)))


;(<define-function> 'a '(b c &key x) '(d e) t)


(defmacro define-function (name lambda-list &body body)
  (<define-function> name lambda-list body nil))

(defmacro define-function* (name lambda-list &body body)
  (<define-function> name lambda-list body t))


;(define-function foo (a b) (list a b))
;(define-function* foo (a b &optional c) (list a b c))

(defun <productive> (form ex?)
  (unless (consp form)
    (error ""))
  (let ((op (car form)))
    (cond ((eq op 'defun)
            (list* (if ex? 'define-function* 'define-function)
                   (cdr form)))
          ((eq op 'lambda)
            (list* 'define-function* (cdr form))))))

;(<altered> '(funcall (defun b c d) c) nil)
;(<altered> '(defun b (c d) e f) t)

(define-annotation* productive (form)
  (<productive> form nil))

(define-annotation* productive* (form)
  (<productive> form t))

(define-annotation mspaced (ident)
  (monolize ident))

;;(productive (defun foo (x) x))
;(define-function* foonar (a b) (list a b))


;;; funcall* apply*

;; リテラルであるかどうかのおおまかな判定
(defun <constant?> (x)
  (if (atom x)
    (or (numberp x) (keywordp x) (null x) (eq 'T x) (vectorp x) (arrayp x))
    (or (eq 'quote (car x)) (eq 'function (car x)))))

(<constant?> t)
      

(defun <make/call*> (ap-op allow-rest-parameters? f arguments)
  (let* ((n (length arguments))
         (m (count '* arguments)))
    (if (and (eql 0 m) (not allow-rest-parameters?))
      (list* ap-op f arguments)
      (let* ((infos (let (tmp)
                      (dolist (x arguments (nreverse tmp))
                        (cond ((eq '* x) (push (gensym "P") tmp))
                              ((and (not allow-rest-parameters?)
                                    (<constant?> x))
                                (push (cons x nil) tmp))
                              (t (push (list (gensym "V") x) tmp))))))
             (args (mapcar (lambda (x) (cond ((atom x) x)
                                             ((cdr x) (first x))
                                             (t (car x))))
                           infos))
             (vars (mapcan (lambda (x) (unless (or (atom x) (null (cdr x))) (list x))) infos))
             (fn (if (and (listp f)
                            (or (eq 'quote (first f))
                                (eq 'function (first f))))
                   f
                   (let ((fnvar (gensym "F")))                     
                     (setq vars (cons (list fnvar f) vars))
                     fnvar)))
             (lambda-list (mapcan (lambda (x) (when (atom x) (list x))) infos))
             (args* (when allow-rest-parameters?
                      (setq lambda-list (nconc lambda-list '(&rest rest)))
                      (if (eq 'funcall ap-op)
                        (if args
                          (nconc (butlast args) `((cons ,(car (last args)) rest)))
                          '(rest))
                        (nconc (butlast args) `((append ,(car (last args)) rest))))))
             (form (list* ap-op fn args))
             (form* (when args* `(if rest (apply ,fn ,@args*) ,form)))
             (call-form (list 'lambda lambda-list (if form* form* form)))
             )
        (if vars
          `(let ,vars ,call-form)
          call-form)))))

(defmacro partial-funcall (f &rest arguments)
  (<make/call*> 'funcall nil f arguments))
(defmacro partial-funcall* (f &rest arguments)
  (<make/call*> 'funcall t f arguments))
(defmacro partial-apply (f first-argument &rest arguments)
  (<make/call*> 'apply nil f (cons first-argument arguments)))
(defmacro partial-apply* (f first-argument &rest arguments)
  (<make/call*> 'apply t f (cons first-argument arguments)))

