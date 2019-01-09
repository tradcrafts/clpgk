;; -*- coding: utf-8 -*-

(oleo.core:oleo-core-header)

(in-package :oleo.base.form)


(defclass 100priorities () ((unused :allocation :class)))
(defclass p/98 (100priorities) ()) (defclass p/97 (p/98) ()) 
(defclass p/96 (p/97) ()) (defclass p/95 (p/96) ()) (defclass p/94 (p/95) ()) 
(defclass p/93 (p/94) ()) (defclass p/92 (p/93) ()) (defclass p/91 (p/92) ()) 
(defclass p/90 (p/91) ()) (defclass p/89 (p/90) ()) (defclass p/88 (p/89) ()) 
(defclass p/87 (p/88) ()) (defclass p/86 (p/87) ()) (defclass p/85 (p/86) ()) 
(defclass p/84 (p/85) ()) (defclass p/83 (p/84) ()) (defclass p/82 (p/83) ()) 
(defclass p/81 (p/82) ()) (defclass p/80 (p/81) ()) (defclass p/79 (p/80) ()) 
(defclass p/78 (p/79) ()) (defclass p/77 (p/78) ()) (defclass p/76 (p/77) ()) 
(defclass p/75 (p/76) ()) (defclass p/74 (p/75) ()) (defclass p/73 (p/74) ()) 
(defclass p/72 (p/73) ()) (defclass p/71 (p/72) ()) (defclass p/70 (p/71) ()) 
(defclass p/69 (p/70) ()) (defclass p/68 (p/69) ()) (defclass p/67 (p/68) ()) 
(defclass p/66 (p/67) ()) (defclass p/65 (p/66) ()) (defclass p/64 (p/65) ()) 
(defclass p/63 (p/64) ()) (defclass p/62 (p/63) ()) (defclass p/61 (p/62) ())
(defclass p/60 (p/61) ()) (defclass p/59 (p/60) ()) (defclass p/58 (p/59) ())
(defclass p/57 (p/58) ()) (defclass p/56 (p/57) ()) (defclass p/55 (p/56) ())
(defclass p/54 (p/55) ()) (defclass p/53 (p/54) ()) (defclass p/52 (p/53) ())
(defclass p/51 (p/52) ()) (defclass p/50 (p/51) ()) (defclass p/49 (p/50) ())
(defclass p/48 (p/49) ()) (defclass p/47 (p/48) ()) (defclass p/46 (p/47) ())
(defclass p/45 (p/46) ()) (defclass p/44 (p/45) ()) (defclass p/43 (p/44) ())
(defclass p/42 (p/43) ()) (defclass p/41 (p/42) ()) (defclass p/40 (p/41) ())
(defclass p/39 (p/40) ()) (defclass p/38 (p/39) ()) (defclass p/37 (p/38) ())
(defclass p/36 (p/37) ()) (defclass p/35 (p/36) ()) (defclass p/34 (p/35) ())
(defclass p/33 (p/34) ()) (defclass p/32 (p/33) ()) (defclass p/31 (p/32) ())
(defclass p/30 (p/31) ()) (defclass p/29 (p/30) ()) (defclass p/28 (p/29) ())
(defclass p/27 (p/28) ()) (defclass p/26 (p/27) ()) (defclass p/25 (p/26) ())
(defclass p/24 (p/25) ()) (defclass p/23 (p/24) ()) (defclass p/22 (p/23) ())
(defclass p/21 (p/22) ()) (defclass p/20 (p/21) ()) (defclass p/19 (p/20) ())
(defclass p/18 (p/19) ()) (defclass p/17 (p/18) ()) (defclass p/16 (p/17) ())
(defclass p/15 (p/16) ()) (defclass p/14 (p/15) ()) (defclass p/13 (p/14) ())
(defclass p/12 (p/13) ()) (defclass p/11 (p/12) ()) (defclass p/10 (p/11) ())
(defclass p/09 (p/10) ()) (defclass p/08 (p/09) ()) (defclass p/07 (p/08) ())
(defclass p/06 (p/07) ()) (defclass p/05 (p/06) ()) (defclass p/04 (p/05) ())
(defclass p/03 (p/04) ()) (defclass p/02 (p/03) ()) (defclass p/01 (p/02) ())
(defclass p/00 (p/01) ())


(defvar +100priorities+ (make-instance 'p/00))

(defconstant +<number-of-priorities>+ 100)
(defparameter +<priorities>+
  #(p/00 p/01 p/02 p/03 p/04 p/05 p/06 p/07 p/08 p/09
    p/10 p/11 p/12 p/13 p/14 p/15 p/16 p/17 p/18 p/19
    p/20 p/21 p/22 p/23 p/24 p/25 p/26 p/27 p/28 p/29
    p/30 p/31 p/32 p/33 p/34 p/35 p/36 p/37 p/38 p/39
    p/40 p/41 p/42 p/43 p/44 p/45 p/46 p/47 p/48 p/49
    p/50 p/51 p/52 p/53 p/54 p/55 p/56 p/57 p/58 p/59
    p/60 p/61 p/62 p/63 p/64 p/65 p/66 p/67 p/68 p/69
    p/70 p/71 p/72 p/73 p/74 p/75 p/76 p/77 p/78 p/79
    p/80 p/81 p/82 p/83 p/84 p/85 p/86 p/87 p/88 p/89
    p/90 p/91 p/92 p/93 p/94 p/95 p/96 p/97 p/98 100priorities))

(defun <special-parameter?> (x)
  (and (symbolp x)
       (member x '(&optional &key &aux &rest))))

(defun <make-params-for-defgeneric> (xs)
  (mapcar (lambda (x) (if (<special-parameter?> x)  x (gensym)))
          xs))



(defparameter *<param-macros>* NIL)

(defun <erase-parameter-macros> (input)
  (setf *<param-macros>*
          (delete-if (lambda (pair &aux (sample (car pair)) (transformer (cdr pair)))
                       (unless (eq '|param-not-match| (funcall transformer input T))
                         (warn "~W was removed" sample)
                         T))
                     *<param-macros>*)))

(defun <apply-parameter-macro> (input)
  (dolist (pair *<param-macros>* '|param-not-match|)
    (let* ((transformer (cdr pair))
           (result (funcall transformer input)))
      (unless (eq '|param-not-match| result)
        (return result)))))

               
(defmacro define-parameter-macro (sample-input-as-ident unification-pattern &body body)
  (let ((tmpvar (gensym)))
    `(let ((transformer (lambda (,tmpvar &optional only-unify) 
                          (do-unify-if (,tmpvar ,unification-pattern)
                            (if only-unify
                              T
                              (progn ,@body))
                            '|param-not-match|))))
       (unless (funcall transformer ',sample-input-as-ident)
         (error "~W failed" ',sample-input-as-ident))
       (<erase-parameter-macros> ',sample-input-as-ident)
       (push (cons ',sample-input-as-ident transformer)
             *<param-macros>*)
       NIL)))

(defvar *<template>* NIL)

(defun <template?> (x &optional force) (and (eq x '*)
                                            (or force *<template>*)))

(defun <var-as-template?> (x) (and *<template>* (eq x (first *<template>*))))

(defun <valid-parameter-symbol?> (x) (and (symbolp x)
                                          (not (keywordp x))
                                          (not (member x '(&optional &key &rest &aux)))
                                          (not (member x '(_ NIL T)))))

(defun <valid-var?> (x) (and (symbolp x)
                             (not (keywordp x))
                             (not (<template?> x t))
                             (not (member x '(&optional &key &rest &aux)))
                             (not (member x '(_ QUOTE)))))

(defun <valid-type?> (x) (and (symbolp x)
                              (not (<template?> x t))
                              (not (keywordp x))))

(defun <valid-exp?> (x) (not (<template?> x t)))

;; Wildcard: _
(define-parameter-macro _ _
  (with-gensyms (tmp) `(,tmp T :ignorable ,tmp)))

(define-parameter-macro (_ type :when exp) (:AND (_ (:AND (:-> #'<valid-type?>) ?type) . ?r) 
                                                 (:FOR r (:OR NIL ((:-> :EQ :when :unless) ?))))
  (with-gensyms (tmp)
    (if r
      `(,tmp ,type ,(first r) (let ((_ ,tmp)) ,(second r)))
      `(,tmp ,type :ignorable ,tmp))))

(define-parameter-macro (&OPTIONAL (_ exp :where exp))
    (&OPTIONAL (:AND (_ ?default . ?r) 
                     (:FOR r ((:-> :EQ :where) ?))))
  (with-gensyms (tmp)
    `(&OPTIONAL (,tmp ,default ,(first r) (let ((_ ,tmp)) ,(second r))))))

;; Wildcard: &optional _
(define-parameter-macro (&OPTIONAL _) (&OPTIONAL _)
  (with-gensyms (tmp) `(&OPTIONAL (,tmp NIL :ignore ,tmp))))

;; Wildcard: &rest _
(define-parameter-macro (&REST _) (&REST _)
  (with-gensyms (tmp) `(&REST (,tmp :ignore ,tmp))))


;; Required Variable: var
(define-parameter-macro sym (:AND (:-> #'<valid-parameter-symbol?>) ?x)
  (cond ((or (<template?> x) (<var-as-template?> x))  *<template>*)
        (T  (list x T))))

;; Optional/Keyword Variable: &optional var | &key var
(define-parameter-macro (&OPTIONAL var) ((:AND (:-> :EQ &OPTIONAL &KEY) ?tag)
                                         (:AND (:-> #'<valid-parameter-symbol?>) ?x))
  `(,tag ,(cond ((or (<template?> x) (<var-as-template?> x))
                  *<template>*)
                (T (list x NIL)))))



;; Rest Variable: &rest var
(define-parameter-macro (&REST var) (&REST (:AND (:-> #'<valid-parameter-symbol?>) ?x))
  `(&REST ,(cond ((or (<template?> x) (<var-as-template?> x))
                   *<template>*)
                 (T (list x)))))

(define-parameter-macro (* *) (:AND (?var ?type . ?r)
                                    (:HERE (or (<template?> var) (<template?> type))))
  (list* (if (<template?> var)   (first  *<template>*) var)
         (if (<template?> type)  (second *<template>*) type)
         r))

(define-parameter-macro (&OPTIONAL (* *)) (&OPTIONAL (:AND (?var ?exp . ?r)
                                                           (:HERE (or (<template?> var) (<template?> exp)))))
  (list '&OPTIONAL (list* (if (<template?> var)  (first  *<template>*) var)
                          (if (<template?> exp)  (second *<template>*) exp)
                          r)))

(define-parameter-macro (&REST (*)) (&REST ((:AND (:-> #'<template?>) ?var) . ?r))
  (list '&REST (cons (first  *<template>*) r)))


;; Priority Number: {0 to 63}
(define-parameter-macro 0 (:AND (:-> :view (x) (and (non-negative-integer-p x)
                                                    (< x +<number-of-priorities>+)))
                                ?num)
  (with-gensyms (tmp) `(,tmp ,(svref +<priorities>+ num) :ignore t)))
                               
(defun <numclass> (x &key strict )
  (cond ((and strict (integerp x)) 'integer)
        ((rationalp x) 'rational)
        ((numberp x) 'number)))

(define-parameter-macro (= 0) (= (:AND (:-> #'numberp) ?x))
  (with-gensyms (tmp)
    (if (rationalp x)
      `(,tmp (eql ,x))
      `(tmp number :when (= ,x ,tmp)))))

;; (define-parameter-macro (/= 0) (/= (:AND (:-> #'numberp) ?x))
;;   (with-gensyms (tmp)
;;     (cond ((rationalp x)
;;             `(,tmp ,(<numclass> x :strict t) :when (not (eql ,x ,tmp))))
;;           (T `(tmp number :when (/= ,x ,tmp))))))

(define-parameter-macro T T
  (with-gensyms (tmp) `(,tmp T :when ,tmp)))
(define-parameter-macro (&OPTIONAL T) (&OPTIONAL T)
  (with-gensyms (tmp) `(&OPTIONAL (,tmp NIL :when ,tmp))))
(define-parameter-macro (&REST T) (&REST T)
  (with-gensyms (tmp) `(&REST (,tmp :when ,tmp))))

(define-parameter-macro NIL NIL
  (with-gensyms (tmp) `(,tmp T :when (not ,tmp))))
(define-parameter-macro (&OPTIONAL NIL) (&OPTIONAL NIL)
  (with-gensyms (tmp) `(&OPTIONAL (,tmp T :when (not ,tmp)))))
(define-parameter-macro (&REST NIL) (&REST NIL)
  (with-gensyms (tmp) `(&REST (,tmp :when (not ,tmp)))))

(define-parameter-macro (QUOTE any) (:AND ?quoted (QUOTE ?))
  (with-gensyms (tmp) `(,tmp (eql ,quoted) :ignorable T)))
(define-parameter-macro (&OPTIONAL (QUOTE any)) (&OPTIONAL (:AND ?quoted (QUOTE ?)))
  (with-gensyms (tmp) `(&OPTIONAL (,tmp '|<<novalue>>| :when (eql ,quoted ,tmp)))))

;; (define-parameter-macro (EQL any) (EQL ?x)
;;   (with-gensyms (tmp) `(,tmp (eql ,x) :ignorable T)))
;; (define-parameter-macro (EQ any) (EQ ?x)
;;   (with-gensyms (tmp) `(,tmp (eql ,x) :ignorable T)))

(define-parameter-macro (:UNIFY ptn) ((:-> :EQ :UNIFY) ?ptn . ?r)
  (with-gensyms (tmp)
    `(,tmp T :let-when ((:UNIFY ,ptn) ,tmp) ,@r)))

(define-parameter-macro (:MATCH ptn) ((:-> :EQ :MATCH) ?ptn . ?r)
  (with-gensyms (tmp)
    `(,tmp T :let-when ((:MATCH ,ptn) ,tmp) ,@r)))

(defun <primitive-parameter?> (x)
  (flet ((primitive-options? (xs)
           (do-unify xs (:EACH (:-> :EQ :where :ignorable :ignore
                                    :when :unless :bind :let :let*
                                    :let-when :let-unless :do)
                               ?))))
    (do-unify x (:OR (:AND (:OR ((:-> #'<valid-var?>)
                                 (:OR (EQL ?)
                                      (:-> #'<valid-type?>))
                                 . ?r)
                                (&aux ((:-> #'<valid-var?>) ? . ?r))
                                (&optional ((:-> #'<valid-var?>) (:-> #'<valid-exp?>) . ?r))
                                (&key ((:-> #'<valid-var?>) (:-> #'<valid-exp?>) . ?r))
                                (&rest ((:-> #'<valid-var?>) . ?r)))
                           (:HERE (primitive-options? r)))
                     (:-> :EQ &optional &key &aux &rest)))))
  
            
;; (<primitive-parameter?> '(&rest (x)))
;; (<primitive-parameter?> '&rest)
       
(defun <trans> (var opts body &key (fail-call '(call-next-method)))
  (cond ((null opts) ;; one of {&OPTIONAL &AUX &REST &KEY}
          body)
        (t ;; (var type option...) の場合
          (let ((code body)
                ignorable
                tests)
            (do ((xs (reverse opts) (cddr xs)))
                ((null xs))
              (case (second xs)
                (:when (setf code (list 'IF (first xs) code fail-call)))
                (:unless (setf code (list 'IF (first xs) fail-call code)))
                (:let-when (setf code (list 'LET-IF (first xs) code fail-call)))
                (:let-unless (setf code (list 'LET-IF (first xs) fail-call code)))
                (:bind (setf code (list 'BIND (first xs) code)))
                (:let (setf code (list 'LET (first xs) code)))
                (:let* (setf code (list 'LET* (first xs) code)))
                (:do (setf code (list 'PROGN (first xs) code)))))

            (do ((xs opts (cddr xs)))
                ((null xs) (setf tests (nreverse tests)))
              (case (first xs)
                (:where (push (second xs) tests))
                ;;(:unless (push (list 'NOT (second xs)) tests))
                ((:ignorable :ignore) (setf ignorable (second xs)))))

            (when tests
              (setf code `(if (and ,@tests) ,code ,fail-call)))
            code))))


(defun <trans-all-params> (params body)
  (dolist (p (reverse params) body)
    (setf body
            (cond ((symbolp p) body)
                  ((member (first p) '(&optional &key &aux))
                    (let ((x (second p)))
                      (<trans> (first x) (cddr x) body)))
                  ((eql (first p) '&rest)
                    (let ((x (second p)))
                      (<trans> (first x) (cdr x) body)))
                  (T (<trans> (first p) (cddr p) body))))))


(defun <to-primitives> (source-params &optional templates)
  (awhen (position '&.. source-params)
    (when (position '&.. (nthcdr (1+ it) source-params))
      (error "too many &.."))
    (when (>= it (list-length templates))
      (error "misplaced &.."))
    (setq source-params (append (subseq source-params 0 it)
                                (subseq templates it)
                                (subseq source-params (1+ it)))))
  (let ((has-templates templates)
        primitives)
    (dolist (p (<lift-params> source-params))
      (let ((*<template>* (car templates)))
        (setq templates (cdr templates))
        (when (and (eq '&AUX p) has-templates)
          (setq has-templates nil))
        (when has-templates
          (unless *<template>*
            (error "template error"))
          (when (or (<special-parameter?> *<template>*)
                    (<special-parameter?> p))
            (unless (eq p *<template>*)
              (error "unmatch &"))))
        (until (<primitive-parameter?> p)
          (setf p (<apply-parameter-macro> p))
          (when (eql '|param-not-match| p)
            (error "param error ~W" p)))
        (push p primitives)))
    (when templates
      ;; テンプレートの残りがあるということは、パラメータの記述に不備があるということである
      (error "template error2"))
    (nreverse primitives)
    ))

;;(<to-primitives> '(a b (c t :when c)))
(defmacro define-generic (method-name (&rest source-params))
  (flet ((transform-params (source-params)
           (<unlift-params> (<to-primitives> source-params))))

    (let ((transformed (transform-params source-params)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defgeneric ,method-name ,(<make-params-for-defgeneric> transformed))
         (setf (get ',method-name '|templates|) ',transformed)))))

; (define-generic foo (a b _ (c t :when c) &optional _))
;; パラメータリストの正当性評価
(defun <valid-param-list-order?> (xs &key (allow-key T) (allow-aux T))
  (let ((pos/optional (position '&OPTIONAL xs))
        (pos/key (position '&KEY xs))
        (pos/rest (position '&REST xs))
        (pos/aux (position '&AUX xs)))
    (cond ((and (not allow-key) pos/key) NIL)
          ((and (not allow-aux) pos/aux) NIL)
          ((none pos/optional pos/key pos/rest pos/aux) T)
          ((or (and pos/optional (< 1 (count '&OPTIONAL xs)))
               (and pos/key (< 1 (count '&KEY xs)))
               (and pos/rest (< 1 (count '&REST xs)))
               (and pos/aux (< 1 (count '&AUX xs)))
               (and pos/optional (or (and pos/key (< pos/key pos/optional))
                                     (and pos/rest (< pos/rest pos/optional))
                                     (and pos/aux (< pos/aux pos/optional))))
               (and pos/rest (or (and pos/key (< pos/key pos/rest))
                                 (and pos/aux (< pos/aux pos/rest))))
               (and pos/key pos/aux (< pos/aux pos/rest)))
            NIL)
          ((and pos/rest (member (nth (1+ pos/rest) xs) '(NIL &AUX &KEY))) NIL)
          ((and pos/rest pos/key (not (eql (+ 2 pos/rest) pos/key))) NIL)
          ((and pos/rest (null pos/key) pos/aux (not (eql (+ 2 pos/rest) pos/aux))) NIL)
          ((and pos/rest (none pos/key pos/aux) (not (eql (length (nthcdr pos/rest xs)) 2))) NIL)
          (T T))
    ))

(defun <define/make-wrapper> (proxy template-params  &key wrapper-name (n-priorities 1))
  (let (in-optional  in-rest
        rest-param  wrapper-params  normal-params  optional-params static-fn-params
        (has-key (member '&KEY template-params))
        (header (if wrapper-name (list 'DEFUN wrapper-name) (list 'LAMBDA)))
        (initial-priorities (replicate n-priorities '+100priorities+))
        )
    (dolist (p template-params)
      (cond ((eq '&OPTIONAL p)
              (setq in-optional T)
              (push p wrapper-params))
            ((eq '&REST p)
              (setq in-rest T
                    in-optional NIL)
              (push p wrapper-params))
            ((eq '&KEY p) (return))
            (in-optional (with-gensyms (tmp)
                           (push `(,tmp ',tmp) wrapper-params)
                           (push tmp optional-params)))
            (in-rest (with-gensyms (tmp)
                       (push tmp wrapper-params)
                       (setq rest-param tmp)))
            (T (with-gensyms (tmp)
                 (push tmp wrapper-params)
                 (push tmp normal-params)))))
    (setq static-fn-params (nreverse normal-params)
          normal-params (nconc initial-priorities static-fn-params)
          wrapper-params (nreverse wrapper-params)
          optional-params (nreverse optional-params))

    (when (and has-key (not rest-param))
      (setq rest-param (gensym)))
    ;;(list wrapper-params normal-params optional-params rest-param)
    ;;    (when rest-param (setq optional-params (nconc optional-params `(,rest-param 
    (cond ((none optional-params rest-param)
            `(,@header ,static-fn-params (,proxy ,@normal-params)))
          ((null optional-params)
            `(,@header (,@static-fn-params &rest ,rest-param)
                       (if ,rest-param
                         (apply (function ,proxy) ,@normal-params ,rest-param)
                         (,proxy ,@normal-params))))
          (T
            `(,@header ,wrapper-params
               (cond
                 ,@(when rest-param `((,rest-param
                                       (apply (function ,proxy) ,@normal-params ,@optional-params ,rest-param))))
                 ,@(do ((xs optional-params (cdr xs))
                        codes
                        (given-params NIL))
                       ((null xs) (nreverse codes))
                     (let ((x (car xs)))
                       (push `((eq ,x ',x) (,proxy ,@normal-params ,@(reverse given-params))) codes)
                       (push x given-params)))
                 (T (,proxy ,@normal-params ,@optional-params))))))
                 
    ))

;; (<define/make-wrapper> 'internal '(&optional x y &rest r))
;; (<define/make-wrapper> 'internal '(a b c &optional x y &rest r))
;; (<define/make-wrapper> 'internal '(a b c &rest r))
;; (<define/make-wrapper> 'internal '(a b c))
;; (<define/make-wrapper> 'internal '(a b c &key k1 k2))

(defmacro define-method (ident (&rest params) &body body)
  (labels ((expand-body (codes)
             (do-unify-if (codes (where ?test-exp . ?rest-codes))
               `(if ,test-exp ,(expand-body rest-codes) (call-next-method))
               `(progn ,@codes))))
    (let* ((templates (get ident '|templates|))
           (primitives (<to-primitives> params templates)))
      `(DEFMETHOD ,ident ,(<unlift-to-raw-params> primitives)
         ,(<trans-all-params> primitives (expand-body body))))))

(defun <make-priority-combination> (num base m)
  (labels ((conv (num base m)
             (unless (zerop m)
               (if (< num base)
                 (cons num (conv 0 base (1- m)))
                 (cons (mod num base) (conv (floor (/ num base)) base (1- m)))))))
    (nreverse (conv num base m))))

(defmacro defn (ident (&rest param-templates) &body defs)
  (unless (<valid-param-list-order?> param-templates :allow-aux nil)
    (error "define: &KEY is not allowed"))
  (dolist (d defs)
    (unless (member '-> d)
      (error "define: illegal clause ~W" d)))
  (let* ((method-ident (intern (string-concat (symbol-name ident) "-PROXY-METHOD")))
         ;;(n-param (list-length param-templates))
         ;;(tmp-params (freplicate n-param #'gensym))
         (n-defs (list-length defs))
         (n-priorities (ceiling (log n-defs +<number-of-priorities>+))))
    
    `(eval-when (:load-toplevel :execute)
       (fmakunbound ',method-ident)
       (define-generic ,method-ident (,@(freplicate n-priorities #'gensym) ,@param-templates))
       ,(<define/make-wrapper> method-ident param-templates :wrapper-name ident :n-priorities n-priorities)
       ,@(do (codes
              (priority-number 0 (1+ priority-number))
              (defs defs (cdr defs)))
             ((null defs) (nreverse codes))
           (let* ((d (car defs))
                  (pos (position '-> d))
                  (priority-combination (<make-priority-combination> priority-number +<number-of-priorities>+ n-priorities)))
             (push `(define-method ,method-ident (,@priority-combination ,@(subseq d 0 pos))
                      ,@(nthcdr (1+ pos) d))
                   codes)))
       ;;(defun ,ident ,tmp-params (,method-ident +<p/0>+ ,@tmp-params))
       (values ',ident ',method-ident))))



;; (<valid-param-list-order?> '(a c b &rest a &aux f f))
;; (<valid-param-list-order?> '(a c b &rest a))
;; (defun tettt (&optional &rest x &key k z) (list x k z))
;; (tettt :k 30 :z 20)

;; (defun tettt (&rest x &optional a b))

(defun <lift-params> (xs)
  (let (mode
        lifted
        required
        optional
        key
        rest
        aux)

    (dolist (x xs)
      (case x
        ((&optional &key &rest &aux)
          (setf mode x)
          (push x lifted))
        (T (push (first (case mode
                          (&optional (push (list '&OPTIONAL x) optional))
                          (&key (push (list '&KEY x) key))
                          (&rest (push (list '&REST x) rest))
                          (&aux (push (list '&AUX x) aux))
                          (T (push x required))))
                 lifted))))

    (when (< 1 (list-length rest))
      (error "too many REST parameters"))
    
    (list (nreverse required)
          (nreverse optional)
          (nreverse key)
          rest
          (nreverse aux))
    (nreverse lifted)
    ))

(defun <unlift-to-raw-params> (lifted-params)
  (mapcar (lambda (x)
            (cond ((<special-parameter?> x)  x)
                  ((member (first x) '(&OPTIONAL &KEY &AUX))
                    (subseq (second x) 0 2))
                  ((eql (first x) '&REST)
                    (caadr x))
                  (T  (subseq x 0 2))))
          lifted-params))

(defun <unlift-params> (lifted-params)
  (mapcar (lambda (x)
            (cond ((<special-parameter?> x)  x)
                  ((<special-parameter?> (first x))  (second x))
                  (T  x)))
          lifted-params))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



