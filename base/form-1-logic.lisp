;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.core:clpgk-core-header)
(in-package :clpgk.base.form)




;; OBSOLETE
;; (defmacro define (name ll &body body)
;;   (error "jn-form: define is OBSOLETE")
;;   (with- (let (ignore used-underbars tests aux) 
;;            + symbol-macrolet ((_ (let ((g (gensym)))
;;                                    (push g used-underbars)
;;                                    g))))
;;     (unless (listp ll)
;;       (setq aux (list ll))
;;       (unless (and (>= (length body) 1)
;;                    (listp (car body)))
;;         (error "define (aux): lambda list required"))
;;       (setq ll (pop body)))

;;     (labels ((conv (x) 
;;                  (cond
;;                    (ignore x)
;;                    ((member x '(&optional &rest &key &aux)) 
;;                      (setq ignore t) 
;;                      x)
;;                    ((and (symbolp x) 
;;                          (not (keywordp x))) 
;;                      x)
;;                    ((atom x)              `(,_ (eql ,x)))
;;                    ((eql '= (car x))      `(,_ (eql ,(cadr x))))
;;                    ((eql '== (car x))      `(,(cadr x) (eql ,(caddr x))))
;;                    ((eql 'quote (car x))  `(,_ (eql ',(cadr x))))
;;                    ((eql '-> (car x)) (let ((g (gensym)))
;;                                         (push `(,@(cdr x) ,g) tests)
;;                                         g))
;;                    ((eql '=> (car x)) 
;;                      (push `(,@(cddr x) ,(cadr x)) tests)
;;                      (cadr x))
;;                    ((= 1 (length x))      `(,_ ,(car x)))
;;                    ((= 2 (length x))      (reverse x))
;;                    (t (error "define: lambda list syntax error")))))
      
;;       (let ((converted-ll (mapcar #'conv ll)))
;;       `(defmethod ,name ,@aux ,converted-ll 
;;         ,@(when used-underbars `((declare (ignore ,@used-underbars))))
;;         ,@(mapcar (lambda (test) `(if (not ,test) (call-next-method))) 
;;                   (nreverse tests))
;;         ,@body)))))

;; "logical forms 論理関数"

;; logical operator `->' (論理包含)
(defmacro then (a &body body)
  `(if ,a (progn ,@body) t))

;; logical operator `->' (論理包含) 多引数版 (左結合)
(defmacro imp (a b &rest rest)
  (flet ((-> (x y) 
           `(if ,x ,y t)))
    (reduce #'-> (cons (-> a b) rest))))

;; logical operator `<=>' (論理同値) (左結合)
(defmacro eqv (a b &rest rest)
  (flet ((<-> (x y)
           (let ((tmp1 (memoized (gensym)))
                 (tmp2 (memoized (gensym))))
             `(let ((,tmp1 ,x)
                    (,tmp2 ,y))
               (if ,tmp1 ,tmp2 (not ,tmp2))))))
    (reduce #'<-> (cons (<-> a b) rest))))
      
    
    


(defmacro logical<= (limit &rest logicalexps)
  (with-gensyms (cnt) 
  `(block LOG
    (let ((,cnt ,limit))
      (when (>= ,cnt 0)
        ,@(mapcar (lambda (exp rest)
                    `(if (<= ,rest ,cnt)
                      (return-from LOG t)
                      (if ,exp 
                        (if (> 0 (decf ,cnt)) 
                          (return-from LOG)))))
                  logicalexps
                  (nreverse (iota (length logicalexps) :start 1)))
        t)))))

(defmacro logical>= (limit &rest logicalexps)
  `(logical<= (- ,(length logicalexps) ,limit)
    ,@(mapcar (lambda (exp) (list 'not exp))
              logicalexps)))

(defmacro logical< (limit &rest logicalexps)
  `(logical<= (1- ,limit) ,@logicalexps))

(defmacro logical> (limit &rest logicalexps)
  `(logical>= (1+ ,limit) ,@logicalexps))



(defmacro logical= (limit &rest logicalexps)
  (with-gensyms (cnt) 
  `(block LOG
    (let ((,cnt ,limit))
      (when (>= ,cnt 0)
        ,@(mapcar (lambda (exp rest)
                    `(if (< ,rest ,cnt)
                      (return-from LOG)
                      (if ,exp 
                        (if (> 0 (decf ,cnt)) 
                          (return-from LOG)))))
                  logicalexps
                  (nreverse (iota (length logicalexps) :start 1)))
        t)))))

(defmacro logical/= (limit &rest logicalexps)
  `(not (logical= ,limit ,@logicalexps)))

;; Predicate Utilities 


(defun <get-is-symbol> (sym &optional need-warn)
  (aif (get sym '|is|)
       it
       #{progn
       (when need-warn (warn "IS: ~D is not defined yet" sym))
       (setf (get sym '|is|)
               (gensym (string-concat "(IS " (symbol-name sym) ")")))))

(defun <gensym-for-isnt> (name)
  (gensym (string-concat "(IS-NOT " (symbol-name name) ")")))

(defun <build-isnt-function> (internal src arity)
  #{let (tmps)
  (dotimes (_ (abs arity))
    (push (gensym) tmps))
  (compile internal   
           (if (<= 0 arity)
             `(lambda ,tmps (not (funcall #',src ,@tmps)))
             `(lambda (,@tmps &rest rest) (not (apply #',src ,@tmps rest))))))

  

(defun <get-isnt-symbol> (sym)
  (atypecase (get sym '|isnt|)
    (null #{let ((internal (<gensym-for-isnt> sym)))
          (setf (get sym '|isnt|) 
                  (compile internal
                           `(lambda (&rest xs) (apply #',(<get-is-symbol> sym t) xs)))))
    (symbol it)
    (t #{let ((internal (<gensym-for-isnt> sym)))
       (setf (get sym '|isnt|)
               (<build-isnt-function> internal (<get-is-symbol> sym) it)))))

(defun <get-arity> (lambda-list)
  (multiple-value-bind (vars optionals rest keys)
      (parse-ordinary-lambda-list lambda-list)
    #{let ((arity (1+ (length vars))))
    (if (and (null optionals) (null rest) (null keys))
      arity
      (- arity))))


(defun <manipulate-isnt> (name arity)
  (atypecase (get name '|isnt|)
    (null (setf (get name '|isnt|) arity))
    (symbol (<build-isnt-function> it (<get-is-symbol> name) arity))
    (t (setf (get name '|isnt|) arity))))

(defmacro define-is (name (&rest rest-lambda-list) &body body)
  (assert (and 'define-is
               (symbolp name)))
  #{let ((sym (<get-is-symbol> name)))
  (when (fboundp sym) 
    (warn "DEFINE-IS: redefining ~D" name))
  #{let ((arity (<get-arity> rest-lambda-list)))
 
  `(progn 
    (defun ,sym (_ ,@rest-lambda-list) ,@body)
    (<manipulate-isnt> ',name ,arity)
    ',name))

(defun <check-name> (name)
  (or (symbolp name)
      (and (consp name)
           (symbolp (first name))
           (proper-list-p name))))

(defun <parse> (xs)
 
  (flet ((<constant?> (x)            
           (not (or (and (consp x) 
                         (not (eq 'quote (first x))))
                    (and (symbolp x)
                         (not (or (keywordp x)
                                  (member x '(nil t)))))))))
    
    (if (every #'<constant?> xs)
      xs
      (let ((pairs (mapcar #/(cons _ (unless #><constant?> (gensym)))
                           xs)))
        (values 
         (mapcar #/(if #>cdr #>cdr #>car)
                 pairs)
         (mapcan #/(when #>cdr `((,#>cdr ,#>car)))
                 pairs)
         (mapcan #/(when #>cdr (list #>cdr))
                 pairs))))))
  
(defmacro is (name &rest args)
  (assert (and 'is (<check-name> name)))
  #{let* ((normal (symbolp name))
          (pre-args (if normal nil (cdr name)))
          (name (if normal name (first name)))
          (fn-name (<get-is-symbol> name)))
  (cond (args
          `(,fn-name ,@(when args (list (first args))) ,@pre-args ,@(cdr args)))
        ((null pre-args)
          `(function ,fn-name))
        (t 
          #{with-gensyms (x r)
          #{multiple-value-bind (pre-args bindings tmpvars) (<parse> pre-args)
          #{let ((code `(lambda (,x &rest ,r) 
                         (apply #',fn-name ,x ,@pre-args ,r))))
          (if (null bindings)
            code
            `(let ,bindings (isolate-without ,tmpvars ,code))))))

(defun <make-isnt-code> (name &rest args)
  (assert (and 'is-not (<check-name> name)))
  #{let* ((normal (symbolp name))
          (pre-args (if normal nil (cdr name)))
          (name (if normal name (first name))))
  (cond (args `(not (,(<get-is-symbol> name) 
                     ,@(when args (list (first args))) ,@pre-args ,@(cdr args))))
        ((null pre-args)
          `(function ,(<get-isnt-symbol> name)))
        (t 
          #{with-gensyms (x r)
          #{multiple-value-bind (pre-args bindings tmpvars) (<parse> pre-args)
          #{let ((code `(lambda (,x &rest ,r) 
                         (not (apply #',(<get-is-symbol> name) ,x ,@pre-args ,r)))))
          (if (null bindings)
            code
            `(let ,bindings (isolate-without ,tmpvars ,code))))))
          
(defmacro isnt (name &rest args)
  (assert (and 'isnt (<check-name> name)))
  (<make-isnt-code> name args))
(defmacro is-not (name &rest args)
  (assert (and 'is-not (<check-name> name)))
  (<make-isnt-code> name args))
  

(defmacro are (name &rest operands)
  (assert (and 'are (<check-name> name)))
  (if operands
    (if (symbolp name)
      `(and ,@(mapcar #/`(is ,name ,_)
                      operands))
      #{multiple-value-bind (pre-args bindings) (<parse> (cdr name))
      #{let ((internal (<get-is-symbol> (first name))))
      `(let ,bindings (and ,@(mapcar #/`(,internal ,_ ,@pre-args)
                                     operands))))
    (if (symbolp name)
      `(lambda (&rest xs) (every (is ,name) xs))
      `(let ((f (is ,name)))
        (lambda (&rest xs) (every f xs))))))

(defmacro arent (name &rest operands)
  (assert (and 'are (<check-name> name)))
  (if operands
    (if (symbolp name)
      `(not (or ,@(mapcar #/`(is ,name ,_)
                          operands)))
      #{multiple-value-bind (pre-args bindings) (<parse> (cdr name))
      #{let ((internal (<get-is-symbol> (first name))))
      `(let ,bindings (not (or ,@(mapcar #/`(,internal ,_ ,@pre-args)
                                         operands)))))
    (if (symbolp name)
      `(lambda (&rest xs) (every (is-not ,name) xs))
      `(let ((f (is-not ,name)))
        (lambda (&rest xs) (every f xs))))))



;;;;;;;;;;;;

