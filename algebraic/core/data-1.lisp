;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.base:clpgk-base-header)
(in-package :clpgk.algebraic.core)

(defgeneric deconstructable-p (obj))
(defgeneric deconstruct (obj &optional output)) ;==> values or list or vector
(defgeneric unliftable-p (obj))
(defgeneric unlift (obj))
(defgeneric reliftable-p (obj)) 
(defgeneric relift (obj function)) 
(defgeneric copy-data (obj &optional copy-datum-recursively-p))
(defun copy-data* (obj) (copy-data obj T))
(defgeneric == (obj-1 obj-2))
(defun != (obj-1 obj-2)
  (not (== obj-1 obj-2)))

(defmethod copy-data (x &optional recursive?)
  (declare (ignore recursive?))
  x)

(defun <copy-data-for-sv> (sv recursive?)
  (declare (type simple-vector sv))
  (let ((result (copy-seq sv)))
    (if recursive?
      (dotimes (i (length sv) result)
        (setf (svref sv i) (copy-data (svref sv i) t)))
      result)))

(defmethod copy-data ((vec vector) &optional recursive?)
  (if (simple-vector-p vec)
    (<copy-data-for-sv> vec recursive?)
    (let ((result (copy-seq vec)))
      (when (and recursive?
                 (not (or (stringp vec) (bit-vector-p vec))))
        (dotimes (i (length vec))
          (setf (aref vec i) (copy-data (aref vec i) t))))
      result)))


(defmethod copy-data ((xs list) &optional recursive?)
  (cond ((null xs) nil)
        ((proper-list-p xs)
          (let (result)
            (dolist (x xs (nreverse result))
              (push (if recursive? (copy-data x t) x) result))))
        ((circular-list-p xs)
          (let ((result (cons (car xs) nil)))
            (do ((head xs)
                 (xs (cdr xs) (cdr xs)))
                ((eq head xs)
                 (setf result (nreverse result)
                       (cdr (last result)) result)
                 result)
              (push (if recursive? (copy-data (car xs) t) (car xs)) result))))
        (t (do* ((result (cons (car xs) nil))
                 (cur result)
                 (c (cdr xs) (cdr c)))
                ((atom c)
                 (setf (cdr cur) (if recursive? (copy-data c t) c))
                 result)
             (let ((new (cons (if recursive? (copy-data (car c) t) (car c))
                              nil)))
               (setf (cdr cur) new
                     cur new))))))
                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Algebraic Data Type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar *private-ids-for-data* nil)
;; (defun private-ids-for-data (n)
;;   (let ((cur-len (length *private-ids-for-data*)))
;;     (if (<= n cur-len)
;;       (subseq  *private-ids-for-data* 0 n)
;;       (copy-list
;;        (nconcf *private-ids-for-data*
;;                (freplicate (- n cur-len) #'gensym))))))
;; (defmacro data-slot-value (i obj)
;;   (let ((cur-len (length *private-ids-for-data*)))
;;     (when (>= i cur-len)
;;       (nconcf *private-ids-for-data*
;;               (freplicate (1+ (- i cur-len)) #'gensym))))
;;   `(slot-value ,obj ',(nth i *private-ids-for-data*)))

(defmacro <take> (x) `(xtuple-snd ,x))

@eval-when-compile
(defmacro <defaccessor> (name op) 
  `(defmacro ,name (x) `(,',op (<take> ,x))))

(defmacro <0> (x) `(<take> ,x))
(<defaccessor> <1> first) 
(<defaccessor> <2> second)
(<defaccessor> <3> third)
(<defaccessor> <4> fourth)
(<defaccessor> <5> fifth)
(<defaccessor> <6> sixth)
(<defaccessor> <7> seventh)
(<defaccessor> <8> eighth)
(<defaccessor> <9> ninth)
(<defaccessor> <10> tenth)

(defparameter *<accessor-ids>* '(<1> <2> <3> <4> <5> <6> <7> <8> <9> <10>))
(defun get-accessor-ids (n)
  (if (eq n 1)
    (list '<0>)
    (let ((cur-len (length *<accessor-ids>*)))
      (if (<= n cur-len)
        (subseq  *<accessor-ids>* 0 n)
        (copy-list
         (nconcf *<accessor-ids>*
                 (let (tmp)
                   (dotimes (i (- n cur-len) (nreverse tmp))
                     (let ((name (gensym)))
                       (push name tmp)
                       (eval `(defmacro ,name (x) 
                               `(nth ,',(+ cur-len i) (<take> ,x)))))))))))))

(defparameter *<accessor-ids>* 'nil)
(defun get-accessor-ids (n)
  (if (eq n 1)
    (list '<0>)
    (let ((cur-len (length *<accessor-ids>*)))
      (if (<= n cur-len)
        (subseq  *<accessor-ids>* 0 n)
        (copy-list
         (nconcf *<accessor-ids>*
                 (let (tmp)
                   (dotimes (i (- n cur-len) (nreverse tmp))
                     (let ((name (gensym "ACCESS")))
                       (push name tmp)
                       (eval `(defmacro ,name (x)
                               `(svref (<take> ,x) ,',(+ cur-len i) ))))))))))))



;; global: sym -> CLPGK.MSPACE::|@sym| , QSPACE::|sym#|
;; local: sym-> @@sym , <sym#>

(defun <make-xi-ident> (sym &optional local?)
  (let* ((name (symbol-name sym))
         (xi-base-name (string-downcase sym))
         (q-pkg (memoized (find-package :clpgk.mspace))))
    (if local?
      (values (intern (string-concat "@@" xi-base-name) *package*)
              (intern (string-concat "<" xi-base-name "#>") *package*))
      (values (intern (string-concat "@" xi-base-name) q-pkg)
              (intern (string-concat xi-base-name "#") q-pkg)))))


(defclass DATATUPLE (XTUPLE) ())

(defmethod print-object ((x DATATUPLE) stream)
  (let* ((type (type-of x))
         (arity (get type 'clpgk.algebraic.xdata::|%data_arity%|))
         (data (xtuple-snd x)))
    (case arity
      (0 (format stream "<~A>" type))
      (1 (format stream "<~A ~A>" type data))
      (t (format stream "(<~A>~{ ~A~})" type (coerce data 'list))))))


(defun <error/deconstruct> (illegal-opt)
  (error "DECONSTRUCT: unknown output option: ~A" illegal-opt))

(defun <define-data> (name decls local?)
  (do-unify decls
    (:EACH (:OR {symbol}
                (:APPEND ({symbol})
                         (:EACH+ (:OR {symbol}
                                      ({symbol} {symbol}))))))
    :on-failure (error "DEFINE-DATA: wrong declaration(s). ~D" decls))
  #{let (main
         (superclass (unless (and (= 1 (length decls))
                                  (eq name (ensure-car (car decls))))
                       (list name))))
  #{macrolet ((add (&rest xs) `(progn ,@(mapcar #/`(push ,_ main) xs))))
  #{dolist (d decls `(eval-when (:compile-toplevel :load-toplevel :execute)
                      ,@ (when superclass
                           `((defclass ,name (DATATUPLE) ())
                             (define-unification (,name t) `(typep ,this ',',name))
                             ))
                      ,@(nreverse main)
                      ',name))
  (cond ((symbolp d)
          (multiple-value-bind (q-ident q-tag) (<make-xi-ident> d local?)
            (add `(defclass ,d ,(if superclass superclass '(DATATUPLE)) ())
                 `(setf (get ',q-tag 'clpgk.algebraic.xdata::|%classdata%|) ',d)
                 `(setf (get ',q-ident 'clpgk.algebraic.xdata::|%data_arity%|) 0)
                 `(setf (get ',q-ident 'clpgk.algebraic.xdata::|%data_tag%|) ',q-tag)
                 `(setf (get ',d 'clpgk.algebraic.xdata::|%data_arity%|) 0) ;;
                 `(setf (get ',d 'clpgk.algebraic.xdata::|%data_tag%|) ',q-tag) ;;
                 `(defun ,d () (memoized (xtuple ',q-tag nil ',d)))
                 `(define-symbol-macro ,d (,d)))))
        ((consp d)
          #{bind ((c (first d))
                  (arity (length (cdr d)))
                  (single (eql 1 arity))
                  (ids (get-accessor-ids arity))
                  ((:values q-ident q-tag) (<make-xi-ident> c local?)))
          (add `(defclass ,c ,(if superclass superclass '(DATATUPLE)) ()))
          (when (eq 1 arity)
            (add `(defmethod unliftable-p ((a ,c)) (declare (ignore a)) t)
                 `(defmethod unlift ((a ,c)) (,(car ids) a))
                 ))
          (when (some #'consp (cdr d))
            (dolists (id ids clause (cdr d))
              (when (consp clause)
                #{let ((accessor (second clause)))
                (add `(define-compiler-macro ,accessor (x) 
                       `(,',id ,x))
                     `(defun ,accessor (x)
                       (,id x))
                     `(defun (setf ,accessor) (val x)
                       (setf (,id x) val))
                     `(define-compiler-macro (setf ,accessor) (val x)
                       `(setf (,',id ,x) ,val))))))
          (add `(defmethod deconstructable-p ((a ,c)) (declare (ignore a)) t)
               `(defmethod deconstruct ((a ,c) &optional (output :values))
                 ,(if (eq 1 arity) 
                    `(let ((x (xtuple-snd a)))
                       (case output (:values x) (:list (list x)) (:vector (vector x))
                             (t (<error/deconstruct> output))))
                    (let ((refs (mapl (lambda (xs) (setf (car xs) `(svref sv ,(car xs))))
                                      (iota arity))))
                      `(let ((sv (xtuple-snd a)))
                         (declare (type simple-vector sv))
                         (case output (:values (values ,@refs)) (:list (list ,@refs)) (:vector (copy-seq sv))
                               (t (<error/deconstruct> output)))))))
               
               `(defmethod == ((a ,c) (b ,c))
                 (and ,@(mapcar #/`(== (,_ a) (,_ b)) ids)))
               `(define-unification (,c t) `(typep ,this ',',c)
                 (:enum (:whole ,@(mapcar #/``(,',_ ,this) ids))))

               `(setf (get ',q-tag 'clpgk.algebraic.xdata::|%classdata%|) ',c)
               `(setf (get ',q-ident 'clpgk.algebraic.xdata::|%data_arity%|) ,arity)
               `(setf (get ',q-ident 'clpgk.algebraic.xdata::|%data_tag%|) ',q-tag)
               `(setf (get ',c 'clpgk.algebraic.xdata::|%data_arity%|) ,arity)
               `(setf (get ',c 'clpgk.algebraic.xdata::|%data_tag%|) ',q-tag)

               
               (if single
                 ;; arity == 1 の場合
                 `(defun ,c (X &key source (mask '|invalid|) update)
                    (cond 
                      (source
                        (assert (and ',c (or (eql X mask)
                                             (typep X ',(ensure-car (cadr d))))))
                        (if update
                          (progn (unless (eql X mask) 
                                   (setf (,(first ids) source) X))
                                 source)
                          (xtuple ',q-tag 
                                  (if (eql X mask) 
                                    (,(first ids) source)
                                    X)
                                  ',c)))
                      (update (warn "update"))
                      (t
                        (assert (and ',c (typep X ',(ensure-car (cadr d)))))
                        (xtuple ',q-tag X ',c))))
                 ;; arity > 1の場合
                 `(defun ,c (,@ids &key source (mask '|invalid|) update)
                    (cond 
                      (source
                        (assert (and ',c
                                     ,@(mapcan (lambda (id d &aux (type (ensure-car d)))
                                                 (unless (eq type T)
                                                   `((or (eql ,id mask)
                                                         (typep ,id ',type)))))
                                               ids (cdr d))))
                        (if update
                          (progn ,@(mapcar #/`(unless (eql ,_ mask) 
                                                (setf (,_ source) ,_))
                                           ids)
                                 source)
                          (xtuple ',q-tag 
                                  (vector ,@(mapcar #/`(if (eql ,_ mask) 
                                                         (,_ source)
                                                         ,_)
                                                    ids))
                                  ',c)))
                      (update (warn "update"))
                      (t
                        (assert (and ',c
                                     ,@(mapcar (lambda (id d)
                                                 `(typep ,id ',(ensure-car d)))
                                               ids (cdr d))))
                        (xtuple ',q-tag (vector ,@ids) ',c)))))

               `(defmethod copy-data ((data ,c) &optional recursive?)
                  (xtuple ',q-tag
                          ,(if single
                             '(copy-data (xtuple-snd data) recursive?)
                             '(<copy-data-for-sv> (xtuple-snd data) recursive?))
                          ',c)))

          (when single
            (add
             `(defmethod reliftable-p ((a ,c)) (declare (ignore a)) t)
             `(defmethod relift ((a ,c) f) (,c (funcall f (xtuple-snd a))))
             ))
                 
          )))

(defmacro define-global-data (name &rest decls)
  (<define-data> name decls nil))
(defmacro define-local-data (name &rest decls)
  (<define-data> name decls t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Newtype ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun <define-newtype> (def-op name type accessor)
  (if accessor
    `(,def-op ,name (,name (,type ,accessor)))
    `(,def-op ,name (,name ,type))))

(defmacro define-global-newtype (name type &key accessor)
  (<define-newtype> 'define-data name type accessor))

(defmacro define-local-newtype (name type &key accessor)
  (<define-newtype> 'define-local-data name type accessor))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Internal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *<slot-ids>* nil)
(defun <get-slot-ids> (n)
  (let ((cur-len (length *<slot-ids>*)))
    (if (<= n cur-len)
      (subseq  *<slot-ids>* 0 n)
      (copy-list
       (nconcf *<slot-ids>*
               (freplicate (- n cur-len) #'gensym))))))

(defmacro data-slot-value (i obj)
  (let ((cur-len (length *<slot-ids>*)))
    (when (>= i cur-len)
      (nconcf *<slot-ids>*
              (freplicate (1+ (- i cur-len)) #'gensym))))
  `(slot-value ,obj ',(nth i *<slot-ids>*)))

;; OBSOLETE
'(defmacro define-internal-data (name &rest decls)
  (do-unify decls
    (:EACH (:OR {symbol}
                (:APPEND ({symbol})
                         (:EACH+ (:OR {symbol}
                                      ({symbol} {symbol}))))))
    :on-failure (error "DEFINE-DATA: wrong declaration(s). ~D" decls))
  #{let (main
         (superclass (unless (and (= 1 (length decls))
                                  (eq name (ensure-car (car decls))))
                       (list name))))
  #{macrolet ((add (&rest xs) `(progn ,@(mapcar #/`(push ,_ main) xs))))
  #{dolist (d decls `(progn ;(eval-when (:load-toplevel :execute)
                      ,@ (when superclass
                           `((defclass ,name () ())
                             (define-unification (,name t) `(typep ,this ',',name))
                             ))
                      ,@(nreverse main)
                      ',name))
  (cond ((symbolp d)
          (add `(defclass ,d ,superclass ())
               `(defmethod print-object ((obj ,d) stream)
                 (format stream "#<data=~W>" ',d))
                                        ;`(defconstant ,d (make-instance ',d))))
               `(defun ,d () (memoized (make-instance ',d)))
               `(define-symbol-macro ,d (,d))))
        ((consp d)
          #{let* ((c (first d))
                  (tmp)
                  (n (length (cdr d)))
                  (ids (<get-slot-ids> n)))
          (dolists (term (cdr d) id ids)
            (cond ((symbolp term)
                    (push `(,id :initarg ,id) tmp))
                  (t
                    (push `(,id :accessor ,(second term) :initarg ,id) tmp))))
          (add `(defclass ,c ,superclass ,(nreverse tmp)))
          (when (eql 1 (length ids))
            (add `(defmethod unliftable-p ((a ,c)) (declare (ignore a)) t)
                 `(defmethod unlift ((a ,c)) (slot-value a ',(car ids)))))
            
          (when (some #'consp (cdr d))
            (dolists (id ids clause (cdr d))
              (when (consp clause)
                #{let ((accessor (second clause)))
                (add `(define-compiler-macro ,accessor (x) 
                       `(slot-value ,x ',',id))
                     `(defun ,accessor (x)
                       (slot-value x ',id))
                     `(defun (setf ,accessor) (val x)
                       (setf (slot-value x ',id) val))
                     `(define-compiler-macro (setf ,accessor) (val x)
                       `(setf (slot-value ,x ',',id) ,val))))))

          (add `(defmethod deconstructable-p ((a ,c)) (declare (ignore a)) t)
               `(defmethod deconstruct ((a ,c) dododo dodo dood odo)
                 (list ,@(mapcar #/`(slot-value a ',_) ids)))
               `(defmethod == ((a ,c) (b ,c))
                 (and ,@(mapcar #/`(== (slot-value a ',_) (slot-value b ',_)) ids)))
               `(define-unification (,c t) `(typep ,this ',',c)
                 (:enum (:whole ,@(mapcar #/``(slot-value ,this ',',_) ids))))
               `(define-match-slots ,c ,@ids)
               `(defmethod print-object ((obj ,c) stream)
                 (format stream "#<data=~A~W>" 
                  ',c 
                  (list ,@(mapcar #/`(slot-value obj ',_) ids))))
               `(proclaim '(inline ,c))
               `(defun ,c (,@ids &key source (mask '|invalid|) update)
                 (cond 
                   (source
                     (assert (and ',c
                                  ,@(mapcar (lambda (id d)
                                              ` (or (eql ,id mask)
                                                    (typep ,id ',(ensure-car d))))
                                            ids (cdr d))))
                     (if update
                       (progn ,@(mapcar #/`(unless (eql ,_ mask) 
                                            (setf (slot-value source ',_) ,_))
                                        ids)
                              source)
                       (construct-instance ,c 
                                           ,@(mapcan #/`(,_ (if (eql ,_ mask) 
                                                              (slot-value source ',_)
                                                              ,_))
                                                     ids))))
                   (update (warn "update"))
                   (t
                     (assert (and ',c
                                  ,@(mapcar (lambda (id d)
                                              `(typep ,id ',(ensure-car d)))
                                            ids (cdr d))))
                     (construct-instance ,c ,@(mapcan #/`(,_ ,_) ids)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Internal Newtype ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-internal-newtype (name type &key accessor)
  (if accessor
    `(define-internal-data ,name (,name (,type ,accessor)))
    `(define-internal-data ,name (,name ,type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-unification (unlift t) `(unliftable-p ,this)
  (:enum
   (:whole `(unlift ,this))))
(define-unification (deconstruct t) `(deconstructable-p ,this)
  (:enum
   (:whole `(deconstruct ,this))))


;;;;;;;;;;;;;;;;;;;;;;;;;; Method Imprementation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod == (a b)
  (eql a b))
(defmethod == ((a number) (b number))
  (equal a b))
(defmethod == ((a vector) (b vector))
  (or (eq a b)
      #{let ((n (length a)))
      (and (= n (length b))
           (dotimes (i n t)
             (unless (== (aref a i) (aref b i))
               (return nil))))))
(defmethod == ((a string) b)
  (and (stringp b) (equal a b)))
(defmethod == (a (b string))
  (and (stringp a) (equal a b)))

(defmethod == ((a list) (b list))
  (or (eq a b)
      (and (= (length a) (length b))
           (do ((xs a (cdr xs))
                (ys b (cdr ys)))
               ((null xs)
                t)
             (unless (== (car xs) (car ys))
               (return nil))))))
          
(defmethod deconstructable-p (obj) (declare (ignore obj))
  nil)

(defmethod deconstruct (obj &optional output)
  (declare (ignore output))
  (error "deconstruct: data type ~D is not allowed" (type-of obj)))

(defmethod unliftable-p (obj) (declare (ignore obj))
  nil)
(defmethod unliftable-p ((a list))
  (eql 1 (length a)))
(defmethod unliftable-p ((a vector))
  (eql 1 (length a)))

(defmethod unlift (obj)
  (error "unlift: data type ~D is not allowed" (type-of obj)))
(defmethod unlift ((a list))
  (if (eql 1 (length a))
    (car a)
    (error "unlift: cannot unlift list ~D" a)))
(defmethod unlift ((a vector))
  (if (eql 1 (length a))
    (aref a 0)
    (error "unlift: cannot unlift vector ~D" a)))
