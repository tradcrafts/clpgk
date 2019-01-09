;; -*- coding: utf-8 -*-

(oleo.core:oleo-core-header)

;(defpkg :j-more-unify-match-defs ())

;(in-package :j-more-unify-match-defs)

(in-package :oleo.base.match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MATCH MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match-macro :case (&rest clauses)
  (unless (do-unify clauses (:each (? (:-> :type proper-list))))
    (error "MATCH: :CASE : wrong clauses ~D" clauses))
  (reduce @\ab `(:IF ,(car a) (:AND ,@(cdr a)) ,b)
          clauses
          :from-end t :initial-value '(:NEVER)))

(define-match-macro :if-not (test-clause then-clause 
                                         &optional (else-clause '(:never)))
  `(:IF (:NOT ,test-clause) ,then-clause ,else-clause))

(define-match-macro :then (test-clause &rest then-clauses)
  `(:IF ,test-clause (:AND ,@then-clauses) (:here t)))

(define-match-macro :then-not (test-clause &rest then-clauses)
  `(:IF-NOT ,test-clause (:AND ,@then-clauses) (:here t)))



(defun <*-case> (opname if-op *-if-op x options clauses)
  (do-unify clauses (:EACH (:-> (consp)))
            :on-failure (error "~D: illegal clause(s) ~D" opname clauses ))
  (reduce @\ab (acase (car a) 
                ((t otherwise)
                 `(,if-op t 
                   (progn ,@(cdr a))
                   ,b))
                (t 
                 `(,*-if-op (,x ,it ,@options)
                         (progn ,@(cdr a))
                         ,b)))
          clauses
          :from-end t :initial-value nil))

(defun <get-enumerated-flag> (xs)
  (apply (lambda (&key enumerated &allow-other-keys) enumerated)
         xs))

(defmacro OLEO.BASE.UNIFY:u-case (x &body clauses)
  (with-gensyms (tmp)
    `(let ((,tmp ,x))
      ,(<*-case> 'u-case 'if 'do-unify-if tmp nil clauses))))
(defmacro OLEO.BASE.MATCH:m-case (x &body clauses)
  (with-gensyms (tmp)
    `(let ((,tmp ,x))
      ,(<*-case> 'm-case 'if 'do-match-if tmp nil clauses))))

(defmacro OLEO.BASE.UNIFY:u-gcase (x &body clauses)
  (with-gensyms (tmp)
    `(let ((,tmp ,x))
      ,(<*-case> 'u-gcase 'gif 'do-unify-gif tmp nil clauses))))
(defmacro OLEO.BASE.MATCH:m-gcase (x &body clauses)
  (with-gensyms (tmp)
    `(let ((,tmp ,x))
      ,(<*-case> 'm-gcase 'gif 'do-match-gif tmp nil clauses))))

(defun <*-case/w> (macroname if-op *-if-op x options clauses)
  (cond ((<get-enumerated-flag> options)
          (unless (proper-list-p x)
            (error "~D: ENUMERATED option is set. but ~D is not a proper list." 
                   macroname x))
          #{let ((tmps (freplicate (length x) #'gensym)))
          `(let ,(zip tmps x)
            ,(<*-case> macroname if-op *-if-op tmps options clauses)))
        (t (with-gensyms (tmp)
             `(let ((,tmp ,x))
               ,(<*-case> macroname if-op *-if-op tmp options clauses))))))

(defmacro OLEO.BASE.UNIFY:u-case/w ((x &rest options) &body clauses)
  (<*-case/w> 'u-case/w 'if 'do-unify-if x options clauses))
(defmacro OLEO.BASE.MATCH:m-case/w ((x &rest options) &body clauses)
  (<*-case/w> 'm-case/w 'if 'do-match-if x options clauses))

(defmacro OLEO.BASE.UNIFY:u-gcase/w ((x &rest options) &body clauses)
  (<*-case/w> 'u-case/w 'gif 'do-unify-gif x options clauses))
(defmacro OLEO.BASE.MATCH:m-gcase/w ((x &rest options) &body clauses)
  (<*-case/w> 'm-case/w 'gif 'do-match-gif x options clauses))


(defmacro OLEO.BASE.UNIFY:u-acase (x &body clauses)
  `(let ((it ,x))
    (u-case it ,@clauses)))
(defmacro OLEO.BASE.MATCH:m-acase (x &body clauses)
  `(let ((it ,x))
    (m-case it ,@clauses)))

(defmacro OLEO.BASE.UNIFY:u-gacase (x &body clauses)
  `(let ((it ,x))
    (u-gcase it ,@clauses)))
(defmacro OLEO.BASE.MATCH:m-gacase (x &body clauses)
  `(let ((it ,x))
    (m-gcase it ,@clauses)))

(defun <*-acase/w> (macroname *-case-op x options clauses)
  (when (<get-enumerated-flag> options)
    (error "~D: ENUMERATED option is not NIL." macroname))
  `(let ((it ,x))
    (,*-case-op (it ,@options) ,@clauses)))

(defmacro OLEO.BASE.UNIFY:u-acase/w ((x &rest options) &body clauses)
  (<*-acase/w> 'u-acase/w 'u-case/w x options clauses))
(defmacro OLEO.BASE.MATCH:m-acase/w ((x &rest options) &body clauses)
  (<*-acase/w> 'm-acase/w 'm-case/w x options clauses))

(defmacro OLEO.BASE.UNIFY:u-gacase/w ((x &rest options) &body clauses)
  (<*-acase/w> 'u-gacase/w 'u-gcase/w x options clauses))
(defmacro OLEO.BASE.MATCH:m-gacase/w ((x &rest options) &body clauses)
  (<*-acase/w> 'm-gacase/w 'm-gcase/w x options clauses))


(defun <*-cond> (macroname if-op *-if-op clauses)
  (flet ((chk (_) (and #>consp (or (consp #>car)
                                   (member #>car '(t otherwise))))))
    (unless (every #'chk clauses)
      (error "~D: illegal clause: ~D" macroname (find-if-not #'chk clauses))))
  (reduce @\ab`(,(if (consp (car a))
                    *-if-op
                    if-op)
               ,(car a)
               (progn ,@(cdr a))
               ,b)
          clauses
          :from-end t :initial-value nil))

(defmacro OLEO.BASE.UNIFY:u-cond (&body clauses)
  (<*-cond> 'u-cond 'if 'do-unify-if clauses))
(defmacro OLEO.BASE.MATCH:m-cond (&body clauses)
  (<*-cond> 'm-cond 'if 'do-match-if clauses))

(defmacro OLEO.BASE.UNIFY:u-gcond (&body clauses)
  (<*-cond> 'u-gcond 'gif 'do-unify-gif clauses))
(defmacro OLEO.BASE.MATCH:m-gcond (&body clauses)
  (<*-cond> 'm-gcond 'gif 'do-match-gif clauses))


;; (defun <check-*-bind-clause> (opname pairs chk)
;;   #{let (global-options)
;;   (awhen (position-if #'keywordp pairs)
;;     (setq global-options (nthcdr it pairs)
;;           pairs (subseq pairs 0 it)))
;;   (unless (every #/(and #>proper-list-p (funcall chk 2 #>length))
;;                  pairs)
;;     (error "~D : illegal clause(s): ~D" opname pairs))
;;   (values pairs global-options))

(defun <*-bind> (macroname *-if-op pairs body)
  (unless (every #/(and #>consp (= 2 #>length))
                 pairs)
    (error "~D: illegal clause(s): ~D" macroname pairs))
  `(,*-if-op (,(mapcar #'second pairs)
                 ,(mapcar #'first pairs)
                 :enumerated t)
    (progn ,@body)
    (error "*-BIND: ~D failed" ',pairs)))

(defmacro OLEO.BASE.UNIFY:u-bind ((&rest pairs) &body body)
  (<*-bind> 'u-bind 'do-unify-if pairs body))
(defmacro OLEO.BASE.MATCH:m-bind ((&rest pairs) &body body)
  (<*-bind> 'm-bind 'do-match-if pairs body))



(defun <*-bind*> (macroname *-if-op pairs body)
  (unless (every #/(and #>consp (<= 2 #>length))
                 pairs)
    (error "~D: illegal clause(s): ~D" macroname pairs))
  (reduce @\ab `(,*-if-op (,(second a)
                             ,(first a)
                             ,@(cddr a))
                ,b
                (error "*-BIND*: ~D failed" ',a))
          pairs
          :from-end t :initial-value `(progn ,@body)))

(defmacro OLEO.BASE.UNIFY:u-bind* ((&rest pairs) &body body)
  (<*-bind*> 'u-bind* 'do-unify-if pairs body))
(defmacro OLEO.BASE.MATCH:m-bind* ((&rest pairs) &body body)
  (<*-bind*> 'm-bind* 'do-match-if pairs body))
