;; -*- coding: utf-8 -*-

(oleo.core:oleo-core-header)

(oleo.core:define-package :oleo.base.ffi* (:oleo.base.ffi)
  (:use :cl)
  (:import/export :oleo.base.iterate*)

  (:export
   #:define-c-library
   #:use-c ;; OBSOLETE
   #:close-c-library #:open-c-library
   #:define-c-functions

   #:c-call
   
   #:garbage-free
   #:with-garbage
   #:as-garbage #:cancel-garbage
   ))

(in-package :oleo.base.ffi)

(defmacro c-call (return-type c-function-name &rest type-and-value-pairs)
  `(cffi:foreign-funcall ,c-function-name ,@type-and-value-pairs ,return-type))

(defun <sysdir> (system-ident)
  (let ((path (asdf:system-definition-pathname system-ident)))
  (string-concat
   #+(OR windows win32) (string-concat (pathname-device path) ":/")
   (directory-namestring path))))

(defmacro define-c-library (system-ident &key soname (path :asdf))
  (unless soname
    (setq soname (string-downcase (ensure-string system-ident))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cffi:define-foreign-library ,system-ident
     ((:not :CL-UNWIRED-STANDALONE-APPLICATION)
      (:default ,soname)
      :search-path ,(cond ((eql path :asdf)
                            (<sysdir> system-ident))
                          ((and (listp path)
                                (eql (car path) :asdf))
                            (<sysdir> (second path)))
                          (t
                            path)))
     (t
      (:default ,soname)
      :search-path ()))))
  

(defmacro use-c (system-ident)
  `(cffi:use-foreign-library ,system-ident))

(defmacro open-c-library (system-ident)
  `(cffi:load-foreign-library ',system-ident))

(defmacro close-c-library (system-ident)
  `(cffi:close-foreign-library ',system-ident))

(defconstant +<pkg>+ *package*)
  
(defvar *<arg-syms>*
  (&mapcar (lambda (i) (intern (format nil "X~D" i) +<pkg>+))
           (&iota -1 :start 1)))


(defun <check/c-functions> (clauses)
  (flet ((valid-cfun (x) (and (stringp x)
                              (>= (length x) 1))))
    (do-unify clauses
      (:EACH (:AND ((:type symbol)
                    (:OR (:-> #'valid-cfun)
                         ((:-> #'valid-cfun) (:type symbol)))
                    . ?r)
                   (:FOR r (:EACH (:or (:type symbol)
                                       (? (:type symbol))))))))))

(defmacro define-c-functions (system-ident &body decls)
  (assert (<check/c-functions> decls))
  (flet ((localfunc? (s)  (eql (char s 0) #\%))
         (local-name (s) (string-concat "CL_" s))
         (get-pure-name (s)  (subseq s 1)))
    (let (calls)
      (dolist (d decls)
        (let ((ret (first d))
              (fn  (second d))             
              (args (mapcar (lambda (a x)
                              (if (consp a)
                                a
                                (list x a)))
                            (cddr d)
                            (&strict (&take (length (cddr d)) *<arg-syms>*)))))

          ;; %funcname --> CL_funcname 
          (cond ((and (atom fn) (localfunc? fn))
                  (let ((pure-name (get-pure-name fn)))
                    (setf fn (list (local-name pure-name)
                                   (cffi:translate-name-from-foreign pure-name *package*)))))
                ((and (consp fn) (localfunc? (first fn)))
                  (setf fn (list (local-name (get-pure-name (first fn)))
                                 (second fn))))
                (t
                  (setf fn (list fn))
                  ))
          
          (push `(cffi:defcfun (,@fn :library ,system-ident) ,ret ,@args)
                calls)))
      `(eval-when (:load-toplevel :execute) ,@(nreverse calls)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `GARBAGE' Manipulators ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *<garbages>*)

(defun as-garbage (object)
  (pushnew object *<garbages>*)
  object)

(defun cancel-garbage (object)
  (deletef *<garbages>* object)
  object)


(defmacro with-garbage ((&key (manage-by 'list)) &body body)
  `(let ((*<garbages>* nil))
     (unwind-protect
       (progn ,@body)
       (dolist (g *<garbages>*)
         (garbage-free g))
       )))


(defgeneric garbage-free (object))

(defmethod garbage-free (x)
  (cond ((typep x 'cffi:foreign-pointer)
          (cffi:foreign-free x))
        (t (error "unknown garbage: ~D" x))))


;;;;;;;;;;;;;;
#Comment

(<check/define-class>
 '((f "aaa" (a c) d)
   (a ("%d" v) c))
 )



(fmakunbound 'garbage-free)

(defmethod garbage-free ((xs list


(type-of (cffi:foreign-alloc :double))

(as-garbage x)

 

(unwind-protect (values 'hello 0) (print 'a) (print 'b)) 
3



L


(with-garbage (:manage-by hash-table)
  CODE)


(c-functions (a b c d e) (x y z foo))





