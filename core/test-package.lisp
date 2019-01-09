;; -*- coding: utf-8 -*-

(oleo.core.init:define-package :oleo.core.test ()
  (:import/export :oleo.core.reader)
  (:use :cl :alexandria)

  (:export
   #:*ignore-testing*
   #:*force-testing* 
   #:enable-testing #:disable-testing
   #:testing

   ;; cl-annot annotations
   #:test #:verify #:todo
   #:eval-when-test
   
   ;;;;;
   #:todo..

   #:check-unit #:check-unit*
   #:check-units #:check-units*
   
   #:check-assert #:check-assert*

   #:precond 

   #:has-errors 
   #:has-no-errors
   #:protected-multiple-value-list #:protected-multiple-value-list*)

  )
