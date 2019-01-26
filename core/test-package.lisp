;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.core.init:define-package :clpgk.core.test ()
  (:import/export :clpgk.core.reader)
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
