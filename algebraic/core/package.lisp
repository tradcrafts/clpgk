;; -*- coding: utf-8 -*-

;; Haskell的な、代数的データ型の実現
;; define-data,define-newtypeは、Qiとのシームレスな接続を実現
;; define-internal-data,define-internal-newtypeは、CL内のみの使用を前提とした効率的な実装

(oleo.base:define-package :oleo.algebraic.core ()
  (:use :cl :oleo.base)
  (:import/export :oleo.algebraic.xdata)
  (:export

   #:applicable
   #:applied #:applied* #:rapplied #:rapplied*
   #:applicate

   #:defun.. #:lambda.. #:named-lambda.. #:flet.. #:labels..
   #:lambda-bind.. #:lambda/bind..
   
   #:== #:!=
   #:deconstruct #:deconstructable-p
   #:unlift #:unliftable-p
   #:relift #:relifttable-p
   #:copy-data #:copy-data*
   #:define-data
   #:define-newtype
   #:define-local-data
   #:define-local-newtype


   ;#:define-internal-data
   ;#:define-internal-newtype
   
   ;#:_maybe #:_just #:_nothing
   ;#:_either #:_left #:_right

   #:maybe #:just #:nothing
   #:either #:left #:right

   #:*tuple-max*
   #:p3 #:p4 #:p5 #:p6 #:p7 #:p8 #:p9 #:p10 #:p11 #:p12 #:p13 #:p14
   #:p15 #:p16 #:p17 #:p18 #:p19 #:p20
   ))

