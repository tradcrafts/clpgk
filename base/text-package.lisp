;; -*- coding: utf-8 -*-

(oleo.core:oleo-core-header)
 
(oleo.core:define-package :oleo.base.text* (:oleo.base.text)
  (:use :cl :cl-ppcre)
  (:import/export :oleo.base.guard*)

  (:export
   #:=~ #:!~

   #:$ #:text-compile-literal

   
   #:text-scan
   #:text-matches #:text-unmatches
   #:text-replace
   #:text-split
   #:regex
   
   #:do-text-matches #:do-text-unmatches
   #:text-all-matches #:text-all-matches-as-strings
   #:text-all-unmatches #:text-all-unmatches-as-strings
   )
  )

(in-package :oleo.base.text)
