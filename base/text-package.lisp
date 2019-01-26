;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.core:clpgk-core-header)
 
(clpgk.core:define-package :clpgk.base.text* (:clpgk.base.text)
  (:use :cl :cl-ppcre)
  (:import/export :clpgk.base.guard*)

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

(in-package :clpgk.base.text)
