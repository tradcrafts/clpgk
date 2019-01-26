;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.core.init:define-package :clpgk.core.reader ()
  (:import/export :clpgk.core.ext)
  (:use :cl :anaphora :alexandria :split-sequence :cl-annot)
  (:export

   ;; 自然な拡張
   #:read-sequential-from-string

   
   ;; 以下、独自ライブラリ
   
   #:|#{-READER|
   #:|#X-READER| #:|#(-READER|
   #:|'-READER|  #:|#O-READER|
   #:|#'-READER| #:|;-READER|
   #:|#B-READER| #:|(-READER|
   #:|#*-READER|

   
   #:reader/ident-reader #:reader/line-reader
   #:reader/skip-white-spaces #:reader/skip-line
   #:reader/skip-chars-while #:reader/take-chars-while

   #:select-reader
   #:register-reader-registerer
   #:register-optional-reader
   #:enable-reader
   
   #:define-lpar-backslash-reader

   
   ;; 以下、アノテーション
   #:where
  
   ))
