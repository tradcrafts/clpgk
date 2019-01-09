(oleo.core.init:define-package :oleo.core.reader ()
  (:import/export :oleo.core.ext)
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
