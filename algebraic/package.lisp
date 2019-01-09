;; J-DATA.LISP  Time-stamp: <2018-09-22 19:20:12 USER>    (incremental autotitle)
;; Haskell的な、代数的データ型の実現
;; define-data,define-newtypeは、Qiとのシームレスな接続を実現
;; define-internal-data,define-internal-newtypeは、CL内のみの使用を前提とした効率的な実装

(oleo.base:define-package :oleo.algebraic ()
  (:use :cl :oleo.base)
  ;(:nicknames #:clap #:ap)
  (:import/export :oleo.algebraic.core :oleo.algebraic.xdata)
  (:export
   
   ))

