;; -*- coding: utf-8 -*-

(oleo.base:define-package :oleo ()
  (:use :cl)
  (:import/export :oleo.base :oleo.algebraic.core :oleo.embed :oleo.prolog)
  (:export
   #:oleo-mode
   )
  (:unexport
   #:oleo-base-header)
  )

(in-package :oleo)

(defmacro oleo-mode ()
  `(oleo.base:oleo-base-header (:clap)))





