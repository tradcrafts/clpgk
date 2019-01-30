;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.base:define-package :clpgk ()
  (:use :cl)
  (:nicknames :pgk)
  (:import/export :clpgk.base :clpgk.algebraic.core :clpgk.embed :clpgk.prolog)
  (:export
   #:pgk-full-mode
   #:pgk-mode
   )
  (:unexport
   #:clpgk-base-header)
  )

(in-package :clpgk)

(defmacro pgk-mode (&rest other-modes)
  `(clpgk.base:clpgk-base-header :embed :lpar :mspace ,@other-modes))

(defmacro pgk-full-mode ()
  '(clpgk.base:clpgk-base-header))








