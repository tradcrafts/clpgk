;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.base:define-package :clpgk ()
  (:use :cl)
  (:import/export :clpgk.base :clpgk.algebraic.core :clpgk.embed :clpgk.prolog)
  (:export
   #:clpgk-mode
   )
  (:unexport
   #:clpgk-base-header)
  )

(in-package :clpgk)

(defmacro clpgk-mode ()
  `(clpgk.base:clpgk-base-header (:clap)))





