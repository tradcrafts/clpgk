;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

;; すこしHaskell的な、代数的データ型の実現
;; define-data,define-newtypeは、組み込み言語とのシームレスな接続を実現
;; define-internal-data,define-internal-newtypeは、CL内のみの使用を前提とした効率的な実装

(clpgk.base:define-package :clpgk.algebraic ()
  (:use :cl :clpgk.base)
  (:import/export :clpgk.algebraic.core :clpgk.algebraic.xdata)
  (:export
   
   ))

