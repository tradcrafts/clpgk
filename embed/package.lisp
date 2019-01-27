;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.base:define-package :clpgk.embed ()
  (:use :cl :clpgk.base :clpgk.prolog :clpgk.embed.core)
  
  (:export
    ;; #:TUPLE #:TUPLE-P #:MAKE-TUPLE #:TUPLE-FST #:TUPLE-SND
   ))

(clpgk.base:define-package :clpgk.embed.test ()
  (:use :cl :clpgk.base :clpgk.algebraic :clpgk.embed))
  

(clpgk.base:define-package :clpgk.embed.devel ()
  (:use :cl :clpgk.base :clpgk.algebraic :clpgk.embed))

