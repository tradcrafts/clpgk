;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.base:define-package :clpgk.embed ()
  (:use :cl :clpgk.base :clpgk.prolog :clpgk.embed.core)
  
  (:export
   ; #:TUPLE #:TUPLE-P #:MAKE-TUPLE #:TUPLE-FST #:TUPLE-SND
   ))

(clpgk.base:define-package :xi-user ()
  (:use :cl :clpgk.base :clpgk.embed))

