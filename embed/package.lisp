;; -*- coding: utf-8 -*-

(oleo.base:define-package :oleo.embed ()
  (:use :cl :oleo.base :oleo.prolog :oleo.embed.core)
  
  (:export
   ; #:TUPLE #:TUPLE-P #:MAKE-TUPLE #:TUPLE-FST #:TUPLE-SND
   ))

(oleo.base:define-package :xi-user ()
  (:use :cl :oleo.base :oleo.embed))

