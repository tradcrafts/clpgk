;; -*- coding: utf-8 -*-

(in-package :cl-user)
;(defpackage :cl.algebraic.qi-asd
;  (:use :cl :asdf))
;(in-package :cl.algebraic.qi-asd)

(asdf:defsystem :oleo.prolog
  :version "0.1"
  :author "j"
  :depends-on (:oleo.base)
  :serial t
  :components
  ((:file "0-prolog-core")
   (:file "1-prolog-base")
   (:file "package")
   (:file "pl-lib-1")
   ))




