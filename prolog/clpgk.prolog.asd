;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(in-package :cl-user)

(asdf:defsystem :clpgk.prolog
  :depends-on (:clpgk.base)
  :serial t
  :components
  ((:file "0-prolog-core")
   (:file "1-prolog-base")
   (:file "package")
   (:file "pl-lib-1")
   ))




