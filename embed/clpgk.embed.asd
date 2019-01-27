;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(in-package :cl-user)

(asdf:defsystem :clpgk.embed
  :license "Qi"
  :depends-on (:clpgk.base :clpgk.algebraic :clpgk.prolog)
  :serial t
  :components ((:file "0-prepare-core")
               (:module "core"
                        :components
                        ((:file "core-01-features")
                         (:file "core-02-YACC")
                         (:file "core-03-reader")
                         (:file "core-04-lib")
                         (:file "core-05-arity")
                         (:file "core-06-eval")
                         ;;(:file "core-07-writer_load7")
                         (:file "core-08-toplevel")
                         (:file "core-09-core")
                         (:file "core-10-optimise")
                         (:file "core-12-load")
                         (:file "core-16-signatures")
                         ))
               (:file "1-finish-core")
               (:file "package")

               (:file "embed-test")
               (:file "embed-test-pattern-match")
               (:file "embed-test-where")
               (:file "embed-test-let")
               (:file "embed-test-case")
               (:file "embed-devel")

               ))





