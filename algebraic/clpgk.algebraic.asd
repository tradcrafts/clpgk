;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(asdf:defsystem :clpgk.algebraic
  :license "LLGPL"
  :depends-on (:clpgk.base)
  :serial t
  :components (
               (:module "xdata"
                        :components
                        ((:file "package")
                         (:file "defs"))
                        )
               (:module "core"
                        :components
                        ((:file "package")
                         (:file "applicable")
                         (:file "data-1")
                         (:file "data-2")))
                        
               
               (:file "package")


               ))



