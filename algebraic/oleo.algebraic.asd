;; -*- coding: utf-8 -*-

(asdf:defsystem :oleo.algebraic
  :version "0.1"
  :author "j"
  :depends-on (:oleo.base)
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



