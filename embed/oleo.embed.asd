(in-package :cl-user)
;(defpackage :cl.algebraic.qi-asd
;  (:use :cl :asdf))
;(in-package :cl.algebraic.qi-asd)

(asdf:defsystem :oleo.embed
  :version "0.1"
  :author "j"
  :depends-on (:oleo.base :oleo.algebraic :oleo.prolog)
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

               (:file "xi-test")
               (:file "xi-test-pattern-match")
               (:file "xi-test-where")
               (:file "xi-test-let")
               (:file "xi-test-case")
               (:file "xi-devel")

               ))





