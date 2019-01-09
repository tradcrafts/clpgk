;; -*- coding: utf-8 -*-

(asdf:defsystem :oleo.core
  :depends-on (:alexandria :kmrcl :metabang-bind :split-sequence :anaphora
                           :cl-ppcre :cl-annot :cl-cont :cffi)
  :serial t
  :components (
               (:file "init")
               (:file "primary-annotations")
               (:file "basic-definitions")
               (:file "ext")
               (:file "reader-package")
               (:file "reader-core-readers")
               (:file "reader-header")
               (:file "reader-primary-readers")
               (:file "test-package")
               (:file "test")
               (:file "test-reader")
               (:file "test-test")
               (:file "finish-testing")
               (:file "pre")
               (:file "common")


               (:file "core")
               ))

