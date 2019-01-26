;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(asdf:defsystem :clpgk.base
  :depends-on (:clpgk.core :alexandria :kmrcl :metabang-bind :split-sequence :anaphora
                           :cl-ppcre :cl-annot :cl-cont :cffi)
  :serial t
  :components (
               (:file "guard")
               (:file "text-package")
               (:file "text-sysdep-features")
               (:file "text-reader")
               (:file "text-regex")
               (:file "text-comment-converter-defs")
               (:file "text-test")
               (:file "attr")
               (:file "member")
               (:file "list")
               (:file "hlist")
               (:file "hash-table")
               (:file "unify-1")
               (:file "unify-2")
               (:file "unify-3")
               (:file "unify-4")
               (:file "unify-5")
               (:file "unify-6")
               (:file "unify-7")
               (:file "unify-8")
               (:file "unify-9")
               (:file "match-1")
               (:file "match-2")
               (:file "form")
               (:file "form-1-logic")
               (:file "form-2-let")
               (:file "form-3-bind")
               (:file "form-4-method")
               (:file "cont")
               (:file "lazy-list")
               (:file "iterate")
               (:file "ffi")
               (:file "base")
               ;(:file "cluw-devel")
               ;(:file "cluw-user")
               ))


