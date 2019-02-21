;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(asdf:defsystem :clpgk
    :version "0.9"
    :description "CLPGK: Embedded functional language and large libraries for our jobs."
    :author "PGkids Laboratory <lab@pgkids.co.jp> http://github.com/PGkids"
    :depends-on (:clpgk.core :clpgk.base :clpgk.algebraic :clpgk.prolog :clpgk.embed)
    :license "LLGPL / Qi"
    :serial t
    :components ((:file "package")
                 ))



