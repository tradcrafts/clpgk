;; -*- coding: utf-8 -*-

(defpackage :clap-test-0
  (:use :cl :clap-test)
  )

(in-package :clap-test-0)
(common-header :xi)

(\Xi+

 (datatype senior

           
           let Age (age S)
           Age : senior\;
           __________
           S : senior-citizen\;
           
           ________
           S : senior-citizen >>
           S : string\;
  
           if(number? Age)
           if(> Age 64)
           ____________
           Age : senior\;)

 (define age
     {string --> number}
     "Yumi" -> 12
     "Bach" -> 250
     "Bux" -> 290
     "Jun" -> 43
     _ -> (error "age err")
     )

 
 (define foo
     {senior-citizen --> senior-citizen}
     "Bach" -> (if true "Bux" "Bach")
     X -> X)

 (define bar
     {senior-citizen --> string}
     X -> (do (print [bar -> X]) X))

 (foo "Bach")
 (bar "Bach")
 .)


(print (~foo "Yumi"))

(\Xi
 (define <SYMS> _ -> [hello <hello> %hello% *hello* +hello+ $hello$ &hello cons CONS])
 .)

(print (<syms> 0))

