(defpackage :clap-test-1
  (:use :cl :clap-test)
  )

(in-package :clap-test-1)
(common-header :xi)

(\Xi+

 (datatype _detail
           ;______
                                        ;(<detail?> X) : verified >> X : _detail\;

           if(or (<detail?> X) (<detail-sym?> X))
           !\;
           _____
           X : _detail\;

           X:string\; Y:number\;
           =====
           [X Y]:_detail\;

           ;let A (newsym a)
           _______________
           (<detail> A) : _detail\;
           )

 (define <detail?> {number --> boolean}
         3 -> true
         _ -> false)

 (define <detail-sym?> {symbol --> boolean}
         ok -> true
         _ -> false)

 (define <foo> {_detail --> number}
         [_ Y] -> Y
         X -> (do (print [fake detail X]) 0))

 (define <bar> { (list _detail) --> string}
         [[X _]\|_] -> X
         _ -> "DOYANEN!")

 (define <detail> { A --> A}
         X -> X)
 
 (<foo> ["Helo" 300])
 (<foo> 3)
 (<foo> ok)
 (<foo> (<detail> "a"))
 (<bar> [["a" 0] ["b" 1]])
 (<bar> [(<detail> "a") (<detail> true)])
 (do (print ok) (print ng) (print end) done)
 .)


(defparameter *foo*
 #;
                                        ;hello
                                        ;doye
                                        ;あほか
                                        ;dododo
  )

(print *foo*)


(defun ~foo (x) (eval x))

(\Xi+

 
 X : number >> P \;
 ______
 (ba X) : number >> P \;

 (define bar
     {number --> number}
     X -> (+ X 1))

 ;(print (bar (foo 0)))
 .)

