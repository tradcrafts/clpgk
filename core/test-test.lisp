;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(eval-when (:compile-toplevel :load-toplevel :execute) (clpgk.core.reader:enable-reader))

(in-package :clpgk.core.test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  テスト ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


@eval-when-test
(defun <raise-error> () (error "test error"))

#Verify PROTECTED-MULTIPLE-VALUE-LIST
(equal '(1 2 3) (protected-multiple-value-list (values 1 2 3)))

#Verify PROTECTED-MULTIPLE-VALUE-LIST
(equal 'error (protected-multiple-value-list (values 1 2 3 (<raise-error>))))

#Verify PROTECTED-MULTIPLE-VALUE-LIST*
(equal '(1 2 3) (protected-multiple-value-list* '|on/err| (values 1 2 3)))

#Verify PROTECTED-MULTIPLE-VALUE-LIST*
(eq '|on/err| (protected-multiple-value-list* '|on/err| (values 1 (<raise-error>) 3)))

#Verify HAS-ERRORS
(let (tmp) (and (not (has-errors (push 1 tmp) (push 2 tmp) (push 'ok tmp)))
                (equal '(ok 2 1) tmp)))

#Verify HAS-ERRORS
(let (tmp) (and (eq T (has-errors (push 1 tmp) (push 2 tmp) (<raise-error>) (push 'ok tmp)))
                (equal '(2 1) tmp)))

#Verify HAS-ERRORS
(and (has-errors (<raise-error>)) (not (has-errors nil)))

#Verify HAS-NO-ERRORS
(and (not (has-no-errors (<raise-error>))) (has-no-errors nil))



#Test
(check-assert
   (precond)
   (precond 'name)
   ;; TODO xに対する警告
   ;(todo.. let ((x 'name)) (precond (identity x)))
   ;; TODO xに対する警告がない
   ;(todo.. let ((x 'name)) (precond (identity x) :assert t))
   (precond :context x)
   (precond :assert)
   (precond :assert t)
   (precond :assert t t)
   (precond :test)
   (precond :test t)
   (precond :test t t)
   (precond :do)
   (precond :src)
   (precond :src :assert :test :do)
   (precond :src :type :same-type)
   (let ((x 0)) 
     (precond :do (setq x nil))
     (null x))
   (let ((x 0) (y 0)) 
     (precond :do (setq x 1) (setq y 2))
     (and (eq x 1) (eq y 2)))
        
   )

#Test
(check-assert* (:predicate has-errors)
    (let (a b c) (precond 'foo :src a b c :same-type integer keyword))
    (let (a) (precond :src a :type))
    (let (a) (precond :src a :same-type))
    (precond :assert nil)
    (precond :assert nil t)
    (precond :assert t nil)
    (precond :assert (progn nil))
    (precond :src 1 :test #'symbolp)
    (precond :src 3 :test #'numberp #'symbolp)
    (precond :src 1 :any)
    (let ((a 3))
      (precond :src a :error "err(~A)" a :type symbol)
      )
    )

'(testing
  (flet ((raise () (error "")))
    (check-units 
                                        ;`has-errors'
      ((has-errors) = nil)
      ((has-errors _)
       (1 nil) (nil nil) ((values 1 2 3) nil) ((raise) t))
      ((has-errors _ _)
       ((raise) (values 1 2) t) (nil (values 1 2 3) nil))
                                        ;`has-no-errors'
      ((has-no-errors) = t)
      ((has-no-errors _)
       (1 t) (nil t) ((values 1 2 3) t) ((raise) nil))
      ((has-no-errors _ _)
       ((raise) (values 1 2) nil) (nil (values 1 2 3) t))
                                        ; `protected-multiple-value-list'
      ((protected-multiple-value-list _)
       (1 '(1)) ((values 1 2 3) '(1 2 3)) ((raise) 'error))
                                        ; `protected-multiple-value-list*'
      ((let (a) (protected-multiple-value-list* _ _))
       ((setq a 0) a '(nil)) ((setq a 0) (values 1 a 3) '(1 nil 3)) 
       ((setq a 0) (raise) 0))
    
      ))
  )

;(<build-precond-code> '(a (<let> (clauses))  c (<let> (clauses2)) d e))
      
;(precond :src a b c :test-same-type number symbol)

;(precond :src 1 :any #'symbolp #'numberp)


'(testing 
  (let (tmp) 
    (check-assert (eq 'foo (do-sources (x (1 2 3) 'foo)
                             (push x tmp)))
                  (equal tmp '(3 2 1))))
  (let (tmp) 
    (check-assert (eq 'bar (do-multiple-value-sources ((x y z) 
                                                       ((values 1 2 3) (values 'a 'b))
                                                       'bar)
                             (push (list x y z) tmp)))
                  (equal tmp '((a b nil) (1 2 3))))))



           


;(precond :do (print 'hello) (print 'world) :src a b :test (x) y :type foo bar)

;(precond :in (identity "foo") "bar" 3 :assert nil)

;(<make-src-pairs-string> 'f 3 'b 100)

;(ext:string-concat "foo" "bar")

'(testing 
  (check-units* (:test nil :splicing // )
    ((has-no-errors (let ((a _) (b _))
                      (precond :src a b :type // :assert  (< (+ a b) _))))
     (1 2 (integer) 4))))


