;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

;; テスト
(eval-when (:compile-toplevel :load-toplevel :execute) (clpgk.core.reader:enable-reader))

(defpackage clpgk.core.1-finish-testing
  (:use :cl :clpgk.core.test)
  )

(in-package :clpgk.core.1-finish-testing)

#Testing WITH-COLLECT
(check-unit (multiple-value-list _)
  ((with-collect ()) nil)
  ((with-collect (c)) '(nil))
  ((with-collect (c d)) '(nil nil))
  ((with-collect (c) (dotimes (i 3) (c i))) '((0 1 2)))
  ((with-collect (c1 c2) (dotimes (i 3) (c1 (1+ i)) (c2 (1- i)))) '((1 2 3) (-1 0 1)))
  )

#Testing DOLIST*
(check-unit _
  ('((1 a) (2 b) (3 c))  (let (tmp)
                           (dolist* ((y . x) (mapcar #'cons '(a b c) '(1 2 3)) (nreverse tmp))
                             (push (list x y) tmp))))
  ('(nil 3) (let ((cnt 0)) (list (dolist* (x '(a b c)) (incf cnt))
                                 cnt)))
  )

#Testing MULTIPLE-VALUE-BIND*
(check-unit _
  ('(a b z) (multiple-value-bind* (x _ y _ z _) (values 'a 1 'b 2 'z 3) (list x y z))))

#Testing  MULTIPLE-VALUE-BIND-IF* MULTIPLE-VALUE-BIND-WHEN*
(check-unit _
  ('(a b c) (multiple-value-bind-if* (x _ y _ z _) (values 'a 1 'b 2 'c 3) (list x y z) 'falied))
  ('falied (multiple-value-bind-if* (x _ y _ z _) nil (list x y z) 'falied))
  (t (multiple-value-bind-if* (x _ y _ z _) nil (list x y z) t))
  ('(a b c) (multiple-value-bind-when* (x _ y _ z _) (values 'a 1 'b 2 'c 3) (list x y z)))
  (nil (multiple-value-bind-when* (x _ y _ z _) nil (list x y z)))
  (nil (multiple-value-bind-when* (x _ y _ z _) nil (list x y z)))
  )
  

#Testing DESTRUCTURING-BIND*
(check-unit _
  ('(a b c) (destructuring-bind* (x _ y _ z _) '(a 1 b 2 c 3) (list x y z))))

#Testing  DESTRUCTURING-BIND-IF* DESTRUCTURING-BIND-WHEN*
(check-unit _
  ('(a b c) (destructuring-bind-if* (x _ (y _) z _) '(a 1 (b 2) c 3) (list x y z) 'falied))
  ('falied (destructuring-bind-if* ((x _ y) _ z _) nil (list x y z) 'falied))
  (t (destructuring-bind-if* (x _ y _ z _) nil (list x y z) t))
  ('(a b c q) (destructuring-bind-when* (x (_ y _ z) _ p) '(a (1 b 2 c) 3 q) (list x y z p)))
  (nil (destructuring-bind-when* (x _ y _ z _) nil (list x y z)))
  (nil (destructuring-bind-when* (x _ (y _) z _) nil (list x y z)))
  )



#Testing STRING-CONCAT
(check-unit _
  ("" (string-concat))
  ("foo" (string-concat "foo"))
  ("foobar" (string-concat "foo" "bar"))
  ("foo,bar,baz." (string-concat "foo" ",bar" ",baz.")))

#verify MEMOIZED
(flet ((foo (x) (memoized (list x))))
  (and (equal (foo 1) '(1))
       (eq (foo 1) (foo 2))))



#Testing
(check-unit _
  (nil (do-sources (x ())))
  ('foo (do-sources (x () 'foo)))
  ('(foo v) (let ((v 'v)) (do-sources (x () (list 'foo v)) (setq v 'bar))))
  ('((foo) (bar)) (with-collect (c) (do-sources (x ((list 'foo) (list 'bar))) (c x))))
  )

#Testing
(check-unit _
  (nil (do-multiple-value-sources (() ())))
  ('foo (do-multiple-value-sources (() (1) 'foo)))
  ('(foo v) (let ((v 'v)) (do-multiple-value-sources ((x) () (list 'foo v)) (setq v 'bar))))
  ('((v . 1) . 2) (let ((v 'v)) (do-multiple-value-sources ((x) (1 2) v) (setq v (cons v x)))))
  (3 (let ((cnt 0)) (do-multiple-value-sources (() ('a 'b 'c) cnt) (incf cnt))))
  ('((1 2) (3 4)) (with-collect (c)
                    (do-multiple-value-sources ((x y) ((values 1 2) (values 3 4))) (c (list x y)))))
  )

#Testing SEAUAL
(check-unit* (:test nil) (sequal '_ _)
  (nil nil)
  ((a) '(a))
  (#b101 #b101)
  ("a" "a")
  (_ '(1 2))
  ((1 _ 3) '(1 2 3))
  ((?a b s)  '(x b s))
  ((a ?b (?b y) x) '(a c (c y) x))
  (#(a ?b (?b y) x) #(a c (c y) x))
  (#(a ?b #(?b y) x) #(a c #(c y) x))
  ((?a ?b ?c (?c ?b ?a)) '(1 2 3 (3 2 1)))
  ((:eval (integerp _)) 1)
  ((?x b (:eval (keywordp _)) d) '(1 b :c d)) 
  )

#Testing SEQUAL (Failure patterns)
(check-unit* (:test nil) (NOT (sequal '_ _))
  (nil 1)
  ((a) '(b))
  ('_ '(1 2))
  ((1 _ 3) '(2 2 3))
  ((?a b s)  '(x a s))
  ((a ?b (?b y) x) '(a b (c y) x))
  (#(a ?b (?b y) x) #(a c (b y) x))
  (#(a ?b #(?b y) x) #(a b #(c y) x))
  ((?a ?b ?c (?c ?b ?a)) '(1 2 3 (4 3 2)))
  ((:eval (integerp _)) 0.0)
  ((a b (:eval (keywordp _)) d) '(a b c d)) 
  )


#Testing ALIST-BIND
(check-unit (_ _ (alexandria:plist-alist '_) _)
  (alist-bind (a b) (a 1 b 2) (list b a) '(2 1))
  (alist-bind (a b) (a 1) (list b a) '(nil 1))
  (alist-bind (a b) nil (list b a) '(nil nil))
  (alist-ebind (a b) (a 1 b 2) (list b a) '(2 1))
  )

#Testing PLIST-BIND
(check-unit (_ _ '_ _)
  (plist-bind (a b) (a 1 b 2) (list b a) '(2 1))
  (plist-bind (a b) (a 1) (list b a) '(nil 1))
  (plist-bind (a b) nil (list b a) '(nil nil))
  (plist-ebind (a b) (a 1 b 2) (list b a) '(2 1))
  )

#Testing ALIST-EBIND
(check-unit* (:test nil) (has-errors (_ _ (alexandria:plist-alist '_) _))
  (alist-ebind (a b) (a 1) (list b a))
  (alist-ebind (a b) nil (list b a))
  (alist-rebuild-ebind (a b) (a 1) (list b a))
  (alist-rebuild-ebind (a b) nil (list b a))
  )

#Testing PLIST-EBIND
(check-unit* (:test nil) (has-errors (_ _ '_ _))
  (plist-ebind (a b) (a 1) (list b a))
  (plist-ebind (a b) nil (list b a))
  (plist-rebuild-ebind (a b) (a 1) (list b a))
  (plist-rebuild-ebind (a b) nil (list b a))
  )

#Testing ALIST-REBUILD-BIND
(let ((src '((a . 1) (b . 2) (c . 3))))
  (check-unit* (:test nil :splicing //) (let ((ans (alexandria:plist-alist '_))
                                             (result (_ _ src //)))
                                         (and (equal ans result)
                                              (not (eq src result))))
                             
    ((a 0 b 2 c 3) alist-rebuild-bind  (a b c) ((setq a 0)))
    ((a 1 b 0 c 3) alist-rebuild-bind  (a b c) ((setq b 0)))
    ((a 0 b 1 c 2) alist-rebuild-bind  (a b c) ((setq a 0) (setq b 1 c 2)))
    ((a 1 b 2 c 3) alist-rebuild-bind  (a b c) ())
    ((a 0 b 1 c 2) alist-rebuild-ebind (a b c) ((setq a 0 b 1 c 2)))
    ((a 1 b 2 c 3) alist-rebuild-ebind (a b c) ())))

#Testing PLIST-REBUILD-BIND
(let ((src '(a 1 b 2 c 3)))
  (check-unit* (:test nil :splicing //) (let ((ans '_)
                                             (result (_ _ src //)))
                                         (and (equal ans result)
                                              (not (eq src result))))
                             
    ((a 0 b 2 c 3) plist-rebuild-bind  (a b c) ((setq a 0)))
    ((a 1 b 0 c 3) plist-rebuild-bind  (a b c) ((setq b 0)))
    ((a 0 b 1 c 2) plist-rebuild-bind  (a b c) ((setq a 0) (setq b 1 c 2)))
    ((a 1 b 2 c 3) plist-rebuild-bind  (a b c) ())
    ((a 0 b 1 c 2) plist-rebuild-ebind (a b c) ((setq a 0 b 1 c 2)))
    ((a 1 b 2 c 3) plist-rebuild-ebind (a b c) ())))


#Verify ALIST-UPDATE-EBIND PLIST-UPDATE-EBIND
(and (eq 0 (alist-update-ebind () nil 0))
     (eq 0 (plist-update-ebind () nil 0)))

#Testing ALIST-UPDATE-EBIND
(let* ((src (copy-tree '((a . 1) (b . 2) (c . 3))))
       (original-elems (copy-list src)))
  (check-unit* (:test nil :splicing //) (let* ((correct-src (alexandria:plist-alist '_))
                                             (ans _)
                                             (result (_ _ src //)))
                                         (and
                                           ;; 既存のALISTの構造を保存したまま更新するか確認
                                           (eql ans result)
                                           ;; 各セルの位置を維持したまま更新するか確認
                                           (every #'identity (mapcar #'eq original-elems src))
                                           (equal correct-src src)
                                           ))
                             
    ((a 0 b 2 c 3) 5 alist-update-ebind  (a b c) ((setq a 0) 5))
    ((a 0 b _ c 3) 6 alist-update-ebind  (a b c) ((setq b '_) 6))
    ((a 3 b 1 c 2) 7 alist-update-ebind  (a b c) ((setq a 3) (setq b 1 c 2) 7))
    ((a 3 b 1 c 2) nil alist-update-ebind  (a b c) ())))

#Testing PLIST-UPDATE-EBIND
(let* ((src (copy-list '(a 1 b 2 c 3)))
       (original-conses (maplist #'identity src)))
  (check-unit* (:test nil :splicing //) (let* ((correct-src '_)
                                             (ans _)
                                             (result (_ _ src //)))
                                         (and
                                           ;; 既存のPLISTの構造を保存したまま更新するか確認
                                           (eql ans result)
                                           ;; 各セルの位置を維持したまま更新するか確認
                                           (every #'identity (maplist (lambda (r c) (eq r (car c)))
                                                                      src original-conses))
                                           (equal correct-src src)
                                           ))
                             
    ((a 0 b 2 c 3) 5 plist-update-ebind  (a b c) ((setq a 0) 5))
    ((a 0 b _ c 3) 6 plist-update-ebind  (a b c) ((setq b '_) 6))
    ((a 3 b 1 c 2) 7 plist-update-ebind  (a b c) ((setq a 3) (setq b 1 c 2) 7))
    ((a 3 b 1 c 2) nil plist-update-ebind  (a b c) ())))


#Testing MAKE-ALIST-LET and MAKE-ALIST-LET*
(check-unit* (:test nil) (equal (alexandria:plist-alist '_) _)
  (nil (make-alist-let ()))
  (nil (make-alist-let* ()))
  ((a 0 b nil) (make-alist-let (a b) (setq a 0)))
  ((a nil b 0) (make-alist-let* (a b) (setq b 0)))
  ((a 1 b 0) (let ((a 0)) (make-alist-let ((a 1) (b a)))))
  ((a 1 b 1) (let ((a 0)) (make-alist-let* ((a 1) (b a)))))
  )

#Testing MAKE-PLIST-LET and MAKE-PLIST-LET*
(check-unit* (:test nil) (equal '_ _)
  (nil (make-plist-let ()))
  (nil (make-plist-let* ()))
  ((a 0 b nil) (make-plist-let (a b) (setq a 0)))
  ((a nil b 0) (make-plist-let* (a b) (setq b 0)))
  ((a 1 b 0) (let ((a 0)) (make-plist-let ((a 1) (b a)))))
  ((a 1 b 1) (let ((a 0)) (make-plist-let* ((a 1) (b a)))))
  )

#verifying UNSAFE-ALIST-PLIST & UNSAFE-PLIST-ALIST (NIL passed)
(and (null (unsafe-alist-plist nil))
     (null (unsafe-plist-alist nil)))

#verifying UNSAFE-ALIST-PLIST
(let* ((alist (copy-tree '((a . 1) (b . 2))))
       (correct-plist (alexandria:alist-plist alist))
       (plist (unsafe-alist-plist alist)))
  (and (eq alist plist)
       (equal plist correct-plist)))

#verifying UNSAFE-PLIST-ALIST
(let* ((plist (copy-list '(a 1 b 2)))
       (correct-alist (alexandria:plist-alist plist))
       (alist (unsafe-plist-alist plist)))
  (and (eq alist plist)
       (equal alist correct-alist)))

#Testing CONSTANT-ALIST
(check-unit _
  (nil (constant-alist))
  ('((a . 0) (b . 1)) (constant-alist a 0 b 1)))


#Testing COMPARE for ALISTS
(check-unit* (:test nil) (eq _ (_ (alexandria:plist-alist (copy-tree '_)) (alexandria:plist-alist (copy-tree '_))))
  (nil alist-compare (a 0) ())
  (nil alist-compare (a 0) (a 0 b 1))
  (t alist-eq (a 0) (a 0))
  (t alist-eql (a 2.71828) (a 2.71828))
  (nil alist-eq (a (x)) (a (x)))
  (nil alist-eql (a (x)) (a (x)))
  (t alist-equal (a ("a" "b")) (a ("a" "b")))
  (nil alist-equal (a ("A" "b")) (a ("a" "b")))
  (t alist-equalp (a ("A" "b")) (a ("a" "b")))
  )


#Testing COMPARE for PLISTS
(check-unit* (:test nil) (eq _ (_ (copy-tree '_) (copy-tree '_)))
  (nil plist-compare (a 0) ())
  (nil plist-compare (a 0) (a 0 b 1))
  (t plist-eq (a 0) (a 0))
  (t plist-eql (a 2.71828) (a 2.71828))
  (nil plist-eq (a (x)) (a (x)))
  (nil plist-eql (a (x)) (a (x)))
  (t plist-equal (a ("a" "b")) (a ("a" "b")))
  (nil plist-equal (a ("A" "b")) (a ("a" "b")))
  (t plist-equalp (a ("A" "b")) (a ("a" "b")))
  )

