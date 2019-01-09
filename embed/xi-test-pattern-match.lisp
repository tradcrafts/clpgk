;; -*- coding: utf-8 -*-

(oleo.base:oleo-base-header :xi)
(in-package :xi-test)

#Verify 単純パターンマッチ
(flet ((\let test
         0 -> zero
         1 -> one
         0.0 -> floating-zero
         "" -> empty-string
         "foo" -> foo-string
         _ -> other))
  (equal '#~(zero one other empty-string foo-string other floating-zero)
         (mapcar #'test '(0 1 2 "" "foo" "FOO" 0.0))))

#Verify ガード付き単純パターンマッチ
(flet ((\let test
         0 -> zero
         X,(number? X) -> number
         X,(boolean? X) -> boolean
         _ -> other))
  (equal '#~(zero number boolean other) (mapcar #'test '#~(0 1 true ok))))

#Verify リストのパターンマッチ
(flet ((\let test
         [X X] -> 0
         [_ R::R] -> 1
         [X::(,Y,Z)] -> [X Y Z]
         _ -> 2))
  (and (= 2 (test '(a b)))
       (= 0 (test '(a a)))
       (= 1 (test '(a (b C d) b c d)))
       (= 1 (test '(a 0 . 0)))
       (equal '(a b c) (test (cons 'a (xtuple 'b 'c))))))


#Verify リストのパターンマッチ＆再帰
(flet ((\letrec test
         [] -> 0
         [X] -> 1
         [X Y] -> 2
         [X::R] -> (+ 1 (test R))))
  (and (= 0 (test nil))
       (= 1 (test '(0)))
       (= 2 (test '(0 1)))
       (= 5 (test '(a b c d e)))))


#; TODO
#; 書き直せ [2018-09-14]
#Verify タプルのパターンマッチ＆ガード
'(flet ((\let test
         (@p A B),(cons? B) -> A
         (@p _ []) -> nul
         (,A,B,B,C) -> [A B C]
         (,A,B,C,D) -> [A B C D]
         (,_,B) -> B))
  (and (= 2 (test (xtuple 1 2)))
       (= 1 (test (xtuple 1 '(2))))
       (eq '#~nul (test (xtuple 0 nil)))
       (equal '(4 3 2 1) (test (xtuple 4 (xtuple 3 (xtuple 2 1)))))
       (equal '(1 2 3) (test (xtuple 1 (xtuple 2 (xtuple 2 3)))))
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment
(equalp (xtuple 1 2) (xtuple 1 2))
(\xenv (#x,y,#~foo))
