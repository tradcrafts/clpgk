;; -*- coding: utf-8 -*-

(in-package :clap)
(common-header :xi)

;;;;;;;;;;;;;;;;;;;;;;  組み込みデータ型の定義 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Either
;; データコンストラクタleft,right が定義される


(define-data either (left t) (right t))
(define-internal-data either# (left# t) (right# t))



(\Xi+ (datatype <either>
                L : A\;
                ======
                (@p left# L) : (left A)\;

                R : B\;
                ======
                (@p right# R) : (right B)\;
                
                 ;let B (newsym B)
                 P : (left A)\;
                 ===
                 P : (either (A B))\;

                 ;let A (newsym A)
                 P : (right B)\;
                 ===
                 P : (either (A B))\;
                ) .)


(\Xi+

      (define isLeft {(either (A B)) --> boolean}
              (@p left# _) -> true
              _ -> false)

      (define isRight {(either (A B)) --> boolean}
              (@p right# _) -> true
              _ -> false)

      (define lefts {[(either (A B))] --> [A]}
              [] -> []
              [(@p left# X)\|Rest] -> [X\|(lefts Rest)]
              [_\|Rest] -> (lefts Rest))

      (define rights {[(either (A B))] --> [B]}
              [] -> []
              [(@p right# X)\|Rest] -> [X\|(rights Rest)]
              [_\|Rest] -> (rights Rest))
      

      (define either {(A --> C) --> (B --> C) --> (either (A B)) --> C}
              F _ (@p left# A) -> (F A)
              _ G (@p right# B) -> (G B))
      
      (define partionEithers {[(either (A B))] --> ([A] * [B])}             
              Xs -> (@p (lefts Xs) (rights Xs)))
      .)






;;;;;;;;;;;;;;;;;;;;;; End of J-DATA.LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#Comment



(~rights (list (left 1) (right 3) (left 10)))
(~either #'1+ #'1- (left 3))
(~partionEithers (mapcar #/(if (oddp _) (left _) (right _)) (iota 10)))
(@p (1 3 5 7 9) (0 2 4 6 8))



ex)
整数を２つ取るデータコンストラクタfoo,そして２つのアクセサfoo-1,foo-2が定義される
(define-data foo (foo (integer foo-1) (integer foo-2)))

ex)
(define-newtype price t :accessor price-get)


ex) コンストラクタの応用　コピーと自己更新
(define-data foo (foo symbol number t))
データの更新付きコピー (コピーコンストラクタとしての用法)
(foo nil nil 'y :source (foo 'a 100 'x)) => (foo 'a 100 'y)と等価
(foo nil t t :source (foo 'a 100 'x) :mask t) => (foo nil 100 'x)と等価

(let ((x (foo 'a 0 'z)))
  (foo nil 1 'q :source x :update t)) => xを自己更新 -> #<foo a 1 q> 
