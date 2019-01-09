(oleo.base:oleo-base-header :clap)
(oleo.base:define-package :xi-test ()
  (:use :cl :oleo.base :oleo.algebraic :oleo.embed))

(in-package :xi-test)

@productive* (defun test-fn-1 (a b) (list a b))

#Verify
(equal (funcall (\\test-fn-1 1) 2) '(1 2))
#Verify
(equal (\\test-fn-1 1 true) '(1 #~true))
#Verify
(equal (#~test-fn-1 3 4) '(3 4))
#Verify
(equal (test-fn-1 'a 'b) '(a b))

;(intern "Unk" :OLEO.MSPACE)
;(find-symbol "AWHEN" (find-package :OLEO.MSPACE))
;(use-package :ANAPHORA :OLEO.MSPACE)
;(\\LIST foo (~hello 1 2))

(define-local-data xi-test-data xi/0 (xi/1 t) (xi/2 t t) (xi/3 t t t))
#Verify
(flet ((\let test
         (@@xi/0) -> 0
         (@@xi/1 X) -> [1 X]
         (@@xi/2 A B) -> [2 A B]
         (@@xi/3 A B C) -> [3 A B C]
         _ -> other
         ))
  (and (eql (test (xi/0)) 0)
       (equal (test (xi/1 'a)) '(1 a))
       (equal (test (xi/2 'a 'b)) '(2 a b))
       (equal (test (xi/3 'a 'b 'c)) '(3 a b c))
       (eq (test 0) '#~other)
       ))


#Verify QUOTEの検証
(flet ((\let test
         '(x) -> '(ok)
         (== '(a) '(b c)) -> '(ok okey)
         _ -> 'ng))
  (and (equal '(ok) (test '(x)))
       (equal '(ok okey) (test '(b c)))
       (eq 'ng (test 0))))

#Verify 文字列比較の検証 ｜...｜は正規表現である
(flet ((\let test
         "hello,world" -> 0
         |Hello\|World| -> 1
         |m/hello/i| -> 2
         _ -> failed))
  (and (eql 0 (test "hello,world"))
       (eql 1 (test "Hello"))
       (eql 1 (test "World"))
       (eql 2 (test "HEllo"))
       (eq '#~failed (test "world"))))

#Test 正規表現２　with view patterns
(flet ((\let test
         (-> |s/foo/bar/| "bar") -> 1
         (-> |s/foo/Bar/i| "Bar") -> 2
         (-> |m/b.+r/S| "boor") -> 3 
         _ -> failed))
  (and (eql 1 (test "foo"))
       (eql 2 (test "FOO"))
       (eql 3 (test "boor"))
       (eq '#~failed (test "br"))
       (eq '#~failed (test "fo"))))

;; 正規表現は |m/PTN/OPTS| と |PTN|が等価表現である。
;; 置換 |s/PTN/REPLACE/OPTS|は|s/PTN/REPLACE/|として省略可能である
;; |r/PTN/OPTS| または |r/PTN/| は、PPCREやCLUWのテキストライブラリでそのまま使える正規表現スキャナを返す


;;文字列以外は偽にするようにする
#Verify
(flet ((\let test
         (or |Foo| |m/foo/|) -> 0
         _ -> other))
  (and (eql 0 (test "Foo"))
       (eql 0 (test "foo"))
       (eql '#~other (test "fOO"))
       (eql '#~other (test 100))))


;;(\xenv (TEXT-SCAN |r/f(o+)o/| "foooooo"))
;(flet ((\let test |$/i/^(foo)+$| -> foo _ -> failed)) (test "foofoofoo"))



#Verify 複合パタン 数値比較の検証
(flet ((\let test
         (@ X (and (< 10) (> 0))) -> X
         (or (> 100) (< 0)) -> b
         (and (WHEN INTEGERP) (<= 50) (>= 50)) -> c
         _ -> other))
  (and (eql 3.0 (test 3.0))
       (eq '#~b (test -1))
       (eq '#~b (test 101))
       (eq '#~other (test 100))
       (eq '#~other (test 50.0))
       (eq '#~c (test 50))
       (eq '#~other (test 0))
       (eq '#~other (test 'a))))

#Verify ==パタンと!=パタンの検証 !=パタンは==パタンの否定形である
(flet ((\let test
         (== foo bar) (!= baz boz) -> 0
         (!= foo bar) (== baz boz) -> 1
         (== true false T) (!= true false) -> 10
         (@ X (== 1 "s" #\A [])) X  -> X
         _ _ -> 3))
  (and (eql 0 (test '#~foo '#~foo))
       (eql 1 (test '#~baz '#~boz))
       (eql #\A (test #\A #\A))
       (eql 3 (test #\A #\B))
       (eql 3 (test '#~foo '#~baz))
       (eql 10 (test '#~true 0))
       (eql 10 (test t 0))
       ))


#Verify 複合パタン===は==の自然な拡張であり、文字列と数値に対しEQUALP比較を行う
(labels ((\let test1
           (== foo "bar" 0) -> a
           (!= foo "bar" 0) -> b)
         (\let test2
           (=== foo "bar" 0) -> a
           (!== foo "bar" 0) -> b)
         (test (x) (list (test1 X) (test2 X))))
  (and (equal '#~(a a) (test 0))
       (equal '#~(b a) (test 0.0))
       (equal '#~(a a) (test "bar"))
       (equal '#~(b a) (test "BAR"))
       (equal '#~(a a) (test '#~foo))
       (equal '#~(b b) (test "baz"))))


#Verify シンボルとしての_とマッチさせるには、複合パタンの==節を使う
(flet ((\let test
         (@ X (== _)) -> X
         _ -> other))
  (and (eq '_ (test '_))
       (eq '#~other (test 0))))
       
       
#Verify
(flet ((\let test
         (or (WHEN NUMBERP) symbol?) -> 0
         (or (WHEN SYMBOLP) (not symbol?)) -> 1
         ))
  (and (eql 0 (test '#~foo))
       (eql 0 (test 10))
       (eql 1 (test 'foo))
       ))


#Verify 正格パターンと遅延パターンを用いた、遅延評価によるたらい回し関数のテスト
(flet ((\letrec tak
         (! X) (! Y) _,(<= X Y)  -> Y
         (! X) (! Y) (& Z) -> (tak (&(tak (1- X) Y Z))
                                   (&(tak (1- Y) Z X))
                                   (&(tak (1- Z) X Y)))))
  (= 14 (tak 14 7 0)))

#Verify
(flet ((\let test (& X) -> (+ X X)))
  (= 10 (test (let ((x 5)) (delay (+ x 0))))) )



#Verify
(flet ((\let test1 (force X) -> X)
       (\let test2 (when promise? (! X [Y])) -> [X Y]))
  (and (eq 'a (test1 (delay (identity 'a))))
       (equal '((b) b) (let ((x 'b)) (test2 (delay (list x)))))))

#Verify
(labels ((\let colon? 0 -> true _ -> false)
         (\letrec test
           [(unless COLON? X)] -> true
           [(when COLON? X) :: _] -> false
           [] -> true
           [(unless COLON?) (when COLON?) (when COLON?) (unless COLON? X) :: R] -> (test [X::R])
           [(unless COLON?) (when COLON?) (unless COLON? X) :: R] -> (test [X::R])
           [(unless COLON?) (unless COLON? X) :: R] -> (test [X::R])
           [_ (when COLON?)] -> false
           [(unless COLON? X) :: R] -> (test [X::R])
           _ -> false))

  (and (eq '#~true (test '(a b c)))
       (eq '#~true (test '(a 0 b c)))
       (eq '#~true (test '(a 0 b 0 c)))
       (eq '#~true (test '(a b 0 c)))
       (eq '#~true (test '(a 0 0 b 0 c 00 d)))
       
       (eq '#~false (test '(0)))
       (eq '#~false (test '(0 a 0 0 b 0)))
       (eq '#~false (test '(a 0 0 b 0 c 00 d 0)))

       ))




                                        ;(funcall (\lambda X -> [_ X]) 200)

(\defun test X -> [the::(&CONS! X (test (+ X 1)))])
(&strict (&take 100 (&nthcdr 100000 (test 3))))
(&strict (&take 100 (&freplicate 100000 (lambda () (random 10)))))


#Verify 遅延評価の検証 delayと&は等価、forceと!も等価な表現である
(flet ((\let test (when number? X) ->
             (let P1 (delay (+ X 1))
                  P2 (& (+ X 2))
                  (and (not (number? P1)) (not (number? P2))
                       (promise? P1) (promise? P2)
                       (let A (force P1)
                            B (! P1)
                            C (! P2)
                            D (force P2)
                            (and (number? A) (number? B) (number? C) (number? D)
                                 (= A B) (= C D) (= A (- C 1)) (= X (- A 1) )))))))
  (eq '#~true (test 3)))
                  


#Verify 遅延リストの検証 nums1..nums4は意味的に等価である
(flet ((\letrec nums1 X -> (&cons! X (nums1 (+ X 1))))
       (\letrec nums2 X -> [X::(delay (nums2 (+ X 1)))])
       (\letrec nums3 X -> [X:(nums3 (+ X 1))])
       (\letrec nums4 X -> [X:(nums4 (+ X 1))])
       (\let test
         [X Y Z::_] -> [X Y Z]
         _ -> failed))
  (and (equal '(0 1 2) (test (nums1 0)))
       (equal '(100 101 102) (test (&nthcdr 100 (nums1 0))))
       (equal '(5 6 7) (test (nums2 5)))
       (equal '(2 3 4) (test (nums3 2)))
       (equal '(10 11 12) (test (nums4 10)))
       (equal '(1100 1101 1102) (test (&nthcdr 100 (nums2 1000))))))


(&strict (&take 3 (&nthcdr 100 (&iota -1))))
(&strict (&take 10 (&nthcdr 10000 (&mapcar #'list (&iota -1)))))

#Verify view節のテスト
(flet ((\let test
         (view VECTOR _ (#_) (#(#X))) -> [v X]
         (-> cons? true) -> cons
         (view LIST X [Y] [_::Z]) -> [a X Y Z]
         _ -> failed
         ))
  (and (equal '#~(v 1) (test #(1)))
       (eq '#~cons (test '(1)))
       (equal '(#~a (10) 10 nil) (test 10))))

#Verify `@c'は`cons'のエイリアスであり、挙動も同じである
;; (@c a b ...)は(@c a (@c b ...))に変換される
(flet ((\let test
         (@c A B C D) -> [a A B C D]
         (@c X Y Z) -> [b X Y Z]
         (@c X Y) -> (@c c (@c X Y []))))
  (and (equal '(#~c 1 2) (test '(1 . 2)))
       (equal '(#~b 2 3 4) (test '(2 3 . 4)))
       (equal '(#~a 3 4 5 6) (test '(3 4 5 . 6)))
       (equal '(#~a 3 4 5 (6 7 . 8)) (test '(3 4 5 6 7 . 8)))))

#;書き直せ [2018-09-14]
#Verify 中置コンマ記法と非中置記法の検証
'(flet ((\let test
         (,A B C D) -> [A B C D]
         (,A,B,C) -> [A B C]
         (#A B C D) -> (#v,A,B,C,D)
         (#A,B,C) -> (#v A B C)
         ))
  (and (equal '(1 2 3 4) (test (xtuple 1 (xtuple 2 (xtuple 3 4)))))
       (equal '(2 3 4) (test (xtuple 2 (xtuple 3 4))))
       (equalp #(#~v 1 2 3 4) (test #(1 2 3 4)))
       (equalp #(#~v 2 3 4) (test #(2 3 4)))
       ))

(\defun test X [Y] -> (case Y
                      0 -> zero
                      X -> x
                      _ -> other))
(test 2 '(3))

(\defun test [X] -> (case X
                      (=> INTEGER Z) -> int
                      (=> STRING Y) -> str
                      Y -> X
                      _ -> other
                      )
        _ -> err)


(flet ((\let test
         (view LIST [(=> NUMBER X)]) -> [list X]
         (-> VECTOR (@sv X)) ->[vector X]))
  (test 0))

(\defun test X Y -> (let Z (+ X Y) (+ Z Z)))

((\lambda [X] -> [(LIST <foobar>) (IOTA X) X::X]) '(10))

                                        ;(\defun test X,(number? X) -> (@sv X (+ X X) 1 X 2 X 3) _ -> (@sv))
                                        ;(print (test 10))
                                        ;(print (test 'a))

(\~ (#1,(+ 2  200),[3 4]))

(flet ((\let test [_::Z] -> Z))
  (test '(1)))

#Verify fork節のテスト(@ ...)は(fork ...)のエイリアスである
(flet ((\let test
         (fork A [X]) -> [A X]
         (@ [X Y] A) -> [A X Y]
         (@ A [X Y::Z] [H::R]) -> [A X Y Z H R]
         _ -> failed))
  (and (equal '((1) 1) (test '(1)))
       (equal '((1 2) 1 2) (test '(1 2)))
       (equal '((1 2 3) 1 2 (3) 1 (2 3)) (test '(1 2 3)))
       (eq '#~failed (test 'a))))

#Verify type節のテスト (=> ...)は(type ...)のエイリアス TYPEPによる判別である
(flet ((\let test
         (type INTEGER 0) -> zero
         (=> NEGATIVE-INTEGER X Y) -> [`'negative-integer X Y]
         (=> INTEGER) -> int
         [(=>STRING S) (=>SYMBOL)] -> [complexType S]
         _ -> failed))
  (and (eq '#~zero (test 0))
       (eq '#~int (test 1))
       (equal '#~(complexType "foo") (test '("foo" bar)))
       (equal '(negative-integer -10 -10) (test -10))
       (eq '#~failed (test 'a))))

#Verify when/WHEN unless/UNLESS節の検証 when/unlessにはxiの述語名、WHEN/UNLESSにはLispの述語名を指定する
(flet ((\let test
         (WHEN NEGATIVE-INTEGER-P X Y) -> [negative X Y]
         (WHEN INTEGERP X) -> (#int,X)
         (when number?) -> num
         (UNLESS STRINGP) -> not-a-string
         (unless cons?) -> not-a-cons
         ;; 次節には未達である
         _ -> failed))
  (and (eq '#~num (test 3.0))
       (equalp #(#~int 3) (test 3))
       (equal '(#~negative -1 -1) (test -1))
       (eq '#~not-a-string (test 'a))
       (eq '#~not-a-cons (test "a"))))
       

(\defun test X <- (let* 
                      `(:unify (?y)) X
                      Y)
        _ -> err)
(test '(3))


#Verify やや複雑に入り組ませる検証
(flet ((\let test
         (#[X],(#(#X))) (#[(#(#X))]) -> (#triple X)
         (#[X],Y) Z -> [vector X Y Z]
         (#(#a,(when number? X)),(#b,(=> NUMBER Y))) _ ,(and (number? X) (number? Y)) -> (+ X Y)
         _ _ -> other
         ))
  (and (eq '#~other (test 3 300))
  
       (equal '#~(vector 100 200 300) (test #((100) 200) 300))
       (equal '(#~vector a a a)  (test #((a) a) 'a))
       (equalp #(#~triple a) (test #((a) #(#(a))) #((#(#(a))))))
       (eql 300 (test #(#(#~a 100) #(#~b 200)) 500))))


#Verify 最小のパターンでのテスト (1 params)
(macrolet ((=== (answer &rest input) `(and (equalp ,answer (test-origin ,@input))
                                          (equalp ,answer (test-cloned ,@input)))))
  (flet ((\let test-origin
           (@sv) -> empty-vector
           (@p X Y) -> [tuple/2 X Y]
           )
         (\let test-cloned
           (#) -> empty-vector
           (,X Y) -> [tuple/2 X Y]
           ))
  (and (=== '#~empty-vector #())
       (=== '(#~tuple/2 a b) (xtuple 'a 'b))
    )))


             


                                        ;#Verify
                                        ;(flet ((\let f (0 X) -> X))
                                        ; (equal 0 (f 0)))


(\def <TEST> _ -> test)
#Verify
(eq '#~test (<test> 0))

(\defun xi-test _ -> defun)
#Verify
(eq '#~defun (xi-test 0))


#Verify
(flet ((\let f X -> (let* `(:unify (?a ?a)) X A)))
  (eq 'foo (f '(foo foo))))

#Verify
(flet ((\let f X -> (let* `(:bind (a (b c) &optional (d 'opt))) X [A B C D])))
  (equal '(1 3 5 opt) (f '(1 (3 5)))))

#Verify `expは、Lispの世界のexpそのものである。その部分だけリーダもLispの世界のものにスイッチされる
(flet ((\let f X -> [`'foo bar `'baz X ``,(+ 1 2) `:kwd `#.(- 1 2)]))
  (equal '(foo #~bar baz boz 3 :kwd -1) (f 'boz)))

#Verify Lispの世界にXiの値を持ち込むには`(:with)構文を用いる
(flet ((\let f X Y-> `(:with ((the-x x) (xi-y y)) (list the-x xi-y))))
  (equal '(foo bar) (f 'foo 'bar)))

#Verify 同じ変数名で持ち込むために、より簡便な構文が用意されている
(flet ((\let f X Y -> `(:with (x y) (list x y))))
  (equal '(foo bar) (f 'foo 'bar)))

#Verify Lisp側に持ち込む変数がたかだか１つで、且つ同名の場合、さらに簡略化された記法が可能である
(flet ((\let f X -> `(:with x (list x x))))
  (equal '(foo foo) (f 'foo)))

#Verify 手続き構文
(let ((ans 2)
      val)
  (flet ((\let f X -> (do
                       `(:with (x) (setq val x))
                       `(setq ans (+ val ans))
                        [`ans X])))
    (equal '(12 10) (f 10))))

#Verify (case where)構文は、ガード節のみで構成された条件分岐構文である。当然、バックトラックも可能
(flet ((\let f X -> (case where
                      (not (number? X)) -> not-a-number
                      (< 0 X) <- (if (element? X '(5 20)) @failure passed)
                      true -> 'fail)))
  (and (equal '#~not-a-number (f 'foo))
       (equal '#~passed (f 10))
       (equal 'fail (f 5))
       ))

;; リスト定数は`'リストとするほうが遥かに効率的である。また、真偽値がT,NILでないことに注意
#Verify
(flet ((\let f X -> (= [X foo] `'(0 #~foo))))
  (and (eq '#~true (f 0))
       (eq '#~false (f 1))))

#verify (where)構文
(flet ((\let f
         X Y Z -> (where
                   0 X
                   1 Y
                   [passed Z])
         _ _ Z -> [failed Z]))
  (and (equal '(#~passed 2) (f 0 1 2))
       (equal '(#~failed 3) (f 1 2 3))))

#Verify やや複雑な問題では、(where)構文がより適している場合がある
(flet ((\let f
         X Z -> (where
                 [_ Y] X
                 [_::R] Z
                 (case [Y::R]
                   [1 2::_] -> 12
                   [2 3 4::_] -> 234
                   [3::Z] -> [rest Z]
                   _ -> other))
         _ _ -> ng))
  (and (= 12 (f '(0 1) '(1 2 3 4 5)))
       (= 234 (f '(1 2) '(2 3 4 5)))
       (equal '(#~rest (b c d e)) (f '(2 3) '(a b c d e)))
       (equal '#~ng (f 0 1))
       (equal '#~ng (f '(1 2 3) '(4 5 6)))))

#Verify 検証された要素を続けてさらに検証するにはwhere*を用いる
(flet ((\let f
         X -> (where*
               [Z] X
               [A B] Z
               [B A])
         _ -> ng))
  (equal '(1 0) (f '((0 1)))))

#Verify ラムダ式 形式1: 最も簡便な書法
(flet ((\let f Z -> (/. X (+ X Z))))
  (= (funcall (f 1) 2) 3))

#Verify ラムダ式 形式1: /.の代わりに\を用いることもできる
(flet ((\let f Z -> (\ X (+ X Z))))
  (= (funcall (f 1) 2) 3))

#Verify ラムダ式 形式1: 単純なlambda構文に展開されるためパタンマッチ不能であるが、Lisp側から引数をそのまま参照できる
(flet ((\let f Z -> (\ X (if (number? Z) `(1+ X) `(1- X)))))
  (= 3 (funcall (f 0) 2))
  (= 1 (funcall (f 'a) 2)))

#Verify ラムダ式 形式1:
(flet ((\let f Z -> (lambda X (if (number? Z) `(1+ X) `(1- X)))))
  (= 3 (funcall (f 0) 2))
  (= 1 (funcall (f 'a) 2)))

#Verify ラムダ式 形式1: 多引数のラムダ式は１引数のネストされたラムダ式に展開される
(flet ((\let f Z -> (\ X Y (+ X Y Z))))
  ;; 注意：カリー化されているため、(funcall (f 1) 2 3)はエラーとなる
  (= 6 (funcall (funcall (f 1) 2) 3)))

#Verify ラムダ式 形式2: パターンマッチが必要なら -> を使う
(flet ((\let f Z -> (\ [X Y] -> (+ X Y Z))))
  (= 6 (funcall (f 1) '(2 3))))

#Verify ラムダ式 形式4: 古い書法 (/. -> exp) ==> exp
(flet ((\let f Z -> (/. X -> (+ X Z))))
  (= 3 (funcall (f 1) 2)))

#Verify ラムダ式 形式5: 引数の後に->が続くものは任意の数の引数を受け取るパターンマッチ可能なクロージャである
(flet ((\let f C -> (lambda X Y,(and (number? X) (number? Y)) -> [(+ X C) (- C Y)]
                            X Y -> [C [X Y]])))
  (and (equal '(3 -2) (funcall (f 1) 2 3))
       (equal '(1 (foo 3)) (funcall (f 1) 'foo 3))))


#Verify 無引数のクロージャを作るために、(lambda exp)は合法である。　注意：(/. exp)は非合法である
(flet ((\let f X -> (let Cnt (+ X 1)
                         (lambda [Cnt X]))))
  (equal '(101 100) (funcall (f 100))))


#Verify ラムダ式 形式5: カリー化とパターンマッチの両方を使いたければ、lambdaでなく/.を使う
(flet ((\let f C -> (/. X Y,(and (number? X) (number? Y)) -> [(+ X C) (- C Y)]
                        X Y -> [C [X Y]])))
  (and (equal '(3 -2) (funcall (funcall (f 1) 2) 3))
       (equal '(1 (foo 3)) (funcall (funcall (f 1) 'foo) 3))))


#Verify 恒等関数
(flet ((\let f X -> X))
  (equal '(#~true 0 1.0) (mapcar #'f '(#~true 0 1.0))))

#Verify 定数関数
(flet ((\let f _ -> <CONST>))
  (equal '(<const> <const> <const>) (mapcar #'f '(#~true 0 1.0))))

#Verify 高階関数
(flet ((\letrec test
         F [X::XS] -> [(F X)::(test F XS)]
         _ [] -> [end]))
  (equal '(2 4 6 7 #~end) (test #'1+ '(1 3 5 6))))

#Verify 単純バックトラック
(flet ((\let f
         X <- (if (number? X) @failure true)
         _ -> false))
  (equal '#~(false false true) (mapcar #'f '(0 1.0 nil))))



#Verify ガード付き単純バックトラック
(flet ((\let f
         X,(number? X) <- (+ X 100), (> 200)
         _ -> false))
  (equal '#~(false false 101) (mapcar #'f '(100 200 1))))


#Test 動的ガード
(flet ((\let test
         X F,(and (callable? F) (F X)) -> passed
         X Y -> [rejected X Y]))
  (check-assert (eq '#~passed (test 0 '#~number?))
                (eq '#~passed (test 1 #'#~number?))
                (equal '#~(rejected 1 symbol?) (test 1 '#~symbol?))
                (equal '#~(rejected 10 20) (test 10 20))))


#Verify letの局所性の検証
(flet ((\let f X Y -> (let X Y
                           Y X
                           [X Y])))
  (equal '(2 1) (f 1 2)))

#Verify let*の検証
(flet ((\let f X Y -> (let* X Y
                            Y X
                            [X Y])))
  (equal '(2 2) (f 1 2)))

#Verify Where節の検証
(flet ((\let f
         X -> (where
               0 X
               first)
         _ ->  second))
  (and (eq '#~first  (f 0))
       (eq '#~second (f 1))))

#Verify Where節の検証
(flet ((\let f
         X Y -> (where
                 0 X
                 X Y
                 first)
         _ _ ->  second))
  (and (eq '#~first  (f 0 0))
       (eq '#~second (f 1 1))))

#Verify Where節の検証
(flet ((\let f
         X Y -> (where
                 0 X
                 1 Y
                 first)
         _ _ ->  second))
  (and (eq '#~first  (f 0 1))
       (eq '#~second (f 1 1))))

#Verify Where節の検証 whereは -> の直後に置ける
(flet ((\let f
         X Y -> (where
                 0 X
                 0 Y
                 first)
         _ _ ->  second))
  (and (eq '#~first (f 0 0))
       (eq '#~second (f 1 0))))



;;


(\def <TEST>
      F X Y,(> (apply F X) 0) ->
      (case X
        X <- (F X),(< Y)
        1,(< Y 0) -> second
        _ -> third))

#; --MAYBE DONE-- 直前のスコープに出てくる変数しか直接的な関数適用表現ができない問題 (apply F X)で回避できるが
;; (<test> #'sqrt 16 0)

#Verify
(flet ((\let test F G N -> (let H (if (> N 0) F G)
                                (H N))))
  (and (eql 2 (test #'1+ #'1- 1))
       (eql -1 (test #'1+ #'1- 0))))

(\def <TEST>
      F X Y,(> (F X) 0) ->
      (case (X,F)
        X F <- (F X),(< Y)
        1 _,(< Y 0) -> second
        _ _ -> third))

#; これはうまくいく
(<test> #'sqrt 16 0)

                                        ;(print '(\xi (@p a b c) (@foo bar baz) @nothing (@just 1) @just [] [1 2] ))
                                        ;(print '(\xi (just# Z) -> (just# (+ 1 X)) (nothing#) -> (mydata# 0 1) (mydata# A _ B C) -> [A B C]))

#Verify データ型のパターンマッチ
(flet ((\let f
         (@nothing) -> []
         (@just [_ X]) -> X
         (@just X) -> [X]
         _ -> false))
  (and (null (f (nothing)))
       (equal '(a) (f (just 'a)))
       (equal 'b (f (just '(a b))))
       (equal '#~false (f 0))))

#Verify
(flet ((\let f
         (@right X),(number? X) -> (+ X 1)
         (@left X),(number? X) -> (- X 1)
         (@right X) -> [right X]
         (@left X) -> [left X]
         _ -> []))
  (and (null (f 0))
       (equal '(#~right a) (f (right 'a)))
       (equal '(#~left b) (f (left 'b)))
       (= 101 (f (right 100)))
       (= -1 (f (left 0)))))

                                        ;(PRINT "AHAH")
                                        ;(PRINT '(\def foo Z -> (\ X -> (+ X Z))))
                                        ;(PRINT '(\def foo Z -> (\ X Y -> (+ X Y Z))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment

(xi

(defun xi/bool (cl/bool) (if cl/bool '#~true '#~false))
(defun cl/bool (xi/bool) (cond ((eq '#~true xi/bool) T)
                               ((eq '#~false xi/bool) NIL)
                               (t (error "#~A is not a Xi Boolean true/false" xi/bool))))
 


'(\def boz X <- (+ 1 X),(< 10) _ -> false)
(funcall (#~boz 30) 1000)
(funcall (#~foo 0 1) 10)
(funcall (funcall (#~boz 0 1) 100) 1)

(_foo 3)

(flet ((\xlocal foo X -> [X]))
  foo)

(defvar *<err>* 0)
(let ((x 10))
  (unwind-protect (error "error") (incf *<err>* x)))
  

(\def <TEST>
       X -> (case (X,X)
              [A] Y ->
              [A Y [X]]))
(<TEST> '(1))

(mapcar (\xlambda [X'R],(> `(:with (r) (length r)) 1) -> R _ -> 0) '(0 1 (2 3 4)))

(\def <TEST>
       X -> (let A `(:with (x) (list 'foo x x x))
                 A))

(<test> 'a)
 
(defun #~call  (f &rest args) (apply f args))
(defun #~call* (f &rest args) (apply #'apply f args))

(\def <TEST>
       F -> (call* F 1 2 [3 4 5]))
(<test> (lambda (&rest _) _))

(defwrap #~minus? minusp xibool (t t) 
        
(lambda (x) (xi-boolean (non-negative-integer-p x)))
(defmacro xiwrap (cl-fn-name &optional (arity 1) )
  (let ((vars (freplicate arity #'gensym)))
    `(lambda ,vars (,cl-fn-name ,@vars))))




(\def <TEST>
       X F,(and (callable? F) (F X)) <- (+ X 100) , F
       _ _ -> false)

(<test> 1 #'#~number?)
(<test> 1 (lambda (x) '#~true))

(<test> 1 '#~number?)
(#~callable? 'a)
(#~function? 'consp)
(#~closure? #'consp)

(symbol-function '#~cons?)

;(\xi (@c 1 2))

(#~apply '#~cons '(1 2))


