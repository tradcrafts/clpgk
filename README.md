<!--dd -*- coding: utf-8 -*- -->  
This file is part of CLPGK  
Copyright (c) 2019 PGkids Laboratory  

## 本パッケージについて
本パッケージは弊社業務での使用を前提とした大規模な基礎ライブラリ群と、リードマクロを用いた埋め込み関数型言語から成っております。
行儀の悪いオレオレライブラリの典型的なやつです。  
公開する予定は当初ありませんでしたが、埋め込み関数型言語が予想以上にいろいろな場面で便利に使えるため公開した次第です。  
ソースコードはところどころものすごく汚く乱雑でTODOとWARNINGの嵐ですが、何卒ご容赦を。  

## 動作条件  
**SBCL**および**CCL**をターゲットとしています  
Allegroは導入していないのでわかりません  
CLISPは試みましたがお手上げです(最適化要件を満たしていないので仮に動いたとしても正常動作はしないでしょう)  
オールインワンLisp環境の**Portacle**でも問題なく動作しました(SBCLなのでWindows版では下記の修正が必要です)  
末尾再帰最適化など適切な最適化が随所でなされる処理系であることをことを前提としています  

## インストール方法  
Quicklispのlocal-projectsディレクトリ以下に展開後、(ql::register-local-projects)で登録、(ql:quickload :clpgk)でロードできます。  
**Windows版のSBCLでは依存ライブラリであるkmrclのインストール中にエラーが出る**と思いますが、signals.lispの当該箇所を条件コンパイルすることで回避できます。  
回避例:  
```lisp:(kmrcl)signals.lisp
    #+sbcl (sb-sys:enable-interrupt signum handler)
FIX:-->  
    #+(and sbcl unix) (sb-sys:enable-interrupt signum handler)  
    #+(and sbcl win32) (error "kmrcl:set-signal-handler is disabled (win32)")  
    #+sbcl (sb-sys:enable-interrupt signum (or old-handler :default))  
FIX-->  
    #+(and sbcl unix) (sb-sys:enable-interrupt signum (or old-handler :default))  
    #+(and sbcl win32) (error "kmrcl:remove-signal-handler is disabled (win32)")  
```

とりあえずWindowsで動かしてみる場合にはCCLのほうが手っ取り早いと思います。ただし、SLIME上で埋め込み関数型言語を扱うのならSBCLがおすすめです。  

## 埋め込み実例
５行クイックソート
```lisp
(in-package :cl-user)
(pgk:pgk-mode) ;; enable pgk reader

(\def qsort
      [] -> []
      [P::Xs] -> (let Smaller [A :: A<-Xs, if (<= A P)]
                      Larger  [B :: B<-Xs, if (> B P)]
                      (append (qsort Smaller) [P] (qsort Larger))))

(let* ((src-xs (pgk:shuffle (\|1...30)))
       (sorted (#~qsort src-xs)))
  (print src-xs)
  (print sorted))

EVAL===>
(2 10 23 6 4 1 9 16 13 3 30 27 15 22 12 18 24 17 7 29 21 26 5 8 11 14 20 28 19
 25) 
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
 30) 


;; あるいはfletに埋め込む方法も (再帰を伴う場合は\letrecを使う)
(flet ((\letrec qsort
         [] -> []
         [P::Xs] -> (let Smaller [A :: A<-Xs, if (<= A P)]
                         Larger  [B :: B<-Xs, if (> B P)]
                         (append (qsort Smaller) [P] (qsort Larger)))))
  (let* ((src-xs (pgk:shuffle (\|1...30)))
         (sorted (qsort src-xs)))
    (print src-xs)
    (print sorted)))

EVAL==>
(27 12 1 23 18 5 6 16 26 30 15 28 10 22 19 3 24 29 13 21 20 11 2 7 17 9 8 4 14
 25) 
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
 30) 
```

遅延評価を用いたたらいまわし関数の定義 (当然爆速です)
```lisp
(\def tarai
      (! X) (! Y) _,(<= X Y)  -> Y
      (! X) (! Y) (& Z) -> (tarai (&(tarai (1- X) Y Z))
                                  (&(tarai (1- Y) Z X))
                                  (&(tarai (1- Z) X Y))))

(time (format t "Answer: ~A~%" (#~tarai 12 6 0)))

EVAL===>
Answer: 12
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  460,313 processor cycles
  4,088 bytes consed
```
遅延リストを用いたエラトステネスのふるいによる無限素数列の生成
```lisp
(\def canDivide?
      _ [] -> false
      N [P::Ps] -> (if (= 0 (MOD N P)) true (canDivide? N Ps)))

(\def getPrimes
      [N::Ns] Ps -> (if (canDivide? N Ps)
                      (getPrimes Ns Ps)
                      [N : (getPrimes Ns [N::Ps])]))

;; 無限素数リストから最初の１００個の素数を得る
(let ((lazy-primes (\\getPrimes [2..] [])))
  (print (pgk:&take! 100 lazy-primes)))

EVAL===>
(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103
 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199
 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313
 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433
 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547) 
 
;; 実装例２: ガードを用いる (若干非効率的になるがコードはなんとなくかっこいい)
(\def getPrimes2
      [N::Ns] Ps, (canDivide? N Ps) -> (getPrimes Ns Ps)
      [N::Ns] Ps -> [N : (getPrimes Ns [N::Ps])])

;; 同様に最初の１００個の素数を得る
(let ((lazy-primes (\\getPrimes2 [2..] [])))
  (print (pgk:&take! 100 lazy-primes)))
EVAL===> 結果省略

;; 実装例３: あるいはlabelsに埋め込む方法も
(labels ((\letrec canDivide?
           _ [] -> false
           N [P::Ps] -> (if (= 0 (MOD N P)) true (canDivide? N Ps)))         
         (\letrec getPrimes
           [N::Ns] Ps -> (if (CANDIVIDE? N Ps)
                           (getPrimes Ns Ps)
                           [N : (getPrimes Ns [N::Ps])])))
  ;; 同様に最初の１００個の素数を得る
  (let ((lazy-primes (getprimes (\|2..) nil)))
    (print (pgk:&take! 100 lazy-primes))))
EVAL===> 結果省略

```
無限フィボナッチ数列の生成
```lisp
(flet ((\letrec fib A B -> [A : (fib B (+ A B))]))
  (let ((lazy-fibs (fib 0 1)))
    (pgk:&take! 20 lazy-fibs)))

EVAL===>
(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765)

```
埋め込みラムダ式によって返されるクロージャは、引数が不足している場合には自動的に部分適用される便利な仕様です
```lisp
(let ((f (\. X Y Z -> [Z Y X])))
  (print (funcall f 1 2 3))
  (print (funcall (funcall f 4 5) 6))
  (print (funcall (funcall (funcall f 7) 8) 9)))

EVAL===>
(3 2 1) 
(6 5 4) 
(9 8 7)
```
埋め込みリスト内包表記も可能
```
;; 変数A,Bと変数Cはパラレルに処理される
(\|[A B C] :: A<-[0..6],IF (EVENP A),B<-[x 'y z] : C<-[0..])

EVAL===>
((0 CLPGK.MSPACE::|x| 0) (0 Y 1) (0 CLPGK.MSPACE::|z| 2)
 (2 CLPGK.MSPACE::|x| 3) (2 Y 4) (2 CLPGK.MSPACE::|z| 5)
 (4 CLPGK.MSPACE::|x| 6) (4 Y 7) (4 CLPGK.MSPACE::|z| 8)
 (6 CLPGK.MSPACE::|x| 9) (6 Y 10) (6 CLPGK.MSPACE::|z| 11))

```
様々な埋め込み数列
```lisp
正格リスト(...を..に変えれば遅延リストとなる)
(\| 1 ... 10)
EVAL==>
(1 2 3 4 5 6 7 8 9 10)

(\| 0 2 ... 10)
EVAL==>
(0 2 4 6 8 10)

(\| 1 2.1 ... 10)
EVAL==>
(1 2.1 3.1999998 4.2999997 5.3999996 6.4999995 7.5999994 8.699999 9.799999)

(\| 1 ... -11)
EVAL==>
NIL

(\| 1 -1 ... -11)
EVAL==>
(1 -1 -3 -5 -7 -9 -11)

(\| 1 -1.1 ... -11)
EVAL==>
(1 -1.0999999 -3.1999998 -5.2999997 -7.3999996 -9.5)

(\| 1 ... (> 10))
EVAL==>
(1 2 3 4 5 6 7 8 9)

;; 第二要素と最終要素が関数の場合
;; 等差数列
(\| 0 (+ 3) ... (>= 30))
EVAL==>
(0 3 6 9 12 15 18 21 24 27 30)

;; 等比数列
(\| 1 (* 3) ... (> 100000))
EVAL==>
(1 3 9 27 81 243 729 2187 6561 19683 59049)

;; この形式の場合、別に数である必要もない
(\| "x" (/. X (concat "(" X ")")) ... (/. X (> 10 (LENGTH X))))
EVAL==>
("x" "(x)" "((x))" "(((x)))" "((((x))))")

                                         
無限リスト
(pgk:&take! 10 (\|10..))
EVAL==>
(10 11 12 13 14 15 16 17 18 19 20)

(pgk:&take! 10 (\|0 -1..))
EVAL==>
(0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10)
```
## ライセンスの特異性について  
CLPGKの提供する埋め込み関数型言語はQi(現在はshenという言語に進化したらしい）というCommon Lispで実装された非常にユニークな関数型言語のコアをゴリゴリに改変して実装しています。  
このQiには独自のライセンスが付属していて、簡単に言えば個人使用と教育目的は許諾するがクローズドな利用と商用利用は許諾しませんよ(クローズドな利用もしくは商用利用をしたければQiの解説書を買ってください)というものなのですが、この解説書というのが洋書かつ絶版で事実上日本国内では入手不可能であり、従ってクローズドな利用も商用利用も現時点ではもはや不可能ということになります。  
Qiのソースコード(Qiコンパイラの吐いたCLコードを含む)を利用した本ライブラリも自動的にこのQiライセンスに準拠しますので、Qiのコードに由来する埋め込み言語や関連ライブラリを利用した瞬間にクローズドソース及び商用には使えないという残念な事態が生じることになります。  
心苦しい限りですが、面倒くさいのでQi由来の部分はそのままQiライセンスを継承することにしました。  
教育目的には自由に使ってよいというのが救いです。  
  
**それ以外の部分はLLGPL**としておりますので自由度は高いです。  
