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
```
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
```lisp
(in-package :cl-user)
(pgk:pgk-mode)

(\def tarai
      (! X) (! Y) _,(<= X Y)  -> Y
      (! X) (! Y) (& Z) -> (tarai (&(tarai (1- X) Y Z))
                                  (&(tarai (1- Y) Z X))
                                  (&(tarai (1- Z) X Y))))

(time (format t "Answer: ~A~%" (#~tarai 12 6 0)))
>>>
Answer: 12
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  460,313 processor cycles
  4,088 bytes consed
```

## ライセンスの特異性について  
CLPGKの提供する埋め込み関数型言語はQi(現在はshenという言語に進化したらしい）というCommon Lispで実装された非常にユニークな関数型言語のコアをゴリゴリに改変して実装しています。  
このQiには独自のライセンスが付属していて、簡単に言えば個人使用と教育目的は許諾するがクローズドな利用と商用利用は許諾しませんよ(クローズドな利用もしくは商用利用をしたければQiの解説書を買ってください)というものなのですが、この解説書というのが洋書かつ絶版で事実上日本国内では入手不可能であり、従ってクローズドな利用も商用利用も現時点ではもはや不可能ということになります。  
Qiのソースコード(Qiコンパイラの吐いたCLコードを含む)を利用した本ライブラリも自動的にこのQiライセンスに準拠しますので、Qiのコードに由来する埋め込み言語や関連ライブラリを利用した瞬間にクローズドソース及び商用には使えないという残念な事態が生じることになります。  
心苦しい限りですが、面倒くさいのでQi由来の部分はそのままQiライセンスを継承することにしました。  
教育目的には自由に使ってよいというのが救いです。  
  
**それ以外の部分はLLGPL**としておりますので自由度は高いです。  
