<!--dd -*- coding: utf-8 -*- -->  
This file is part of CLPGK  
Copyright (c) 2019 PGkids Laboratory  

本パッケージは当社の業務で使うための大規模なライブラリ群と、リードマクロを用いた埋め込み関数型言語から成っております。
オレオレライブラリの典型的なやつです。  
当初公開する予定はありませんでしたが、埋め込み関数型言語が予想以上にいろいろな場面で便利に使えるため公開した次第です。  
ソースコードはところどころものすごく汚く乱雑ですが、何卒ご容赦を。

動作条件  
SBCLおよびCCLをターゲットとしています  
Allegroは導入していないのでわかりません  
CLISPは試みましたがお手上げでした  
オールインワンLisp環境のPortacleでも問題なく動作しました(SBCLなのでWindows版では下記の修正が必要ですが)。  

インストール方法  
Quicklispのlocal-projectsディレクトリ以下に展開後、(ql::register-local-projects)で登録、(ql:quickload :clpgk)でロードできます。  
Windows版のSBCLでは依存ライブラリであるkmrclのコンパイル中にエラーが出ると思いますが、signals.lispの当該箇所を条件コンパイルすることで回避できます。  
回避例:  
    #+sbcl (sb-sys:enable-interrupt signum handler)  
FIX:-->  
    #+(and sbcl unix) (sb-sys:enable-interrupt signum handler)  
    #+(and sbcl win32) (error "kmrcl:set-signal-handler is disabled (win32)")  

    #+sbcl (sb-sys:enable-interrupt signum (or old-handler :default))  
FIX-->  
    #+(and sbcl unix) (sb-sys:enable-interrupt signum (or old-handler :default))  
    #+(and sbcl win32) (error "kmrcl:remove-signal-handler is disabled (win32)")  

とりあえずWindowsで動かしてみる場合にはCCLのほうが手っ取り早いと思います。ただし、SLIME上で埋め込み関数型言語を扱うのならSBCLがおすすめです。  

ライセンスの特異性について  
CLPKGの提供する埋め込み関数型言語はQi(現在はshenという言語に進化したらしい）というCommon Lispで実装された非常にユニークな関数型言語のコアをゴリゴリに改変して実装しています。  
このQiには独自のライセンスが付属していて、簡単に言えば個人使用と教育目的は許諾するがクローズドな利用と商用利用は許諾しませんよ(クローズドな利用もしくは商用利用をしたければQiの解説書を買ってください)というものなのですが、この解説書というのが洋書かつ絶版で事実上入手不可能であり、従ってクローズドな利用も商用利用も不可能ということになります。  
Qiのソースを利用した本ライブラリも自動的にこのQiライセンスに感染しますので、Qiのコードに由来する埋め込み言語や関連ライブラリを利用した瞬間にクローズドソース及び商用には使えないという極めて残念な事態が生じることになります。  
たいへん心苦しい限りですが、面倒くさいのでQi由来の部分はそのままQiライセンスを継承することにしました。  
  
それ以外の部分はLLGPLとしておりますので自由度は高いです。  
