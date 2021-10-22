;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

;; テスト

(clpgk.core:clpgk-core-header)
(in-package :clpgk.base.text)


; #Verify このファイルがCRLF形式であることの確認
;(equal #"foo\r\nbar"
;       "foo
;bar")

#Verify キャリッジリターンが正しく無視されるかの確認
(equal #"foo
baz"
       #|`string\n'foo\nbaz|#)


#Testing STRING/SIMPLE
(check-unit '_
  (#|`string'|# "")
  (#|`string'abc|# "abc")
  (#|`string'abc
   def|# #"abc\n   def")
  )

#Testing STRING 標準ESC
(check-unit '_
  (#|`string'\a|# "\\a") (#|`string\a'\a|# #"\a")
  (#|`string'\b|# "\\b") (#|`string\b'\b|# #"\b")
  (#|`string'\f|# "\\f") (#|`string\f'\f|# #"\f")
  (#|`string'\n|# "\\n") (#|`string\n'\n|# #"\n")
  (#|`string'\r|# "\\r") (#|`string\r'\r|# #"\r")
  (#|`string'\t|# "\\t") (#|`string\t'\t|# #"\t")
  (#|`string'\v|# "\\v") (#|`string\v'\v|# #"\v")
  (#|`string'\0|# "\\0") (#|`string\0'\0|# #"\0")
  (#|`string'\a\b\f\n\r\t\v\0|# "\\a\\b\\f\\n\\r\\t\\v\\0")
  (#|`string\abfnrtv0'\a\b\f\n\r\t\v\0|# #"\a\b\f\n\r\t\v\0")
  )

#Testing STRING 特殊ESC
(check-unit '_
  (#|`string'\~|# "\\~")
  (#|`string\~'\~|# #"\\")
  (#|`string'hello\.world|# "hello\\.world")
  (#|`string\.'hello\.world|# #"hello")
  )

#Testing STRING * / ignore first line
(check-unit '_
  (#|`string*'hello|# "")
  (#|`string*'abc
   def
   |# #"   def\n   ")
  )

#Testing STRING _ / ignore beginning spaces
(check-unit '_
  (#|`string_'  hello|# "hello")
  (#|`string_'abc
   def
   |# #"abc\ndef\n")
  (#|`string*_'abc
   def|# #"def")
  )

#Testing STRING . [2018-04-09] 
(check-unit '_
  (#|`string.'hello|# "ello")
  (#|`string.'.abc
   .def
   |# #"abc\n  .def\n  ")
  (#|`string_.' .abc
   .def|# #"abc\ndef")
  (#|`string_.\.' .ab\.c
   .d\.ef|# #"ab\nd")
  )

#Testing STRING + / JOIN
(check-unit '_
  (#|`string.+_'.hello
   .world|# "helloworld")
  (#|`string.+_\n'.hello\n
   .world\n|# #"hello\nworld\n")
  )

#Testing STRING + / CHOMP
(check-unit '_
  (#|`string/\n'hello\n|#"hello")
  (#|`string_./\.'  .hello
   .world\.xxx
   .!\.yyy
   |# #"hello\nworld\n!")
  )

#Testing STRING/1
(check-unit '_
  (#|`string'foobar|# "foobar")

  (#|`string*'foobar|# "")
  (#|`string*'foobar
   baz|# "   baz")
  (#|`string_'foobar|# "foobar")
  (#|`string_'foobar
   baz|# #"foobar\nbaz")
  (#|`string*_'foobar|# "")
  (#|`string*_'foobar
   baz|# "baz")
  )

#Testing STRING/2
(check-unit '_
  (#|`string*_.\.'foobar
   .line-1\.
   .line-2\.|# #"line-1\nline-2")
  (#|`string!._\a0.~'.hello\n\a
  .aaa  あいう えお\.aaa\.
  .ok   \0ng
  .123\.abc
  .\~. 
  \.hello
   |#
   #"hello\\n\a
aaa  あいう えお
ok   \0ng
123
\\. 
.hello
")
  )

#Testing LINES-1
(check-unit '_
  (#|`lines'|# '(""))
  (#|`lines'foo
   bar|# '("foo" "   bar"))
  (#|`lines_'|# '(""))
  (#|`lines_'foo
   bar|# '("foo" "bar"))

  (#|`lines_'|# '(""))
  (#|`lines*'foo
   bar|# '("   bar"))
  (#|`lines_*'|# '(""))
  (#|`lines_*'foo
   bar|# '("bar"))
  
  )


#Comment

(defun =~ (regex target)
  (when (scan regex target) T))

(defun !~ (regex target)
  (when (not (scan regex target)) T))

(scan (create-scanner "^foo*$") "fooooooooo")

(regex-replace "f(o)o" "foofoofoo" "b")

(cl-ppcre:all-matches-as-strings "(foo)|(bar)|(boz)" "barfoobazbozd")
(cl-ppcre:all-matches-as-strings "(o+)[a-c]" "barfoobazbodzd")
(cl-ppcre:all-matches "(foo).*(bar)" "foofoobar")

cl-ppcre:

                

;;(tagbody (go bar) foo (print 2) bar (print 3))

(do-matches (s e reg tgt) body)
(scan "," "a,b,c,d,ef")
(let ((target "q,,,xa,,,,b,c,d,ef,,g,,,,j,"))
  (<do-unmatches> (s e ",," target 'okok) (print (subseq target s e))))

(do-matches (s e "" "a b c") (print s))

    
(<do-unmatches> (s e regex source ret) e1 e2)

(let ((target "00,x,a,b,cde"))
  (<do-unmatches> (s e ",x" target 'ok) (print (subseq target s e))))
(let ((target "00,x,a,b,cde"))
  (<do-unmatches-as-strings> (s "," target 'ok) (print s)))
    

(all-matches "fo" "fofofo")
(text-matches "fo+" "foobarfobarfooobarfffo" :limit 0 :from-end t)


  
(text-replace "foo" "drz" "barfoobafoo1foo2"  :limit 1)

(let ((a "foobar")) (equal a (regex-replace "faoo" a "baz")))

(
            
(text-split "ab" "abfooabcdabcab" 12)
(text-split "ab" "abababfooabcdabcababab" :limit 1)

(defmacro do-text-matches ((var-or-vars regex target &optional result-form) &body body)
  (let ((vars (cond ((symbolp var-or-vars) (list var-or-vars))
                    ((consp var-or-vars) var-or-vars)
                    (t (error "errr")))))
    (case (length vars)
      (1 `(do-matches-as-strings (,(first vars) ,regex ,target ,result-form) ,@body))
      (2 `(do-matches (,(first vars) ,(second vars) ,regex ,target ,result-form) ,@body))
      (3 (with-gensyms (src start end)
           `(let ((,src ,target))
              (do-matches (,start ,end ,regex ,src ,result-form)
                (let ((,(first vars)  ,start)
                      (,(second vars) ,end)
                      (,(third vars)  (subseq ,src ,start ,end)))
                  ,@body))))))))

(do-text-matches (s "o" "foobarboo") (print s))
(do-text-matches ((s e) "o" "foobarboo") (print (list s e)))
(do-text-matches ((s e r) "o" "foobarboo") (print (list s e r)))


;; UNMATCHES
;; todo text-unmatches

(defmacro do-text-split (() regex target
    
(in-package :cl-ppcre)

(scan (create-scanner "/(.*)" :extended-mode t :multi-line-mode t) "a/a/")

(split (create-scanner "[xy ]+") "axxxxbxyxyxyyy c")

"\\a"
(quote #"\\a")

regex-match
regex-parse

(

(cl-ppcre:parse-string "/(x+)foo(y+)/")
(#|`compiled-regex'foo|#)
'(#|`scanner'([fo]+)|# "unkfoobarbaz")

#Testing scanner and compiled-scanner
(macrolet ((tst (scanner-1 scanner-2 target) `(multiple-value-list (,scanner-1 ',target))
(check-unit* (:test equalp) (multiple-value-list (_ 
  (3 3))

                                                 
                                                 
