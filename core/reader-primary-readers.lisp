;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(in-package :clpgk.core.reader)
(enable-annot-syntax)

(defvar *<tmp-pkg>* (make-package "j-header-tmp"))

(defun <read-lpar-slash-ident> (stream)
  (let ((first (read-char stream)))
    (cond ((null first) (error "(\\...: unexpected stream termination"))
          ((alphanumericp first)
            (unread-char first stream)
            (string-upcase 
             (reader/take-chars-while stream
                                      (lambda (c) (not (member c '(#\) #\Space #\Newline #\Tab #\Return)))))))
          (t (coerce (list first) 'string)))))

(defun <register-primary-readers> ()

  ;; mspaceへのシンボルのインターン
  ;; ~fooBar は USER::|fooBar|のショートカットとなる
  ;; * < % で始まる長さ１以上シンボル、及び、_で始まる長さ２以上のシンボルは
  ;; ローカルなパッケージ（呼び出し元パッケージ）にインターンされる
  ;; (set-macro-character
  ;;  #\~
  ;;  (lambda (stream char1)
  ;;    (declare (ignore char1))

  ;;    (let (result
  ;;          (cur-pkg *package*))
  ;;      (let* ((*package* (clpgk.base:memoized (find-package :clpgk.mspace)))
  ;;             (*readtable* CLPGK.LE.CORE:*QI-READTABLE*)
  ;;             (src (read stream t nil t)))
  ;;        (setf result (if (CLPGK.LE.CORE::%need-trans-p% src)
  ;;                       (let ((*package* cur-pkg))
  ;;                         (CLPGK.LE.CORE::%trans% src))
  ;;                       src)))
  ;;      result)))
  @select-reader (:~ :mspace)
  (set-dispatch-macro-character
   #\# #\~
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (let ((*package* (memoized (find-package :clpgk.mspace)))
           (*readtable* *<readtable/case>*))
       (read stream t nil t))))


  ;; 入力マクロ #}
  ;; #}exp -> (cl-cont:without-call/cc exp)
  @select-reader (:~)
  (set-dispatch-macro-character 
   #\# #\}
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (list 'cl-cont:without-call/cc (read stream t nil t))))


  ;; ヒストリ追加用入力マクロ #H
  ;; #T...改行までを文字列としてヒストリに追加する
  @select-reader (:h :history)
  (set-dispatch-macro-character 
   #\# #\H
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (do (tmp 
          c)
         ((eq #\Newline (setq c (read-char stream nil))) ;;行末まで読み飛ばす
          (let* ((line (coerce (nreverse tmp) 'string))
                 (hist-str (string-concat (package-name *package*) ": " line)))
            `'(<add-history> ,hist-str)
            (values)))
          (push c tmp))))

  ;; 入力マクロ #C
  ;; ファイルの末尾までをコメントアウトする
  @select-reader (:c :comment)
  (set-dispatch-macro-character
   #\# #\C
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (do ()
         ((null (read-char stream nil))
          (values)))))

  ;; 入力マクロ #!
  ;; expの否定 #!exp
  @select-reader (:! :not)
  (set-dispatch-macro-character 
   #\# #\!
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (list 'not (read stream t nil t))))
  
  ;; 入力マクロ #/
  ;; ラムダ式 #/exp -> (lambda (_) exp)
  @select-reader (:/ :lambda)
  (set-dispatch-macro-character 
   #\# #\/
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     `(lambda (_) (declare (ignorable _)) ,(read stream t nil t))))
  
  ;; 入力マクロ #>
  ;; 変数 _ への投入 (一引数ラムダ式を生成する入力マクロ#/と共に使う）
  ;; #>foo -> (foo *)
  ;; #>(a b 'c) -> (a (b (funcall 'c *)))
  @select-reader (:>)
  (set-dispatch-macro-character 
   #\# #\>
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (let ((e (read stream t nil t)))
       (aif (<combine/helper> e '_)
            it
            (nconc e (list '_))))))
  
  ;; 入力マクロ #@
  ;; #@exp1 は、exp1を捨てる
  ;; 通常は#@シンボルとして、コメントとして利用する
  @select-reader (:discard)
  (set-dispatch-macro-character 
   #\# #\@
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     ;; １つめのS式は読み捨てる
     (let* ((*package* *<tmp-pkg>*)
            (exp (read stream t nil t)))
       (when (and (symbolp exp)
                  (eq (symbol-package exp) *<tmp-pkg>*))
         (unintern exp)))
     (values)))

  ;; TODO #Warn warn-msgline
  @select-reader (:warn :warning)
  (set-dispatch-macro-character 
   #\# #\;
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     ;;行末まで読み飛ばす
     (do (c
          msg)
         ((eq #\Newline c)
          (setq msg (member-if-not (lambda (x) (member x '(#\Space #\tab #\;)))
                                   (nreverse (cdr msg))))
          (unread-char #\Newline stream)
          (console-message "MSG/TODO: ~A" (coerce msg 'string))
          )
       (setq c (read-char stream nil))
       (push c msg)
       )
     (values)
     ))

  ;; 入力マクロ #F
  ;; エラーチェック
  ;; #F exp は、(has-errors exp)に展開される
  ;; 通常、#Vと併用する
  @select-reader (:f :fail)
  (set-dispatch-macro-character 
   #\# #\F
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     `(has-errors ,(read stream t nil t))))

  ;; 入力マクロ #E
  ;; マクロ展開
  ;; #E exp は、(macroexpand-1 'exp)に展開される
  ;; 通常、#Vや#Fと併用する
  @select-reader (:e :expand)
  (set-dispatch-macro-character 
   #\# #\E
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     `(macroexpand-1 ',(read stream t nil t))))
  
  ;; これはいまのところ保留 組み込みの文字列リーダの挙動がおかしくなるので
  ;; 入力マクロ \
  ;; \a.b.c (foo a b c) -> (lambda (a b c) (foo a b c))
  '(set-macro-character 
   #\\
   (lambda (stream char)
     @ignore (char)
     
     (let ((params (mapcar #'intern 
                           (split-sequence #\. (symbol-name (read stream t nil t)))))
           (body (read stream t nil t)))
       `(lambda ,params ,body))))


  ;; 入力マクロ #&
  ;; コンバイン
  ;; #&(a 'b (c q) #'d) -> (lambda (#:x) (a (funcall 'b (c q (funcall #'d #:x)))))
  ;; #&a -> (lambda (#:x) (a #:x))
  ;; #&car.1+.list -> (lambda (#:x) (list (1+ (car #:x))))
  ;; #&'a -> (lambda (#:x) (funcall 'a #:x))
  ;; #&#'a -> (lambda (#:x) (funcall #'a #:x))
  ;; #&((a b c)) -> (lambda (#:x) (a b c #:x))
  ;; #&((a (lambda (x) (b x)))) -> (lambda (#:x) (a ((lambda (x) (b x)) #:x)))
  @select-reader (:& :combine)
  (set-dispatch-macro-character 
   #\# #\&
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (with-gensyms (x)
       `(lambda (,x) ,(<combine> (read stream t nil t) x "#&")))))

  ;; )が現れるまでの要素をまとめる
  ;; (defun ... #{when test #{case x (a 'foo) (b 'bar))
  ;; ==> (defun ... (when test (case x (a 'foo) (b 'bar))))
  ;; 先頭がコンスの場合、先頭がラムダ式とそうでない場合で挙動が異なる
  ;; 非ラムダ式の場合、先頭はカリー化されたフォームとして扱われる
  ;; (... #{(let ...) a b) => (... (let ... a b))
  ;; ラムダ式の場合は、単に要素をまとめるだけである
  ;; (... #{(lambda ...) a b) => ((lambda ...) a b)
  @select-reader (:{)
  (set-dispatch-macro-character 
   #\# #\{
   #'|#{-READER|)



  ;; 入力マクロ##
  ;; @where節を伴うHaskell風の記述を実現する
  ;; ## A... @where B...
  ;; A... は (progn A...) => A' にまとめられる
  ;; B... は A'を巻き込む形で展開される
  ;; (defun (a) ## (f (g a)) @where 
  ;;   let ((a 100)) 
  ;;   flet ((f (x) (list x x)) (g (x) (+ a x)))
  ;;   (list 'x 'y 'z)
  ;;
  ;; letやfletのような、オペレータ名＋定義節を持つものは、オペレータ名と定義節を
  ;; 列挙する。(list)はそのようなオペレータでないので、カリー化形式で与える
  @select-reader (:#)
  (set-dispatch-macro-character 
   #\# #\#
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (let* ((xs (read-delimited-list #\) stream t))
            (defs (cdr (member 'where xs)))
            (body (car (split-sequence 'where xs :count 1))))
       (unread-char #\) stream)
       (do ((xs defs (cdr xs))
            (ys))
           ((null xs) 
            (reduce  (lambda (a b) (nconc a (list b)))
                     (nreverse ys) :from-end t :initial-value `(progn ,@body)))
         (cond ((atom (car xs))
                 (push (list (car xs) (cadr xs)) ys)
                 (unless (cdr xs) 
                   (error "reader-macro ##: invalid @where clause ~D" xs))
                 (setq xs (cdr xs)))
               (t (push (car xs) ys)))))))

  @select-reader (:embed)
  (set-macro-character
   #\(
   (lambda (stream char1)
     (declare (ignore char1))
     (let ((c (read-char stream)))
       (cond ((eq #\\ c)
               (let* ((ident (<read-lpar-slash-ident> stream))
                      (reader-function (gethash ident *lpar-backslash-reader-table*)))
                 (unless reader-function
                   (error "(\\~A .... is wrong format" ident))
                 (funcall reader-function stream)))
             ((eq #\. c) ; TODO
               (unread-char c stream)
               (let* ((elems (|(-READER| stream #\())
                      (fn (first elems))
                      (params (rest elems)))
                 (unless (fboundp fn)
                   (error "(~A ...) is wrong format" fn))
                 (apply fn params)))
             (t 
               (unread-char c stream)
               (|(-READER| stream #\())))))

       
  )

(defun <need-splicing?> (e)
  (and (consp e)
       (eq (car e) '|need-splicing|)))

(defun <combine/helper> (x param)
  (if (symbolp x)
    (cond ((find-if (lambda (x) (eq x #\.)) (symbol-name x))
            (reduce (lambda (f p)
                      (if (<need-splicing?> p)
                        `(,f ,@(cdr p))
                        (list f p)))
                    (mapcar #'intern 
                            (nreverse (split-sequence #\. (symbol-name x))))
                    :initial-value param :from-end t))
          (t (if (<need-splicing?> param)
               `(,x ,@(cdr param))
               (list x param))))
    (case (car x)
      (LAMBDA (if (<need-splicing?> param)
                `(,x ,@(cdr param))
                (list x param)))
      ((QUOTE FUNCTION) (if (<need-splicing?> param)
                          `(FUNCALL ,x ,@(cdr param))
                          (list 'FUNCALL x param))))))

(defun <combine> (a param operator-name)
  (unless (or (symbolp a)
              (and (consp a) 
                   (every (lambda (x) (or (symbolp x) 
                                          (and (consp x)
                                               (symbolp (car x)))))
                          a)))
    (error "reader-macro ~D: syntax error: ~D" operator-name a))
  (aif (<combine/helper> a param)
       it
       (reduce (lambda (f p) (aif (<combine/helper> f p)
                                  it
                                  (append f (if (<need-splicing?> p)
                                              (cdr p)
                                              (list p)))))
                  (reverse a)
                  :from-end t :initial-value param)))

;;OBSOLETE
'(set-dispatch-macro-character 
 #\# #\L
 (lambda (stream char1 char2)
   (declare (ignore char1 char2))
   (let ((x (read stream t nil t))
         (main (read stream t nil t)))
     `(let ((_ ,x)) ,main))))


(register-reader-registerer '|primary| '<register-primary-readers>)

;; OBSOLETE
'(set-dispatch-macro-character 
 #\# #\_
 (lambda (stream char1 char2)
   (declare (ignore char1 char2))
   (let ((e1 (read stream t nil t))
         (e2 (read stream t nil t)))
     (aif (<combine/helper> e2 e1)
       it
       (nconc e2 (list e1))))))

;; OBSOLETE
'(set-dispatch-macro-character 
 #\# #\@
 (lambda (stream char1 char2)
   (declare (ignore char1 char2))
   (let ((e1 (read stream t nil t))
         (e2 (read stream t nil t)))
     (unless (proper-list-p e1)
       (error "#@: ~D is not a proper list" e1))
     (aif (<combine/helper> e2 (cons '|need-splicing| e1))
          it
          (nconc e2 e1)))))

;; OBSOLETE
'(set-dispatch-macro-character 
 #\# #\$
 (lambda (stream char1 char2)
   (declare (ignore char1 char2))
   (let ((e1 (read stream t nil t))
         (e2 (read stream t nil t))
         (e3 (read stream t nil t))
         (tmp (gensym)))
     (unless (proper-list-p e3)
       (error "#@: ~D is not a proper list" e1))
     (let* ((params (mapcar (lambda (x)
                              (aif (<combine/helper> x tmp)
                                   it
                                   (nconc x (list tmp))))
                            e3))
            (code (aif (<combine/helper> e1 (cons '|need-splicing| params))
                       it
                       (nconc e1 params))))
       `(let ((,tmp ,e2)) ,code)))))
