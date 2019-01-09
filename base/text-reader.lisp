;; -*- coding: utf-8 -*-
;; J-TEXT.LISP  Time-stamp: <2019-01-09 11:41:07 user> [autotitle,incremental]
(oleo.core:oleo-core-header)

(in-package :oleo.base.text)


(defmacro <compiled-regex> (regex)
  `(memoized (create-scanner ,regex)))

(defun <build-commentary> (lines)
  (unless lines 
    (return-from <build-commentary> (make-string "")))
  (let* ((pos (block escape
                (do ((i 0 (1+ i))
                     (first t t)
                     (mode #\;))
                    ((eql mode #\Space)
                     i)
                  (dolist (x lines)
                    (when (>= i (length x))
                      (return-from escape i))
                    (unless (eql mode (char x i))
                      (if (and first 
                               (eql #\Space (char x i)))
                        (setq mode #\Space)
                        (return-from escape i)))
                    (setq first nil)))))
         (contents (mapcar #/(subseq _ pos) lines)))
    (apply 'string-concat
           (cons (first contents) (mapcar #/(concatenate 'string #(#\Newline) _)
                                          (cdr contents))))))

(defun <dynamic-document?> (src)
  (scan (<compiled-regex> "\\\\{(.*?})\\\\")
        src))

(defun <to-dest> (vec)
  (when vec
    (read (make-string-input-stream (svref vec 0)))))

(defun <be-dynamic-if-possible> (dest src &optional exps)
  (multiple-value-bind (begin end s-match e-match) 
      (<dynamic-document?> src)
    (cond (begin 
            #{let ((sub (subseq src (svref s-match 0) (svref e-match 0))))
            (<be-dynamic-if-possible> 
             dest
             (string-concat (subseq src 0 begin) 
                            (subseq src end))
               (nconc (nreverse (read-delimited-list #\} (make-string-input-stream sub)))
                      exps)))
          ((or exps dest)
            `(format ,(<to-dest> dest) ,src ,@(nreverse exps)))
          (t src))))
          

(defun <read-next-char-without-spaces> (stream)
  (let ((c (read-char stream nil)))
    (if (member c '(#\Space #\Tab #\Newline))
      (<read-next-char-without-spaces> stream)
      c)))

(defgeneric <regex-dispatch> (cmd args))

(defun  <make-flag> () (cons nil nil))
(defun <flag-set?> (flag)
  (car flag))
(defun <flag-write>  (flag x)
  (setf (car flag) t
        (cdr flag) x))
(defun <flag-read> (flag)
  (cdr flag))

(defmacro <opt/scan2str> (reg-form)
  #{let ((flag (<make-flag>)))
  `(let ((flag ',flag))
    (cond ((<flag-read> flag)
            (let ((result ,reg-form))
              result))
          (t 
            (multiple-value-bind (a b) ,reg-form
              (cond ((and #!(<flag-set?> flag) (zerop (length b)))
                      (<flag-write> flag t)
                      a)
                    (t b)))))))
    

(defmacro <opt/scan> (reg-form)
  `(multiple-value-bind (start end v1 v2)
    ,reg-form
    #{let ((len (length v1)))
    (if (zerop len)
      (vector start end)
      (let ((vec (make-array (* 2 len))))
        (dotimes (i len vec)
          #{let ((idx (* 2 i)))
          (setf (svref vec idx) (svref v1 i)
                (svref vec (1+ idx)) (svref v2 i)))))))
      
      
      
      
;(defmacro <compiled-regex> (regex)
;  (create-scanner regex))

(defmethod <regex-dispatch> ((_ (eql #\~)) args)
  (declare (ignore _))
  #{let ((reg (create-scanner (car args))))
  `(lambda (x) (scan ,reg x)))


    
    

(defparameter *<comment-converter-callbacks>* nil)

(defmacro define-comment-converter (ident-str
                                    (content-var &optional options-var acceptable-options)
                                    &body body)
  `(<register-comment-converter> ,ident-str
                                 (lambda (,content-var ,(if options-var options-var '|option|))
                                   ,@(unless options-var
                                       `((when |option|
                                           (error "#|`~A'...|# cannot take an option" ,ident-str))))
                                   ,@(when (and options-var acceptable-options)
                                       (unless (consp acceptable-options)
                                         (setq acceptable-options (coerce acceptable-options 'list)))
                                       `((dolist (|opt| ,options-var)
                                           (unless (member |opt| ',acceptable-options)
                                             (error "#|`~A'...|# but ~A" ,ident-str ,options-var))))
                                       )
                                   ,@body)))

(defun <register-comment-converter> (ident-str callback)
  (aif (assoc ident-str *<comment-converter-callbacks>* :test #'equal)
       (setf (cdr it) callback)
       (push (cons ident-str callback) *<comment-converter-callbacks>*))
  ident-str)




;(read-from-string "1")
;(read-all-from-string "    1 2 hel 3 (+ 1 2) #|`string'ok|# " )
;(eval '(read-all-from-string "    1 2 hel 3 (+ 1 2) #|`string'ok|# " ))
;(read-all-from-string "1 'f 2")
  
;(read-from-string "" nil 'eof)


(defun <dispatch> (command option-str source)
  (let ((options (when option-str (coerce option-str 'list))))
    (aif (assoc command *<comment-converter-callbacks>* :test #'equal)
         (funcall (cdr it) source options)
         (error "dispatch error ~A" command))))
;; オプション付きの場合 #|`command{option}...'...|#
      


;; (defun <parse-comment> (s)
;;   (multiple-value-bind (start length begin/positions end/positions)
;;       (scan (memoized (create-scanner "([-a-zA-Z*]+)'(.*)" :multi-line-mode t))
;;             s)
;;     (declare (ignore length))
;;     (when start
;;       (bind ((#(b1 b2) begin/positions)
;;              (#(e1 e2) end/positions))
;;         (declare (ignorable e2))
;;         (values (subseq s b1 e1)
;;                 (subseq s b2))))))


@inline
(defun <char-escape> (c)
  (case c
    (#\a #\Bel)
    (#\b #\BackSpace)
    (#\f #\Page)
    (#\n #\LineFeed)
    (#\r #\Return)
    (#\t #\Tab)
    (#\v #\Vt)
    (#\0 #\Nul)
    (t c)))

(defun <register-readers> ()

  ;; #"cstring" c-style string format
  (set-dispatch-macro-character 
   #\# #\"
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (let (escaped
           tmp)
       (do ((c (read-char stream) (read-char stream)))
           ((eq c #\")
            (coerce (nreverse tmp) 'string))
         (cond (escaped
                 (setq escaped nil)
                 (push (<char-escape> c) tmp))
               ((eq c #\\)
                 (setq escaped t))
               ((eq c #\Return)
                 ;; ソース上に現れるWinのキャリッジリターンは無視する
                 )
               (t  (push c tmp)))))))
     
     '(set-dispatch-macro-character 
   #\# #\;
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (let* ((head (read-line stream nil t))
            (dest (second (multiple-value-list 
                            (scan-to-strings 
                             (<compiled-regex> "^\\s*destination\\s*=\\s*(.*?)\\s*;")
                             head))))
            tmp)

       (do (c)
           ((not (eql #\; (setq c (<read-next-char-without-spaces> stream))))
            (when c (unread-char c stream))
            (<be-dynamic-if-possible> dest (<build-commentary> (nreverse tmp))))
         (push (read-line stream) tmp)))))


  ;; 入力マクロ #| |#
  ;; 高機能な文字列リテラル　（コメントではない）
  ;; 内部では\nと\tが使える。それ以外では\は単に\として扱われる。
  (set-dispatch-macro-character
   #\# #\|
   (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     #{let* ((fstchar (read-char stream))
             (functional? (eql fstchar #\`)))
     
     (unless functional?
       (unread-char fstchar stream))
     
     (do (c 
          tmp
          single-quotation-appeared?
          command-ident
          command-option
          command-immediately? ignore-beginning-spaces? join-lines? ignore-first-line? ignore-initial-chars?
          need-chomp?
          (readable? t)
          escaped-chars
          backslash-flag
          (beginning-flag t)
          end-flag)
         ((and (eql #\# (setq c (read-char stream)))
               end-flag)
          (if functional?
            (if single-quotation-appeared?
              (let* ((source (cond (ignore-first-line?
                                     (string "")) ;; 改行が現れぬまま終端に達した場合
                                   ((and need-chomp? (cdr tmp) (eq #\Newline (second tmp)))
                                     (concatenate 'string (nreverse (cddr tmp)))) ;; DO CHOMP
                                   (T
                                     (concatenate 'string (nreverse (cdr tmp))))))
                     (result (<dispatch> command-ident command-option source)))
                (if command-immediately?
                  (eval result) ;; 即時評価
                  result))
              (error "invalid command ~W" command-ident))                
            (values) ;; commented out
            ))
       
       (when functional?
         (when (and (eq c #\') (not single-quotation-appeared?))
           (setq single-quotation-appeared? T)
           (let ((cmdline (concatenate 'string (nreverse tmp))))
             (multiple-value-bind (s e starts ends)
                 (scan (memoized (create-scanner "^([a-zA-Z0-9:-]+)([+_*./!]*)(?:(?:{([a-zA-Z0-9]*)})|)(?:(?:\\\\([abfnrtv0.~]*))|)$"))
                       cmdline)
               (declare (ignore e))
               (unless s (error "invalid format #|`~A'...|#" cmdline))
               (setq command-ident  (subseq cmdline 0 (svref ends 0)))
               (when (not (eq (svref starts 1) (svref ends 1)))
                 (let ((options (subseq cmdline (svref starts 1) (svref ends 1))))
                   (when (position #\! options) (setq command-immediately? T))
                   (when (position #\_ options) (setq ignore-beginning-spaces? T))
                   (when (position #\+ options) (setq join-lines? T))
                   (when (position #\. options) (setq ignore-initial-chars? T))
                   (when (position #\* options) (setq ignore-first-line? T))
                   (when (position #\/ options) (setq need-chomp? T))
                   ))
               (awhen (svref starts 2) (setq command-option (subseq cmdline it (svref ends 2))))
               (awhen (svref starts 3) (setq escaped-chars  (subseq cmdline it (svref ends 3))))
               ))
           ;; reset buffer
           (setq tmp nil
                 c   (read-char stream))
           )

         (when backslash-flag
           (setq backslash-flag nil)
           (when (and escaped-chars (position c escaped-chars))
             (cond ((eq c #\.)
                     (setf (car tmp) #\Newline
                           readable? nil
                           beginning-flag nil
                           c         (read-char stream)))
                   ((eq c #\~)
                     (setf (car tmp) #\\
                           c         nil))
                   (T
                     (setf (car tmp) (<char-escape> c)
                           c         (read-char stream)))))
           )
         
         #; "改行コードの扱い"
         @todo
         (unless (eq c #\Return)
           (when (and readable? c)
             (push c tmp))))
       (setq end-flag (eq #\| c))
       
       (when (and single-quotation-appeared? (not end-flag))
         (when beginning-flag
           (cond ((and ignore-beginning-spaces? (or (eq #\Space c) (eq #\Tab c)))  (pop tmp))
                 ;;((and beginning-underbars-as-spaces? (eq #\_ c))  (setf beginning-flag :UNDERBAR
                 ;;                                                        (car tmp) #\Space))
                 (t
                   (setq beginning-flag nil)
                   (when ignore-initial-chars?
                     (pop tmp)
                     (unless (eq #\Newline c)
                       (setq c nil)
                         )
                   )))
                 
           )
         (when (eq #\Newline c)
           (setq beginning-flag T
                 readable?      T)
           (when join-lines?  (pop tmp))
           (when ignore-first-line?  (setq ignore-first-line? nil
                                           tmp nil)))
         (setq backslash-flag (eq #\\ c))
         ;;(unless readable? (pop tmp))
         ))))

)

(register-reader-registerer '|text| '<register-readers>)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; End Of Main J-TEXT.LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#Comment


'(#|`string!._\a.~'.hello\n\n
  .doya  美織 どやねん\.aaa\.
  .ok   ng
  .123\.abc
  .\~. 
  .hello
  |#)

'(#|`string'foo

xx
|#)

      

(<foo> "`document'foobar")

****OBSOLETE****
入力マクロ #;
ヒアドキュメントを提供する
最も単純な使い方としては、
#; ...
; ...
; ... (‥非コメント行もしくはEOFまで)
これは、"...(改行)..."という文字列に変換される。※最終行は改行されない
なお、#;
に続く部分は改行まで無視される

先行する(最短の)スペースは無視される
#;
;  a
; b
;   c
の場合、" a(改行)b(改行)  c"となり、先行するスペースは１文字とみなされる。

動的なヒアドキュメントとしての使用 ※(format ...)の形を生成する
同一行中に、\{...}\ がある場合、ヒアドキュメントとみなす。
...の部分は、０個以上のS式であり、現れた順序でformatの引数として渡される。
よって、\{...}\はどこにあっても差し支えないが、フォーマット指定子の直後に
置くことによって視認性が向上する利点がある
#;
; the first answer is ~D or ~D\{a b}\.
; the next answer is ~D\{c}\.
の場合、(format nil "the first answer is ~D or ~D.(改行)the next answer is ~D." a b c)
を生成する。
動的なヒアドキュメントの場合、formatに渡すdestinationを指定できる
#;destination=dest; ...
ここで、...から改行までは無視される。
#;
と
#;destination=nil;
は、同じことである。






(progn #;
  ;\{}\
  )

(format nil


(<dynamic-document?> "\\{a}\\")

(scan-to-strings (<compiled-regex> "^\\s*destination\\s*=\\s*(.*?)\\s*;") 
                 "   destination = nil;")

(#|if\n\too|#)


(let ((a "a")) (eq (format nil a) (format nil a)))

(progn #;destination=(foobar b);
  ;; hello
  ;; world
  )

(defun foo (a b) #;destination=(progn t); `ok'
  ;; foo~D ~D \{a (1+ a)}\ baz ~D\{'man}\
  ;; and ~C\{#\newline}\~D\{b}\bar
  ;; おま~%~D\{(+ a b 1000000)}\ ~*\{nil}\ちん
  )
(foo 10 200.2)

(read-delimited-list #\} (make-string-input-stream "1 2 3}") )

(




'(#;
  ;; a
  ;; b
  )
(format t "a")

(format nil "3")
(scan-to-strings "(\\S+)\\s(.+)" "aaaaa 3")

(foo #;
 ;; Hello\{~D 'ok }\World
 ;; bak\{~A (a b c)}\
 )

(foo "ok")

(


(j-header)
(scan "\\\\{\\s*(\\S+)\\s+(.+)}\\\\"
      #;
      ;; foo
      ;; get \{~Dx (foo    bar  ) }\
      ;;           \{~A y}\
  ;; bar barz
      )

'(#;
                                        ; MAN
                                        ; de
                                        ; f

)

(scan-to-strings (<compiled-regex> "^\\s*(\\d+)\\s*(\\w+)\s*$") "     11 aa")
(scan (<compiled-regex> "^((\\d+)|foo(\\w+))|xx([a-c]+)") "xxaca")

(scan-to-strings (<compiled-regex> ";*\s*" 1 ")

(mapcar #'eq (multiple-value-list (scan "" ""))
        (multiple-value-list (scan "" "")))

(scan "a|(\\d+)" "100")

(#|=^\s*(\d+)\s*$|# src) 

(in-package :jun)
(do-unify 3 [integer])



(parse-integer "1234")

#;3
;; <helo><this|>$<x$>

#|\w+|#
(scan-to-strings "\\s*(\\d+)\\s+(\\d+)" "   32 88    ")
#|`(\d+),(\w+)|#
(multiple-value-bind (a b) nil (list a b))
(scan-to-strings "\\w+" "  abc d  " :sharedp t)
(scan "\\s*(\\d,x)\\s*(\\d+)\\s*-" "     0,x 000-")
(scan "^\\s*(\\d+\\s*)$" "      32  ")
(length "\\\\")
(scan "\\b" " ")
(scan "(\\w)+\\s+(\\d+)" "a 87")
(scan "(\\w)""abc")
#|s/fobar/|#

(#|s/^(\d+)\s*,\s*(\w+)/this/g|#
(scan "^a{2,3}$" "aaa")
 (a b) exp
(multiple-value-bind (a b c) (values 0 1 2)
                     (list a b c))
 (multiple-value-list (values 0 1 2))
 
(read (make-string-input-stream "  10  "))
(subseq "" 10)


(scan "\




;;;;;;;;;;;;;;;;;;;;;;;;;;;; End Of J-TEXT.LISP ;;;;;;;;;;;;;;;;;;;;;;;;;


