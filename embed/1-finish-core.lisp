;; -*- coding: utf-8 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package :oleo.embed.core :oleo.mspace)


(let* ((*readtable* OLEO.EMBED.CORE:*qi-readtable*))
  (set-dispatch-macro-character 
   #\# #\{ 
   #'oleo.base:|#{-READER|))

;; (DEFUN OLEO.EMBED.CORE:QILOAD (filename)
;;   (let ((*package* (find-package :Q))
;;         (*readtable* OLEO.EMBED.CORE:*qi-readtable*))
;;     (OLEO.EMBED.CORE:|load| filename)))


(in-package :oleo.embed.core)

(defvar *<loading-results>*)
  
(defun <execute-code> (c)
  (let* ((typing (when (consp c)
                  (case (first c)
                    (|define| (eql '|{| (third c)))
                    (|datatype| t)
                    (|declare| nil)
                    (|synonyms| t))))
    
         (result (|load-help| (if typing '|true| '|false|)
                              (list c))))
    (push result *<loading-results>*)))

(defun <execute-each-codes> (x)
  (cond ((and (consp x)
              (eql '|<xi/codes>| (car x)))
          (mapc #'<execute-each-codes> (cdr x)))
        (t
          (<execute-code> x))))
        

(defun %load% (V1)
  (setq |*tempsigs*| nil)
  (|initialise_environment|)
  (LET (*V-STACK*
        (|*inferences*| 0)
        (*<loading-results>* nil))
    (mapc #'<execute-each-codes> V1)
    (nreverse *<loading-results>*)))
    ;(|load-help| |*tc*| (THE LIST V1)))
  ;;(IF (EQ |*tc*| '|true|) 
  ;;  (|output| "~%~%typechecked in ~A inferences" (THE NUMBER (|inferences| '_))))
  ;;(TERPRI)
  ;;|loaded|
  ;)

(defun <special-symbol-p> (s)
  (member s '(|@failure|)))

;; パッケージローカルなシンボルとして扱うルール
;; < * + % @@で始まるシンボルはローカル扱いとする
;; core-03-reader_load_3.lispから呼ばれる
(defun %local-intern-needed-p% (name)
  (declare (type string name))
  (let ((c (char name 0)))
    (or (upper-case-p c)
        (and (member c '(#\~ #\< #\* #\+ #\%))
             ;; ただし、以下のものは例外とする
             (not (equal "<" name))
             (not (equal "<-" name))
             (not (equal "<=" name))
             (not (equal "+" name))
             (not (equal "*" name))
             )
        (and (eq c #\_)
             (not (string= name "_")))
        (and (eq c #\@) (> (length name) 1) (eq (char name 1) #\@))
        )))

;; core-03-reader_load_3.lispから呼ばれる
(defun %local-intern% (name pkg)
  (let ((*package* pkg)
        (c (char name 0)))
    (intern (if (eq c #\~)
              (string-upcase (subseq name 1))
              name))))

(defun %trans% (x)
  (cond ((symbolp x)   
          (if (<special-symbol-p> x)
            (case x
              (|@failure| (list '|<<xi-simple-fail>>| '|true|)))
            x))
        ((atom x) x)
        (t (do ((cur x (cdr cur))
                tmp)
               ((atom cur)
                (let ((result (nreverse tmp)))
                  (setf (cdr (last result)) (%trans% cur))
                  result)
                )
             (push (%trans% (car cur)) tmp)))))

(defun %need-trans-p% (x)
  (cond ((symbolp x) (<special-symbol-p> x))
        ((atom x) nil)
        (t (do ((cur x (cdr cur)))
               ((atom cur)
                (%need-trans-p% cur))                
             (when (%need-trans-p% (car cur))
               (return t))))))


(defmacro declare-for-qi (lisp-name qi-signature)
  (let* ((qi-name (if (get lisp-name '<qi-name>)
                    (get lisp-name '<qi-name>)
                    (setf (get lisp-name '<qi-name>)
                            (gensym (string-concat "qi;" (symbol-name lisp-name))))))
         (arity (count '~--> qi-signature))
         (tmp-vars (do ((tmp nil (push (gensym) tmp))
                        (n arity (1- n)))
                       ((zerop n)
                        tmp))))

    `(progn
      (defun ,qi-name ,tmp-vars (,lisp-name ,@tmp-vars))
      (define-compiler-macro ,qi-name ,tmp-vars (list ',lisp-name ,@tmp-vars))
      (~declare ',qi-name ',qi-signature)
      ',lisp-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun <register-readers> ()


  )

;(SETF (READTABLE-CASE *READTABLE*) :UPCASE)

;; (\リーダの定義

(defun <terminated?> (x)
  (or (not (characterp x))
      (member x '(#\" #\( #\) #\[ #\] #\. #\, #\Space #\Tab #\Return #\Newline))))

(defun qi-reader (stream &key transformer pre-execute)
  (do (c 
       tmp
       (paren-count (list 0))
       escape-flag
       lambda-flag
       float-flag tmp-digits
       reserve-op-mode
       bar-flag
       bar-mode-string-buffer
       end-flag)
      (end-flag
       (let* ((raw-chars (nreverse tmp))
              (chars (if transformer
                       (funcall transformer raw-chars)
                       raw-chars))
              (raw-parsed (let ((*<caller-package>* *package*)
                                (*package* (find-package :OLEO.MSPACE))
                                (*readtable* OLEO.EMBED.CORE:*qi-readtable*)
                                (errstr "Xi.Core: parse failure: ~%~%~{~C~} ..."))
                            (OLEO.EMBED.CORE::|compile| 'OLEO.EMBED.CORE::|<st_input>| chars errstr)))
                                        ;`(progn ,@(mapcar (lambda (x) `(XI.CORE::|eval| ',x))
                                        ;                  parsed))))
              (parsed (if (OLEO.EMBED.CORE::%need-trans-p% raw-parsed)
                        (OLEO.EMBED.CORE::%trans% raw-parsed)
                        raw-parsed))
              (form (if pre-execute
                      (let ((|*<definition-only-p>*| t))
                        (OLEO.EMBED.CORE::%load% parsed)
                        |*<definition-only-p>*|)
                      `(OLEO.EMBED.CORE::%load% ',parsed))))
         ;(WARN "~D" form)
         form
         ;; (if type-check
         ;;   `(let ((xi.core::|*tc*| ',type-check)) ,form)
         ;;   form)
         ))
    
    (unless (characterp (setq c (read-char stream)))
      (error "Xi.Core: unexpected end of stream"))
    

    (when reserve-op-mode
      (unless (member c '(#\- #\= #\< #\>))
        (cond (NIL (equal '(#\-) reserve-op-mode)
                ;; 一文字の-記号は、数値の符号またはシンボルの一部と見做す
                (push #\- tmp))
              ((let ((n (length reserve-op-mode)))
                 (and (> n 2)
                      (member (first reserve-op-mode) '(#\- #\=))
                      (= n (count (first reserve-op-mode) reserve-op-mode))))
                ;; ３文字以上連続する{=,-}は３文字。-は_に内部変換することに注意。
                (setq tmp (nconc (if (eql #\- (first reserve-op-mode))
                                   (list #\Space #\_ #\_ #\_ #\Space)
                                   (list #\Space #\= #\= #\= #\Space))
                                 tmp)))
                      
              (t
                ;;(setq tmp (nconc (list #\Space) reserve-op-mode (list #\Space) tmp)))
                (setq tmp (nconc reserve-op-mode tmp)))
              )
        (setq reserve-op-mode nil)
        ))
                      

    (when float-flag
      (setq float-flag nil)
      (if (digit-char-p c)
        (setq tmp-digits (list #\.))
        (setq tmp (nconc (list #\Space #\; #\Space) tmp))))

    (when (and tmp-digits (not (digit-char-p c)))
      (unless (<terminated?> c)
        (error "not term ~A" c))
      (setq tmp (nconc (cons #\Space tmp-digits) tmp)
            tmp-digits nil))
     
    
    (cond  (tmp-digits
             ;; ここでは(digit-char-p c)が必ず満たされていることに注意せよ
             (push c tmp-digits)
             )
           (lambda-flag
            (setq lambda-flag nil)
            ;;(push c tmp)
            (push #\/ tmp)
            (push #\. tmp)
            (push #\Space tmp)
            (push c tmp)
            )
          (escape-flag
            (setq escape-flag nil)
            (push c tmp))
          (bar-flag
            (cond ((and (eql c #\|) (eql #\\ (car bar-mode-string-buffer)))
                    ;; \| の形でエスケープされている場合
                    (setf (car bar-mode-string-buffer) #\|))
                  ((eql c #\|)
                    (let* ((str (coerce (nreverse bar-mode-string-buffer) 'string))
                           (form (text-compile-literal str)))
                      (if (eq 'LAMBDA (car form))
                        (let ((form `(lambda (x &aux (result (,form x)))
                                       (cond ((eq T result) '|true|)
                                             ((null result) '|false|)
                                             (t result)))))
                          (push (vector '|<lifted>| (|make_xi_internal_lambda| 'Var (list form 'Var) 'STRING)) tmp))
                        (push (vector '|<string>| form) tmp)))
                    (setq bar-mode-string-buffer nil)
                    (setq bar-flag nil))
                  (t (push c bar-mode-string-buffer)))
                                        ;(push c tmp)
              

            ;(setq bar-flag nil)
            ;(unless (eq c #\|)
                                        ;  (error "#Q .. }#: misplaced |")))
            )

          (reserve-op-mode
            (push c reserve-op-mode)
            (cond ((or (equal '(#\> #\-) reserve-op-mode) ;; ->
                       ;;(equal '(#\> #\- #\-) reserve-op-mode) ;; -->
                       ;;(equal '(#\> #\>) reserve-op-mode) ;; >>
                       (equal '(#\> #\=) reserve-op-mode) ;; =>
                       (equal '(#\- #\<) reserve-op-mode)) ;; <-
                    (setq tmp (nconc (list #\Space) reserve-op-mode (list #\Space) tmp))
                    (setq reserve-op-mode nil))
                  ;; ((equal '(#\> #\= #\=) reserve-op-mode) ; ==> to ___
                  ;;   (setq tmp (nconc (list #\Space #\_ #\_ #\_ #\Space) tmp))
                  ;;   (setq reserve-op-mode nil))
                  ;; ((equal '(#\> #\= #\<) reserve-op-mode) ; <=> to ===
                  ;;   (setq tmp (nconc (list #\Space #\= #\= #\= #\Space) tmp))
                  ;;   (setq reserve-op-mode nil))

                  )

            )
          ((eq c #\\) 
            (cond ((eq (car tmp) #\#) ;;文字リテラル#\における\を読み込んだ場合
                    (setq escape-flag t)
                    (push #\\ tmp))
                  ((eq (car tmp) #\()
                    (setq lambda-flag t))))

          ((member c '(#\- #\= #\< #\>))
            ;(push #\Space tmp)
            ;(push c tmp)
            (setq reserve-op-mode (list c))
            )

          ((and (eq c #\#) (consp tmp) (eql #\( (car tmp)))
            (setq tmp (nconc (list #\Space #\;) tmp)))

          ;; /. の場合は直後にスペースを挿入する
          ((and (eq c #\.) (eq #\/ (car tmp)) (<terminated?> (second tmp)))
            (push #\. tmp)
            (push #\Space tmp))

          ((and (eq c #\.) (consp tmp) (digit-char-p (car tmp))
                (<terminated?> (find-if-not (lambda (x) (and (characterp x) (digit-char-p x)))
                                            tmp)))
            (setq float-flag t)
            )
          
          ((and (eq c #\.)
                ;;(consp tmp)
                ;; 小数点表記 ddd.ddd の場合を除く
                ;;(not (digit-char-p (car tmp)))
                )
            (setq tmp (nconc (list #\Space #\; #\Space) tmp))) ;HACK

          ((and (eq c #\`)) (push (vector '|<lisp>| (read stream t nil t))
                                  tmp)); HACK
          ((eq c #\') (push (vector '|<lifted>| (list 'QUOTE (read stream t nil t)))
                            tmp))
          ((eq c #\:) (setq tmp (nconc (list #\Space #\| #\Space) tmp)))

          ;;;;;;;;;;;; OBSOLETE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; ((eq c #\：) (setq tmp (nconc (list #\Space #\: #\Space) tmp))) ;OBSOLETE
          ;; ((eq c #\⇒) (setq tmp (nconc (coerce " ___ " 'list) tmp)))
          ;; ((eq c #\⇔) (setq tmp (nconc (coerce " === " 'list) tmp)))
          ;; ((eq c #\＝) (setq tmp (nconc (coerce " >- " 'list) tmp)))
          ;; ((eq c #\≒) (setq tmp (nconc (coerce " -< " 'list) tmp)))
          ;; ((eq c #\→) (setq tmp (nconc (coerce " >-- " 'list) tmp)))
          ;; ((eq c #\；) (push #\; tmp))
          ;; ((eq c #\｜) (push #\| tmp))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          
          ((eq c #\") 
            (unread-char #\" stream)
            (push (vector '|<string>| (read stream t nil t)) tmp))
          (nil (eq c #\") 
               (unread-char #\" stream)
               
            (let ((src-str (format nil "~S" (read stream t nil t))))
              (setq tmp (nconc (nreverse (coerce src-str 'list))
                               tmp))))
          ((eq c #\;) (do () ((eq #\Newline
                                  (read-char stream)))))
          ((eq c #\|)
            (setq bar-flag t))
          
          ((and (eq c #\{) (eql #\# (car tmp)))
            ;; #{ の処理: #\(に置換し、対応する#\)の数をセット
            (setf (car tmp) #\()
            (incf (car paren-count)))
          
          (t 
            (push c tmp)
            (cond ((eq c #\() (push 0 paren-count))
                  ((eq c #\))
                    (let ((m (pop paren-count)))
                      ;; #{ の後処理。対応する#\)を必要な数だけ追加する
                      (unless (zerop m)
                        (dotimes (_ m) (push #\) tmp))))
                    (when (null paren-count)
                      (pop tmp)
                      (setq end-flag t))))))))



(defun <trans> (header chars footer)
  (nconc (coerce header 'list)
         chars
         (coerce footer 'list)))


(oleo.base:define-lpar-backslash-reader "XI" (stream)
  (FORMAT T "\\XI is OBSOLETE~%")
  (qi-reader stream))

(oleo.base:define-lpar-backslash-reader "XDEF" (stream)
  (FORMAT T "\\XDEF is OBSOLETE~%")
  (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def " chars ")"))))

(oleo.base:define-lpar-backslash-reader "XLAMBDA" (stream)
  (FORMAT T "\\XLAMBDA is OBSOLETE~%")
  (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def xi_lambda " chars ")"))))

(oleo.base:define-lpar-backslash-reader "XDEFUN" (stream)
  (FORMAT T "\\XDEFUN is OBSOLETE~%")
  (let ((*<LOCALLY-P>* T))
    (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def " chars ")")))))

(oleo.base:define-lpar-backslash-reader "XDEFMACRO" (stream)
  (FORMAT T "\\XDEFMACRO is OBSOLETE~%")
  (let ((*<LOCALLY-P>* T)
        (*<DEF-OP>* 'DEFMACRO))
    (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def " chars ")")))))

(oleo.base:define-lpar-backslash-reader "X" (stream)
  (FORMAT T "\\X is OBSOLETE~%")
  (let ((*<LOCALLY-P>* T)
        (*<DEF-OP>* NIL))
    (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def " chars ")")))))

;;;

(oleo.base:define-lpar-backslash-reader "XENV" (stream)
  (qi-reader stream))

(oleo.base:define-lpar-backslash-reader "DEF" (stream)
  (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def " chars ")"))))

(oleo.base:define-lpar-backslash-reader "LAMBDA" (stream)
  (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def xi_lambda " chars ")"))))

(oleo.base:define-lpar-backslash-reader "DEFUN" (stream)
  (let ((*<LOCALLY-LET-OP>* 'LABELS))
    (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def " chars ")")))))

(oleo.base:define-lpar-backslash-reader "DEFMACRO" (stream)
  (let ((*<LOCALLY-LET-OP>* 'LABELS)
        (*<DEF-OP>* 'DEFMACRO))
    (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def " chars ")")))))

(oleo.base:define-lpar-backslash-reader "LET" (stream)
  (let ((*<LOCALLY-LET-OP>* 'FLET)
        (*<DEF-OP>* NIL))
    (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def " chars ")")))))

(oleo.base:define-lpar-backslash-reader "LETREC" (stream)
  (let ((*<LOCALLY-LET-OP>* 'LABELS)
        (*<DEF-OP>* NIL))
    (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def " chars ")")))))

(defun <deconstruct-lambda> (lambda-form) (third lambda-form))

(oleo.base:define-lpar-backslash-reader "\\" (stream)
  (<deconstruct-lambda>
   (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def xi_lambda -> (" chars "))")))))


(oleo.base:define-lpar-backslash-reader "|" (stream)
  (<deconstruct-lambda>
   (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def xi_lambda -> [" chars "])")))))

(oleo.base:define-lpar-backslash-reader "." (stream)
  (<deconstruct-lambda>
   (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def xi_lambda -> (/. " chars "))")))))

(oleo.base:define-lpar-backslash-reader "~" (stream)
  (<deconstruct-lambda>
   (qi-reader stream :pre-execute t :transformer (lambda (chars) (<trans> "(def xi_lambda -> " chars ")")))))


;;OBSOLETE
;;(oleo.base:define-lpar-backslash-reader "XDECL" (stream)
;;  (qi-reader stream :transformer (lambda (chars) (<trans> "(decl " chars ")"))))

;;OBSOLETE
;;(oleo.base:define-lpar-backslash-reader "X-YACC" (stream)
;;  (qi-reader stream))


;; (oleo.base:define-lpar-backslash-reader "X-PROLOG" (stream)
;;   (do (c 
;;        tmp
;;        escape-flag
;;        comma-flag
;;        bar-flag
;;        end-flag)
;;       (end-flag
;;        (let* ((chars (nreverse (cdr tmp)))
;;               (parsed (let ((*package* (find-package :Q))
;;                             (*readtable* OLEO.EMBED.CORE:*qi-readtable*)
;;                             (errstr "syntax error in Prolog(Qi) here: ~%~%~{~C~}"))
;;                         (OLEO.EMBED.CORE::|compile| 'OLEO.EMBED.CORE::|<horn_clauses>| chars errstr))))
;;          (if (OLEO.EMBED.CORE::%need-trans-p% parsed)
;;            `(OLEO.EMBED.CORE:|s-prolog| ',(OLEO.EMBED.CORE::%trans% parsed))
;;            `(OLEO.EMBED.CORE:|s-prolog| ',parsed))))    
    
;;     (unless (characterp (setq c (read-char stream)))
;;       (error "Xi.Core: unexpected end of stream"))

;;     (when comma-flag
;;       (unless (eq c #\))
;;         (setq comma-flag nil)))

;;     (cond (comma-flag
;;             (setq end-flag t))
;;           (escape-flag
;;             (setq escape-flag nil)
;;             (push c tmp))
;;           (bar-flag ;; TODO
;;             (when (eql c #\|)
;;               (setq bar-flag nil))
;;             (push c tmp)
                  
;;             ;(unless (eq c #\|) (error "#Q .. }#: misplaced |"))
;;             )
;;           ((eq c #\\) 
;;             (setq escape-flag t)
;;             (when (eq (car tmp) #\#) ;;文字リテラル#\における\を読み込んだ場合
;;               (push #\\ tmp)))
;;           ((eq c #\｜) (push #\| tmp))
;;           ((eq c #\") 
;;             (unread-char #\" stream)
;;             (let ((src-str (format nil "~S" (read stream t nil t))))
;;               (setq tmp (nconc (nreverse (coerce src-str 'list))
;;                                tmp))))
;;           ((eq c #\;) (do () ((eq #\Newline
;;                                   (read-char stream)))))
;;           (t 
;;             (push c tmp)
;;             (setq comma-flag (eq c #\.))
;;             (setq bar-flag (eq c #\|))))))


(oleo.base:register-reader-registerer '|embed/1-finish-core| '<register-readers>)
