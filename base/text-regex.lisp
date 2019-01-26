;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.core:clpgk-core-header)
(in-package :clpgk.base.text)

#Verify cl-ppcreの要件チェック
(multiple-value-bind (start end grp/starts grp/ends) (scan "x" "x")
  (declare (ignore start end))
  (and (simple-vector-p grp/starts)
       (simple-vector-p grp/ends)
       (equalp grp/starts #())
       (equalp grp/ends #())))

(defmacro <create-scanner> (src extended-mode &optional multi-line-mode single-line-mode case-insensitive-mode)
  `(let ((*allow-named-registers* t))
     (create-scanner ,src
                     ,@(when extended-mode (list :extended-mode extended-mode))
                     ,@(when multi-line-mode (list :multi-line-mode multi-line-mode))
                     ,@(when single-line-mode (list :single-line-mode single-line-mode))
                     ,@(when case-insensitive-mode (list :case-insensitive-mode case-insensitive-mode)))))

(defmacro <static-regex> (regex)
  (cond ((stringp regex)
          `(memoized (<create-scanner> ,regex nil nil nil nil)))
        (t regex)))

(defun regex (source-string
              &key extended-mode multi-line-mode single-line-mode case-insensitive-mode)
  (<create-scanner> source-string extended-mode multi-line-mode single-line-mode case-insensitive-mode))

(define-compiler-macro regex (source-string
                              &key extended-mode multi-line-mode single-line-mode case-insensitive-mode)
  (let ((common-form `(<create-scanner> ,source-string ,extended-mode ,multi-line-mode ,single-line-mode ,case-insensitive-mode)))
    (if (stringp source-string)
      `(memoized ,common-form)
      common-form)))

(defun =~ (regex target)
  (multiple-value-bind (start end) (scan regex target)
    (when start (values T start end))))

(defun !~ (regex target)
  (multiple-value-bind (start end) (scan regex target)
    (if start
      (values nil start end)
      T)))

;(!~ "^," "a,a,b")

;(disassemble (regex "foo"))

(defun <vec-cons> (x vec &aux (n (length vec)))
  (let ((result (make-array (1+ n))))
    (setf (svref result 0) x)
    (dotimes (i n result)
      (setf (svref result (1+ i)) (svref vec i)))))

(defun <pre-skip-scanner> (kwd)
  (case kwd
    (:whitespaces (regex #"^[ \t\n\r\v]*"))
    (:spaces (regex "^[ ]*"))
    (:tabs (regex #"^[\t]*"))
    (t (error "unknown pre-skip keyword ~W" kwd))))

(defun <text-scan> (regex target output start end strict retry sub-result pre-skip post-skip)

  (when (keywordp pre-skip)   (setq pre-skip (<pre-skip-scanner> pre-skip)))
  (when (keywordp post-skip)  (setq post-skip (<pre-skip-scanner> post-skip)))
  ;;(when (keywordp ignore)     (setq ignore (<pre-skip-scanner> ignore)))

  (macrolet ((results (main-result)
               `(let ((<<result>> ,main-result))
                  (if (eq sub-result :positions)
                    (values <<result>> match/start match/end pre-skip/start pre-skip/end post-skip/end)
                    (values <<result>> (ecase sub-result
                                         ((:end NIL) match/end)
                                         (:start match/start)
                                         (:region (vector match/start match/end))
                                         (:continuation continuation)))))))
    (unless end (setq end (length target)))
    (multiple-value-bind (pre-skip/start pre-skip/end)
        (when pre-skip
          ;;(unless strict (setq strict T))
          (scan pre-skip target :start start :end end))
      (if (and (eql start pre-skip/start)
               (not (eql pre-skip/start pre-skip/end)))
        (setq start pre-skip/end)
        (setq pre-skip/start nil
              pre-skip/end   nil))
      
      (multiple-value-bind (match/start match/end grp/starts grp/ends) (scan regex target :start start :end end)
        (when (or (and match/start
                       strict
                       (not (eql start match/start)))
                  (null match/start))
          ;; 全てNILの場合も有り得る 
          (return-from <text-scan> (values nil nil nil pre-skip/start pre-skip/end nil)))
        (when match/start
          (unless pre-skip/start
            (setq pre-skip/start match/start
                  pre-skip/end   match/start))
          (multiple-value-bind (post-skip/start post-skip/end)
              (when post-skip (scan post-skip target :start match/end :end end))
            (when (or (null post-skip/start) (not (eql match/end post-skip/start)))
              (setq post-skip/start match/end
                    post-skip/end   match/end))
            
            (let ((continuation
                    (when (eq sub-result :continuation)
                      (lambda (&key (regex regex) (output output) (start post-skip/end) (end end)
                                    (sub-result :continuation))
                        (text-scan regex target :output output :start start :end end :strict strict :retry retry
                                   :sub-result sub-result :pre-skip pre-skip :post-skip post-skip)))))
              
              (case output
                (:position (results match/start))
                (:region (results (vector match/start match/end)))
                (:region-as-list (results (list match/start match/end)))
                (:string (results (subseq target match/start match/end)))
                (:sub-positions (results grp/starts))
                (:sub-positions-as-list (results (coerce grp/starts 'list)))
                (:sub-regions
                  (dotimes (i (length grp/starts) (results grp/starts))
                    (awhen (svref grp/starts i) (setf (svref grp/starts i) (vector it (svref grp/ends i))))))
                (:sub-regions-as-list
                  (dotimes (i (length grp/starts) (results (coerce grp/starts 'list)))
                    (awhen (svref grp/starts i) (setf (svref grp/starts i)  (list it (svref grp/ends i))))))
                ((:sub-strings :sub-strings-as-list)
                  (dotimes (i (length grp/starts) (results (if (eq :sub-strings-as-list output)
                                                             (coerce grp/starts 'list)
                                                             grp/starts)
                                                           ))
                    (setf (svref grp/starts i) (awhen (svref grp/starts i)
                                                 (subseq target it (svref grp/ends i))))))
                (:all-positions (results (<vec-cons> match/start grp/starts)))
                (:all-positions-as-list (results (cons match/start (coerce grp/starts 'list))))
                (:all-regions
                  (dotimes (i (length grp/starts) (results (<vec-cons> (vector match/start match/end) grp/starts)))
                    (awhen (svref grp/starts i) (setf (svref grp/starts i) (vector it (svref grp/ends i))))))
                (:all-regions-as-list
                  (dotimes (i (length grp/starts) (results (cons (list match/start match/end) (coerce grp/starts 'list))))
                    (awhen (svref grp/starts i) (setf (svref grp/starts i)  (list it (svref grp/ends i))))))
                ((:all-strings :all-strings-as-list)
                  (dotimes (i (length grp/starts) (let ((main (subseq target match/start match/end)))
                                                    (results (if (eq :all-strings-as-list output)
                                                               (cons main (coerce grp/starts 'list))
                                                               (<vec-cons> main grp/starts))
                                                             )))
                    (setf (svref grp/starts i) (awhen (svref grp/starts i)
                                                 (subseq target it (svref grp/ends i))))))
                ((:selected-index :selected-string :selected-position :selected-region :selected-region-as-list)
                  (aif (when grp/starts
                         (let (idx)
                           (dotimes (i (length grp/starts)
                                     (when idx
                                       (case output
                                         (:selected-index idx)
                                         (:selected-string (subseq target (svref grp/starts idx) (svref grp/ends idx)))
                                         (:selected-position (svref grp/starts idx))
                                         (:selected-region (vector (svref grp/starts idx) (svref grp/ends idx)))
                                         (T ;; :selected-region-as-list
                                           (list (svref grp/starts idx) (svref grp/ends idx))))))
                             (when (svref grp/starts i)
                               (if idx (return nil) (setq idx i))))))
                       ;; Success
                       (results it)
                       ;; Failed. but...
                       (if retry
                         (unless (or (eql match/start match/end)
                                     (eql end match/end))
                           (<text-scan> regex target output post-skip/end end strict T sub-result pre-skip post-skip))
                         (values nil nil nil start match/end)
                         )
                       ))
                ((:reacted-index :reacted-string :reacted-position :reacted-region :reacted-region-as-list)
                  (aif (when grp/starts
                         (let ((idx (dotimes (i (length grp/starts))
                                      (when (svref grp/starts i)
                                        (return i)))))
                           (when idx
                             (case output
                               (:reacted-index idx)
                               (:reacted-string (subseq target (svref grp/starts idx) (svref grp/ends idx)))
                               (:reacted-position (svref grp/starts idx))
                               (:reacted-region (vector (svref grp/starts idx) (svref grp/ends idx)))
                               (T ;; :selected-region-as-list
                                 (list (svref grp/starts idx) (svref grp/ends idx)))))))
                       (results it)
                       (case output
                         (:reacted-index nil)
                         (:reacted-string (results (subseq target match/start match/end)))
                         (:reacted-position (results match/start))
                         (:reacted-region (results (vector match/start match/end)))
                         (T ;; :reacted-region-as-list
                           (results (list match/start match/end))))))
                ((t) (results t))
                (T (error "Unknown :output ~W" output))))))))))

(defun text-scan (regex
                  target
                  &key
                  (output :string)
                  (start 0)
                  (end (length target))
                  strict
                  retry
                  (sub-result :end)
                  pre-skip
                  post-skip)
  (<text-scan> regex target output start end strict retry sub-result pre-skip post-skip))

(define-compiler-macro text-scan (regex
                                  target
                                  &key
                                  (output :string)
                                  (start '|<unbound>|)
                                  (end '|<unbound>|)
                                  (strict '|<unbound>|)
                                  (retry '|<unbound>|)
                                  (sub-result '|<unbound>|)
                                  (pre-skip '|<unbound>|)
                                  (post-skip '|<unbound>|)
                                  )
  (flet ((unbound? (x) (eq x '|<unbound>|)))
    `(<text-scan> (<static-regex> ,regex) ,target ,output
                  ,(if (or (unbound? start) (null start)) 0 start)
                  ,(unless (unbound? end) end)
                  ,(unless (unbound? strict) strict)
                  ,(unless (unbound? retry) retry)
                  ,(if (unbound? sub-result) :end sub-result)
                  ,(when (and (not (unbound? pre-skip)) pre-skip) `(<static-regex> ,pre-skip))
                  ,(when (and (not (unbound? post-skip)) post-skip) `(<static-regex> ,post-skip))
                  )
  ))
  
#Testing
(let ((s ",foo,bar,,baz,"))
  (check-unit* (:test equalp)  (multiple-value-list _)
    ((text-scan "," s) '("," 1))
    ((text-scan "ooo,|o,b" s :output :string) '("o,b" 6))
    ((text-scan ",$|^a" s :output :position) '(13 14))
    ((text-scan "^,|,$" s :output :region) '(#(0 1) 1))
    ((text-scan "a.," s :output :region-as-list) '((6 9) 9))
    ((text-scan "(.,)|(,,)" s :output :sub-strings) '(#("o," nil) 5))
    ((text-scan "((,,)|(.,))" s :output :sub-strings-as-list) '(("o," nil "o,") 5))
    ((text-scan "(.,(.+),,)" s :output :sub-positions) '(#(3 5) 10))
    ((text-scan "(,+b)|(o+,)" s :output :sub-positions-as-list) '((nil 2) 5))
    ((text-scan "(.,(.+),,)" s :output :sub-regions) '(#(#(3 10) #(5 8)) 10))
    ((text-scan "(,+b)|(o+,)" s :output :sub-regions-as-list) '((nil (2 5)) 5))

    ((text-scan "(.,)|(,,)" s :output :all-strings) '(#("o," "o," nil) 5))
    ((text-scan "((,,)|(.,))" s :output :all-strings-as-list) '(("o," "o," nil "o,") 5))
    ((text-scan "(.,(.+),,)" s :output :all-positions) '(#(3 3 5) 10))
    ((text-scan "(,+b)|(o+,)" s :output :all-positions-as-list) '((2 nil 2) 5))
    ((text-scan "(.,(.+),,)" s :output :all-regions) '(#(#(3 10) #(3 10) #(5 8)) 10))
    ((text-scan "(,+b)|(o+,)" s :output :all-regions-as-list) '(((2 5) nil (2 5)) 5))

    ((text-scan "(,+b)|(o+,)" s :output :selected-string) '("oo," 5))
    ((text-scan "((,+b)|boz)|(o+,)" s :output :selected-string) '("oo," 5))
    ((text-scan "((,+b)|(o+,))" s :output :selected-string) '(nil nil nil 0 5))
    ((text-scan "((,+b)|(o+,))" s :output :reacted-string) '("oo," 5))

    ((text-scan "(,+b)|(o+,)" s :output :selected-index) '(1 5))
    ((text-scan "((,+b)|boz)|(o+,)" s :output :selected-index) '(2 5))
    ((text-scan "((FOO)|(boz))|(BAZ)" s :output :selected-index) '(nil nil nil nil nil nil))
    ((text-scan "((FOO)|(boz))|(BAZ)" s :output :selected-index :pre-skip ",*") '(nil nil nil 0 1 nil))
    ((text-scan "((FOO)|(boz))|(BAZ)" s :output :selected-index :pre-skip ":*") '(nil nil nil nil nil nil))
    ((text-scan "(BAZ)|((,+b)|(o+,))" s :output :reacted-index) '(1 5))

    ((text-scan "(BAZ)|((,+b)|(o+,))" s :output :selected-position) '(nil nil nil 0 5))
    ((text-scan "(,f)|((,+b)|(o+,))" s :output :selected-position) '(0 2))
    ((text-scan "(,f)|((,+b)|(o+,))" s :output :selected-region) '(#(0 2) 2))
    ((text-scan "(BAZ)|((,+b)|(o+,))" s :output :reacted-region) '(#(2 5) 5))
    ((text-scan "((BAZ)|(foo)),|((,+b)|(o+,))" s :output :reacted-region-as-list) '((1 4) 5))

))

;;(text-scan "-|(foo)|(bar)" "-----foo--fo" :output :selected-string :retry t :strict t)
;(text-scan "[fo]+" ",foo,bar,,baz," :output :string)
;(scan "[?]+" "!??x??")
;;(text-scan "," ",foo")
;;(text-scan ",a|,,a" ",a,,a")
;;(text-scan "[1-9]+[.]?[1-9]*|," "32,")
;;(text-scan "((ab|cd),?)+" "abcd,ab,c" :strict t)
;;(text-scan (regex "[ \\t]+") "x   d")
;;(text-scan "(.*f+:)([a-z]+)(.*)" "xxxff:abc!" :output :sub-strings-as-list )
;;(text-scan "foo" "  foo" :strict t :pre-skip :spaces :output :region)
;(text-scan (regex "((,)|(a))") "xabcd," :output :all-regions-as-list :start 1 :need-continuation t)
;(text-scan "(,)|(ah)|(ax)|(a)|(a)" "a" :output :all-strings)
;(text-scan "(,)|(ah)|(ax)|(a)" "a" :output :reacted-region-as-list :sub-result :positions)

;;(text-scan #"[\n\t ]+end$" #"  \n  end")

;(eval '(progn (defun bar () 3) (compile 'bar) (compiled-function-p #'bar)))
;(macroform (text-scan "(,)|(ah)|(ax)|(a)" "a" :output :reacted-region-as-list))

;;(text-scan "foo|bar" ",,foo::barboz" :strict t :pre-skip ",+" :post-skip ":+" :sub-result :positions)

#; `retry' `strict' の追加

;;;;;;;;;;;;;;;;;;
;; allow-unmatched or allow-terminated VERSION
(defmacro do-text ((var-or-multiple-vars regex target &key (output :string) start end strict retry pre-skip post-skip
                                         allow-unmatched allow-terminated)
                   &body body
                   &aux (mvars (if (listp var-or-multiple-vars) var-or-multiple-vars (list var-or-multiple-vars))))
  (setq allow-terminated (when allow-terminated t)
        allow-unmatched (when allow-unmatched t))
  (when (stringp pre-skip)
    (setq pre-skip `(<static-regex> ,pre-skip)))
  (with-gensyms (<regex> <target> <output> <start> <end> <retry> <strict> <pre-skip> <post-skip>
                         result match/start match/end skip/start skip/end real/end
                         next/1 next/2 next/3)
    `(let* ((,<regex> (<static-regex> ,regex))
            (,<target> ,target)
            (,<output> ,output)
            (,<strict> ,strict)
            (,<retry> ,retry)
            (,<pre-skip> ,pre-skip)
            (,<post-skip> ,post-skip)
            (,<start> ,(if start `(aif ,start it 0) 0))
            (,<end> ,(if end `(aif ,end it (length ,<target>)) `(length ,<target>))))
       (loop (multiple-value-bind (,result ,match/start ,match/end ,skip/start ,skip/end ,real/end)
                 (<text-scan> ,<regex> ,<target> ,<output> ,<start> ,<end> ,<strict> ,<retry> :positions ,<pre-skip> ,<post-skip>)
               (declare (ignorable ,skip/start ,skip/end))
               ;;(setq ,<start> ,skip/end)
               (multiple-value-bind (,next/1 ,next/2 ,next/3 ,@mvars)
                   (cond                         
                     ,@(when allow-unmatched
                         `(((and ,match/start (not (eql ,<start> ,skip/start)))
                            (values ,result ,match/start ,match/end :unmatched ,<start> ,match/start))
                           ((and ,match/start (not (eql ,skip/end ,match/start)))
                            (values ,result ,match/start ,match/end :unmatched ,skip/end ,match/start))))
                     
                     (,match/start (values nil nil nil ,result ,match/start ,match/end))
                     ;;,@(when (or allow-unmatched allow-terminated)
                     ;; todo
                     ,@(cond ((and allow-unmatched allow-terminated)
                               `(((or (eql ,<start> ,<end>)
                                      (eql ,skip/end ,<end>))
                                  (values nil nil nil :terminated ,<end> ,<end>))
                                 (,skip/end  (values :terminated ,skip/end ,<end> :unmatched ,skip/end ,<end>))
                                 (T  (values :terminated ,<start> ,<end> :unmatched ,<start> ,<end>))))
                             (allow-unmatched
                               `(((or (eql ,<start> ,<end>)
                                      (eql ,skip/end ,<end>))
                                  (return))
                                 (T  (values nil nil nil :unmatched (if ,skip/end ,skip/end ,<start>) ,<end>))))
                             (allow-terminated
                               `((,skip/end  (values nil nil nil :terminated ,skip/end ,<end>))
                                 (T  (values nil nil nil :terminated ,<start> ,<end>))
                                 ))
                             (t '((T  (return))))
                             )
                     ;;)
                     ;; ,@(when NIL allow-unmatched
                     ;;     `(((not (eql ,<start> ,<end>))
                     ;;        ,(if allow-terminated
                     ;;           `(values :terminated ,<start> ,<end> :unmatched ,<start> ,<end>)
                     ;;           `(values nil nil nil :unmatched ,<start> ,<end>)))))
                     ;; ,@(if allow-terminated
                     ;;     `((T (values nil nil nil :terminated ,<start> ,<end>)))
                     ;;     '((T (return))))
                     )
                 (tagbody |<<doTextBegin>>| (progn ,@body)
                   (when ,next/1
                     (multiple-value-setq ,mvars (values ,next/1 ,next/2 ,next/3))
                     (setq ,next/1 nil)
                     (go |<<doTextBegin>>|))))
               (if ,result
                 (setq ,<start> ,real/end)
                 (return))
               )))))

#Testing
(macrolet ((chk1 (&rest args)  `(let (tmp)
                                  (do-text (x ,@args)
                                    (push x tmp))
                                  (nreverse tmp)))
           (chk2 (regex target &rest args)  `(let (<tmp>
                                                   (<target> ,target))
                                               (do-text ((x s e) ,regex <target> ,@args)
                                                 (push (case x
                                                         (:unmatched (list (subseq <target> s e)))
                                                         (:terminated (list (list (subseq <target> s e))))
                                                         (t x))
                                                       <tmp>))
                                               (nreverse <tmp>))))
  (check-unit* (:test equalp)  _
    ((chk1 ".oo" "foobarfoobazboo") '("foo" "foo" "boo"))
    ((chk2 ".oo" "foobarfoobazboo" :allow-unmatched t) '("foo" ("bar") "foo" ("baz") "boo"))
    ((chk2 ".oo" "foobarfoobazboo" :allow-unmatched t :pre-skip "b.z") '("foo" ("bar") "foo" "boo"))
    ((chk2 ".oo" "foobarfoobazbooboz" :allow-unmatched t :post-skip "b.z") '("foo" ("bar") "foo" "boo"))
    ;;todo
    ((chk2 ".oo" "" :pre-skip :spaces :allow-unmatched t :allow-terminated t) '(((""))))
    ((chk2 ".oo" "  " :pre-skip :spaces :allow-unmatched t :allow-terminated t) '(((""))))
    ((chk2 ".oo" "  " :pre-skip :spaces :allow-unmatched t) '())
    ((chk2 ".oo" "foobarfoobazbooboz" :allow-unmatched t :pre-skip "b.z") '("foo" ("bar") "foo" "boo"))
    ((chk2 ".oo" "  end" :pre-skip :spaces :allow-unmatched t :allow-terminated t) '(("end") (("end"))))
    ((chk2 ".oo" "  end" :pre-skip :spaces :allow-unmatched t) '(("end")))
    ((chk2 ".oo" "  end" :pre-skip :spaces :allow-terminated t) '((("end"))))

    ((chk2 ".oo" "  " :allow-unmatched t :pre-skip :spaces) '())
    ((chk2 ".oo" "  " :allow-unmatched t :post-skip :spaces) '(("  ")))

    ((chk1 ".oo" "foobarfoobazbooboz" :allow-unmatched t) '("foo" :unmatched "foo" :unmatched "boo" :unmatched))
    
    ((chk1 ".oo" "foobarfoobazbooboz" :allow-unmatched t :allow-terminated t) '("foo" :unmatched "foo" :unmatched "boo" :unmatched :terminated))

    
    ((chk1 ".oo" "foobarfoobazboo" :allow-unmatched t :allow-terminated t) '("foo" :unmatched "foo" :unmatched "boo" :terminated))
    ((chk1 ".oo" "foobarfoobazboo" :allow-unmatched t :allow-terminated t :post-skip "ba.") '("foo" "foo" "boo" :terminated))
    ((chk1 ".oo" "foobarfoobazbooboz" :allow-unmatched t :allow-terminated t :post-skip "ba.") '("foo" "foo" "boo" :unmatched :terminated))
   ((chk1 ".oo" "foobarfoobazbooboz" :allow-unmatched t :allow-terminated t :post-skip "ba." :pre-skip "f..") '(:unmatched "foo" "boo" :unmatched :terminated))
   ((chk1 "..." "foo,   bar, baz," :allow-unmatched t :allow-terminated t :pre-skip :spaces :post-skip ",") '("foo" "bar" "baz" :terminated))
   ((chk1 "..." " foo bar,baz," :allow-unmatched t :allow-terminated t :pre-skip :spaces :post-skip ",") '("foo" "bar" "baz" :terminated))
   ((chk1 "foo|bar" " foo bar,baz," :allow-unmatched t :allow-terminated t :pre-skip :spaces :post-skip ",") '("foo" "bar" :unmatched :terminated))
   ((chk1 "..." " foo bar,baz,  " :allow-unmatched t :allow-terminated t :pre-skip :spaces :post-skip ",") '("foo" "bar" "baz" :terminated))
   ))

;(do-text (x ".oo" "foobarqoobazbooboz" :allow-unmatched t :allow-terminated t :post-skip "ba." :pre-skip "f..")
;  (print x))
(text-scan ".oo" "afaafoobarqoobazbooboz" :post-skip "ba." :pre-skip "f.." :sub-result :positions)

#Testing
(macrolet ((chk2 (regex target &rest args)
             `(let (<tmp>
                    (<target> ,target))
                (do-text ((x s e) ,regex <target> ,@args)
                  (push (case x
                          (:unmatched (list (subseq <target> s e)))
                          (:terminated (list (list (subseq <target> s e))))
                          (t x))
                        <tmp>))
                (nreverse <tmp>))))
  (check-unit* (:test equalp)  _

    ((chk2 "foo" "foobarfoo" :allow-unmatched t) '("foo" ("bar") "foo"))
    ((chk2 "foo" "foobarfoo" :allow-unmatched t :allow-terminated t) '("foo" ("bar") "foo" ((""))))
    ))

  

;;(do-text (x "(.o+)|," " foo   , boo,zoo"  :pre-skip :spaces :strict t :retry t :output :selected-string) (print x))
;;(do-text (x "(.o+)|," "foo,boo,..zoo"  :strict t :retry t :output :selected-string :allow-terminated t) (print x))
;;(do-text (x "(.o+)|," "afoo,.aboo,.zooxyz" :retry t :output :selected-string :allow-unmatched t) (print x))
;;(do-text (x "(.o+)|," "afoo,.aboo,.zoo" :retry t :output :selected-string :allow-unmatched t) (print x))

;;(do-text ((x y z) "(.o+)|," "afoo,.aboo,.zoo" :retry t :output :selected-string :allow-unmatched t :allow-terminated t) (print (list x y z)))

;;(do-text ((x y z) "foo" "foobarfoobazz"  :output :string :allow-terminated t :allow-unmatched t) (print (list x y z)))

;;(do-text ((x y z) "(foo)|(bar)|baz" "foobarfoobarfoof"  :output :selected-string :retry t :strict t :allow-terminated t :allow-unmatched t) (print (list x y z)))

;;(do-text ((x y z) "foo" "  foo  barfoo  last"  :allow-terminated t :allow-unmatched t :pre-skip :spaces) (print (list x y z)))
;;(do-text ((x y z) "foo" "  foo  barfoo    "  :allow-terminated t :allow-unmatched t :pre-skip :spaces) (print (list x y z)))


;;(do-text ((x s e) ".o+" "afoo,.aboo,.zooxyz" :allow-unmatched t :allow-terminated t) (print (list x s e)))
;;(text-scan ".o+" "afoo,.aboo,.zooxyz" :sub-result :positions)


    
;(text-scan-each "(.o+)|,|(;.*$)" "foo,boo;x,zoo,foo" :output :selected-string :retry t :strict t)
(text-scan "(?:[ ]*)([x]+.)" " xxxxfx" :output :selected-string)


(defun text-map (regex target
                        &key (map #'list) (arity 3) option test
                        (output :string)
                        start end strict retry pre-skip post-skip limit force)
  (flet ((call/0 (f x s e ex-param) (declare (ignore x s e)) (if ex-param (funcall f ex-param) (funcall f)))
         (call/1 (f x s e ex-param) (declare (ignore s e)) (if ex-param (funcall f x ex-param) (funcall f x)))
         (call/2 (f x s e ex-param) (declare (ignore e)) (if ex-param (funcall f x s ex-param) (funcall f x s)))
         (call/3 (f x s e ex-param) (if ex-param (funcall f x s e ex-param) (funcall f x s e))))
    (if (eql 0 limit)
      (values nil :limited)
      (let (tmp
            (cnt limit)
            (caller (ecase arity (0 #'call/0) (1 #'call/1) (2 #'call/2) (3 #'call/3))))
        (do-text ((x s e) regex target :allow-unmatched t :allow-terminated t
                  :start start :end end :output output :strict strict :retry retry
                  :pre-skip pre-skip :post-skip post-skip)
          (when (and (eq :unmatched x) strict)
            (if force
              (return-from text-map (values (nreverse tmp) :forced))
              (return)))
          (when (eq :terminated x)
            (return-from text-map (values (nreverse tmp) T)))
          (let ((mapped (funcall caller map x s e option)))
            (when (or (null test) (funcall test mapped))
              (push mapped tmp)
              (when (and cnt (zerop (decf cnt)))
                (return-from text-map (values (nreverse tmp) :limited))))))))))

(text-map "<(.?oo)>|(.?oo)" ",<foo>,goobarfoobazzoo" :output :selected-string :pre-skip ",")
(text-map ".?oo" ",foo,goobarfoobazzoo" :output :string :pre-skip ",")
(text-map ".?oo" ",foo  ,goo    ,qoobar" :output :string :pre-skip "," :post-skip " +")
(text-map "foo|bar" "foobarbozbar" :arity 1 :map #'identity :strict nil :retry t :force t)
(text-map "foo|bar" "foobarbozbar" :arity 1 :map (lambda (x) (unless (eq :unmatched x) (list 'ok x))) :test #'identity)

 
(defun text-matches (regex target &key (output :string) start end strict retry pre-skip post-skip limit)
  (if (eql 0 limit)
    (values nil T)
    (let (tmp)
      (do-text ((x s e) regex target :allow-terminated t :start start :end end :output output
                :strict strict :retry retry :pre-skip pre-skip :post-skip post-skip)
        (when (eq :terminated x)
          (when (and strict (not (eql s e))) (return))
          (return-from text-matches (values (nreverse tmp) T)))
        (push x tmp)
        (when (and limit (zerop (decf limit)))
          (return-from text-matches (values (nreverse tmp) T))))
      (values nil nil))))

(defun text-unmatches (regex target &key (output :string) start end pre-skip post-skip limit)
  (flet ((to-string (s e src)                                (subseq src s e))
         (to-position (s e src) (declare (ignore e src))     s)
         (to-region (s e src) (declare (ignore src))         (vector s e))
         (to-region-as-list (s e src) (declare (ignore src)) (list s e)))
    (let ((f (ecase output
               (:string         #'to-string)
               (:position       #'to-position)
               (:region         #'to-region)
               (:region-as-list #'to-region-as-list))))
      (unless (eql 0 limit)
        (let (tmp)
          (do-text ((x s e) regex target :allow-unmatched t :allow-terminated t :start start :end end :output output
                    :pre-skip pre-skip :post-skip post-skip)
            (case x
              (:unmatched (push (funcall f s e target) tmp)
                (when (and limit (zerop (decf limit)))
                  (return-from text-unmatches (nreverse tmp))))
              (:terminated (return-from text-unmatches (nreverse tmp))))))))))

;(text-matches ".?oo|bar" "foobarfoofoo" :start 0  :strict t :limit 3)
;TODO--> (scan "f" "xfoo" :start #\F :end 3)

(text-matches ".?oo" "foobarfoobazzoo" :output :string :strict t)
;(do-text ((x s e) ".?oo" "foobarbazzoo" :strict t :allow-unmatched t :allow-terminated t) (print (list x s e)))
(text-unmatches ".?oo" ",foo,qoobarfoo,bazzoo,zoo" :output :string :pre-skip ",")
(text-matches ".?oo" ",foo,qoobarfoobazzoo,zoo" :output :string :pre-skip ",")
(text-scan ".?oo" ",,foo,qoobarfoobazzoo,zoo" :output :string :pre-skip "," :sub-result :positions)
(text-map ".?oo" ",foobarbaz" :pre-skip ",")
(scan "((ff)|(f)|(fff))+" "zfff")

;(let ((s (make-string 10)))
;  (setf (char s 0) #\a)
                                        ;  s)


;; todo test keyword
'(defun text-replace (regex target to &key limit test (output :string) option &aux
                           (to/len (when (stringp to) (length to)))
                           (internal-output (if (stringp to) T output))
                           )

  (unless to/len (setq to (ensure-function to)))
  (when (and limit (zerop limit))  (return-from text-replace (copy-seq target)))

  (awhen (text-map regex target :output internal-output :map #'vector)
    (let ((slen 0)
          (limit-count (when limit limit)))
      (dolist (info it)
        (cond 
          ((eql :unmatched (svref info 0))
            (incf slen (- (svref info 2) (svref info 1))))
          ((and test
                (not (funcall test (subseq target (svref info 1) (svref info 2)))))
            (incf slen (- (svref info 2) (svref info 1)))
            ;; testが偽となったため、書き換える(unmatched扱い)
            (setf (svref info 0) :unmatched))
          (t
            (if to/len
              (incf slen to/len)
              (let* ((s (if option
                          (funcall to (svref info 0) option)
                          (funcall to (svref info 0))))
                     (len (length s)))
                (setf (svref info 0) (cons s len))
                (incf slen len)))
            (when (and limit-count (zerop (decf limit-count)))
              (incf slen (- (length target) (svref info 2)))
              (return)))))
      (let ((result (make-string slen))
            (cur 0))
        (dolist (info it result)
          (acase (svref info 0)
            (:unmatched
              (do ((i (svref info 1) (1+ i))
                   (end (svref info 2)))
                  ((eql i end))
                (setf (char result cur) (char target i))
                (incf cur)))
            (t
             (let ((to (if to/len to (car it)))
                   (to/len (if to/len to/len (cdr it))))                   
                (do ((i 0 (1+ i))
                     (end to/len))
                    ((eql i end))
                  (setf (char result cur) (char to i))
                  (incf cur)))
              (when (and limit (zerop (decf limit)))
                (do ((i (svref info 2) (1+ i))
                     (end (length target)))
                    ((eql i end))
                  (setf (char result cur) (char target i))
                  (incf cur))                
                (return-from text-replace result)))))))))

;; DEVEL
;; testが反応しなかったパターンはlimit,omit何れの数にも入らないことに注意
(defun text-replace (regex to target &key limit omit test option &aux
                           (to/len (when (stringp to) (length to))))
  
  (unless to/len (setq to (ensure-function to)))
  (when (and limit (zerop limit))  (return-from text-replace (copy-seq target)))

  (let ((slen 0)
        (limit-count (when limit limit))
        it)
    (do-text ((x s e) regex target :output :reacted-region :allow-unmatched t)
      (let (breakp)
        (cond 
          ((eq :unmatched x)
            (incf slen (- e s)))
          ((and test
                (not (funcall test (subseq target (svref x 0) (svref x 1)))))
            (incf slen (- e s))
            ;; testが偽となったため、書き換える(unmatched扱い)
            (setq x :unmatched))
          ((and omit (< 0 omit))
            (incf slen (- e s))
            ;; 省略対象のため、書き換える(unmatched扱い)
            (setq x :unmatched)
            (when (zerop (decf omit))
              (setq omit nil)))
          (t
            (if to/len
              (incf slen to/len)
              (let* ((reacted (subseq target (svref x 0) (svref x 1)))
                     (res (if option
                            (funcall to reacted option)
                            (funcall to reacted))))
                (setq x (vector (svref x 0) (svref x 1) res))
                (incf slen (length res))))
            (when (and limit-count (zerop (decf limit-count)))
              (incf slen (- (length target) e))
              (setq breakp T))))

        (if (eq x :unmatched)
          (push (vector :unmatched s e) it)
          (let ((a (svref x 0))
                (b (svref x 1)))
            (unless (eql a s)
              (incf slen (- a s))
              (push (vector :unmatched s a) it))
            (push (vector x a b) it)
            (unless (eql b e)
              (incf slen (- e b))
              (push (vector :unmatched b e) it))))

        
        (when breakp (return))
      ))

    (setq it (nreverse it))
    (let ((result (make-string slen))
          (cur 0))
        (dolist (info it result)
          (acase (svref info 0)
            (:unmatched
              (do ((i (svref info 1) (1+ i))
                   (end (svref info 2)))
                  ((eql i end))
                (setf (char result cur) (char target i))
                (incf cur)))
            (t
             (let* ((to (if to/len to (svref it 2)))
                    (to/len (if to/len to/len (length to))))
               (do ((i 0 (1+ i))
                    (end to/len))
                   ((eql i end))
                 (setf (char result cur) (char to i))
                 (incf cur)))
              (when (and limit (zerop (decf limit)))
                (do ((i (svref info 2) (1+ i))
                     (end (length target)))
                    ((eql i end))
                  (setf (char result cur) (char target i))
                  (incf cur))                
                (return-from text-replace result))))))))

        


;;(text-replace "f(o+)o|b(a*)z" "bar" "foooooobazfoobz")
;(text-map "foo" "  xfoobar  foobaz" :pre-skip :spaces :output t :map #'vector )
;(text-replace ".oo" "!!" "qoofoobarzoobazfooxxboo."  :limit 3 :test (lambda (x) (not (equal "foo" x))))
;(text-replace ".o+"  (lambda (x) (string-concat "<" x ">")) "foobarzooobar" :limit 100)
;(text-replace ".o+"  (lambda (x a) (string-concat a x a)) "foobarzooobar" :limit 1 :omit 1 :option ":")

;; testが反応しなかったパターンはlimit,omit何れの数にも入らないことに注意
(defun text-split (regex target &key limit omit test)
  (when (and limit (zerop limit))
             (return-from text-split (list (copy-seq target))))

  (let (result
        (prev 0))
    (do-text ((x s e) regex target :output :reacted-region :allow-terminated t)
      (cond
        ((eq :terminated x)
          (push (subseq target prev e) result))
        ((and test (not (funcall test (subseq target (svref x 0) (svref x 1)))))
          ;; do nothing
          )
        ((and omit (< 0 omit))
          (when (zerop (decf omit))
            (setq omit nil)))
        ((and limit (zerop (decf limit)))
          (push (subseq target prev (svref x 0)) result)
          (push (subseq target (svref x 1) nil) result)
          (return))
        (t
          (push (subseq target prev (svref x 0)) result)
          (setq prev (svref x 1)))))
    (nreverse result)))

;;(text-split " *, *" "  a,    b    ,c,d,eeee!" :limit 2 :omit 1)
;;(text-split "," "a,b,c,d,e,f" :omit nil :limit nil)
;;(text-split " (,) " "a,b , c,d , e,f" :omit 1 :limit nil)

#Testing
(check-unit* (:test equalp) _
  ((text-split "," "a,b,c") '("a" "b" "c"))
  ((text-split "," ",a,b,c,") '("" "a" "b" "c" ""))
  ((text-split ",|-" "-a-b,c,d-e,f" :omit 1 :test (lambda (x) (equal "-" x)))
   '("-a" "b,c,d" "e,f"))
  ((text-split "," "a,b,c" :limit 2) '("a" "b" "c"))
  ((text-split "," "a,b,c" :limit 0) '("a,b,c"))
  ((text-split "," "a,b,c" :limit 1) '("a" "b,c"))
  ((text-split "," "a,b,c" :limit 1 :omit 2) '("a,b,c"))
  ((text-split "," "a,b,c" :limit 1 :omit 1) '("a,b" "c"))
  ((text-split "," "a,b,c" :limit 2 :omit 2) '("a,b,c"))
  ((text-split "," "a,b,c," :omit 2) '("a,b,c" ""))
  ((text-split "," "a,b,c," :limit 2) '("a" "b" "c,"))
  ((text-split "," "a,b,c," :limit 2 :omit 1) '("a,b" "c" ""))
  ((text-split ",|-" "a,b-c," ) '("a" "b" "c" ""))
  ((text-split ",|-" "a,b-c," :test (lambda (x) (equal "," x))) '("a" "b-c" ""))
  
  )


(defun text-parse (regex target &key limit test)
  (when (and limit (zerop limit)) (return-from text-parse nil))
  (let (result)
    (do-text ((x s e) regex target :output :reacted-string :allow-unmatched t)
      (let ((elem (list (if (stringp x) x (subseq target s e))
                        (not (eq x :unmatched)))))
        (when (or (null test) (funcall test elem))
          (push elem result)
          (when (and limit (zerop (decf limit)))
            (return)))))
    (nreverse result)))

(text-parse "foo" "foobarfoo")
(text-parse "f(o+)o|a.?" "foobarfoooo")
(text-parse "[a-z]+" "foo , bar,baz" :limit 4 :test (lambda (x) (second x)))

(text-scan "foo" "  foo " :output t :sub-result :positions)

;; (defun text-separate/devtest (regex target &key allow-nil force)
;;   (flet ((sub (target start end  allow-nil)
;;            (if (and allow-nil
;;                     (or end
;;                         (setq end (length target))))
;;              nil
;;              (subseq target (or start 0) end))))
;;     (multiple-value-bind (match s e)
;;         (text-scan regex target :output t :sub-result :positions)
;;       (cond
;;         (match)
;;         (force (list (sub target 0 s allow-nil)
;;                      (when match (subseq target s e))
;;                      (when match (subseq target e nil allow-nil))))
;;         (t nil)))))

(defun text-separate (regex target &key allow-nil)
  (multiple-value-bind (match s e)
      (text-scan regex target :output t :sub-result :positions)
    (when match
      (if allow-nil
        (list (unless (eq 0 s) (subseq target 0 s))
              (unless (eql s e) (subseq target s e))
              (unless (eql e (length target)) (subseq target e nil)))
        (list (subseq target 0 s)
              (subseq target s e)
              (subseq target e nil))))))

(text-separate "foo" "foo")
(text-separate "" "" :allow-nil t)
;(text-separate "foo" "fooofoo" :allow-nil t :force t)
(text-scan "foo" "   foo  y" :pre-skip :spaces :post-skip :spaces :sub-result :positions)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;REGEX PATTERN PARSER `$'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun <make-scanner-form> (src opt)
  (unless (and (stringp src) (stringp opt))
    (error "xi: invalid form (regex ~A ~A)" src opt))
  `(regex ,src
          ,@(when (position #\x opt) '(:extended-mode t))
          ,@(when (position #\s opt) '(:single-line-mode t))
          ,@(when (position #\m opt) '(:multi-line-mode t))
          ,@(when (position #\i opt) '(:case-insensitive-mode t))))

(defun <parse-regex-pattern> (ptn)
  (flet ((convert-slashs (s)
           ;; バックスラッシュでエスケープされたスラッシュを変換
           (if (find #\/ s)
             (unless (text-scan "^/|[^\\\\]/" s :output t)
               (text-replace "\\\\/" "/" s))
             s))
         (fatal (s) (error "invalild regex pattern: ~S" s)))

    (cond ((text-scan "^r/" ptn)
            (aif (text-scan "^r/(.*[^\\\\])/([ismx]*)$" ptn :output :sub-strings)
                 (let ((regex (convert-slashs (svref it 0)))
                       (opts (svref it 1)))
                   (unless regex (error "~S" ptn))
                   (values regex opts nil t))
                 (fatal ptn)))
          ((text-scan "^m/" ptn)
            (aif (text-scan "^m/(.*[^\\\\])/([ismxgVLPSRG]*)$" ptn :output :sub-strings)
                 (let ((regex (convert-slashs (svref it 0)))
                       (opts (svref it 1)))
                   (unless regex (fatal ptn))
                   (values regex opts))
                 (fatal ptn)))
          ((text-scan "^s/" ptn)
            (aif (text-scan "^s/(.*[^\\\\])/(?:/|(.*[^\\\\])/)([ismxg]*)$" ptn :output :sub-strings)
                 (let ((regex (convert-slashs (svref it 0)))
                       (to (aif (svref it 1) (convert-slashs it) ""))
                       (opts (svref it 2)))
                   (unless (and regex to) (fatal ptn))
                   (values regex opts to))
                 (fatal ptn)))
          ((> (length ptn) 0)
            (values ptn ""))
          (t (fatal ptn)))))


(defun %make-regex-form% (regex-pattern)
  (multiple-value-bind (regex opt to make-scanner-only?) (<parse-regex-pattern> regex-pattern)
    ;; 要件チェック OBSOLETE
    ;;(<check> regex-pattern regex opt to)
    (let ((scanner (<make-scanner-form> regex opt)))
      (when make-scanner-only?  (return-from %make-regex-form% scanner))
    (let* ((target 'x)
           (scan-form (if to
                        (if (position #\g opt)
                         `(text-replace ,scanner ,to ,target)
                         `(text-replace ,scanner ,to ,target :limit 1))
                       (let* ((global (position #\g opt))
                              (as-vector (position #\V opt))
                              (as-list (position #\L opt))
                              (opt/P (position #\P opt))
                              (opt/R (position #\R opt))
                              (opt/S (position #\S opt))
                              (group (position #\G opt))
                              (predicate? (none opt/P opt/R opt/S group global))
                              )
                         (unless (or predicate?
                                     (only opt/P opt/R opt/S))
                           (error "exclusive"))
                         (when (none as-vector as-list) (setq as-list t))
                         (unless (only as-vector as-list)
                           (error "exclusive"))
                         (let* ((output (if group
                                          (if as-list
                                            (cond (opt/P :sub-positions-as-list)
                                                  (opt/R :sub-regions-as-list)
                                                  (t :sub-strings-as-list)) ;; DEFAULT
                                            (cond (opt/P :sub-positions)
                                                  (opt/R :sub-regions)
                                                  (t :sub-strings))) ;; DEFAULT
                                          (cond (predicate? t)
                                                (opt/P :position)
                                                (opt/R :region)
                                                (t :string)))) ;; DEFAULT
                                (form (if global
                                        `(text-matches ,scanner ,target :output ,output)
                                        `(text-scan ,scanner ,target :output ,output))))
                           (if predicate? `(when ,form t) form))))))
      `(lambda (,target) ,scan-form)))))
                           


(defmacro $ (regex-pattern &optional (arg '|no_arg|))
  (assert (stringp regex-pattern))
  (let ((lambda-expression (%make-regex-form% regex-pattern)))
    (if (eq '|no_arg| arg)
      lambda-expression
      (list lambda-expression arg))))


(defun text-compile-literal (perl-style-regex-pattern-string)
  (assert (and 'text-compile-literal
               (stringp perl-style-regex-pattern-string)))
  (%make-regex-form% perl-style-regex-pattern-string))

;; (text-compile-literal "r/foo/")
;; ($ "m/foo/" "foo")
;; (text-scan ($ "r/foo/i") "Foo")
;; (funcall (macro-function '$) '("foo") t)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun text-matches (regex target &key limit from-end)
  (assert (or (null limit) (non-negative-integer-p limit)))
  (cond ((null limit)
          (all-matches-as-strings regex target))
        ((zerop limit) nil)
        (from-end  (do ((positions (nreverse (all-matches regex target)) (cddr positions))
                        (cnt 0 (1+ cnt))
                        result)
                       ((or (>= cnt limit) (null positions))
                        result)
                     (push (subseq target (second positions) (first positions)) result)))
        (t  (let ((cnt 0)
                  result)
              (do-matches (start end regex target)
                (push (subseq target start end) result)
                (when (>= (incf cnt) limit)
                  (return)))
              (nreverse result)))))

(defun <all-unmatches> (regex target unite-matches)
  (flet ((unite-matches (positions)
           ;; 連続するマッチ部分のポジション情報を結合する (1 3 3 4 5 6 6 7) --> (1 4 5 7)
           (let ((xs positions))
             (while xs
               (if (eql (second xs) (third xs))
                 (setf (cdr xs) (cdddr xs))
                 (setf xs (cddr xs)))))
           positions))
    (let ((len (length target))
          (positions (all-matches regex target)))
      (when unite-matches
        (setq positions (unite-matches positions)))
      (cond ((null positions)  (list 0 len))
            (t
              (setq positions (if (eq 0 (first positions))
                                (cdr positions)
                                (cons 0 positions)))              
              (setq positions 
                      (if (eql len (lastcar positions))
                        (nbutlast positions)
                        (nconc positions (list len))))
              positions)))))

;(<all-unmatches> "," "a,,,bc,,d" nil)
;(<all-unmatches-as-strings> "," "a,,,bc,,d" nil)

(defun <all-unmatches-as-strings> (regex target unite-matches)
  (do ((positions (<all-unmatches> regex target unite-matches) (cddr positions))
       result)
      ((null positions)
       (nreverse result))
    (push (subseq target (first positions) (second positions)) result)))


(defun text-all-matches (regex target)
  (all-matches regex target))

(defun text-all-matches-as-strings (regex target)
  (all-matches-as-strings regex target))

(defun text-all-unmatches (regex target &key (unite-matches t))
  (<all-unmatches> regex target unite-matches))

(defun text-all-unmatches-as-strings (regex target &key (unite-matches t))
  (<all-unmatches-as-strings> regex target unite-matches))

;(text-all-unmatches-as-strings "," "ax,,bxx,cxxxx,dx")

(defmacro <do-unmatches> ((v-start v-end regex target &optional result-form) &body body)
  (with-gensyms (v1 v2 prev next first? ptn src srclen)
    `(let* ((,first? t)
            ,prev
            (,next 0)
            (,ptn ,regex)
            (,src ,target)
            (,srclen (length ,src)))
       (loop
          (multiple-value-bind (,v1 ,v2) (scan ,ptn ,src :start ,next)
            (multiple-value-bind (,v-start ,v-end) (cond (,first?
                                                           (setq ,first? nil)
                                                           (unless (eq 0 ,v1)
                                                             (values 0 ,v1)))
                                                         ((null ,v1) (unless (eql ,prev ,srclen)
                                                                       (values ,prev ,srclen)))
                                                         ((not (eql ,prev ,v1))
                                                           (values ,prev ,v1)))
              (when ,v-start
                ,@body))
            (if (and ,v1 (not (eql ,v2 ,srclen)))
              (setq ,prev ,v2
                    ,next (if (eql ,v1 ,v2) (1+ ,v2) ,v2))
              (return (progn ,result-form))))))))

(defmacro <do-unmatches-as-strings> ((v-str regex target &optional result-form) &body body)
  (with-gensyms (src start end)
    `(let ((,src ,target))
       (<do-unmatches> (,start ,end ,regex ,src ,result-form)
         (let ((,v-str (subseq ,src ,start ,end)))
           ,@body)))))

(defun text-unmatches (regex target &key limit from-end)
  (assert (or (null limit) (non-negative-integer-p limit)))
  (cond ((null limit)
          (<all-unmatches-as-strings> regex target))
        ((zerop limit) nil)
        (from-end  (do ((positions (nreverse (<all-unmatches> regex target)) (cddr positions))
                        (cnt 0 (1+ cnt))
                        result)
                       ((or (>= cnt limit) (null positions))
                        result)
                     (push (subseq target (second positions) (first positions)) result)))
        (t  (let ((cnt 0)
                  result)
              (do-matches (start end regex target)
                (push (subseq target start end) result)
                (when (>= (incf cnt) limit)
                  (return)))
              (nreverse result)))))

(defun text-replace (regex target to &key limit)
  (cond ((null limit)
          (regex-replace-all regex target to))
        ((= limit 1)
          (regex-replace regex target to))
        ((zerop limit) (string target))
        (t
          (let ((prev-pos 0)
                tmp
                (cnt 0))
            (do-matches (start end regex target)
              (push (subseq target prev-pos start) tmp)
              (push to tmp)
              (setq prev-pos end)
              (when (>= (incf cnt) limit)
                (return)))
            (push (subseq target prev-pos) tmp)
            (values (apply #'string-concat (nreverse tmp))
                    (when (> cnt 0) T))))))
            

(defun constant-true-value? (x)
  (if (consp x)
    (let ((h (car x)))
      (case h ((quote function) t))
      )
    (not (null x))))

(defun constant-false-value? (x)
  (null x))

(defun <make-call-form> (function-designator args)
  (if (or (atom function-designator)
          (and (consp function-designator)
               (eq 'LAMBDA (car function-designator))))
    (cons function-designator args)
    (list* 'FUNCALL function-designator args)))

(defun make-form/function-call (function-designator &rest args)
  (<make-call-form> function-designator args))

(defun make-form/test-call (function-designator-or-nil &rest args)
  (if function-designator-or-nil
    (<make-call-form> function-designator-or-nil args)
    T))

(defun call-1-or-true (function-designator-or-nil arg-1)
  (if function-designator-or-nil
    (funcall function-designator-or-nil arg1)
    T))
    
;(constant-true-value-p '4)
       
(defun text-split (regex target &key limit test)
  (cond ((and (null limit) (null test))
          (split regex target))
        ((< limit 2) (list (string target)))
        (t
          (let ((prev-pos 0)
                result
                (max (1- limit))
                (cnt 0))
            (do-matches (start end regex target)
              (push (subseq target prev-pos start) result)
              (setq prev-pos end)
              (when (>= (incf cnt) max)
                (return)))
            (push (subseq target prev-pos) result)
            (nreverse result)))))

@eval-when-compile
(defmacro <def/do-matches> (macroname do-matches/pos do-matches/str)
    `(defmacro ,macroname ((var-or-vars regex target &optional result-form) &body body)
         (let ((vars (cond ((symbolp var-or-vars) (list var-or-vars))
                           ((consp var-or-vars) var-or-vars)
                           (t (error "errr")))))
           (case (length vars)
             (1 `(,',do-matches/str (,(first vars) ,regex ,target ,result-form) ,@body))
             (2 `(,',do-matches/pos (,(first vars) ,(second vars) ,regex ,target ,result-form) ,@body))
             (3 (with-gensyms (src start end)
                  `(let ((,src ,target))
                     (,',do-matches/pos (,start ,end ,regex ,src ,result-form)
                       (let ((,(first vars)  ,start)
                             (,(second vars) ,end)
                             (,(third vars)  (subseq ,src ,start ,end)))
                         ,@body)))))))))

(<def/do-matches> do-text-matches do-matches do-matches-as-strings)
;(do-t-matches (s "o" "foobarboo") (print s))
;(do-t-matches ((s e) "o" "foobarboo") (print (list s e)))
;(do-t-matches ((s e r) "o" "foobarboo") (print (list s e r)))

(<def/do-matches> do-text-unmatches <do-unmatches> <do-unmatches-as-strings>)
;(do-t-unmatches (s "o" "foobarboo") (print s))
;(do-t-unmatches ((s e) "o" "foobarboo") (print (list s e)))
;(do-t-unmatches ((s e r) "o" "foobarboo") (print (list s e r)))


#Comment
(defun text-replace-if (regex target &key limit)
