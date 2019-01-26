;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(in-package :cl-user)

(clpgk.core:clpgk-core-header)

(defpackage :clpgk.base.unify-global-var (:use))

(clpgk.core:define-package :clpgk.base.unify* (:clpgk.base.unify)
  (:use :cl)
  (:import/export :clpgk.base.hash-table*)
  (:export 
   #:bound-unification-variable-p
   #:unbind-unification-variable
   #:u-nil
   #:u-null

   #:do-unify
   #:do-unify-if      #:do-unify-gif
   #:do-unify-when    #:do-unify-gwhen
   #:do-unify-unless  #:do-unify-gunless

   #:define-unify-macro
   #:define-unify-subr

   #:define-unification
   #:this
   #:error

   
   ;; 以下のオペレータは J-MORE-UNIFY-MATCH-DEFS.LISPで定義される
   #:u-case #:u-acase       #:u-gcase #:u-gacase
   #:u-case/w #:u-acase/w   #:u-gcase/w #:u-gacase/w
   #:u-cond #:u-gcond
   #:u-bind #:u-bind*

  )
)


(in-package :clpgk.base.unify)



;; アノテーション @guard
;; ガード構文
;; @guard a b -> (:AND a (:HERE b))
(define-annotation guard (before after) `(:AND ,before (:HERE ,after)))

(defvar *unify-rvalue* '|wrong-place|)
;@eval-always
(defvar *pkg-unify-global-var* (find-package :clpgk.base.unify-global-var))

(defun <register-readers> ()
  @select-reader :unify
 (set-dispatch-macro-character 
  #\# #\?
  (lambda (stream char1 char2)
    @ignore (char1 char2)
    (let ((sym (read stream t nil t)))
      @car (mapcar #/(intern _ *pkg-unify-global-var*)
                   (list (string-concat "?" (symbol-name sym)))))))

  @select-reader :unify
 (set-dispatch-macro-character 
  #\# #\`
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    (list '|unif-quot|
          (read stream t nil t))))

  @select-reader :unify
 (set-dispatch-macro-character 
  #\# #\U
  (lambda (stream char1 char2 &aux (tmp (gensym)))
    @ignore (char1 char2)
    `(lambda (,tmp) (do-unify ,tmp ,(read stream t nil t)))))


;; 入力マクロ {}
;; {symbol ...} -> (:-> symbol :WHOLE ...)
;; {:keyword ...} -> (:-> :keyword ...)
;; {exp ...} -> (:-> other ...)
;; {} -> (:AND ?) , {?a} -> (:AND ?a) , {?a ?b} -> (:AND ?a ?b)
;; これらに変数が先行する場合も、適切に処理される
;; {?a ?b symbol ...} -> (:-> ?a ?b sybmol :WHOLE ...)
;; なお、{}{?a}などが:ANDで構造化されるのは意図的なものであり、
;; 普通は構造化の意図を持って使用される（リスト・ベクタに対する評価順序の制御）
 @select-reader clpgk.core.reader::%internal%
 (set-macro-character #\} (get-macro-character #\)))

 @select-reader clpgk.core.reader::%internal%
 (set-macro-character 
  #\{
  (lambda (stream char)
    @ignore (char)
    (let* ((contents (read-delimited-list #\} stream t))
           (main (member-if-not #/(and (symbolp _) (clpgk.base.unify::is-var? _))
                                contents))
           (header (subseq contents 0 (- (length contents) (length main))))
           (x (car main)))
      (cond ((null main)
              (case (length header)
                (0 '(:AND ?))
                (t `(:AND ,@header))))
            ((and (symbolp x) #!(keywordp x))
              `(:-> ,@header ,x :whole ,@(cdr main)))
            (t 
              `(:-> ,@contents)))))))

(register-reader-registerer '|unify-1| '<register-readers>)

 
(defstruct <terr>)
(defvar <terr> (make-<terr>))



;; １つだけ真の場合のみ真となる。成功すれば、真となった値を返す。
;; (defmacro only (&rest xs)
;;   (cond 
;;     ((null xs) nil)
;;     ((null (cdr xs)) (car xs))
;;     (t (let (tmp
;;              (flag (gensym))
;;              (r (gensym)))
;;          (dolist (x (cdr xs))
;;            (push `(if (setq ,r ,x) 
;;                    (if ,flag 
;;                      (return-from ONLY nil) 
;;                      (setq ,flag ,r))) 
;;                  tmp))
;;          `(block ONLY (let (,flag)
;;                         (setq ,flag ,(car xs))
;;                         ,@(nreverse tmp)
;;                         ,flag))))))


;(defvar *match-rule-generators* (make-hash-table))

(define-symbol-macro u-nil '*UNBOUND-UNIFICATION-VARIABLE*)


(defun bound-unification-variable-p (x)
  (not (eq '*UNBOUND-UNIFICATION-VARIABLE* x)))

(define-compiler-macro bound-unification-variable-p (x) 
  `(not (eq '*UNBOUND-UNIFICATION-VARIABLE* ,x)))

(defun u-null (x) (eq '*UNBOUND-UNIFICATION-VARIABLE* x))
(define-compiler-macro u-null (x) `(eq '*UNBOUND-UNIFICATION-VARIABLE* ,x))

(defmacro unbind-unification-variable (place &rest other-places) 
  `(setf 
    ,place '*UNBOUND-UNIFICATION-VARIABLE*
    ,@(mapcan #/(list _ ''*UNBOUND-UNIFICATION-VARIABLE*)
              other-places)))

(defvar *impossible* #())

(defstruct uinfo 
  all-vars (var-prefix #\?) (test 'equal) labels macro-table)

'(defmethod print-object ((x uinfo) stream) 
  (format stream "<uinfo>"))

(defmacro internal-label (x) 
  `(intern (concatenate 'string "_unif_label_" (symbol-name ,x)) :clpgk.base.unify))

(defmacro unbound-var? (x) `(eq '*UNBOUND-UNIFICATION-VARIABLE* ,x))

(defmacro true-list? (xs) ;循環リストには不可
  `(null (cdr (last ,xs))))

(defun general-list-length (xs) ;循環リストには不可
  (if xs
    (let ((last (last xs)))
      (do ((i 1 (1+ i))
           (xs xs (cdr xs)))
          ((eq xs last) i)))
    0))

(defun is-list-length-just (n xs)
  (do ((xs xs (cdr xs))
       (i n (1- i)))
      ((or (zerop i) (atom xs))
       (and (zerop i) (null xs)))))


(defun is-list-length-greater-than (n xs)
  (do ((xs xs (cdr xs))
       (i n (1- i)))
      ((or (zerop i) (atom xs))
       (zerop i))))

(defun is-var? (x) (eql #\? (char (symbol-name x) 0)))
(defun is-wildcard? (x) (= 1 (length (symbol-name x))))
(defun is-forced-var? (x &aux (name (symbol-name x)))
  (and (> (length name) 2)
       (eql #\? (char name 1))))


(defun to-var (x &aux (name (symbol-name x)))
  (if (and (> (length name) 2)
           (eql #\? (char name 1))) 
    (intern (subseq name 2))
    (intern (subseq name 1))))

(defun var-to-?var (x)
  (intern (string-concat "?" (symbol-name x))))


;;;

(defun <reserved-unify-kwd?> (kwd)
  (member kwd 
          '(:and :or :not :here :do :for :type :-> :call :list*
            :only :none :for :if :if-not :env
            :always :never :then :then-not :to
            :each :each+ :accum :accum+ 
            :sv-each :sv-each+ :sv-accum :sv-accum+ 
            :seq-each :seq-each+ :seq-accum :seq-accum+ 
            :case :only-case :comp :comp-not
            :prefix :suffix :prefixes :suffixes 
            :with :decomp :decomp*
            :assign :apply :return :define :subr :macro
            :scan :fold :fork :chain
            :unify :match
            :append :save)))

(defun structured? (x)
  (and (consp x)
       (let ((s (car x)))
         (or (and (keywordp s)
                  (or (<reserved-unify-kwd?> s)
                      t
                      (progn (warn "unify: (:~D ...)は(#`:~D ...)にエスケープすべき" s s)
                             nil)))
             (eq '|unif-quot| s)))))


(defmacro do-unify-compare (info x y)
  (let ((f (uinfo-test info)))
    (if (symbolp f)
      (list f x y)
      (list 'funcall f x y))))

(defmacro pre-same? (src dst info)
  (cond ((symbolp src) 
          (if (is-var? src)
            (cond ((is-wildcard? src) t)
                  ((is-forced-var? src) 
                    `(progn (setq ,(to-var src) ,dst) t))
                  (t (let ((var (to-var src)))
                       `(if (unbound-var? ,var) 
                         (progn (setq ,var ,dst) t)
                         (do-unify-compare ,info ,var ,dst)))))
            `(eq ',src ,dst)))
        ((consp src) 
          ;(if (eq :-> (car src))
          (if (structured? src)
            t
            (if (true-list? src)
              `(and
                (consp ,dst)
                (is-list-length-just ,(length src) ,dst))
              `(and
                (consp ,dst)
                (is-list-length-greater-than ,(general-list-length src) ,dst)))))
        ((simple-vector-p src) 
            `(and (simple-vector-p ,dst) (= ,(length src) (length ,dst))))
        ((numberp src) `(eql ,src ,dst))
        ((characterp src) `(eq ,src ,dst))
        (t (list 'do-unify-compare info src dst))))
                                                                  
(defun inner-structure? (x) 
  (or (consp x) 
      (and (simple-vector-p x) (/= 0 (length x)))))

(defmacro <check> (ptn dst info)
  (if (inner-structure? ptn)
    (let* ((var (gensym))
           (pre-code (make-pre-code ptn dst var info))
           (post-code (make-post-code ptn dst var info)))
      `(let ((,var ,dst)) (and ,pre-code ,post-code)))))

(defmacro simple-unify (ptn dst info)
  (let ((var (gensym)))
    `(let ((,var ,dst))
      (and
        (pre-same? ,ptn ,var ,info)
        ,@(when (inner-structure? ptn)
                `((<check> ,ptn ,var ,info)))))))
    
(defmethod make-post-code ((ptn vector) dst var info)
  (let (tmp
        (len (length ptn)))
    (do ((i 0 (1+ i)))
        ((= i len))
      (when (inner-structure? (svref ptn i))
        (if (structured? (svref ptn i))
          (push `(complex-unify (svref ,var ,i) ,(svref ptn i)  ,info) tmp)
          (push `(<check> ,(svref ptn i) (svref ,var ,i) ,info) tmp))))
    (if tmp
      `(progn (setq ,var ,dst) (and ,@(nreverse tmp)))
      t)))
       
(defmethod make-post-code (other dst var info)
  (declare (ignore other) (ignore dst) (ignore var) (ignore info))
  t)


(defun valid-form-header? (h) (or (symbolp h) (consp h)))

(defun expand-for-form (f &rest args)
  ; 予めfの型に関しては調べられていることが条件
  (if (symbolp f)
    `(,f ,@args)
    (if (consp f)
      (if (eq 'lambda (car f))
        `(,f ,@args)
        `(,@f ,@args)))))

    
(defun special-pre-code (ptn dst var info)
  (declare (ignore dst))

  (unless (consp ptn)
    (error "UNIFY LIBRALY: illegal :special clause"))

  (let ((success t)
        (h (car ptn)))

    (while (and (symbolp h) 
                (is-var? h))
      (setq success `(and ,success (pre-same? ,h ,var ,info)))
      (setq ptn (cdr ptn))
      (setq h (car ptn)))
      
      
    (when (and (symbolp h) 
               (not (keywordp h)))
      (unless (funcall '<rule-exist-p> h)
      ;(unless (gethash h *match-rule-generators*)
        (error "match rule ~D not found" h))
      (return-from special-pre-code success))

    (when (and (not (eq :append h))
               (not (true-list? ptn)))
      (error "truelist required"))

                                        ; TODO　エラー処理
    (cond ((member h '(:member :not-member
                               :type :not-type
                               :eq :not-eq
                               :eql :not-eql
                               :equal :not-equal
                               :equalp :not-equalp))
              success)
          ((member h '(:member-by :not-member-by))
            (if (and (>= (length ptn) 2)
                     (valid-form-header? (second ptn)))
              success
              (error "C")))
          ((member h '(:and :or :none :only))
            (dolist (f (cdr ptn) success)
              (unless (valid-form-header? f)
                (error "B"))))
          
          ((member h '(:view :peek))
            (if (and (>= (length ptn) 2)
                     (consp (second ptn)))
              success
              (error ":-> :~D valid lambda list required" h)))

          ((member h '(:length= :length/= :length< :length<= :length> :length>=))
            (if (= (length ptn) 2)
              success
              (error ":-> :~D 引数(1)が必要" h)))
          ((member h '(:length<< :length<=< :length<=<= :length<<=))
            (if (= (length ptn) 3)
              success
              (error ":-> :~D 引数(2)が必要" h)))

          ((eq h :setf)
            (if (>= (length ptn) 2)
              success
              (error ":setf place(s) required")))

          ((eq h :push)
            (if (>= (length ptn) 2)
              success
              (error ":push place(s) required")))
          ((eq h :pushnew)
            (if (>= (length ptn) 2)
              success
              (error ":pushnew place(s) required")))

          ;; OBSOLETE
          ((eq h :unify)
            (warn "(:-> :UNIFY ...) is DEPLICATED")
            (if (= (length ptn) 2)
              success
              (error "C")))

          ;; OBSOLETE
          ((eq h :to)
            (warn "(:-> :to ...) is DEPLICATED. use (:to ...)")
            (if (>= (length ptn) 2)
              success
              (error "C")))

          ((member h '(:true :false :null :pass :not-pass))
            (if (= (length ptn) 1)
              success
              (error "C")))

          ((consp h)
            (dolist (x (cdr ptn) success)
              (when (atom x)
                (error "curried form or lambda-expression or #'function required: ~D" x))))
          

          (t (error "UNKNOWN")))))


(defun special-post-code (ptn dst var info &aux (h (car ptn)))


  (while (and (symbolp h) (is-var? h))
    (setq ptn (cdr ptn))
    (setq h (car ptn)))
  
  ;; 値マッチングの場合（既にチェック済み）
  (when (and (symbolp h) 
             (not (keywordp h)))
    (return-from special-post-code
      ;(funcall (gethash h *match-rule-generators*) var (cdr ptn) info)))
      (funcall '<generate-match-code> h var (cdr ptn) info)))

  ; TODO　エラー処理
  (acond ((or (eq :member h)
             (eq :not-member h))
          (let (tmp)
            (dolist (x (cdr ptn))
              (push `(do-unify-compare ,info ',x ,var) tmp))
            (let ((exp `(or ,@(nreverse tmp))))
              (if (eq :member h)
                exp
                (list 'not exp)))))

         ((assoc h '((:eq . eq) (:not-eq . eq) (:eql . eql) 
                     (:not-eql . eql)
                     (:equal . equal) (:not-equal . equal)
                     (:equalp . equalp) (:not-equalp . equalp)))
          (let (tmp)
            (setq tmp (dolist (x (cdr ptn) (nreverse tmp))
                        (push `(,(cdr it) ,var ',x) tmp)))
            (if (eq #\N (char (symbol-name h) 0)) ;; not-で始まる
              `(not (or ,@tmp))
              `(or ,@tmp))))

         ;; OBSOLETE
         ((eq :unify h)
          (list 'complex-unify dst (second ptn) info))

         ((or (eq :member-by h)
              (eq :not-member-by h))
          (let (tmp
                (f (second ptn)))
            (setq tmp (dolist (x (cddr ptn) (nreverse tmp))
                        (push (expand-for-form f var (list 'quote x)) tmp)))
            (cond ((eq :member-by h) `(or ,@tmp))
                  ((eq :not-member-by h) `(not (or ,@tmp))))))
         
         ;; OBSOLETE
         ((eq :to h) 
          (funcall 'code-for-to (cdr ptn) var info))

        ((or (eq :type h)
             (eq :not-type h))
          (let (tmp)
            (dolist (x (cdr ptn))
              (push `(<typecomp> ,var ,x) tmp))
            (let ((exp `(or ,@(nreverse tmp))))
              (if (eq :type h)
                exp
                (list 'not exp)))))

        ((member h '(:and :or :none :only))
          (let (tmp)
            (setq tmp (dolist (f (cdr ptn) (nreverse tmp))
                        (push (expand-for-form f var) tmp)))
            (cond ((eq :and h) `(and ,@tmp))
                  ((eq :or h) `(or ,@tmp))
                  ((eq :none h) `(not (or ,@tmp)))
                  ((eq :only h) `(only ,@tmp)))))

        ((member h '(:view :peek))
         #{let ((main `((lambda ,@(cdr ptn)) ,var)))
         (if (eq h :view)
           main
           `(progn ,main t)))

        ((member h '(:length= :length/= :length< :length<= :length> :length>=))
         `(and 
           (typep ,var 'sequence)
           (,(intern (symbol-name h) :clpgk.base.unify) ,(second ptn) ,var)))
           
        ((member h '(:length<< :length<=< :length<=<= :length<<=))
         `(and 
           (typep ,var 'sequence)
           (,(intern (symbol-name h) :clpgk.base.unify) ,(second ptn) ,(third ptn) ,var)))

        ((eq :setf h)
         `(progn 
           (setf ,@(mapcan #/(list _ var) (cdr ptn)))
           t))

        ((eq :push h)
         `(progn 
           ,@(mapcar #/(list 'push var _) (cdr ptn))
           t))
        ((eq :pushnew h)
         `(progn 
           ,@(mapcar #/(list 'pushnew var _) (cdr ptn))
           t))
           

        ((eq :true h) var)
        ((or (eq :false h) (eq :null h)) (list 'null var))
        ((eq :pass h) t)
        ((eq :not-pass h) nil)

        ;; カリー化関数(表現)の合成適用
        ((consp h)
         (reduce (lambda (a b) (case (first a)
                                 (LAMBDA    (list a b))
                                 (FUNCTION  (list 'funcall a b))
                                 (t         `(,@a ,b))))
                 ptn 
                 :from-end t :initial-value var))

        ))



(defmethod make-post-code ((ptn cons) dst var info)
  (if (eq :-> (car ptn))
    (special-post-code (cdr ptn) dst var info)
    (flet ((optimized-cdr (i) `(,@(cond ((= 1 i) '(cdr))
                                        ((= 2 i) '(cddr))
                                        ((= 3 i) '(cdddr))
                                        ((= 4 i) '(cddddr))
                                        (t (list 'nthcdr i)))
                                ,var)))
      (let (tmp (tmpvar (gensym)))
        (do ((i 0 (1+ i))
             (p ptn (cdr p)))
            ((atom p)
             (when (and (not (null p))
                        (inner-structure? p))
               (push `(setq ,tmpvar ,(optimized-cdr i)) tmp)
               (push `(<check> ,p ,tmpvar ,info) tmp)
                 
               ;(push `(<check> ,p ,(optimized-cdr i) ,info) tmp)
               )
             )

          (when (inner-structure? (car p))
            (let ((d (if (= 0 i)
                       var
                       `(setq ,var ,(optimized-cdr i)))))
              (setq i 0)
              (push `(setq ,tmpvar ,d) tmp)  
              (if (structured? (car p))
                (push `(complex-unify (car ,tmpvar) ,(car p) ,info) tmp)
                (push `(<check> ,(car p) (car ,tmpvar) ,info) tmp)))))
        (if tmp
          `(let (,tmpvar) (setq ,var ,dst) (and ,@(nreverse tmp)))
          t)))))

(defmethod make-pre-code ((ptn cons) dst var info)
  (if (eq :-> (car ptn))
    (special-pre-code (cdr ptn) dst var info)
    (let (tmp)
      (do ((ptn (cdr ptn) (cdr ptn)))
          ((atom ptn)
           (unless (null ptn)
             (push `(pre-same? ,ptn (cdr ,var) ,info) tmp)))
        (push `(progn (setq ,var (cdr ,var)) (pre-same? ,(car ptn) (car ,var) ,info))
              tmp))
      `(and 
        (pre-same? ,(car ptn) (car ,var) ,info)
        ,@(nreverse tmp)))))
 
(defmethod make-pre-code ((ptn vector) dst var info)
  (declare (ignore dst))
  (let (tmp)
    (dotimes (i (length ptn))
      (push `(pre-same? ,(svref ptn i) (svref ,var ,i) ,info) tmp))
    `(and ,@(nreverse tmp))))


(defun save/restore-pair (vars)
  (let (tmpvars)
    (dotimes (_ (length vars))
      (push (gensym) tmpvars))
    (if tmpvars
      (values (mapcar #'list tmpvars vars)
              `((setq ,@(mapcan #'list vars tmpvars))))
      (values nil nil))))

(defun temporary-save/restore-pair (vars)
  (let (tmpvars)
    (dotimes (_ (length vars))
      (push (gensym) tmpvars))
    (if tmpvars
      (values tmpvars
              `((setq ,@(mapcan #'list tmpvars vars)))
              `((setq ,@(mapcan #'list vars tmpvars))))
      (values nil nil nil))))

;;;

(defun mk (start end)
  (let (tmp)
    (do ((xs start (cdr xs)))
        ((eq xs end)
         ;(push (car end) tmp)
         (nreverse tmp))
      (push (car xs) tmp))))

(defun search-flag (ptn minimize)
  (cond ((null ptn) nil)
        ((null (car ptn)) (search-flag (cdr ptn) minimize))
        ((eq :minimize (car ptn)) (search-flag (cdr ptn) t))
        ((eq :maximize (car ptn)) (search-flag (cdr ptn) nil))
        (t (values ptn minimize))))
            
(defun end? (ptn)
  (if ptn
    (if (member (car ptn) '(:minimize :maximize))
      (end? (cdr ptn)))
    t))

(defmacro append/match (ptn xs info minimize)
  (multiple-value-bind (p m) (search-flag ptn minimize)
    (setq ptn p
          minimize m))

  (cond 
    ((null ptn) `(null ,xs))
    ;((null (car ptn)) (list 'append/match (cdr ptn) xs info minimize))

    ;((and (consp (car ptn)) (eq :and (caar ptn)))

    ((null (search-flag (cdr ptn) nil))
      (list 'complex-unify xs (car ptn) info)) 

    ((structured? (car ptn))
      (let ((r (gensym)) (start (gensym)) (sub (gensym))) 
        (multiple-value-bind (save restore) 
            (save/restore-pair (uinfo-all-vars info))
          (if minimize               
            `(let (,@save ,r (,start ,xs))
              (do ((,xs ,xs (cdr ,xs)))
                  ((progn ;(setq ,var (mk ,start ,xs)) 
                          (or (or (and (listp ,xs) (let ((,sub (mk ,start ,xs))) (complex-unify ,sub ,(car ptn) ,info)) (setq ,r (append/match ,(cdr ptn) ,xs ,info ,minimize)))
                                  (progn ,@restore nil))
                              (atom ,xs)))
                   ,r)))
            (multiple-value-bind (tmp tmpsave tmprestore)
                (temporary-save/restore-pair (uinfo-all-vars info))
              (let ((rr (gensym)))
                `(let (,@save ,@tmp ,r ,rr (,start ,xs))
                  (do ((,xs ,xs (cdr ,xs)))
                      ((progn ;(setq ,var (mk ,start ,xs))
                              (let ((,rr (and (listp ,xs) (let ((,sub (mk ,start ,xs))) (complex-unify ,sub ,(car ptn) ,info))  (append/match ,(cdr ptn) ,xs ,info ,minimize))) )
                                (when ,rr (setq ,r t) ,@tmpsave)
                                ,@restore         
                                (atom ,xs)))
                       (when ,r ,@tmprestore t))))))))))

    ((listp (car ptn))
      (let (tmp
            final
            (var (gensym)))
        (do ((p (car ptn) (cdr p)))
            ((atom p)
             (setq final (if (null p)
                           `(append/match ,(cdr ptn) ,var ,info ,minimize)
                           `(and ,(end? (cdr ptn)) (complex-unify ,var ,p ,info)))))
          (push `(when (and (consp ,var) (complex-unify (car ,var) ,(car p) ,info))
                  (setq ,var (cdr ,var))
                  t)
                tmp))
        `(let ((,var ,xs))
          (and ,@(nreverse tmp)
               ,final))))

      ;;　以下、事前チェックによりシンボルであることが確定
    ((is-wildcard? (car ptn))
      (if (end? (cdr ptn)) 
        t
        (multiple-value-bind (save restore) 
            (save/restore-pair (uinfo-all-vars info))
          (if minimize
                                        ;(print xs)
            (let ((r (gensym)))
              `(let (,@save ,r)
                (do ((,xs ,xs (cdr ,xs)))
                    ((or (or (and (listp ,xs) (setq ,r (append/match ,(cdr ptn) ,xs ,info ,minimize)))
                             (progn ,@restore nil))
                         (atom ,xs))
                     ,r))))
            (multiple-value-bind (tmp tmpsave tmprestore)
                (temporary-save/restore-pair (uinfo-all-vars info))
              (let ((r (gensym)) (rr (gensym)))
                `(let (,@save ,@tmp ,r ,rr)
                  (do ((,xs ,xs (cdr ,xs)))
                      ((let ((,rr (and (listp ,xs) (append/match ,(cdr ptn) ,xs ,info ,minimize))))
                         (when ,rr (setq ,r t) ,@tmpsave)
                         ,@restore
                         (atom ,xs))
                       (when ,r ,@tmprestore t))))))))))

    ;;通常変数
    (t
      (let ((var (to-var (car ptn))))
        `(cond 
          ((or ,(is-forced-var? (car ptn)) 
               (unbound-var? ,var))
           ,(cond ((end? (cdr ptn)) `(progn (setq ,var ,xs) t)) ;`(progn (setq ,var (copy-list ,xs)) t)) ;TODO rest部を使うように
                  (t (let ((r (gensym)) (start (gensym))) 
                       (multiple-value-bind (save restore) 
                           (save/restore-pair (uinfo-all-vars info))
                       (if minimize               
                         `(let (,@save ,r (,start ,xs))
                           (do ((,xs ,xs (cdr ,xs)))
                               ((progn (setq ,var (mk ,start ,xs)) 
                                       (or (or (and (listp ,xs) (setq ,r (append/match ,(cdr ptn) ,xs ,info ,minimize)))
                                               (progn ,@restore nil))
                                           (atom ,xs)))
                                ,r)))
                         (multiple-value-bind (tmp tmpsave tmprestore)
                             (temporary-save/restore-pair (uinfo-all-vars info))
                           (let ((rr (gensym)))
                             `(let (,@save ,@tmp ,r ,rr (,start ,xs))
                               (do ((,xs ,xs (cdr ,xs)))
                                   ((progn (setq ,var (mk ,start ,xs))
                                      (let ((,rr (and (listp ,xs) (append/match ,(cdr ptn) ,xs ,info ,minimize))) )
                                      (when ,rr (setq ,r t) ,@tmpsave)
                                      ,@restore         
                                      (atom ,xs)))
                                    ;(when ,r (setq ,var (mk ,start ,end)) t)
                                    (when ,r ,@tmprestore t))))))))))))
          ((listp ,var)
           (do ((,xs ,xs (cdr ,xs))
                (,var ,var (cdr ,var)))
               ((or (atom ,var) (atom ,xs) (not (do-unify-compare ,info (car ,var) (car ,xs))))
                (cond ((null ,var)
                        (append/match ,(cdr ptn) ,xs ,info ,minimize))
                      ((and ,(end? (cdr ptn))
                            (atom ,var))
                        (do-unify-compare ,info ,var ,xs))
                  )))))))


    ))

;;;

(defun <get-internal-symbol> (sym internal suffix-str)
  (aif (get sym internal)
       it
       #{let ((new (gensym (string-concat suffix-str (symbol-name sym) ))))
       (setf (get sym internal) new)))

(defun <get-macro-symbol> (ident)
  (<get-internal-symbol> ident '|unify-macro| "UNIFY-MACRO-"))

(defmacro define-unify-macro (name (&rest lambda-list) &body body)
  (unless (symbolp name)
    (error "DEFINE-UNIFY-MACRO: ~D is not a symbol" name))
  (when (<reserved-unify-kwd?> name)
    (error "DEFINE-UNIFY-MACRO: ~D is a reserved keyword" name))

  #{let ((sym (<get-macro-symbol> name)))
  (when (fboundp sym) 
    (warn "DEFINE-UNIFY-MACRO: redefining ~D" name))

  `(progn 
    (defun ,sym ,lambda-list ,@body)
    ',name))

(defun <expand-unify-macro> (repl info)
  #{let ((table (uinfo-macro-table info)))
  (unless table
    (setq table
            (setf (uinfo-macro-table info) (make-hash-table))))
  #{multiple-value-bind (expanded exists) (gethash repl table)
  (if exists
    expanded
    #{let* ((name (first repl))
            (sym (<get-macro-symbol> name)))
    (unless (fboundp sym)
      (error "UNIFY-MACRO: undefined macro :~D" name))
    (unless (proper-list-p repl)
      (error "UNIFY-MACRO: invalid macro argument: ~D" repl))
    (setf (gethash repl table) 
            (apply sym (cdr repl)))))


(defun <get-subr-symbol> (subr)
  (<get-internal-symbol> subr '|unify-subr| "UNIFY-SUBR-"))

(defmacro define-unify-subr (name (&rest lambda-list) &body defs)
  (assert (and 'define-unify-subr
               (symbolp name)
               (<= 1 (length defs))))
  #{let ((sym (<get-subr-symbol> name))
         (tmp (gensym)))
  (when (fboundp sym) 
    (warn "DEFINE-UNIFY-SUBR: redefining ~D" name))
  `(progn 
    (defun ,sym (,tmp ,@lambda-list)
      (do-unify ,tmp ,(if (cdr defs) `(:and ,@defs) (first defs))))
    ',name))




(defmacro <typecomp> (x type-spec)
  (if (not (symbolp type-spec))
    `(typep ,x ',type-spec)
    (aif (assoc type-spec '((symbol . symbolp) (keywordp . keywordp) (cons . consp)
                            (list . listp) (integer . integerp) (number . numberp)
                            (float . floatp) (null . null) (vector . vectorp)
                            (string . stringp) (simple-vector . simple-vector-p)))
         (list (cdr it) x)
         (if (eq t type-spec)
           t
           `(typep ,x ',type-spec)))))

(defmacro type-unify (ptn x info)
  (cond ((symbolp ptn)
          (if (is-var? ptn) 
            (cond ((is-wildcard? ptn) t)
                  ((is-forced-var? ptn)
                    `(progn (setq ,(to-var ptn) (type-of ,x)) t))
                  (t (let ((var (to-var ptn)))
                       `(if (unbound-var? ,var) 
                         (progn (setq ,var (type-of ,x)) t)
                         (typep ,x ,var)))))
            (list '<typecomp> x ptn)))
        ((vectorp ptn) 
          (let (tmp
                (v (gensym)))
            (push `(vectorp (setq ,v ,x)) tmp)
            (push `(= ,(length ptn) (length ,v)) tmp)
            (dotimes (i (length ptn))
              (push `(type-unify ,(svref ptn i) (svref ,v ,i) ,info) tmp))
            `(let (,v) (and ,@(nreverse tmp)))))
        ((consp ptn)
          (let (tmp
                (v (gensym)))
            (push `(consp (setq ,v ,x)) tmp)
            ;(push `(= ,(length ptn) (length ,v)) tmp)
            (do ((ptn ptn (cdr ptn))
                 (not-first nil t))
                ((atom ptn)
                 (if (null ptn)
                   (push `(null (cdr ,v)) tmp)
                   (push `(type-unify ,ptn (cdr ,v) ,info) tmp))
                 `(let (,v) (and ,@(nreverse tmp))))
              (when not-first (push `(consp (setq ,v (cdr ,v))) tmp))
              (push `(type-unify ,(car ptn) (car ,v) ,info) tmp))))
        (t (error ":TYPE unknown type specifier"))))
              

(defun <transform-define-clauses> (define-clauses)
  ;; define節の各内部節が((name [...]) ptn...)のものを、
  ;; (name (:env [...] (:AND ptn...))に変形する
  ;; また、(name ptn...)は必要に応じて(name (:AND ptn...))に変形される
  ;; なお、define-clausesは、事前に構文の要件がチェックされているものとする
  (mapcar #/(let ((main (if (eql 1 (length #>cdr))
                          #>second
                          `(:AND ,@#>cdr))))
              (cond ((symbolp #>first) (list #>first main))
                    (t `(,#>caar (:env ,@#>cdar ,main)))))
          define-clauses))


(defmacro united-complex-unify (src complex-ptns info)
    (list 'complex-unify 
          src 
          (if (eq 1 (length complex-ptns))
            (first complex-ptns)
            `(:AND ,@complex-ptns))
          info))
          

(defun <check-decomp-clauses> (clauses)
  (every #'proper-list-p clauses))

(defun <check-decomp*-clauses> (clauses)
  #{let ((n (length clauses)))
  (and (consp clauses)
       (do ((c clauses (cdr c))
            (i 1 (1+ i)))
           ((= i n) t)
         (unless (and (consp (car c))
                      (proper-list-p (car c)))
           (return nil)))))

;(is-list-length-greater-than 3 (circulate '(1 2 3)))

(defun <make-decomp*-code> (clauses src info)
  #{let* ((ptn-for-rest (lastcar clauses))
          (clauses (butlast clauses))
          (counter-pairs (mapcar #/(list (gensym) #>car) 
                                 clauses))
          (tmp (gensym)))
  `(and 
    (listp ,src)
    (let (,@counter-pairs
          (,tmp ,src))
      (and (is-list-length-greater-than (+ ,@(mapcar #'car counter-pairs)) 
                                        ,src)
           ,@(mapcar @\ab `(prog1
                           (let ((,tmp (subseq ,tmp 0 ,(first a))))
                             (united-complex-unify ,tmp ,(cdr b) ,info))
                           (setq ,tmp (nthcdr ,(first a) ,tmp)))
                     counter-pairs
                     clauses)
           (complex-unify ,tmp ,ptn-for-rest ,info)
           ))))
    
(defun <make-decomp-code> (clauses src info)
  #{let ((counter-pairs (mapcar #/(list (gensym) #>car) 
                              clauses))
         (tmp (gensym)))
  `(and 
    (proper-list-p ,src)
    (let (,@counter-pairs
          (,tmp ,src))
      (and (eql (length ,src)
                (+ ,@(mapcar #'car counter-pairs)))
           ,@(mapcar @\ab `(prog1
                           (let ((,tmp (subseq ,tmp 0 ,(first a))))
                             (united-complex-unify ,tmp ,(cdr b) ,info))
                           (setq ,tmp (nthcdr ,(first a) ,tmp)))
                     counter-pairs
                     clauses
                     )))))
    


;;;





;; j-match.lispからも参照される
(defun %make-unify-code% (src ptns)
  `(do-unify ,src ,(if (eq 1 (length ptns))
                       (first ptns)
                       `(:and ,@ptns))))



(defmacro complex-unify (src complex-ptn info)
  (labels 
      ((f (ptn src info) 
         #{let* ((is-a-cons? (consp ptn))
                 (head (when is-a-cons? (first ptn))))
         (if is-a-cons?
           (cond
             ((and (not (proper-list-p ptn))
                   (structured? ptn))
               (error "UNIFY: illegal structure: ~D" ptn))
             ((member head '(:each :each+ :accum :accum+))
               (unless (<= 1 (length (cdr ptn)))
                 (error "EACH"))
               (multiple-value-bind (save restore) 
                   (save/restore-pair (uinfo-all-vars info))
                 (when (member head '(:accum :accum+))
                   (setq save nil 
                         restore nil))
                 (let ((q (gensym))
                       (x (gensym))
                       (cnt (gensym))
                       (len (length (cdr ptn))))
                   `(when (and
                            (proper-list-p ,src)
                            ,@(when (member head '(:each+ :accum+))
                                    `((consp ,src)))
                            ,@(if (< 1 len) `((zerop (mod (length ,src) ,len))) nil))
                     (let ,save
                                  
                       (do ((,q ,src (cdr ,q))
                            (,cnt 0 (1+ ,cnt)))
                           ((null ,q)
                            t)
                         (if (let ((,x (car ,q)))
                               ,(if (< 1 len) 
                                    `(case (mod ,cnt ,len)
                                      ,@(let (tmp)
                                             (dotimes (i len)
                                               (push `(,i (not (complex-unify ,x ,(nth (1+ i) ptn) ,info))) tmp))
                                             (nreverse tmp)))
                                    `(not (complex-unify ,x ,(second ptn) ,info))))
                           (progn ,@restore (return nil))
                           ,@(when (member head '(:each :each+)) 
                                   (if (= 1 len) 
                                     restore
                                     `((when (eql ,(1- len) (mod ,cnt ,len)) ,@restore)))))))))))
                                     
             ((member head '(:seq-each :seq-each+ :seq-accum :seq-accum+))
               (unless (<= 1 (length (cdr ptn)))
                 (error "SEQEACH"))
               (multiple-value-bind (save restore) 
                   (save/restore-pair (uinfo-all-vars info))
                 (when (member head '(:seq-accum :seq-accum+))
                   (setq save nil 
                         restore nil))
                 (let ((q (gensym))
                       (end (gensym))
                       (x (gensym))
                       (cnt (gensym))
                       (len (length (cdr ptn))))
                   `(when (and 
                            (typep ,src 'sequence)
                            (or #!(listp ,src)
                                (proper-list-p ,src))
                            ,@(when (member head '(:seq-each+ :seq-accum+))
                                    `((< 0 (length ,src))))
                            ,@(if (< 1 len) `((zerop (mod (length ,src) ,len))) nil))
                     (let ,save
                                  
                       (do ((,q (if (listp ,src) ,src 0) (if (consp ,q) (cdr ,q) (1+ ,q)))
                            (,cnt 0 (1+ ,cnt))
                            (,end (unless (listp ,src) (length ,src))))
                           ((if (listp ,q) (null ,q) (= ,q ,end))
                            t)
                         (if (let ((,x (if (consp ,src) (car ,q) (aref ,src ,q))))
                               ,(if (< 1 len) 
                                    `(case (mod ,cnt ,len)
                                      ,@(let (tmp)
                                             (dotimes (i len)
                                               (push `(,i (not (complex-unify ,x ,(nth (1+ i) ptn) ,info))) tmp))
                                             (nreverse tmp)))
                                    `(not (complex-unify ,x ,(second ptn) ,info))))
                           (progn ,@restore (return nil))
                           ,@(when (member head '(:seq-each :seq-each+)) 
                                   (if (= 1 len) 
                                     restore
                                     `((when (eql ,(1- len) (mod ,cnt ,len)) ,@restore)))))))))))
                                     
                                    

             ((member head '(:sv-each :sv-each+ :sv-accum :sv-accum+))
               (unless (<= 1 (length (cdr ptn)))
                 (error "SVEACH"))
               (multiple-value-bind (save restore) 
                   (save/restore-pair (uinfo-all-vars info))
                 (when (member head '(:sv-accum :sv-accum+))
                   (setq save nil 
                         restore nil))
                 (let ((q (gensym))
                       (end (gensym))
                       (x (gensym))
                       (cnt (gensym))
                       (len (length (cdr ptn))))
                   `(when (and 
                            (simple-vector-p ,src)
                            ,@(when (member head '(:sv-each+ :sv-accum+))
                                 `((< 0 (length ,src))))
                            ,@(if (< 1 len) `((zerop (mod (length ,src) ,len))) nil))
                     (let ,save
                                  
                       (do ((,q 0 (1+ ,q))
                            (,cnt 0 (1+ ,cnt))
                            (,end (length ,src)))
                           ((= ,q ,end)
                            t)
                         (if (let ((,x (svref ,src ,q)))
                               ,(if (< 1 len) 
                                    `(case (mod ,cnt ,len)
                                      ,@(let (tmp)
                                             (dotimes (i len)
                                               (push `(,i (not (complex-unify ,x ,(nth (1+ i) ptn) ,info))) tmp))
                                             (nreverse tmp)))
                                    `(not (complex-unify ,x ,(second ptn) ,info))))
                           (progn ,@restore (return nil))
                           ,@(when (member head '(:sv-each :sv-each+)) 
                                   (if (= 1 len) 
                                     restore
                                     `((when (eql ,(1- len) (mod ,cnt ,len)) ,@restore)))))))))))
                                     
                                    

                                                             
           ((eq :or head)
            (multiple-value-bind (save restore) 
                (save/restore-pair (uinfo-all-vars info))
              `(block ESCAPE
                (let ,save
                  ,@(mapcar (lambda (p) (if (funcall 'is-do? p)
                                          (list 'complex-unify src p info)
                                          `(if (complex-unify ,src ,p ,info)
                                            (return-from ESCAPE t)
                                            ,@restore)))
                            (cdr ptn)))
                nil)))
           (nil (eq :and head)
            (multiple-value-bind (save restore) 
                (save/restore-pair (uinfo-all-vars info))
              `(block ESCAPE
                (let ,save
                  ,@(mapcar (lambda (p) (if (funcall 'is-do? p)
                                          (list 'complex-unify src p info)
                                          `(unless (complex-unify ,src ,p ,info)
                                            ,@restore
                                            (return-from ESCAPE nil))))
                            (cdr ptn)))
                t)))
           ((eq :and head)
             (case (length (cdr ptn))
               (0 t)
               (1 (list 'complex-unify src (second ptn) info))
               (t
                 `(block ESCAPE
                   ,@(mapcar (lambda (p) (if (funcall 'is-do? p)
                                           (list 'complex-unify src p info)
                                           `(unless (complex-unify ,src ,p ,info)
                                             (return-from ESCAPE nil))))
                             (cdr ptn))
                   t))))

           ((eq :only head)
             (multiple-value-bind (save restore) 
                 (save/restore-pair (uinfo-all-vars info))
               (multiple-value-bind (succeeded-save succeeded-restore) 
                   (save/restore-pair (uinfo-all-vars info))
                 (let* ((already (gensym))
                        (store `(setq ,already t ,@(mapcan #'copy-list succeeded-save))))
                   `(block ESCAPE
                     (let (,already 
                           ,@save
                           ,@(mapcar #'car succeeded-save))
                       ,@(mapcar (lambda (p) (if (funcall 'is-do? p)
                                               (list 'complex-unify src p info)
                                               `(progn 
                                                 (when (complex-unify ,src ,p ,info)
                                                   (if ,already
                                                     (return-from ESCAPE nil)
                                                     ,store))
                                                 ,@restore)))
                                 (cdr ptn))
                       (when ,already ,@succeeded-restore t)))))))


           ((eq :none head)
            (multiple-value-bind (save restore) 
                (save/restore-pair (uinfo-all-vars info))
              `(block ESCAPE
                (let ,save
                  ,@(mapcar (lambda (p) (if (funcall 'is-do? p)
                                          (list 'complex-unify src p info)
                                          `(if (prog1 (complex-unify ,src ,p ,info) ,@restore)    
                                            (return-from ESCAPE nil))))
                            (cdr ptn)))
                t)))
           ((eq :not head) ;ただしNOT節の中でDO節が使われた場合の挙動は未定義
            (unless (= 1 (length (cdr ptn)))
              (error "unify NOT: illegal")) 
            (multiple-value-bind (save restore) 
                (save/restore-pair (uinfo-all-vars info))
              (let ((tmp (gensym)))
                `(let (,@save (,tmp (complex-unify ,src ,(second ptn) ,info)))
                  ,@restore
                  (not ,tmp)))))

           ((eq :type head)
            (unless (= 1 (length (cdr ptn)))
              (error "unify TYPE: illegal")) 
             `(type-unify ,(second ptn) ,src ,info))

           ((or (eq :if head)
                (eq :if-not head))
            (unless (let ((len (length (cdr ptn))))
                      (or (= 2 len) (= 3 len)))
              (error "unify IF: illegal"))
            (let ((test-clause (list 'complex-unify src (second ptn) info))
                  (then-clause (list 'complex-unify src (third ptn) info))
                  (else-clause (when (= 3 (length (cdr ptn))) (list 'complex-unify src (fourth ptn) info))))
              (multiple-value-bind (save restore) 
                  (save/restore-pair (uinfo-all-vars info))
                (if (eq :if head)
                  (setq else-clause `(progn ,@restore ,else-clause))
                  (setq test-clause (list 'not test-clause)
                        then-clause `(progn ,@restore ,then-clause)))
                `(let ,save
                  (if ,test-clause ,then-clause ,else-clause)))))


           ((eq :case head)
             (unless (funcall 'valid-case-clause? (cdr ptn))
               (error "CASE"))
             (multiple-value-bind (save restore) 
                 (save/restore-pair (uinfo-all-vars info))
               (let (tmp
                     (cnt (length (cdr ptn))))
                 (dolist (clause (cdr ptn))
                   (push `(if (complex-unify ,src ,(first clause) ,info)
                             (return-from UCASE-RET 
                               (united-complex-unify ,src ,(cdr clause) ,info))
                             ,@(unless (zerop (decf cnt)) restore))
                         tmp))
                 `(block UCASE-RET (let ,save ,@(nreverse tmp))))))

           ((eq :only-case head)
             (unless (funcall 'valid-case-clause? (cdr ptn))
               (error "ONLY-CASE"))
             (multiple-value-bind (save restore) 
                 (save/restore-pair (uinfo-all-vars info))
               (multiple-value-bind (succeeded-save succeeded-restore) 
                   (save/restore-pair (uinfo-all-vars info))
                 (let (tmp
                       (cnt -1)
                       (pos (gensym))
                       (setq-clauses (mapcan 'copy-list succeeded-save)))
                   (dolist (clause (cdr ptn))
                     (push `(when (complex-unify ,src ,(first clause) ,info)
                             (when ,pos 
                               (return-from ONLYCASE-RET nil))
                             (setq ,pos ,(incf cnt) ,@setq-clauses)
                             ,@restore)
                           tmp))
                   (setq cnt -1)
                   `(block ONLYCASE-RET 
                     (let (,pos ,@save ,@(mapcar #'car succeeded-save) )
                       ,@(nreverse tmp)
                       (when ,pos 
                         ,@succeeded-restore
                         (case ,pos
                           ,@(mapcar #/(list (incf cnt) 
                                             `(united-complex-unify ,src ,(cdr _) ,info))
                                     (cdr ptn))))))))))
                 
           ((eq :append head)
             (unless (funcall 'valid-append-clause? (cdr ptn))
               (error "APPEND"))
             `(and (listp ,src)
                   (append/match ,(cdr ptn) ,src ,info t)))
               
           ((eq :list* head)
             (unless (and (proper-list-p (cdr ptn))
                          (<= 2 (length (cdr ptn))))
               (error "LIST*"))
             (let* ((x (intern "!clpgk.base.unify-list*-tmpvar!"))
                    (x? (memoized (intern "??!clpgk.base.unify-list*-tmpvar!"
                                          *pkg-unify-global-var*)))
                    (new-ptn/1 (append (butlast (cdr ptn)) x?))
                    (new-ptn/2 (lastcar ptn))
                    (new-ptn `(:WITH (let (,x))
                               (:AND ,new-ptn/1 (:FOR ,x ,new-ptn/2)))))
               (when (structured? new-ptn/1)
                 ;; 先頭のキーワードをエスケープ
                 (setf (car new-ptn/1) `(:-> :EQ ,(car new-ptn/1)))) 
               `(complex-unify ,src ,new-ptn ,info)))


           ((eq :prefix head)
             (unless (<= 2 (length (cdr ptn)))
               (error "PREFIX"))
             #{with-gensyms (f tmp)
             `(flet ((,f (,tmp) (complex-unify ,tmp ,(lastcar ptn) ,info)))
               (complex-unify
                ,src 
                (:OR 
                  (:LIST* ,@(butlast (cdr ptn)) (:HERE (,f _)))
                  (:HERE (,f _)))
                ,info)))

           ((eq :prefixes head)
             (unless (<= 2 (length (cdr ptn)))
               (error "PREFIXES"))
             #{let ((tmp (gensym)))
             #{let* ((n (length (cdr ptn)))
                     (m (1- n))
                     (fns (freplicate m #'gensym))
                     (aps (mapcar #/`(:HERE (,_ _)) fns))
                     (new-ptn `(:OR 
                                (:LIST* ,(second ptn) ,@aps)
                                ,@(maplist #/(if (<= 2 #>length)
                                               `(:LIST* ,@_)
                                               #>car)
                                           aps))))
             `(flet ,(mapcar @\ab`(,a (,tmp) (complex-unify ,tmp ,b ,info))
                             fns (cddr ptn))
               (complex-unify ,src ,new-ptn ,info)))

           ((eq :suffix head)
             (unless (<= 2 (length (cdr ptn)))
               (error "SUFFIX"))
             #{with-gensyms (g f tmp)
             `(flet ((,g (,tmp) (complex-unify ,tmp ,(second ptn) ,info))
                     (,f (,tmp) (complex-unify ,tmp ,(lastcar ptn) ,info)))
               (complex-unify
                ,src 
                (:OR 
                  (:LIST* (:HERE (,g _)) ,@(butlast (cddr ptn)) (:HERE (,f _)))
                  (:LIST* (:HERE (,g _)) (:HERE (,f _))))
                ,info)))

           ((eq :suffixes head)
             (unless (<= 2 (length (cdr ptn)))
               (error "SUFFIXES"))
             #{let ((tmp (gensym)))
             #{let* ((n (length (cdr ptn)))
                     (m (1- n))
                     (fns (freplicate m #'gensym))
                     (aps (mapcar #/`(:HERE (,_ _)) fns))
                     (rest (lastcar aps))
                     (new-ptn `(:LIST* 
                                ,(second ptn)
                                (:OR
                                  ,@(maplist #/`(:LIST* ,@(reverse _) ,rest)
                                             (nreverse (nbutlast aps)))
                                  ,rest))))
             `(flet ,(mapcar @\ab`(,a (,tmp) (complex-unify ,tmp ,b ,info))
                             fns (cddr ptn))
               (complex-unify ,src ,new-ptn ,info)))


           ((eq :with head)
             (unless (and (<= 1 (length (cdr ptn)))
                          (consp (second ptn))
                          (proper-list-p (second ptn)))
               (error "WITH"))
             (append (second ptn) 
                     (list `(united-complex-unify ,src ,(cddr ptn) ,info))))

             

           ((eq :for head)
             (unless (<= 1 (length (cdr ptn)))
               (error "FOR"))
             (let ((x (gensym)))
               `(let ((,x ,(second ptn)))
                 (united-complex-unify ,x ,(cddr ptn) ,info))))
           
           ((eq :call head) 
             (let ((x src)
                   pair)
               (case (length ptn)
                 (2)
                 (3 (setq x (third ptn)))
                 (otherwise (error "APPLY")))
               (unless (setq pair (assoc (second ptn) (uinfo-labels info)))
                 (error "APPLY: undefined label ~D" (second ptn)))
               (list (cdr pair) x)))

           ((eq :subr head) 
             (unless (and (<= 1 (length (cdr ptn)))
                          (symbolp (second ptn)))
               (error ":SUBR"))
             #{let ((internal (<get-subr-symbol> (second ptn))))
             (unless (fboundp internal)
               (warn ":SUBR : undefined subroutine ~D" (second ptn)))
             `(,internal ,src ,@(cddr ptn)))


           ((eq :return head)
             (when (< 1 (length (cdr ptn)))
               (error ":RETURN"))
             `(if (eq '|wrong-place| *unify-rvalue*)
               (error ":RETURN : in wrong place")
               (progn (setq *unify-rvalue* ,(if (cdr ptn) (second ptn) src))
                      t)))
             
           ((member head '(:assign :apply))
             (unless (= 2 (length (cdr ptn)))
               (error ":~D" head))
             #{with-gensyms (tmp rvalue)
             #{let ((lv (second ptn))
                    (rv (third ptn))
                    (right-info (copy-uinfo info)))
             (pushnew '*unify-rvalue* (uinfo-all-vars right-info))
             (when (eq head :apply)
               (rotatef lv rv))
                     
             ` (multiple-value-bind (,tmp ,rvalue) 
                   (let ((*unify-rvalue* '|unbound|))
                     (values (complex-unify ,src ,rv ,right-info)
                             *unify-rvalue*))
                 (when (and ,tmp #!(eq '|unbound| ,rvalue))
                   (complex-unify ,rvalue ,lv ,info))))
             

           ((eq :define head)
             (unless (and (= 2 (length (cdr ptn)))
                          (funcall 'valid-define-list? (second ptn)))
               (error ":DEFINE"))
             #{let* ((tmp (gensym))
                     (def (<transform-define-clauses> (second ptn)))
                     (new-labels (uinfo-labels info))
                     (flet-body (mapcar #/(let ((label (internal-label #>first)))
                                            (prog1
                                              `(,label (,tmp) 
                                                (complex-unify ,tmp ,#>second ,info))
                                              (push (cons #>first label) new-labels)))
                                        def))
                     (new-uinfo (copy-uinfo info)))
             (setf (uinfo-labels new-uinfo) new-labels)
             
             `(labels ,flet-body
               (complex-unify ,src ,(third ptn) ,new-uinfo)))
             

           ((member head '(:then :then-not))
             (unless (<= 1 (length (cdr ptn)))
               (error ":~D" head))
             #{let* ((op (if (eq head :then) :if :if-not))
                     (new-ptn `(,op ,
                                (second ptn) 
                                (:and ,@(cddr ptn)) 
                                (:-> :pass))))
             (list 'complex-unify src new-ptn info))
             

           ((eq :fork head)
             (unless (= 2 (length (cdr ptn)))
               (error ":FORK"))
             #{let ((rvalue (gensym))
                    (tmp (gensym))
                    (new-info (copy-uinfo info)))
             (pushnew '*unify-rvalue* (uinfo-all-vars new-info))
             `(multiple-value-bind (,tmp ,rvalue)
               (let ((*unify-rvalue* '|unbound|))
                 (values (complex-unify ,src ,(second ptn) ,new-info)
                         *unify-rvalue*))
               (when ,tmp
                 (if (eq '|unbound| ,rvalue)
                   t
                   (complex-unify ,rvalue ,(third ptn) ,info)))))
               
           ((eq :always head) 
             t)
           ((eq :never head)
             nil)

           ((eq :chain head)
             (unless (<= 1 (length (cdr ptn)))
               (error ":CHAIN"))
             #{let ((rvalue (gensym))
                    (tmp (gensym))
                    (new-info (copy-uinfo info)))
             (pushnew '*unify-rvalue* (uinfo-all-vars new-info))
             `(let ((,rvalue ,src))
               ,(reduce @\ab `(multiple-value-bind (,tmp ,rvalue)
                              (let ((*unify-rvalue* '|unbound|))
                                (values (complex-unify ,rvalue ,a ,new-info)
                                        *unify-rvalue*))
                              (when ,tmp
                                (if (eq '|unbound| ,rvalue) t ,b)))
                        (butlast (cdr ptn))
                        :from-end t
                        :initial-value 
                        `(complex-unify ,rvalue ,(lastcar ptn) ,info))))


           ((or (eq :scan head)
                (eq :fold head))
             #{let ((rvalue (gensym))
                    (new-info (copy-uinfo info)))
             (pushnew '*unify-rvalue* (uinfo-all-vars new-info))
             (multiple-value-bind (save restore) 
                 (save/restore-pair (uinfo-all-vars info))
               (when (eq :fold head)
                 (setq save nil 
                       restore nil))
               `(let (,@save
                      (,rvalue ,src))
                 (loop
                   (let ((*unify-rvalue* '|unbound|))
                     (unless (united-complex-unify ,rvalue ,(cdr ptn) ,new-info)
                       (return nil))
                     (when (eq '|unbound| *unify-rvalue*)
                       (return t))
                     ,@restore
                     (setq ,rvalue *unify-rvalue*))))))

           ((eq :decomp head)
             (unless (<check-decomp-clauses> (cdr ptn))
               (error ":DECOMP"))
             (<make-decomp-code> (cdr ptn) src info))

           ((eq :decomp* head)
             (unless (<check-decomp*-clauses> (cdr ptn))
               (error ":DECOMP*"))
             (<make-decomp*-code> (cdr ptn) src info))
 
           ((eq :save head)             
             (unless (= 2 (length (cdr ptn)))               
               (error "SAVE"))
             (unless (funcall 'valid-variables-list? (second ptn))
               (error "SAVE: invalid variable list"))
             (multiple-value-bind (save restore) (save/restore-pair (second ptn))
               `(let(,@save) (if (complex-unify ,src ,(third ptn) ,info)
                               t
                               (progn ,@restore nil)))))

          ;; TODO: OBSOLETE?
           (nil (or (eq :while head)
                    (eq :unless head))
             (unless (= 2 (length (cdr ptn)))               
               (error "WHILE"))
             (multiple-value-bind (save restore) 
                 (save/restore-pair (uinfo-all-vars info))
               `(block U-WHILE
                 (let ,save
                   (tagbody U-WHILE-LOOP
                     (if ,(if (eq head :while)
                              `(complex-unify ,src ,(second ptn) ,info)
                              ` (unless (complex-unify ,src ,(second ptn) ,info)
                                  (progn ,@restore t)))
                       (if (complex-unify ,src ,(third ptn) ,info)
                         (progn (setq ,@(mapcan #'copy-list save))
                                (go U-WHILE-LOOP))
                         (return-from U-WHILE  nil))
                       (progn ,@(when (eq head :while) restore)
                              (return-from U-WHILE t))))))))

           ((eq :env head)
             (funcall 'code-for-env src (cdr ptn) info))

           ((eq :do head)
             (case (second ptn)
               (:unbind `(progn (unbind-unification-variable ,@(cddr ptn)) t))
               (otherwise `(let ((_ ,src)) (declare (ignorable _))  ,@(cdr ptn) t))))

           ((eq :here head)
            `(let ((_ ,src)) (declare (ignorable _)) ,@(cdr ptn)))

           ((member head '(:comp :comp-not))
             (unless (eq 2 (length ptn))
               (error ":~D" head))
             #{let ((code (list 'do-unify-compare info src (second ptn))))
             (if (eq head :comp)
               code
               (list 'not code)))

           ((eq :unify head)
             (%make-unify-code% src (cdr ptn)))

           ((eq :match head)
             ;; %make-match-code%は、j-match.lisp内で定義される
             ;; よって、この時点では未定義である
             (funcall '%make-match-code% src (cdr ptn)))

           ((eq :to head) 
             (funcall 'code-for-to (cdr ptn) src info))

           ((eq '|unif-quot| head)
             `(do-unify-compare ,info ,src ',(second ptn) ))
           
           
           ((eq :macro head)
             (unless (and (<= 1 (length (cdr ptn)))
                          (symbolp (second ptn)))
               (error ":MACRO : ~D is not a macro name" (second ptn)))
             `(complex-unify ,src ,(<expand-unify-macro> (cdr ptn) info) ,info))

           ;; TODO
           ((and (keywordp head)
                 (not (eq :-> head)))
             ;; マクロの場合
             `(complex-unify ,src ,(<expand-unify-macro> ptn info) ,info))
             

           (t (list 'simple-unify ptn src info)))
         (list 'simple-unify ptn src info))))

  (f complex-ptn src info)))

;;;;;;;;;;;;;;;;;;;;;;;;;; END OF J-UNIFY-1.LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
