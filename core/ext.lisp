;; @ clip

(oleo.core.init:define-package :oleo.core.ext ()
  (:use :cl :oleo.core.init :anaphora :cl-annot )

  (:import/export :oleo.core.basic-definitions)

  #+clisp (:import-from ext #:string-concat #:getenv #:memoized #:with-collect)
  #-clisp (:import-from kmrcl #:getpid)
  
  (:export

   #:implement-later
   
   #:_ 
   
   ;#:define-package ;; from 0-init.lisp
   ;#:export* ;; from 0-init.lisp

   #:dolist*

   #:multiple-value-bind-if #:multiple-value-bind-when
   #:multiple-value-bind* #:multiple-value-bind-if* #:multiple-value-bind-when*

   #:destructuring-bind-if #:destructuring-bind-when
   #:destructuring-bind* #:destructuring-bind-if* #:destructuring-bind-when*

   #:string-concat #:string-join
   #:getenv
   #:memoized #:memoized-with-checking
   #:system
   #:getpid

   #:with-collect

   #:do-sources #:do-multiple-value-sources

   #:console-message

   #:compiler-macroexpand-1 ;; コンパイラマクロの展開
   #:macroform ;; 通常マクロ及びコンパイラマクロである場合に展開するマクロ


   ;; シーケンスに特化した比較器
   #:sequal

   ;; alist utilities
   #:alist-compare
   #:alist-eq #:alist-eql #:alist-equal #:alist-equalp

   #:unsafe-alist-plist #:unsafe-plist-alist
   #:alist-bind #:alist-ebind #:alist-rebuild-bind #:alist-rebuild-ebind
   #:alist-update-ebind
   #:make-alist-let #:make-alist-let*
   #:constant-alist
   ;; plist utilities
   #:plist-compare
   #:plist-eq #:plist-eql #:plist-equal #:plist-equalp

   #:plist-bind #:plist-ebind #:plist-rebuild-bind #:plist-rebuild-ebind
   #:plist-update-ebind
   #:make-plist-let #:make-plist-let*
   
   )

  (:import/export-from annot.eval-when #:eval-when-compile #:eval-when-load #:eval-when-execute #:eval-always)
  (:import/export-from annot.doc #:doc)

  )

(in-package :oleo.core.ext)

(defun <error/not-implemented> (type sym args)
  (error "the ~A `~A' is not implemented!! args=~A" type sym args))
(defun <error/not-implemented/function> (sym args)
  (<error/not-implemented> "FUNCTION" sym args))
(defun <error/not-implemented/macro> (sym args)
  (<error/not-implemented> "MACRO" sym args))
(defun <error/not-implemented/var> (sym)
  (error "the SPECIAL VARIABLE `~A' is not implemented!!" sym))

(defun <implement-later> (forms)
  (flet ((warn/ignore (x) `(warn "IMPLEMENT-LATER: IGNORE ~A" ',x)))
    (let (tmp)
      (dolist (x forms tmp)
        (cond ((consp x)
                (case (first x)
                  ((DEFGENERIC WARN ERROR CONSOLE-MESSAGE) ;; 無視しない
                    (push x tmp))
                  ((PROGN IMPLEMENT-LATER)
                    (setq tmp (nconc (<implement-later> (cdr x)) tmp)))
                  ((EVAL-WHEN) (push `(eval-when ,(second x)
                                        ,@(nreverse (<implement-later> (cddr x))))
                                     tmp))
                  ((DEFUN) (push `(defun ,(second x) (&rest args)
                                    (<error/not-implemented/function>  ',(second x) args))
                                 tmp))
                  ((DEFMACRO) (push `(defmacro ,(second x) (&rest args)
                                       `(<error/not-implemented/macro> ',',(second x) ',args))
                                    tmp))
                  ((QUOTE DECLARE DECLAIM PROCLAIM) ;; DO NOTHING
                    )
                  ((DEFVAR DEFPARAMETER DEFCONSTANT)
                    (push `(define-symbol-macro ,(second x)
                             (<error/not-implemented/var> ',(second x)))
                          tmp))
                  (t (push (warn/ignore x) tmp))))
              (t
                (push (warn/ignore x) tmp))))

      )))

(defmacro implement-later (&body forms)
  (let ((tmp (<implement-later> forms)))
    (case (length tmp)
      (0 '(eval-when ()))
      (1 (car tmp))
      (t `(progn ,@(nreverse tmp))))))


;; (implement-later (eval-when a b))
;; (implement-later (foo a) b c 3)
;; (implement-later a (progn b c (progn d e)))
;; (implement-later (defun foobar (a b) c))
;; (implement-later (defmacro foobar (a b) c))
;; (implement-later (defvar svar1 0))
;; (implement-later (defparameter svar2 0))
;; svar2
;; (foobar 1 2)


(defun system (cmdline)
  (asdf:run-shell-command cmdline))

(defun compilermacroexpand-1 (form)
  (if (and (consp form)
           (alexandria:proper-list-p form)
           (symbolp (first form)))
    (aif (compiler-macro-function (first form))
         (values (funcall it form nil) T)
         form)
    form))


(defmacro macroform (form)
  (multiple-value-bind (form* expanded?) (compilermacroexpand-1 form)
    (if expanded?
      form*
      (macroexpand-1 form))))


(defun <collect-vars/helper> (x)
  (declare (special **tmpvars))
  (cond ((consp x)
          (mapcar #'<collect-vars/helper> x))
        ((eq x '_)
          (let ((newvar (gensym)))
            (push newvar **tmpvars)
            newvar))
        (t x)))

(defun <collect-vars> (vars)
  (let (**tmpvars)
    (declare (special **tmpvars))
    (values (<collect-vars/helper> vars) **tmpvars)))

;(<collect-vars> '_)
;(<collect-vars> '(a b _ (_ c)))
;(<collect-vars> '(a (b c _ x) c))
  

(defun <mk-vars-bind*> (op bind-form value-form body)
  (multiple-value-bind (vars tmpvars) (<collect-vars> bind-form)
    `(,op ,vars ,value-form
          (DECLARE (IGNORABLE ,@tmpvars))
          ,@body)))
    

(defun <mk-multi-bind-if> (op vars value-form then-form else-form &optional allow-omitted-vars)
  (let ((n (length vars))
        tmpvars)
    (dotimes (_ n) (declare (ignorable _))
             (push (gensym) tmpvars))
    `(,op ,tmpvars ,value-form
       (if ,(first tmpvars)
         (let ,(if allow-omitted-vars
                 (mapcan (lambda (v tmpv) (unless (eq v '_) (list (list v tmpv))))
                         vars tmpvars)
                 (mapcar #'list vars tmpvars))
           ,@(when allow-omitted-vars
               `((DECLARE (IGNORABLE ,@(mapcan (lambda (v tmpv) (when (eq v '_) v (list tmpv)))
                                               vars tmpvars)))))
           ,then-form)
         ,else-form))))

(defun <mk-multi-bind-if*> (op vars value-form then-form else-form)
  (<mk-multi-bind-if> op vars value-form then-form else-form T))

(defmacro destructuring-bind* (vars value-form &body body)
  (<mk-vars-bind*> 'DESTRUCTURING-BIND vars value-form body))
(defmacro multiple-value-bind* (vars value-form &body body)
  (<mk-vars-bind*> 'MULTIPLE-VALUE-BIND vars value-form body))

;; (destructuring-bind* (a _ b) (list 1 2 3) (list b a))
;; (multiple-value-bind* (a _ b) (values 1 2 3) (list a b))

(defun <mk-list-bind-if> (op vars value-form then-form else-form)
  (let ((tmpvar (gensym)))
    `(let ((,tmpvar ,value-form))
       (if ,tmpvar
         (,op ,vars ,tmpvar
              ,then-form)
         ,else-form))))



(defun <mk-list-bind-if*> (op vars value-form then-form else-form)
  (let ((tmpvar-1 (gensym)))
    (multiple-value-bind (lambdalist tmpvars) (<collect-vars> vars)
      
      `(let ((,tmpvar-1 ,value-form))
         (if ,tmpvar-1
           (,op ,lambdalist ,tmpvar-1
                (DECLARE (IGNORABLE ,@tmpvars))
                ,then-form)
           ,else-form)))))

;(<mk-list-bind-if*> 'ope '(a _ (_ b)) 'exp 'then 'else)

(defmacro multiple-value-bind-if (vars expr then-form &optional else-form)
  (<mk-multi-bind-if> 'MULTIPLE-VALUE-BIND vars expr then-form else-form))
(defmacro multiple-value-bind-when (vars expr &body body)
  (<mk-multi-bind-if> 'MULTIPLE-VALUE-BIND vars expr (cons 'PROGN body) nil))
(defmacro multiple-value-bind-if* (vars expr then-form &optional else-form)
  (<mk-multi-bind-if*> 'MULTIPLE-VALUE-BIND* vars expr then-form else-form))
(defmacro multiple-value-bind-when* (vars expr &body body)
  (<mk-multi-bind-if*> 'MULTIPLE-VALUE-BIND* vars expr (cons 'PROGN body) nil))

;; (multiple-value-bind-if (a b) (values 1 2) (list a b) 'failed)
;; (multiple-value-bind-when (a b) (values 1 2) (list a b))
;; (multiple-value-bind-if* (a _ b) (values 1 2 3) (list a b) 'failed)
;; (multiple-value-bind-if* (a _ b) nil (list a b) 'failed)
;; (multiple-value-bind-when* (a _ b) (values 1 2 3) (list a b))
;; (multiple-value-bind-when* (a _ b) nil (list a b))

(defmacro destructuring-bind-if (vars expr then-form &optional else-form)
  (<mk-list-bind-if> 'DESTRUCTURING-BIND vars expr then-form else-form))
(defmacro destructuring-bind-when (vars expr &body body)
  (<mk-list-bind-if> 'DESTRUCTURING-BIND vars expr (cons 'PROGN body) nil))
(defmacro destructuring-bind-if* (vars expr then-form &optional else-form)
  (<mk-list-bind-if*> 'DESTRUCTURING-BIND vars expr then-form else-form))
(defmacro destructuring-bind-when* (vars expr &body body)
  (<mk-list-bind-if*> 'DESTRUCTURING-BIND vars expr (cons 'PROGN body) nil))

;; (destructuring-bind-if (a b) (list 1 2) (list a b) 'failed)
;; (destructuring-bind-when (a b) (list 1 2) (list a b))
;; (destructuring-bind-if* (a _ b) (list 1 2 3) (list a b) 'failed)
;; (destructuring-bind-if* (a _ b) nil (list a b) 'failed)
;; (destructuring-bind-when* (a _ b) (list 1 2 3) (list a b))


;(multiple-value-bind-if (a b) (list 1 2) (+ a b) 2)


;(defun testes (x) (list x x))
;(define-compiler-macro testes (x) `(list 'ok ,x))
;(macroexpand-1 (testes 1))
;(funcall (compiler-macro-function 'testes) '(testes 23) nil)
;(compilermacroexpand-1 '(1))


(defun console-message (control-string &rest params)
  (format *error-output* "~%##SYSTEM## ")
  (apply #'format *error-output* control-string params)
  (format *error-output* "~%")
  (finish-output *error-output*))

(defmacro dolist* ((var-or-bindform value-form &optional ret-form) &body body)
  (if (symbolp var-or-bindform)
    `(dolist (,var-or-bindform ,value-form ,ret-form) ,@body)
    (let ((tmp (gensym)))
      `(dolist (,tmp ,value-form ,ret-form)
        (bind ((,var-or-bindform ,tmp))
          ,@body)))))


;; 文字列のリストを受け取って連結するバージョン
;; 場合によっては、こちらのほうが効率が良いはず
(defun string-join (string-list)
  (let ((n 0))
    (dolist (s string-list)
      (assert (stringp s))
      (incf n (length s)))
    (let ((new (make-string n))
          (cur 0))
      (dolist (s string-list)
        (declare (type string s))
        (dotimes (i (length s))
          (setf (char new cur) (char s i))
          (incf cur)))
      new)))

#-clisp
(defun string-concat (&rest xs) (string-join xs))

;;OBSOLETE-->; (defun string-concat (&rest xs) (apply #'concatenate 'string xs))


;(string-join '("ab" "cb" "okokok!"))
                
      

#-clisp
(defmacro memoized (form)
  (let ((tmpvar (gensym)))
    `(if (boundp ',tmpvar)
       (symbol-value ',tmpvar)
       (setf (symbol-value ',tmpvar) ,form))))

;; 更新を伴う可能性のある場合のメモ化構文
;; 保存されたvariable-formの値と、都度呼び出し時のvariable-formを、与えられたcheck関数に新旧の値を渡す
;; 任意のcheck関数の比較によって真が返されれば既にメモ化された値を使用し、偽が返されれば値を更新する
;; デフォルトでは新旧のチェック値をEQで比較する
(defmacro memoized-with-checking (form variable-form &optional (check '(lambda (new old) (eq new old))))
  (let ((tmpvar (gensym)))
    `(car (if (and (boundp ',tmpvar)
                   (funcall ,check (cdr (symbol-value ',tmpvar)) ,variable-form))
            (symbol-value ',tmpvar)
            (setf (symbol-value ',tmpvar) (cons ,form ,variable-form))))))

;;(defun foo (x)
;;  (memoized-with-checking (random 100) x))

#+sbcl
(defun getenv (name) (sb-ext:posix-getenv name))
#+sbcl
(defun (setf getenv) (x name) (setf (sb-ext:posix-getenv name) x))

#+ccl
(defun getenv (name) (ccl:getenv name))
#+ccl
(defun (setf getenv) (x name)
  (ccl:setenv name x)
  x)

#+clisp
(defun getpid () (ext:process-id))

#-clisp
(defmacro with-collect ((&rest collectors) &body body)
  (let* ((m (length collectors))
         (stacks (mapcar (lambda (_) (declare (ignore _)) (gensym))
                         collectors))
         (macrolet-defs (mapcar (lambda (c s)
                                  `(,c (x)
                                       `(let ((x ,x))
                                          (push x ,',s) x)))
                                collectors stacks)))
    `(macrolet ,macrolet-defs
       (let ,stacks
         ,@body
         (values ,@(mapcar (lambda (s) (list 'nreverse s))
                         stacks))))))



(defmacro do-sources ((var (&rest src-forms) &optional ret-form) &body body)
  (let ((tmp (gensym))
        (n (length src-forms)))
    `(dotimes (,tmp ,n ,ret-form)
      (let ((,var (case ,tmp
                    ,@(let (cases) 
                           (dotimes (i n (nreverse cases))
                             (push (list i (nth i src-forms)) cases))))))
        ,@body))))

(defmacro do-multiple-value-sources (((&rest vars) (&rest src-forms) &optional ret-form)
                                     &body body)
  (let ((tmp (gensym))
        (n (length src-forms)))
    `(dotimes (,tmp ,n ,ret-form)
      (multiple-value-bind ,vars (case ,tmp
                                   ,@(let (cases) 
                                          (dotimes (i n (nreverse cases))
                                            (push (list i (nth i src-forms)) cases))))
        ,@body))))



;;;;;;;;;; SEAUAL ;;;;;;;;;;;;;;;;;
;;;  主にシーケンス比較に特化した比較関数
;;;  テスト用途を想定しているため、EVALやALISTを使うなど不効率ではあるが
;;;  関数として実装しているので利用範囲は広い
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *<sequal/test>*)
(defvar *<sequal/wildcard>*)
(defvar *<sequal/var-prefix>*)
(defvar *<sequal/eval>*)
(defvar *<sequal/var-alist>*)

(defun <sequal/isvar?> (x)
  (and (symbolp x)
       (not (keywordp x))
       (let ((s (symbol-name x)))
         (and (> (length s) 0)
              (eq (char s 0) *<sequal/var-prefix>*)))))

(defun <sequal/compare> (a b)
  (or (eql a b)
      (funcall *<sequal/test>* a b)))

;; 比較対象のシーケンスの基本構造はリストとシンプルベクタから成るものとする
;; それ以外のシーケンスはデータとして比較する
(defun <sequal> (ptn x)
  (cond ((eql ptn *<sequal/wildcard>*)
          T)
        ((and (consp ptn) (eql (car ptn) *<sequal/eval>*))
          (eval `(let ((_ ',x)) ,@(cdr ptn))))
        ((and (consp ptn) (consp x))
          (when (<sequal> (car ptn) (car x))
            (<sequal> (cdr ptn) (cdr x))))
        ((and (simple-vector-p ptn) (simple-vector-p x))
          (let ((n (length ptn)))
            (when (and (eql n (length x)))
              (dotimes (i n T)
                (declare (type simple-vector ptn x))
                (unless (<sequal> (svref ptn i) (svref x i))
                  (return))))))
        ((and (or (and (stringp ptn) (stringp x))
                  (and (bit-vector-p ptn) (bit-vector-p x)))
              (equal ptn x))
          T)
                  
        ((<sequal/isvar?> ptn)
          (aif (assoc ptn *<sequal/var-alist>*)
            (<sequal/compare> (cdr it) x)
            (push (cons ptn x) *<sequal/var-alist>*)))
        ((<sequal/compare> ptn x)
          T)
        (T nil)))

       
(defun sequal (template dst &key (wildcard '_) (test 'equal) (var-prefix #\?) (eval :eval))
  (let ((*<sequal/test>* test)
        (*<sequal/wildcard>* wildcard)
        (*<sequal/var-prefix>* var-prefix)
        (*<sequal/eval>* eval)
        *<sequal/var-alist>*
        )
    (when (<sequal> template dst)
      (or *<sequal/var-alist>* T))))


;;(sequal '(?x) '(a))
;; (sequal '(a (:eval (numberp _) (symbolp _))) '(a 3))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alist plist utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun alist-compare (alist-x alist-y &key (test 'eql))
  (and (eql (length alist-x)
            (length alist-y))
       (dolist (cell alist-x T)
         (aif (assoc (car cell) alist-y)
              (unless (funcall test (cdr cell) (cdr it))
                (return))
              (return)))))

(defun alist-eql (alist-x alist-y) (alist-compare alist-x alist-y))
(defun alist-eq (alist-x alist-y) (alist-compare alist-x alist-y :test 'eq))
(defun alist-equal (alist-x alist-y) (alist-compare alist-x alist-y :test 'equal))
(defun alist-equalp (alist-x alist-y) (alist-compare alist-x alist-y :test 'equalp))

;; PLIST記述で、定数としての(quoteされた)ALISTを生成する
;; (constant-alist a 0 b 1) --> '((a . 0) (b . 1))
(defmacro constant-alist (&rest keys-and-values)
  (unless (evenp (length keys-and-values))
    (error "CONSTANT-ALIST: wrong nunmber of parameters"))
  (list 'QUOTE (unsafe-plist-alist keys-and-values)))

;; コンスセルの挿げ替えによるALIST->PLIST変換。型チェック無しなので高速である
(defun unsafe-alist-plist (alist)
  (declare (type list alist))
  (when alist
    (do ((head alist))
        ((null alist) head)
      (let ((pair (car alist))
            (tail (cdr alist)))
        (declare (type cons pair))
        (setf (car alist) (car pair)
              (car pair) (cdr pair)
              (cdr alist) pair
              (cdr pair) tail
              alist tail)))))


;; コンスセルの挿げ替えによるPLIST->ALIST変換。型チェック無しなので高速である
(defun unsafe-plist-alist (plist)
  (declare (type list plist))
  (when plist
    (do ((head plist))
        ((null plist) head)
      (let ((pair (cdr plist))
            (tail (cddr plist)))
        (declare (type cons pair))
        (setf (cdr pair) (car pair)
              (car pair) (car plist)
              (car plist) pair
              (cdr plist) tail
              plist tail)))))

(defun <alist-ebind-error> (sym alist)
  (error "ALIST-EBIND: could not find symbol ~A: alist=~A" sym alist))

(declaim (inline <alist-update>))
(defun <alist-update> (key val alist)
  (aif (assoc key alist)
       (progn (setf (cdr it) val) alist)
       (push (cons key val) alist)))

;(reduce #'list '(1 2 3) :from-end t :initial-value :init) 
  
(defun <alist-bind> (error? rebuild? update? vars alist-form body)
  (let* ((alist (gensym))
         (need-modify (and (not rebuild?) update?))
         (update-form (when update?
                        (reduce (lambda (key alist) `(<alist-update> ',key ,key ,alist))
                                vars
                                :initial-value alist :from-end t))))
    `(let ((,alist ,alist-form))
       (let ,(mapcar (lambda (v) `(,v ,(if error?
                                         `(aif (assoc ',v ,alist)
                                               (cdr it)
                                               (<alist-ebind-error> ',v ,alist))
                                         `(cdr (assoc ',v ,alist)))))
                     vars)
         (,(if need-modify 'UNWIND-PROTECT 'PROGN)
           (progn
             ,@body
             ,@(when rebuild? `((setq ,alist (copy-alist ,alist))))
             ,@(when (and rebuild? update?)
                 (list update-form))
             ;,@(when rebuild? (list alist))
             )
           ,@(when need-modify (list update-form))
           )))))


(defmacro alist-bind (vars alist-form &body body)
  (<alist-bind> nil nil nil vars alist-form body))
(defmacro alist-ebind (vars alist-form &body body)
  (<alist-bind> T nil nil vars alist-form body))

(defmacro alist-rebuild-bind (vars alist-form &body body)
  (<alist-bind> nil t t vars alist-form body))
(defmacro alist-rebuild-ebind (vars alist-form &body body)
  (<alist-bind> t t t vars alist-form body))

(defmacro alist-update-ebind (vars alist-form &body body)
  (<alist-bind> t nil t vars alist-form body))


#|
(alist-rebuild-bind (a b) '((a . 0) (b . 1)) (setq a 'aaa))
(let ((alist '((a . 0) (b . 1))))
  (alist-update-ebind (a b) alist (setq a 'aaa))
  alist)
(alist-rebuild-bind (a b) nil (setq a 1 b 100))
|#

(defun <plist-fetch> (key plist)
  (when plist
    (if (eql key (car plist))
      (cdr plist)
      (<plist-fetch> key (cddr plist)))))

(defun plist-compare (plist-x plist-y &key (test 'eql))
  (and (eql (length plist-x)
            (length plist-y))
       (do ((pair-chain plist-x (cddr pair-chain)))
           ((null pair-chain) T)
         (aif (<plist-fetch> (first pair-chain) plist-y)
              (unless (funcall test (second pair-chain) (car it))
                (return))
              (return)))))

(defun plist-eql (plist-x plist-y) (plist-compare plist-x plist-y))
(defun plist-eq (plist-x plist-y) (plist-compare plist-x plist-y :test 'eq))
(defun plist-equal (plist-x plist-y) (plist-compare plist-x plist-y :test 'equal))
(defun plist-equalp (plist-x plist-y) (plist-compare plist-x plist-y :test 'equalp))

(defun <plist-ebind-error> (sym plist)
  (error "PLIST-EBIND: could not find symbol ~A: plist=~A" sym plist))

(declaim (inline <plist-update>))
(defun <plist-update> (key val plist)
  (aif (<plist-fetch> key plist)
       (progn (setf (car it) val) plist)
       (push (list key val) plist)))

;(reduce #'list '(1 2 3) :from-end t :initial-value :init) 
  
(defun <plist-bind> (error? rebuild? update? vars plist-form body)
  (let* ((plist (gensym))
         (need-modify (and (not rebuild?) update?))
         (update-form (when update?
                        (reduce (lambda (key plist) `(<plist-update> ',key ,key ,plist))
                                vars
                                :initial-value plist :from-end t))))
    `(let ((,plist ,plist-form))
       (let ,(mapcar (lambda (v) `(,v ,(if error?
                                         `(aif (<plist-fetch> ',v ,plist)
                                               (car it)
                                               (<plist-ebind-error> ',v ,plist))
                                         `(car (<plist-fetch> ',v ,plist)))))
                     vars)
         (,(if need-modify 'UNWIND-PROTECT 'PROGN)
           (progn
             ,@body
             ,@(when rebuild? `((setq ,plist (copy-list ,plist))))
             ,@(when (and rebuild? update?)
                 (list update-form))
             ;,@(when rebuild? (list plist))
             )
           ,@(when need-modify (list update-form))
           )))))


(defmacro plist-bind (vars plist-form &body body)
  (<plist-bind> nil nil nil vars plist-form body))
(defmacro plist-ebind (vars plist-form &body body)
  (<plist-bind> T nil nil vars plist-form body))

(defmacro plist-rebuild-bind (vars plist-form &body body)
  (<plist-bind> nil t t vars plist-form body))
(defmacro plist-rebuild-ebind (vars plist-form &body body)
  (<plist-bind> t t t vars plist-form body))

(defmacro plist-update-ebind (vars plist-form &body body)
  (<plist-bind> t nil t vars plist-form body))


(defun <make-alist/plist-let> (for-alist * bindings body)
  `(,(if * 'LET* 'LET)
     ,bindings
     ,@body
     (list ,@(if for-alist
               (mapcar (lambda (x &aux (v (if (consp x) (car x) x)))
                       `(cons ',v ,v))
                     bindings)
               (mapcan (lambda (x &aux (v (if (consp x) (car x) x)))
                         `(',v ,v))
                       bindings)))))

(defmacro make-alist-let (bindings &body body)
  (<make-alist/plist-let> T nil bindings body))
(defmacro make-alist-let* (bindings &body body)
  (<make-alist/plist-let> T T bindings body))
(defmacro make-plist-let (bindings &body body)
  (<make-alist/plist-let> nil nil bindings body))
(defmacro make-plist-let* (bindings &body body)
  (<make-alist/plist-let> nil T bindings body))

;(make-alist-let ((a 0) b (c 1)) (setq b (vector a c)))
;(make-alist-let* ((a 0) b (c 1)) (setq b (vector a c)))
;(make-plist-let ((a 0) b (c 1)) (setq b (vector a c)))
;(make-plist-let* ((a 0) b (c 1)) (setq b (vector a c)))

;(alist-bind (a b) '((a . 0) (b . 1)) (setq a 'aaa))

;(alist-bind (a b) '((a . 0) (c . 1)) (list a b))


