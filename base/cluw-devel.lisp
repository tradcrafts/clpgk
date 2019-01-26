;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.core:clpgk-core-header)
(in-package :clpgk.base)

(random 16)
;@eval-always
(defun te/pre (x)  (print 'test-ok! )3)
(defmacro tem (x)  `(list ,(te/pre x)))


#Testing
(check-assert (equal '(3) (tem nil)))


;;;;;;;;;;;;;;;;

(defstruct <object-pool> queue size cnt ctor clean)

(defun make-object-pool (constructor &key (size 16) clean)
  (make-<object-pool> :queue (make-array size)
                      :size size
                      :cnt 0
                      :ctor constructor
                      :clean clean))

(defun object-pool-pop (object-pool)
  (let ((cnt (<object-pool>-cnt object-pool)))
    (cond ((eql 0 cnt)
            (funcall (<object-pool>-ctor object-pool)))
          (T (let* ((i (1- cnt))
                    (queue (<object-pool>-queue object-pool))
                    (object-pooled-object (svref queue i)))
               (setf (<object-pool>-cnt object-pool) i
                     (svref queue i) nil)
               object-pooled-object)))))

(defun object-pool-put (object-pool object)
  ;; Clean object if needed
  (awhen (<object-pool>-clean object-pool)  (funcall it object))
  (let ((cnt (<object-pool>-cnt object-pool)))
    (when (< cnt (<object-pool>-size object-pool))
      (setf (svref (<object-pool>-queue object-pool) cnt) object
            (<object-pool>-cnt object-pool) (1+ cnt))
      T)))


;;;;


(define-generic unbuilder/rebuilder (typename (src-var symbol) (elem-vars list) (n-elems integer)))
(define-method unbuilder/rebuilder (_ _ _ _) NIL)

(define-method unbuilder/rebuilder ('vector &..)
  (values
   `(when (and (vectorp ,src-var)
               (eql (length ,src-var) ,n-elems))
      ,@(do ((i 0 (1+ i))
             (vs elem-vars (cdr vs))
             tmp)
            ((null vs) (nreverse tmp))
          (push `(setq ,(car vs) (aref ,src-var ,i)) tmp))
      T)
   `(setq ,src-var (vector ,@elem-vars))))

;(unbuilder/rebuilder 'vector 's '(a b c) 3)


(defun <atomic-element?> (x)
  (or (symbolp x)
      (numberp x)
      (stringp x)))
      
(defun <valid-ptn?> (ptn)
  (cond ((<atomic-element?> x) T)
        ((vectorp ptn)
          (and (not (zerop (length ptn)))
               (symbolp (aref ptn 0))
               (every #'<valid-ptn?> ptn)))
        ((atom ptn) T) ;; TODO シンボル、数値など限定すべき
        ((proper-list-p ptn) (every #'<valid-ptn?> ptn))
        ((circular-list-p ptn) NIL) ;; TODO 対応を考えよ
        (T ;; 末尾がドットリストになっているリスト
          (do ((xs ptn (cdr xs)))
              ((atom xs) (<valid-ptn?> xs))
            (unless (<valid-ptn?> (car xs))
              (return NIL))))))

;(<valid-ptn?> '(a b . x))

(defun <number-of-list-elements> (xs)
  (cond ((proper-list-p xs) (list-length xs))
        ((circular-list-p xs) NIL) ;; TODO
        (T (do ((xs xs (cdr xs))
                (n 1 (1+ n)))
               ((atom xs) n)))))

;(<number-of-list-elements> '(1 2 3 4 5 . 3))

;; (defun <make-code/proper-concing> (vars)
;;   (when vars
;;     (reduce (lambda (a b) (list 'CONS a b))
;;             (butlast vars) :from-end t :initial-value (list 'CONS (lastcar vars) NIL))))

;; (defun <make-code/dotted-concing> (vars)
;;   (reduce (lambda (a b) (list 'CONS a b))
;;           vars :from-end t))

;(<make-code/proper-concing> '(a b c d))
;(<make-code/dotted-concing> '(a b c d))
  
(defun <list-unbuilder/rebuilder> (src-var src)
  (do ((xs src (cdr xs))
       elem-vars  elem-ptns  codes  rebuilder-code unbuilder-code)
      ((atom xs)
       (cond ((null xs)
               (push `(when ,src-var (return)) codes))
             (T
               (push xs elem-ptns)
               (push (gensym) elem-vars)
               ;(push `(when (null ,src-var) (return)) codes)
               (push `(setq ,(first elem-vars) ,src-var) codes)))
       (setq elem-vars (nreverse elem-vars)
             elem-ptns (nreverse elem-ptns)
             unbuilder-code `(block nil ,@(nreverse codes) T)
             rebuilder-code `(setq ,src-var (,(if xs 'LIST* 'LIST) ,@elem-vars)))
       
       (values elem-vars elem-ptns unbuilder-code rebuilder-code)
       )
    (push (car xs) elem-ptns)
    (push (gensym) elem-vars)
    (push `(when (atom ,src-var) (return)) codes)
    (push `(setq ,(first elem-vars) (car ,src-var)) codes)
    (push `(setq ,src-var (cdr ,src-var)) codes)))
    

;(<list-unbuilder/rebuilder> 'x '(a b  e))


(defun <unbuilder/rebuilder> (src-var ptn)
  (cond ((consp ptn)
          (multiple-value-bind (elem-vars elem-ptns unbuilder rebuilder)
              (<list-unbuilder/rebuilder> src-var ptn)
            (values src-var elem-vars elem-ptns unbuilder rebuilder)))
        ((vectorp ptn)
          (bind (((typename . elem-ptns) (coerce ptn 'list))
                 (elem-vars (freplicate (length elem-ptns) #'gensym)))
            (multiple-value-bind (unbuilder rebuilder)
                (unbuilder/rebuilder typename src-var elem-vars (length elem-vars))
              (values src-var elem-vars elem-ptns unbuilder rebuilder))))))

;(<unbuilder/rebuilder> 'x #(vector a b c))

(defvar *<substructure-vars*>)
(defvar *<elem-vars>*)
(defvar *<unbuilders>*)
(defvar *<rebuilders>*)
(defvar *<userside-vars-alist>*)
(defvar *<external-vars>*)
(defvar *<internal-vars>*)

;; 外部変数であればインターンしたシンボルを返す ??foo -> foo
(defun <external-variable?> (x &key (intern T))
  (and (symbolp x)
       (not (keywordp x))
       (let ((s (symbol-name x)))
         (and (< 2 (length s))
              (char= #\? (char s 0))
              (char= #\? (char s 1))
              (if intern
                (intern (subseq s 2))
                T)))))

;; ユーザ変数であればインターンしたシンボルを返す ?foo -> foo
(defun <userside-variable?> (x)
  (aif (<external-variable?> x)
       it
       (and (symbolp x)
            (not (keywordp x))
            (let ((s (symbol-name x)))
              (and (< 1 (length s))
                   (char= #\? (char s 0))
                   (char/= #\? (char s 1))
                   (intern (subseq s 1)))))))


(defun <wildcard?> (x)
  (and (symbolp x) (not (keywordp x)) (string= "?" (symbol-name x))))


(defun <<collect>> (ptn &optional (src-var (gensym)))
  (push src-var *<substructure-vars*>)
  (multiple-value-bind (v elem-vars elem-ptns unbuilder rebuilder)
      (<unbuilder/rebuilder> src-var ptn)
    (declare (ignorable v))
    (setq *<elem-vars>* (append elem-vars *<elem-vars>*))
    (push unbuilder *<unbuilders>*)
    (push rebuilder *<rebuilders>*)
    (dolists (evar elem-vars eptn elem-ptns)
      (aif (<userside-variable?> eptn)
           (let ((info (assoc it *<userside-vars-alist>*)))
             (if (<external-variable?> eptn :intern nil)
               (pushnew it *<external-vars>*)
               (cond ((member it *<external-vars>*)
                       ;; ??xxxと?xxxの混在に対しては警告に留める
                       (warn "variable ~W: both" it))
                     (T (pushnew it *<internal-vars>*))))
             (if info
               (nconc info (list evar))
               (push (list it evar) *<userside-vars-alist>*)))
           (cond ((or (consp eptn) (simple-vector-p eptn))
                   (<<collect>> eptn evar))                 
                 ((not (<wildcard?> eptn))
                   (push `(<equal?> ,evar ',eptn) *<unbuilders>*))))
      )))

(defun <collect> (ptn)
  (let ((toplevel-var (gensym))
        *<substructure-vars*> *<elem-vars>*
        *<internal-vars>* *<external-vars>*
        *<unbuilders>*  *<rebuilders>*  *<userside-vars-alist>*)
    (<<collect>> ptn toplevel-var)
    (values toplevel-var
            *<substructure-vars*>
            *<elem-vars>*
            *<userside-vars-alist>*
            ;;*<internal-vars>*
            *<external-vars>*
            (nreverse *<unbuilders>*)
            *<rebuilders>*)))

;(<collect> '(a b c))
;(<collect> #(vector a '(b x) c))

(defun <make-codes/compare-userside-vars> (vars)
  (when (cdr vars)
    (let ((fst-var (first vars)))
      (mapcar (lambda (var) (list '<equal?> fst-var var))
              (rest vars)))))


(defmacro do-unbuild-if ((x ptn
                            &key
                            (rebuild 'rebuild) (procedural t) (functional nil)
                            (test 'eql) (where T))
                         then &optional else
                         &aux (nosrc (eq T x)))
  (multiple-value-bind (toplevel-var substructure-vars
                                     elem-vars user-vars-alist external-vars
                                     unbuilders rebuilders)
      (<collect> ptn)
    (when nosrc ;; ソースなしの場合
      (setq unbuilders
              `((progn ,@(mapcan (lambda (x) (do-unify-when (x (<equal?> ?var ?val-exp))
                                              `((setq ,var ,val-exp))))
                                unbuilders)
                       T))))
    `(macrolet ((<equal?> (v e) (list ',test v e)))
      (let ((,toplevel-var ,x)
           ,@elem-vars)
       (declare (ignorable ,@substructure-vars))
       (if (and ,@unbuilders)
         ;; THEN
         (progn
           (setq ,toplevel-var nil)
           (let ,(when procedural (mapcan (lambda (uvar-info &aux (uvar (first uvar-info)))
                                            (unless (member uvar external-vars)
                                              (list (subseq uvar-info 0 2))))
                                          user-vars-alist))
             (when (and ,@(unless nosrc (mapcan (lambda (uvar-info &aux (uvar (first uvar-info)))
                                    (<make-codes/compare-userside-vars> (if (member uvar external-vars)
                                                                          uvar-info
                                                                          (cdr uvar-info))))

                                    ;; (when (cddr uvar-info) ;; 内部変数が2個以上の場合のみ
                                    ;;   (mapcar (lambda (v) (list '<equal?> fst-var v))
                                    ;;           (cddr uvar-info))))
                                  user-vars-alist))
                        ,where
                        )
               ,@(unless nosrc
                   ;; 参照切りコード群
                   (mapcar (lambda (v) (list 'SETQ v NIL)) substructure-vars))
               
               (flet ((,rebuild (,@(when (and functional (atom functional)) '(&KEY))
                                 ,@(when functional
                                     (cond ((consp functional) functional)
                                           (procedural
                                             (mapcar (lambda (uvar-info &aux (uvar (first uvar-info)))
                                                       (list uvar uvar))
                                                     user-vars-alist))
                                           (T
                                             (mapcar (lambda (uvar-info) (subseq uvar-info 0 2))
                                                     user-vars-alist))
                                       ))
                                 ,@(when (or procedural (and (consp functional)
                                                             (not (member '&AUX functional))))
                                     '(&AUX))
                                   ,@(when (or procedural functional)
                                       (mapcan (lambda (uvar-info &aux (uvar (first uvar-info)))
                                                 (mapcar (lambda (v) (list v uvar))
                                                         (cdr uvar-info)))
                                               user-vars-alist))
                                   )
                        (let ,substructure-vars
                          ,@rebuilders
                          ,toplevel-var)
                        ))
                 
                 ,then
                 )
               ;(values ,toplevel-var T)
               )))
         ;; ELSE
         ,(unless nosrc else)
         )))))

(defmacro do-unbuild-when ((src ptn &rest options) &body body)
  `(do-unbuild-if (,src ,ptn ,@options) (progn ,@body)))

(defmacro do-unbuild-unless ((src ptn &rest options) &body body)
  `(do-unbuild-if (,src ,ptn ,@options) NIL (progn ,@body)))


(defmacro do-rebuild (src ptn &body body)
  (with-gensyms (rebuilder)
    `(do-unbuild-if (,src ,ptn :functional nil :procedural t :rebuild ,rebuilder)
       (progn
         ,@body
         (,rebuilder))
       (error "er"))))

(defmacro make-rebuilder (src ptn &optional lambdalist)
  (let ((opt/functional (if lambdalist lambdalist T)))
    `(do-unbuild-if (,src ,ptn :functional ,opt/functional :procedural t :rebuild |<rebuilder>|)
       #'|<rebuilder>|
       (error "err2"))))


(enable-testing t)
#Test ttest trying...
(check-unit (+ _ _) (1 2 3))

#Verify this is a vtest
(progn (print 'veryfy) t)

#Verify
(equal (nreverse (iota 20)) (iota 20 :start 19 :step -1))

@Verify
(equal (nreverse (iota 10)) (iota 10 :start 9 :step -1))

#Verify
#!(equal 1 2)

;(print (list #|`Hello|# #|comment|# #||# #|`string'world|#))


;; @todo (print #{list
;;              #; 
;;                                         ; Hello
;;                                         ; World
;;                                         ; ok
;;        )

(print #@数値である 'okok)




#;;;;;   Doya;n !-1!

#Comment


#;#Waran warn!-2!!!!


(print 'done)

;;;;;;;


(testing
  (check-unit (/ 0 0) ()))
    
(let ((a 3)) (do-unbuild-if ('(3 2 3) (??a 2 ??a)) a))

(let ((a 3)) (do-unbuild-when (T (??a 2 ?b . ??a) :where (oddp a)) (setq b 'b) (rebuild)))

(let ((a 3)) (do-unbuild-when (T (?a 2 ?a)) (setq a 'a) (rebuild)))

(let ((a 1)) (do-unbuild-when ('(1 2 1) (??a 2 ??a . ?r)) (list 'ok r)))

(defmacro do-transform-if (src ptn/src ptn/dst)
  `(do-unbuild-if (,src ,ptn/src :rebuild |<rebuilder/1>|)
     

                        
(funcall (make-rebuilder t (?x foo ?y) (&optional (x y) (y x)))  )

(do-rebuild T (a ?b c ?b ?c) (setq b 3 c 0))

(clpgk.base.form::lambda/let ((:unify ?x)) x)
    
(do-unbuild-if ('(a #(x y x) r z) (? #(vector ?foo ?bar ?foo) . ?baz) :functional t :procedural nil)
  (progn ;(setq foo 'man bar 'chin baz (cdr baz))
         (list (rebuild)
               ;(progn (setq foo 1000) (rebuild))
               (rebuild :baz 0)
               (rebuild :foo 1 :bar 2))))
(do-unbuild-if ('(a #(x y x) r z) (? #(vector ?foo ?bar ?foo) . ?baz)
                 :functional (foo bar &aux (baz 'boz)) :procedural nil)
  (rebuild 1 2))

(do-unbuild-if ('(a #(x y x) r z) (a #(vector ?foo ?bar ?foo) . ?baz)
                 :functional (foo) :procedural t)
  (mapcar #'rebuild (iota 10)))

(do-unbuild-if (T (a #(vector ?foo bar ?foo) . ?baz)
                  :functional (foo &optional (baz (list (1+ foo) (1- foo)))) :procedural t)
  (progn 
         (mapcar #'rebuild (iota 10))))

(do-unify-when ('(3 4) (??x ??x)) x)

(sb (a #(vector ?foo ?bar) . ?baz) '(a #(x y) r) (setq foo 'man bar 'chin baz nil))
(sb (a (b c d . e) c) '(a (b c d . e) c))


(defn fact ((n integer) (r integer))
   ((= 1) &.. -> r)
   (n r -> (fact (1- n) (* n r))))
 (fact 6 1)


(<make-params-for-defgeneric> '(a b &rest x))

(ensure-function 'cons)

(setq p (make-object-pool (lambda () (list '_)) :clean nil) )
((object-pool-put p 11)
(object-pool-pop p)

(define fact (number number)
  ((= 0) n -> n)
  (_ n -> (fact (1- n) (1+ n))))

     

(defmacro deff (
      


(defun object-object-pool-get (


(define-method test ((a t :where wt :where wt2 :when tes :let b :let* c :when lw :unless d)) 'ok)

(define-method test ((:unify (?a ?b . ?r) :when (< a b)) ) (list a b r))
(define-method test ((:match (list a b a) :when a) ) (list 'match a b))
(test '(0 2 0 ))

(defmethod testa (a &optional (x 0)) (list a x))
(testa 1 'x)
number


(define-method testm (63 (a ta :when a :let (letb) :let* (letb*)) b 'foo (= 100)(c tc :unless c)
                         &optional _ (z _ :when) (foo ?) &key k1)
  where (foo a)
  the-body)

(1 of 20)
(expt 32 2)


(defun testes (a &optional o w &rest r &key b c)
  (list a o w b c r))
(testes 1 :b 5 :c 2)

(<unlift-params> (<lift-params> '(a b &optional o1 o2 &key k1 &rest r1 &aux a1 a2 a3)))

    



(<trans> '(a b :when when-t :when when-2 :unless un-3 :bind (binding) :let-when (lbind)) 'the-body)
(<apply-parameter-macro> '(= b))

(<primitive-parameter?> '(foo foo :ignorable t :when t))



(eval-when (:compile-toplevel :load-toplevel)
(defvar +<per-priority-unit>+ 32)
(defvar +<priority-classes>+ #(p/1 p/2 p/3 p/4 p/5 p/6 p/7 p/8 p/9 p/10
                                    p/11 p/12 p/13 p/14 p/15 p/16 p/17 p/18 p/19 p/20
                                    p/21 p/22 p/23 p/24 p/25 p/26 p/27 p/28 p/29 p/30
                                    p/31 p/32))


)




                                    
(defmethod m1 (a (eq 3)) a)

*<param-macros>*

(<apply-parameter-macro> '(= x))
(<apply-parameter-macro> ''x)

(defun tesfun (a b c)
  (declare (ignore a))
  (declare (ignore c))
  b)
(tesfun 1 2 3)

(define-medthod-parameter (the-variable 

(defmethod speedtest1 ((_ p/1) x) 1)
(defmethod speedtest1 ((_ p/32) x) 1)
(time (let ((p (make-instance 'p/15)))
        (dotimes (i 100000000) (speedtest1 p i))))


(defun <priority-number-to-class-combination> (n i)
  (let ((number-of-unit (ceiling (log n +<per-priority-unit>+))))
    (do ((cnt number-of-unit (1- cnt))
         (src i (floor (/ src +<per-priority-unit>+)))
         result)
        ((zerop cnt) result)
      (let ((idx (mod src +<per-priority-unit>+)))
        (push (list (gensym)
                    (svref +<priority-classes>+ idx))
              result)))))


(defun fmakunbound/p (ident)
  (let ((info (get ident '|generic/p|)))
    (unless info
      (error "FMAKUNBOUND/P: ~W was not defined by DEFGENERIC/P" ident))
    (let ((proxy-method (second info)))
      (fmakunbound proxy-method)
      (fmakunbound ident)
      (values ident proxy-method))))
  
(defmacro defgeneric/p (&whole definition
                        ident (max-priority &rest lambdalist))
  (unless (and (positive-integer-p max-priority)
               (symbolp ident))
    (error "DEFGENERIC/P: syntax error ~W" definition))
  (let* ((number-of-priority-classes (ceiling (log max-priority +<per-priority-unit>+)))
         (info (get ident '|generic/p|))
         (proxy-method (if info (second info) (gensym)))
         (tops (replicate number-of-priority-classes '+<p/1>+)))
    
    `(progn
       (setf (get ',ident '|generic/p|) (list ,max-priority ',proxy-method))
       (defgeneric ,proxy-method (,@(freplicate number-of-priority-classes #'gensym)
                                  ,@lambdalist))
       (defun ,ident (&rest args)
         (apply (function ,proxy-method) ,@tops args))
       (define-compiler-macro ,ident (&rest args)
         `(,',proxy-method ,@',tops ,@args))

            )))

(defmacro defmethod/p (&whole definition
                       ident (priority  &rest lambdalist) &body body)
  (let ((info (get ident '|generic/p|)))
    (unless info
      (error "DEFMETHOD/P: ~W was not defined by DEFGENERIC/P" definition))
    (bind (((max-priority proxy-method) info))
      (unless (and (non-negative-integer-p priority)
                  (< priority max-priority))
        (error "DEFMETHOD/P: syntax error ~W" definition))

      (let ((class-combination (<priority-number-to-class-combination> max-priority priority)))
        `(progn
           (defmethod ,proxy-method (,@class-combination ,@lambdalist)
             (declare (ignore ,@(mapcar #'first class-combination)))
             ,@body)
           ',ident
           )))))

(eval-when (:compile-toplevel :load-toplevel)
  (push 'both (get '* 'test-1)))
(eval-when (:load-toplevel)
  (push 'load (get '* 'test-2)))
(eval-when (:compile-toplevel)
  (push 'compile (get '* 'test-3)))

(defvar testvar (push 'eval (get '* 'test-4)))



;;;;;;;
#Comment

(defgeneric gtes (a b &key x y z))
(defmethod gtes ((a integer) b &key (x 0) (y 1) (z 100))
  (list a b x y z))

(define-method fact (64 64 x y)
    




(* 32 32 32 32)

x1 + x2*base + x3*base^2
100 10

;;(defconstant +<priority-tmpvars>+ (coerce (freplicate +<per-priority-unit>+ #'gensym) 'vector))


(<priority-number-to-class-combination> 10000 308)

(let ((x 0)) (dotimes (i 1000000) (incf x (log (+ i 10) 32))) x)
(floor (/ 10000 32))
(mod 1023 32)

(defgeneric foobar (a b))

(defmethod foobar (a b) (list a b))

(fmakunbound 'foobar)
 

(fmakunbound/p 'gbar)
(defgeneric/p gbar (1000 a b c))
(defmethod/p gbar (10 a b c) (if (numberp a) (list 'num a b c) (call-next-method)))
(defmethod/p gbar (30 a b c) (list 'fin a b c))

(defmacro testratio (x)
  `(list ,(numerator x) ,(denominator x)))

(testratio 339/33)
(numerator 30/30)
(denominator 30/30)
(pdef fact 
      
          
(defmethod/p      

(gbar 'a 2 3)
(gbar 1 2 3)

(p/defmethod fact (0 x y)

(type-of (1 2)
             
(defmethod testm ((_ p/15) (x integer))
  (if (< x 100) (list 'first x) (call-next-method)))
(defmethod testm ((_ p/16) (x integer))
  (if (< x 1000) (list 'second x) (call-next-method)))
(defmethod testm ((_ prio) x)
  (list 'third x))

(testm (make-instance 'p/14) 300)




(let-if :T (:COMPLEX 1 2 3) 'ok 'ng)
(bind:lambda-bind ((a b) (c d)) 'e)
(defun/bind bar ((a b c) d) (list 'ok a b c d))
(bar '(1 2 3) 30)
(disassemble 'bar)
(lambda/bind (a b) (list a b))
(funcall (lambda/bind ((x y) a &rest r) (list x y a r)) '(1 2) 3 4 5)

(let-lambda (a _ &rest r))
(let-defun foo (x (:unify (?a ?a ?b)) &rest r) (list a b x r))
(foo 1 '(2 2 30) 3)
(let-if dst-exp src-exp then-exp)
(let-if (:not dst-exp) src-exp then-exp)
(let-if (:not _) src-exp then-exp)
(let-if (:and a b) (:complex x y) then-exp)
(let-if (:and _ _) (:complex x y) then-exp)

(:if x)
(let-lambda (x (:unify y z)) (list x y))
(let ((x '(1 2 3 4 2 5 1)))
  (let-while (:unify (:and (?a ?b . ?) (:here (< a b))))
      x
    (print x)
    (setf x (cdr x))))
    
(cond (t 3))
(let ((foo 3)))
(let-if nil (:complex a b) 'ok 'ng)
(let-cond ((:unify ?x (?y)) (:complex 1 '(2)) (list x y))
          (T nil 'ng))
(let-case (:complex t 4) ((:and x y) (list 'a x y)) ((:or x y) (list 'b x y)) (t err))

(let-if (:and a) 100 (list 'and a) 'ng)
(let-if (:none a b) (:complex nil nil) (list 'and a b))
(let-if (:unify (?a ?b)) '(1 2) (list 'unify b a) 'ng)
(let-if (:match (list a b)) '(1 2) (list 'match b a) 'ng)
(let-if (:unify (?a ?b) (?c ?a)) (:complex '(1 2) '(3 1)) (list a b c) 'ng)
(labels ((aa (x) 1)
       (bb (x) (+ 100 (aa x))))
  (bb 30))
        
        



