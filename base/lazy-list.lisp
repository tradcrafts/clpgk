;; -*- coding: utf-8 -*-

(oleo.core:oleo-core-header)

(oleo.core:define-package :oleo.base.lazy-list* (:oleo.base.lazy-list)
  (:use :cl :cl-cont)
  (:import/export :oleo.base.cont*)
  (:export 
   #:&cons #:&cons! #:&list #:&list*
   #:&length
   #:&car #:&cdr #:&nth #:&nthcdr #:&last
   
   #:&caar #:&cadr #:&cdar #:&cddr 
   #:&caaar #:&caadr #:&cadar #:&caddr #:&cdaar #:&cdadr #:&cddar #:&cdddr 
   #:&caaaar #:&caaadr #:&caadar #:&caaddr #:&cadaar #:&cadadr #:&caddar #:&cadddr 
   #:&cdaaar #:&cdaadr #:&cdadar #:&cdaddr #:&cddaar #:&cddadr #:&cdddar #:&cddddr 
   #:&first #:&second #:&third #:&fourth #:&fifth 
   #:&sixth #:&seventh #:&eighth #:&ninth #:&tenth

   #:&collect
   #:&dolist #:&dolists
   #:&repeat #:&replicate #:&frepeat #:&freplicate #:&iota
   #:&member #:&member-if #:&member-if-not
   #:&substitute #:&substitute-if #:&substitute-if-not

   #:&take #:&take!
   #:&drop

   #:&lazy #:&copy-list
   #:&strict #:&nstrict
   #:&butlast
   #:&intersperse #:&intercalate
   #:&take-while #:&drop-while
   #:&tails
   #:&nub #:&group
   #:&reduce
   #:&minimum #:&maximum
   #:&mapcar #:&maplist #:&mapc #:&mapl #:&mapcan #:&mapcon
   #:&nmapcar
   #:&append #:&concat
   #:&cycle
   #:&filter #:&filter-not
   #:&flatten

   #:&collect #:&with-collect

   #:&assoc #:&assoc-if #:&assoc-if-not
   #:&alist-plist #:&plist-alist
   #:&pairlis

   #:&recurs #:&recurs*
   ))

(in-package :oleo.base.lazy-list)

(defmacro unbound (place) 
  (let ((v (gensym)))
    `(let ((,v ,place))
      (setf ,place nil)
       ,v)))

(defmacro &cons (head tail)
  `(cons (delay ,head) (delay ,tail)))

(defmacro &cons! (head tail)
  `(cons ,head (delay ,tail)))

(defmacro &list (&rest forms)
  `(list ,@(mapcar #/(list 'delay _)
                   forms)))
(defmacro &list* (&rest forms)
  `(list* ,@(mapcar #/(list 'delay _)
                   forms)))

@inline
(defun &car (xs) (force (car xs)))

@inline
(defun &cdr (xs)
  (force (car xs))
  (if (promise-p (cdr xs))
    (setf (cdr xs) (force (cdr xs)))
    (cdr xs)))

(defun &nthcdr (i xs)

  (declare (optimize (speed 3) (debug 0) (safety 0)))

  (do ((xs (unbound xs) (&cdr xs)))
      ((or (zerop i) (null xs))
       xs)
    (decf i)))

(defun &nth (i xs)
  (&car (&nthcdr i (unbound xs))))

(defun <&last> (xs)
  (do ((xs (unbound xs) (cdr xs)))
      ((atom (&cdr xs))
       xs)))

(defun &last (xs &optional (n 1))
"
;;  LASTの遅延版。LASTと完全互換
"
  (assert (and '&last (integerp n) (<= 0 n)))
  (case n
    (1 (<&last> xs))
    (0 (cdr (<&last> xs)))
    (t (let ((head xs))
         (setq xs (do ((i (1- n) (1- i))
                       (xs xs (cdr xs)))
                      ((or (zerop i) (atom (&cdr xs)))
                       xs)))
         (if (atom (&cdr xs))
           head
           (do ((xs (unbound xs) (cdr xs)))
               ((atom (&cdr xs))
                head)
             (setq head (cdr head))))))))


(eval-when (:compile-toplevel)
  (defun genCmbExp (exp ls)
    (let ((ops (mapcar (lambda (l) (if (eq 'a l) '&car '&cdr))
                       (reverse ls))))
      (foldr-expression (cons exp ops))))
  (defun genCmbNam (ls)
    (let ((cs (mapcar (lambda (l) (if (eq 'a l) #\A #\D))
                      ls)))
      (intern (concatenate 'string "&C" cs "R"))))
  (defmacro defCmb (ls) 
    `(defun ,(genCmbNam ls) (x) ,(genCmbExp 'x ls)))
  (defmacro defAcc (nam ls)
    `(defun ,nam (x) ,(genCmbExp 'x ls)))
  )




(defCmb (a a))     (defCmb (a d))     (defCmb (d a))     (defCmb (d d))
(defCmb (a a a))   (defCmb (a a d))   (defCmb (a d a))   (defCmb (a d d))
(defCmb (d a a))   (defCmb (d a d))   (defCmb (d d a))   (defCmb (d d d))
(defCmb (a a a a)) (defCmb (a a a d)) (defCmb (a a d a)) (defCmb (a a d d))
(defCmb (a d a a)) (defCmb (a d a d)) (defCmb (a d d a)) (defCmb (a d d d))
(defCmb (d a a a)) (defCmb (d a a d)) (defCmb (d a d a)) (defCmb (d a d d))
(defCmb (d d a a)) (defCmb (d d a d)) (defCmb (d d d a)) (defCmb (d d d d))


(defAcc &first  (a)) (defAcc &second (a d)) (defAcc &third (a d d))
(defAcc &fourth (a d d d))             (defAcc &fifth   (a d d d d))
(defAcc &sixth  (a d d d d d))         (defAcc &seventh (a d d d d d d))
(defAcc &eighth (a d d d d d d d))     (defAcc &ninth   (a d d d d d d d d))
(defAcc &tenth  (a d d d d d d d d d))

(defmacro &dolist ((v xs &optional result) &body body)
  (let ((g (gensym)))
    `(let (,v)
       (do ((,g ,xs (&cdr ,g)))
    ((null ,g) ,result)
    (setq ,v (&car ,g))
    ,@body))))

@doc"
;; DOLISTSの遅延リスト対応版
;; (&dolists (var1 list1 var2 list2 ... [result]) ...)
"
(defmacro &dolists ((&rest pairs) &body body)
  (unless (<= 2 (length pairs))
    (error "&DOLISTS: illegal variable clause ~D" pairs))
  #{let ((ret))
  (when (oddp (length pairs))
    (setq ret (lastcar pairs))
    (nbutlast pairs))
  #{let* ((tmp (plist-alist pairs))
          (vars (mapcar 'car tmp))
          (lists (mapcar 'cdr tmp))
          (internals (freplicate (/ (length pairs) 2) #'gensym)))
  `(do ,(mapcar (lambda (internal list) `(,internal ,list (&cdr ,internal)))
                internals lists)
       ((not (and ,@internals))
        ,@(when ret `((let ,vars (declare (ignorable ,@vars)) ,ret))))
    (let ,(mapcar (lambda (var internal) `(,var (&car ,internal)))
                  vars internals)
      ,@body)))

(defun <&iota-infinite> (cur step)
  (&cons! cur (<&iota-infinite> (+ cur step) step)))

(defun <&iota-finite> (n cur step)
  (unless (zerop n)
    (&cons! cur (<&iota-finite> (1- n) (+ cur step) step))))

;; function &iota
(defun &iota (n &key (start 0) (step 1))
  (assert (and '&iota 
               (integerp n)
               (numberp start)
               (numberp step)))
  (if (<= 0 n)
    (<&iota-finite> n start step )
    (<&iota-infinite> start step)))

(defun <&tails/haskell> (xs)
  (if xs
    (&cons! xs (<&tails/haskell> (&cdr xs)))
    (list nil)))
(defun <&tails/lisp> (xs)
  (when xs
    (&cons! xs (<&tails/lisp> (&cdr xs)))))

;; &TAILS tail部の遅延リストを作る. :haskellが真の場合haskell流となる
(defun &tails (xs &key haskell)
  (if haskell
    (<&tails/haskell> xs)
    (<&tails/lisp> xs)))

(defun &strict (xs)
  (let ((ret nil))
    (&dolist (x (unbound xs) (nreverse ret))
      (push x ret))))
;; 破壊版
(defun &nstrict (xs)
  (do ((cur xs (&cdr cur)))
      ((null cur)
       xs)
    (setf (car cur) (&car cur))))
      
(defun &butlast (xs)
  (let ((Cdr (&cdr xs)))
    (when Cdr
      (&cons! (&car xs) (&butlast Cdr)))))

(defun &concat (xss)
  (when xss
    (let ((xs (&car xss)))
      (if xs 
   (&cons! (car xs) 
    (&concat (&cons! (cdr xs) (&cdr xss))))
 (&concat (&cdr xss))))))

(defun <&intersperse> (sep xs i)
  (when xs
    (cond ((oddp (incf i))
            (&cons! (car xs) (<&intersperse> sep (&cdr xs) i)))
          (t (&cons! sep (<&intersperse> sep xs i))))))

;; リストxsの要素間にsepを挿入し、新たなリストを作る
(defun &intersperse (sep xs)
  (<&intersperse> sep xs 0))

;; リストのリストxssの各要素間にリストsepsを挿入し、連結して新たなリストを作る
(defun &intercalate (seps xss)
  (&concat (&intersperse seps xss)))

;; 遅延リスト化
(defun &lazy (xs)
  (when xs
    (&cons! (car xs) (&lazy (&cdr xs)))))


;; 遅延リストのコピー。非遅延リスト、循環リストを含む場合も適切に処理される
(defun &copy-list (xs)
  (cond ((proper-list-p xs) 
          (copy-list xs))
        ((circular-list-p xs)
          (&lazy xs))
        (t (when xs
             (do ((copied)
                  (last)
                  (cur xs (cdr cur)))
                 ((atom (cdr cur))
                  (let ((tail (&cons! (car cur) (&copy-list (&cdr cur)))))
                    (cond (last
                            (setf (cdr last) tail)
                            copied)
                          (t tail))))
               (if last
                 (setq last 
                         (setf (cdr last) 
                                 (cons (car cur) nil)))
                 (setq copied 
                         (setq last 
                                 (cons (car cur) nil)))))))))

(defun <&cycle> (cur xs)
  (if cur
    (&cons! (car cur) (<&cycle> (&cdr cur) xs))
    (<&cycle> xs xs)))

(defun &cycle (xs)
"
;; 循環無限リストの生成
;; xsは遅延リストでもよいし、循環リストでもよい
;; xsに遅延リストを渡す場合には注意が必要。
;; この関数は先頭を保持するため、無限遅延リストを渡せばメモリリークの原因になる
"
  (when xs
    (cond ((proper-list-p xs)
            (&lazy (cycle xs)))
          ((circular-list-p xs)
            (&lazy xs))
          (t (<&cycle> xs xs)))))

;; 挙動はalexandria:flattenに準ずる
;; 内部リストは遅延リストであることを前提とする
(defun &flatten (src)
  (unless (listp src)
    (return-from &flatten (list src)))
  #{let ((stack (list src)))
  #{labels ((f ()
               #{when stack
               #{let* ((xs (force (car stack)))
                       (x (cond ((consp xs) 
                                  (if (cdr xs)
                                    (setf (car stack) (cdr xs))
                                    (pop stack))
                                  (&car xs))
                                (t 
                                  (pop stack)
                                  xs))))
               (cond ((null x) (f))
                     ((atom x) (&cons! x (f)))
                     (t
                       (push x stack)
                       (f)))))
  (f))

;; function f must take a lazy arguments
(defun &foldr (f z xs)
  (if xs
      (funcall f (car xs) (delay (&foldr f z (&cdr xs))))
    z))

;; implementations by foldr@




(defun new (i)
  (&cons (progn (print (list 'PROVIDED i)) i) (new (1+ i))))

(defun new~ (i)
  (&cons (progn  i) (new~ (1+ i))))

(defun &take (n xs)
  (when (and xs (< 0 n))
    (&cons! (car xs) 
            (&take (1- n) (&cdr xs)))))

(defun &take! (n xs)
  (when (and xs (< 0 n))
    (do ((i n (1- i))
         (xs (unbound xs) (&cdr xs))
         tmp)
        ((or (NULL xs) (< i 0)) (nreverse tmp))
      (push (&car xs) tmp))))

;; (&take! 5 (&nthcdr 1000 (&iota 10000)))

@inline
(defun &drop (n xs) (&nthcdr n xs))


(defun &from (xs)
  (let (c)
    (&dolist (x (unbound xs)) 
       (setq c (cons x c)))
    (nreverse c)))
    

(defun &length (xs)
  (do ((cnt 0 (1+ cnt))
       (xs (unbound xs) (&cdr xs)))
      ((null xs) cnt)))

(defun <&repeat-1> (x)
  (&cons! x (<&repeat-1> x)))
(defun <&repeat-circle> (xs)
  (&cons! (car xs) (<&repeat-circle> (cdr xs))))
;; 要素xの無限リスト
(defun &repeat (x &rest more-values)
  (if more-values
    (<&repeat-circle> (circulate (cons x more-values)))
    (<&repeat-1> x)))

(defun <&replicate-1> (n x)
  (unless (zerop n)
    (&cons! x (<&replicate-1> (1- n) x))))
(defun <&replicate-circle> (n xs)
  (unless (zerop n)
    (&cons! (car xs) (<&replicate-circle> (1- n) (cdr xs)))))

;; 要素xをn個持つリストの生成。n<0なら無限リスト(&repeatに等価)
(defun &replicate (n x &rest more-values)
  (if (<= 0 n)
    (if more-values
      (<&replicate-circle> n (circulate (cons x more-values)))
      (<&replicate-1> n x))
    (apply '&repeat x more-values)))

(defun <&freplicate-1> (n f)
  (unless (zerop n)
    (&cons (funcall f) (<&freplicate-1> (1- n) f))))
(defun <&freplicate> (n fs)
  (unless (zerop n)
    (&cons! (funcall (car fs)) (<&freplicate> (1- n) (cdr fs)))))
(defun &freplicate (n f &rest more-functions)
  (assert (and '&freplicate (integerp n) (<= 0 n)))
  (if more-functions
    (<&freplicate> n (circulate (cons f more-functions)))
    (<&freplicate-1> n f)))

(defun <&frepeat-1> (f)
  (&cons (funcall f) (<&frepeat-1> f)))
(defun <&frepeat> (fs)
  (&cons! (funcall (car fs)) (<&frepeat> (cdr fs))))

(defun &frepeat (f &rest more-functions)
  (if more-functions
    (<&frepeat> (circulate (cons f more-functions)))
    (<&frepeat-1> f)))


(defun valid-all? (xss) (and xss (every #'consp xss)))

(defun <&mapcar> (f xss tmp)
  (when (valid-all? xss)
    (&cons (prog1 
             (apply f (map-into tmp #'&car xss))
             (fill tmp nil))
    (funcall '<&mapcar> f (map-into xss #'&cdr xss) tmp))))
(defun &mapcar (f &rest xss)
  (<&mapcar> f xss (make-list (length xss))))

(defun <&maplist> (f xss)
  (when (valid-all? xss)
    (&cons (apply f xss)
    (funcall '<&maplist> f (map-into xss #'&cdr xss)))))
(defun &maplist (f &rest xss)
  (<&maplist> f xss))

(defun &mapc (f &rest xss)
  (assert (and xss (every 'listp xss)))
  (let ((cars (make-list (length xss))))
    (while (every 'identity xss)
      (apply f (map-into cars '&car xss))
      (map-into xss '&cdr xss)))
  xss)

(defun &mapl (f &rest xss)
  (assert (and xss (every 'listp xss)))
  (while (every 'identity xss)
    (apply f xss)
    (map-into xss '&cdr xss))
  xss)

(defun &mapcan (f &rest xss)
  (&concat (apply '&mapcar f xss)))
(defun &mapcon (f &rest xss)
  (&concat (apply '&maplist f xss)))

(defun <&nmapcar/1> (f xs)
  (when xs
    (setf (car xs) (let ((thunk-car (car xs))) 
                     (delay (funcall f (force thunk-car))))
          (cdr xs) (let ((thunk-cdr (cdr xs)))
                     (delay (<&nmapcar/1> f (force thunk-cdr)))))
    xs))

(defun <&nmapcar> (f xs zs tmp)
  (when (and xs (valid-all? zs))
    (setf (car xs) (let ((thunk-car (car xs))) 
                     (delay (prog1 
                              (apply f 
                                     (force thunk-car) 
                                     (map-into tmp '&car zs))
                              (fill tmp nil))))
                            
          (cdr xs) (let ((thunk-cdr (cdr xs)))
                     (delay (<&nmapcar> f 
                                        (force thunk-cdr)
                                        (map-into zs '&cdr zs)
                                        tmp))))
    xs))

;; xsにfを適用し、xsを遅延再構成する
;; othersにxs（およびその派生物）が含まれている場合の挙動は不定である
(defun &nmapcar (f xs &rest others)
  (if others
    (<&nmapcar> f xs others (make-list (length others)))
    (<&nmapcar/1> f xs)))

(defun &member (a xs &key test test-not key)
  (assert (and '&member
               (not (and test test-not))))
  (cond ((and (or test test-not) (not key))
          (setq key #'identity))
        ((and key (not (or test test-not)))
          (setq test #'eql)))
  (cond (test (do ((xs (unbound xs) (&cdr xs)))
                  ((or (null xs)
                       (funcall test a (funcall key (&car xs))))
                   xs)))
        (test-not (do ((xs (unbound xs) (&cdr xs)))
                  ((or (null xs)
                       (not (funcall test-not a (funcall key (&car xs)))))
                   xs)))
        (t (do ((xs (unbound xs) (&cdr xs)))
               ((or (null xs) (eql a (&car xs)))
                xs)))))
          

(defun &member-if (f xs &key key)
  (if key
    (do ((xs (unbound xs) (&cdr xs)))
        ((or (null xs)
             (funcall f (funcall key (&car xs))))
         xs))
    (do ((xs (unbound xs) (&cdr xs)))
        ((or (null xs)
             (funcall f (&car xs)))
         xs))))

(defun &member-if-not (f xs &key key)
  (if key
    (do ((xs (unbound xs) (&cdr xs)))
        ((or (null xs)
             (not (funcall f (funcall key (&car xs)))))
         xs))
    (do ((xs (unbound xs) (&cdr xs)))
        ((or (null xs)
             (not (funcall f (&car xs))))
         xs))))

(defun &substitute-if (new f xs &key key count)
  (let* ((f (cond ((and key count) 
                    #/(if (and (< 0 count)
                               (funcall f (funcall key _))
                               (<= 0 (decf count)))   
                        new _))
                  (key 
                    #/(if (funcall f (funcall key _)) new _))
                  (count
                    #/(if (and (< 0 count)
                               (funcall f _)
                               (<= 0 (decf count)))
                        new _))
                  (t #/(if (funcall f _) new _)))))
    (&mapcar f xs)))

(defun &substitute-if-not (new f xs &key key count)
  (&substitute-if new #/(not (funcall f _)) xs :key key :count count))

(defun &substitute (new old xs &key test test-not key count)
  (let ((f (cond (test #/(funcall test old _))
                 (test-not #/(not (funcall test-not old _)))
                 (t #/(eql old _)))))
    (&substitute-if new f xs :key key :count count)))

(defun <&nub> (xs acc comparer)
  (awhen (&member-if-not comparer xs)
    (push (car it) (cdr acc))
    (&cons! (car it) 
            (<&nub> (&cdr it) acc comparer))))

(defun &nub (xs &key test key)
  (when xs
    (let* ((x (&car xs))
           (acc (list nil x))
           (comparer (cond ((and test key) 
                             #/(member (funcall key _) (cdr acc) :test test :key key))
                           (test #/(member _ (cdr acc) :test test))
                           (key  #/(member (funcall key _) (cdr acc) :key key))
                           (t    #/(member _ (cdr acc))))))
      (&cons! x (<&nub> (&cdr xs) acc comparer)))))

(define-symbol-macro COMPOSED-TEST-KEY-FUNCTION
  (when (or test key)
     (cond ((and test key) 
             (lambda (a b) (funcall test (funcall key a) (funcall key b))))
           (test
             (lambda (a b) (funcall test a b)))
           (key
             (lambda (a b) (eql (funcall key a) (funcall key b)))))))


(defun <&group> (xs acc f)
  (if xs
    (let ((x (&car xs)))
      (if (funcall f (car acc) x)
        (<&group> (&cdr xs) (cons x acc) f)
        (&cons! (nreverse acc) (<&group> (&cdr xs) (list x) f))))
    (list (nreverse acc))))
        
(defun &group (xs &key test key)
  (when xs
    (let ((f (aif COMPOSED-TEST-KEY-FUNCTION it 'eq)))
      (if f
        (<&group> (&cdr xs) (list (car xs)) f)))))

(defun <&take-while> (f xs)
  (when (and xs (funcall f (&car xs)))
    (&cons! (car xs) (<&take-while> f (&cdr xs)))))
(defun &take-while (f xs &key key)
  (if key
    (<&take-while> #/(funcall f (funcall key _)) xs)
    (<&take-while> f xs)))

(defun &drop-while (f xs &key key)
  (&member-if-not f xs :key key))

(defun &unzip2 (xs)
  (list (&mapcar #'&first xs)
 (&mapcar #'&second xs)))
     
(defun &unzip3 (xs)
  (list (&mapcar #'&first xs)
 (&mapcar #'&second xs)
 (&mapcar #'&third xs)))


(defun <&zip> (xss)
  (when (valid-all? xss)
    (&cons! (mapcar #'car xss)
            (<&zip> (mapcar #'&cdr xss)))))

(defun &zip (&rest lists)
  (<&zip> lists))

(defun <&append> (xss)
  (when xss
    (let ((xs (car xss)))
      (if xs 
        (&cons! (car xs) 
                (<&append> (cons (&cdr xs) (cdr xss))))
        (<&append> (cdr xss))))))

(defun &append (&rest lists)
  (<&append> lists))


(defun &iterate (f x)
  (&cons! x (&iterate f (funcall f x))))

(defun &every (f xs)
  (&dolist (x (unbound xs) t)
    (unless (funcall f x) (return nil))))

(defun &some (f xs)
  (&dolist (x (unbound xs))
    (when (funcall f x) (return t))))

(defun &and (xs) (&every #'identity (unbound xs)))
(defun &or (xs) (&some #'identity (unbound xs)))

(defun &fold (f r xs)
  (&dolist (x (unbound xs) r) 
    (setq r (funcall f r x))))

(defun &fold-with-key (k f r xs)
  (&dolist (x (unbound xs) r) 
    (setq r (funcall f r (funcall k x)))))

(defun &reduce (f xs &key key (initial-value '|unbound|))
  (cond ((null xs) (error "&reduce: cons required"))
        (t
          (let* ((init (eq initial-value '|unbound|))
                 (r (if init (&car xs) initial-value))
                 (xs (if init (&cdr (unbound xs)) (unbound xs))))                      
            (if key
              (&fold-with-key key f 
                              (if init (funcall key r) r) 
                              xs)
              (&fold f r xs))))))

(defun &maximum (xs &key key)
  (&reduce (if key 
             (lambda (a b) (if (> (funcall key a) (funcall key b)) a b))
             (lambda (a b) (if (> a b) a b)))
           xs))
(defun &minimum (xs &key key)
  (&reduce (if key 
             (lambda (a b) (if (< (funcall key a) (funcall key b)) a b))
             (lambda (a b) (if (< a b) a b)))
           xs))

(defun <&filter> (f xs)
  (when xs
    (&cons! (car xs) 
            (<&filter> f (&member-if f (&cdr xs))))))
(defun &filter (f xs &key key)
  (if key (let ((g #/(funcall f (funcall key _))))
            (<&filter> g (&member-if g xs)))
    (<&filter> f (&member-if f xs))))

(defun <&filter-not> (f xs)
  (when xs
    (&cons! (car xs) 
            (<&filter-not> f (&member-if-not f (&cdr xs))))))
(defun &filter-not (f xs &key key)
  (if key (let ((g #/(funcall f (funcall key _))))
            (<&filter-not> g (&member-if-not g xs)))
    (<&filter-not> f (&member-if-not f xs))))




(defun &alist-plist (alist)
  #{when alist
  #{let ((x (&car alist)))
  (cons (car x) (&cons! (cdr x) (&alist-plist (&cdr alist)))))

(defun &plist-alist (plist)
  #{when plist
  #{let ((1st (&first plist))
         (2nd (&second plist)))
  (&cons! (cons 1st 2nd) (&plist-alist (&cddr plist))))

(defun <&pairlis> (keys values alist)
  (cond ((and keys values)
          (&cons! (cons (&car keys) (&car values))
                  (<&pairlis> (&cdr keys) (&cdr values) alist)))
        ((or keys values) (error "&pairlis: keys and values are not same length"))
        (t alist)))

@doc"
;; PAIRLISの遅延版 rest-alistは`そのまま'連結される
;; 遅延版の挙動: keys,valuesの先頭から順に構成される
"
(defun &pairlis (keys values &optional rest-alist)
  (<&pairlis> keys values rest-alist))

(defun &assoc (x alist &key key test test-not)
  (if key
    (&member x alist :key #/(funcall key (car _)) :test test :test-not test-not)
    (&member x alist :key #'car :test test :test-not test-not)))
(defun &assoc-if (f alist &key key)
  (if key
    (&member-if f alist :key #/(funcall key (car _)))
    (&member-if f alist :key #'car)))
(defun &assoc-if-not (f alist &key key)
  (if key
    (&member-if-not f alist :key #/(funcall key (car _)))
    (&member-if-not f alist :key #'car)))


(defun rpt (x) (let ((xs (list x))) (setf (cdr xs) xs)))

(defun &for (i)
  (when (> i 0)
    (&cons i (&for (1- i)))))


;; FUNCTIONAL OPERATIONS

;; generate sections
(defun sec  (f a) (lambda (x) (funcall f a x)))
(defun secr (f a) (lambda (x) (funcall f a x)))
(defun secl (f a) (lambda (x) (funcall f x a)))



(defun <&collect-one> (op body)
  `(let ((g (macrolet ((,op (x) `(yield (cons ,x nil))))
              (make-generator ,@body))))
    (labels ((f () (awhen (funcall g)
                     (setf (cdr it) (delay (f)))
                     it)))
      (f))))


(defmacro &collect (&body body)
  (<&collect-one> '_ body))


(defmacro &with-collect ((&rest operators) &body body)
  (unless (every #'symbolp operators)
    (error "&WITH-COLLECT: illegal clause ~D" operators))
  #{let* ((m (length operators))
          (nums (iota m)))
  (cond ((null operators) `(progn ,@body (values)))
        ((eq 1 m)
          (<&collect-one> (first operators) body))
        (t
          `(let ((g (macrolet ,(mapcar @\ab`(,a (x) `(yield (cons ,',b ,x)))
                                       operators
                                       nums)
                      (make-generator ,@body))))
            (labels ((f () (awhen (funcall g)
                             (&cons! it (f)))))
              (let ((source (f)))
                (values ,@(mapcar #/`(&mapcan 
                                      (lambda (x) (when (eq ,_ (car x)) 
                                                    (list (cdr x)))) 
                                      source)
                                  nums))))))))
                

;;;;;;;;  漸化式 ;;;;;;;;;

(defun <&recurs> (f init-value other-init-values)
  (if (null other-init-values)
    (let ((arg init-value))
      (&frepeat (lambda () (setq arg (funcall f arg)))))
    (let ((args (cons init-value other-init-values)))
      (&frepeat (lambda () (aprog1 (apply f args)
                             (setf (first args) it
                                   args (rotate args -1))))))))

;; 漸化式による無限シーケンスの生成
(defun &recurs (f init-value &rest other-init-values)
  (nconc (cons init-value (copy-list other-init-values))
         (<&recurs> f init-value other-init-values)))

;; 漸化式による無限シーケンスの生成(ただし初項を含めない)
(defun &recurs* (f init-value &rest other-init-values)
  (<&recurs> f init-value other-init-values))




;;;;;;;;;;;;;;;;;;;;;;;;;; End of J-LAZYLIST.LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment 
;;;;;;;;;;;;;;;;;;;;;;;;;;


(j-header)
(&strict (&collect ## 
   @_ (list i p q x)  @where
   (dotimes (i 2))
   (dolists (p '(a b) q '(x y z)))
   (&dolist (x (&iota 4)))))

