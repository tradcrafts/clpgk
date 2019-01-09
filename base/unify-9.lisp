(oleo.core:oleo-core-header)

(in-package :oleo.base.unify)

(define-unification t t
  (:enum
   (:whole this)
   (:this this)
   (:copy this))
  (:bool
   (:true? `(not (null ,this)))
   (:false? `(null ,this))))

(define-unification (sequence t) `(typep ,this 'sequence)
  (:enum
   (:length `(length ,this))
   (:copy `(copy-seq ,this))
   (:copy-tree `(copy-tree ,this))
   (:reverse `(reverse ,this))
   )
  (:query
   (:nreverse () `(nreverse ,this))
   (:copy-to (to) `(copy-sequence ',to ,this))
   (:split (delim) `(split-sequence ',delim ,this)))
  (:bool 
   (:empty? `(emptyp ,this)))
)

(define-unification (cons sequence)  `(consp ,this)
  (:enum
   (:whole `(car ,this) `(cdr ,this))
   (:car `(car ,this))
   (:cdr `(cdr ,this))
   (:last `(last ,this))
   (:lastcar `(lastcar ,this))
   (:butlast `(butlast ,this))
   )
  (:query
   (:nth (i) `(nth ,i ,this))
   (:nthcdr (i) `(nthcdr ,i ,this))
   (:nbutlast () `(nbutlast ,this))
   )
  (:safe-query
   (:ref (i) `(let ((i ,i))
               (if (< i (length ,this))
                 (nth i ,this)
                 ,error))))
  (:bool
   (:proper-list? `(proper-list-p ,this))
   (:circular-list? `(circular-list ,this))
   (:circular-tree? `(circular-tree ,this))
   (:end? `(null (cdr ,this)))))

(define-unification (list cons) `(listp ,this)
  (:bool
   (:null? `(null ,this)))
  (:enum
   (:whole this)
   (:length `(length ,this))))

(define-unification (proper-list list) `(proper-list-p ,this)
  )
(define-unification (circular-list list) `(circular-list-p ,this)
  )
(define-unification (circular-tree list) `(circular-tree ,this)
  )

(define-unification (plist proper-list)  ` (and (proper-list-p ,this) 
                                      (evenp (length ,this)))
  (:enum
   (:copy `(copy-list ,this))
   (:alist `(plist-alist ,this))
   (:hash-table `(plist-hash-table ,this))
   (:count `(/ (length ,this) 2))
   (:keys (with-gensyms (i)
            ` (let ((,i 0))
                (mapcan (lambda (x) (when (oddp (incf ,i)) (list x)))
                        ,this))))
   (:values (with-gensyms (i)
            ` (let ((,i 0))
                (mapcan (lambda (x) (when (evenp (incf ,i)) (list x)))
                        ,this))))
   )
  
  (:bool-query
    (:exist? (key) `(getf ,this ,key))
    )

   (:query
    (:mapkeys (f) (with-gensyms (i fn)
                    ` (let ((,i 0)
                            (,fn ,f))
                        (mapc (lambda (x) (when (oddp (incf ,i)) (funcall ,fn  x)))
                              ,this))))
    (:mapvalues (f) (with-gensyms (i fn)
                    ` (let ((,i 0)
                            (,fn ,f))
                        (mapc (lambda (x) (when (evenp (incf ,i)) (funcall ,fn  x)))
                              ,this))))
    )

   (:safe-query
    (:get (key) `(aif (getf ,this ,key) it ,error))
    )

  )

(define-unification (alist proper-list)  ` (and (proper-list-p ,this)
                                      (every #'consp ,this))
  (:enum
   (:copy `(mapcar (lambda (c) (cons (car c) (cdr c))) ,this))
   (:plist `(alist-plist ,this))
   (:hash-table `(alist-hash-table ,this))
   (:keys `(mapcar #'car ,this))
   (:values `(mapcar #'cdr ,this))
   (:count `(length ,this))
   )

  (:bool-query
   (:exist? (key) `(assoc ,key ,this))
   )

  (:query
   (:mapkeys (f) (with-gensyms (fn)
                   `(let ((,fn ,f)) (mapc (lambda (x) (funcall ,fn (car x))) ,this))))
   (:mapvalues (f) (with-gensyms (fn)
                     `(let ((,fn ,f)) (mapc (lambda (x) (funcall ,fn (cdr x))) ,this))))

   )

  (:safe-query
   (:get (key) `(aif (assoc ,key ,this) it ,error))
   )
  )

(define-unification (hash-table proper-list)  ` (hash-table-p ,this)
  (:enum
   (:copy `(make-hash-table 
            :initial-contents (hash-table-alist ,this)
            :test (hash-table-test ,this)
            :size (hash-table-size ,this)
            :rehash-size (hash-table-rehash-size ,this)
            :rehash-threshold (hash-table-rehash-threshold ,this)))
   (:alist `(hash-table-alist ,this))
   (:plist `(hash-table-plist ,this))

   (:count `(hash-table-count ,this))
   )

  (:bool-query
   (:exist? (key) ` (multiple-value-bind (val e) (gethash ,key ,this) 
                      (declare (ignore val))
                      e))
   )

  (:query
   (:mapkeys (f) `(progn (maphash-keys ,f ,this) ,this))
   (:mapvalues (f) `(progn (maphash-values ,f ,this) ,this))
   )
  (:safe-query
   (:get (key) ` (multiple-value-bind (val e) (gethash ,key ,this) 
                   (if e val ,error)))
   )
  )

(define-unification (number t) `(numberp ,this)
  (:bool
   (:zero? `(zerop ,this))
   (:positive? `(< 0 ,this))
   (:negative? `(> 0 ,this))
   (:integer? `(integerp ,this))
   (:real? `(not (integerp ,this))))
  (:bool-query
   (:< (x) `(< ,this ,x))
   (:<= (x) `(<= ,this ,x))
   (:> (x) `(> ,this ,x))
   (:>= (x) `(>= ,this ,x))
   ;; 閉区間
   (:<=.=> (a b) `(and (<= ,a ,this) (<= ,this ,b)))
   ;; 開区間
   (:<.> (a b) `(and (< ,a ,this) (< ,this ,b)))
   ;; 半開区間
   (:<=.> (a b) `(and (<= ,a ,this) (< ,this ,b)))
   (:<.=> (a b) `(and (< ,a ,this) (<= ,this ,b))))
  )
  
(define-unification (integer number) `(integerp ,this)
  (:bool
   (:odd? `(oddp ,this))
   (:even? `(evenp ,this)))
  (:query
   (:mod (a) `(mod ,this ,a)))
  (:safe-query
   (:test (a) `(if (integerp ,a) (* ,a ,a) ,error)))
  )

(define-unification (symbol t) `(symbolp ,this)
  (:enum 
   (:name `(symbol-name ,this))
   (:package `(symbol-package ,this)))
  (:bool
   (:keyword? `(keywordp ,this))))

(define-unification (keyword symbol) `(keywordp ,this)
  )

(define-unification (vector sequence) `(vectorp ,this)
  )
(define-unification (string vector) `(stringp ,this)
  (:enum
   (:read `(with-input-from-string (is ,this) (read is)))
   (:intern `(intern ,this)))
  )
(define-unification (simple-vector vector) `(simple-vector-p ,this)
  )



