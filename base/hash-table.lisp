;; NAME-HASH-TABLE Time-stamp: <2018-10-05 16:07:27 USER> [autotitle,overwrite]

(oleo.core:oleo-core-header)

(oleo.core:define-package :oleo.base.hash-table* (:oleo.base.hash-table)
  (:use :cl)
  (:import/export :oleo.base.hlist*)
  (:export

   ;;for NAME-HASH-TABLE
   #:make-name-hash-table
   #:get-name-hash
   #:set-name-hash
   #:rem-name-hash
   #:clr-name-hash
   #:map-name-hash

   ;; General Hash Table Utilities
   #:do-hash-table
   #:hash-table-bind #:hash-table-ebind #:hash-table-rebuild-bind #:hash-table-rebuild-ebind
   #:hash-table-update-bind #:hash-table-update-ebind
   #:make-hash-table-let #:make-hash-table-let*

   #:hash-table-compare
   #:hash-table-eq #:hash-table-eql #:hash-table-equal #:hash-table-equalp
   
   )
  )

(in-package :oleo.base.hash-table)

;; シンボル専用ハッシュテーブル
;; シンボルの名前で検索するため、パッケージ横断した同名のシンボルは同一のキーと見做す
;; ex) foo::baz bar::baz :baz はどれも同一のキー `baz' として扱われる

;@eval-always
(defstruct (name-hash-table (:constructor mknh)) symhash strhash)

(defun make-name-hash-table ()
  (mknh :symhash (make-hash-table :test 'eq)
        :strhash (make-hash-table :test 'equal)))

(defun get-name-hash (key namehashtable)
  (unless (symbolp key) (error "get-name-hash: symbol required : ~D" key))
  (alet (acond ((gethash key (name-hash-table-symhash namehashtable))
                it)
               ((gethash (symbol-name key) (name-hash-table-strhash namehashtable))
                (setf (gethash key (name-hash-table-symhash namehashtable)) it)
                (setf (cdr it) (cons key (cdr it)))
                it
                )
               
               (t
                nil))
    (values (car it) (consp it))))

(defun set-name-hash (key value namehashtable)
  (unless (symbolp key) (error "set-name-hash: symbol required: ~D" key))
  (acond ((gethash key (name-hash-table-symhash namehashtable))
          (setf (car it) value))
          ((gethash (symbol-name key) (name-hash-table-strhash namehashtable))
           (setf (gethash key (name-hash-table-symhash namehashtable)) it)
           (setf (cdr it) (cons key (cdr it))) 
           (setf (car it) value))
           (t
            (let ((cell (list value key)))
              (setf (gethash key (name-hash-table-symhash namehashtable)) cell)
              (setf (gethash (symbol-name key) (name-hash-table-strhash namehashtable))
                      cell)
              (car cell)))))

  
(defun rem-name-hash (key namehashtable )
  (let ((cell (gethash (symbol-name key) (name-hash-table-strhash namehashtable))))
    (when cell
      (mapc #'(lambda (k) (remhash k (name-hash-table-symhash namehashtable)))
            (cdr cell))
      (remhash (symbol-name key) (name-hash-table-strhash namehashtable)))))
                
(defun map-name-hash (func namehashtable)
  (maphash #'(lambda (k cell)
               (declare (ignore k))
               (funcall func (car (last cell))   (car cell)))
           (name-hash-table-strhash namehashtable)))

(defun clr-name-hash (namehashtable)
  (clrhash (name-hash-table-strhash namehashtable))
  (clrhash (name-hash-table-symhash namehashtable)))



;;;

(defmacro do-hash-table ((key-var val-var hash-table-form &optional result-form) &body body)
  (let ((fn (gensym))
        (e (gensym)))
    `(with-hash-table-iterator (,fn ,hash-table-form)
       (loop
          (multiple-value-bind (,e ,key-var ,val-var) (,fn)
            (unless ,e (return ,result-form))
            ,@body)))))



(defun hash-table-compare (hash-table-x hash-table-y &key (test 'eql))
  (and (eql (hash-table-count hash-table-x)
            (hash-table-count hash-table-y))
       (do-hash-table (key val hash-table-x T)
         (multiple-value-bind (dst-val exist?) (gethash key hash-table-y)
           (unless (and exist?
                        (funcall test val dst-val))
             (return))))))

(defun hash-table-eql (hash-table-x hash-table-y) (hash-table-compare hash-table-x hash-table-y))
(defun hash-table-eq (hash-table-x hash-table-y) (hash-table-compare hash-table-x hash-table-y :test 'eq))
(defun hash-table-equal (hash-table-x hash-table-y) (hash-table-compare hash-table-x hash-table-y :test 'equal))
(defun hash-table-equalp (hash-table-x hash-table-y) (hash-table-compare hash-table-x hash-table-y :test 'equalp))

#Testing
(check-unit* (:test nil) (eq _ (_ (plist-hash-table (copy-tree '_)) (plist-hash-table (copy-tree '_))))
  
  (nil hash-table-compare (a 0) ())
  (nil hash-table-compare (a 0) (a 0 b 1))
  (t hash-table-eq (a 0) (a 0))
  (t hash-table-eql (a 2.71828) (a 2.71828))
  (nil hash-table-eq (a (x)) (a (x)))
  (nil hash-table-eql (a (x)) (a (x)))
  (t hash-table-equal (a ("a" "b")) (a ("a" "b")))
  (nil hash-table-equal (a ("A" "b")) (a ("a" "b")))
  (t hash-table-equalp (a ("A" "b")) (a ("a" "b")))
  )

;;;;;

(defun <hash-table-ebind-error> (sym hash-table)
  (error "HASH-TABLE-EBIND: could not find symbol ~A: hash-table=~A" sym hash-table))

(declaim (inline <hash-table-update>))
(defun <hash-table-update> (key val hash-table)
  (aif (assoc key hash-table)
       (progn (setf (cdr it) val) hash-table)
       (push (cons key val) hash-table)))

;(reduce #'list '(1 2 3) :from-end t :initial-value :init) 



(defun <hash-table-bind> (error? rebuild? update? vars hash-table-form body)
  (let* ((hash-table (gensym))
         (need-modify (and (not rebuild?) update?))
         (update-form (when update?
                        `(progn
                           (setf ,@(mapcan (lambda (key) `((gethash ',key ,hash-table) ,key))
                                           vars))
                           ,hash-table))))
    `(let ((,hash-table ,hash-table-form))
       (let ,(mapcar (lambda (v) `(,v ,(if error?
                                         `(multiple-value-bind (value exist?) (gethash ',v ,hash-table)
                                            (if exist?
                                              value
                                              (<hash-table-ebind-error> ',v ,hash-table)))
                                         `(gethash ',v ,hash-table))))
                     vars)
         (,(if need-modify 'UNWIND-PROTECT 'PROGN)
           (progn
             ,@body
             ,@(when rebuild? `((setq ,hash-table (copy-hash-table ,hash-table))))
             ,@(when (and rebuild? update?)
                 (list update-form))
             )
           ,@(when need-modify (list update-form))
           )))))


(defmacro hash-table-bind (vars hash-table-form &body body)
  (<hash-table-bind> nil nil nil vars hash-table-form body))
(defmacro hash-table-ebind (vars hash-table-form &body body)
  (<hash-table-bind> T nil nil vars hash-table-form body))

(defmacro hash-table-rebuild-bind (vars hash-table-form &body body)
  (<hash-table-bind> nil t t vars hash-table-form body))
(defmacro hash-table-rebuild-ebind (vars hash-table-form &body body)
  (<hash-table-bind> t t t vars hash-table-form body))


(defmacro hash-table-update-bind (vars hash-table-form &body body)
  (<hash-table-bind> nil nil t vars hash-table-form body))
(defmacro hash-table-update-ebind (vars hash-table-form &body body)
  (<hash-table-bind> t nil t vars hash-table-form body))


;; (hash-table-bind (a b) (plist-hash-table '(a 0)) (list a b))
;; (hash-table-ebind (a b) (plist-hash-table '(a 0)) (list a b))
;; (hash-table-rebuild-bind (a b) (plist-hash-table '(a 0)) (list a b))
;; (hash-table-rebuild-ebind (a ) (plist-hash-table '(a 0)) (list a))
;; (hash-table-update-ebind (a ) (plist-hash-table '(a 0)) (list a))
;; (hash-table-update-bind (a b) (plist-hash-table '(a 0)) (list a))

#Testing HASH-TABLE-BIND
(check-unit (_ _ (plist-hash-table '_) _)
  (hash-table-bind (a b) (a 1 b 2) (list b a) '(2 1))
  (hash-table-bind (a b) (a 1) (list b a) '(nil 1))
  (hash-table-bind (a b) nil (list b a) '(nil nil))
  (hash-table-ebind (a b) (a 1 b 2) (list b a) '(2 1))
  )

#Testing HASH-TABLE-EBIND HASH-TABLE-REBUILD-EBIND HASH-TABLE-UPDATE-EBIND
(check-unit* (:test nil) (has-errors (_ _ (plist-hash-table '_) _))
  (hash-table-ebind (a b) (a 1) (list b a))
  (hash-table-ebind (a b) nil (list b a))
  (hash-table-rebuild-ebind (a b) (a 1) (list b a))
  (hash-table-rebuild-ebind (a b) nil (list b a))
  (hash-table-update-ebind (a b) (a 1) (list b a))
  (hash-table-update-ebind (a b) nil (list b a))
  )

#Testing HASH-TABLE-REBUILD-BIND
(let ((src (alist-hash-table '((a . 1) (b . 2) (c . 3)))))
  (check-unit* (:test nil :splicing ~) (let ((ans (plist-hash-table '_))
                                             (result (_ _ src ~)))
                                         (and (hash-table-eql ans result)
                                              (not (eq src result))))
                             
    ((a 0 b 2 c 3) hash-table-rebuild-bind  (a b c) ((setq a 0)))
    ((a 1 b 0 c 3) hash-table-rebuild-bind  (a b c) ((setq b 0)))
    ((a 0 b 1 c 2) hash-table-rebuild-bind  (a b c) ((setq a 0) (setq b 1 c 2)))
    ((a 1 b 2 c 3) hash-table-rebuild-bind  (a b c) ())
    ((a 0 b 1 c 2) hash-table-rebuild-ebind (a b c) ((setq a 0 b 1 c 2)))
    ((a 1 b 2 c 3) hash-table-rebuild-ebind (a b c) ())))



#Testing HASH-TABLE-UPDATE-BIND HASH-TABLE-UPDATE-EBIND
(let* ((src (plist-hash-table '(a 1 b 2 c 3))))
  (check-unit* (:test nil :splicing ~) (let* ((correct-src (plist-hash-table '_))
                                              (ans _)
                                              (result (_ _ src ~)))
                                         (and
                                           (eql ans result)
                                           (hash-table-equal correct-src src)
                                           ))
                             
    ((a 0 b 2 c 3) 5 hash-table-update-ebind  (a b c) ((setq a 0) 5))
    ((a 0 b _ c 3) 6 hash-table-update-bind  (a b c) ((setq b '_) 6))
    ((a 3 b 1 c 2) 7 hash-table-update-ebind  (a b c) ((setq a 3) (setq b 1 c 2) 7))
    ((a 3 b 1 c 2) nil hash-table-update-bind  (a b c) ())))


#C


(alist-compare (plist-alist '(a 0)) (plist-alist '(a 0 )))
                
         
