;; -*- coding: utf-8 -*-
;; This file is part of CLPGK.
;; Copyright (c) 2019 PGkids Laboratory

(clpgk.core:clpgk-core-header)

(in-package :clpgk.base.unify)


(defun ts-helper-vec-evens/odds (evens? xs &aux (test (if evens? 'evenp 'oddp)))
  #{let ((seqtype (if (stringp xs) 'string 'vector)))

  (if nil ;(stringp xs) 
    '''''(copy-sequence 'string
                   (ts-helper-sv-evens/odds evens? 
                                            (copy-sequence 'simple-vector xs)))
    (macrolet ((newlen () `(if evens? 
                            ,(valid-for-transform-sequence-p :evens 'len) 
                            ,(valid-for-transform-sequence-p :odds 'len))))
      (let* ((len (length xs))
             (r (make-sequence seqtype (newlen))))
        (do ((i 0 (1+ i))
             (j -1))
            ((= len i) r)
          (when (funcall test i) (setf (aref r (incf j)) (aref xs i))))))))


(defmethod transform-sequence ((_ (eql :evens))  xs copied?)
  (declare (ignore _))
  `(cond 
    ((listp ,xs) (ts-helper-list-evens ,xs ,copied?))
    ((vectorp ,xs) (ts-helper-vec-evens/odds t ,xs))
    (t )))

(defmethod transform-sequence ((_ (eql :odds))  xs copied?)
  (declare (ignore _))
  `(cond 
    ((listp ,xs) (ts-helper-list-evens (cdr ,xs) ,copied?))
    ((vectorp ,xs) (ts-helper-vec-evens/odds nil ,xs))
    (t )))



(defmethod transform-sequence ((_ (eql :list)) xs copied?)
  (declare (ignore _))
  `(if (listp ,xs) 
    (if ,copied? ,xs (copy-list ,xs))
    (concatenate 'list ,xs)))

(defmethod transform-sequence ((_ (eql :vector)) xs copied?)
  (declare (ignore _))
  `(if (vectorp ,xs)
    (if ,copied? ,xs (copy-seq ,xs))
    (concatenate 'vector ,xs)))

(defmethod transform-sequence ((_ (eql :string)) xs copied?)
  (declare (ignore _))
  `(if (stringp ,xs)
    (if ,copied? ,xs (copy-seq ,xs))
    (if (every #'characterp ,xs)
      (concatenate 'string ,xs)
      <terr>)))


(defmethod transform-sequence ((_ (eql :copy-whole)) xs copied?)
  (declare (ignore _))
  `(if ,copied? ,xs (copy-whole ,xs)))

;; simple vector版copy-tree
;; ベクタ部だけを再帰的に複製する
(defun sv-copy-tree (xs)
  (let ((new (copy-seq xs)))
    (dotimes (i (length new) new)
      (when (simple-vector-p (svref new i))
        (setf (svref new i) (sv-copy-tree (svref new i)))))))

;; リスト、シンプルベクタ、文字列を再帰的に再構成する
(defun copy-whole (xs)
  (cond ((listp xs)
          (let ((new (copy-list xs)))
            (do ((xs new (cdr xs)))
                ((null xs) new)
              (setf (car xs) (copy-whole (car xs))))))
        ((simple-vector-p xs)
          (let ((new (copy-seq xs)))
            (dotimes (i (length new) new)
              (setf (svref new i) (copy-whole (svref new i))))))
        ((stringp xs) (copy-seq xs))
        (t xs)))

(defmethod transform-sequence ((_ (eql :copy-tree)) xs copied?)
  (declare (ignore _) (ignore copied?))
  `(cond 
    ((listp ,xs) (copy-tree ,xs))
    ((stringp ,xs) (copy-seq ,xs))
    (t (sv-copy-tree ,xs))))

(defmethod transform-sequence ((_ (eql :reverse)) xs copied?)
  (declare (ignore _))
  `(if ,copied?  (nreverse ,xs)  (reverse ,xs)))

(defmethod transform-sequence ((_ (eql :half)) xs copied?)
  (declare (ignore _))
  `(if (listp ,xs)
    (transform-sequence/half* ,xs ,copied?)
    (concatenate 'list (transform-sequence/half* ,xs ,copied?))))

(defmethod transform-sequence ((_ (eql :half*)) xs copied?)
  (declare (ignore _))
  `(if (stringp ,xs)
    <terr>
    (transform-sequence/half* ,xs ,copied?)))
    

(defmethod transform-sequence/half* ((xs list) copied?)
  (declare (ignore _))
  (cond ((null xs) (list nil nil))
        (copied? (let ((left-end (nthcdr (1- (/ (length xs) 2)) xs)))
                   (prog1 
                     (list xs (cdr left-end))
                     (setf (cdr left-end) nil))))
        (t (let ((len/2 (/ (length xs) 2)))
             (list (subseq xs 0 len/2) (subseq xs len/2)))))) 

(defmethod transform-sequence/half* ((xs vector) copied?)
  (declare (ignore _) (ignore copied?))
  (let ((len/2 (/ (length xs) 2)))
    (vector (subseq xs 0 len/2) 
            (subseq xs len/2))))


(defmethod transform-sequence ((_ (eql :head)) xs copied?)
  (declare (ignore _))
  `(if (listp ,xs)
    (if ,copied? 
      (prog1 ,xs (setf (cdr ,xs) nil))
      (cons (car ,xs) nil))
    (subseq ,xs 0 1)))

(defmethod transform-sequence ((_ (eql :tail)) xs copied?)
  (declare (ignore _))
  `(if (listp ,xs)
    (if ,copied? (cdr ,xs) (copy-list (cdr ,xs)))
    (subseq ,xs 1)))
    

(defmethod transform-sequence ((_ (eql :tails)) xs copied?)
  (declare (ignore _))
  `(if (listp ,xs) 
    (maplist 'identity (if ,copied? ,xs (copy-list ,xs)))
    (transform-sequence/tails ,xs)))

(defun transform-sequence/tails (xs)
  (let (tmp)
    (dotimes (i (length xs) (nreverse tmp))
      (push (subseq xs i) tmp))))

(defmethod transform-sequence ((_ (eql :tails*)) xs copied?)
  (declare (ignore _))
  `(if (listp ,xs) 
    (maplist 'identity (if ,copied? ,xs (copy-list ,xs)))
    (transform-sequence/tails* ,xs)))

(defmethod transform-sequence/tails* ((xs string))
  (declare (ignore xs))
  <terr>)

(defmethod transform-sequence/tails* ((xs vector))
  (let ((vec (make-sequence 'vector (length xs))))
    (dotimes (i (length xs))
      (setf (svref vec i) (subseq xs i)))
    vec))


(defmethod transform-sequence ((_ (eql :double)) xs copied?)
  (declare (ignore _))
  `(if (listp ,xs)
    (transform-sequence/double* ,xs ,copied?)
    (concatenate 'list (transform-sequence/double* ,xs ,copied?))))

(defmethod transform-sequence ((_ (eql :double*)) xs copied?)
  (declare (ignore _))
  `(if (stringp ,xs)
    <terr>
    (transform-sequence/double* ,xs ,copied?)))

(defmethod transform-sequence/double* ((xs list) copied?)
  (let (tmp)
    (if copied?
      (while xs
        (push xs tmp)
        (setq xs (prog1 
                   (cddr xs)
                   (setf (cddr xs) nil))))
      (do ((xs xs (cddr xs)))
          ((null xs))
        (push (list (first xs) (second xs)) tmp)))
    (nreverse tmp)))

(defmethod transform-sequence/double* ((xs vector) copied?)
  (declare (ignore copied?))
  (let ((len (length xs)))
    (do ((r (make-sequence 'vector (/ len 2)))
         (i 0 (+ i 2))
         (j 0 (1+ j)))
        ((>= i len) r)
      (setf (svref r j) (subseq xs i (+ i 2))))))

(defmethod transform-sequence ((_ (eql :triple)) xs copied?)
  (declare (ignore _))
  `(if (listp ,xs)
    (transform-sequence/triple* ,xs ,copied?)
    (concatenate 'list (transform-sequence/triple* ,xs ,copied?))))

(defmethod transform-sequence ((_ (eql :triple*)) xs copied?)
  (declare (ignore _))
  `(if (stringp ,xs)
    <terr>
    (transform-sequence/triple* ,xs ,copied?)))

(defmethod transform-sequence/triple* ((xs list) copied?)
  (let (tmp)
    (if copied?
      (while xs
        (push xs tmp)
        (setq xs (prog1 
                   (cdddr xs)
                   (setf (cdddr xs) nil))))
      (do ((xs xs (cdddr xs)))
          ((null xs))
        (push (list (first xs) (second xs) (third xs)) tmp)))
    (nreverse tmp)))

(defmethod transform-sequence/triple* ((xs vector) copied?)
  (declare (ignore copied?))
  (let ((len (length xs)))
    (do ((r (make-sequence 'vector (/ len 3)))
         (i 0 (+ i 3))
         (j 0 (1+ j)))
        ((>= i len) r)
      (setf (svref r j) (subseq xs i (+ i 3))))))

(defmethod transform-sequence ((_ (eql :double-slide)) xs copied?)
  (declare (ignore _))
  `(if (listp ,xs)    
    (transform-sequence/double-slide* ,xs ,copied?)
    (concatenate 'list (transform-sequence/double-slide* ,xs ,copied?))))

(defmethod transform-sequence ((_ (eql :double-slide*)) xs copied?)
  (declare (ignore _))
  `(if (stringp ,xs)
    <terr>
    (transform-sequence/double-slide* ,xs ,copied?)))

(defmethod transform-sequence/double-slide* ((xs list) copied?)
  (let (tmp)
    (if copied?
      (while (cdr xs)
        (push xs tmp)
        (setq xs (prog1 
                   (cdr xs)
                   (setf (cdr xs) (cons (second xs) nil)))))
      (do ((xs xs (cdr xs)))
          ((null (cdr xs)))
        (push (list (first xs) (second xs)) tmp)))
    (nreverse tmp)))

(defmethod transform-sequence/double-slide* ((xs vector) copied?)
  (declare (ignore copied?))
  (let ((end (1- (length xs))))
    (do ((r (make-sequence 'vector end))
         (i 0 (1+ i)))
        ((= i end) r)
      (setf (svref r i) ;(vector (svref xs i) (svref xs (1+ i)))
              (subseq xs i (+ i 2))
            ))))

(defmethod transform-sequence ((_ (eql :triple-slide)) xs copied?)
  (declare (ignore _))
  `(if (listp ,xs)    
    (transform-sequence/triple-slide* ,xs ,copied?)
    (concatenate 'list (transform-sequence/triple-slide* ,xs ,copied?))))

(defmethod transform-sequence ((_ (eql :triple-slide*)) xs copied?)
  (declare (ignore _))
  `(if (stringp ,xs)
    <terr>
    (transform-sequence/triple-slide* ,xs ,copied?)))

(defmethod transform-sequence/triple-slide* ((xs list) copied?)
  (let (tmp)
    (if copied?
      (while (cddr xs)
        (push xs tmp)
        (setq xs (prog1 
                   (cdr xs)
                   (setf (cdr xs) (list (second xs) (third xs))))))
      (do ((xs xs (cdr xs)))
          ((null (cddr xs)))
        (push (list (first xs) (second xs) (third xs)) tmp)))
    (nreverse tmp)))

(defmethod transform-sequence/triple-slide* ((xs vector) copied?)
  (declare (ignore copied?))
  (let ((end (- (length xs) 2)))
    (do ((r (make-sequence 'vector end))
         (i 0 (1+ i)))
        ((= i end) r)
      (setf (svref r i) (subseq xs i (+ i 3))))))



;; simple vector版flatten
;; 非破壊的である
;; alexandria:flattenに倣い、simple vectorでない引数が与えられればベクタで囲って返す
(defun svflatten (src)
  (if (simple-vector-p src)
    (let (tmp)
      (dotimes (i (length src))
        (if (simple-vector-p (svref src i))
          (push (svflatten (svref src i)) tmp)
          (push (cons (svref src i) nil) tmp)))
      (apply #'concatenate 'vector (nreverse tmp)))
    (vector src)))

(defmethod transform-sequence ((_ (eql :flatten)) xs copied?)
  (declare (ignore _))
  `(cond 
    ((listp ,xs) (flatten ,xs))
    ((stringp ,xs) (if ,copied? ,xs (copy-seq ,xs)))
    ((vectorp ,xs) (svflatten ,xs))
    (t ,xs)))


(reduce 'nconc '((1 2) (3 4)))

(defun svjoin (vec)
  (let* ((dst-list (reduce #'nconc
                           (map 'list 
                                (lambda (x) (if (simple-vector-p x) 
                                              (copy-sequence 'list x)
                                              (list x)))
                                vec)
                           :from-end t))
         (dst-len (length dst-list)))
    (make-array dst-len :initial-contents dst-list)))
                     
(defmethod transform-sequence ((_ (eql :join)) xs copied?)
  (declare (ignore _))
  `(cond 
    ((listp ,xs) (apply 'nconc (mapcar #/#>ensure-list ,xs)))
    ((stringp ,xs) (if ,copied? ,xs (copy-seq ,xs)))
    ((vectorp ,xs) (svjoin ,xs))
    (t ,xs)))
    

