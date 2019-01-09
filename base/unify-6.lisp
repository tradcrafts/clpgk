;; -*- coding: utf-8 -*-

(oleo.core:oleo-core-header)

(in-package :oleo.base.unify)

               
(defgeneric valid-for-transform-sequence-p (command len))

(defmacro trseq (src copied? commands)
  (let ((v (gensym))
        (init? (not copied?)))
    `(let ((,v ,src))
      (if (and ,@(mapcar 
                  (lambda (cmd) 
                    (prog1 `(not (eq <terr> (setq ,v (<trseq> ,v ,copied? ,cmd ,init?))))
                      (setq init? (eq 'function (car cmd)))
                      (unless copied? (setq copied? t))))
                  commands))
        ,v
        <terr>))))
  
(defmacro <trseq> (src copied? cmd init?) 
  (flet ((main (v) `(and
                    ,@(mapcar (lambda (c) 
                             (prog1 `(not (eq <terr> 
                                           (setq ,v 
                                                   ,(transform-sequence c v copied?))))
                               (unless copied? (setq copied? t))))
                           (cdr cmd))))
         (check (v) (if init?
                      `(and 
                        (not (eq <terr> ,v))
                        (or (and (listp ,v) (true-list? ,v))
                            (vectorp ,v)
                            (stringp ,v)))
                      `(not (<terr>-p ,v)))))
  (case (car cmd)
    (:safe 
      (let* ((v (gensym))
             (len-v (gensym))
             (pre-test
               `(and
                 ,@(mapcar (lambda (c) `(setq ,len-v 
                                         ,(valid-for-transform-sequence-p c len-v)))
                           (cdr cmd)))))
        `(let ((,v ,src)
               ,len-v) 
          (if (and ,(check v) (setq ,len-v (length ,v)) ,pre-test ,(main v))
            ,v
            <terr>))))
    (:unsafe
      (let ((v (gensym)))
        `(let ((,v ,src))
          (if (and ,(check v) ,(main v))
            ,v
            <terr>))))
    (function
      (let ((f (second cmd)))
        (cond ((symbolp f) (list f src))
              ((consp f)
                (if (eq 'LAMBDA (first f))
                  (list f src)
                  `(,@f ,src)))
              (t (error ":-> :to : illegal FUNCTION command: ~D" cmd)))))
      
    (LAMBDA
        (list cmd src))

    (:par
      (let ((v (gensym)))
        `(let ((,v ,src)
               r
               tmp)
          (block TRANSEQ-ABORT
            ,@(mapcar (lambda (cmd) `(if (eq <terr> (setq r (trseq ,v nil ,cmd)))
                                      (return-from TRANSEQ-ABORT <terr>)
                                      (push r tmp)))
                      (cdr cmd))
            (nreverse tmp)))))

    (:par*
      (let ((v (gensym)))
        `(let ((,v ,src)
               is-string
               r
               tmp)
          (if ,(check v)
            (block TRANSEQ-ABORT
              (when (stringp ,v) (setq is-string t))
              ,@(mapcar (lambda (cmd) `(if (or (eq <terr> (setq r (trseq ,v nil ,cmd)))
                                               (and is-string
                                                    (not (characterp r))))
                                        (return-from TRANSEQ-ABORT <terr>)
                                        (push r tmp)))
                        (cdr cmd))
              (if (listp ,v) 
                (nreverse tmp)
                (if (and ,copied?
                         (= (length ,v) ,(length (cdr cmd))))
                  (map-into ,v 'identity (nreverse tmp))
                  (concatenate (if (stringp ,v) 'string 'vector)
                               (nreverse tmp)))))
                
            <terr>))))

    (:grp 
      (let ((v (gensym)))
        `(let ((,v ,src))
          (trseq ,v nil ,(cdr cmd)))))

    (:map
      (let ((v (gensym)))
        `(let ((,v ,src))
          (if ,(check v)
            (catch 'TRANSEQ-ABORT
              (map 'list
                   (lambda (x)
                     (let ((y (trseq x nil ,(cdr cmd))))
                       (if (eq <terr> y)
                         (throw 'TRANSEQ-ABORT <terr>)
                         y)))
                   ,v))
            <terr>))))
                         
    (:map*
      (let* ((v (gensym))
             (fnc-header (if copied? 
                           (list 'map-into v)
                           `(map (cond ((listp ,v) 'list)
                                       ((stringp ,v) 
                                         (setq is-string t) 
                                         'string)
                                       ((vectorp ,v) 'vector))))))
        `(let ((,v ,src)
               is-string)
          (if ,(check v)
            (catch 'TRANSEQ-ABORT
              (,@fnc-header
               (lambda (x)
                 (let ((y (trseq x nil ,(cdr cmd))))
                   (if (or (eq <terr> y)
                           (and is-string (not (characterp y))))
                     (throw 'TRANSEQ-ABORT <terr>)
                     y)))
               ,v))
            <terr>))))
                         
                     
      
    )))
        

(defun code-for-to (ptn var info)
  (do-unify-when
      (ptn
       (:and (:append ?spec (?body))
             (:call collect spec))
       :let (command)
       :define
       ((safe-builtin? (:-> :eq :double :triple :double* :triple*
                            :double-slide :triple-slide :double-slide* :triple-slide*
                            :reverse :tails :tails*
                            :tail :head :half :half*
                            :evens :odds
                            :numbering :copy-whole :copy-tree
                            :list :vector :string))
        (unsafe-builtin? (:-> :eq :flatten :join))
        ((collect :set :my c r rest sub :let escaped)
         (:or nil
           (:and (:append :maximize
                    (:and ?c
                          (:or (:and (:each+ (:call safe-builtin?))
                                     (:do (setq c (cons :safe (nreverse c)))))
                               (:and (:each+ (:call unsafe-builtin?))
                                     (:do (setq c (cons :unsafe (nreverse c)))))
                               (:and ((function  ?))
                                     (:do (setq c (car c))))
                               (:and ((lambda . ?))
                                     (:do (setq c (car c))))
                               (:env :my h
                                     (:and (((:-> ?h :eq :map :map* :grp) . ?r))
                                           (:do (setq escaped command
                                                      command nil))
                                           (:call collect r)
                                           (:do (setq c (cons h command)
                                                      command escaped))))
                               (:env :my sub h :let subs
                                 (:and (((:-> ?h :eq :par :par*) . ?r))
                                       (:do (setq escaped command
                                                  command nil))
                                       (:for r 
                                           (:each (:and ?sub
                                                        (:if (:call grouped? sub)
                                                          (:call collect sub)
                                                          (:call collect (list sub)))
                                                        (:do (push command subs)
                                                             (setq command nil)))))
                                       (:do (setq c (cons h (nreverse subs))
                                                  command escaped))))))
                    ?rest)
                    (:do (push c command))
                    (:call collect rest))))
        ((grouped? :set)
         (:and (:-> :and listp true-list?)
               ((:-> :not-eq function lambda :map :map* :par :par* :grp) . ?)))
        )
       :on-failure (error ":TO")
       )

    ;(print command)
    `(let ((r (trseq ,var ,nil ,command)))
      (if (eq <terr> r)
        nil
        (complex-unify r ,body ,info)))
    ))



(defparameter *collect-vars-func* 'collect-vars-complexly)


;============== TRANSFORM ===================
;; 以下はマクロ展開で使われる。 
;; 引数lenはgensymされたユニークな識別子であることが保証される。
(defmethod valid-for-transform-sequence-p (_ len)
  (declare (ignore _))
  len)

(defmethod valid-for-transform-sequence-p ((_ (eql :odds)) len)
  (declare (ignore _))
  `(if (evenp ,len) 
    (/ ,len 2)
    (/ (1- ,len) 2)))

(defmethod valid-for-transform-sequence-p ((_ (eql :evens)) len)
  (declare (ignore _))
  `(if (evenp ,len) 
    (/ ,len 2)
    (/ (1+ ,len) 2)))

(defmethod valid-for-transform-sequence-p ((_ (eql :double)) len)
  (declare (ignore _))
  `(when (zerop (mod ,len 2))  (/ ,len 2)))
(defmethod valid-for-transform-sequence-p ((_ (eql :double*)) len)
  (declare (ignore _))
  (valid-for-transform-sequence-p :double len))

(defmethod valid-for-transform-sequence-p ((_ (eql :triple)) len)
  (declare (ignore _))
  `(when (zerop (mod ,len 3))  (/ ,len 3)))
(defmethod valid-for-transform-sequence-p ((_ (eql :triple*)) len)
  (declare (ignore _))
  (valid-for-transform-sequence-p :triple len))

(defmethod valid-for-transform-sequence-p ((_ (eql :half)) len)
  (declare (ignore _))
  `(when (zerop (mod ,len 2)) 2))
(defmethod valid-for-transform-sequence-p ((_ (eql :half*)) len)
  (declare (ignore _))
  (valid-for-transform-sequence-p :half len))

(defmethod valid-for-transform-sequence-p ((_ (eql :tail)) len)
  (declare (ignore _))
  `(when (> ,len 0) (1- ,len)))

(defmethod valid-for-transform-sequence-p ((_ (eql :head)) len)
  (declare (ignore _))
  `(when (> ,len 0) 1))

(defmethod valid-for-transform-sequence-p ((_ (eql :numbering)) len)
  (declare (ignore _))
  `(* 2 ,len))

(defmethod valid-for-transform-sequence-p ((_ (eql :tails)) len) 
  (declare (ignore _))
  `(when (< 0 ,len) ,len))
(defmethod valid-for-transform-sequence-p ((_ (eql :tails*)) len) 
  (declare (ignore _))
  `(when (< 0 ,len) ,len))

(defmethod valid-for-transform-sequence-p ((_ (eql :double-slide)) len) 
  (declare (ignore _))
  `(when (< 1 ,len) (1- ,len)))
(defmethod valid-for-transform-sequence-p ((_ (eql :double-slide*)) len)
  (declare (ignore _))
  (valid-for-transform-sequence-p :double-slide len))

(defmethod valid-for-transform-sequence-p ((_ (eql :triple-slide)) len)
  (declare (ignore _))
  `(when (< 2 ,len)  (- ,len 2)))
(defmethod valid-for-transform-sequence-p ((_ (eql :triple-slide*)) len)
  (declare (ignore _))
  (valid-for-transform-sequence-p :triple-slide len))

;; 以下もマクロ展開で使われる。 
;; 引数xsはgensymされたユニークな識別子、copied?はnilかtであることが保証される。

(defmethod transform-sequence ((_ (eql :numbering))  xs copied?)
  (list 'transform-sequence/numbering xs copied?))

(defmethod transform-sequence/numbering ((xs list) copied?)
  (let ((cnt -1))
    (if copied?
      (do ((xs xs (cdr xs))
           (last nil xs)
           first)
          ((null xs) first)
          (if first 
            (setf (cdr last) (cons (incf cnt) xs))
            (setq first (cons (incf cnt) xs))))
      (mapcan (lambda (x) (list (incf cnt) x))
              xs))))

(defmethod transform-sequence/numbering ((xs vector) copied?)
  (declare (ignore copied?))
  (let ((cnt -1)
        (r (make-sequence 'vector (* 2 (length xs)))))
    (dotimes (i (length xs) r)
      (setf (svref r (* i 2)) (incf cnt))
      (setf (svref r (1+ (* i 2))) (svref xs i)))))

(defmethod transform-sequence/numbering ((xs string) copied?)
  (declare (ignore xs copied?))
  <terr>)

(defun ts-helper-list-evens (xs copied?)
  (if copied?
    (do ((cur xs (cdr cur)))
        ((null cur) xs)
      (setf (cdr cur) (cddr cur)))
    (do ((xs xs (cddr xs))
         (r nil (cons (car xs) r)))
        ((null xs) (nreverse r)))))

