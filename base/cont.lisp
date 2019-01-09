(oleo.core:oleo-core-header)
;(extern :cl-cont)
;(declaim (optimize (speed 3)
;                   (compilation-speed 0)))


(oleo.core:define-package :oleo.base.cont* (:oleo.base.cont)

  ;(:import-from :util.pre.symbols #:fail)
  (:use :cl :cl-cont)
  (:import/export :oleo.base.form*)
  (:export 

   #:promise-p
   #:delay #:force 
   #:promise-stable-p #:promise-mutable-p


   #:local
   
   #:define-generator #:define-generator/w-rewinder
   #:make-generator 
   #:yield
   
   #:with-root/cc #:root/cc #:continue/cc
   #:push/cc
   #:mark/cc #:cut/cc
   #:backtrack/cc #:rewind/cc
   #:try-backtrack/cc #:try-rewind/cc
   #:can-backtrack-p/cc
   
   #:fail
   #:choose 
   #:choose-list #:choose-n
   #:choose-for
   #:choose-one

   ;; from CL-CONT
   #:call/cc 	#:copy-transformation-context
   #:cpstransformer 	#:defcpstransformer
   #:defun/cc 	#:expr->cps
   #:expr-sequence->cps 	#:lambda/cc
   #:let/cc 	#:with-call/cc
   #:without-call/cc

   ))


(in-package :oleo.base.cont)

(defstruct promise status value)

@inline
(defun promise-mutable-p (x)
  (eq (promise-status x) 'mutable))

@inline
(defun promise-stable-p (x)
  (not (eq (promise-status x) 'mutable)))


(defun <delay> (form &optional mutable-p)
  (if (or (and (consp form)
               #!(eq 'quote (car form)))
          (symbolp form))
    `(make-promise :status ,(when mutable-p ''mutable) :value (lambda () ,form))
    form))


(defmacro delay (form &key mutable)
  (<delay> form mutable))

(defun force (x)
  (cond ((promise-p x)
          #{let ((s (promise-status x)))
          (if (eq s 'stable)
            (promise-value x)
            (let ((evaluated (funcall (promise-value x))))
              (if s
                evaluated ;; s == 'mutable
                (setf (promise-status x) 'stable
                      (promise-value x) evaluated)))))
        (t x)))


(defmacro local (&body body)
  `(without-call/cc ,@body))

(defvar *gen/cc* nil)
(defvar *root/cc* nil)
(defvar *context/cc* nil)

(defmacro yield (&body body)
  `(if *gen/cc*
    (let/cc cc
      (setq *gen/cc* cc)
      (when *root/cc* 
        (setq *root/cc* 'yield) 
        nil)
      ,@body)
    (error "yield: please use define-generator")))

(defmacro define-generator (name &body body)
  `(without-call/cc
    (let ((cc (lambda/cc (&rest _) @ignore _ ,@body)))
      (defun ,name (&rest vals)
        (let* ((*gen/cc* cc))
          (multiple-value-prog1
            (apply cc vals)
            (setq cc *gen/cc*)))))))

(defmacro define-generator/w-rewinder (name &body body)
  `(without-call/cc
    (let* ((top (lambda/cc (&rest _) @ignore _ ,@body))
           (cc top))
      (defun ,(intern (concatenate 'string "REWIND-" (symbol-name name))) ()
        (setq cc top)
        nil)
      (defun ,name (&rest vals)
        (let* ((*gen/cc* cc))
          (multiple-value-prog1
            (apply cc vals)
            (setq cc *gen/cc*)))))))
        
(defmacro make-generator (&body body)
  `(without-call/cc
    (let ((cc (lambda/cc (&rest _) @ignore _ ,@body)))
      (lambda (&rest vals)
        (let* ((*gen/cc* cc))
          (multiple-value-prog1
            (apply cc vals)
            (setq cc *gen/cc*)))))))



(defmacro %with-root/cc% (context body)
  `(if *root/cc*
    (progn ,@body)
    (let ((=root/cc= (lambda/cc () ,@body))
          (=context/cc= (without-call/cc ,context))
          (loop t)
          ret)
      ;(print "%with-root/cc% ACTIVATE!")
      (while loop
        (without-call/cc
          (setq loop nil)
          (let ((*root/cc* =root/cc=)
                (*context/cc* =context/cc=))
            (while (functionp *root/cc*)
              ;(print "%with-root/cc GET!")
              (let ((c *root/cc*))
                (setq *root/cc* t)
                (setq ret (multiple-value-list (funcall c)))))
            (when (eql 'yield *root/cc*)
              (setq =context/cc= *context/cc*)
              (setq =root/cc= *gen/cc*)
              (setq loop t))))
        (when loop
          (let/cc cc 
            (setq *gen/cc* cc) 
            (apply #'values ret))))
      (without-call/cc 
        (apply #'values ret)))))

(defmacro with-root/cc (&body body)
  `(%with-root/cc% nil ,body))


(defmacro continue/cc (v &body body)
  (let ((sym (gensym))
        (con (gensym)))
    `(if *root/cc*
      (let/cc ,sym
       (without-call/cc 
         (setq *root/cc* ,sym))
       (let* ((,con *context/cc*)
              (,v (lambda () (%with-root/cc% ,con (funcall ,sym)))))
         ,@body)))
    (error "continue/cc: please use with-root/cc")))


;; (defmacro root/cc (c)
;;   `(if *root/cc*
;;     (let/cc _ (setq *root/cc* c))
;;     (error "root/cc: please use with-root/cc")))

(defun/cc root/cc (c)
  (if *root/cc*
    (let/cc |unused|
      (declare (ignore |unused|))
      (without-call/cc 
        (setq *root/cc* c)))
    (error "root/cc: please use with-root/cc")))

(defun/cc push/cc (&rest ret)
  (if *root/cc*
    (let/cc cc 
      (setq *root/cc* (if ret 
                        (lambda (&rest |unused|) (declare (ignore |unused|)) 
                                (apply cc ret)) 
                        cc))
      (push cc *context/cc*))
    (error "push/cc: please use with-root/cc")))

(defun backtrackable-p ()
  (find-if #'functionp *context/cc*))

(defun/cc backtrack/cc (&rest vals)
  (if (backtrackable-p)
    (let ((c (pop *context/cc*)))
      (until (functionp c)
        (setq c (pop *context/cc*)))
      (root/cc (if vals 
                 (lambda (&rest |unused|) (declare (ignore |unused|))
                         (apply c vals)) 
                 c)))
    (error "backtrack/cc: context not found")))

(defun/cc rewind/cc (&rest vals)
  (aif (backtrackable-p)
       (root/cc (if vals 
                  (lambda (&rest |unused|) (declare (ignore |unused|))
                          (apply it vals)) 
                  it))
    (error "retry/cc: context not found")))
       

(defun/cc try-backtrack/cc (&rest vals)
  (if (backtrackable-p)
    (apply #'backtrack/cc vals)
    t))

(defun/cc try-rewind/cc (&rest vals)
  (if (backtrackable-p)
    (apply #'rewind/cc vals)
    t))

(setf (symbol-function 'fail) #'try-rewind/cc)

(defun can-backtrack-p/cc () (if (backtrackable-p) t nil))


(defun cut/cc (&optional (key 'root))
  (if (symbolp key)
    (if *root/cc*
      (while *context/cc*
        (when (eql key (pop *context/cc*))
          (return)))
      (error "cut/cc: please use with-root/cc"))
    (error "cut/cc: symbol required")))

(defmethod mark/cc ((key symbol))
  (if *root/cc*
    (push key *context/cc*)
    (error "mark/cc: please use with-root/cc")))


(defmacro choose-list (xs &key into (mark (gensym)))
  (with-gensyms (lst)
    `(let ((,lst ,xs))
      (mark/cc ',mark)
      (push/cc)
      (prog1
        ,(if into `(setf ,into (pop ,lst)) `(pop ,lst))
        (unless ,lst
          (cut/cc ',mark))))))

(defmacro choose-n (n &key into (mark (gensym)))
  `(choose-for :end ,n :into ,into :by 1 :mark ,mark))


(defmacro choose-for (&key (start 0) end by into (mark (gensym)))
  (with-gensyms (cur to d)
    `(let* ((,cur ,start)
            (,to ,end)
            (,d ,(if by by `(if (> ,to ,cur) 1 -1))))
      (mark/cc ',mark)
      (push/cc)
      (prog1
        ,(if into `(setf ,into ,cur) cur) 
        (setq ,cur (+ ,cur ,d))
        (when (= ,cur ,to)
          (cut/cc ',mark))))))

      

