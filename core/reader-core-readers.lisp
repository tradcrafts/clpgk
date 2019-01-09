;; -*- coding: utf-8 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; in `READER' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-annot:enable-annot-syntax)
(in-package :oleo.core.reader)

(defun <read-into-list> (source limit)
  (unless (and limit (zerop limit))
    (let (tmp
          (start 0))
      (loop (multiple-value-bind (x next) (read-from-string source nil '|read-eof| :start start)
              (cond ((eq x '|read-eof|)
                      (return))
                    (t
                      (setq start next)
                      (push x tmp)
                      (when (and limit (zerop (decf limit)))
                        (return))

                      ))))
      (nreverse tmp))))

(defun read-sequential-from-string (source &key limit)
  (<read-into-list> source limit))


(defmacro define-and-export-reader-1 (macro-char)
  (let ((fn-name (intern 
                  (string-concat (coerce (list macro-char) 'string)
                                 "-READER"))))
    `;(eval-when (:compile-toplevel :load-toplevel :execute)
      (let ((native (get-macro-character ,macro-char)))
       (defun ,fn-name (stream char)
         (funcall native stream char)))));)

(defmacro define-and-export-reader-2 (macro-char/1 macro-char/2)
  (let ((fn-name (intern 
                  (string-concat (coerce (list macro-char/1 macro-char/2) 'string)
                                 "-READER"))))
    ` ;(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((native (get-dispatch-macro-character ,macro-char/1 ,macro-char/2)))
         (defun ,fn-name (stream char1 char2)
           (funcall native stream char1 char2)))));)


(define-and-export-reader-1 #\')
(define-and-export-reader-1 #\()
(define-and-export-reader-1 #\;)
(define-and-export-reader-2 #\# #\')
(define-and-export-reader-2 #\# #\()
(define-and-export-reader-2 #\# #\*)
(define-and-export-reader-2 #\# #\B)
(define-and-export-reader-2 #\# #\O)
(define-and-export-reader-2 #\# #\X)

;; )が現れるまでの要素をまとめる
;; (defun ... #{when test #{case x (a 'foo) (b 'bar))
;; ==> (defun ... (when test (case x (a 'foo) (b 'bar))))
;; 先頭がコンスの場合、先頭がラムダ式とそうでない場合で挙動が異なる
;; 非ラムダ式の場合、先頭はカリー化されたフォームとして扱われる
;; (... #{(let ...) a b) => (... (let ... a b))
;; ラムダ式の場合は、単に要素をまとめるだけである
;; (... #{(lambda ...) a b) => ((lambda ...) a b)
(defun |#{-READER| (stream char1 char2)
  (declare (ignore char1 char2))
  (let ((xs (read-delimited-list #\) stream t)))
    (unread-char #\) stream)
    (if (and xs (consp (car xs)))
      (case (caar xs)
        (lambda xs)
        (t (nconc (car xs) (cdr xs))))
      xs)))

(defun reader/skip-chars-while (stream comparar)
  (let ((c (read-char stream)))
    (cond ((funcall comparar c)
            (reader/skip-chars-while stream comparar))
          (t 
            (unread-char c stream)
            nil))))

(defun reader/take-chars-while (stream comparar)
  (do ((c (read-char stream) (read-char stream))
       tmp)
      ((not (funcall comparar c))
       (unread-char c stream)
       (coerce (nreverse tmp) 'string))
    (push c tmp)))
  
(defun reader/skip-white-spaces (stream)
  (reader/skip-chars-while stream (lambda (c) 
                             (member c '(#\Space #\Newline #\Tab #\Return)))))

(defun reader/skip-line (stream)
  (reader/skip-chars-while stream (lambda (c) 
                             (not (eq #\Newline c))))
  ;; 書き戻された#\Newlineを読み飛ばす
  (read-char stream)
  nil)



(defun reader/line-reader (stream)
  (prog1
    (reader/take-chars-while stream (lambda (c) 
                               (not (eq #\Newline c))))
    ;; 書き戻された#\Newlineを読み飛ばす
    (read-char stream)))

(defvar *<tmp-pkg>* (make-package "reader-tmp-pkg-1"))

(defun reader/ident-reader (stream)
  (let* ((*package* *<tmp-pkg>*)
         (exp (read stream t nil t)))
    (unless (symbolp exp)
      (error "~A is not a valid identifier" exp))
    (let ((ident (copy-seq (symbol-name exp))))
      (when (eq (symbol-package exp) *<tmp-pkg>*)
        (unintern exp))
      ident)))

(defvar *lpar-backslash-reader-table* (make-hash-table :test 'equalp))

(defmacro define-lpar-backslash-reader (ident-str lambdalist &body body)
  ;(precond 'define-lpar-backslash-reader :src ident-str :type string)
  `(setf (gethash ,ident-str *lpar-backslash-reader-table*)
    (lambda ,lambdalist ,@body)))


