
(in-package :oleo.core.test)

;; @eval-when-test form は、当該パッケージがテスト環境を必要とする場合にのみformを有効化する
(define-annotation eval-when-test (form)
  (if (<test-needed?>)
    form
    (eval-when ())))

(define-annotation test (form) `(testing ,form))

(define-annotation verify (form) `(testing (check-assert ,form)))

(define-annotation todo (form) `(todo.. ,@form))


(defun <register-readers> ()
  (flet ((reader-for-testing (f)
           (lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (do (tmp 
          c)
         ((eq #\Newline (setq c (read-char stream nil))) ;;行末まで読み飛ばす
          (let ((tag (coerce (cdr (member #\Space (nreverse tmp)) )
                             'string))
                (src (read stream t nil t)))
            (funcall f tag src)))
       (push c tmp)))))
    
    ;; ユニットテスト用の入力マクロ #T
    ;; #T...改行までを無視し、次のオブジェクトを`(testing (eval ',object))とする
    (set-dispatch-macro-character 
     #\# #\T (reader-for-testing
              (lambda (tag src)
                `(testing (let ((*%TESTING-TAG-STRING%* ,tag))
                            (eval ',src))))))
    
    ;; 入力マクロ #V `verify'
    ;; 式のベリファイ
    ;; #V exp は、(testing (eval '(check-assert, exp)))に展開される
    (set-dispatch-macro-character 
     #\# #\V (reader-for-testing
              (lambda (tag src)
                `(testing (let ((*%TESTING-TAG-STRING%* ,tag))
                            (check-units* (:test nil :error t :special |unuse| :predicate (lambda (x) x))
                                ((eval ',src) nil)))))))
    ))




(register-reader-registerer '|test| '<register-readers>)
