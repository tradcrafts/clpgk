
(in-package :oleo.core.test)

;; 真の場合テストを完全に無効化する (最優先フラグ)
(defvar *ignore-testing* nil) 

;; *ignore-testing*が偽の場合、このフラグが真であればテストを強制的に有効化する
(defvar *force-testing* nil) 

(defvar *<package-testing-table>* (make-hash-table :test 'equal))

(defun <get-*package-testing*> ()
  (intern "*PACKAGE-TESTING*"))

(defun <test-needed?> ()
  (and (not *ignore-testing*)
       (or *force-testing*
           (let ((var (<get-*package-testing*>)))
             (or (not (boundp var))     ;; {デフォルト状態　もしくは…
                 (symbol-value var)))))) ;; 明示的に真が設定されている場合}

(defmacro testing (&body tests)
  (if (<test-needed?>)
    `(eval-when (:load-toplevel :execute) 
      (<start-package-testing> *package*)
      ,@tests)
    '(eval-when ())))

(defun <start-package-testing> (pkg-object)
  (let ((name (package-name pkg-object)))
    (unless (gethash name *<package-testing-table>*)
      (console-message "PACKAGE ~A: TESTING..." name)
      (setf (gethash name *<package-testing-table>*) t))))

;;;;;;;
;; デフォルトではパッケージごとのTEST許可フラグは真である
;; (noundp '*package-testing*)が偽の場合(デフォルト状態)も真と見做す
;;;;;;;


(defmacro enable-testing (&optional (enable t))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,(<get-*package-testing*>) ,enable)))

(defmacro disable-testing (&optional (disable t))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,(<get-*package-testing*>) (not ,disable))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




'(enable-testing T)

(defvar *%testing-tag-string%* "")
(defun <tag-string> ()
  (if (zerop (length *%testing-tag-string%*))
    ""
    (format nil "TESTING: ~A~%" *%testing-tag-string%*)))
    

(defvar *<params>*)
(defvar *<special-symbol>*)
(defvar *<splicing-symbol>*)
(defvar *<test>*)
(defvar *<error>*)
(defvar *<predicate>*)

(defun <next-param> ()
  (if *<params>*
    (prog1 (car *<params>*)
      (setf *<params>* (cdr *<params>*)))
    (error "UNIT TESTING: too few parameters")))

(defun <term> (x)
  (cond ((eq x *<special-symbol>*)
          (<next-param>))
        ((and *<splicing-symbol>*
              (eq x *<splicing-symbol>*))
          (error "UNIT TESTING: misplaced ~A (special splicing symbol)" x))
        (t x)))

(defun <need-splicing?> (xs)
  (when *<splicing-symbol>*
    (do ((c xs (cdr c)))
        ((atom c) nil)
      (when (eq (car c) *<splicing-symbol>*)
        (return t)))))


(defun <traverse> (exp)
  (typecase exp
    (circular-list (error "circulate list is not allowed"))
    (cons 
      (let (tmp)
        (if (<need-splicing?> exp)
          (do ((x exp (cdr x)))
              ((atom x) 
               (push (<traverse> x) tmp)
               (apply #'nconc (nreverse tmp)))
            (if (eq (car x) *<splicing-symbol>*)
              (push (<next-param>) tmp)
              (push (list (<traverse> (car x))) tmp)))
          (do ((x exp (cdr x)))
              ((atom x) 
               (push (<traverse> x) tmp)
               (apply #'list* (nreverse tmp)))
            (push (<traverse> (car x)) tmp)))))
    (string exp)
    (simple-vector (coerce (<traverse> (coerce exp 'list)) 'vector))
    (t (<term> exp))))


(defun <pkg-info> ()
  (let ((pathname (or *load-pathname* 
                      *compile-file-pathname*))
        (pkgname (package-name *package*)))
    (if pathname 
      (format nil "~A(~A)" pkgname pathname)
      (format nil "~A" pkgname))))


(defmacro todo.. (&body form-elements)
  (let* ((msg (when (stringp (first form-elements))
                (first form-elements)))
         (form (if msg (rest form-elements) form-elements)))
        (cond (msg 
            (console-message "TODO: package ~A: ~A" (<pkg-info>) msg))
          (form
            (let ((s (let ((*print-length* 20))
                       (format nil "~S" form)))
                  (max 80))
              (console-message "TODO: package ~A: ~A" 
                    (<pkg-info>)
                    (if (> (length s) max)
                      (subseq s 0 max)
                      s))))
          (t 
            (console-message "TODO: package ~A" (<pkg-info>))))
    form))

(defun <test-failed> (form param)
  (error "~APACKAGE ~A: UNIT TESTING FAILED: ~A <- ~A" 
         (<tag-string>) (<pkg-info>) form param))
  
(defun <check-unit-code> (form paramss)
  (when *<predicate>*
    (setq form `(,*<predicate>* ,form)))
  `(and ,@(mapcar (lambda (x)
                    (let ((code (if *<test>*
                                  `(,*<test>* 
                                    ,(lastcar x)
                                    ,(let ((*<params>* (butlast x)))
                                          (<traverse> form)))
                                  (let ((*<params>* x))
                                    (<traverse> form)))))
                      (if *<error>*
                        `(or ,code (<test-failed> ',form ',x))
                        code)))
                  paramss)))



(defun <check-unit-equation> (x op y)
  (when *<predicate>*
    (setq x `(,*<predicate>* ,x)
          y `(,*<predicate>* ,y)))
  (let ((main `(,*<test>* ,x ,y))
        (*<test>* nil)
        (*<predicate>* nil))
    (<check-unit-code> (if (eq op '=) main (list 'not main)) 
                       '(nil))))

(defun <check-unit> (form paramss &aux (c (first paramss)))
  (cond ((or (eq c '=) (eq c '/=))
          (if (and *<test>* 
                   (eql 2 (length paramss)))
            (<check-unit-equation> form (first paramss) (second paramss))
            (error "~A" (cons form paramss))))
        (t (<check-unit-code> form paramss))))
  

(defmacro check-unit* ((&key (special '_) 
                             splicing
                             (test 'equal) 
                             predicate
                             (error t)) 
                       form &body paramss)
  (let ((*<special-symbol>* special)
        (*<splicing-symbol>* splicing)
        (*<test>* test)
        (*<predicate>* predicate)
        (*<error>* error))
    (<check-unit> form paramss)))

(defmacro check-unit (form &body paramss)
  `(check-unit* () ,form ,@paramss))

(defun <check-units> (test-clauses)
  `(and ,@(mapcar (lambda (x) (<check-unit> (car x) (cdr x)))
                  test-clauses)))

        

(defmacro check-units* ((&key (special '_)
                              splicing
                              (test 'equal) 
                              predicate
                              (error t))
                   &body test-clauses)
  (let ((*<special-symbol>* special)
        (*<splicing-symbol>* splicing)
        (*<test>* test)
        (*<predicate>* predicate)
        (*<error>* error))
    (<check-units> test-clauses)))

(defmacro check-units (&body test-clauses)
  `(check-units* () ,@test-clauses))
 
(defmacro check-assert* ((&key (error t) predicate)
                         &body forms)
  `(check-units* (:test nil :error ,error :predicate ,predicate)
    ,@(mapcar (lambda (x) `(,x ()))                                
              forms)))

(defmacro check-assert (&body forms)
  `(check-assert* () ,@forms))


'(testing
  (flet ((pred (x) (declare (ignore x)) 'ok))
    (check-units* (:predicate pred)
      (1 = 2)
      (nil = 'a)
      (_ (1 'ok)))
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     Utilities 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro protected-multiple-value-list (form)
  `(multiple-value-bind (r e) (ignore-errors (multiple-value-list ,form))
    (if e 'error r)))

(defmacro protected-multiple-value-list* (on-error-exp form)
  (with-gensyms (r e)
    `(multiple-value-bind (,r ,e) (ignore-errors (multiple-value-list ,form))
      (if ,e ,on-error-exp ,r))))

(defmacro has-errors (&body body)
  `(multiple-value-bind (r e) (ignore-errors ,@body nil)
    (declare (ignore r))
    (when e t)))

(defmacro has-no-errors (&body body)
  `(multiple-value-bind (r e) (ignore-errors ,@body nil)
    (declare (ignore r))
    (unless e t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *<precond-codes>*)
(defvar *<precond-src>*)
(defvar *<precond-error-args>*)
(defvar *<precond-in-args>*)
(defvar *<precond-context-args>*)

(defun <precond-read-args> (elems)
  (do ((c elems (cdr c))
       tmp)
      ((or (null c) (keywordp (car c)))
       (values (nreverse tmp)
               c))
    (push (car c) tmp)))
  

(defmacro <precond-with-args> (args elems &body body)
  `(multiple-value-bind (,args |rest-elems|) (<precond-read-args> ,elems)
    ,@body
    |rest-elems|))
  
(defun <precond-error-args> (elems)
  (<precond-with-args> src elems
    (setq *<precond-error-args>* src)))

(defun <precond-src> (elems)
  (<precond-with-args> src elems
    (let ((bindings (mapcar (lambda (x) (list (gensym) x))
                            src)))
      (push `(<let> ,bindings) *<precond-codes>*)
      (setq *<precond-src>* bindings))))


(defun <perr> (&rest formatters)
  (error (apply #'string-concat
                (<tag-string>)
                "PRECOND: "
                (format nil "in package ~A:" (<pkg-info>))
                (mapcar (lambda (x) (string-concat ": " (apply #'format nil x)))
                        formatters))))

(defun <error/precond> (fmt &rest args)
  (if (or *<precond-in-args>*
          *<precond-context-args>*
          *<precond-error-args>*)
    `(<perr> 
      ,@(when *<precond-in-args>*
              `((list ,@*<precond-in-args>*)))
      ,@(when *<precond-context-args>*
              `((list ,@*<precond-context-args>*)))
      ,@(when *<precond-error-args>*
              `((list ,@*<precond-error-args>*)))
      (list ,fmt ,@args))
    `(<perr> (list ,fmt ,@args))))


(defun <make-src-pairs-string> (&rest src-pairs)
  (do ((c src-pairs (cddr c))
       tmp)
      ((null c)
       (apply #'string-concat (nreverse tmp)))
    (push (format nil "~A=~A " (first c) (second c))
          tmp)))

(defun <src-pairs-string> ()
  `(<make-src-pairs-string> ,@(mapcan (lambda (binding) 
                                        `(',(second binding) ,(first binding)))
                                      *<precond-src>*)))

(defun <error/precond-assert> (form src-info)
  (<error/precond> "test failed: (funcall ~A {src}) : sources {~A}" `',form src-info))

(defun <precond-assert> (elems)
  (<precond-with-args> forms elems
    (let ((src-pairs-string-form (<src-pairs-string>)))
      (dolist (form forms)
        (push `(or ,form ,(<error/precond-assert> form src-pairs-string-form))
              *<precond-codes>*)))))


(defun <precond-do> (elems)
  (<precond-with-args> forms elems
    (dolist (form forms)
      (push form *<precond-codes>*))))

(defun <error/precond-test-type> (src-form types)
  (<error/precond> "~A (= ~A) must be (one of) ~A" 
                   `',src-form src-form `',types))

(defun <precond-test-type> (elems)
  (<precond-with-args> types elems
    (dolist (binding *<precond-src>*)
      (let ((src (first binding)))
        (push `(or 
                ,(let (subcodes)
                      (dolist (type types `(or ,@subcodes))
                        (push `(typep ,src ',type)
                              subcodes)))
                ,(<error/precond-test-type> src types))
              *<precond-codes>*)))))

(defun <error/precond-test-same-type> (src-forms types)
  (let ((tmp (apply #'string-concat 
                    (mapcar (lambda (x) (declare (ignore x))
                              "~A (= ~A) ")
                            src-forms)))
        (tmp2 (mapcan (lambda (binding) `(',(second binding) ,(first binding)))
                      src-forms)))

    (apply #'<error/precond> 
           (string-concat tmp "must be (one of) ~A")
           (nconc tmp2 (list `',types)))))


(defun <precond-test-same-type> (elems)
  (<precond-with-args> types elems
    (when *<precond-src>*
      (let (code)
        (dolist (type types)
          (push (let (subcodes)
                  (dolist (binding *<precond-src>* `(and ,@subcodes))
                    (push `(typep ,(first binding) ',type)
                          subcodes)))
                code))
        (push `(or 
                ,@(nreverse code)
                ,(<error/precond-test-same-type> *<precond-src>* types))
              *<precond-codes>*)))))


(defun <error/precond-test> (func-form src-info)
  (<error/precond> "test failed: (funcall ~A {src}) : sources {~A}" 
                   `',func-form src-info))

(defun <precond-test> (elems)
  (<precond-with-args> funcs elems
    (let ((tmp (gensym))
          (src-pairs-string-form (<src-pairs-string>)))
      (push `(do-sources (,tmp ,(mapcar #'first *<precond-src>*) t)
              ,@(mapcar (lambda (f)
                          `(unless (funcall ,f ,tmp)
                            ,(<error/precond-test> f src-pairs-string-form)))
                        funcs))
            *<precond-codes>*))))

(defun <error/precond-any> (func-forms src-info)
  (<error/precond> "test failed: (funcall <one-of>{~A} {src}) : sources {~A}" 
                   `',func-forms src-info))

(defun <precond-any> (elems)
  (<precond-with-args> funcs elems
    (let ((tmp (gensym))
          (src-pairs-string-form (<src-pairs-string>)))
      (push `(do-sources (,tmp ,(mapcar #'first *<precond-src>*) t)
              (unless (or ,@(mapcar (lambda (f) (list 'funcall f tmp))
                                    funcs))
                ,(<error/precond-any> funcs src-pairs-string-form)))
            *<precond-codes>*))))

;(precond :src 1 :any #'numberp (lambda (x) (print x)))



(defun <precond-context> (elems)
  (<precond-with-args> args elems
    (setq *<precond-context-args>* args)))

        
(defun <precond> (elems &aux (first (first elems)))
  (if (null elems)
    T
    (macrolet ((ap (op) `(<precond> (,op (cdr elems)))))
      (case first
        (:ASSERT (ap <precond-assert>))
        (:SRC (ap <precond-src>))
        (:TEST (ap <precond-test>))
        (:ANY (ap <precond-any>))
        (:TYPE (ap <precond-test-type>))
        (:SAME-TYPE (ap <precond-test-same-type>))
        (:DO (ap <precond-do>))
        (:ERROR (ap <precond-error-args>))
        (:CONTEXT (ap <precond-context>))
        (t (error "precond: unknown command ~A" first))))))

(defun <build-precond-code> (codes)
  (let (series)
    (do ((p codes (cdr p)))
        ((null p)
         (nreverse series))
      (let ((c (car p)))
        (if (and (consp c)
                 (eq '<let> (first c)))
          (return `(,@(nreverse series)
                    (let ,(second c)
                      ,@(<build-precond-code> (cdr p)))))
          (push c series))))))

(defmacro precond (&body elems)
  (let ((*<precond-codes>* nil)
        (*<precond-src>* nil)
        (*<precond-in-args>* nil)
        (*<precond-context-args>* nil)
        (*<precond-error-args>* nil))
    (when (and elems
               (not (keywordp (first elems))))
      (setq *<precond-in-args>* (list "in ~A" (first elems))
            elems (cdr elems)))
    (<precond> elems)
    `(progn 
      ,@(<build-precond-code> (nreverse *<precond-codes>*))
      T)))

          

'(defun foo ()
  (do-multiple-value-sources ((a b c) ((values 1 2 3) (values 2 3 4)) t)
    (print (list a b c))))



               
  




