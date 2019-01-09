(oleo.core:oleo-core-header)

(in-package :oleo.base.unify)


;; 注意！　:env節はこの関数が定義されるまで使えない
(defun code-for-env (src ptn info &aux (all-vars (uinfo-all-vars info)))
  (do-unify-when
      (ptn
       (:and (:append ?spec (?body))
             (:call f spec))
       :let (tmp)
       :define
       ((f (:or nil
                (:and
                  (:do :unbind cmd vars rest)
                  (:or (:append ((:-> ?cmd :eq :set :add :del))
                                :maximize
                                (:and ?vars
                                      (:each (:call var?)))
                                ?rest)
                       (:and (:append ((:-> ?cmd :eq :my :my* :let :let*))
                                      :maximize
                                      (:and ?vars
                                            (:each (:or (:call var?)
                                                        ((:call var?) ?))))
                                      ?rest)
                             (:do (when (or (eq cmd :my) (eq cmd :my*))
                                    (setq vars (mapcar 
                                                (lambda (v) 
                                                  (if (symbolp v)
                                                    (list v ''*UNBOUND-UNIFICATION-VARIABLE*)
                                                    v))
                                                vars))))))
                  (:do (push (cons cmd vars) tmp))
                  (:call f rest))))
        (var? (:-> :view (x) (and (symbolp x) (not (keywordp x))))))
       :on-failure (error ":ENV"))
  
    (let* ((new-info (copy-uinfo info))
           (main (list 'complex-unify src body new-info)))
      (dolist (x tmp)
        (let ((form (cdr (assoc (car x) '((:my . let) (:my* . let*) 
                                          (:let . let) (:let* . let*))))))
          (when form 
            (setq main (list form (cdr x) main)))))
      (dolist (x (nreverse tmp) (setf (uinfo-all-vars new-info) all-vars))
        (case (car x)
          (:set (setq all-vars (cdr x)))
          (:add (setq all-vars (union (cdr x) all-vars)))
          (:del (setq all-vars (set-difference all-vars (cdr x))))
          ((:my :my*) (setq all-vars  (union (mapcar #'car (cdr x))
                                             all-vars)))))
      main)))


;; 注意！　この時点で完全な内部変数の捕捉が可能となる
(defun collect-vars-complexly (x info)
  (declare (special *avoid-vars*))
  (cond ((simple-vector-p x)
          (dotimes (i (length x))
            (collect-vars-complexly (svref x i) info)))
        ((symbolp x) (collect-variable-if-possible x))
        ((atom x)) ;;ignored
        ((not (structured? x))
          (do ((x x (cdr x)))
              ((atom x)
               (unless (null x) (collect-vars-complexly x info)))
            (collect-vars-complexly (car x) info)))
        ((member (car x) '(:here :do :save :call 
                           :unify :match :comp :comp-not
                           :return :subr |unif-quot|))) ;;ignored
        ;((eq :type (car x)) (dolist (e (cdr x)) (error "~D" e) (collect-vars-simply e info)))
        ((do-unify 
             x 
           (:case 
               (((:-> :eq :type) ?y) (:FOR y (:DO (collect-vars-simply _ info))))
             (((:-> :eq :define) ?y ?z)
               (:AND (:HERE (funcall 'valid-define-list? y))
                     (:FOR y (:EACH (:OR (:LIST* 
                                          (:-> :type symbol) 
                                          (:EACH (:DO (collect-vars-complexly _ info))))
                                         (:LIST* 
                                          (? . ?rest) 
                                          (:DO (collect-vars-complexly 
                                                `(:env ,@rest (:AND ,@_))
                                                info))))))
                     (:DO (collect-vars-complexly z info))))
               
             (((:-> :eq :for :with) ? . ?rest)
               (:for rest (:EACH (:do (collect-vars-complexly _ info)))))
             ((:and ((:-> :eq :->) . ?rest)
                    ;(:DO (setq *test* rest))

                    (:for rest (:append :maximize (:each (:-> :view (x) (when (and (symbolp x) (is-var? x)) (collect-variable-if-possible x) t))) ?r)))
               (:OR (:AND 
                      (:FOR r 
                          ((:-> :VIEW (s) (and (symbolp s)  (not (keywordp s)))) . ?y))
                      (:DO (dolist (x y) (collect-vars-complexly x info))))
                    (:and (:for r (:or ((:-> :eq :unify) ?y)
                                       (:append ((:-> :eq :to)) ? (?y))))
                          (:do (collect-vars-complexly y info)))))
             (((:-> :eq :case :only-case) . ?rest)
               (:for rest (:each (:each+ (:do (collect-vars-complexly _ info))))))
             
             ((:list* (:-> :eq :decomp) 
                      (:each (:list* ? 
                                     (:each (:do (collect-vars-complexly _ info)))))))

             ((:list* (:-> :eq :decomp*) 
                      (:append ?r (?z)))
               (:do (collect-vars-complexly z info))
               (:for r 
                   (:each (:list* ? 
                                  (:each (:do (collect-vars-complexly _ info)))))))

             ((:list* :env (:append ?r (?z)))
               (:with 
                (let (directive
                      (*avoid-vars* *avoid-vars*))
                  (declare (special *avoid-vars*)))
                (:and (:for r 
                          (:each 
                           (:and (:or (?y ?) ?y)
                                 (:here (symbolp y))
                                 (:do (cond ((and #!#>consp
                                                  (keywordp y))
                                              (setq directive y))
                                            ((member directive '(:my :my*))
                                              #{let ((q (var-to-?var y)))
                                              (push q *avoid-vars*)
                                              (push (var-to-?var q) *avoid-vars*)))))))
                      (:do (collect-vars-complexly z info)))))

             ((:list* :to (:-> :type proper-list))
               (:do (collect-vars-complexly #>lastcar  info)))

             (((:and (:-> :type keyword)
                     (:here #!#><reserved-unify-kwd?>))
               . ?)
               ;; マクロの場合
               (:do (collect-vars-complexly (<expand-unify-macro> x info) info)))

             ((? . ?r)
               (:do (do ((r r (cdr r)))
                        ((atom r)
                         (unless (null r) (collect-vars-complexly r info)))
                      (collect-vars-complexly (car r) info))))
               ;(:for r (:each (:-> :view (x) (collect-vars-complexly x info) t))))
             )))))

