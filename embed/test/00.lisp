
(in-package :qi-test-package)
(cluw:common-header :qi)

(assert (print (\Qi (+ 1 2).)))
(assert (print (\Qi+ (+ 1 2).)))

(assert (eql (symbol-package '~hello)
             (find-package "Q")))

(assert (print '~haael))
(print 'DONE-QI)

(print (\Qi av .))


(defmacro λ ((&rest ll) &body body)
  `(lambda ,ll ,@body))

(funcall (λ (x) (list x)) 100)

       
(defun どやねん＠ (x) (print (list x x)))

(どやねん＠ '＠あほんだら多変量解析＠)






