(oleo.core:oleo-core-header)
(oleo.base:define-package #:cluw-user ()
  (:use :cl :cl-user :cluw)
  )

(in-package :cluw-user)


@later! (defun bazboz)

#C

(defun bool-rand () (zerop (random 2)))



(defun brnd (&optional n) (if n (freplicate n #'brnd) (zerop (random 2))))
(defun product (b1 b2) (if b1 (if b2 (complex 1 0) (complex -1 0)) (if b2 (complex 0 1) (complex 0 -1))))
(reduce '+ (mapcar #'product (brnd 10000) (brnd 10000)))
(+ 250 500 750)

(+ (complex 1 0) (complex 0 -1))

     1
 -1     1
    -1

(defun rnd*
(freplicate 10 'brnd)

(cluw:

 (let* ((s (read-file-into-string "u:/files"))
        (lines (split-sequence #\Newline s))
        tmp)
   (dolist (x lines)
     (let* ((vec (oleo.base.text::text-scan  "(.*?)-(.*)" x :output :sub-strings-as-list))
            (author (first vec))
            (title (second vec)))
       (unless (member author tmp :test 'equalp)
         (push author tmp))))
   (write-string-into-file 
    (string-join (mapcar (lambda (a f) (format nil "move \"~A\"~%" a)) tmp))
    "u:/bat.bat"))

;;;;
(let* ((s (read-file-into-string "u:/files"))
        (lines (split-sequence #\Newline s))
        tmp)
   (dolist (x lines)
     (let* ((vec (oleo.base.text::text-scan  "(.*?)-(.*)" x :output :sub-strings-as-list))
            (author (first vec))
            (title (second vec)))
       (push (format nil "move \"~A\" \"~A\\[~A] ~A\"~%" x author author title)
             tmp)))

       (write-string-into-file (string-join tmp)
                               "u:/bat2.bat"
                               :if-exists :allow))

 
  

 
 
(oleo.base.text::text-scan  "(.*)-(.*)" "a-aa" :output :sub-strings)
 
