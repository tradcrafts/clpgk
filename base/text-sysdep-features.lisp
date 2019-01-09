;; -*- coding: utf-8 -*-
(oleo.core:oleo-core-header)

(in-package :oleo.base.text)

;; `+<charset-to-external-format-conversion-table>+' は定数である
;; 環境非依存のエンコーディング定数とCL実装異存のエンコーディング定数の対応表
;; 実装異存のエンコーディング定数がエイリアスをもっている場合には、全てをリストで列挙する

#+sbcl
(defvar +<external-format-conversion-table>+
  '((:LATIN1 :LATIN1)
    (:ASCII  :ASCII)
    (:UTF8   (:UTF-8 :UTF8))
    (:SJIS   (:SJIS :CP932))
    (:EUCJP  :EUCJP)))

#+ccl
(defconstant +<external-format-conversion-table>+
  '((:LATIN1 (:ISO-8859-1 :ISO_8859-1 :LATIN1 :L1 :IBM819 :CP819 :CSISOLATIN1))
    (:ASCII  (:US-ASCII :CSASCII :CP637 :IBM637 :US :ISO646-US :ASCII :ISO-IR-6))
    (:UTF8   :UTF-8)
    (:SJIS   (:WINDOWS-31J :CP932))
    (:EUCJP  (:EUC-JP :EUCJP))))



(defun <external-format-alist> ()
  (flet ((to-alist (conversion-table)
           (mapcan (lambda (x &aux (fst (first x)) (snd (second x)))
                     (mapcar (lambda (x) (cons fst x))
                             (ensure-list snd)))
                   conversion-table)))
    (memoized-with-checking (to-alist +<external-format-conversion-table>+)
                            +<external-format-conversion-table>+)))

(defun to-external-format (ident)
  (when ident
    (aif (assoc ident (<external-format-alist>))
         (cdr it)
         (error "TO-EXTERNAL-FORMAT: invalid identifier ~A" ident))))

(defun from-external-format (ident)
  (when ident
    (aif (rassoc ident (<external-format-alist>))
         (car it)
         (error "FROM-EXTERNAL-FORMAT: invalid identifier ~A" ident))))

#Comment
;(<external-format-alist>)


;;(to-external-format :sjis)
;;(from-external-format :cp932)
