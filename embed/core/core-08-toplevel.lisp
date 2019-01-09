; Beginning of Licence
;
; This software is licensed only for personal and educational use and
; not for the production of commercial software.  Modifications to this
; program are allowed but the resulting source must be annotated to
; indicate the nature of and the author of these changes.  
;
; Any modified source is bound by this licence and must remain available 
; as open source under the same conditions it was supplied and with this 
; licence at the top.

; This software is supplied AS IS without any warranty.  In no way shall 
; Mark Tarver or Lambda Associates be held liable for any damages resulting 
; from the use of this program.

; The terms of these conditions remain binding unless the individual 
; holds a valid license to use Qi commercially.  This license is found 
; in the final page of 'Functional Programming in Qi'.  In that event 
; the terms of that license apply to the license holder. 
;
; (c) copyright Mark Tarver, 2008
; End of Licence

(eval-when (:compile-toplevel :load-toplevel :execute)
  (SETF *READTABLE* (COPY-READTABLE *READTABLE*))
  (SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
  )

(IN-PACKAGE :OLEO.EMBED.CORE)

(DEFUN qi ()
 (PROG ()
       (credits)
       LOOP    
       (initialise_environment)
       (prompt) 
       (FORCE-OUTPUT)
       (HANDLER-CASE 
          (read-evaluate-print) (ERROR (condition) (PRINC condition)))
       (GO LOOP)))

(DEFUN credits ()
  (FORMAT T "~%Qi II 2008, Copyright (C) 2001-2008 Mark Tarver~%")
  (output "www.lambdassociates.org~%") (output "~A~%" *version*))

(SETQ *version* "version 1.06")

(DEFUN initialise_environment NIL
   (SETQ *call* 0) 
   (SETQ *inferences* 0)
   (IF (BOUNDP '*tempsigs*) (MAPC 'destroy *tempsigs*)) 
   (SETQ *tempsigs* NIL))

(SETQ *history* NIL)

(DEFUN read-evaluate-print NIL
 (LET ((Lineread (toplineread)))
  (LET ((History (value '*history*)))
   (LET ((NewLineread (retrieve-from-history-if-needed Lineread History)))
    (LET ((NewHistory (update_history NewLineread History)))
     (LET ((Parsed (fst NewLineread))) (toplevel Parsed)))))))

(DEFUN toplineread () (toplineread_loop (READ-CHAR) NIL))

(DEFUN toplineread_loop (V94 V95)
 (COND ((EQL #\^ V94) (error "line read aborted"))
  ((wrapper (element? V94 (LIST #\Newline #\Return)))
   (LET ((Line (compile '<st_input> V95)))
    (if (THE SYMBOL (or (qi_= Line 'fail!) (THE SYMBOL (empty? Line))))
     (toplineread_loop (READ-CHAR) (APPEND V95 (LIST V94))) (@p Line V95))))
  (T (toplineread_loop (READ-CHAR) (APPEND V95 (LIST V94))))))

(DEFUN tc (V101)
 (COND ((EQ '+ V101) (SETQ *tc* 'true)) ((EQ '- V101) (SETQ *tc* 'false))
  (T (error "tc expects a + or -"))))

(tc '-)

(DEFUN prompt NIL
 (if *tc* (FORMAT T "~%~%(~A+) " (LIST-LENGTH *history*))
  (FORMAT T "~%~%(~A-) " (LIST-LENGTH *history*))))

(DEFUN toplevel (V19) (toplevel_evaluate V19 *tc*))

(DEFUN update_history (V17 V18) (SETQ *history* (CONS V17 V18)))

(DEFUN retrieve-from-history-if-needed
 (V15 V16)
 (COND
  ((AND (TUPLE-P V15) (CONSP (snd V15))
    (EQL #\! (CAR (snd V15)))
    (CONSP (CDR (snd V15)))
    (EQL #\! (CAR (CDR (snd V15))))
    (NULL (CDR (CDR (snd V15))))
    (CONSP V16))
   (FORMAT T "~{~C~}~%" (snd (CAR V16)))
   (CAR V16))
  ((AND (TUPLE-P V15) (CONSP (snd V15))
    (EQL #\! (CAR (snd V15))))
   (LET
    ((Key?
      (make-key (CDR (snd V15))
       V16)))
    (LET
     ((Find
       (head (find-past-inputs Key?
        V16))))
     (LET
      ((PastPrint
        (FORMAT 'T "~{~C~}~%" (snd Find))))
      Find))))
  ((AND (TUPLE-P V15) (CONSP (snd V15))
    (EQL #\% (CAR (snd V15)))
    (NULL (CDR (snd V15))))
   (print-past-inputs #'(LAMBDA (X) 'true)
     (reverse V16) 0)
    (abort))
  ((AND (TUPLE-P V15) (CONSP (snd V15))
    (EQL #\% (CAR (snd V15))))
   (LET
    ((Key?
      (make-key (CDR (snd V15))
       V16)))
    (LET
     ((Pastprint
       (print-past-inputs Key?
        (reverse V16) 0)))
     (abort))))
  (T V15)))

(DEFUN find-past-inputs (V124 V125)
 (LET ((F (find V124 V125)))
  (if (THE SYMBOL (empty? F)) (error "input not found~%") F)))

(DEFUN make-key (V127 V128)
 (LET ((N (implode V127)))
  (if (THE SYMBOL (integer? N)) #'(LAMBDA (X) (qi_= X (NTH N (REVERSE V128))))
   #'(LAMBDA (X) (prefix? V127 (THE LIST (trim-gubbins (snd X))))))))

(DEFUN trim-gubbins (V2123)
 (COND
  ((AND (CONSP V2123) (EQL #\Space (CAR V2123))) (trim-gubbins (CDR V2123)))
  ((AND (CONSP V2123) (EQL #\Newline (CAR V2123))) (trim-gubbins (CDR V2123)))
  ((AND (CONSP V2123) (EQL #\Return (CAR V2123))) (trim-gubbins (CDR V2123)))
  ((AND (CONSP V2123) (EQL #\Tab (CAR V2123))) (trim-gubbins (CDR V2123)))
  ((AND (CONSP V2123) (EQL #\( (CAR V2123))) (trim-gubbins (CDR V2123)))
  (T V2123)))

(DEFUN find (V139 V140)
 (COND ((NULL V140) NIL)
  ((AND (CONSP V140) (wrapper (apply V139 (CAR V140))))
   (cons (CAR V140) (find V139 (CDR V140))))
  ((CONSP V140) (find V139 (CDR V140))) (T (f_error 'find))))

(DEFUN implode (V141) (READ-FROM-STRING (COERCE V141 'STRING)))

(DEFUN prefix? (V153 V154)
 (COND ((NULL V153) 'true)
  ((AND (CONSP V153) (CONSP V154) (ABSEQUAL (CAR V153) (CAR V154)))
   (prefix? (CDR V153) (CDR V154)))
  (T 'false)))

(DEFUN print-past-inputs (V494 V495 V496)
 (COND ((NULL V495) '_)
  ((AND (CONSP V495) (wrapper (not (apply V494 (CAR V495)))))
   (print-past-inputs V494 (CDR V495) (1+ V496)))
  ((AND (CONSP V495) (TUPLE-P (CAR V495)))
    (FORMAT T "~A. ~{~C~}~%" V496 (snd (CAR V495)))
    (print-past-inputs V494 (CDR V495) (1+ V496)))
  (T (implementation_error 'print-past-inputs))))

(DEFUN toplevel_evaluate (V182 V183)
 (COND ((AND (CONSP V182) (EQ 'false V183)) (print (eval (CAR V182))))
    ((AND (CONSP V182) (CONSP (CDR V182)) (EQ '|:| (CAR (CDR V182)))
    (CONSP (CDR (CDR V182))) (NULL (CDR (CDR (CDR V182)))))
   (typecheck-and-evaluate (CAR V182)
    (normalise-type (curry-type (CAR (CDR (CDR V182)))))))
  ((CONSP V182) 
    ;; HACK
    ;;(WARN "toplevel_evaluate ~W" (CAR V182))
    (typecheck-and-evaluate (CAR V182) (gensym "A")))
  (T (implementation_error 'toplevel_evaluate))))

;; (DEFUN typecheck-and-evaluate (V194 V195)
;;  (LET ((Typecheck (statictypecheck NIL V194 V195)))
;;   (IF (EQ Typecheck 'false) (ERROR "type error~%")
;;    (PROGN (print (eval V194)) (FORMAT T " : ~A" (pretty-type Typecheck))))))

;; 2015-8-29 JUN Hacked Added
(DEFVAR *load-eval-type-pairs*)

;; 2015-8-29 JUN Hacked
(DEFUN typecheck-and-evaluate (V194 V195)
 (LET ((Typecheck (statictypecheck NIL V194 V195)))
  (IF (EQ Typecheck 'false) 
    (ERROR "type error~%")
    (LET ((EV (eval V194))
          (PT (pretty-type Typecheck)))
      (PUSH (LIST EV PT)
            *load-eval-type-pairs*)))))

(DEFUN pretty-type (V197)
 (mult_subst *alphabet* (extract-vars V197) V197))

(DEFUN mult_subst (V202 V203 V204)
 (COND ((NULL V202) V204) ((NULL V203) V204)
  ((AND (CONSP V202) (CONSP V203))
   (mult_subst (CDR V202) (CDR V203) (subst (CAR V202) (CAR V203) V204)))
  (T (implementation_error 'mult_subst))))
