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

(DEFUN print (X) (output "~S" X) X)

(DEFUN output (&REST ARGS) 
  (FORMAT T (rectify_string (APPLY 'FORMAT (CONS NIL (MAPCAR 'user-syntax-out ARGS)))))
  (FORCE-OUTPUT)
  "done")

(DEFUN output-math (&REST ARGS) 
  (PROGV '(*output-math*) '(T)
   (FORMAT T (rectify_string (APPLY 'FORMAT (CONS NIL (MAPCAR 'user-syntax-out ARGS))))))
  (FORCE-OUTPUT)
  "done")

(DEFUN user-syntax-out (X)
  (apply-user-syntax X *syntax-out*))

(SETQ *syntax-out* NIL)

(DEFUN syntax-out (F N)
 (SETQ *syntax-out* (set-user-syntax F N *syntax-out*)))

(DEFUN error (&REST ARGS) 
  (LET ((String (rectify_string (APPLY 'FORMAT 
                                        (CONS NIL (MAPCAR 'user-syntax-out ARGS))))))
       (ERROR (FORMAT NIL "error: ~A" String))))


(DEFUN make-string (String &REST ARGS)
  (LET ((RectArgs (MAPCAR 'rectify_arg ARGS))) 
       (APPLY 'FORMAT (CONS NIL (CONS String RectArgs)))))

(DEFUN rectify_arg (X)
  (IF (STRINGP X)
      X 
      (COERCE (compile '<st_output> (explode X)) 'STRING)))

(DEFUN rectify_string (X)
  (COERCE (compile '<st_output> (COERCE X 'LIST)) 'STRING))   

(DEFUN rep-~ (V75)
 (COND ((NULL V75) NIL)
  ((AND (CONSP V75) (EQL #\~ (CAR V75)))
   (CONS #\~ (CONS #\~ (rep-~ (CDR V75)))))
  ((CONSP V75) (CONS (CAR V75) (rep-~ (CDR V75)))) (T (f_error 'rep-~))))

(DEFUN <st_output> (Stream)
 (OR
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\"))
    (LET ((<string> (<string> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <string>))
      (IF (AND (CONSP (FIRST <string>)) (EQL (FIRST (FIRST <string>)) #\"))
       (LET
        ((<st_output>
          (<st_output> (LIST (REST (FIRST <string>)) (SECOND <string>)))))
        (IF (NOT (failure? <st_output>))
         (LIST (FIRST <st_output>)
          (CONS #\"
           (APPEND (rep-~ (SECOND <string>)) (CONS #\" (SECOND <st_output>)))))
         NIL))
       NIL)
      NIL))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\())
    (IF
     (AND (CONSP (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
      (EQL (FIRST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))) #\@))
     (IF
      (AND
       (CONSP
        (FIRST
         (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
          (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream))))))
       (EQL
        (FIRST
         (FIRST
          (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
           (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream))))))
        #\p))
      (LET
       ((<st_output1>
         (<st_output1>
          (LIST
           (REST
            (FIRST
             (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
              (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream))))))
           (SECOND
            (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
             (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream)))))))))
       (IF (NOT (failure? <st_output1>))
        (IF
         (AND (CONSP (FIRST <st_output1>))
          (EQL (FIRST (FIRST <st_output1>)) #\)))
         (LET
          ((<st_output2>
            (<st_output2>
             (LIST (REST (FIRST <st_output1>)) (SECOND <st_output1>)))))
          (IF (NOT (failure? <st_output2>))
           (LIST (FIRST <st_output2>)
            (CONS #\(
             (CONS #\@
              (CONS #\p
               (APPEND (SECOND <st_output1>)
                (CONS #\) (SECOND <st_output2>)))))))
           NIL))
         NIL)
        NIL))
      NIL)
     NIL)
    NIL))
  (BLOCK localfailure
   (LET ((<nil> (<nil> Stream)))
    (IF (NOT (failure? <nil>))
     (LET ((<st_output> (<st_output> <nil>)))
      (IF (NOT (failure? <st_output>))
       (LIST (FIRST <st_output>)
        (CONS (lparen) (CONS (rparen) (SECOND <st_output>))))
       NIL))
     NIL)))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\())
    (LET
     ((<st_output1>
       (<st_output1> (LIST (REST (FIRST Stream)) (SECOND Stream)))))
     (IF (NOT (failure? <st_output1>))
      (IF
       (AND (CONSP (FIRST <st_output1>))
        (EQL (FIRST (FIRST <st_output1>)) #\)))
       (LET
        ((<st_output2>
          (<st_output2>
           (LIST (REST (FIRST <st_output1>)) (SECOND <st_output1>)))))
        (IF (NOT (failure? <st_output2>))
         (LIST (FIRST <st_output2>)
          (APPEND (CONS (lparen) (SECOND <st_output1>))
           (CONS (rparen) (SECOND <st_output2>))))
         NIL))
       NIL)
      NIL))
    NIL))
  (BLOCK localfailure
   (LET ((<special> (<special> Stream)))
    (IF (NOT (failure? <special>))
     (LET ((<st_output> (<st_output> <special>)))
      (IF (NOT (failure? <st_output>))
       (LIST (FIRST <st_output>)
        (APPEND (SECOND <special>) (SECOND <st_output>)))
       NIL))
     NIL)))
  (BLOCK localfailure
   (LET ((<output_token> (<output_token> Stream)))
    (IF (NOT (failure? <output_token>))
     (LET ((<st_output> (<st_output> <output_token>)))
      (IF (NOT (failure? <st_output>))
       (LIST (FIRST <st_output>)
        (CONS (SECOND <output_token>) (SECOND <st_output>)))
       NIL))
     NIL)))
  (BLOCK localfailure
   (LET ((<e> (<e> Stream)))
    (IF (NOT (failure? <e>)) (LIST (FIRST <e>) NIL) NIL)))))

(DEFUN lparen () (IF (BOUNDP '*output-math*) #\( #\[))

(DEFUN rparen () (IF (BOUNDP '*output-math*) #\) #\]))

(DEFUN <nil> (Stream)
 (OR
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\N))
    (IF
     (AND (CONSP (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
      (EQL (FIRST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))) #\I))
     (IF
      (AND
       (CONSP
        (FIRST
         (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
          (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream))))))
       (EQL
        (FIRST
         (FIRST
          (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
           (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream))))))
        #\L))
      (LIST
       (FIRST
        (LIST
         (REST
          (FIRST
           (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
            (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream))))))
         (SECOND
          (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
           (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream)))))))
       (CONS #\N (CONS #\I (CONS #\L NIL))))
      NIL)
     NIL)
    NIL))))

(DEFUN <output_token> (Stream)
 (OR
  (BLOCK localfailure
   (IF (CONSP (FIRST Stream))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
     (if (element? (CAAR Stream) (CONS #\( (CONS #\) NIL)))
      (RETURN-FROM localfailure NIL) (CAAR Stream)))
    NIL))))

(DEFUN <st_output1> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<st_output> (<st_output> Stream)))
    (IF (NOT (failure? <st_output>)) <st_output> NIL)))))

(DEFUN <st_output2> (Stream)
 (OR
  (BLOCK localfailure
   (LET ((<st_output> (<st_output> Stream)))
    (IF (NOT (failure? <st_output>)) <st_output> NIL)))))

(DEFUN <special> (Stream)
 (OR
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\~))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))
     (CONS #\~ (CONS #\~ NIL)))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\q))
    (IF
     (AND (CONSP (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
      (EQL (FIRST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))) #\i))
     (IF
      (AND
       (CONSP
        (FIRST
         (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
          (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream))))))
       (EQL
        (FIRST
         (FIRST
          (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
           (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream))))))
        #\_))
      (LET
       ((<comparator>
         (<comparator>
          (LIST
           (REST
            (FIRST
             (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
              (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream))))))
           (SECOND
            (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
             (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream)))))))))
       (IF (NOT (failure? <comparator>)) <comparator> NIL))
      NIL)
     NIL)
    NIL))))

(DEFUN <comparator> (Stream)
 (OR
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\=))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (CONS #\= NIL))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\>))
    (IF
     (AND (CONSP (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
      (EQL (FIRST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))) #\=))
     (LIST
      (FIRST
       (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
        (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream)))))
      (CONS #\> (CONS #\= NIL)))
     NIL)
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\<))
    (IF
     (AND (CONSP (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
      (EQL (FIRST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream)))) #\=))
     (LIST
      (FIRST
       (LIST (REST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))))
        (SECOND (LIST (REST (FIRST Stream)) (SECOND Stream)))))
      (CONS #\< (CONS #\= NIL)))
     NIL)
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\>))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (CONS #\> NIL))
    NIL))
  (BLOCK localfailure
   (IF (AND (CONSP (FIRST Stream)) (EQL (FIRST (FIRST Stream)) #\<))
    (LIST (FIRST (LIST (REST (FIRST Stream)) (SECOND Stream))) (CONS #\< NIL))
    NIL))))
