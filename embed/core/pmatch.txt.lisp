;; -*- coding: utf-8 -*-

(DEFUN pmatch (V540 V541) (pmatch-help V540 V541 NIL))

(DEFUN pmatch-help (V554 V555 V556)
 (BLOCK NIL
  (IF (ABSEQUAL V555 V554) (RETURN V556)
   (TAGBODY
    (IF (CONSP V554)
     (LET ((Car571 (CAR V554)) (Cdr572 (CDR V554)))
      (TAGBODY
       (IF (CONSP V555)
        (IF (ABSEQUAL (CAR V555) Car571)
         (RETURN (pmatch-help Cdr572 (CDR V555) V556)) (GO tag560)))
       tag560
       (TAGBODY
        (IF (wrapper (THE SYMBOL (variable? Car571)))
         (IF
          (wrapper
           (succeeds?
            (SETQ *backtrack*
             (LET ((NilBind (nilbind Car571 V556)))
              (LET ((ValX (value-in Car571 NilBind)))
               (pmatch-help (rep Car571 Cdr572 ValX) V555 NilBind))))))
          (RETURN *backtrack*) (GO tag562)))
        tag562
        (TAGBODY
         (IF (CONSP V555)
          (IF (wrapper (THE SYMBOL (variable? Car571)))
           (IF
            (wrapper
             (succeeds?
              (SETQ *backtrack*
               (pmatch-help V554 (CDR V555)
                (consbind Car571 (CAR V555) V556)))))
            (RETURN *backtrack*) (GO tag564))
           (GO tag564)))
         tag564
         (IF (CONSP Car571)
          (IF (CONSP V555)
           (LET ((Car570 (CAR V555)))
            (IF (CONSP Car570)
             (RETURN
              (LET ((P (pmatch-help Car571 Car570 V556)))
               (IF (EQL P ) P (pmatch-help Cdr572 (CDR V555) P))))
             (GO tag559)))
           (GO tag559))
          (GO tag559)))))))
    tag559 (RETURN (esc))))))

(DEFUN nilbind (V573 V574)
 (BLOCK NIL
  (IF (NULL V574) (RETURN (CONS (CONS V573 (CONS NIL NIL)) NIL))
   (TAGBODY
    (IF (CONSP V574)
     (LET ((Car582 (CAR V574)))
      (IF (CONSP Car582)
       (LET ((Cdr581 (CDR Car582)))
        (IF (CONSP Cdr581)
         (IF (NULL (CDR Cdr581))
          (IF (ABSEQUAL (CAR Car582) V573) (RETURN V574) (GO tag576))
          (GO tag576))
         (GO tag576)))
       (GO tag576))))
    tag576
    (IF (CONSP V574) (RETURN (CONS (CAR V574) (nilbind V573 (CDR V574))))
     (RETURN (f_error 'nilbind)))))))

(DEFUN consbind (V583 V584 V585)
 (BLOCK NIL
  (IF (NULL V585) (RETURN (CONS (CONS V583 (CONS (CONS V584 NIL) NIL)) NIL))
   (TAGBODY
    (IF (CONSP V585)
     (LET ((Car594 (CAR V585)))
      (IF (CONSP Car594)
       (LET ((Car592 (CAR Car594)) (Cdr593 (CDR Car594)))
        (IF (CONSP Cdr593)
         (IF (NULL (CDR Cdr593))
          (IF (ABSEQUAL Car592 V583)
           (RETURN
            (CONS
             (CONS Car592 (CONS (APPEND (CAR Cdr593) (CONS V584 NIL)) NIL))
             (CDR V585)))
           (GO tag587))
          (GO tag587))
         (GO tag587)))
       (GO tag587))))
    tag587
    (IF (CONSP V585) (RETURN (CONS (CAR V585) (consbind V583 V584 (CDR V585))))
     (RETURN (f_error 'consbind)))))))

(DEFUN rep (V599 V600 V601)
 (BLOCK NIL
  (IF (NULL V600) (RETURN NIL)
   (TAGBODY
    (IF (CONSP V600)
     (LET ((Car605 (CAR V600)))
      (IF (ABSEQUAL Car605 V599)
       (RETURN (APPEND V601 (rep Car605 (CDR V600) V601))) (GO tag603))))
    tag603
    (IF (CONSP V600) (RETURN (CONS (CAR V600) (rep V599 (CDR V600) V601)))
     (RETURN (f_error 'rep)))))))

(DEFUN value-in (V606 V607) (head (tail (assoc V606 V607))))
