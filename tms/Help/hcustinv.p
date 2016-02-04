/* ------------------------------------------------------
  MODULE .......: hcustinv.p
  FUNCTION .....: help module for customers bills
  APPLICATION ..: TMS
  AUTHOR .......: TK
  CREATED ......: 02-07-02
  CHANGED ......: 12.11.02 jr Removed f7 old memo
                  12.11.02 jr "nnlaskus" => "invoice" in memo
                  12.09.03/aam brand,
                               other tuning
                  06.02.04 jp  CustNum for memo             
                  15.04.04/aam index CustName removed
                  20.03.06/aam use proper index for find etc.
                  18.04.06/aam use payments.p instead of nnlasu.p
                  16.06.06/aam ClaimState instead of ClaimQty
                  22.03.07 kl  new param for RUN Ar/payments

  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEFINE INPUT PARAMETER CustNum LIKE Customer.CustNum.
DEFINE INPUT PARAMETER paid    AS LOG.

DEF NEW shared VAR order AS INT NO-UNDO.
DEF shared VAR siirto AS CHAR.

DEF VAR memory          AS RECID                NO-UNDO.
def var line            as int format "99"      NO-UNDO.
DEF VAR must-print      AS LOG                  NO-UNDO.
DEF VAR must-add        AS LOG                  NO-UNDO.
DEF VAR ufkey           AS LOG                  NO-UNDO.
DEF VAR rtab            AS RECID EXTENT 24      NO-UNDO.
DEF VAR i               AS INT                  NO-UNDO.
DEF VAR xrecid          AS RECID.
DEF VAR ex-order        AS INT                  NO-UNDO.
DEF VAR firstline       AS INT                  NO-UNDO.
DEF VAR BalDue         LIKE Invoice.InvAmt      NO-UNDO.
def var notes           as lo format "MEMO/"    NO-UNDO.
DEF VAR lcCustName      AS CHAR                 NO-UNDO.

form
    Invoice.InvNum                  /* column-label "Invoice"         */
    Invoice.InvDate                 /* column-label "Date"            */
    Invoice.DueDate                 /* column-label "Dueday"          */
    Invoice.InvAmt                  COLUMN-LABEL "Inv.Amount"
    BalDue                          COLUMN-LABEL "Debt"     
    Invoice.ClaimState              COLUMN-LABEL "Claimed" FORMAT ">9.9<"
    Invoice.InvType                 COLUMN-LABEL "Type"
    notes                           column-label "Memo"
WITH CENTERED OVERLAY scroll 1 10 DOWN ROW 3
    COLOR value(cfc) 
    TITLE COLOR value(ctc)
       " CHOOSE INVOICE " +
       " (" + STRING(CustNum) + " " + lcCustName + ") "
    FRAME sel.

FIND Customer WHERE Customer.CustNum = CustNum NO-LOCK.
lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                              BUFFER Customer).

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.


RUN local-find-first.

IF AVAILABLE Invoice THEN DO:
   ASSIGN
   memory = recid(Invoice)
   must-print = TRUE.
END.
ELSE DO:
   MESSAGE "No invoices available for choosing"
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.

ASSIGN xrecid = ? ufkey = TRUE firstline = 0 order = 1.

LOOP:
repeat WITH FRAME sel ON ENDKEY UNDO LOOP, NEXT LOOP:

   print-line:
   DO :
      IF must-print THEN DO:
         IF order <> ex-order THEN DO:
           ex-order = order.
         END.

         up FRAME-LINE - 1.
         FIND Invoice where recid(Invoice) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory */

         repeat WITH FRAME sel:
            IF AVAILABLE Invoice THEN DO:

               RUN local-find-others.

               RUN local-disp-row.

               rtab[FRAME-LINE] = recid(Invoice).
                  
               RUN local-find-next.
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.

         up FRAME-LINE - 1.
         DOWN firstline.
         ASSIGN firstline = 0.
         ASSIGN must-print = FALSE.
         PAUSE 0 no-message.

         /* one page of data has been Printed AND
         mAllA linellA choosea odotellen. */
      END. /* must-print = TRUE */
   END. /* print-line */

   BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         ufk   = 0
         ufk[3]= 927 
         ufk[4]= 829
         ufk[5]= 11  
         ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         {Syst/uright1.i '"3,4,7"'}
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW Invoice.InvNum ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Invoice.InvNum WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         message "YOU ARE ON AN EMPTY ROW, MOVE UPWARDS !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      if nap = "cursor-right" THEN DO:
         order = order + 1.
         IF order = 2 THEN order = 1.
      END.
      if nap = "cursor-left" THEN DO:
         order = order - 1.
         IF order = 0 THEN order = 1.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN
         firstline = 0
         memory = rtab[FRAME-LINE].
         FIND Invoice where recid(Invoice) = memory NO-LOCK .
         DO i = 1 TO FRAME-LINE - 1:

            RUN local-find-prev.
            IF AVAILABLE Invoice THEN DO:
               ASSIGN
               firstline = i
               memory = recid(Invoice).
            END.
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND Invoice where recid(Invoice) = rtab[1] no-lock.
            RUN local-find-prev.
            IF NOT AVAILABLE Invoice THEN DO:
               message "This is the 1st row !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.

               RUN local-find-others.

               RUN local-disp-row.

               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(Invoice)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:

         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND Invoice where recid(Invoice) = rtab[FRAME-DOWN] no-lock .
            
            RUN local-find-next.
            IF NOT AVAILABLE Invoice THEN DO:
               message "This is the last row !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* was found vielA seuraava tietue */
               scroll up.

               RUN local-find-others.

               RUN local-disp-row.

               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(Invoice).
               /* ja lopuksi pannaan memoryin ylimmAn linen avain */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND Invoice where recid(Invoice) = memory no-lock no-error.
         
         RUN local-find-prev.

         IF AVAILABLE Invoice THEN DO:
            memory = recid(Invoice).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               
               RUN local-find-prev.

               IF AVAILABLE Invoice THEN memory = recid(Invoice).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "this is the first page !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:

        /* cursor TO the downmost line */

        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "This is the last page !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND Invoice where recid(Invoice) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Hakurutiini */
     else if lookup(nap,"1,f1") > 0 THEN DO:
        ex-order = order.
        ASSIGN order = 1.
        RUN Ar/nnlaha.p.
        ASSIGN ufkey = TRUE.
        IF si-recid <> ? THEN
          ASSIGN memory = si-recid
                  must-print = TRUE.
        ELSE ASSIGN
             order = ex-order
             must-print = FALSE.
        cfc = "sel". RUN Syst/ufcolor. ccc = cfc.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"3,f3") >0 THEN DO TRANS: /* memo */
        FIND Invoice WHERE recid(Invoice) = rtab;<frame-line;>
        NO-LOCK NO-ERROR.
        RUN Mc/memo(INPUT Invoice.CustNum,
                 INPUT "invoice",
                 INPUT STRING(Invoice.InvNum),
                 INPUT "Invoice number").
        ufkey = TRUE.
        NEXT.
     END.


     else if lookup(nap,"f4,4") > 0 THEN DO:
        FIND Invoice where recid(Invoice) = rtab[FRAME-LINE] no-lock no-error.
        RUN Ar/payments(0,Invoice.InvNum,"").
        ufkey = TRUE.
        NEXT.
     END.

     else if lookup(nap,"enter,return,5,f5") > 0 THEN DO : /* valitaan tAmA */
        FIND Invoice where recid(Invoice) = rtab[FRAME-LINE] no-lock no-error.
        siirto = string(Invoice.InvNum).
        LEAVE LOOP.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
        RUN local-find-first.
        ASSIGN
        memory = recid(Invoice)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        
        RUN local-find-last.

        ASSIGN
        memory = recid(Invoice)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-find-others:

   RUN Ar/invbal(Invoice.InvNum, OUTPUT BalDue).

   FIND FIRST memo WHERE
              Memo.Brand = Invoice.Brand AND
              Memo.HostTable = "Invoice" AND
              Memo.KeyValue  = STRING(Invoice.InvNum)
   NO-LOCK NO-ERROR.

   IF AVAIL memo AND MemoText NE ""
   THEN notes = TRUE. 
   ELSE notes = FALSE.

END PROCEDURE.

PROCEDURE local-disp-row:

   DISPLAY Invoice.InvNum Invoice.InvDate
           Invoice.DueDate Invoice.InvAmt BalDue notes
           Invoice.ClaimState
           Invoice.InvType
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-first:

   FIND FIRST Invoice where 
              Invoice.Brand   = gcBrand AND
              Invoice.CustNum = CustNum AND
             (IF paid = false THEN Invoice.PaymState < 2 ELSE TRUE) AND
             Invoice.InvType < 3
   USE-INDEX CustNum NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-last:

   FIND LAST Invoice where 
             Invoice.Brand   = gcBrand AND
             Invoice.CustNum = CustNum AND
             (IF paid = false THEN Invoice.PaymState < 2 ELSE TRUE) AND
             Invoice.InvType < 3
   USE-INDEX CustNum NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-next:

   FIND NEXT Invoice where 
             Invoice.Brand   = gcBrand AND
             Invoice.CustNum = CustNum AND
             (IF paid = false THEN Invoice.PaymState < 2 ELSE TRUE) AND
             Invoice.InvType < 3
   USE-INDEX CustNum NO-LOCK NO-ERROR.

END PROCEDURE.
                    
PROCEDURE local-find-prev:

   FIND PREV Invoice where 
             Invoice.Brand   = gcBrand AND
             Invoice.CustNum = CustNum AND
             (IF paid = false THEN Invoice.PaymState < 2 ELSE TRUE) AND
             Invoice.InvType < 3
   USE-INDEX CustNum NO-LOCK NO-ERROR.

END PROCEDURE.


