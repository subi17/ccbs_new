/* ----------------------------------------------------------------------------
  MODULE .......: nnlase.p
  FUNCTION .....: Avoimien laskujen BROWSE
  APPLICATION ..: VP
  AUTHOR .......: NN
  CREATED ......: 12-07-91
  CHANGED ......: 13.10.98 pt  disp "notes", english Version
                  25.05.99 jp  uright1 & uright2 added  
                  06.04.00 pt  RUN Ar/invbal.p
                  20.05.02/tk  RUN Mc/memo.p
                  14.10.02/aam disp Currency
                  01.11.02/aam PaymState added
                  12.11.02 jr  "Invoice" => "invoice" in memo
                  12.11.02 jr Removed f7 old memo
                  10.12.02 lp siirto = ?
                  15.09.03/aam brand
                  06.02.04 jp  CustNum for memo
                  09.02.04/aam parameters from gcHelpParam
                  14.04.04/aam index CustName replaced with CustNum 
                  18.04.06/aam use payments.p instead of nnlasu.p
                  22.03.07 kl  new param for RUN Ar/payments.p

  Version ......: M15
  -------------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable Invoice

{Syst/commali.i}

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
DEF VAR BalDue          LIKE Invoice.InvAmt     NO-UNDO.
def var notes           as lo format "M/"       NO-UNDO.

DEF VAR liInvCust       AS INT                  NO-UNDO.
DEF VAR liInvNum        AS INT                  NO-UNDO.
DEF VAR lcExtInvID      AS CHAR                 NO-UNDO.
DEF VAR ldtInvDate      AS DATE                 NO-UNDO.
DEF VAR liCustNum       AS INT                  NO-UNDO. 
DEF VAR llOnlyOpen      AS LOG                  NO-UNDO. 

form
    Invoice.ExtInvID  FORMAT "X(12)"
    Invoice.InvNum    COLUMN-LABEL "Syst.Nbr"
    Invoice.CustNum   format ">>>>>>>9" column-label "Customer"
    Invoice.CustName  format "x(10)"    column-label "Name" 
    Invoice.InvDate   column-label "Date"  
    Invoice.InvAmt    FORMAT "->>>>>>9.99"  column-label "Amount"
    BalDue            FORMAT "->>>>>>9.99"  column-label "Balance"     
    Invoice.PaymState FORMAT ">"            column-label "P"
    notes                                   column-label "M"
WITH
    width 80 OVERLAY scroll 1 14 DOWN ROW 2 centered
    COLOR value(cfc) TITLE COLOR value(ctc)
    " INVOICES  (" + gcBrand + ") "
    FRAME sel.

form 
    "Invoice:" lcExtInvID  FORMAT "X(12)" 
    help "Invoice number"    
    with row 4 col 2 title color value(ctc) " FIND INVOICE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME F1.

form 
    "System Invoice:" liInvNum FORMAT ">>>>>>>9" 
    help "System invoice number"
    with row 4 col 2 title color value(ctc) " FIND SYST INVOICE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME F2.

form 
    "Customer:" liCustNum FORMAT ">>>>>>>9" 
    help "Customer number"
    with row 4 col 2 title color value(ctc) " FIND CUSTOMER"
    COLOR value(cfc) NO-LABELS OVERLAY FRAME F3.

form 
    "Invoice Date:" ldtInvDate FORMAT "99-99-99" 
    help "Invoice date"
    with row 4 col 2 title color value(ctc) " FIND DATE"
    COLOR value(cfc) NO-LABELS OVERLAY FRAME F4.

{Func/brand.i}

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

IF gcHelpParam > "" THEN DO:
   liInvCust = INTEGER(ENTRY(1,gcHelpParam)) NO-ERROR.
   IF NUM-ENTRIES(gcHelpParam) > 1
   THEN llOnlyOpen = (ENTRY(2,gcHelpParam) = "TRUE"). 
END.

order = 1.

RUN local-find-first.

IF AVAILABLE Invoice THEN DO:
   ASSIGN
   memory = recid(Invoice)
   must-print = TRUE.
END.
ELSE DO:
   BELL.
   message "No invoices !".
   PAUSE 2 no-message.
   RETURN.
END.

ASSIGN xrecid = ? ufkey = TRUE firstline = 0 order = 1 siirto = ?.

LOOP:
repeat WITH FRAME sel ON ENDKEY UNDO LOOP, NEXT LOOP:

print-line:  
   DO :
      IF must-print THEN DO:

         up FRAME-LINE - 1.
         FIND Invoice where recid(Invoice) = memory no-lock NO-ERROR.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory */

         repeat WITH FRAME sel:
            IF AVAILABLE Invoice THEN DO:

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

      ex-order = order.
      
      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 92  ufk[2]= 1634 ufk[3]= 707 ufk[4]= 28
         ufk[5]= 11  ufk[6]= 927 ufk[7]= 829 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         IF liInvCust > 0 THEN ASSIGN 
            ufk[1] = 0
            ufk[2] = 0
            ufk[3] = 0
            ufk[4] = 0.

         {Syst/uright1.i '"6,7"'}
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW Invoice.ExtInvID {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) Invoice.ExtInvID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW Invoice.InvNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) Invoice.InvNum WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
         CHOOSE ROW Invoice.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) Invoice.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
         CHOOSE ROW Invoice.InvDate {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) Invoice.InvDate WITH FRAME sel.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      IF rtab[FRAME-LINE] = ? AND LOOKUP(nap,"8,F8") = 0 THEN DO:
         BELL.
         message "YOU ARE ON AN EMPTY ROW, MOVE UPWARDS !".
         PAUSE 1 no-message.
         NEXT.
      END.

      if nap = "cursor-right" AND liInvCust = 0 THEN DO:
         order = order + 1.
         IF order = 5 THEN order = 1.
      END.
      ELSE if nap = "cursor-left" AND liInvCust = 0 THEN DO:
         order = order - 1.
         IF order = 0 THEN order = 4.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN
         firstline = 0
         memory = rtab[FRAME-LINE].
         FIND Invoice where recid(Invoice) = memory NO-LOCK.
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
         
         FIND Invoice where recid(Invoice) = memory no-lock NO-ERROR.
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

     ELSE IF LOOKUP(nap,"1,F1") > 0 AND ufk[1] > 0 THEN DO:  
        cfc = "puyr". RUN Syst/ufcolor.p.
        lcExtInvID = "".
        ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        UPDATE lcExtInvID WITH FRAME F1.
        HIDE FRAME F1 no-pause.

        IF lcExtInvID > "" THEN DO:
           FIND FIRST Invoice where 
                      Invoice.Brand   = gcBrand AND
                      Invoice.ExtInvID >= lcExtInvID
           no-lock NO-ERROR.

           IF NOT fRecFound(1) THEN NEXT BROWSE.
 
           NEXT LOOP.
        END.
     END. 

     ELSE IF LOOKUP(nap,"2,F2") > 0 AND ufk[2] > 0 THEN DO:  
        cfc = "puyr". RUN Syst/ufcolor.p.
        lcExtInvID = "".
        ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        UPDATE liInvNum WITH FRAME F2.
        HIDE FRAME F2 no-pause.

        IF liInvNum > 0 THEN DO:
           FIND FIRST Invoice where 
                      Invoice.InvNum >= liInvNum
           no-lock NO-ERROR.

           IF NOT fRecFound(2) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. 

     ELSE IF LOOKUP(nap,"3,F3") > 0 AND ufk[3] > 0  THEN DO:  
        cfc = "puyr". RUN Syst/ufcolor.p.
        lcExtInvID = "".
        ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        UPDATE liCustNum WITH FRAME F3.
        HIDE FRAME F3 no-pause.

        IF liCustNum > 0 THEN DO:
           FIND FIRST Invoice where 
                      Invoice.Brand   = gcBrand AND
                      Invoice.CustNum >= liCustNum
           no-lock NO-ERROR.

           IF NOT fRecFound(3) THEN NEXT BROWSE.
 
           NEXT LOOP.
        END.
     END. 

     ELSE IF LOOKUP(nap,"4,F4") > 0 AND ufk[4] > 0 THEN DO:
        cfc = "puyr". RUN Syst/ufcolor.p.
        ldtInvDate = ?.
        ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        UPDATE ldtInvDate WITH FRAME F4.
        HIDE FRAME F4 no-pause.

        IF ldtInvDate <> ? THEN DO:
           FIND LAST Invoice where 
                     Invoice.Brand  = gcBrand AND
                     Invoice.InvDate = ldtInvDate    
           no-lock NO-ERROR.
           IF NOT AVAILABLE Invoice THEN 
           FIND FIRST Invoice where 
                      Invoice.Brand  = gcBrand AND
                      Invoice.InvDate > ldtInvDate    
           no-lock NO-ERROR.

           IF NOT fRecFound(4) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. 

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANS: /* memo */
        FIND Invoice WHERE recid(Invoice) = rtab[frame-line(sel)]
           NO-LOCK NO-ERROR.
        RUN Mc/memo.p(INPUT Invoice.CustNum,
                 INPUT "invoice",
                 INPUT STRING(Invoice.InvNum),
                 INPUT "Invoice number").
        ufkey = TRUE.
        NEXT.
     END.

     else if lookup(nap,"f7,7") > 0 THEN DO:
        FIND Invoice where recid(Invoice) = rtab[FRAME-LINE(sel)] 
           no-lock NO-ERROR.
        RUN Ar/payments.p(0,Invoice.InvNum,"").
        ufkey = TRUE.
        NEXT.
     END.

     else if lookup(nap,"enter,return,5,f5") > 0 THEN DO : /* valitaan tAmA */
        FIND Invoice where recid(Invoice) = rtab[FRAME-LINE(sel)] 
           no-lock NO-ERROR.
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

PROCEDURE local-disp-row:

    RUN Ar/invbal.p(Invoice.InvNum, OUTPUT BalDue).

    notes = CAN-FIND(FIRST memo WHERE 
                           memo.Brand     = Invoice.Brand AND
                           memo.HostTable = "Invoice" AND
                           memo.KeyValue  = STRING(Invoice.InvNum)).

    DISPLAY Invoice.ExtInvID
            Invoice.InvNum 
            Invoice.CustNum 
            Invoice.CustName 
            Invoice.InvDate
            Invoice.InvAmt 
            Invoice.PaymState
            BalDue notes
            WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-next:

   IF liInvCust > 0 THEN DO:
      IF NOT llOnlyOpen THEN 
      FIND NEXT Invoice USE-INDEX CustNum WHERE
                Invoice.Brand = gcBrand AND
                Invoice.CustNum = liInvCust NO-LOCK NO-ERROR.
      ELSE  
      FIND NEXT Invoice USE-INDEX CustNum WHERE
                Invoice.Brand   = gcBrand   AND
                Invoice.CustNum = liInvCust AND
                Invoice.PaymState < 2 NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF      order = 1 THEN FIND NEXT Invoice USE-INDEX ExtInvID WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN FIND NEXT Invoice USE-INDEX InvNum WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN FIND NEXT Invoice USE-INDEX CustNum WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 4 THEN FIND NEXT Invoice USE-INDEX InvDate WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-prev:

   IF liInvCust > 0 THEN DO:
      IF NOT llOnlyOpen THEN 
      FIND PREV Invoice USE-INDEX CustNum WHERE
                Invoice.Brand = gcBrand AND
                Invoice.CustNum = liInvCust NO-LOCK NO-ERROR.
      ELSE  
      FIND PREV Invoice USE-INDEX CustNum WHERE
                Invoice.Brand   = gcBrand   AND
                Invoice.CustNum = liInvCust AND
                Invoice.PaymState < 2 NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF      order = 1 THEN FIND PREV Invoice USE-INDEX ExtInvID WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN FIND PREV Invoice USE-INDEX InvNum WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN FIND PREV Invoice USE-INDEX CustNum WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 4 THEN FIND PREV Invoice USE-INDEX InvDate WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-first:

   IF liInvCust > 0 THEN DO:
      IF NOT llOnlyOpen THEN 
      FIND FIRST Invoice USE-INDEX CustNum WHERE
                 Invoice.Brand = gcBrand AND
                 Invoice.CustNum = liInvCust NO-LOCK NO-ERROR.
      ELSE  
      FIND FIRST Invoice USE-INDEX CustNum WHERE
                 Invoice.Brand   = gcBrand   AND
                 Invoice.CustNum = liInvCust AND
                 Invoice.PaymState < 2 NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      IF      order = 1 THEN FIND FIRST Invoice USE-INDEX ExtInvID WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN FIND FIRST Invoice USE-INDEX InvNum WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN FIND FIRST Invoice USE-INDEX CustNum WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 4 THEN FIND FIRST Invoice USE-INDEX InvDate WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-last:

   IF liInvCust > 0 THEN DO:
      IF NOT llOnlyOpen THEN 
      FIND LAST Invoice USE-INDEX CustNum WHERE
                Invoice.Brand = gcBrand AND
                Invoice.CustNum = liInvCust NO-LOCK NO-ERROR.
      ELSE  
      FIND LAST Invoice USE-INDEX CustNum WHERE
                Invoice.Brand   = gcBrand   AND
                Invoice.CustNum = liInvCust AND
                Invoice.PaymState < 2 NO-LOCK NO-ERROR.
   END.           ELSE DO:
      IF      order = 1 THEN FIND LAST Invoice USE-INDEX ExtInvID WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN FIND LAST Invoice USE-INDEX InvNum WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN FIND LAST Invoice USE-INDEX CustNum WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
      ELSE IF order = 4 THEN FIND LAST Invoice USE-INDEX InvDate WHERE
         Invoice.Brand = gcBrand NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

