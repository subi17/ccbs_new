/* -----------------------------------------------
  MODULE .......: NNLAKY.P
  FUNCTION .....: Avoimien laskujen kysely
  APPLICATION ..: NN
  AUTHOR .......: TT
  CREATED ......: 26.02.97
  CHANGED ......: 09.10.98 pr RUN nninme (memo): F4
                  20.05.02/tk RUN Mc/memo.p
                  22.07.02 tk print full page on "end"
                  12.11.02 jr "nnlasku" => "invoice" in memo
                  21.03.03/aam claiming history
                  17.09.03/aam use invdet.i for details,
                               brand,
                               don't go directly into update mode with "enter"
                               etc.
                  06.02.04 jp  CustNum for memos             
                  14.04.04/aam index CustName replaced with CustNum
                  18.04.06/aam use payments.p instead of nnlasu.p
                  22.03.07 kl  new param for RUN Ar/payments.p

  Version ......: M15
  ------------------------------------------------------ */

&GLOBAL-DEFINE TMSCodeDef   NO
&GLOBAL-DEFINE TimeStampDef NO
&GLOBAL-DEFINE Brtable Invoice

{Syst/commali.i}
{Func/cparam2.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invoice'}
{Ar/invdet.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.
   RUN StarEventInitialize(lhInvoice).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhInvoice).
   END.

END.

DEF NEW shared VAR order AS INT NO-UNDO.

DEF VAR memory      AS re              NO-UNDO.
def var line        as int format "99" NO-UNDO.
DEF VAR must-print  AS lo              NO-UNDO.
DEF VAR must-add    AS lo              NO-UNDO.
DEF VAR ufkey       AS lo              NO-UNDO.
DEF VAR rtab        AS re EXTENT 24    NO-UNDO.
DEF VAR i           AS i               NO-UNDO.
DEF VAR xrecid      AS re.
DEF VAR ex-order    AS i               NO-UNDO.
DEF VAR firstline   AS i               NO-UNDO.
DEF VAR avoinna     LIKE Invoice.InvAmt NO-UNDO.
DEF VAR ssuor       LIKE Invoice.InvAmt NO-UNDO.
DEF VAR salen       LIKE Invoice.InvAmt NO-UNDO.
def var memo        as log format "M/" NO-UNDO.
DEF VAR endloop     AS INT             NO-UNDO.
DEF VAR oldday       LIKE Invoice.DueDate NO-UNDO.
def var cu-text      as c                 no-undo.
DEF VAR liInvNum    LIKE Invoice.InvNum   NO-UNDO.
DEF VAR liCustNum   LIKE Invoice.CustNum  NO-UNDO.

form
    Invoice.Brand      column-label "Bran" format "x(4)"
    Invoice.InvNum     column-label "Invoice#"
    memo               column-label "M/"
    Invoice.CustNum    column-label "CustNum" format ">>>>>>>9"
    Invoice.CustName   column-label "Name" format "x(10)"
    Invoice.InvDate    column-label "Date" 
    Invoice.DueDate    column-label "Dueday"   
    Invoice.InvAmt     column-label "BilledAmt"  format "->>>>>>9.99"
    avoinna            column-label "BalanceDue" format "->>>>>>9.99"

WITH width 80 OVERLAY ROW 1 scroll 1 15 DOWN 
    COLOR value(Syst.Var:cfc) TITLE COLOR value(Syst.Var:ctc)
    " " + Syst.Var:ynimi + " ALL INVOICES "
    + string(TODAY,"99-99-99") + " " FRAME sel.

{Func/brand.i}

form /* Invoicen numerohakua varten */
    "Brand .:" lcBrand skip
    "Invoice:" liInvNum
    help "Give Invoice No."    
    with row 4 col 2 title color value(Syst.Var:ctc) " FIND INVOICE No."
    COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME F1.

form /* Invoicen asnolla hakua varten */
    "Brand ..:" lcBrand skip
    "Customer:" liCustNum
    help "Give Customer Name"
    with row 4 col 2 title color value(Syst.Var:ctc) " FIND CUSTOMER "
    COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME F2.


Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
view FRAME sel.


FIND FIRST Invoice WHERE Invoice.Brand = lcBrand 
   USE-INDEX InvNum NO-LOCK no-error.
IF AVAILABLE Invoice THEN DO:
   ASSIGN
   memory = recid(Invoice)
   must-print = TRUE.
END.
ELSE DO:
   BELL.
   message "No bills !".
   PAUSE 2 no-message.
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
           if order = 1 then put screen row 18 col 30 " By Invoice No.  ".
           if order = 2 then put screen row 18 col 30 " By CustomerName ".
           if order = 3 then put screen row 18 col 30 " By Invoice Date ".
         END.

         up FRAME-LINE - 1.
         FIND Invoice where recid(Invoice) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory */

         repeat WITH FRAME sel:
            IF AVAILABLE Invoice THEN DO:

               RUN local-disp-row.

               rtab[FRAME-LINE] = recid(Invoice).
               IF      order = 1 THEN FIND NEXT Invoice USE-INDEX InvNum
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
               ELSE IF order = 2 THEN FIND NEXT Invoice USE-INDEX CustNum
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
               ELSE IF order = 3 THEN FIND NEXT Invoice USE-INDEX InvDate
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.

         IF endloop = 0 THEN up FRAME-LINE - 1.
         DOWN firstline.
         ASSIGN 
            firstline = 0
            must-print = FALSE
            endloop = 0.
         PAUSE 0 no-message.

         /* one page of data has been Printed AND
         mAllA linellA choosea odotellen. */
      END. /* must-print = TRUE */
   END. /* print-line */

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         Syst.Var:ufk[1]= 92  Syst.Var:ufk[2]= 707 Syst.Var:ufk[3]= 927 Syst.Var:ufk[4]= 1492
         Syst.Var:ufk[5]= 829  Syst.Var:ufk[6]= 0  Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
         Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW Invoice.InvNum {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(Syst.Var:ccc) Invoice.InvNum WITH FRAME sel.
      END.
      IF order = 2 THEN DO:
         CHOOSE ROW Invoice.CustNum {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(Syst.Var:ccc) Invoice.CustNum WITH FRAME sel.
      END.
      IF order = 3 THEN DO:
         CHOOSE ROW Invoice.InvDate {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(Syst.Var:ccc) Invoice.InvDate WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         message "You are on a empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      if Syst.Var:nap = "cursor-right" THEN DO:
         order = order + 1.
         IF order = 4 THEN order = 1.
      END.
      if Syst.Var:nap = "cursor-left" THEN DO:
         order = order - 1.
         IF order = 0 THEN order = 3.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN
         firstline = 0
         memory = rtab[FRAME-LINE].
         FIND Invoice where recid(Invoice) = memory.
         DO i = 1 TO FRAME-LINE - 1:

            IF order = 1 THEN
            FIND prev Invoice USE-INDEX InvNum WHERE 
               Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 2 THEN
            FIND prev Invoice USE-INDEX CustNum WHERE 
               Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 3 THEN
            FIND prev Invoice USE-INDEX InvDate WHERE 
               Invoice.Brand = lcBrand NO-LOCK no-error.

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
      if lookup(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND Invoice where recid(Invoice) = rtab[1] no-lock.
            IF      order = 1 THEN FIND prev Invoice USE-INDEX InvNum
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 2 THEN FIND prev Invoice USE-INDEX CustNum
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 3 THEN FIND prev Invoice USE-INDEX InvDate
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
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
      else if lookup(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:

         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND Invoice where recid(Invoice) = rtab[FRAME-DOWN] no-lock .
            IF      order = 1 THEN FIND NEXT Invoice USE-INDEX InvNum
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 2 THEN FIND NEXT Invoice USE-INDEX CustNum
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 3 THEN FIND NEXT Invoice USE-INDEX InvDate
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
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
      else if lookup(Syst.Var:nap,"prev-page,page-up") > 0 THEN DO:
         memory = rtab[1].
         FIND Invoice where recid(Invoice) = memory no-lock no-error.
         IF      order = 1 THEN FIND prev Invoice USE-INDEX InvNum
         WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
         ELSE IF order = 2 THEN FIND prev Invoice USE-INDEX CustNum
         WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
         ELSE IF order = 3 THEN FIND prev Invoice USE-INDEX InvDate
         WHERE Invoice.Brand = lcBrand NO-LOCK no-error.

         IF AVAILABLE Invoice THEN DO:
            memory = recid(Invoice).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF      order = 1 THEN FIND prev Invoice USE-INDEX InvNum
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
               ELSE IF order = 2 THEN FIND prev Invoice USE-INDEX CustNum
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
               ELSE IF order = 3 THEN FIND prev Invoice USE-INDEX InvDate
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
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
     else if lookup(Syst.Var:nap,"next-page,page-down") > 0 THEN DO WITH FRAME sel:

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

     /* Haku 1 */
     else if lookup(Syst.Var:nap,"1,f1") > 0 THEN DO:

           Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
           liInvNum = 0.
           Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           DISPLAY lcBrand WITH FRAME F1.
           UPDATE lcBrand WHEN Syst.Var:gcAllBrand
                  liInvNum WITH FRAME F1.
           HIDE FRAME F1 no-pause.

           IF liInvNum <> 0 THEN DO:
              IF lcBrand = "*" THEN 
              FIND FIRST Invoice where 
                 Invoice.InvNum >= liInvNum
              no-lock no-error.

              ELSE
              FIND FIRST Invoice where 
                 Invoice.Brand  = lcBrand AND
                 Invoice.InvNum >= liInvNum
              no-lock no-error.

              IF NOT fRecFound(1) THEN NEXT BROWSE.

              NEXT LOOP.
           END.
     END. /* Haku sar. 1 */

     else if lookup(Syst.Var:nap,"2,f2") > 0 THEN DO:

           Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
           liCustNum = 0.
           Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           DISPLAY lcBrand WITH FRAME F2.
           UPDATE lcBrand WHEN Syst.Var:gcAllBrand
                  liCustNum WITH FRAME F2.
           HIDE FRAME F2 no-pause.

           IF liCustNum > 0 THEN DO:

              FIND FIRST Invoice where 
                         Invoice.Brand  = lcBrand AND
                         Invoice.CustNum >= INPUT liCustNum
              no-lock no-error.

              IF NOT fRecFound(2) THEN NEXT BROWSE.

              NEXT LOOP.
           END.
     END.

     IF LOOKUP(Syst.Var:nap,"3,F3") > 0 THEN DO : /* memo */
        FIND Invoice WHERE RECID(Invoice) = rtab[FRAME-LINE(sel)]
        NO-LOCK NO-ERROR.
        RUN Mc/memo.p(INPUT Invoice.CustNum,
                 INPUT "invoice",
                 INPUT STRING(Invoice.InvNum),
                 INPUT "Invoice number").
        ufkey = TRUE.
        NEXT.
     END.

     else if lookup(Syst.Var:nap,"4,f4") > 0 THEN DO : /* claiming history */
        FIND Invoice where recid(Invoice) = rtab[FRAME-LINE] no-lock no-error.
        RUN Ar/claimhis.p(0,
                     INPUT Invoice.InvNum).
        ufkey = TRUE.
        NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"5,f5") > 0 THEN DO : /* valitaan tAmA */
        FIND Invoice where recid(Invoice) = rtab[FRAME-LINE] no-lock no-error.
        RUN Ar/payments.p(0,Invoice.InvNum,"").
        ufkey = TRUE.
        NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"home") > 0 THEN DO:
        IF      order = 1 THEN FIND FIRST Invoice USE-INDEX InvNum
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        ELSE IF order = 2 THEN FIND FIRST Invoice USE-INDEX CustNum
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        ELSE IF order = 3 THEN FIND FIRST Invoice USE-INDEX InvDate
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        ASSIGN
        memory = recid(Invoice)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"end") > 0 THEN DO : /* LAST record */
        IF      order = 1 THEN FIND LAST Invoice USE-INDEX InvNum
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        ELSE IF order = 2 THEN FIND LAST Invoice USE-INDEX CustNum
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        ELSE IF order = 3 THEN FIND LAST Invoice USE-INDEX InvDate
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.

        DO endloop = 1 TO FRAME-DOWN - 1:
           IF      order = 1 THEN FIND PREV Invoice USE-INDEX InvNum
           WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
           ELSE IF order = 2 THEN FIND PREV Invoice USE-INDEX CustNum
           WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
           ELSE IF order = 3 THEN FIND PREV Invoice USE-INDEX InvDate
           WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        END.

        ASSIGN
        memory = recid(Invoice)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else IF lookup(Syst.Var:nap,"enter,return") > 0 THEN 
     LP:
     REPEAT : 

        Syst.Var:ehto = 5.
        RUN Syst/ufkey.p.
        ufkey = true.

        DO transaction
        on endkey undo, leave:

           find Invoice where recid(Invoice) = rtab[frame-line(sel)] 
           no-error. 

           /* show details */
           RUN pInvoiceDetails(Invoice.InvNum,
                               FALSE).

           oldday = Invoice.DueDate.

           ASSIGN
           Syst.Var:ufk = 0
           Syst.Var:ufk[1] = IF lcRight = "RW" THEN 7 ELSE 0
           Syst.Var:ufk[8] = 8
           Syst.Var:ehto = 3.
           RUN Syst/ufkey.p.

           READKEY.

           if LOOKUP(KEYLABEL(LASTKEY),"1,f1") > 0 AND 
              lcRight = "RW" THEN DO:

              Syst.Var:ehto = 9.
              RUN Syst/ufkey.p.

              FIND Current Invoice EXCLUSIVE-LOCK.

              IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).

              update 
              Invoice.DueDate 
              Invoice.InvCfg[1]
              Invoice.ClaimPerm 
              Invoice.InvCfg[2]
              with frame fInvDet.

              IF INPUT Invoice.DueDate = ?
              THEN DO:
                 MESSAGE "Dueday can not be empty - press ENTER !".
                 PAUSE NO-MESSAGE.
                 Invoice.DueDate = oldday.
                 NEXT LP.
              END.

              IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).

           END.

           xrecid = recid(Invoice).
           hide frame fInvDet no-pause.
           display Invoice.DueDate with frame sel.

        end.

        NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.

PROCEDURE local-disp-row:

   RUN Ar/invbal.p(Invoice.InvNum,
              OUTPUT avoinna).

   memo = CAN-FIND(FIRST memo WHERE
                         memo.Brand     = Invoice.Brand AND
                         memo.HostTable = "invoice" AND
                         memo.KeyValue  = STRING(Invoice.InvNum) AND
                         memo.MemoText  NE "").

   DISPLAY Invoice.Brand
           Invoice.InvNum Invoice.CustNum Invoice.CustName memo
           Invoice.InvDate Invoice.DueDate
           Invoice.InvAmt avoinna
           WITH FRAME sel.

END PROCEDURE.
