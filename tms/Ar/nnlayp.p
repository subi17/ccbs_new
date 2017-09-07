/* ----------------------------------------------------------------------------
  MODULE .......: NNLAYP.P
  FUNCTION .....: Laskutietojen BROWSE/yllapito
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 10.10.96
  MODIFIED .....: 07.11.96 tt  Mukaan laskulinen kpl-maara
                  28.11.96 pt  change eri framella, n{yt. kaikki kent{t
                  05.01.97 pt  alv-% yritystietueelta
                  06.02.97 pt  pyor. laskenta, order.3 indeksiksi InvAmt
                  24.02.97 pt  laasno muutettu CustNum:ksi
                  19.09.97 pt  nAyt. InterestAmt ja pyOrist. 50 Ayriin
                  30.03.98 pt  a NEW PARAMETER TRUE into F5/nnlryp
                  02.07.98 kl  order & FIND BY invoice Date
                  05.08.98 pt  F7 added
                  16.09.98 kl  InvCfg + "F4"
                  25.09.98 kl  error-status:error
                  30.09.98 kl  F4, FIND amount fixed
                  13.10.98 kl  memo !! OPEN WITH m,M
                  14.04.99 pt  F4-search WITH invoice Date also
                  12.07.99 kl  Currency modifications
                  13.07.99 kl  previous continued
                  24.08.99 pt  DISP ChgStamp & ExpStamp @ FRAME lis
                               DISP Invoice.OverPaym, after 'nnrlyp.p' re-
                               calculate totals WITH Invoice.OverPaym AND
                               ROUND InvAmt into nearest krone
                  01.09.99 pt  (hidden function "T") shows time stamps,
                               use VATCode when re-calculating total
                  25.10.99 kl  functions in ASSIGN
                  25.01.00 kl  finding customer after inv.rows fixed
                  11.05.01 kl  Invoice.DiscPerc & Invoice.DirDisc
                  21.05.01 kl  DISPLAY formats
                  16.10.01 kl  DISPLAY formats
                  04.01.02 aam don't recalculate sums FOR old invoices
                               (after nnlryp)
                  14.03.02 lp  updated browse BY DAY
                  25.03.02 lp  change FRAME LIS LIKE in the nnasla.p
                  30.04.02 lp  Checking: Dueday cann't be empty
                  02.05.02 tk  eventlogging added
                  20.05.02 tk  RUN Mc/memo.p
                  08.07.02 jp  F7 Send Invoice Specification via email
                  23.08.02 tk  Invoice.OverPaym, f2->claimhist
                  09.09.02 jp  Kr, label removed
                  01.10.02/aam sums can't be changed in nnlryp -> no need
                               to recalculate invoice sums
                  14.10.02/aam DirDisc removed, show always Currency
                  15.11.02 lp  use invdet.i for invoice details
                  05.03.03 tk  tokens
                  23.03.03 kl  date find fixed
                  25.03.03/aam amount find fixed 
                  15.09.03/aam brand,
                               don't go directly into update mode with "enter",
                               due date cannot be changed if ddstate > 0
                  10.10.03/aam itsendlo
                  02.02.04 jp  memo to customer
                  12.03.04/aam new column PaymState
                  18.05.04/aam max. addition to due date from DueDateTrans
                  14.07.04/tk  memo to invoice
                  16.08.04/aam find with amount uses PaymState  
                  18.04.06/aam use payments.p instead of nnlasu.p
                  30.11.06/aam ExtInvID
                  22.03.07 kl  new param for RUN Ar/payments.p
                  25.04.07/aam eventlog in detail view, 
                               use pInvoiceUpdate
  Version ......: M15
 --------------------------------------------------------------------------- */
&GLOBAL-DEFINE TimeStampDef NO
&GLOBAL-DEFINE TMSCodeDef NO
&GLOBAL-DEFINE BrTable Invoice

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invoice'}
{Ar/invdet.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.
   RUN StarEventInitialize(lhInvoice).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhInvoice).
   END.

END.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR hakuCustNum LIKE Invoice.CustNum  NO-UNDO.
DEF VAR hakunetto  LIKE Invoice.AmtExclVAT NO-UNDO.
DEF VAR InvDate      LIKE Invoice.InvDate   NO-UNDO.
DEF VAR firstline  AS INT               NO-UNDO.
DEF VAR order      AS INT               NO-UNDO.
DEF VAR ex-order   AS INT               NO-UNDO.
DEF VAR memory     AS RECID             NO-UNDO.
def var line       as int format "99"   NO-UNDO.               
DEF VAR delline    AS INT               NO-UNDO.
DEF VAR must-print AS LOG               NO-UNDO.
DEF VAR alepros    AS dec               NO-UNDO.
DEF VAR ufkey      AS LOG               NO-UNDO.
DEF VAR fr-header  AS CHAR              NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24   NO-UNDO.
DEF VAR i          AS INT               NO-UNDO.
DEF VAR ok         AS LOG               NO-UNDO.
DEF VAR unetto     AS DE                NO-UNDO.
def var notes      as lo  format "M/"   NO-UNDO.
DEF VAR defcurr    AS c                 NO-UNDO.
DEF VAR cu-text    AS c                 NO-UNDO.
DEF VAR xrecid     AS RECID.
DEF VAR oldday     LIKE Invoice.DueDate NO-UNDO.
DEF VAR endloop    AS INT               NO-UNDO.
DEF VAR liInvoice  AS INT               NO-UNDO.
DEF VAR liDueDate  AS INT               NO-UNDO. 
DEF VAR ldtChkDate AS DATE              NO-UNDO. 
DEF VAR liPaymState AS INT              NO-UNDO.
DEF VAR lcCustName  AS CHAR             NO-UNDO.
DEF VAR lcExtInvID  AS CHAR             NO-UNDO.

ASSIGN defcurr   = fCParamC("DefCurrency")
       liDueDate = fCParamI("DueDateTrans").

form
    Invoice.ExtInvID     FORMAT "X(14)" 
    notes                 column-label "M"                 
    Invoice.InvDate      /* column-label "Inv. date"     */   format "99-99-99"
    Invoice.CustNum     /* column-label "Cust.nr"       */
    lcCustName          column-label "Customer name"    format "x(13)"
    Invoice.InvAmt   /* column-label "To pay"        */ format "->>>,>>9.99"
    Invoice.Currency       column-label "CUR"        format "x(3)"     
    Invoice.DueDate     /* column-label "Exp. day"      */ format "99-99-99"
    Invoice.PaymState     column-label "P"    format "9" 
    Invoice.PrintState     /* column-label "S"             */

WITH width 80 OVERLAY scroll 1 15 DOWN ROW 1
    COLOR value(cfc)
    title color value(ctc) " " + ynimi + " INVOICE File "
    + string(pvm,"99-99-99") + " " FRAME sel.

form
   skip(1)
   " You have CHANGED invoice no. " Invoice.ExtInvID NO-LABEL skip(1)
   " Billed amount BEFORE CHANGE was" Invoice.CurrAmt  AT 45
   format "z,zzz,zz9.99"  SKIP
   " Billed amount AFTER  CHANGE was"         unetto  AT 45
   format "z,zzz,zz9.99" skip(1)
   " Is this OK (Y/N) ?" ok  format "Yes/No"
WITH
    OVERLAY ROW 3 centered COLOR value(cfc) TITLE COLOR value(ctc)
    " ATTENTION ! ! ! " NO-LABEL WITH FRAME unetto.

{Func/brand.i}

form /* Invoicen numerohakua varten */
    "Brand .:" lcBrand skip
    "Invoice:" lcExtInvID  FORMAT "X(14)" 
    help "Give Invoice No."    
    with row 4 col 2 title color value(ctc) " FIND INVOICE No."
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /* Invoicen asnolla hakua varten */
    "Brand ..:" lcBrand skip
    "Customer:" hakuCustNum
    help "Give Customer No."
    with row 4 col 2 title color value(ctc) " FIND CUST # "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hakie.

form /* Invoicen summan hakua varten */
    "Brand .......:" lcBrand skip
    "Payment State:" liPaymState 
       format "9"
       help "Payment state (0=not paid, 1-2=paid, 3=credit loss)"
       skip
    "Amount ......:" hakunetto   
       help "Give Billed Amount"   SKIP
 with row 4 col 2 title color value(ctc) " FIND AMOUNT "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.

form /* Invoicen paivayshakua varten */
    "Brand:" lcBrand skip
    "Date :" InvDate
    help "Give Date of Invoice"
    with row 4 col 2 title color value(ctc) " FIND Date "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr3.

form /* memo */
WITH
    OVERLAY ROW 7 centered NO-LABEL
    color value(cfc) title color value(cfc) " Update memo "
    FRAME memo.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST Invoice WHERE Invoice.Brand = lcBrand USE-INDEX ExtInvID
NO-LOCK no-error.
IF AVAILABLE Invoice THEN
   ASSIGN
   memory = recid(Invoice)
   must-print = TRUE.
ELSE DO:
   BELL.
   message "There aren't any invoices - press ENTER !".
   PAUSE no-message.
   RETURN.
END.

ASSIGN xrecid = ? delline = 0 ufkey = TRUE order = 1 firstline = 0.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
    END.

   print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND Invoice where recid(Invoice) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE Invoice THEN DO:

               RUN local-disp-row.
               rtab[FRAME-LINE] = recid(Invoice).
               IF order = 1 THEN FIND NEXT Invoice USE-INDEX ExtInvID
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
               ELSE IF order = 2 THEN FIND prev Invoice USE-INDEX InvDate
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
               ELSE IF order = 3 THEN FIND NEXT Invoice USE-INDEX CustNum
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
               ELSE IF order = 4 THEN FIND NEXT Invoice USE-INDEX InvAmt
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
         ASSIGN firstline = 0
                must-print = FALSE
                endloop = 0.
         PAUSE 0 no-message.

         /* one page of data has been Printed AND
         the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

   BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN  ufk = 0
         ufk[1]= 94  ufk[2] = 1492 ufk[3] = 927 ufk[4] = 1883
         ufk[5]= 790 ufk[6] = 829 ufk[7] = 1796 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW Invoice.ExtInvID {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Invoice.ExtInvID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW Invoice.InvDate {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Invoice.InvDate WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
         CHOOSE ROW Invoice.CustNum  {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Invoice.CustNum  WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
         CHOOSE ROW Invoice.InvAmt {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Invoice.InvAmt WITH FRAME sel.
      END.
       IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 5 THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 4.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND Invoice no-lock where recid(Invoice) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 1 THEN FIND prev Invoice USE-INDEX ExtInvID
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 2 THEN FIND NEXT Invoice USE-INDEX InvDate   
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 3 THEN FIND prev Invoice USE-INDEX CustNum
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 4 THEN FIND prev Invoice USE-INDEX InvAmt
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            IF AVAILABLE Invoice THEN
               ASSIGN firstline = i memory = recid(Invoice).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? THEN DO:
         BELL.
         message "You are on a empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND Invoice where recid(Invoice) = rtab[1] no-lock.
            IF order = 1 THEN FIND prev Invoice USE-INDEX ExtInvID
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 2 THEN FIND NEXT Invoice USE-INDEX InvDate
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 3 THEN FIND prev Invoice USE-INDEX CustNum
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 4 THEN FIND prev Invoice USE-INDEX InvAmt
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            IF NOT AVAILABLE Invoice THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
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
            IF order = 1 THEN FIND NEXT Invoice USE-INDEX ExtInvID
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 2 THEN FIND prev Invoice USE-INDEX InvDate
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 3 THEN FIND NEXT Invoice USE-INDEX CustNum
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            ELSE IF order = 4 THEN FIND NEXT Invoice USE-INDEX InvAmt
            WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
            IF NOT AVAILABLE Invoice THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               RUN local-disp-row.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(Invoice).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND Invoice where recid(Invoice) = memory no-lock no-error.
         IF order = 1 THEN FIND prev Invoice USE-INDEX ExtInvID
         WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
         ELSE IF order = 2 THEN FIND NEXT Invoice USE-INDEX InvDate
         WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
         ELSE IF order = 3 THEN FIND prev Invoice USE-INDEX CustNum
         WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
         ELSE IF order = 4 THEN FIND prev Invoice USE-INDEX InvAmt
         WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
         IF AVAILABLE Invoice THEN DO:
            memory = recid(Invoice).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 1 THEN FIND prev Invoice USE-INDEX ExtInvID
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
               ELSE IF order = 2 THEN FIND NEXT Invoice USE-INDEX InvDate
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
               ELSE IF order = 3 THEN FIND prev Invoice USE-INDEX CustNum
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
               ELSE IF order = 4 THEN FIND prev Invoice USE-INDEX InvAmt
               WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
               IF AVAILABLE Invoice THEN memory = recid(Invoice).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE !".
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

     /* search functions */
     if lookup(nap,"1,f1") > 0 THEN 
     etsi:    
     repeat WITH FRAME sel:

        ASSIGN ufk = 0 ehto = 0  ufk[8] = 8
               ufk[1]= 133 ufk[2]= 28  ufk[3]= 702 ufk[4]= 789.

        RUN Syst/ufkey.p.
        ufkey = TRUE.

        IF toimi = 8 THEN LEAVE etsi.


        /* Haku 1 */
        IF toimi = 1 THEN DO:  /* haku sarakk. 1 */
           cfc = "puyr". RUN Syst/ufcolor.p.
           lcExtInvID = "".
           ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           DISPLAY lcBrand WITH FRAME hayr.
           UPDATE lcBrand WHEN gcAllBrand
                  lcExtInvID WITH FRAME hayr.
           HIDE FRAME hayr no-pause.

           IF lcExtInvID <> "" THEN DO:
              IF lcBrand = "*" THEN 
              FIND FIRST Invoice where 
                 Invoice.Brand = lcBrand AND
                 Invoice.ExtInvID >= INPUT lcExtInvID
              no-lock no-error.

              ELSE
              FIND FIRST Invoice where 
                 Invoice.Brand   = lcBrand AND
                 Invoice.ExtInvID >= INPUT lcExtInvID
              no-lock no-error.

              IF NOT fRecFound(1) THEN NEXT BROWSE.

              NEXT LOOP.
           END.
        END. /* Haku sar. 1 */

        /* Haku sarakk. 2 */
        IF toimi = 2 THEN DO:  /* haku sar. 2 */
           cfc = "puyr". RUN Syst/ufcolor.p.
           InvDate = ?.
           ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           DISPLAY lcBrand WITH FRAME hayr3.
           UPDATE lcBrand WHEN gcAllBrand
                  InvDate WITH FRAME hayr3.
           HIDE FRAME hayr3 no-pause.

           IF InvDate <> ? THEN DO:
              FIND LAST Invoice where 
                        Invoice.Brand  = lcBrand AND
                        Invoice.InvDate = INPUT InvDate    
              no-lock no-error.
              IF NOT AVAILABLE Invoice THEN 
              FIND FIRST Invoice where 
                         Invoice.Brand  = lcBrand AND
                         Invoice.InvDate > INPUT InvDate    
              no-lock no-error.

              IF NOT fRecFound(2) THEN NEXT BROWSE.

              NEXT LOOP.
           END.
        END. /* Haku sar. 2 */

        /* Haku 3 */
        ELSE IF toimi = 3 THEN DO:  /* haku sarakk. 3 */
           cfc = "puyr". RUN Syst/ufcolor.p.
           hakuCustNum = 0.
           ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           DISPLAY lcBrand WITH FRAME hakie.
           UPDATE lcBrand WHEN gcAllBrand
                  hakuCustNum WITH FRAME hakie.
           HIDE FRAME hakie no-pause.

           IF hakuCustNum <> 0 THEN DO:

              FIND FIRST Invoice where 
                         Invoice.Brand  = lcBrand AND
                         Invoice.CustNum >= INPUT hakuCustNum
              no-lock no-error.

              IF NOT fRecFound(3) THEN NEXT BROWSE.

              NEXT LOOP.
           END.
        END. /* Haku sar. 3 */

        /* Haku sarakk. 4 */
        IF toimi = 4 THEN DO:  /* haku sar. 4 */
           InvDate = ?.
           cfc = "puyr". RUN Syst/ufcolor.p.
           hakunetto = 0.
           ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           liPaymState = 0.
           DISPLAY lcBrand WITH FRAME hayr2.
           UPDATE lcBrand WHEN gcAllBrand
                  liPaymState
                  hakunetto 
                  WITH FRAME hayr2.
           HIDE FRAME hayr2 no-pause.

           FIND FIRST Invoice WHERE  
                      Invoice.Brand     = lcBrand AND
                      Invoice.PaymState = liPaymState AND
                      Invoice.InvAmt   >= hakunetto
           USE-INDEX InvAmt NO-LOCK NO-ERROR.
           
           IF NOT AVAILABLE Invoice THEN 
           FIND LAST Invoice WHERE  
                     Invoice.Brand     = lcBrand AND
                     Invoice.PaymState = liPaymState AND
                     Invoice.InvAmt   <  hakunetto
           USE-INDEX InvAmt NO-LOCK NO-ERROR.

           IF NOT fRecFound(4) THEN NEXT BROWSE.
 
           NEXT LOOP.
        END. /* Haku sar. 4 */

     END. /* seek */

     ELSE IF LOOKUP(nap,"3,F3") > 0 THEN DO TRANS: /* memo */
        FIND Invoice WHERE RECID(Invoice) = rtab[FRAME-LINE(sel)]
        NO-LOCK NO-ERROR.
        RUN Mc/memo.p(INPUT Invoice.CustNum,
                 INPUT "Invoice",
                 INPUT STRING(Invoice.InvNum),
                 INPUT "Invoice number").
        ufkey = TRUE.
        NEXT.
     END.

     else if lookup(nap,"2,f2") > 0 THEN DO : /* claiming history */
        FIND Invoice where recid(Invoice) = rtab[FRAME-LINE] no-lock no-error.
        RUN Ar/claimhis.p(0,
                     INPUT Invoice.InvNum).
        ufkey = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 THEN DO : /* payments */
        FIND Invoice where recid(Invoice) = rtab[FRAME-LINE] no-lock no-error.
        RUN Ar/payments.p(0,Invoice.InvNum,"").
        ufkey = TRUE.
        NEXT LOOP.
     END.

     /* view mail send log */
     else if lookup(nap,"7,f7") > 0 THEN DO : 
        FIND Invoice where recid(Invoice) = rtab[FRAME-LINE] no-lock no-error.
        RUN Mc/itsendlo.p(0,
                     Invoice.InvNum,
                     0,
                     0).
        ufkey = TRUE.
        NEXT LOOP.
     END.

     
     else if lookup(nap,"T") > 0 THEN DO : /* time stamps */
        FIND Invoice where recid(Invoice) = rtab[FRAME-LINE] no-lock no-error.
        MESSAGE 
        "Created/Changed" Invoice.ChgStamp "Transferred" Invoice.ExpStamp.
        message "Press ENTER !".
        PAUSE no-message.

        NEXT LOOP.
     END.


     else if lookup(nap,"5,f5") > 0 THEN DO :  /* laskulinet */
        FIND Invoice where recid(Invoice) = rtab[FRAME-LINE] 
        no-lock no-error.

        cfc = "lis". RUN Syst/ufcolor.p.

        /* kysytaan linet */
        RUN Ar/nnlryp.p(Invoice.InvNum,0).
        UFKEY = TRUE.

        NEXT LOOP.
     END.

     else if lookup(nap,"enter,return") > 0 THEN DO:

        FIND Invoice WHERE recid(Invoice) = rtab[frame-line(sel)]
           NO-LOCK NO-ERROR.
        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).

        LP:
        REPEAT WITH FRAME fInvDet
        ON ENDKEY UNDO, NEXT BROWSE:

           /* show details */
           RUN pInvoiceDetails(Invoice.InvNum,
                               FALSE).

           RUN pInvoiceUpdate(Invoice.InvNum). 
            
           IF RETURN-VALUE = "QUIT" THEN LEAVE.     
        END.

        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).
        
        ASSIGN 
           xrecid = recid(Invoice)
           ufkey  = TRUE.
        HIDE FRAME fInvDet NO-PAUSE.

        DISPLAY Invoice.DueDate WITH FRAME sel.

        NEXT LOOP.
     END.


     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST Invoice USE-INDEX ExtInvID
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        ELSE IF order = 2 THEN FIND LAST  Invoice USE-INDEX InvDate
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        ELSE IF order = 3 THEN FIND FIRST Invoice USE-INDEX CustNum
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        ELSE IF order = 4 THEN FIND FIRST Invoice USE-INDEX InvAmt
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        ASSIGN memory = recid(Invoice) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST Invoice USE-INDEX ExtInvID
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        ELSE IF order = 2 THEN FIND FIRST Invoice USE-INDEX InvDate
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        ELSE IF order = 3 THEN FIND LAST Invoice USE-INDEX CustNum
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        ELSE IF order = 4 THEN FIND LAST Invoice USE-INDEX InvAmt
        WHERE Invoice.Brand = lcBrand NO-LOCK no-error.

        DO endloop = 1 TO FRAME-DOWN - 1:
           IF order = 1 THEN FIND PREV Invoice USE-INDEX ExtInvID
           WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
           ELSE IF order = 2 THEN FIND NEXT Invoice USE-INDEX InvDate
           WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
           ELSE IF order = 3 THEN FIND PREV Invoice USE-INDEX CustNum
           WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
           ELSE IF order = 4 THEN FIND PREV Invoice USE-INDEX InvAmt
           WHERE Invoice.Brand = lcBrand NO-LOCK no-error.
        END.

        ASSIGN memory = recid(Invoice) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"4,f4") > 0 THEN DO:
        
        FIND Invoice where recid(Invoice) = rtab[frame-line(sel)] no-lock.
        
        RUN Mc/commontt.p(Invoice.CustNum).
        
        ASSIGN  ufk = 0
                ufk[1]= 94  ufk[2] = 1492 ufk[3] = 927 ufk[4] = 1883
                ufk[5]= 790 ufk[6] = 829 ufk[7] = 1796 ufk[8]= 8 ufk[9]= 1
                ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-disp-row:

   FIND Customer where Customer.CustNum = Invoice.CustNum
   no-lock no-error.
   IF AVAILABLE Customer THEN 
      lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                    BUFFER Customer).
   ELSE lcCustName = "".                                 

   FIND FIRST memo WHERE
              memo.Brand     = Invoice.Brand AND
              memo.custnum   = Invoice.Custnum AND
              memo.HostTable = "Invoice"     AND
              memo.KeyValue  = STRING(Invoice.InvNum)
   NO-LOCK NO-ERROR.
   IF AVAIL memo AND MemoText NE ""
   THEN notes = TRUE. ELSE notes = FALSE.

   DISPLAY Invoice.ExtInvID 
           notes
           Invoice.InvDate
           Invoice.DueDate
           Invoice.InvAmt
           lcCustName 
           Invoice.CustNum
           Invoice.PrintState
           Invoice.PaymState 
           Invoice.Currency
           WITH FRAME sel.

END PROCEDURE.
