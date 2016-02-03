/* -----------------------------------------------
  MODULE .......: nnlryp.p
  FUNCTION .....: Laskulineen yllApito
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 02.12.1996
  changePVM ....: 11.12.1996 pt - pvm:t mukaan displayhin
                  06.02.1997 pt - sallittu linen lisays
                  24.02.1997 pt - muutettu laasno CustNum:ksi jne
                  09.02.1998 kl - memo pAivitys
                  30.03.1998 pt - PARAMETER may_update
                  01.04.1998 pt - VARIABLE 'ok' on DELETE
                  20.10.1998 kl - longer DECIMAL formats
                  22.10.1998 kl - PARAMETER InvRow.InvNum TO nnlrpu
                  24.11.1998 kl - english, PrintState = 9
                  06.05.1999 kl - UPDATE FFItem IF ROW is removed
                  10.05.1999 kl - DO NOT look FOR contract ROW Calls
                  10.11.1999 kl - parameters TO nnlrpu changed (use InvRowId)
                  19.01.2000 kl - DISPLAY formats
                  24.05.2000 jp - (f4) - genesys call line - NOT activated 
                  19.11.2001 kl - common Version FOR call browsing
                  08.01.2002 jp - InstDuePeriod time fixed
                  29.01.2002 lp - added F2 --> ROW sum without VAT
                  12.02.2002 jp - contract(FFItem) browser
                  25.04.2002 tk - eventlogging added
                  10.09.2002 aam  do not allow changing of sums,
                                  deletion not allowed
                  12.11.2002 jr   "InvRow" => "invrow" in memo
                  19.12.2002 jp   rowtype 6 (GPRS) use same call browser than
                                  mobile (2)
                  24.02.2003 aam  prefix to nnlrpu, 
                                  sum without vat (f2) updated
                  03.03.2003 tk   tokens
                  19.03.2003 tk   dynamic memo removed
                  09.03.2003 aam  nnlrbit
                  15.09.2003 aam  brand
                  11.02.2004 aam  VatPerc & RowType added,
                                  FFRow removed, VatIncl to frame title
                  19.03.2004 aam  CLI,
                                  use temptable -> correct order,
                                  irowmcdr instead of nnlrpu2,
                                  irowffee instead of nnlrcoit,
                                  irowsfee instead of nnlrbit
                  28.04.2004 aam  format for accounts
                  06.05.2005 aam  amounts with 3 decimals                
                  12.06.2006 jp   mobcallbr
                  31.01.2007 kl   "CLI" > "MSISDN".
                                  cli 9, prodcode 12 charactres

  Version ......: M15
  shared .......: InvNum (INPUT PARAMETER)
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i}

{Func/faccper.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invrow'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhInvRow AS HANDLE NO-UNDO.
   lhInvRow = BUFFER InvRow:HANDLE.
   RUN StarEventInitialize(lhInvRow).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhInvRow). 
   END.

END.


DEF INPUT PARAMETER iiInvNum    AS INT NO-UNDO.
DEF INPUT PARAMETER iiSubInvNum AS INT NO-UNDO.

DEF VAR may_update   AS LOG             NO-UNDO INIT FALSE.
DEF VAR memory       AS RECID           NO-UNDO.
def var line         as int format "99" NO-UNDO.
DEF VAR delline      AS INT             NO-UNDO.
DEF VAR must-print   AS LOG             NO-UNDO.
DEF VAR must-add     AS LOG             NO-UNDO.
DEF VAR ufkey        AS LOG             NO-UNDO.
DEF VAR fr-header    AS CHAR            NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24 NO-UNDO.
DEF VAR i            AS INT             NO-UNDO.
DEF VAR deletepwd    AS CHAR            NO-UNDO.
DEF VAR xVatAmt      AS DEC             NO-UNDO.
DEF VAR nettowVAT    AS DEC             NO-UNDO.
DEF VAR liVatAcc     AS INT             NO-UNDO. 
DEF VAR ocREasonCode AS CHAR            NO-UNDO.
DEF VAR odtDate1     AS DATE            NO-UNDO.
DEF VAR odtDate2     AS DATE            NO-UNDO.
DEF VAR olAccept     AS LOG             NO-UNDO.

DEF VAR tunimi       LIKE BillItem.BIName.
def var ok           as lo              no-undo format "Yes/No".
DEF VAR xrecid       AS RECID.

DEF TEMP-TABLE ttRow NO-UNDO
    FIELD InvRow   AS RECID
    FIELD Order    AS INT
    INDEX Order Order.
    
FIND Invoice where Invoice.InvNum  = iiInvNum no-lock.
FIND Customer where Customer.CustNum = Invoice.CustNum no-lock.

form
    InvRow.CLI       column-label "MSISDN"     format "x(9)"
    InvRow.BillCode  column-label "Prod.code"  format "x(12)"
    tunimi           column-label "Prod.name"  format "x(9)"
    InvRow.FromDate  column-label "From."      format "99-99-99"
    InvRow.ToDate    column-label "To"         format "99-99-99"
    InvRow.Qty       column-label "Qty"        format "->>>>>>9"
    InvRow.Amt       column-label "Row netto"  format "->>>>>>9.999"
    InvRow.VatPerc   column-label "VAT"        FORMAT ">>9"
    InvRow.RowType   column-label "T"          FORMAT "9"
WITH
    OVERLAY scroll 1 13 DOWN ROW 2 centered
    COLOR value(cfc) TITLE COLOR value(ctc)
    " Rows in invoice nr " + string(Invoice.ExtInvID) + 
       " (VAT " + STRING(Invoice.VatIncl,"Incl./Excl.") + ") "
    FRAME sel.

form /* lisays / change */
    InvRow.BillCode label "Prod. code .."
    tunimi          label "Prod. name .." format "x(24)"
    InvRow.FromDate label "From ........" format "99-99-99"
    InvRow.ToDate   label "To .........." format "99-99-99"
    InvRow.Qty      label "Amount ......" format "zzzzzz9-"
    InvRow.GrossAmt label "Row gross ..." format "->>>>>9.999"
    InvRow.Amt      label "Row net ....." format "->>>>>9.999"
WITH
    OVERLAY ROW 4 centered side-labels 1 col
    COLOR value(cfc) TITLE COLOR value(ctc) fr-header
    FRAME lis.

FORM /* ACCOUNT DATA */
   nettoWVat      LABEL "Row Netto Excl.VAT"  FORMAT "->>,>>>,>>9.999" 
   InvRow.SlsAcc  LABEL "Sales Account ...."  FORMAT ">>>>>>>9" 
   xVatAmt        LABEL "VAT Of Row ......."  FORMAT "->>,>>>,>>9.999"
   liVatAcc       LABEL "VAT Account......."  FORMAT ">>>>>>>9"
   WITH  OVERLAY CENTERED ROW 8 SIDE-LABELS     
   COLOR VALUE(cfc) TITLE COLOR VALUE(cfc) " ACCOUNT DATA "
   FRAME fmore.


form /* memo */
WITH
    OVERLAY ROW 7 centered NO-LABEL
    color value(cfc) title color value(cfc) " Update memo "
    FRAME memo.

FOR EACH InvRow OF Invoice NO-LOCK WHERE
   (IF iiSubInvNum > 0
    THEN InvRow.SubInvNum = iiSubInvNum
    ELSE TRUE)
BY InvRow.CLI
BY InvRow.BillCode
BY InvRow.FromDate:

    CREATE ttRow.
    ASSIGN ttRow.InvRow = RECID(InvRow)
           i            = i + 1
           ttRow.Order  = i.
END.           
           
cfc = "kory". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST ttRow no-error.
IF AVAILABLE ttRow THEN DO:
   ASSIGN
   memory = recid(ttRow)
   must-print = TRUE
   must-add    = FALSE.
END.
ELSE DO:
   BELL.
   message "No rows in this invoice - press ENTER !".
   PAUSE no-message.
   RETURN.
END.

ASSIGN xrecid = ? delline = 0 ufkey = TRUE.

LOOP:
repeat WITH FRAME sel:

   print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND ttRow where recid(ttRow) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         alkaen lineltA delline */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE ttRow THEN DO:

               RUN local-disp-row.

               rtab[FRAME-LINE] = recid(ttRow).
               FIND NEXT ttRow  no-error.
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.

         up FRAME-LINE - 1.
         ASSIGN must-print = FALSE.
         PAUSE 0 no-message.

         /* one page of data has been Printed AND
         mAllA linellA choosea odotellen. */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN ufk = 0
         ufk[1]= 0  ufk[2]= 560 ufk[3]= 927 ufk[4]= 716
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
         ufk[6]= 1561
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         IF NOT may_update THEN ASSIGN ufk[5] = 0.

         RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      CHOOSE ROW InvRow.BillCode ;(uchoose.i;) no-error WITH FRAME sel.
      COLOR DISPLAY value(ccc) InvRow.BillCode WITH FRAME sel.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         message "You are on a empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND ttRow where recid(ttRow) = rtab[1].
            FIND prev ttRow  no-error.
            IF NOT AVAILABLE ttRow THEN DO:
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
               rtab[1] = recid(ttRow)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:

         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND ttRow where recid(ttRow) = rtab[FRAME-DOWN] .
            FIND NEXT ttRow  no-error.
            IF NOT AVAILABLE ttRow THEN DO:
               message "YOU ARE ON THE LAST ROW!".
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
               rtab[FRAME-DOWN] = recid(ttRow).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND ttRow where recid(ttRow) = memory no-error.
         FIND prev ttRow  no-error.

         IF AVAILABLE ttRow THEN DO:
            memory = recid(ttRow).

            /* mennaan tiedostoa taaksepain 1 sivun verran */
            DO line = 1 TO (FRAME-DOWN - 1):
               FIND prev ttRow  no-error.
               IF AVAILABLE ttRow THEN memory = recid(ttRow).
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
            FIND ttRow where recid(ttRow) = memory.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* accounting data */
     ELSE IF LOOKUP(nap,"2,f2") > 0  THEN DO: 

        FIND ttRow WHERE RECID(ttRow) = rtab[FRAME-LINE(sel)].
        FIND InvRow WHERE RECID(InvRow) = ttRow.InvRow NO-LOCK.

        /* IF vat is included THEN remove it */
        IF Invoice.VatIncl THEN ASSIGN 
           xVatAmt = ROUND(InvRow.Amt * InvRow.VatPerc
                                  / (100 + InvRow.VatPerc),3)
           nettowVat = InvRow.Amt - xVatAmt.
        ELSE ASSIGN 
           xVatAmt = ROUND(InvRow.Amt * InvRow.VatPerc / 100,3)
           nettowVat = InvRow.Amt. 

        liVatAcc = 0.
        IF InvRow.VatPerc > 0 THEN DO i = 1 to 10:
           IF Invoice.VatPercent[i] = InvRow.VatPerc
           THEN liVatAcc = Invoice.VatAccount[i].
        END.

        PAUSE 0.
        DISP nettowVat  
             InvRow.SlsAcc
             xVatAmt     
             liVatAcc 
             WITH FRAME fmore.

        ASSIGN 
           ehto = 5
           ufkey = TRUE.
        RUN ufkey.p.
        
        PAUSE MESSAGE "Press ENTER to continue".
        HIDE FRAME fmore NO-PAUSE.      
        NEXT LOOP.
     END.


     ELSE IF LOOKUP(nap,"3,f3") > 0   /* memo */
        THEN DO TRANS WITH FRAME memo ON ENDKEY UNDO, NEXT LOOP:

        ASSIGN ehto = 9 ufkey = TRUE.
        RUN ufkey.
        FIND ttRow WHERE RECID(ttRow) = rtab[FRAME-LINE(sel)].
        FIND InvRow WHERE RECID(InvRow) = ttRow.InvRow EXCLUSIVE-LOCK.

        UPDATE text(InvRow.Memo[1 for 5]) WITH FRAME memo 1 col.
        if keylabel(lastkey) = "F4" then do: 
           hide frame memo no-pause. 
           undo LOOP, next LOOP. 
        end. 
        hide frame memo no-pause.

        NEXT LOOP.
     END.


     else if lookup(nap,"4,f4") > 0 THEN DO: 

        FIND ttRow where recid(ttRow) = rtab[FRAME-LINE].
        FIND InvRow WHERE RECID(InvRow) = ttRow.InvRow NO-LOCK.
 
        case InvRow.RowType:
        
           /* fixed Calls and invseq search */
           when 1 THEN DO:
              ASSIGN ufkey = TRUE.
              RUN nnlrpu.p(Invoice.CustNum,
                         InvRow.FromDate,
                         InvRow.ToDate,
                         InvRow.BillCode,
                         InvRow.InvNum,
                         InvRow.Prefix).
           END.
           /* mobile Calls */
           when 2 OR WHEN 6 THEN DO:
              ASSIGN ufkey = TRUE.

           RUN  mobguard2(INPUT  FALSE,
                            OUTPUT ocReasonCode,
                            OUTPUT odtDate1,
                            OUTPUT odtDate2,
                            output olAccept).
            
            IF olAccept = FALSE THEN NEXT LOOP.

            FIND FIRST SubInvoice OF Invoice WHERE 
               SubInvoice.SubInvNum = InvRow.SubInvNum NO-LOCK NO-ERROR.
            IF AVAILABLE SubInvoice THEN    
              RUN mobcallbr(INPUT  "post",
                            INPUT  InvRow.FromDate,
                            INPUT  InvRow.ToDate,
                            INPUT  Invoice.CustNum,                        
                            INPUT  "inv",
                            INPUT  InvRow.Cli,
                            INPUT  SubInvoice.Invseq,
                            INPUT  Invoice.InvNum,
                            INPUT  InvRow.PreFix,
                            INPUT  InvRow.BillCode,
                            INPUT  "1",
                            INPUT  0,
                            INPUT  0).
                            
           END.

           when 3  THEN DO:
              ASSIGN ufkey = TRUE.
              RUN irowffee(Invoice.InvNum,
                           InvRow.BillCode,
                           InvRow.CLI).
           END.

           when 4  THEN DO:
              ASSIGN ufkey = TRUE.
              RUN irowsfee(Invoice.InvNum,
                           InvRow.BillCode,
                           InvRow.CLI).

           END. 

           otherwise DO:
              message "This is a single contract row ! ( -press ENTER- )".
              PAUSE no-message.
           END.
        END.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"F6,6") > 0 AND ufk[6] > 0 THEN DO:
        FIND ttRow where recid(ttRow) = rtab[FRAME-LINE].
        FIND InvRow WHERE RECID(InvRow) = ttRow.InvRow NO-LOCK.
        FIND FIRST SubInvoice WHERE
                   SubInvoice.InvNum = InvRow.InvNum AND
                   SubInvoice.SubInvNum = InvRow.SubInvNum NO-LOCK NO-ERROR.
        ufkey = TRUE.
        IF AVAILABLE SubInvoice AND SubInvoice.MsSeq > 0 THEN 
           RUN invrowcounter.p(0,
                               SubInvoice.MsSeq,
                               InvRow.InvNum,
                               InvRow.BillCode).
        NEXT LOOP.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO: /* ensimmainen tietue */
        FIND FIRST ttRow .
        ASSIGN
        memory = recid(ttRow)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        FIND LAST ttRow.
        ASSIGN
        memory = recid(ttRow)
        must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.
     
  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-disp-row:

   FIND InvRow WHERE RECID(InvRow) = ttRow.InvRow NO-LOCK.

   FIND BillItem where 
        BillItem.Brand    = Invoice.Brand AND
        BillItem.BillCode = InvRow.BillCode
   no-lock no-error.
   IF AVAIL BillItem THEN tunimi = BillItem.BiName.
   else tunimi = "!! BLANK !!".

   DISPLAY
   InvRow.CLI
   InvRow.BillCode tunimi InvRow.Amt InvRow.FromDate
   InvRow.ToDate InvRow.Qty InvRow.RowType InvRow.VatPerc
   WITH FRAME sel.

END PROCEDURE.

