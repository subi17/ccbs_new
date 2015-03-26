/* -----------------------------------------------------------------
  MODULE .......: NNFRIEX.p
  TASK .........: creates an Excel File of Calls TO 020-numbers
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 14.01.1998
  CHANGED ......: 11.09.2002 jp / some fix
  Version ......: M15
  ------------------------------------------------------------------ */

{commali.i} 

DEF VAR exdir     AS c  NO-UNDO.
DEF VAR exName    AS c  NO-UNDO.
DEF VAR exdate1   AS DA NO-UNDO.
DEF VAR exdate2   AS DA NO-UNDO.

DEF NEW shared STREAM excel.

DEF VAR fname    AS CH NO-UNDO.
DEF VAR pvm1     AS DA NO-UNDO.
DEF VAR pvm2     AS DA NO-UNDO.
DEF VAR netto    AS DE NO-UNDO.
DEF VAR CustNum   LIKE Customer.CustNum NO-UNDO.
DEF VAR exhdr    AS c NO-UNDO.
DEF VAR tab      AS c NO-UNDO.
DEF VAR i        AS i NO-UNDO.
DEF VAR c        AS c NO-UNDO.

/* get default direcory Name FOR OUTPUT */
DO FOR TMSUser:
   FIND TMSUser where TMSUser.UserCode = katun no-lock.
   ASSIGN exdir = TMSUser.RepDir.
END.



/* initial values */
tab      = chr(9).
exName   = exdir + "/" + "freecall.txt".
exhdr    = "020-nummer,AntSamt,AntMin,Att Betala".
tab      = chr(9).

form
   skip(1)
"  Instruction:  This program creates an TAB-separated ASCII-file with "  skip
"                information of free Calls to all free B-sub. numbers of" skip
"                a certain customer, within a certain InstDuePeriod of time"     skip
skip(1)
"                File's Name .:" exname format "x(34)" no-label        skip(2)
"                Calls within.:" exdate1 format "99-99-99"
help "Earliest day of call"
"-" exdate2 format "99-99-99" validate(input exdate2 >= input exdate1,
"Check order of dates !") help "Latest day of call"                   skip
"                Customer ....:" CustNum help "Customer's number"
Customer.CustName no-label format "x(20)"
skip(7)
WITH
   width 80 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " SPECIFICATION OF FREE Calls " +
   string(pvm,"99-99-99") + " " NO-LABELS FRAME start.

exdate2 = date(month(TODAY),1,year(TODAY)) - 1.
exdate1 = date(month(exdate2),1,year(exdate2)).

cfc = "sel". RUN ufcolor.

CRIT:
repeat WITH FRAME start:
   ehto = 9. RUN ufkey.
   DISP exName.
   UPDATE
      exname
      exdate1  validate(exdate1 ne ?,"Input earlist day !")
      exdate2  validate(input exdate2 >= input exdate1,"Invalid Order !")
      CustNum
   WITH FRAME start EDITING.
      READKEY.
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
         PAUSE 0.
         if frame-field = "CustNum" THEN DO:
            FIND Customer where Customer.CustNum = 
                          INPUT CustNum no-lock no-error.
            IF NOT AVAIL Customer THEN DO:
               BELL.
               message "Unknown customer !".
               NEXT.
            END.
            DISP Customer.CustName WITH FRAME start.
         END.
      END.
      APPLY LASTKEY.
   END. /* EDITING */

task:
   repeat WITH FRAME start:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN ufkey.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN LEAVE task.
   END.
   message "Sorting and printing ...".


   OUTPUT STREAM excel TO value(exName).

   PUT STREAM excel UNFORMATTED ynimi.  RUN uexskip(2).
   PUT STREAM excel UNFORMATTED
   "Free Calls during " string(exdate1,"99.99.9999") " - "
   string(exdate2,"99.99.9999") " owned by customer "
   string(CustNum) " " Customer.CustName.
   RUN uexskip(2).

   DO i = 1 TO num-entries(exhdr).
      PUT STREAM excel UNFORMATTED entry(i,exhdr) tab.
   END.
   RUN uexskip(2).

   FOR EACH FixCDR no-lock USE-INDEX InvCust where
            FixCDR.Date    >= exdate1 AND
            FixCDR.Date    <= exdate2 AND
            FixCDR.InvCust  = CustNum  AND
            FixCDR.BillCode = "F"
   BREAK
   BY FixCDR.BSub.
      netto = FixCDR.GrossPrice - FixCDR.DiscValue.
      accumulate
      Duration (sub-count BY FixCDR.BSub)
      Duration (sub-total BY FixCDR.BSub)
      netto    (sub-total BY FixCDR.BSub).

      IF last-of(BSub) THEN DO:
         netto = (accum sub-total BY FixCDR.BSub netto).
         c = string(netto,"zzzzz9.99").
         substr(c,7,1) = ",".
         PUT STREAM excel UNFORMATTED
         BSub                                                           tab
         (accum sub-count BY FixCDR.BSub Duration)                       tab
         (accum sub-total by FixCDR.BSub Duration) / 60 format "zzzzzz9" tab
         c.



         RUN uexskip(1).

      END.


   END.
   OUTPUT STREAM excel CLOSE.
   message "File" exName "ready - press ENTER !".
   PAUSE no-message.
   LEAVE.
END.
HIDE FRAME start no-pause.
HIDE MESSAGE.

