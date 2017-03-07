/* -----------------------------------------------------------------
  MODULE .......: NNBSEX.P
  TASK .........: creates an Excel File of daily payments
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 22.10.1997
  CHANGED ......: 07.11.1997 pt default OUTPUT dir from user record
                  23.05.1999 pt OUTPUT File Name updateable
  Version ......: M15
  ------------------------------------------------------------------ */

{Syst/commali.i} 

DEF VAR exdir     AS c  NO-UNDO.
DEF VAR exName    AS c  NO-UNDO.
DEF VAR exdate1   AS DA NO-UNDO.
DEF VAR exdate2   AS DA NO-UNDO.
def var exasno    as i  no-undo format "zzzzzz9".
DEF VAR exdec     AS c  NO-UNDO.
def var exdeci    as lo format "Period/Comma" NO-UNDO.
def var ok        as lo format "Yes/No"      NO-UNDO.
DEF VAR a-hdr     AS c  NO-UNDO.
DEF VAR a-order   AS i  NO-UNDO EXTENT 30.
DEF VAR po        AS i  NO-UNDO.
DEF VAR acct      AS c  NO-UNDO.
DEF VAR i         AS i  NO-UNDO.
DEF VAR csum      AS c  NO-UNDO.
DEF VAR tab       AS c  NO-UNDO.
DEF VAR InvGroup   LIKE  InvGroup.InvGroup.

DEF NEW shared STREAM excel.

DEF WORKFILE wday
   FIELD  wdate     AS DA
   FIELD  wsum      AS DE  EXTENT 30.

DEF WORKFILE worder
   FIELD acct       AS c
   FIELD colu       AS i.

/* get default direcory Name FOR OUTPUT */
DO FOR TMSUSer:
   FIND TMSUSer where TMSUSer.UserCode = katun no-lock.
   ASSIGN exdir = TMSUSer.RepDir.
END.

/* initial values */
tab      = chr(9).
exName = exdir + "/" + "betkonto.txt".

form
   skip(1)
"  Information:  This program writes a summary of all payments in customer"
"                AccNum receivable within the time Period given below."
skip(1)
"                Summary is written as a TAB-separated ASCII-file, one"
"                row / day and payments for each AccNum on separate columns."
skip(1)
"                File can be opened in Ms Excel as is is."  skip(1)
"                File is Called " exName format "x(30)" no-label skip(2)
"                Payments within .......:" exdate1 format "99-99-99"
help "Earliest payment date"
"-" exdate2 format "99-99-99" help "Latest payment date"   skip
"                Invoice group..........:" InvGroup
help "Invoice group's code, empty for all" SKIP
"                Decimal separator .....:" exdeci help "Period/Comma" skip(2)
WITH
   width 80 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " EXCEL-SUMMARY OF ACCOUNTS " +
   string(pvm,"99-99-99") + " " NO-LABELS FRAME start.

exdate2 = date(month(TODAY),1,year(TODAY)) - 1.
exdate1 = date(month(exdate2),1,year(exdate2)).

cfc = "sel". RUN Syst/ufcolor.p.

CRIT:
repeat WITH FRAME start:
   ehto = 9. RUN Syst/ufkey.p.
   DISP exName.
   UPDATE
      exName
      exdate1  validate(exdate1 ne ?,"Give first Date !")
      exdate2  validate(input exdate2 >= input exdate1,"Invalid order !")
      InvGroup  validate(InvGroup = "" OR
                        can-find(InvGroup where InvGroup.InvGroup = InvGroup),
                        "Group does not exist!")
      exdeci
   WITH FRAME start EDITING.
      READKEY.
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
         PAUSE 0.
      END.
      APPLY LASTKEY.
   END. /* EDITING */

task:
   repeat WITH FRAME start:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.p.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are you SURE you want to start processing (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE task.
      END.
   END.

   message "Browsing and sorting data...".
   FOR
      EACH   Payment no-lock where
             Payment.PaymDate >= exdate1       AND
             Payment.PaymDate <= exdate2,
      FIRST  Customer  no-lock where
             Customer.CustNum = Payment.CustNum AND
             (if InvGroup ne "" THEN Customer.InvGroup = InvGroup
             ELSE TRUE)

   BREAK
   BY Payment.PaymDate:

      IF first-of(Payment.PaymDate) THEN DO:
         CREATE wday.
         ASSIGN wday.wdate = Payment.PaymDate.
      END.

      FIND FIRST wday where wday.wdate = Payment.PaymDate.

      DO i = 1 TO 10.
         acct = string(Payment.AccNum[i],"zzzz").
         if acct = "" THEN NEXT.

         /* build an INDEX FOR AccNum numbers */
         repeat:
            po = lookup(acct,a-hdr).
            IF po > 0 THEN LEAVE.
            a-hdr = a-hdr + acct + ",".
         END.

         /* now 'po' points TO the column of this AccNum number */
         IF po > 30 THEN DO:
            BELL. MESSAGE
               "Too many accounts (more than 30) ! - processing interrupted !".
            message "Press ENTER !".
            PAUSE no-message.
            RETURN.
         END.
         ASSIGN wday.wsum[po] = wday.wsum[po] + Payment.Posting[i].
      END.
   END.

   /* make a sorted list of AccNum numbers */
   DO i = 1 TO num-entries(a-hdr) - 1.
      CREATE worder.
      ASSIGN worder.colu = i worder.acct = entry(i,a-hdr).
   END.
   i = 0.
   FOR EACH worder BY worder.acct:
      i = i + 1.
      a-order[i] = worder.colu.
   END.

   message "Writing the File ...".

   OUTPUT STREAM excel TO value(exName).

   /* headers FIRST */
   FIND InvGroup where InvGroup.InvGroup = InvGroup no-lock no-error.
   PUT STREAM excel UNFORMATTED ynimi.
   RUN Syst/uexskip.p(1).

   put stream excel unformatted "Invoicing group: ".
   if InvGroup ne "" THEN PUT STREAM excel UNFORMATTED
      InvGroup.InvGroup + " - " + InvGroup.IGName.
   else put stream excel unformatted "ALL".
   RUN Syst/uexskip.p(1).

   PUT STREAM excel UNFORMATTED
  "Summary of all payments in customer AccNum receivable within time Period " +
   string(exdate1,"99.99.9999") " - " string(exdate2,"99.99.9999").
   RUN Syst/uexskip.p(2).

   put stream excel unformatted "Date" tab.
   DO i = 1 TO num-entries(a-hdr) - 1.
      PUT STREAM excel UNFORMATTED entry(a-order[i],a-hdr) tab.
   END.
   RUN Syst/uexskip.p(2).

   /* daily values */
   FOR EACH wday BY wday.wdate.
      put stream excel unformatted string(wday.wdate,"99.99.9999") tab.
      DO i = 1 TO num-entries(a-hdr) - 1.
         IF wday.wsum[a-order[i]] NE 0 THEN DO:

            /* convert sum into CHAR AND substitute dec point BY comma */
            csum = string(wday.wsum[a-order[i]],"->>>>>>>9.99").
            if not exdeci then substr(csum,10,1) = ",".

            PUT STREAM excel UNFORMATTED csum.

         END.
         PUT STREAM excel UNFORMATTED tab.
      END.
      RUN Syst/uexskip.p(1).
   END.
   OUTPUT STREAM excel CLOSE.
   PAUSE 0.
   message "File is ready - press ENTER !".
   PAUSE no-message.
   LEAVE.
END.
HIDE FRAME start no-pause.

