/* -----------------------------------------------------------------
  MODULE .......: NNFSEX.P
  TASK .........: creates an Excel File of invoices
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 22.10.1997
  CHANGED ......: 07.11.1997 pt get default OUTPUT dir from user File
                  23.03.1998 pt user can UPDATE exName prior TO RUN
                  05.08.1998 pt ConnType
                  21.09.1998 kl limitation TO 40 accounts
                  28.05.2002 aam use TMSParam instead of nnohti
                  01.10.2002 aam new vat handling 
  Version ......: M15
  ------------------------------------------------------------------ */

{commali.i}  
{tmsparam2.i}

DEF VAR exdir     AS c  NO-UNDO.
DEF VAR exName    AS c  NO-UNDO.
DEF VAR exdate1   AS DA NO-UNDO.
DEF VAR exdate2   AS DA NO-UNDO.
def var exasno    as i  no-undo format "zzzzzz9".
DEF VAR exdec     AS c  NO-UNDO.
def var exdeci    as lo format "Period/Comma" NO-UNDO.
DEF VAR exConnType    LIKE Customer.ConnType NO-UNDO.
def var ok        as lo format "Yes/No"      NO-UNDO.
DEF VAR a-hdr     AS c  NO-UNDO.
DEF VAR a-order   AS i  NO-UNDO EXTENT 40.
DEF VAR po        AS i  NO-UNDO.
DEF VAR acct      AS c  NO-UNDO.
DEF VAR i         AS i  NO-UNDO.
DEF VAR csum      AS c  NO-UNDO.
DEF VAR tab       AS c  NO-UNDO.
DEF VAR xsum      AS DE NO-UNDO EXTENT 15.
DEF VAR xacct     AS i  NO-UNDO EXTENT 15.
DEF VAR InvGroup   LIKE  InvGroup.InvGroup.

DEF NEW shared STREAM excel.

DEF WORKFILE winvoice
   FIELD  wlanro    LIKE Invoice.InvNum
   FIELD  wdate     AS DA
   FIELD  wsum      AS DE  EXTENT 40.

DEF WORKFILE worder
   FIELD acct       AS c
   FIELD colu       AS i.


/* get default direcory Name FOR OUTPUT */
DO FOR TMSUser:
   FIND TMSUser where TMSUser.UserCode = katun no-lock.
   ASSIGN exdir = TMSUser.RepDir.
END.



/* initial values */
tab      = chr(9).
exName   = exdir + "/" + "fakkonto.txt".

assign a-hdr = string(fCParamI("ReceivAcc")) + "," +
               string(fCParamI("VatAcc"))    + "," +
               string(fCParamI("RoundAcc"))  + "," +
               string(fCParamI("OTIntAcc"))  + ",".

form
   skip(1)
"  Information:  This program writes a summary of all invoices in"
"                customer AccNum receivable that are written within"
"                the time InstDuePeriod given below."
                 SKIP
skip(1)
"                Summary is written as a TAB-separated ASCII-file,"
"                one row / invoice."
skip(1)
"                File can be opened in Ms Excel as it is."  skip(1)
"                File's Name is" exname format "x(35)" no-label skip(1)
"                Invoice's time InstDuePeriod:" exdate1 format "99-99-99"
help "Earliest invoicing date"
"-" exdate2 format "99-99-99" help "Latest invoicing date"   skip
"                Inv.group ...........:" InvGroup
help "Invoicing group's code, empty for all"  SKIP
"                Connection ..........:" exConnType
help "(D)irect, (I)ndirect  (?)=ALL" SKIP
"                Decimal separator ...:" exdeci help "Period/Comma" skip

 skip(1)
WITH
   width 80 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " EXCEL-SUMMARY OF ACCOUNTS " +
   string(pvm,"99-99-99") + " " NO-LABELS FRAME start.

exdate2 = date(month(TODAY),1,year(TODAY)) - 1.
exdate1 = date(month(exdate2),1,year(exdate2)).

cfc = "sel". RUN ufcolor.

CRIT:
repeat WITH FRAME start:
   ehto = 9. RUN ufkey.
   DISP exName.
   UPDATE
      exName
      exdate1  validate(exdate1 ne ?,"Give first Date !")
      exdate2  validate(input exdate2 >= input exdate1,"Invalid order !")
      InvGroup  validate(InvGroup = "" OR can-find(InvGroup where
                                 InvGroup.InvGroup = InvGroup),
                                 "Group does not exist !")
      exConnType
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
      RUN ufkey.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are you SURE you want to start processing (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE task.
      END.
   END.

   message "Browsing and sorting data ...".
   FOR
      EACH  Invoice no-lock where
            Invoice.InvDate >= exdate1        AND
            Invoice.InvDate <= exdate2,
      FIRST Customer no-lock where
            Customer.CustNum = Invoice.CustNum AND
            (IF exConnType  NE ?  THEN Customer.ConnType    = exConnType  ELSE TRUE) AND
            (if InvGroup ne "" THEN Customer.InvGroup = InvGroup ELSE TRUE),
      EACH InvRow of Invoice no-lock

   BREAK
   BY Invoice.InvNum
   BY InvRow.BillCode.


      IF first-of(Invoice.InvNum) THEN DO:

         CREATE winvoice.
         ASSIGN
         winvoice.wlanro = Invoice.InvNum
         winvoice.wdate  = Invoice.InvDate.

         /* accounts AND sums on invoice header record */
         ASSIGN
         xsum    = 0                        xacct    = 0
         xsum[1] = 0 - Invoice.Rounding     xacct[1] = Invoice.RoundAccNum
         xsum[2] = 0 - Invoice.InterestAmt  xacct[2] = Invoice.IntAccNum
         xsum[3] =     Invoice.InvAmt       xacct[3] = Invoice.ARAccNum
         xsum[4] = 0 - Invoice.AdvPaym      xacct[4] = Invoice.APAccNum
         xsum[5] = 0 - Invoice.OverPaym     xacct[5] = Invoice.OPAccNum.

         DO i = 1 TO 10:
            ASSIGN xsum[i + 5]  = 0 - Invoice.VatAmount[i]
                   xacct[i + 5] = Invoice.VatAccount[i].
         END. 

         DO i = 1 TO 15.
            acct = string(xacct[i],"zzzz").
            if acct = "" THEN NEXT.

            /* build an INDEX FOR AccNum numbers */
            repeat:
               po = lookup(acct,a-hdr).
               IF po > 0 THEN LEAVE.
               a-hdr = a-hdr + acct + ",".
            END.

            /* now 'po' points TO the column of this AccNum number */
            IF po > 40 THEN DO:
               BELL. 
               message "Too many accounts (more than 40) " + 
                       "! - processig will be interrupted !".
               message "Press ENTER !".
               PAUSE no-message.
               RETURN.
            END.
            ASSIGN winvoice.wsum[po] = winvoice.wsum[po] + xsum[i].

         END.
      END.

      FIND FIRST winvoice where winvoice.wlanro = Invoice.InvNum.

      acct = string(InvRow.SlsAccNum,"zzzz").
      if acct = "" THEN NEXT.

      /* build an INDEX FOR AccNum numbers */
      repeat:
         po = lookup(acct,a-hdr).
         IF po > 0 THEN LEAVE.
         a-hdr = a-hdr + acct + ",".
      END.

      /* now 'po' points TO the column of this AccNum number */
      IF po > 40 THEN DO:
         BELL. 
         message "Too many accounts (more than 40)" +
                 "! - processing will be interrupted !".
         message "Press ENTER !".
         PAUSE no-message.
         RETURN.
      END.
      ASSIGN winvoice.wsum[po] = winvoice.wsum[po] - InvRow.Amt.

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
   if InvGroup ne "" THEN DO:
      FIND InvGroup where InvGroup.InvGroup = InvGroup.
   END.   
   PUT STREAM excel UNFORMATTED ynimi.
   RUN uexskip(1).
   put stream excel unformatted "Invoicing group: ".
   if InvGroup ne "" THEN PUT STREAM excel UNFORMATTED
      InvGroup.InvGroup + " - " + InvGroup.IGName.
   else put stream excel unformatted "ALL".
   RUN uexskip(1).
   PUT STREAM excel UNFORMATTED
  "Summary of all invoices in customer AccNum receivable within time InstDuePeriod " +
   string(exdate1,"99.99.9999") " - " string(exdate2,"99.99.9999").
   RUN uexskip(2).
   put stream excel unformatted "Inv.nr." tab "Date" tab.
   DO i = 1 TO num-entries(a-hdr) - 1.
      PUT STREAM excel UNFORMATTED entry(a-order[i],a-hdr) tab.
   END.
   RUN uexskip(2).

   /* daily values */
   FOR EACH winvoice
   BY winvoice.wdate
   BY winvoice.wlanro.


      PUT STREAM excel UNFORMATTED
      string(winvoice.wlanro)             tab
      string(winvoice.wdate,"99.99.9999") tab.

      DO i = 1 TO num-entries(a-hdr) - 1.
         IF winvoice.wsum[a-order[i]] NE 0 THEN DO:

            /* convert sum into CHAR AND substitute dec point BY comma */
            csum = string(winvoice.wsum[a-order[i]],"->>>>>>>9.99").
            if not exdeci then substr(csum,10,1) = ",".

            PUT STREAM excel UNFORMATTED csum.

         END.
         PUT STREAM excel UNFORMATTED tab.
      END.
      RUN uexskip(1).

   END.
   OUTPUT STREAM excel CLOSE.

   LEAVE.
END.
HIDE FRAME start no-pause.

