/* -----------------------------------------------------------------
  MODULE .......: NNBPEX.P
  TASK .........: creates an Excel File of Billed Products
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 12.08.1998
  CHANGED ......: 12.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------------------ */

{Syst/commali.i}          

DEF VAR exdir     AS c  NO-UNDO.
DEF VAR exName    AS c  NO-UNDO.
DEF VAR ProdName  AS c  NO-UNDO.
DEF VAR exdate1   AS DA NO-UNDO.
DEF VAR exdate2   AS DA NO-UNDO.
def var exasno    as i  no-undo format "zzzzzz9".
DEF VAR exdec     AS c  NO-UNDO.
def var exdeci    as lo format "Period/Comma" NO-UNDO.
DEF VAR exConnType    LIKE Customer.ConnType NO-UNDO.
def var ok        as lo format "Yes/No"      NO-UNDO.
DEF VAR a-hdr     AS c  NO-UNDO.
DEF VAR i         AS i  NO-UNDO.
DEF VAR ptot      AS DE NO-UNDO.
DEF VAR csum      AS c  NO-UNDO.
DEF VAR tab       AS c  NO-UNDO.
DEF VAR InvGroup   LIKE  InvGroup.InvGroup.
DEF VAR totint    AS DE NO-UNDO.
DEF VAR totrou    AS DE NO-UNDO.

DEF NEW shared STREAM excel.

/* get default direcory Name FOR OUTPUT */
DO FOR TMSUser:
   FIND TMSUSer where TMSUser.UserCode = katun no-lock.
   ASSIGN exdir = TMSUSer.RepDir.
END.

/* initial values */
tab      = chr(9).
exName   = exdir + "/" + "billprod.txt".

ASSIGN a-hdr =
   "ProdCode,ProdName,Billed Value".

form
   skip(1)
"  Information:  This program writes a summary of all Billed PRODUCTS"
"                from invoices that are WRITTEN within"
"                the time Period given below."
                 SKIP
skip(1)
"                Summary is written as a TAB-separated ASCII-file,"
"                one row / BillCode."
skip(1)
"                File can be opened in Ms Excel as it is."  skip(1)
"                File's Name is" exname format "x(35)" no-label skip(1)
"                Invoice's time period:" exdate1 format "99-99-99"
help "Earliest invoicing date"
"-" exdate2 format "99-99-99" help "Latest invoicing date"   skip
"                Invoicing group .....:" InvGroup
help "Invoicing group's code, empty for all"  SKIP
"                Connection ..........:" exConnType
help "(D)irect, (I)ndirect  (?)=ALL" SKIP
"                Decimal separator ...:" exdeci help "Period/Comma" skip

 skip(1)
WITH
   width 80 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " EXCEL-SUMMARY OF Billed PRODUCTS " +
   string(pvm,"99-99-99") + " " NO-LABELS FRAME start.

exdate2 = date(month(TODAY),1,year(TODAY)) - 1.
exdate1 = date(month(exdate2),1,year(exdate2)).

cfc = "sel". RUN Syst/ufcolor.
exConnType = ?.  InvGroup = "T1E".

CRIT:
repeat WITH FRAME start:
   ehto = 9. RUN Syst/ufkey.
   DISP exName.
   UPDATE
      exName
      exdate1  validate(exdate1 ne ?,"Give first Date !")
      exdate2  validate(input exdate2 >= input exdate1,"Invalid order !")
      InvGroup  validate(InvGroup = "" OR can-find(InvGroup where
                                 InvGroup.Brand  = gcBrand AND
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
      RUN Syst/ufkey.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are you SURE you want to start processing (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE task.
      END.
   END.
   OUTPUT STREAM excel TO value(exName).

   /* headers FIRST */
   if InvGroup ne "" THEN DO:
      FIND InvGroup NO-LOCK where 
           InvGroup.Brand  = gcBrand AND
           InvGroup.InvGroup = InvGroup.
   END.   
   PUT STREAM excel UNFORMATTED ynimi.
   RUN Syst/uexskip(1).
   put stream excel unformatted "Invoicing group: ".
   if InvGroup ne "" THEN PUT STREAM excel UNFORMATTED
      InvGroup.InvGroup + " - " + InvGroup.IGName.
   else put stream excel unformatted "ALL".
   RUN Syst/uexskip(1).
   PUT STREAM excel UNFORMATTED
  "Summary of all Billed products within time Period " +
   string(exdate1,"99.99.9999") " - " string(exdate2,"99.99.9999").
   RUN Syst/uexskip(2).
   put stream excel unformatted "ProdCode" tab "ProdName" tab "BilledAmt".
   RUN Syst/uexskip(2).

   message "Browsing and sorting data ...".




   FOR
      EACH  Invoice no-lock where
            Invoice.Brand  = gcBrand AND
            Invoice.InvDate >= exdate1  AND
            Invoice.InvDate <= exdate2,
      FIRST Customer no-lock where
            Customer.CustNum = Invoice.CustNum AND
            (IF exConnType  NE ?  THEN Customer.ConnType    = exConnType  ELSE TRUE) AND
            (if InvGroup ne "" THEN Customer.InvGroup = InvGroup ELSE TRUE):

      totint = totint + InterestAmt.
      totrou = totrou + Rounding.

   END.

   FOR
      EACH  Invoice no-lock where
            Invoice.Brand  = gcBrand AND
            Invoice.InvDate >= exdate1  AND
            Invoice.InvDate <= exdate2,
      FIRST Customer no-lock where
            Customer.CustNum = Invoice.CustNum AND
            (IF exConnType  NE ?  THEN Customer.ConnType    = exConnType  ELSE TRUE) AND
            (if InvGroup ne "" THEN Customer.InvGroup = InvGroup ELSE TRUE),
      EACH InvRow of Invoice no-lock

   BREAK
   BY InvRow.BillCode
   BY InvRow.InvNum.


      accumulate InvRow.Amt (sub-total BY InvRow.BillCode).

      IF last-of(InvRow.BillCode) THEN DO:
         FIND BillItem where 
              BillItem.Brand  = gcBrand AND
              BillItem.BillCode = InvRow.BillCode no-lock no-error.
         IF AVAIL BillItem THEN ProdName = BillItem.BIName.
         else                ProdName = "!! UNKNOWN !!".
         ptot = (accum sub-total BY InvRow.BillCode InvRow.Amt).
         csum = string(ptot,"-zzzzzzzz9.99").
         /* convert sum into CHAR AND substitute dec point BY comma */
         if not exdeci then substr(csum,11,1) = ",".

         PUT STREAM excel UNFORMATTED
         InvRow.BillCode                  tab
         ProdName                        tab
         csum.
         RUN Syst/uexskip(1).
      END.

   END.

   /* In addition we show tot amts of Interest AND rounding */
   IF totint NE 0 THEN DO:
      csum = string(totint,"-zzzzzzzz9.99").
      if not exdeci then substr(csum,11,1) = ",".
      PUT STREAM excel UNFORMATTED
      tab /* no BillCode */
      "Övertidsränta"  tab csum .
      RUN Syst/uexskip(1).
   END.

   IF totrou NE 0 THEN DO:
      csum = string(totrou,"-zzzzzzzz9.99").
      if not exdeci then substr(csum,11,1) = ",".
      PUT STREAM excel UNFORMATTED
      tab /* no BillCode */
      "Öresutjämning"  tab csum .
      RUN Syst/uexskip(1).
   END.

   OUTPUT STREAM excel CLOSE.
   message "File is ready - press ENTER !".
   PAUSE no-message.
   LEAVE.
END.
HIDE FRAME start no-pause.

