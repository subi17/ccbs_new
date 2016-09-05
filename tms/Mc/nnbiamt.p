/* --------------------------------------------------
  MODULE .......: NNBIAMT.P
  FUNCTION .....: Calculate invoices / invoice group
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 07.01.99 kl
  MODIFIED .....: 10.02.00 BY kl: fixing ...
                  17.09.02 jr added help texts
                  17.09.02 jr added state F9 & validate
                  12.09.03/aam brand
  Version ......: M15
------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}

DEF TEMP-TABLE ig NO-UNDO
   FIELD InvGroup   AS c
   FIELD IGName   AS c
   FIELD CreditInvNum    AS lo
   FIELD amt-inv   AS i
   FIELD amt-calls AS i
   FIELD amt-min   AS i
   FIELD amt-canet AS DE
   FIELD amt-contr AS i
   FIELD amt-conet AS DE
   FIELD amt-net   AS DE
   FIELD amt-vat   AS DE
   FIELD amt-round AS DE
   FIELD amt-gross AS DE

   INDEX InvGroup AS primary
      InvGroup
      CreditInvNum.


def var date1     as da no-undo format "99-99-99".
def var date2     as da no-undo format "99-99-99".
DEF VAR cday      AS c  NO-UNDO.
def var fname     as c  no-undo format "x(35)".
def var ok        as lo no-undo format "Yes/No".
DEF VAR amt-calls AS i  NO-UNDO.
DEF VAR amt-min   AS i  NO-UNDO.
DEF VAR amt-canet AS DE NO-UNDO.
DEF VAR amt-conet AS DE NO-UNDO.
DEF VAR amt-contr AS i  NO-UNDO.
DEF VAR c-conet   AS c  NO-UNDO.
DEF VAR c-canet   AS c  NO-UNDO.
DEF VAR c-net     AS c  NO-UNDO.
DEF VAR c-vat     AS c  NO-UNDO.
DEF VAR c-gross   AS c  NO-UNDO.
DEF VAR c-round   AS c  NO-UNDO.
DEF VAR bCredit   AS lo NO-UNDO.

DEF VAR status1   LIKE Invoice.PrintState   NO-UNDO.
DEF VAR status2   LIKE Invoice.PrintState   NO-UNDO.

DEF VAR lcCode  AS CHAR NO-UNDO.
DEF VAR lcField AS CHAR NO-UNDO.
DEF VAR llFind  AS LOG  NO-UNDO.

form
   skip(1)
   "INSTRUCTION:  This module creates a tab separated spreadsheet File of"
   "              all invoices created during the time period determined"   
   "              below. Credited values are on their own rows."     skip(1)
   "              The File will be divided by the invoice groups."   skip(2)
   "              Dates .......:" 
   date1 HELP "Beginning date" "-" 
   date2 HELP "Ending date"  skip(1)
   "              State .......:" status1  
      help "Invoices from status code ..."
   "-" status2 NO-LABEL
      help "Invoices to status code ..."                             skip(1)
   "              FileName ....:" fname                              
      help "Path and Name of the output file"                        skip(4)

with centered width 80 no-label title " Bills per invoice group " FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = katun.
   fname = TMSUser.RepDir + "/billamt.txt".
END.

/* default Date values */
FIND FIRST Invoice no-lock WHERE Invoice.Brand = gcBrand NO-ERROR.
IF AVAILABLE Invoice THEN
ASSIGN   
   date2 = Invoice.InvDate
   date1 = date(month(TODAY),1,year(TODAY)).

CRIT:
repeat WITH FRAME frm:

   HIDE MESSAGE no-pause.

   ehto = 9. RUN Syst/ufkey.p.
   repeat WITH FRAME frm ON ENDKEY UNDO, LEAVE:
      UPDATE 
            date1   
            date2 
            status1 
            status2 
            fname 
      EDITING:
         READKEY.
         IF FRAME-FIELD = "date2" THEN DO:
            IF INPUT date2 < INPUT date1 THEN DO:
               BELL.
               MESSAGE "Check order !".
               NEXT-PROMPT date1.
               next.
            END.
         END.
         IF (FRAME-FIELD = "status1" OR FRAME-FIELD = "status2") THEN
         DO:
            IF keylabel(lastkey) = "F9" THEN 
            DO:
               ASSIGN lcField = FRAME-FIELD.
               RUN Help/h-tmscodes.p(INPUT "Invoice",  /* TableName*/
                                    "PrintState", /* FieldName */
                                    "Report", /* GroupCode */
                              OUTPUT lcCode).
               IF lcCode ne "" THEN 
               DO:
                  IF lcField = "Status1"                                                           THEN DISPLAY INTEGER(lcCode) ;& status1.
                  ELSE IF lcField = "status2"
                  THEN DISPLAY INTEGER(lcCode) ;& status2.
               END.
            END.
                                                                                            IF FRAME-FIELD = "status1" THEN
            DO:
               APPLY LAST-KEY.
               READKEY.
               /* TMSCODE VALIDATE */
               RUN Syst/v-tmscodes.p(INPUT "Invoice",    /* TableName */
                                    "PrintState", /* FieldName */
                                    "Report",     /* GroupCode */
                              INPUT INPUT status1,
                              OUTPUT llFind).
               IF NOT llFind THEN
               DO:
                  NEXT-PROMPT status1.
                  NEXT.
               END.
            END.

            IF FRAME-FIELD = "status2" THEN
            DO:
               APPLY LAST-KEY.
               READKEY.
               /* TMSCODE VALIDATE */
               RUN Syst/v-tmscodes.p(INPUT "Invoice",    /* TableName */
                                    "PrintState", /* FieldName */
                                    "Report",     /* GroupCode */
                              INPUT INPUT status2,
                              OUTPUT llFind).
               IF NOT llFind THEN
               DO:
                  NEXT-PROMPT status2.
                  NEXT.
               END.

               IF INPUT status2 < INPUT status1 THEN 
               DO:
                  BELL.
                  MESSAGE "Check order !".
                  NEXT-PROMPT status1.
                  NEXT.
               END.
            END.    
         END.
         APPLY LASTKEY.
      END.
      LEAVE. 
   END. 

task:
   repeat WITH FRAME frm ON ENDKEY UNDO, RETURN:
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

   message "Browsing invoice data ...".
   OUTPUT STREAM excel TO value(fname).

   FOR EACH Invoice no-lock where 
            Invoice.Brand     = gcBrand  AND
            Invoice.InvDate  >= date1    AND
            Invoice.InvDate  <= date2    AND
            Invoice.PrintState >= status1  AND
            Invoice.PrintState <= status2,
      FIRST Customer of Invoice no-lock,
      EACH InvRow of Invoice no-lock

   BREAK BY InvRow.InvNum:

      IF InvRow.FFRow THEN ASSIGN
         amt-contr = 1
         amt-conet = InvRow.Amt
         amt-calls = 0
         amt-min   = 0
         amt-canet = 0.
      ELSE ASSIGN
         amt-calls = InvRow.Qty
         amt-min   = InvRow.Minutes
         amt-canet = InvRow.Amt
         amt-contr = 0
         amt-conet = 0.

      accumulate
         amt-calls (sub-total BY InvRow.InvNum)
         amt-min   (sub-total BY InvRow.InvNum)
         amt-canet (sub-total BY InvRow.InvNum)
         amt-contr (sub-total BY InvRow.InvNum)
         amt-conet (sub-total BY InvRow.InvNum).

      IF last-of(InvRow.InvNum) THEN DO:
         FIND FIRST ig where
                    ig.InvGroup = Customer.InvGroup       AND
                    ig.CreditInvNum  = (Invoice.InvAmt < 0)
         no-error.
         IF NOT AVAIL ig THEN DO:
            FIND FIRST InvGroup where
                       InvGroup.Brand    = Customer.Brand AND
                       InvGroup.InvGroup = Customer.InvGroup
            no-lock no-error.
            CREATE ig.
            IF AVAIL InvGroup THEN
               ASSIGN
                  ig.InvGroup = InvGroup.InvGroup
                  ig.IGName = InvGroup.IGName.
            ELSE
               ASSIGN
                  ig.InvGroup = "-UNKNOWN-"
                  ig.IGName = "-UNKNOWN-".

         END.
         ASSIGN
            ig.CreditInvNum    = (Invoice.InvAmt < 0)
            ig.amt-inv   = ig.amt-inv   + 1
            ig.amt-calls = ig.amt-calls + 
                           accum sub-total BY InvRow.InvNum amt-calls
            ig.amt-min   = ig.amt-min   + 
                           accum sub-total BY InvRow.InvNum amt-min
            ig.amt-canet = ig.amt-canet + 
                           accum sub-total BY InvRow.InvNum amt-canet 
            ig.amt-contr = ig.amt-contr + 
                           accum sub-total BY InvRow.InvNum amt-contr
            ig.amt-conet = ig.amt-conet + 
                           accum sub-total BY InvRow.InvNum amt-conet 
            ig.amt-net   = ig.amt-net   + Invoice.AmtExclVAT
            ig.amt-gross = ig.amt-gross + Invoice.InvAmt
            ig.amt-vat   = ig.amt-vat   + Invoice.VATAmt
            ig.amt-round = ig.amt-round + Invoice.Rounding.

      END.

   END.

   PUT STREAM excel UNFORMATTED
      "IgCode"    tab
      "IgName"    tab
      "Credit"    tab
      "#Inv"      tab
      "#Calls"    tab
      "#Min"      tab
      "Call Net"  tab
      "#Contr"    tab
      "Contr Net" tab
      "TOTNet"    tab
      "TOTVat"    tab
      "TOTRound"  tab
      "TOTGross"  my-nl.

   FOR EACH ig no-lock:

      ASSIGN
         c-canet = string(ig.amt-canet,"->>>>>>>>9.99")
         c-conet = string(ig.amt-conet,"->>>>>>>>9.99")
         c-net   = string(ig.amt-net,"->>>>>>>>9.99")
         c-vat   = string(ig.amt-vat,"->>>>>>>>9.99")
         c-gross = string(ig.amt-gross,"->>>>>>>>9.99")
         c-round = string(ig.amt-round,"->>>>>>>>9.99")
         substr(c-canet,11,1) = ","
         substr(c-conet,11,1) = ","
         substr(c-net,11,1)   = ","
         substr(c-vat,11,1)   = ","
         substr(c-gross,11,1) = ","
         substr(c-round,11,1) = ",".

      PUT STREAM excel UNFORMATTED
         ig.InvGroup   tab
         ig.IGName   tab
         ig.CreditInvNum format "Yes/No" tab
         ig.amt-inv   tab
         ig.amt-calls tab
         ig.amt-min   tab
         c-canet      tab
         ig.amt-contr tab
         c-conet      tab
         c-net        tab
         c-vat        tab
         c-round      tab
         c-gross      my-nl.

   END.
   MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.
   LEAVE.
END.

