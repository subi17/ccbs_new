/* --------------------------------------------------
  MODULE .......: NNMTAMT.P
  FUNCTION .....: Calculate Calls / customer / MONTH
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 15.12.98 kl
  MODIFIED .....: 27.01.99 kl - WORKFILE into TEMP-TABLE
                  11.04.00 kl - tuning    
  Version ......: M15
------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}

DEF VAR Month    AS i  NO-UNDO.
DEF VAR xMonth   AS i  NO-UNDO EXTENT 12.
def var date1  as da no-undo format "99-99-99".
def var date2  as da no-undo format "99-99-99".
DEF VAR per1   AS i  NO-UNDO.
DEF VAR per2   AS i  NO-UNDO.
DEF VAR c-inv  AS c  NO-UNDO.
DEF VAR i      AS i  NO-UNDO.
def var ok     as lo no-undo format "Yes/No".
DEF VAR months AS c  NO-UNDO EXTENT 12.
def var fname  as c  no-undo format "x(30)".

ASSIGN   
   date2 = date(month(TODAY),1,year(TODAY)) - 1
   date1 = date(month(date2),1,year(date2)).

DEF TEMP-TABLE sums
   FIELD CustNum   LIKE Customer.CustNum
   FIELD cust-name LIKE Customer.CustName
   FIELD InvGroup   LIKE Customer.InvGroup
   FIELD c-date    AS   DA
   FIELD Minutes       AS   i               EXTENT 12
   FIELD inv       AS   DE              EXTENT 12

   INDEX CustNum
      CustNum
   INDEX InvGroup
      InvGroup
      CustNum.

ASSIGN
   months[1]  = "JAN"
   months[2]  = "FEM"
   months[3]  = "MAR"
   months[4]  = "APR"
   months[5]  = "MAY"
   months[6]  = "JUN"
   months[7]  = "JUL"
   months[8]  = "AUG"
   months[9]  = "SEP"
   months[10] = "OCT"
   months[11] = "NOV"
   months[12] = "DEC".

form
   skip(1)
   "INSTRUCTION:  This module creates as tab separated ascii File of"
   "              all Calls per customer / month during time peroid"
   "              determined below."                                   skip(4)
   "              Date Period .:" date1 "-" date2                      skip(1)
   "              File Name ...:" fname                              skip(6)
with centered width 80 no-label title " Calls per customer " FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = UserCode.
   fname = TMSUser.RepDir + "/mthQty.txt".
END.

CRIT:
repeat WITH FRAME frm:

   HIDE MESSAGE no-pause.

   ehto = 9. RUN Syst/ufkey.
   UPDATE 
      date1 date2 validate(input date2 >= input date1, "check order !")
      fname 
   WITH FRAME frm.

task:
   repeat WITH FRAME frm ON ENDKEY UNDO, RETURN:
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

   message "Browsing call data ...".

   FOR EACH Customer no-lock where
            Customer.CustNum > 1000.

      CREATE sums.
         ASSIGN
            sums.CustNum   = Customer.CustNum
            sums.cust-name = Customer.CustName
            sums.InvGroup   = Customer.InvGroup
            sums.c-date    = Customer.ContrBeg.

   END.

   ASSIGN
      per1 = month(date1)
      per2 = month(date2).

   FOR EACH FixCDR no-lock where
            FixCDR.Date >= date1   AND
            FixCDR.Date <= date2,

      FIRST InvSeq no-lock where
            InvSeq.InvSeq = FixCDR.InvSeq AND
            InvSeq.Billed = TRUE.

      FIND FIRST sums where
                 sums.CustNum = FixCDR.InvCust
      USE-INDEX CustNum no-error.

      IF NOT AVAIL sums THEN NEXT.

      ASSIGN
         Month               = month(FixCDR.Date)
         sums.Minutes[Month] = sums.Minutes[Month] + FixCDR.Duration
         sums.inv[Month]     = sums.inv[Month] + FixCDR.GrossPrice - 
                                                 FixCDR.DiscValue.

   END.

   OUTPUT STREAM excel TO value(fname).

   PUT STREAM excel UNFORMATTED
      "Calls between " string(date1,"99-99-99") " - " string(date2,"99-99-99")
      my-nl.

   /* Headers */
   PUT STREAM excel UNFORMATTED
      "CustNr"    tab
      "CustName"  tab
      "IgCode"    tab
      "ContrDate" tab.

   /* Calculated months */
   DO i = per1 TO per2.
      PUT STREAM excel UNFORMATTED
         months[i] ": Min" tab "Value" tab.
   END.
   PUT STREAM excel UNFORMATTED my-nl.

   /* print calculated values */
   FOR EACH sums no-lock USE-INDEX InvGroup
   BY sums.InvGroup
   BY sums.CustNum.

      PUT STREAM excel UNFORMATTED
         sums.CustNum   tab
         sums.cust-name tab
         sums.InvGroup   tab
         string(sums.c-date,"99-99-99") tab.

      DO i = per1 TO per2.
         PUT STREAM excel UNFORMATTED
            string(sums.Minutes[i] / 60,"zzzzzz9") tab.
         ASSIGN
            c-inv = string(sums.inv[i],">>>>>>>>>9.99")
            substr(c-inv,11,1) = ",".
         PUT STREAM excel UNFORMATTED
            c-inv tab.
      END.

      PUT STREAM excel UNFORMATTED my-nl.

   END.

   OUTPUT STREAM excel CLOSE.

   MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

   LEAVE.

END.

