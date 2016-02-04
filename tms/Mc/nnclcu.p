/* ---------------------------------------------------
  MODULE .......: NNCLCU
  FUNCTION .....: List closed customer & their a-numbers
  SOVELLUTUS ...: NN Tele1
  AUTHOR .......: KL
  CREATED ......: 10.11.98
  MODIFIED .....: 08.07.99 kl default directory
  Version ......: M15
------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}

DEF VAR line    AS c  NO-UNDO.
def var exFile  as c  no-undo format "x(30)".
def var ok      as lo no-undo format "Yes/No".
DEF VAR btman   AS lo NO-UNDO.

DEF VAR date1   LIKE FixCDR.Date NO-UNDO.
DEF VAR date2   LIKE FixCDR.Date NO-UNDO.

DEF WORKFILE sums
   FIELD BillCode  AS c
   FIELD pname AS c
   FIELD Qty   AS i
   FIELD amts  AS DE
   FIELD psec  AS DE
   FIELD osec  AS DE
   FIELD inv   AS DE
   FIELD tman  AS lo.

form
   skip(1)
   "INSTRUCTION:  This module creates as tab separated ascii File of"
   "              all customer whose contract have been ended during"
   "              the time Period determined below."                   skip(4)
   "              From Date ...:" date1                                SKIP
   "              To Date .....:" date2                                SKIP
   "              File Name ...:" exFile                               skip(6)
with centered width 80 no-label title " Closed customers " FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = katun.
   exFile = TMSUser.RepDir + "/contrend.txt".
END.

CRIT:
repeat WITH FRAME frm:

   HIDE MESSAGE no-pause.

   ehto = 9. RUN Syst/ufkey.
   UPDATE 
      date1 
      date2 validate(input date2 >= input date1,"Invalid order !")
      exFile 
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

   message "Browsing customer data ...".

   OUTPUT STREAM excel TO value(exFile).

   PUT STREAM excel UNFORMATTED
      "A-numbers for customers whose contract ended between: " + 
      string(date1) + " - " + string(date2)
      my-nl.

   PUT STREAM excel UNFORMATTED
      "Cust. nr"       tab
      "Cust. name"     tab
      "A-number"       tab
      "Contract ended" my-nl.

   FOR EACH Customer no-lock where
            Customer.ContrEnd >= date1 AND
            Customer.ContrEnd <= date2,
       EACH CLI  no-lock where
            CLI.CustNum = Customer.CustNum.

       PUT STREAM excel UNFORMATTED
          Customer.CustNum  tab
          Customer.CustName tab
          CLI.CLI   tab
          Customer.ContrEnd my-nl.
   END.

   OUTPUT STREAM excel CLOSE.

END. /* CRIT */

