/* --------------------------------------------------
  MODULE .......: NNXORPR.P
  FUNCTION .....: Project & customer list FOR XOR
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 04.03.99 kl
  MODIFIED .....: 
  Version ......: M15
------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}

DEF STREAM excel2.

def var fname1  as c  no-undo format "x(30)".
def var fname2  as c  no-undo format "x(30)".
DEF VAR ok      AS lo NO-UNDO.
DEF VAR Currency AS c  NO-UNDO.      

form
   skip(1)
   "INSTRUCTION:  This module creates two tab separated ascii files of"
   "              all customer data that will be used in XOR files."
   "              Files contain project data and customer data."     skip(5)
   "              Project file name ...:" fname1                     SKIP
   "              Customer file name ..:" fname2                     skip(6)
with centered width 80 no-label title " ISValue per exchange " FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = katun.
   fname1 = TMSUser.RepDir + "/xor-proj.txt".
   fname2 = TMSUser.RepDir + "/xor-cust.txt".
END.


CRIT:
repeat WITH FRAME frm:

   HIDE MESSAGE no-pause.

   ehto = 9. RUN ufkey.
   UPDATE 
      fname1 fname2
   WITH FRAME frm.

task:
   repeat WITH FRAME frm ON ENDKEY UNDO, RETURN:
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

   OUTPUT STREAM excel  TO value(fname1).
   OUTPUT STREAM excel2 TO value(fname2).

   message "Browsing customer data ...".

   FOR EACH Customer no-lock.      

      Currency = string(Customer.CustNum,"999999").

      /* Size digit */
      case Customer.Size:
         when "XL" then Currency = Currency + "0".
         when "L"  then Currency = Currency + "1".
         when "M"  then Currency = Currency + "2".
         when "S"  then Currency = Currency + "3".
      END.
      /* connection digit */
      Currency = Currency + string(Customer.ConnType,"0/5").

      PUT STREAM excel UNFORMATTED
         Currency         tab
         Customer.CustName my-nl.

      PUT STREAM excel2 UNFORMATTED
         Customer.CustNum                    tab
         Customer.CustName                   tab
         fill(tab,11)
         Customer.ConnType format "Dir/InDir"   tab
         Customer.Size                      my-nl.

   END.

END.

OUTPUT STREAM excel  CLOSE.
OUTPUT STREAM excel2 CLOSE.
