/* --------------------------------------------------
  MODULE .......: NNCUPA.P
  FUNCTION .....: List customers ALL payments
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 22.12.99 kl
  MODIFIED .....: 03.03.03 tk tokens
  Version ......: M15
------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}
{Func/function.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'payment'}

DEF INPUT PARAM CustNum AS i NO-UNDO.

def var fname as c  no-undo format "x(35)".
def var ok    as lo no-undo format "Yes/No".

DEF BUFFER cust FOR Customer.

FIND FIRST cust where
           cust.CustNum = CustNum
no-lock no-error.           

form
   skip(1)
   "INSTRUCTION:  This module creates as tab separated ascii File of"
   "              all customers payments registered in the database."  skip(5)
   "              Customer ....:" cust.CustNum cust.CustName             skip(1) 
   "              File Name ...:" fname                                skip(6)
with overlay centered width 80 no-label title " List customers payments " 
   FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = Syst.CUICommon:katun.
   fname = fChkPath(TMSUser.RepDir) + string(CustNum) + "paym.txt".
END.

CRIT:
repeat WITH FRAME frm:

   HIDE MESSAGE no-pause.

   Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p.

   DISP
      cust.CustNum
      cust.CustName
   WITH FRAME frm.

   UPDATE 
      fname 
   WITH FRAME frm.

task:
   repeat WITH FRAME frm:
      ASSIGN Syst.CUICommon:ufk = 0 Syst.CUICommon:ufk[1] = 7 Syst.CUICommon:ufk[5] = 63 Syst.CUICommon:ufk[8] = 8 Syst.CUICommon:ehto = 0.
      RUN Syst/ufkey.p.
      IF Syst.CUICommon:toimi = 1 THEN NEXT  CRIT.
      IF Syst.CUICommon:toimi = 4 THEN LEAVE CRIT.
      IF Syst.CUICommon:toimi = 8 THEN LEAVE CRIT.

      IF Syst.CUICommon:toimi = 5 THEN DO:
         ok = FALSE.
         message "Are you SURE you want to start processing (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE task.
      END.
   END.

   OUTPUT STREAM excel TO value(fname).

   /* column header */
   PUT STREAM excel UNFORMATTED
      "CustNr"    tab
      "CustName"  tab
      "Date Paid" tab
      "Voucher"   tab
      "Paid"      tab
      "Invoice"   my-nl.

   /* list payments */
   FOR EACH Payment of cust no-lock.

      PUT STREAM excel UNFORMATTED
         cust.CustNum                            tab
         cust.CustName                           tab
         Payment.PaymDate format "99-99-99"      tab
         Payment.Voucher                        tab
         fDecToC(Payment.PaymAmt,"->>>>>>9.99") tab
         Payment.InvNum                         my-nl.

   END.

   OUTPUT STREAM excel CLOSE.

   LEAVE CRIT.

END.

HIDE FRAME frm no-pause.


