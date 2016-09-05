/* --------------------------------------------------
  MODULE .......: NNCILI.P
  FUNCTION .....: List contract payment definitions
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 14.02.99 kl
  MODIFIED .....: 05.03.03 tk tokens
                  16.09.03/aam brand
  Version ......: M15
------------------------------------------------------ */

{Func/excel.i}
{Func/function.i}
{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'fixedfee'}

DEF VAR cname   AS c  NO-UNDO.
def var fname   as c  no-undo format "x(40)".
def var ok      as lo no-undo format "Yes/No".
def var Qty     as i  no-undo format "zzzz9".
DEF VAR CustNum       NO-UNDO LIKE Customer.CustNum.

form
   skip(1)
   "INSTRUCTION:  This module creates as tab separated ascii File of"
   "              customers contract payments due to criteria given"   SKIP
   "              below."                                              skip(3)
   "              Customer ....:" CustNum 
      help "Customer number, 0 = ALL"     Customer.CustName              skip(1) 
   "              File Name ...:" fname
      help "Name of the output file"                                   skip(7)
with centered width 80 no-label 
   title " " + ynimi + "  Contract payment report " FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = UserCode.
   fname = fChkPath(TMSUser.RepDir) + "contract.txt".
END.

disp "ALL" @ Customer.CustName WITH FRAME frm.

CRIT:
repeat WITH FRAME frm:

   HIDE MESSAGE no-pause.

   ehto = 9. RUN Syst/ufkey.p.
   UPDATE 
      CustNum
      fname 
   WITH FRAME frm EDITING:
      READKEY.
      nap = keylabel(LASTKEY).
      IF lookup(nap,poisnap) > 0 THEN DO:
         if frame-field = "CustNum" THEN DO:
            ASSIGN CustNum.
            IF CustNum NE 0 THEN DO:
               FIND FIRST Customer where
                          Customer.Brand   = gcBrand AND
                          Customer.CustNum = CustNum
               no-lock no-error.
               IF AVAIL Customer THEN DISP Customer.CustName WITH FRAME frm.
               ELSE DO:
                  MESSAGE
                     "Customer" string(CustNum) "was not found !"
                  VIEW-AS ALERT-BOX error.
                  NEXT.
               END.
            END.
         END.
      END.
      APPLY LASTKEY.
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

   message "Browsing contract data ...".

   OUTPUT STREAM excel TO value(fname).

   Qty = 0.

   PUT STREAM excel UNFORMATTED
      "CustNr"    tab
      "CustName"  tab
      "ProdCode"  tab
      "Payment"   tab
      "Interval"  tab
      "BegPeriod" tab
      "ExpPeriod" tab
      "InvType"   tab
      "Memo"      my-nl.

   FOR EACH FixedFee no-lock where
            FixedFee.Brand = gcBrand AND
           (IF CustNum NE 0 
            THEN FixedFee.CustNum = CustNum
            ELSE TRUE).

      FIND FIRST Customer where
                 Customer.CustNum = FixedFee.CustNum
      no-lock no-error.

      IF AVAIL Customer THEN cname = Customer.CustName.
      else cname = "-UNKNOWN-".

      PUT STREAM excel UNFORMATTED
         FixedFee.CustNum   tab
         cname           tab
         FixedFee.BillCode tab
         fDecToC(FixedFee.Amt,"-zzzzz9.99")   tab
         FixedFee.Interval    tab
         FixedFee.BegPeriod tab
         FixedFee.EndPeriod tab
         FixedFee.BillMethod   tab
         FixedFee.Memo   my-nl.

      Qty = Qty + 1.

   END. /* FOR EACH */

   OUTPUT STREAM excel CLOSE.

   MESSAGE
      "Totally" Qty "rows listed into file" fname
   VIEW-AS ALERT-BOX MESSAGE.

   HIDE FRAME frm.

   LEAVE.
END.
