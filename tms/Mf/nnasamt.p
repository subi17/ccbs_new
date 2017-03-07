/* --------------------------------------------------
  MODULE .......: NNASAMT.P
  FUNCTION .....: Calculate customers calls per CLI number
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 08.03.00 kl
  MODIFIED .....: 03.03.03 tk tokens
                  18.03.03 tk did not update calls.valc
  Version ......: M15
------------------------------------------------------ */

{Func/function.i}
{Func/excel.i}
{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'cli'}


DEF INPUT PARAMETER CustNum AS i NO-UNDO. 

/*
DEF VAR CustNum AS i NO-UNDO init 10000.
*/

def var date1   as da no-undo format "99-99-99".
def var date2   as da no-undo format "99-99-99".
def var fname   as c  no-undo format "x(30)".
def var ok      as lo no-undo format "Yes/No".
DEF VAR amttot  AS i  NO-UNDO.
DEF VAR amtasub AS i  NO-UNDO.

DEF TEMP-TABLE calls NO-UNDO
   FIELD CLI AS c
   FIELD BillCode AS c
   FIELD amtc AS i
   FIELD valc AS DE
   FIELD amtm AS i

   INDEX CLI AS primary
      CLI
      BillCode.

form
   skip(1)
   "INSTRUCTION:  This module creates as tab separated ascii File of"
   "              all customers calls per CLI number during time"
   "              Period determined below."                            skip(4)
   "              Customer ....:" Customer.CustNum Customer.CustName       SKIP
   "              Dates .......:" date1 "-" date2                      SKIP
   "              File Name ...:" fname                                skip(5)
WITH 
   centered ROW 2 width 80 NO-LABEL OVERLAY 
   title " calls per CLI report " FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = UserCode.
   fname = TMSUser.RepDir + "/asubamt.txt".
END.

ASSIGN   
   date2 = date(month(TODAY),1,year(TODAY)) - 1
   date1 = date(month(date2),1,year(date2)).

FIND FIRST Customer where 
           Customer.CustNum = CustNum
no-lock no-error.

CRIT:
repeat WITH FRAME frm:

   HIDE MESSAGE no-pause.

   ehto = 9. RUN Syst/ufkey.p.
   DISP Customer.CustNum Customer.CustName WITH FRAME frm.
   UPDATE 
      date1 date2
      fname 
   WITH FRAME frm.

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

   message "Browsing call data ...".

   FOR EACH CLI no-lock where
            CLI.CustNum = CustNum,

       EACH FixCDR USE-INDEX CLI no-lock where
            FixCDR.CLI = CLI.CLI AND
            FixCDR.Date >= date1         AND
            FixCDR.Date <= date2.

      FIND FIRST calls where
                 calls.CLI = FixCDR.CLI AND
                 calls.BillCode = FixCDR.BillCode
      no-error.

      IF NOT AVAIL calls THEN DO:
         CREATE calls.
         ASSIGN
            calls.CLI = FixCDR.CLI
            calls.BillCode = FixCDR.BillCode.
      END.

      ASSIGN
         calls.amtc = calls.amtc + 1
         calls.valc = calls.valc + FixCDR.GrossPrice - FixCDR.DiscValue
         calls.amtm = calls.amtm + FixCDR.Duration.

   END.

   OUTPUT STREAM excel TO value(fname).

   PUT STREAM excel UNFORMATTED
      "ASUB"     tab
      "ProdCode" tab
      "ProdName" tab
      "#Calls"   tab
      "#Min"     tab
      "Value W/O VAT" my-nl.

   FOR EACH calls no-lock,

      FIRST BillItem no-lock where
            BillItem.BillCode = calls.BillCode

   BREAK BY calls.CLI.

      IF first-of(calls.CLI) THEN amtasub = amtasub + 1.

      amttot = amttot + calls.amtc.

      PUT STREAM excel UNFORMATTED
         calls.CLI           tab
         calls.BillCode           tab
         BillItem.BIName        tab
         calls.amtc           tab
         int(calls.amtm / 60) tab
         fDecToC(calls.valc,"zzzzzz9.99") my-nl.

   END.

   PUT STREAM excel UNFORMATTED
      "Totally " + string(amtasub) " CLI number(s)".

   OUTPUT STREAM excel CLOSE.

   HIDE MESSAGE no-pause.
   MESSAGE
      amttot  "call(s)" SKIP
      amtasub "ASUB(S)"
   view-as alert-box title " TOTALS ".

   LEAVE CRIT.

END.

HIDE FRAME frm.

