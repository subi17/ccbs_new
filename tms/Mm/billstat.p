/* -----------------------------------------------------
  MODULE .......: billstat.p
  FUNCTION .....: Billing Statistic
  APPLICATION ..: NN
  AUTHOR .......: JP
  CREATED ......: 07.03.2002
  MODIFIED .....: 17.10.02/aam frame layout 
                  05.09.03 jp  brand
                  18.05.04/aam equal dates accepted
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}

DEF VAR path    AS  CHAR NO-UNDO FORMAT "x(50)".
DEF VAR date1   AS  DATE NO-UNDO.
DEF VAR date2   AS  DATE NO-UNDO.
DEF VAR i       AS  INT  NO-UNDO.

{Func/tmsparam.i DefBillStatDir     return}.  path = tmsparam.CharVal.


DEF TEMP-TABLE invdata
FIELD  stat   LIKE Invoice.PaymState
FIELD  price  LIKE Invoice.Amt
FIELD  qty    AS   INT 
INDEX stat    IS PRIMARY UNIQUE stat.    

form
SKIP(4)
" NOTE: This program writes a summary of all invoices that are written " SKIP
"       within the time period given below.                            "                                                                          SKIP(2)
date1    LABEL " Invoice date .........."       FORMAT "99-99-9999"
         HELP "Earliest date of invoices"
         "-"
date2    NO-LABEL HELP "Latest date of invoices" FORMAT "99-99-9999" skip
path     LABEL " Path .................."
HELP "Output filename"               


skip(7)
with
   row 1 width 79 overlay SIDE-LABELS title " " +
   " BILLING STATISTIC "  frame rajat.

ASSIGN
date1 = date(month(today),1,year(today))
date2 = date1 + 40
date2 = date(month(date2),1,year(date2)) - 1.


rajat:
repeat with frame rajat:

   pause 0.
   ehto = 9. RUN Syst/ufkey.

   update 
   date1 date2
   validate (input date1 <= input date2," Invalid date order !") 
   path.

toimi:
   repeat with frame toimi:
      assign ufk = 0 ehto = 0 ufk[1] = 132 ufk[5] = 63 ufk[8] = 8.
      RUN Syst/ufkey.
      if toimi = 1 then next  rajat.
      if toimi = 8 then leave rajat.
      if toimi = 5 then leave toimi.
   end.



   FOR EACH Invoice WHERE
            Invoice.Brand    = gcBrand AND 
            Invoice.InvDate >= date1   AND
            Invoice.InvDate <= date2  NO-LOCK.


      FIND FIRST invdata WHERE
           invdata.stat = Invoice.PrintState no-error.

      IF NOT AVAIL invdata THEN DO:
          CREATE invdata.
          ASSIGN invdata.stat = Invoice.PrintState.
      END.     
      ASSIGN
      invdata.price = invdata.price + Invoice.Amt
      invdata.qty   = invdata.qty   + 1.

      PUT SCREEN ROW 18 "Calculating ..." + STRING(Invoice.InvNum).

   END.       /* Invoice */

   OUTPUT STREAM excel TO VALUE(path).

   PUT STREAM excel UNFORMATTED
   "                  BILLING STATISTIC " date1 " - " date2 MY-NL
   "==================================================================" my-nl  
   my-nl
   "Status   " TAB "Amount   " TAB "Price" my-nl.

   FOR EACH invdata.
      PUT STREAM excel UNFORMATTED
      IF      invdata.stat = 0 THEN " 0: Not printed"
      ELSE IF invdata.stat = 1 THEN " 1: Printed to Printhouse "
      ELSE IF invdata.stat = 2 THEN " 2: Printed locally"
      ELSE IF invdata.stat = 3 THEN " 3: Printed to Printhouse and locally"
      ELSE IF invdata.stat = 9 THEN " 9: Credited" 
      ELSE " " + STRING(invdata.stat) + "       "               TAB
      invdata.qty                                               TAB
      invdata.price                                             MY-NL.
   END.

   MESSAGE 
   "Invoice status created"
   VIEW-AS ALERT-BOX.

   PUT SCREEN ROW 18 "                                   ".
   HIDE MESSAGE NO-PAUSE.
   OUTPUT STREAM excel CLOSE.

   LEAVE.
END.
