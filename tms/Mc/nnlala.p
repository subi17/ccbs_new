/*------------------------------------------------------
  MODULE .......: NNLALA.P
  FUNCTION .....: Laskee yhteen kaikki avoimet myyntilaskut
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 12.03.1997 pt
  MODIFIED .....: 08.07.1998 kl Date limitation + english
                  05.03.2003 tk tokens
                  18.03.2003 aam use invbal
                  15.09.2003 aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/cparam2.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invoice'}

DEF VAR tdebt  AS DE  NO-UNDO EXTENT 3.
DEF VAR debt   AS DE  NO-UNDO.
def var ok     as lo  no-undo format "Yes/No".
DEF VAR Date   AS DA  NO-UNDO init TODAY.
DEF VAR ldate  AS DA  NO-UNDO.
DEF VAR edate  AS DA  NO-UNDO.
DEF VAR curr1  AS C   NO-UNDO.
DEF VAR curr3  AS C   NO-UNDO.
DEF VAR curr2  AS C   NO-UNDO.

ASSIGN
curr1 = fCparamC("DefCurrency")
curr2 = curr1
curr3 = curr1.

form
   skip(2)
   "  INSTRUCTION: This program creates a summary of all unpaid    "  SKIP
   "               invoices summed together before the Date        "  SKIP
   "               given below:"                                      skip(3)
   "               Invoices before: " Date  no-label format "99-99-9999"
   skip(8)
WITH ROW 1 side-labels width 80
   title color value(ctc) " " + ynimi + " SUMMARY OF UNPAID INVOICES " +
   string(pvm,"99-99-99") + " " COLOR value(cfc) FRAME frm.

form
   skip(1)
   "Not expired invoices ..:" tdebt[1] format "zz,zzz,zz9.99-" curr1 SKIP
   "Expired invoices ......:" tdebt[2] format "zz,zzz,zz9.99-" curr2 SKIP   "All invoices     ......:" tdebt[3] format "zz,zzz,zz9.99-" curr3 SKIP(1)
   "Date of earliest invoice :" edate  format "99-99-9999"           SKIP
   "Date of latest invoice ..:" ldate  format "99-99-9999"           SKIP
   skip(1)
WITH
   centered ROW 5 COLOR value(cfc) TITLE COLOR value(ctc)
   " UNPAID INVOICES "  NO-LABELS
   OVERLAY FRAME debt.

ehto = 9. RUN ufkey.

UPDATE Date WITH FRAME frm.

message "Do You want to sum all unpaid invoices (Y/N) ?" UPDATE ok.
IF NOT ok THEN RETURN.

FIND FIRST Invoice WHERE Invoice.Brand = gcBrand no-lock. 
edate = Invoice.InvDate.
FIND LAST  Invoice WHERE Invoice.Brand = gcBrand no-lock. 
ldate = Invoice.InvDate.

message "Wait ...".

    FOR EACH Invoice no-lock where 
             Invoice.Brand    = gcBrand AND
             Invoice.InvDate <= Date.

       ASSIGN
          edate = minimum(Invoice.InvDate,edate)
          ldate = maximum(Invoice.InvDate,ldate).

       RUN invbal(Invoice.InvNum, OUTPUT debt).

       IF Invoice.DueDate < Date THEN tdebt[2] = tdebt[2] + debt.
       ELSE                           tdebt[1] = tdebt[1] + debt.
       tdebt[3] = tdebt[3] + debt.

    END.


    cfc = "lis". RUN ufcolor.
    DISPLAY tdebt edate ldate curr1 curr2 curr3 WITH FRAME debt.

message "Press ENTER !".
PAUSE no-message.
HIDE FRAME debt.

