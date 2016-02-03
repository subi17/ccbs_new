/* ---------------------------------------------------------------------
MODULE .........: nncibr.p
TASK ...........: Browse contract invoices
APPLICATION ....: Ticket Master
CREATED ........: 13.05.1998 pt
MODIFIED .......: 01.10.1998 kl CustNum
                  02.12.1998 kl FixedFee & FFItem
                  08.12.1998 kl DO NOT check expiring Period, use only IPeriod
                  16.05.2002 aam formats
                  05.03.2003 aam use temp-table to speed up one customer cases
                  05.03.2003 tk tokens
                  16.09.2003 aam brand
VERSION ........: M15
------------------------------------------------------------------------ */

{Syst/commali.i}
{Func/fixedfee.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'fixedfee'}

DEF VAR idate  AS DA NO-UNDO.
DEF VAR CustNum AS i  NO-UNDO.
DEF VAR period  AS i  NO-UNDO.
idate = pvm.

DEF TEMP-TABLE ttCust NO-UNDO
    FIELD CustNum AS INT
    INDEX CustNum CustNum.

form
  idate label "Date of next invoice ..." format "99-99-99"
  help "Date when next invoices are to be written"
  CustNum label "   Customer number ..."     format "zzzzz9"
  help "Customer to check, empty (0) = ALL"
with overlay width 80 title " When do you print next invoices ? " FRAME Date
  side-labels ROW 1.

form
   Customer.CustNum
   Customer.CustName   format "x(16)" column-label "Name"
   FFItem.BillPeriod
   FFItem.Memo[1] format "x(20)" column-label "Expl."
   FFItem.Amt
WITH
   OVERLAY FRAME LOG ROW 4 12 DOWN width 80 TITLE
   " " + ynimi + "  SUMMARY OF FUTURE CONTRACT PAYMENTS ". 

ehto = 9. RUN ufkey.
PAUSE 0.
view FRAME LOG.
UPDATE 
   idate 
   CustNum VALIDATE(INPUT CustNum = 0 OR
                    CAN-FIND(FIRST Customer WHERE 
                                   Customer.Brand   = gcBrand AND
                                   Customer.CustNum = INPUT CustNum),
                    "Unknown customer !")
WITH FRAME Date.
PAUSE 0.

ufk = 0. ehto = 3. RUN ufkey.

period = year(idate) * 100 + month(idate).

EMPTY TEMP-TABLE ttCust.

IF CustNum > 0 THEN DO:
   CREATE ttCust.
   ttCust.CustNum = CustNum.
END.

ELSE DO:

   MESSAGE "Collecting fees ..".

   FOR EACH FixedFee no-lock WHERE
            FixedFee.Brand = gcBrand,
       FIRST FFItem of FixedFee no-lock where 
             FFItem.BillPeriod <= Period AND
             FFItem.Billed     = FALSE:

      IF CAN-FIND(FIRST ttCust WHERE ttCust.CustNum = FixedFee.CustNum)
      THEN NEXT.

      CREATE ttCust.
      ttCust.CustNum = FixedFee.CustNum.
   END.

   HIDE MESSAGE NO-PAUSE.

END.

FOR EACH ttCust,
    EACH FixedFee NO-LOCK WHERE
         FixedFee.CustNum = ttCust.CustNum,
    EACH FFItem of FixedFee no-lock where 
         FFItem.BillPeriod <= Period AND
         FFItem.Billed     = FALSE,
 FIRST Customer no-lock where
       Customer.CustNum = FFItem.CustNum
BY Customer.CustNum
BY Customer.CustName
WITH FRAME LOG.

   DISP 
      Customer.CustNum 
      Customer.CustName    
      FFItem.BillPeriod
      FFItem.Memo[1]  
      FFItem.Amt.

   accumulate FFItem.Amt (total).
   disp (accum total FFItem.Amt) format "->>>>>9.99".

   IF FRAME-LINE >= FRAME-DOWN THEN PAUSE 
     message "More rows - press ENTER ! (F4 to QUIT)".
   if keylabel(lastkey) = "F4" THEN RETURN.
   DOWN.
END.

PAUSE 0.
message "Press ENTER !".
PAUSE no-message.

HIDE FRAME Date no-pause.
HIDE FRAME LOG no-pause.

