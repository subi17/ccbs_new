/* ---------------------------------------------------------------------
MODULE .........: nncial.p
TASK ...........: Browse expiring contract invoices
APPLICATION ....: NN
CREATED ........: 16.09.1998 kl
MODIFIED .......: 01.10.1998 kl CustNum
                  02.12.1998 kl FixedFee & FFItem
                  07.12.1998 kl use only Period
                  05.03.2003 aam use temp-table to speed up one customer cases
                  05.03.2003 tk tokens
                  15.09.2003 aam brand
VERSION ........: M15
------------------------------------------------------------------------ */

{commali.i}
{fixedfee.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'fixedfee'}

DEF VAR idate   AS DA NO-UNDO.
DEF VAR CustNum AS i  NO-UNDO.
DEF VAR period  AS i  NO-UNDO.

DEF TEMP-TABLE ttCust NO-UNDO
    FIELD CustNum AS INT
    INDEX CustNum CustNum.

idate = pvm.

form
  idate label "Date of next invoice ..." format "99-99-99"
  help "Date when next invoices are to be written"
  CustNum label "   Customer number ..."     format "zzzzz9"
  help "Customer to check, empty (0) = ALL"
with overlay width 80 title " When do you print next invoices ? " FRAME Date
  side-labels ROW 1.

form
   Salesman.SmName  format "x(15)"
   Customer.CustNum
   Customer.CustName   format "x(12)" column-label "Cust. name"
   FFItem.Memo[1] format "x(20)"
   FFItem.Amt
   FixedFee.EndPeriod
WITH
   OVERLAY FRAME LOG ROW 4 12 DOWN width 80 TITLE
   " SUMMARY OF EXPIRING CONTRACT PAYMENTS ". 

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
            FixedFee.Brand      = gcBrand AND
            FixedFee.EndPeriod <= Period,
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
         FixedFee.CustNum = ttCust.CustNum AND
         FixedFee.EndPeriod <= Period,
    EACH FFItem of FixedFee no-lock where 
         FFItem.BillPeriod <= Period AND
         FFItem.Billed     = FALSE,
   FIRST Customer no-lock where
         Customer.CustNum = FFItem.CustNum
BY Customer.CustNum
BY Customer.CustName
WITH FRAME LOG.

   FIND FIRST Salesman no-lock where 
              Salesman.Brand    = gcBrand AND
              Salesman.Salesman = Customer.Salesman.
   DISP 
      Salesman.SmName   format "x(15)"
      Customer.CustNum
      Customer.CustName                          
      FFItem.Memo[1] 
      FFItem.Amt
      FixedFee.EndPeriod.

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



















