/* -----------------------------------------------
  MODULE .......: SIMREP.P
  FUNCTION .....: Create SIM card report for one customer
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 30.09.03
  MODIFIED .....: 13.10.03 tk utuloste
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}
{Func/function.i}

{Syst/utumaa.i new}

ASSIGN tuni1 = ""
       tuni2 = "".


DEFINE INPUT PARAMETER CustNum LIKE Customer.CustNum NO-UNDO.

FORM
   skip(2)
   "     This program will generate a SIM card report for chosen customer" skip
   SKIP(5)
WITH NO-LABELS CENTERED OVERLAY ROW 3 WIDTH 76 
TITLE " " + ynimi + " SIM CARD REPORT FOR CUSTOMER " + string(CustNum) + " "
FRAME fname.

FIND FIRST Customer NO-LOCK WHERE
           Customer.Custnum = CustNum.

IF NOT CAN-FIND(First MobSub WHERE MobSub.CustNum = Customer.CustNum) THEN DO:
   MESSAGE "This customer doesn't have mobile subscriptions !" 
   VIEW-AS ALERT-BOX.
   RETURN.
END.

VIEW FRAME fname.

tila = true.
{Syst/utuloste.i return}

PUT stream tul UNFORMATTED
   "SIM card report for customer " 
     string(CustNum) " " Customer.CustName skip(1)
   "Subscriber name" tab
   "MSISDN number" tab
   "PIN1" tab
   "PIN2" tab
   "PUK1" tab
   "PUK2" skip.

FOR EACH MobSub NO-LOCK WHERE 
         MobSub.CustNum = Customer.CustNum,
         FIRST Customer OF Mobsub NO-LOCK,
    EACH IMSI NO-LOCK WHERE 
         IMSI.ICC = MobSub.ICC:

   PUT stream tul UNFORMATTED
      DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, BUFFER Customer)
      MobSub.CLI tab
      IMSI.PIN1 tab
      IMSI.PIN2 tab
      IMSI.PUK1 tab
      IMSI.PUK2 skip.

END.

/* Eject the last page */
PUT STREAM tul UNFORMATTED chr(12).

/* Close the printer stream */
tila = FALSE.
{Syst/utuloste.i}

MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

