/* ----------------------------------------------------------------------
  MODULE .......: bicdump.p
  TASK .........  YOT-3489

   Combine two dumps together.
    - Missing BIC codes dump via mail (existing first part)
    - Invalid customer data via cron dump (new second part)
   Dumps merged to one which is this. Old cron dump was removed.
   Dump is done once in a week.

   Invalid Customer data
   1.  where Charge Type = direct debit but Bank Account is empty or erroneous. 
   REMOVED ERROR: 2.  where Charge Type is different than Direct Debit. 
   3.  where Delivery Type is different than Via Print House. 
   4.  where Postal Code is empty or erroneous. 
   5.  where Region Code is empty or erroneous. 
   6.  where ID Type = NIF or NIE and Customer ID is empty or incorrect. 
   7.  Where Country is different then Spain. 
   8.  Where Address or City is empty. 

  OLD TASK: YCM-612, invalid_custdata.p
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 23.06.08
  CHANGED ......:
                  20.11.2014, Janne Tourunen
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "Qvantel".
gcBrand = "1".

{Func/email.i}
{Func/cparam2.i}
{Syst/dumpfile_run.i}
{Func/timestamp.i}
{Func/fbankdata.i}
{Func/fcustdata.i}
{Syst/tmsconst.i}
{Func/date.i}
{Func/customer_address.i}


DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcMail AS CHAR NO-UNDO.
DEF VAR lcBrand AS CHAR NO-UNDO.
DEF VAR liOrderId AS INT NO-UNDO.
DEF VAR lcMailConfDir AS CHAR NO-UNDO. 
DEF VAR ldaDumpDate AS DATE NO-UNDO.
DEF VAR lcDumpEventTime AS CHAR NO-UNDO.
DEF VAR liDumpTime AS INT NO-UNDO. 
DEF VAR lcClause AS CHAR NO-UNDO. 
DEF VAR lcReason AS CHARACTER NO-UNDO. 

fSplitTS(idLastDump, OUTPUT ldaDumpDate, OUTPUT liDumpTime).

ASSIGN
   lcMail = "/scratch/log/bic_missing/mail.txt"
   lcMailConfDir = fCParamC("RepConfDir")
   lcMailConfDir = lcMailConfDir + "bicmissing.email"
   lcDumpEventTime = STRING(liDumpTime,"HH:MM:SS").

DEF STREAM mailcnt.
DEF STREAM biclog.
OUTPUT STREAM biclog TO VALUE(icFile).

/* First part of this file, BIC dump */
FOR EACH EventLog NO-LOCK WHERE
         EventLog.EventDate >= ldaDumpDate AND
         EventLog.EventTime > lcDumpEventTime AND
         EventLog.TableName = "Order"
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:
   
   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   ASSIGN
      lcBrand = ENTRY(1,EventLog.Key,chr(255))
      liOrderId = int(ENTRY(2,EventLog.Key,chr(255))) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN NEXT.

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand = lcBrand AND
              OrderCustomer.OrderId = liOrderId AND
              OrderCustomer.RowType = 1 USE-INDEX OrderId NO-ERROR.

   IF OrderCustomer.BankCode = "" THEN NEXT.

   IF NOT CAN-FIND( FIRST BankIdCode NO-LOCK WHERE
      BankIdCode.BankCode = SUBSTRING(OrderCustomer.BankCode,5,4)) THEN DO:
      PUT STREAM biclog UNFORMATTED OrderCustomer.CustNum "|"
         liOrderId "|Bank not included in SEPA BIC bank list" SKIP.
      oiEvents = oiEvents + 1.
   END.
END.

/* Second part of this file, customer data check dump */
FOR EACH Customer WHERE 
   Customer.Brand = gcBrand NO-LOCK:

   FIND FIRST MobSub WHERE
      MobSub.Custnum = Customer.Custnum AND
      MobSub.PayType = FALSE NO-LOCK NO-ERROR.
   
   IF NOT AVAIL MobSub THEN NEXT.
   
   lcReason = "".
   
   FIND FIRST Region WHERE
      Region.Region = Customer.Region NO-LOCK NO-ERROR.
   
   IF Customer.ChargeType = 2 AND (LENGTH(Customer.BankAcc) < 24 or
      NOT fCheckBankAcc(Customer.BankAcc)) THEN DO:
      lcReason = 
         "Charge Type = direct debit but Bank Account is empty or erroneous.".
   END.

   ELSE IF Customer.DelType NE {&INV_DEL_TYPE_PAPER} AND
           Customer.DelType NE {&INV_DEL_TYPE_EMAIL} AND
           Customer.DelType NE {&INV_DEL_TYPE_SMS}   AND
           Customer.DelType NE {&INV_DEL_TYPE_EMAIL_PENDING} AND
           Customer.DelType NE {&INV_DEL_TYPE_NO_DELIVERY} THEN DO:
      lcReason = "Invoice Delivery Type is not allowed".
   END.

   ELSE IF NOT AVAIL Region 
      THEN lcReason = "Region Code is empty or erroneous.".

   ELSE IF LOOKUP(Customer.CustIdType,"NIF,NIE") > 0 AND
      NOT fChkCustID(INPUT Customer.CustIDType,
                     INPUT Customer.OrgId) THEN
      lcReason = "ID Type = NIF or NIE and Customer ID is empty or incorrect.".

   ELSE IF Customer.Country NE "ES" THEN
      lcReason = "Country is different than Spain.".

   ELSE IF TRIM(Customer.Address) = "" OR TRIM(Customer.PostOffice) = "" THEN
      lcReason = "Address or City is empty.".

   /* Check for Customer Bank Account YTS-10339 */
   ELSE IF NOT CAN-FIND( FIRST BankIdCode NO-LOCK WHERE
      BankIdCode.BankCode = SUBSTRING(Customer.BankAcct,5,4)) THEN
      lcReason = "Bank not included in SEPA BIC bank list".

   IF lcReason = "" THEN 
      lcReason = fCheckAddress(Customer.CustName + Customer.SurName2 +
                                  Customer.CompanyName,
                               Customer.ZipCode,
                               Customer.Country,
                               OUTPUT lcCountry).

   IF lcReason NE "" THEN DO:
      PUT STREAM biclog unformatted Customer.AgrCust "|0|" lcReason SKIP.
      oiEvents = oiEvents + 1.
   END.

END.

OUTPUT STREAM biclog CLOSE.

/* Mail sending before file transfer */
GetRecipients(lcMailConfDir).

IF oiEvents = 1 THEN
   lcClause = " incorrect customer case.".
ELSE 
   lcClause = " incorrect customer cases.".

OUTPUT STREAM mailcnt TO VALUE(lcMail).
PUT STREAM mailcnt UNFORMATTED
   "See the attached log file, " oiEvents lcClause SKIP.
OUTPUT STREAM mailcnt CLOSE.
SendMail(lcMail,icFile).


IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
