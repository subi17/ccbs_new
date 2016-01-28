DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand      AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcMessage    AS CHARACTER NO-UNDO.

DEFINE VARIABLE llSelfEmployed       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcEmployer           AS CHARACTER NO-UNDO.
DEFINE VARIABLE liSubLimit           AS INTEGER   NO-UNDO.
DEFINE VARIABLE liSubActLimit        AS INTEGER   NO-UNDO.
DEFINE VARIABLE llDefSubLimit        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llDefSubActLimit     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llSubLimitReached    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llSubActLimitReached AS LOGICAL   NO-UNDO.
DEFINE VARIABLE liSubCount           AS INTEGER   NO-UNDO.
DEFINE VARIABLE liActOrderCount      AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldaOrderDate         AS DATE      NO-UNDO.

{cparam2.i}
{timestamp.i}
{ftransdir.i}
{fcustdata.i}
{tmsconst.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/customer_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fToUTF8 RETURNS CHARACTER 
         (INPUT pcText AS CHARACTER):

   RETURN  CODEPAGE-CONVERT(pcText,"UTF-8",SESSION:CHARSET).

END FUNCTION.

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH Customer WHERE
         Customer.Brand = gcBrand NO-LOCK:

   FIND FIRST CustCat NO-LOCK WHERE
              CustCat.Brand = gcBrand AND
              CustCat.Category = Customer.Category NO-ERROR.
   IF AVAILABLE CustCat THEN llSelfEmployed = CustCat.SelfEmployed.

   IF Customer.CustIDType = "passport" THEN llSelfEmployed = FALSE.

   IF Customer.CustIDType NE "CIF" AND
      Customer.Profession > "" THEN lcEmployer = Customer.CompanyName.

   liSubLimit = fGetMobsubLimit(INPUT Customer.Custnum,
                                INPUT Customer.Category,
                                OUTPUT llDefSubLimit).

   liSubActLimit = fGetMobsubActLimit(INPUT Customer.Custnum,
                                      INPUT Customer.Category,
                                      OUTPUT llDefSubActLimit).

   FOR EACH OrderCustomer NO-LOCK WHERE
            OrderCustomer.Brand      EQ gcBrand AND
            OrderCustomer.CustIdType EQ Customer.CustIdType AND
            OrderCustomer.CustId     EQ Customer.OrgId AND
            OrderCustomer.RowType    EQ 1,
       EACH Order NO-LOCK WHERE
            Order.Brand              EQ gcBrand AND
            Order.orderid            EQ OrderCustomer.Orderid AND
            Order.OrderType          NE {&ORDER_TYPE_RENEWAL} AND
            Order.OrderType          NE {&ORDER_TYPE_STC} AND
            Order.SalesMan NE "GIFT":

      IF LOOKUP(STRING(Order.statuscode),{&ORDER_CLOSE_STATUSES}) EQ 0
      THEN DO:
         IF Order.StatusCode EQ {&ORDER_STATUS_DELIVERED} THEN DO:
            fTS2Date(Order.CrStamp, OUTPUT ldaOrderDate).
            IF INTERVAL(TODAY, ldaOrderDate, "months") >= 24 THEN NEXT.
         END.
         liActOrderCount = liActOrderCount + 1.
      END.

      IF LOOKUP(STRING(Order.statuscode),{&ORDER_INACTIVE_STATUSES}) EQ 0 THEN
         liSubCount = liSubCount + 1.

   END.

   FOR EACH Mobsub NO-LOCK WHERE
            MobSub.Brand    EQ gcBrand AND
            Mobsub.AgrCust  EQ Customer.CustNum AND
            MobSub.SalesMan NE "GIFT":
      liSubCount = liSubCount + 1.
   END.

   IF liSubCount >= liSubLimit THEN llSubLimitReached = TRUE.
   IF liActOrderCount >= liSubActLimit THEN llSubActLimitReached = TRUE.

   ASSIGN liEvents  = liEvents + 1
          lcMessage = "Customer"                               + lcDel +
                      "CREATE"                                 + lcDel +
                      fNotNull(STRING(RECID(Customer)))        + lcDel +
                      fNotNull(STRING(Customer.CustNum))       + lcDel +
                      fNotNull(STRING(ldtTimeStamp))           + lcDel +
                      fNotNull(STRING(Customer.CustNum))       + lcDel +
                      fNotNull(Customer.CustId)                + lcDel +
                      fNotNull(Customer.OrgId)                 + lcDel +
                      fNotNull(Customer.FirstName)             + lcDel +
                      fNotNull(Customer.CustName)              + lcDel +
                      fNotNull(Customer.SurName2)              + lcDel +
                      fNotNull(Customer.ZipCode)               + lcDel +
                      fNotNull(Customer.Region)                + lcDel +
                      fNotNull(STRING(Customer.Language))      + lcDel +
                      fNotNull(STRING(Customer.DelType))       + lcDel +
                      fNotNull(Customer.CompanyName)           + lcDel +
                      fNotNull(Customer.Profession)            + lcDel +
                      fNotNull(Customer.Address)               + lcDel +
                      fNotNull(Customer.PostOffice)            + lcDel +
                      fNotNull(Customer.Country)               + lcDel +
                      fNotNull(Customer.Nationality)           + lcDel +
                      fNotNull(Customer.Email)                 + lcDel +
                      fNotNull(Customer.Phone)                 + lcDel +
                      fNotNull(Customer.SMSNumber)             + lcDel +
                      fNotNull(Customer.BankAcct)              + lcDel +
                      /*YPR-3204*/
                      fNotNull(Customer.HonTitle)                + lcDel +
                      fNotNull(STRING(Customer.Birthday))        + lcDel +
                      fNotNull(STRING(llSelfEmployed))           + lcDel +
                      fNotNull(STRING(Customer.FoundationDate))  + lcDel +
                      fNotNull(Customer.AuthCustId)              + lcDel +
                      fNotNull(Customer.AuthCustIdType)          + lcDel +
                      fNotNull(lcEmployer)                       + lcDel +
                      fNotNull(STRING(llSubLimitReached))        + lcDel +
                      fNotNull(STRING(llSubActLimitReached)).

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "Customer" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   lcMessage = fToUTF8(lcMessage).

   PUT STREAM slog UNFORMATTED lcMessage SKIP.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).

