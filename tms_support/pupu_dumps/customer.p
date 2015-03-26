DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand      AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcMessage    AS CHARACTER NO-UNDO.

{cparam2.i}
{timestamp.i}
{ftransdir.i}

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
                      fNotNull(Customer.BankAcct).

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
