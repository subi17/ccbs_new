DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand      AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp AS DATETIME   NO-UNDO.
DEFINE VARIABLE lcKeyValue   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel2       AS CHARACTER NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcMessage    AS CHARACTER NO-UNDO.

{Func/cparam2.i}
{Func/timestamp.i}
{Func/ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/reptext_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       lcDel2 = CHR(255)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

FUNCTION fToUTF8 RETURNS CHARACTER 
         (INPUT pcText AS CHARACTER):

   RETURN  CODEPAGE-CONVERT(pcText,"UTF-8",SESSION:CHARSET).

END FUNCTION.

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH RepText WHERE
         RepText.Brand   = gcBrand AND
         RepText.ToDate >= TODAY NO-LOCK:

   lcKeyValue = STRING(RepText.TextType) + lcDel2 +
                RepText.LinkCode         + lcDel2 +
                STRING(RepText.Language) + lcDel2 +
                STRING(RepText.ToDate).

   ASSIGN liEvents  = liEvents + 1
          lcMessage = "RepText"                           + lcDel +
                      "CREATE"                            + lcDel +
                      fNotNull(STRING(RECID(RepText)))    + lcDel +
                      fNotNull(lcKeyValue)                + lcDel +
                      fNotNull(STRING(ldtTimeStamp))      + lcDel +
                      fNotNull(STRING(RepText.TextType))  + lcDel +
                      fNotNull(RepText.LinkCode)          + lcDel +
                      fNotNull(STRING(RepText.Language))  + lcDel +
                      fNotNull(REPLACE(RepText.RepText,CHR(10)," ")) + lcDel +
                      fNotNull(STRING(RepText.FromDate))  + lcDel +
                      fNotNull(STRING(RepText.ToDate)).

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "RepText" 
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


