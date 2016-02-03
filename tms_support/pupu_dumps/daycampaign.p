DEFINE VARIABLE lcLogFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel             AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand           AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE liEvents          AS INTEGER   NO-UNDO.
DEFINE VARIABLE llDSS2Compatible  AS LOGICAL   NO-UNDO.

{Func/cparam2.i}
{Func/timestamp.i}
{Func/ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/daycampaign_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH DayCampaign WHERE
         DayCampaign.Brand = gcBrand AND
         DayCampaign.ValidTo >= TODAY NO-LOCK:

   liEvents = liEvents + 1.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "DayCampaign" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   llDSS2Compatible = DayCampaign.DCevent EQ "CONT15" OR
                      CAN-FIND(FIRST CLIType WHERE
                                     CLIType.Brand    = gcBrand AND
                                     CLIType.CLIType  = DayCampaign.DCEvent AND
                                     CLIType.LineType > 0).

   PUT STREAM slog UNFORMATTED
       "DayCampaign"                               lcDel
       "CREATE"                                    lcDel
       fNotNull(STRING(RECID(DayCampaign)))        lcDel
       fNotNull(DayCampaign.DCEvent)               lcDel
       fNotNull(STRING(ldtTimeStamp))              lcDel
       fNotNull(DayCampaign.DCEvent)               lcDel
       fNotNull(DayCampaign.DCName)                lcDel
       fNotNull(DayCampaign.DCType)                lcDel
       fNotNull(STRING(DayCampaign.ValidFrom))     lcDel
       fNotNull(STRING(DayCampaign.ValidTo))       lcDel
       fNotNull(DayCampaign.FeeModel)              lcDel
       fNotNull(DayCampaign.TermFeeModel)          lcDel
       fNotNull(STRING(DayCampaign.DurMonths))     lcDel
       fNotNull(STRING(DayCampaign.InstanceLimit)) lcDel
       fNotNull(DayCampaign.BundleUpsell)          lcDel
       fNotNull(STRING(DayCampaign.DSSPriority))   lcDel
       fNotNull(STRING(llDSS2Compatible))          SKIP.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).

