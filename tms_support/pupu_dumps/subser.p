DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand      AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE lcKeyValue   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel2       AS CHARACTER NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcServList   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcServCom    AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCount      AS INTEGER   NO-UNDO.
DEFINE VARIABLE liNumEntries AS INTEGER   NO-UNDO.

{cparam2.i}
{timestamp.i}
{ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/subser_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       lcDel2 = CHR(255)
       lcServList = "VMS,LANG,CF,IRDCUTOFF,BB,NAM,CALLSPEC,LTE,BPSUB"
       liNumEntries = NUM-ENTRIES(lcServList)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH MobSub WHERE
         MobSub.Brand = gcBrand NO-LOCK:

   DO liCount = 1 TO liNumEntries:

      lcServCom = ENTRY(liCount,lcServList).

      FOR FIRST SubSer WHERE
                SubSer.MsSeq = MobSub.MsSeq AND
                SubSer.ServCom = lcServCom NO-LOCK:

         liEvents = liEvents + 1.

         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
            PAUSE 0.
            DISP liEvents LABEL "SubSer" 
            WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
            TITLE " Collecting " FRAME fQty.
         END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

         lcKeyValue = STRING(SubSer.MsSeq) + lcDel2 + SubSer.ServCom +
                      lcDel2 + STRING(SubSer.SSDate).

         PUT STREAM slog UNFORMATTED
         "SubSer"                          lcDel
         "CREATE"                          lcDel
         fNotNull(STRING(RECID(SubSer)))   lcDel
         fNotNull(lcKeyValue)              lcDel
         fNotNull(STRING(ldtTimeStamp))    lcDel
         fNotNull(STRING(SubSer.MsSeq))    lcDel
         fNotNull(SubSer.ServCom)          lcDel
         fNotNull(STRING(SubSer.SSDate))   lcDel
         fNotNull(STRING(SubSer.SSStat))   lcDel
         fNotNull(SubSer.SSParam)          SKIP.
      END.
   END.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).
