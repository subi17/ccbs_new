/* ----------------------------------------------------------------------
  MODULE .......: FATime_dump.p
  TASK .........: Create a dump file for FATimes
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 20.04.09
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}
{Func/create_eventlog.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric    AS CHAR     NO-UNDO.
DEF VAR lcDelimiter  AS CHAR     NO-UNDO.
DEF VAR ldaModified  AS DATE     NO-UNDO.
DEF VAR liModified   AS INTEGER  NO-UNDO. 
DEF VAR liCnt        AS INT      NO-UNDO.
DEF VAR lhTable      AS HANDLE   NO-UNDO.
DEF VAR lcCreator    AS CHAR     NO-UNDO.
DEF VAR ldCreated    AS DEC      NO-UNDO.
DEF VAR lcKeyFields  AS CHAR     NO-UNDO.
DEF VAR lcEventKey   AS CHAR     NO-UNDO.
DEF VAR lcDumpFields AS CHAR     NO-UNDO.
DEF VAR lcValue      AS CHAR     NO-UNDO.
DEF VAR lhField      AS HANDLE   NO-UNDO.
DEF VAR lcField      AS CHAR     NO-UNDO.
DEF VAR liFatNum     AS INT NO-UNDO.
DEF VAR lcLastDumpTime AS CHAR NO-UNDO.

DEF STREAM sFile.

lcNumeric = SESSION:NUMERIC-FORMAT.

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).

   IF DumpFile.DecimalPoint = "."
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END.
ELSE DO:
   ASSIGN
      lcDelimiter = CHR(9)
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END.

FOR EACH DFField OF DumpFile NO-LOCK WHERE
         DFField.ToDate   >= TODAY AND
         DFField.FromDate <= TODAY
BY DFField.OrderNbr:
   lcDumpFields = lcDumpFields + (IF lcDumpFields > "" THEN "," ELSE "") +
                  DFField.DFField.
END.

fSplitTS(idLastDump,
         OUTPUT ldaModified,
         OUTPUT liModified).

ASSIGN
   lhTable     = BUFFER FATime:HANDLE
   lcKeyFields = fEventKeyFields(lhTable).

OUTPUT STREAM sFile TO VALUE(icFile).

IF icDumpMode EQ "modified" THEN DO:

   lcLastDumpTime = STRING(liModified,"hh:mm:ss").

   FatimeLoop:
   FOR EACH EventLog NO-LOCK where
            EventLog.eventdate >= ldaModified and
            EventLog.tablename = "fatime" and
            EventLog.action = "CREATE" use-index eventdate
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      IF EventLog.EventDate EQ ldaModified AND
         EventLog.eventtime < lcLastDumpTime THEN NEXT FatimeLoop.

      liFatNum = int(entry(3,eventlog.key,chr(255))) no-error.
      IF ERROR-STATUS:ERROR THEN NEXT FatimeLoop.
      
      FIND fatime NO-LOCK where
           fatime.brand = gcBrand and
           fatime.fatnum = liFatNum no-error.
      IF NOT AVAIL FAtime OR FAtime.OrigFat NE 0 THEN NEXT FatimeLoop.
   
      ASSIGN
         lcCreator = EventLog.UserCode
         ldCreated = fHMS2TS(EventLog.EventDate,EventLog.EventTime).

      RUN pDumpFatime.

   END.

END.
ELSE DO:
FatimeLoop:
FOR EACH FATime NO-LOCK USE-INDEX CLI WHERE
         FATime.Brand   = gcBrand AND
         FATime.OrigFat = 0
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   ASSIGN
      lcCreator  = ""
      ldCreated  = 0
      lcEventKey = fEventKeyValues(lhTable,lcKeyFields).

   FOR FIRST EventLog NO-LOCK WHERE
             EventLog.TableName = "FATime"   AND
             EventLog.Key       = lcEventKey AND
             EventLog.Action    = "create":

      ASSIGN
         lcCreator = EventLog.UserCode
         ldCreated = fHMS2TS(EventLog.EventDate,EventLog.EventTime).
   END.

   IF ldCreated = 0 THEN NEXT.

   RUN pDumpFatime.

END.
END.

PROCEDURE pDumpFatime:
         
   IF lcCreator > "" THEN DO:
      FIND FIRST TMSUser WHERE TMSUser.UserCode = lcCreator NO-LOCK NO-ERROR.
      IF AVAILABLE TMSUser THEN lcCreator = TMSUser.UserName.
   END.

   DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

      lcField = ENTRY(liCnt,lcDumpFields).

      IF lcField BEGINS "#" THEN DO:
         CASE lcField:
         WHEN "#Creator" THEN lcValue = lcCreator.
         WHEN "#Created" THEN lcValue = STRING(ldCreated,"99999999.99999").
         OTHERWISE lcValue = "".
         END CASE.
      END.

      ELSE DO:
         lhField = lhTable:BUFFER-FIELD(lcField).

         IF lhField:DATA-TYPE = "DECIMAL" THEN
            lcValue = TRIM(STRING(lhField:BUFFER-VALUE,"->>>,>>>,>>9.9<")).
         ELSE lcValue = lhField:BUFFER-VALUE.
      END.

      PUT STREAM sFile UNFORMATTED
         lcValue.

      IF liCnt < NUM-ENTRIES(lcDumpFields) THEN
      PUT STREAM sFile UNFORMATTED
         lcDelimiter.

   END.

   PUT STREAM sFile UNFORMATTED
      SKIP.

   oiEvents = oiEvents + 1.

   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "FATimes"
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.

END PROCEDURE.

IF NOT SESSION:BATCH THEN
   HIDE FRAME fQty NO-PAUSE.

OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.
