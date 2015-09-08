/* ----------------------------------------------------------------------
  MODULE .......: barring_dump.p
  TASK .........: Create a dump file for Barring
  APPLICATION ..: tms
  AUTHOR .......: ilkkasav
  CREATED ......: 25.06.15
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{dumpfile_run.i}
{create_eventlog.i}

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
DEF VAR ldValue      AS DEC      NO-UNDO.
DEF VAR ldFromStamp  AS DEC      NO-UNDO.
DEF VAR ldCurrent    AS DEC      NO-UNDO.

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
         OUTPUT liCnt).

ASSIGN
   lhTable     = BUFFER Barring:HANDLE
   lcKeyFields = fEventKeyFields(lhTable)
   ldCurrent   = fMakeTS().

IF icDumpMode = "Modified" THEN ldFromStamp = idLastDump.
ELSE ldFromStamp = 20060101.

OUTPUT STREAM sFile TO VALUE(icFile).


BarringsLoop:
FOR EACH Barring NO-LOCK WHERE
         Barring.EventTS >= ldFromStamp       
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

      lcField = ENTRY(liCnt,lcDumpFields).
      
      lhField = lhTable:BUFFER-FIELD(lcField).
      
      IF lhField:DATA-TYPE NE "CHARACTER" THEN 
         lcValue = STRING(lhField:BUFFER-VALUE).
      ELSE 
         lcValue = lhField:BUFFER-VALUE.
      
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
      DISP oiEvents LABEL "Barrings" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END.                       

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.


