/* ----------------------------------------------------------------------
  MODULE .......: fusion_message_dump.p
  TASK .........: Create a dump file from FusionMessage table
  APPLICATION ..: tms
  AUTHOR .......: Pasi Hautaniemi 
  CREATED ......: 18.10.16 
  Version ......: yoigo
---------------------------------------------------------------------- */


{commali.i}
{dumpfile_run.i}
{create_eventlog.i}
{timestamp.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric      AS CHAR   NO-UNDO.
DEF VAR lcDelimiter    AS CHAR   NO-UNDO.
DEF VAR lcDumpFields   AS CHAR   NO-UNDO.
DEF VAR liCnt          AS INT    NO-UNDO.
DEF VAR lhTable        AS HANDLE NO-UNDO.
DEF VAR lhField        AS HANDLE NO-UNDO.
DEF VAR lcField        AS CHAR   NO-UNDO. 
DEF VAR lcValue        AS CHAR   NO-UNDO.

DEF TEMP-TABLE ttPicked NO-UNDO
   FIELD MessageSeq AS INT 
   INDEX MessageSeq MessageSeq.
 
DEF STREAM sLog.

/* Check that same message not handled two times */
FUNCTION fPick RETURNS LOGICAL
   (iiMessageSeq AS INT):

   IF CAN-FIND(FIRST ttPicked WHERE ttPicked.MessageSeq = iiMessageSeq) THEN
      RETURN FALSE.
   
   CREATE ttPicked.
   ttPicked.MessageSeq = iiMessageSeq.
   RETURN TRUE.
   
END FUNCTION.

/***** Main start **********/

OUTPUT STREAM sLog TO VALUE(icFile).

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.

FOR EACH DFField OF DumpFile NO-LOCK WHERE
         DFField.ToDate   >= TODAY AND
         DFField.FromDate <= TODAY
BY DFField.OrderNbr:
   lcDumpFields = lcDumpFields + (IF lcDumpFields > "" THEN "," ELSE "") +
                  DFField.DFField.
END.
ASSIGN
   lhTable = BUFFER FusionMessage:HANDLE.

lcNumeric = SESSION:NUMERIC-FORMAT.

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

IF icDumpMode = "modified" THEN DO:
   RUN pSearchFusionMessages. 
END.   

/* full dumps */
ELSE DO:
   RUN pAllFusionMessages.
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
   
OUTPUT STREAM sLog CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.

/******** Main end *******/


PROCEDURE pSearchFusionMessages:

   FOR EACH FusionMessage NO-LOCK WHERE
            FusionMessage.HandledTS > idLastDump
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      IF NOT fPick(FusionMessage.MessageSeq) THEN NEXT.
      RUN pDumpFusionTable.
   END.

END PROCEDURE.


PROCEDURE pAllFusionMessages:

   DEF VAR ldLastDump    AS DEC    NO-UNDO.
   ldLastDump = fMake2Dt(TODAY - 30,0).   /* Default time */

   /* When was last full dump done */   
   FIND FIRST DumpLog NO-LOCK WHERE
              DumpLog.DumpId = icDumpID AND
              DumpLog.DumpType = icDumpMode AND
              DumpLog.DumpLogStatus NE 5 USE-INDEX DumpId NO-ERROR.
   IF AVAIL DumpLog THEN DO:
      IF DumpLog.CreateStart > fHMS2TS(TODAY - 65,'') THEN /* Max time */
         ldLastDump = DumpLog.CreateStart.
   END.

   idLastDump = ldLastDump.
   RUN pSearchFusionMessages. 

END PROCEDURE.

/* Write all field values from dumptool configuration to file */
PROCEDURE pDumpFusionTable:
   
   DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

      ASSIGN
         lcField = ENTRY(liCnt,lcDumpFields)
         lcValue = "".

      lhField = lhTable:BUFFER-FIELD(lcField).

      IF lhField:DATA-TYPE = "DECIMAL"
         THEN lcValue = STRING(lhField:BUFFER-VALUE,"99999999.99999").
      ELSE lcValue = lhField:BUFFER-VALUE.

      /* YOT-2952 */
      IF lhField:DATA-TYPE EQ "CHARACTER" THEN lcValue = REPLACE(lcValue,CHR(10)," ").
      
      PUT STREAM sLog UNFORMATTED
         lcValue.
   
      IF liCnt < NUM-ENTRIES(lcDumpFields) THEN 
         PUT STREAM sLog UNFORMATTED
            lcDelimiter.
   END.
 
   PUT STREAM sLog UNFORMATTED
      SKIP.

   oiEvents = oiEvents + 1.
END.
