/* ----------------------------------------------------------------------
  MODULE .......: fusion_message_dump.p
  TASK .........: Create a dump file from FusionMessage table
  APPLICATION ..: tms
  AUTHOR .......: Pasi Hautaniemi 
  CREATED ......: 18.10.16 
  Version ......: yoigo
---------------------------------------------------------------------- */


{Syst/commali.i}
{Syst/dumpfile_run.i}
{Func/create_eventlog.i}
{Func/timestamp.i}

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

ASSIGN lhTable = BUFFER TPServiceMessage:HANDLE.

lcNumeric = SESSION:NUMERIC-FORMAT.

IF AVAILABLE DumpFile THEN 
DO:
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
   RUN pSearchTPServiceMessages. 
END.   

/* full dumps */
ELSE DO:
   RUN pSearchTPServiceMessages.
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
   
OUTPUT STREAM sLog CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.

/******** Main end *******/


PROCEDURE pSearchTPServiceMessages:

   FOR EACH TPServiceMessage NO-LOCK WHERE
            TPServiceMessage.UpdateTS > idLastDump
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      IF NOT fPick(TPServiceMessage.MessageSeq) THEN 
          NEXT.

      RUN pDumpTPServiceTable.
   END.

END PROCEDURE.

/* Write all field values from dumptool configuration to file */
PROCEDURE pDumpTPServiceTable:
   
   DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

      ASSIGN
         lcField = ENTRY(liCnt,lcDumpFields)
         lcValue = "".

      lhField = lhTable:BUFFER-FIELD(lcField).

      IF lhField:DATA-TYPE = "DECIMAL" THEN 
          lcValue = STRING(lhField:BUFFER-VALUE,"99999999.99999").
      ELSE IF lhField:DATA-TYPE EQ "CHARACTER" THEN 
          lcValue = REPLACE(lhField:BUFFER-VALUE, CHR(10)," ").    
      ELSE 
          lcValue = lhField:BUFFER-VALUE.

      PUT STREAM sLog UNFORMATTED lcValue.
   
      IF liCnt < NUM-ENTRIES(lcDumpFields) THEN 
         PUT STREAM sLog UNFORMATTED lcDelimiter.
   END.
 
   PUT STREAM sLog UNFORMATTED SKIP.

   oiEvents = oiEvents + 1.
   
END PROCEDURE.
