/* ----------------------------------------------------------------------
  MODULE .......: billrunstat_dump.p
  TASK .........: Dump BillRun statistics
  APPLICATION ..: tms
  AUTHOR .......: vikas
  CREATED ......: 12.08.11
  Version ......: yoigo
---------------------------------------------------------------------- */
{commali.i}
{cparam2.i}
{timestamp.i}
{dumpfile_run.i}
{ftransdir.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric                  AS CHAR NO-UNDO.
DEF VAR lcDel                      AS CHAR NO-UNDO.
DEF VAR lcTrackDir                 AS CHAR NO-UNDO.

DEF STREAM sFile.

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):
   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.
END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

ASSIGN lcNumeric  = SESSION:NUMERIC-FORMAT
       lcTrackDir = fCParamC("BillRunStatTrackDir").

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   lcDel = fInitDelimiter(DumpFile.DumpDelimiter).

   IF DumpFile.DecimalPoint = "."
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END. /* IF AVAILABLE DumpFile THEN DO: */
ELSE DO:
   ASSIGN 
      lcDel = "|"
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END. /* ELSE DO: */

OUTPUT STREAM sFile TO VALUE(icFile).

/* Add the header in the output file */
PUT STREAM sFile UNFORMATTED
    "Queue ID"              lcDel
    "Queue Name"            lcDel
    "Queue Timing ID"       lcDel
    "Queue Scheduled"       lcDel
    "Queue Done"            lcDel
    "Queue Status"          lcDel
    "Queue RunMode"         lcDel
    "Configuration ID"      lcDel
    "Configuration Name"    lcDel
    "Execution ID"          lcDel
    "Queue Order"           lcDel
    "Execution Started"     lcDel
    "Execution Ended"       lcDel
    "Execution Status"      lcDel
    "FR Process ID"         lcDel
    "Process Sequence"      lcDel
    "Process Started"       lcDel
    "Process Ended"         lcDel
    "Process Status"        lcDel
    "Process Events"        SKIP.

QUEUE:
FOR EACH FuncRunQSchedule NO-LOCK:

   IF icDumpMode = "Modified" AND
      FuncRunQSchedule.StartTS <= idLastDump THEN NEXT.

   FIND FIRST FuncRunQueue WHERE
              FuncRunQueue.FRQueueID = FuncRunQSchedule.FRQueueID AND
              FuncRunQueue.Active = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunQueue THEN NEXT.

   FOR EACH FuncRunExec WHERE
            FuncRunExec.FRQScheduleID = FuncRunQSchedule.FRQScheduleID NO-LOCK,
       EACH FuncRunProcess WHERE
            FuncRunProcess.FRConfigID = FuncRunExec.FRConfigID AND
            FuncRunProcess.FRExecID   = FuncRunExec.FRExecID NO-LOCK,
      FIRST FuncRunConfig WHERE
            FuncRunConfig.FRConfigID = FuncRunExec.FRConfigID NO-LOCK:

      oiEvents = oiEvents + 1.

      /* Dump BillRun statistics */
      PUT STREAM sFile UNFORMATTED
          fNotNull(STRING(FuncRunQueue.FRQueueID))                        lcDel
          fNotNull(STRING(FuncRunQueue.QueueDesc))                        lcDel
          fNotNull(STRING(FuncRunQSchedule.FRQScheduleID))                lcDel
          fNotNull(STRING(fTimeStamp2DateTime(FuncRunQSchedule.StartTS))) lcDel
          fNotNull(STRING(fTimeStamp2DateTime(FuncRunQSchedule.DoneTS)))  lcDel
          fNotNull(STRING(FuncRunQSchedule.RunState))                     lcDel
          fNotNull(STRING(FuncRunQSchedule.RunMode))                      lcDel
          fNotNull(STRING(FuncRunConfig.FRConfigID))                      lcDel
          fNotNull(STRING(FuncRunConfig.ConfName))                        lcDel
          fNotNull(STRING(FuncRunExec.FRExecID))                          lcDel
          fNotNull(STRING(FuncRunExec.FRQRowSeq))                         lcDel
          fNotNull(STRING(fTimeStamp2DateTime(FuncRunExec.StartTS)))      lcDel
          fNotNull(STRING(fTimeStamp2DateTime(FuncRunExec.EndTS)))        lcDel
          fNotNull(STRING(FuncRunExec.RunState))                          lcDel
          fNotNull(STRING(FuncRunProcess.FRProcessID))                    lcDel
          fNotNull(STRING(FuncRunProcess.ProcSeq))                        lcDel
          fNotNull(STRING(fTimeStamp2DateTime(FuncRunProcess.StartTS)))   lcDel
          fNotNull(STRING(fTimeStamp2DateTime(FuncRunProcess.EndTS)))     lcDel
          fNotNull(STRING(FuncRunProcess.RunState))                       lcDel
          fNotNull(STRING(FuncRunProcess.Processed))                      SKIP.
   END. /* FOR EACH FuncRunExec NO-LOCK WHERE */
END. /* FOR EACH FuncRunQSchedule NO-LOCK: */

IF NOT SESSION:BATCH THEN
   HIDE FRAME fQty NO-PAUSE.

OUTPUT STREAM sFile CLOSE.

/* Copy the report to Track directory */
fCopy2TargetDir(icFile, ".txt", lcTrackDir).

SESSION:NUMERIC-FORMAT = lcNumeric.

