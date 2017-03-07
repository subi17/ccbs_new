/* ----------------------------------------------------------------------
  MODULE .......: mservicelpool_dump
  TASK .........: Create a dump file for the MServiceLPool 
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

DEF VAR lcNumeric    AS CHAR     NO-UNDO.
DEF VAR lcDelimiter  AS CHAR     NO-UNDO.
DEF VAR lcCreator    AS CHAR     NO-UNDO.
DEF VAR liCnt        AS INT      NO-UNDO.
DEF VAR lcDumpFields AS CHAR     NO-UNDO.
DEF VAR lcValue      AS CHAR     NO-UNDO.
DEF VAR lcField      AS CHAR     NO-UNDO. 
DEF VAR lhField      AS HANDLE   NO-UNDO.

DEF VAR ldaModified      AS DATE   NO-UNDO.
DEF VAR liTimeMod        AS INT   NO-UNDO.
DEF VAR lhMServiceLPool  AS HANDLE NO-UNDO.
DEF VAR ldtLastDump      AS DATETIME NO-UNDO.
DEF VAR lcModFields      AS CHAR   NO-UNDO.
DEF STREAM sFile.


DEF VAR ldValidTo AS DEC NO-UNDO.
DEF VAR lcContract AS CHAR NO-UNDO. 

DEF VAR lcEventKey AS CHAR NO-UNDO.
DEF VAR ldEventTS  AS DEC NO-UNDO.


DEF BUFFER bMServiceLPool FOR MServiceLPool. 

DEF TEMP-TABLE ttPicked NO-UNDO 
   FIELD ContrID   AS RECID
   INDEX ContrID   ContrID.
  
FUNCTION fCollectMServiceLPool RETURNS LOGIC:

   IF CAN-FIND(FIRST ttPicked WHERE 
                     ttPicked.ContrID   = RECID(MServiceLPool)) THEN 
      RETURN FALSE.

   CREATE ttPicked.
   ASSIGN ttPicked.ContrID   = RECID(MServiceLPool).

   RETURN TRUE. 
END FUNCTION.

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
         OUTPUT liTimeMod).

ldtLastDump = fTimeStamp2DateTime(idLastDump).

ASSIGN lhMServiceLPool = BUFFER MServiceLPool:HANDLE.

OUTPUT STREAM sFile TO VALUE(icFile).

IF icDumpMode = "modified" THEN DO:
   
   IF AVAILABLE DumpFile THEN lcModFields = DumpFile.EventLogFields.

   DO liCnt = 1 TO NUM-ENTRIES(icEventSource,"|"):
   
      /* check modification from eventlog */
      IF ENTRY(liCnt,icEventSource,"|") = "EventLog" THEN DO:
      
         RUN pFindFromEventLog(lhMServiceLPool,
                               ENTRY(liCnt,icEventFields,"|"),
                               lcModFields,
                               idLastDump,
                               "fCollectMServiceLPool").
      END.
      
      /* check modification from a xx field */
      ELSE IF ENTRY(liCnt,icEventSource,"|") = "field" AND
              ENTRY(liCnt,icEventFields,"|") = "xx" 
      THEN DO:
      END.
   END.

   FOR EACH ttPicked,
      FIRST MServiceLPool NO-LOCK WHERE 
            RECID(MServiceLPool) = ttPicked.ContrID :
      RUN pReadMServiceLPool.
      RUN pWriteMServiceLPool.
   END.


END.   /* modified */
ELSE DO: 

   /* all MServiceLPool ------------------------------------*/
   MServiceLPool_loop:
   FOR EACH MServiceLPool NO-LOCK
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      RUN pReadMServiceLPool.
      RUN pWriteMServiceLPool.

      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
             PAUSE 0. 
             DISP oiEvents LABEL "MServiceLPool" 
             WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
             TITLE " Collecting " FRAME fQty.
      END.

   END. /* end loop ----------------------------------------*/

END.  /* full dump */

IF NOT SESSION:BATCH THEN HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.

PROCEDURE pReadMServiceLPool :

      
      DEF VAR lcGroupCode AS CHAR NO-UNDO. 
      DEF VAR lcSLCode AS CHAR NO-UNDO. 
      DEF VAR liServTime AS INT NO-UNDO. 

      lcEventKey = fGetEventKey(lhMServiceLPool).
      FIND FIRST EventLog NO-LOCK USE-INDEX TableName WHERE
                 EventLog.TableName = lhMServiceLPool:Name AND
                 EventLog.Key       = lcEventKey NO-ERROR.
      IF NOT AVAIL EventLog THEN RETURN.
      ldEventTS = fHMS2TS(EventLog.EventDate, EventLog.EventTime).

      FIND FIRST ServiceLimit WHERE
                 ServiceLimit.SLSeq    = MServiceLPool.SLSeq NO-LOCK NO-ERROR.
      IF AVAIL ServiceLimit THEN ASSIGN  
         lcGroupCode = ServiceLimit.groupcode
         lcSLCode    = ServiceLimit.SLCode.
      ELSE RETURN.

       IF lcSLCode NE "" THEN 
         lcContract = lcGroupCode + "/" + lcSLCode. 

      IF MServiceLPool.EndTs >= 99999999 THEN ldValidTo = fHMS2TS(12/31/2049,"0").
      ELSE 
         ldValidTo = MServiceLPool.EndTS. 


END PROCEDURE.

PROCEDURE pWriteMServiceLPool :

      DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

         lcField = ENTRY(liCnt,lcDumpFields).
      
         IF lcField BEGINS "#" THEN DO:
            CASE lcField:
            WHEN "#Contract" THEN lcValue = lcContract.
            WHEN "#RowId" THEN lcValue =  STRING( ROWID(MServiceLPool)).
            WHEN "#ValidFrom" THEN lcValue = fTS2HMS(MServiceLPool.FromTS).
            WHEN "#ValidTo" THEN lcValue = fTS2HMS(ldValidTo).
            WHEN "#EventTS" THEN lcValue = fTS2HMS(ldEventTS).
            WHEN "#AgrCustomer" THEN lcValue = STRING(MServiceLPool.CustNum).
            OTHERWISE lcValue = "".
            END CASE.
         END.
         ELSE DO:
            lhField = lhMServiceLPool:BUFFER-FIELD(lcField).
            IF lhField:DATA-TYPE = "DECIMAL" THEN
               lcValue = TRIM(STRING(lhField:BUFFER-VALUE,"->>>,>>>,>>9.9<")).
            ELSE lcValue = lhField:BUFFER-VALUE.
         END.

         PUT STREAM sFile UNFORMATTED lcValue.

         IF liCnt < NUM-ENTRIES(lcDumpFields) THEN 
         PUT STREAM sFile UNFORMATTED lcDelimiter.
      
      END.
    
      PUT STREAM sFile UNFORMATTED  SKIP.
   
      oiEvents = oiEvents + 1.

      lcContract = "".

END PROCEDURE.

