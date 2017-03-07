/* ----------------------------------------------------------------------
  MODULE .......: acc_request_dump.p
  TASK .........: Create a dump file for acc requests
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 18.05.09
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
DEF VAR lcCreator    AS CHAR     NO-UNDO.
DEF VAR ldaModified  AS DATE     NO-UNDO.
DEF VAR liCnt        AS INT      NO-UNDO.
DEF VAR lhTable      AS HANDLE   NO-UNDO.
DEF VAR lcModifier   AS CHAR     NO-UNDO.
DEF VAR ldModified   AS DEC      NO-UNDO.
DEF VAR lcKeyFields  AS CHAR     NO-UNDO.
DEF VAR lcEventKey   AS CHAR     NO-UNDO.
DEF VAR lcDumpFields AS CHAR     NO-UNDO.
DEF VAR lcValue      AS CHAR     NO-UNDO.
DEF VAR lhField      AS HANDLE   NO-UNDO.
DEF VAR lcField      AS CHAR     NO-UNDO. 

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
   lhTable     = BUFFER MsRequest:HANDLE
   lcKeyFields = fEventKeyFields(lhTable).

OUTPUT STREAM sFile TO VALUE(icFile).


MsRequestLoop:
FOR EACH MsRequest NO-LOCK USE-INDEX CLI WHERE
         MsRequest.Brand   = gcBrand AND
         MsRequest.ReqType = 10      
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   ASSIGN
      lcModifier = ""
      ldModified = 0.

   IF icDumpMode = "modified" THEN DO:
      IF MsRequest.CreStamp < idLastDump AND 
         MsRequest.UpdateStamp < idLastDump AND
         MsRequest.DoneStamp < idLastDump THEN NEXT MsRequestLoop.
   END.      
      
   /* get cancellation details from eventlog */
   IF MsRequest.ReqStat = 4 OR MsRequest.ReqStat = 9 THEN DO:
   
      lcEventKey = fEventKeyValues(lhTable,lcKeyFields).

      FOR EACH EventLog NO-LOCK WHERE
               EventLog.TableName  = "MsRequest" AND
               EventLog.Key        = lcEventKey  AND
               EventLog.EventDate >= ldaModified AND
               EventLog.Action     = "modify"
      BY EventLog.EventDate DESC
      BY EventLog.EventTime DESC:
      
         ldModified = fHMS2TS(EventLog.EventDate,EventLog.EventTime).
         IF ldModified >= MsRequest.DoneStamp THEN DO:
            lcModifier = EventLog.UserCode.
            LEAVE.
         END.
      END.
   END.

   lcCreator = MsRequest.UserCode.
   FIND FIRST TMSUser WHERE TMSUser.UserCode = lcCreator NO-LOCK NO-ERROR.
   IF AVAILABLE TMSUser THEN lcCreator = TMSUser.UserName.
   
   IF lcModifier > "" THEN DO:
      FIND FIRST TMSUser WHERE TMSUser.UserCode = lcModifier NO-LOCK NO-ERROR.
      IF AVAILABLE TMSUser THEN lcModifier = TMSUser.UserName.
   END.

   DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

      lcField = ENTRY(liCnt,lcDumpFields).
      
      IF lcField BEGINS "#" THEN DO:
         CASE lcField:
         WHEN "#Creator"  THEN lcValue = lcCreator.
         WHEN "#Modifier" THEN lcValue = lcModifier.
         WHEN "#Modified" THEN lcValue = STRING(ldModified,"99999999.99999").
         OTHERWISE lcValue = "".
         END CASE.
      END.
      
      ELSE DO:
         lhField = lhTable:BUFFER-FIELD(lcField).
      
         IF lhField:DATA-TYPE = "DECIMAL" THEN DO:
            IF INDEX(lhField:NAME,"stamp") > 0 OR 
               lhField:NAME = "ReqDParam1" THEN 
               lcValue = STRING(lhField:BUFFER-VALUE,"99999999.99999").
            ELSE 
               lcValue = TRIM(STRING(lhField:BUFFER-VALUE,"->>>,>>>,>>9.9<")).
         END.       
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
      DISP oiEvents LABEL "Requests" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END.                       

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.


