/* ----------------------------------------------------------------------
  MODULE .......: dmsdoc_delta_dump.p
  TASK .........: Collect rows to a dump file for DMSDoc records
  APPLICATION ..: tms
  AUTHOR .......: ivekov
  CREATED ......: 7.9.2015
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{dumpfile_run.i}
{tmsconst.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric       AS CHAR     NO-UNDO.
DEF VAR lcDelimiter     AS CHAR     NO-UNDO.
DEF VAR ldaModified     AS DATE     NO-UNDO.
DEF VAR liCnt           AS INT      NO-UNDO.
DEF VAR lhTable         AS HANDLE   NO-UNDO.
DEF VAR lcKeyFields     AS CHAR     NO-UNDO.
DEF VAR lcDumpFields    AS CHAR     NO-UNDO.
DEF VAR lcValue         AS CHAR     NO-UNDO.
DEF VAR lhField         AS HANDLE   NO-UNDO.
DEF VAR lcField         AS CHAR     NO-UNDO.
DEF VAR ldaLastDumpDate AS DATE     NO-UNDO.
DEF VAR liLastDumpTime  AS INT      NO-UNDO.
DEF VAR lcLastDumpTime  AS CHAR     NO-UNDO.
DEF VAR liDMSID         AS INT      NO-UNDO. 

DEF STREAM sFile.

lcNumeric = SESSION:NUMERIC-FORMAT.

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF NOT AVAIL DumpFile THEN DO:
   olInterrupted = TRUE.
   RETURN.
END.

lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).

IF DumpFile.DecimalPoint = "." 
THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".

FOR EACH DFField OF DumpFile NO-LOCK WHERE
         DFField.ToDate   >= TODAY AND
         DFField.FromDate <= TODAY
BY DFField.OrderNbr:
   lcDumpFields = lcDumpFields + (IF lcDumpFields > "" THEN "," ELSE "") +
                  DFField.DFField.
END.

ASSIGN
   lhTable     = BUFFER DMSDoc:HANDLE
   lcKeyFields = fEventKeyFields(lhTable).

OUTPUT STREAM sFile TO VALUE(icFile).

IF idLastDump > 0 THEN
   fSplitTs(idLastDump, OUTPUT ldaLastDumpDate, OUTPUT liLastDumpTime).

lcLastDumpTime = STRING(liLastDumpTime,"hh:mm:ss").


IF icDumpMode EQ "Full" THEN
   FOR EACH EventLog NO-LOCK WHERE
            EventLog.TableName = "DMSDoc"
            USE-INDEX TableName:

      RUN pWriteFile.
   END.
ELSE FOR EACH EventLog NO-LOCK WHERE
              EventLog.EventDate >= ldaLastDumpDate AND
              EventLog.TableName = "DMSDoc"
              USE-INDEX EventDate:
      IF EventLog.EventDate EQ ldaLastDumpDate AND
         EventLog.EventTime < lcLastDumpTime THEN NEXT.

      RUN pWriteFile.   
   END.

PROCEDURE pWriteFile:

   liDMSID = INT(EventLog.Key) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN NEXT.

   FOR EACH DMSDoc NO-LOCK WHERE
            DMSDoc.DMSID = liDMSID
            ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.
      
      FIND FIRST DMS NO-LOCK WHERE
                 DMS.DMSID = DMSDoc.DMSID
                 NO-ERROR.
      IF NOT AVAILABLE DMS THEN NEXT.

      DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

         lcField = ENTRY(liCnt,lcDumpFields).

         IF lcField BEGINS "#" THEN DO:

            CASE lcField:
               WHEN "#OrderID" THEN DO:
                  IF DMS.HostTable = "MsRequest" THEN DO:
                     FIND FIRST MsRequest NO-LOCK WHERE
                                MsRequest.MsRequest = DMS.HostID
                                NO-ERROR.
                     IF AVAILABLE MsRequest THEN
                        lcValue = STRING(MsRequest.ReqIparam1).
                     ELSE lcValue = "".
                  END.
                  ELSE IF DMS.HostTable = "Order" THEN DO:
                     FIND FIRST Order NO-LOCK WHERE
                                Order.Brand = gcBrand AND
                                Order.OrderID = DMS.HostID NO-ERROR.
                     IF AVAILABLE Order THEN
                        lcValue = STRING(Order.OrderId).
                     ELSE lcValue = "".
                  END.
                  ELSE lcValue = "".
               END.
               OTHERWISE lcValue = "".
            END CASE.
         END.

         ELSE DO:
            lhField = lhTable:BUFFER-FIELD(lcField).
            lcValue = lhField:BUFFER-VALUE.
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
         DISP oiEvents LABEL "DMSDoc Records" 
         WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
            TITLE " Collecting " FRAME fQty.
      END.

   END.

END PROCEDURE.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.


