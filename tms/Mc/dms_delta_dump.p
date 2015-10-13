/* ----------------------------------------------------------------------
  MODULE .......: dms_delta_dump.p
  TASK .........: Collect rows to a dump file for DMS records
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
DEF VAR ldFromStamp  AS DEC      NO-UNDO.

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

fSplitTS(idLastDump,
         OUTPUT ldaModified,
         OUTPUT liCnt).

ASSIGN
   lhTable     = BUFFER DMS:HANDLE
   lcKeyFields = fEventKeyFields(lhTable).

IF icDumpMode = "Modified" THEN ldFromStamp = idLastDump.
ELSE ldFromStamp = 20060101.

OUTPUT STREAM sFile TO VALUE(icFile).

FOR EACH DMS NO-LOCK WHERE
         DMS.StatusTS >= ldFromStamp
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.
   
   DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

      lcField = ENTRY(liCnt,lcDumpFields).

      IF lcField BEGINS "#" THEN DO:

         CASE lcField:
            WHEN "#TypeOfID" THEN DO:
               IF DMS.HostTable = "MsRequest" THEN DO:
                  FIND FIRST MsRequest NO-LOCK WHERE
                             MsRequest.MsRequest = DMS.HostID
                             NO-ERROR.
                  IF AVAILABLE MsRequest THEN DO:
                     CASE MsRequest.ReqType:
                        WHEN {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} THEN
                           lcValue = "STC_ID".
                        WHEN {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE} THEN
                           lcValue = "ACC_ID".
                        OTHERWISE
                           lcValue = "REQ_ID".
                     END CASE.
                  END.
               END.
               ELSE IF DMS.HostTable = "Order" THEN 
                  lcValue = "Order_ID".
               ELSE
                  lcValue = "ID".
            END.
            WHEN "#OrderStatus" THEN DO:
               FIND FIRST Order NO-LOCK WHERE
                          Order.Brand = gcBrand AND
                          Order.OrderID = DMS.HostID NO-ERROR.
               IF AVAIL Order AND DMS.HostTable = "Order" THEN
                  lcValue = Order.StatusCode.
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
      DISP oiEvents LABEL "DMS Records" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.

END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.


