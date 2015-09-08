/* ----------------------------------------------------------------------
  MODULE .......: cdl_received_dump.p
  TASK .........: Creates a dump file for received CDL offer messages. YPR-2106
  APPLICATION ..: tms
  AUTHOR .......: anttis 
  CREATED ......: 13.5.2015
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{dumpfile_run.i}
{create_eventlog.i}
{tmsconst.i}
{cparam2.i}
{smsmessage.i}

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
DEF VAR lcKeyFields  AS CHAR     NO-UNDO.
DEF VAR lcDumpFields AS CHAR     NO-UNDO.
DEF VAR lcValue      AS CHAR     NO-UNDO.
DEF VAR lhField      AS HANDLE   NO-UNDO.
DEF VAR lcField      AS CHAR     NO-UNDO.

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
   lhTable     = BUFFER SMSMessage:HANDLE
   lcKeyFields = fEventKeyFields(lhTable).

OUTPUT STREAM sFile TO VALUE(icFile).

IF icDumpMode EQ "full" then
   idLastDump = 0.

FOR EACH SMSMessage NO-LOCK WHERE
         SMSMessage.CreStamp >= idLastDump AND
         SMSMessage.DeliType = {&SMS_DELITYPE_IN}
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
         WHEN "#CONTRACT_ID" THEN DO:
            FIND FIRST Order NO-LOCK WHERE
                       Order.Brand = gcBrand AND
                       Order.OrderID = SMSMessage.OrderID NO-ERROR.
            IF AVAIL Order AND SMSMessage.OrderID > 0 THEN
               lcValue = Order.ContractId.
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
      DISP oiEvents LABEL "CDL received messages" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END.                       

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.
