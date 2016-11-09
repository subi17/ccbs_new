/* ----------------------------------------------------------------------
  MODULE .......: termreturn_dump.p
  TASK .........: Collect rows to a dump file for termreturn records
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 3.2.2015
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
   lhTable     = BUFFER TermReturn:HANDLE
   lcKeyFields = fEventKeyFields(lhTable).

IF icDumpMode = "Modified" THEN ldFromStamp = idLastDump.
ELSE ldFromStamp = 20150101. /*Full dump*/

OUTPUT STREAM sFile TO VALUE(icFile).

FOR EACH TermReturn NO-LOCK WHERE
         TermReturn.ReturnTS >= ldFromStamp
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
            WHEN "#ResultInPos" THEN DO:
               IF((TermReturn.DeviceScreen EQ TRUE AND
                   TermReturn.DeviceStart EQ TRUE) OR
                  (TermReturn.DeviceScreen EQ ? AND
                   TermReturn.DeviceStart  EQ ?)) THEN 
                  lcValue = "OK".
               ELSE
                  lcValue = "NOK".
            END.
            WHEN "#ReturnTypeId" THEN DO:
               IF TermReturn.EnvelopeNumber EQ "" THEN
                  lcValue = "Q25TPB".
               ELSE
                  lcValue = "Q25TPE".
            END.
            WHEN "#CustAddr" THEN DO:
               lcValue = "".
            END.
            WHEN "#CustContactNum" THEN DO:
               lcValue = "".
            END.
            WHEN "#POSScreening" THEN DO:
               IF TermReturn.DeviceStart EQ ? AND 
                  TermReturn.DeviceScreen EQ ? THEN
                  lcValue = "BASIC".
               ELSE
                  lcValue = "DETAILED".
            END.
            WHEN "#Q25Amount" THEN DO:
               FIND FIRST Order NO-LOCK WHERE
                          Order.Brand   = gcBrand AND
                          Order.OrderId = TermReturn.OrderId NO-ERROR.
               IF AVAILABLE Order THEN DO:
                  FIND SingleFee WHERE
                       SingleFee.Brand       = gcBrand AND
                       SingleFee.HostTable   = "Mobsub" AND
                       SingleFee.KeyValue    = STRING(Order.MsSeq) AND
                       SingleFee.OrderId     = Order.OrderId AND
                       SingleFee.CalcObj     = "RVTERM" NO-LOCK NO-ERROR.
                  IF AVAIL SingleFee THEN lcValue = STRING(ROUND(SingleFee.Amt,2)).
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
      DISP oiEvents LABEL "TermReturn Records" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.

END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.


