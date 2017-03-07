/* ----------------------------------------------------------------------
  MODULE .......: cctool_dump.p
  TASK .........: Create a dump file for charge/comp  requests
  APPLICATION ..: tms
  AUTHOR .......: rafaeldv 
  CREATED ......: 17.06.2009
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}
{Func/create_eventlog.i}
{Func/timestamp.i}

DEF TEMP-TABLE ttSource NO-UNDO
   FIELD PPSource AS CHAR.
   
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
DEF VAR ldDate       AS DATE     NO-UNDO.
DEF VAR liTime       AS INT      NO-UNDO.
DEF VAR ldAmount     AS DEC      NO-UNDO.

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

CREATE ttSource.
ASSIGN ttSource.PPSource = "CHARGE".
CREATE ttSource.
ASSIGN ttSource.PPSource = "COMP".


OUTPUT STREAM sFile TO VALUE(icFile).

IF icDumpMode NE "modified" THEN idLastDump = 0.

/* postpaid charges and compensation -----------------------------*/
MsRequestLoop:
FOR EACH MsRequest NO-LOCK USE-INDEX CLI WHERE
         MsRequest.Brand   = gcBrand AND
         MsRequest.ReqType = 76 AND
         MsRequest.CreStamp >= idLastDump,
    FIRST FMItem NO-LOCK WHERE 
          FMItem.Brand = gcBrand AND
          FMItem.FeeModel = MsRequest.ReqCParam1 AND
          DATETIME(FMItem.ToDate) >= 
             fTimeStamp2DateTime(MsRequest.CreStamp) AND 
          DATETIME(FMItem.FromDate) <= fTimeStamp2DateTime(MsRequest.CreStamp),
    FIRST BillItem NO-LOCK WHERE 
          BillItem.Brand = gcBrand AND
          BillItem.BillCode = FMItem.BillCode
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   lcCreator = MsRequest.UserCode.
   lcCreator = TRIM(ENTRY( NUM-ENTRIES(lcCreator,"/"),lcCreator, "/" )).
   FIND FIRST TMSUser WHERE TMSUser.UserCode = lcCreator NO-LOCK NO-ERROR.
   IF AVAILABLE TMSUser THEN lcCreator = TMSUser.UserName.

   fSplitTS (MsRequest.CreStamp,
             OUTPUT ldDate,
             OUTPUT liTime).

   ldAmount = ABSOLUTE(MsRequest.ReqDParam1).


   DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

      lcField = ENTRY(liCnt,lcDumpFields).
      
      IF lcField BEGINS "#" THEN DO:
         CASE lcField:
         WHEN "#Creator"   THEN lcValue = lcCreator.
         WHEN "#BillCode"  THEN lcValue = BillItem.BillCode.
         WHEN "#BIName"    THEN lcValue = BillItem.BIName.
         WHEN "#SubType"   THEN lcValue = "POSTPAID".
         WHEN "#Date"      THEN lcValue = STRING(ldDate).
         WHEN "#Time"      THEN lcValue = STRING(liTime,"hh:mm:ss").
         WHEN "#Amount"    THEN lcValue = STRING(ldAmount,"->>9.9<").
         WHEN "#EventType" THEN 
            IF MsRequest.ReqDParam1 > 0 THEN lcValue = "CHARGE". 
            ELSE lcValue = "COMP".
         WHEN "#MSISDN"    THEN lcValue = MsRequest.CLI.
         OTHERWISE lcValue = "".
         END CASE.
      END.
      ELSE lcValue = "". 
      
      PUT STREAM sFile UNFORMATTED
         lcValue.

      IF liCnt < NUM-ENTRIES(lcDumpFields) THEN 
      PUT STREAM sFile UNFORMATTED
         lcDelimiter.
   
   END.
 
   PUT STREAM sFile UNFORMATTED
      SKIP.
   
   oiEvents = oiEvents + 1.

END. /* end MsRequestLoop ----------------------------------------*/



/* prepaid charges and compensation -----------------------------*/

PrePaidRequestLoop:
FOR EACH ttSource,
    EACH PrePaidRequest NO-LOCK WHERE
         PrePaidRequest.Brand  = gcBrand AND
         PrePaidRequest.Source = ttSource.PPSource AND
         PrePaidRequest.TSRequest >= idLastDump, 
    FIRST FMItem NO-LOCK WHERE 
          FMItem.Brand = gcBrand AND
          FMItem.FeeModel = PrePaidRequest.ReqCParam1 AND
          DATETIME(FMItem.ToDate) >= 
             fTimeStamp2DateTime(PrePaidRequest.TSRequest) AND 
          DATETIME(FMItem.FromDate) <= 
             fTimeStamp2DateTime(PrePaidRequest.TSRequest),
    FIRST BillItem NO-LOCK WHERE 
          BillItem.Brand = gcBrand AND
          BillItem.BillCode = FMItem.BillCode
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.
  
   lcCreator = PrePaidRequest.UserCode.
   lcCreator = TRIM(ENTRY( NUM-ENTRIES(lcCreator,"/"),lcCreator, "/" )).
   FIND FIRST TMSUser WHERE TMSUser.UserCode = lcCreator NO-LOCK NO-ERROR.
   IF AVAILABLE TMSUser THEN lcCreator = TMSUser.UserName.

   fSplitTS (PrePaidRequest.TSRequest,
             OUTPUT ldDate,
             OUTPUT liTime).

   ldAmount = ABSOLUTE(PrePaidRequest.TopUpAmt + PrePaidRequest.VatAmt) / 100 .

   DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

      lcField = ENTRY(liCnt,lcDumpFields).
      
      IF lcField BEGINS "#" THEN DO:
         CASE lcField:
         WHEN "#Creator"   THEN lcValue = lcCreator.
         WHEN "#BillCode"  THEN lcValue = BillItem.BillCode.
         WHEN "#BIName"    THEN lcValue = BillItem.BIName.
         WHEN "#SubType"   THEN lcValue = "PREPAID".
         WHEN "#Date"      THEN lcValue = STRING(ldDate).
         WHEN "#Time"      THEN lcValue = STRING(liTime,"hh:mm:ss").
         WHEN "#Amount"    THEN lcValue = STRING(ldAmount,"->>9.9<").
         WHEN "#EventType" THEN lcValue = PrePaidRequest.Source.
         WHEN "#MSISDN"    THEN lcValue = PrePaidRequest.CLI.
         OTHERWISE lcValue = "".
         END CASE.
      END.
      ELSE lcValue = "".
      
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

    
END. /* end PrePaidRequestLoop ----------------------------------------*/


IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.


