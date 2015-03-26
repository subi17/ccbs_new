/* ----------------------------------------------------------------------
  MODULE .......: ifs_installment.p
  TASK .........: Create a dump file for IFS from monthly installments
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 17.02.10
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{dumpfile_run.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR liBatches     AS INT  NO-UNDO.
DEF VAR ldFrom        AS DEC  NO-UNDO.
DEF VAR ldTo          AS DEC  NO-UNDO.
DEF VAR ldCheck       AS DEC  NO-UNDO.
DEF VAR lcDelimiter   AS CHAR NO-UNDO INIT ";".
DEF VAR ldaDueDate    AS DATE NO-UNDO.
   
DEFINE STREAM sLog.


FUNCTION fDispDecimal RETURNS CHAR
   (idAmount AS DEC):

   RETURN TRIM(REPLACE(STRING(idAmount,"->>>>>>>>>9.99"),",",".")).
      
END FUNCTION.

FUNCTION fDate2String RETURNS CHAR
   (idaDate AS DATE):
   
   IF idaDate = ? THEN RETURN "".
   
   RETURN STRING(YEAR(idaDate),"9999") +
          STRING(MONTH(idaDate),"99")  +
          STRING(DAY(idaDate),"99").
   
END FUNCTION.



/******* MAIN start *********/

OUTPUT STREAM sLog TO VALUE(icFile).

/* check from last 20 days if there are ones that have been completed 
   yesterday */
IF icDumpMode = "modified" THEN ASSIGN
   ldFrom  = fMake2Dt(TODAY - 1,0)
   ldTo    = fMake2Dt(TODAY - 1,86399)
   ldCheck = fMake2Dt(TODAY - 20,0).
/* take all */
ELSE ASSIGN
   ldFrom  = fMake2Dt(2/1/10,0)
   ldTo    = fMake2Dt(TODAY - 1,86399)
   ldCheck = ldFrom.


FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.


RequestLoop:
FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand     = gcBrand AND
         MsRequest.ReqType   = 8       AND
         MsRequest.ReqStatus = 2       AND
         MsRequest.ActStamp >= ldCheck AND
         MsRequest.DoneStamp >= ldFrom AND
         MsRequest.DoneStamp <= ldTo,
   FIRST DayCampaign NO-LOCK USE-INDEX DCEvent WHERE
         DayCampaign.Brand = gcBrand AND
         DayCampaign.DCEvent = MsRequest.ReqCParam3 AND
         DayCampaign.DCType = "5",
   FIRST DCCLI NO-LOCK WHERE
         DCCLI.MsSeq   = MsRequest.MsSeq AND
         DCCLI.DCEvent = MsRequest.ReqCParam3,
   FIRST FixedFee NO-LOCK WHERE
         FixedFee.Brand     = gcBrand AND
         FixedFee.HostTable = "MobSub" AND
         FixedFee.KeyValue  = STRING(MsRequest.MsSeq) AND
         FixedFee.FeeModel  = DayCampaign.FeeModel
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   /* calculate these directly from items (not from FMItem.FFItemQty), 
      to get the actualized quantity */
   liBatches = 0.
   FOR EACH FFItem OF FixedFee NO-LOCK:
      liBatches = liBatches + 1.
   END.

   /* due date is the 6. of the second month */
   IF MONTH(DCCLI.ValidFrom) = 12 THEN 
      ldaDueDate = DATE(1,6,YEAR(DCCLI.ValidFrom) + 1).
   ELSE ldaDueDate = DATE(MONTH(DCCLI.ValidFrom) + 1,6,YEAR(DCCLI.ValidFrom)).
   
   PUT STREAM sLog UNFORMATTED 
      "H"                             lcDelimiter  /*  1: line_type */
      MsRequest.CustNum               lcDelimiter  /*  2: identity  */
      MsRequest.MsSeq                 lcDelimiter  /*  3: subscription_id */
      FixedFee.Amt                    lcDelimiter  /*  4: amount */
      liBatches                       lcDelimiter  /*  5: number_items */
      fDate2String(DCCLI.ValidFrom)   lcDelimiter  /*  6: accounting_date */
      fDate2String(ldaDueDate)        lcDelimiter  /*  7: due_date */
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
   
OUTPUT STREAM sLog CLOSE.


/******* MAIN end *********/

