/* ----------------------------------------------------------------------
  MODULE .......: ifs_installment_cancel.p
  TASK .........: Create a dump file for IFS from installment cancellings
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 17.02.10
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}

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
DEF VAR ldaFrom       AS DATE NO-UNDO.
DEF VAR ldaTo         AS DATE NO-UNDO.
DEF VAR ldaActDate    AS DATE NO-UNDO.
DEF VAR liTime        AS INT  NO-UNDO.
DEF VAR ldDebt        AS DEC  NO-UNDO.
   
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

/* check from last 2 months if there are ones that have been completed 
   during last month */
IF icDumpMode = "modified" THEN ASSIGN
   ldaTo   = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
   ldaFrom = DATE(MONTH(ldaTo),1,YEAR(ldaTo))
   ldFrom  = fMake2Dt(ldaFrom,0)
   ldTo    = fMake2Dt(ldaTo,86399)
   ldCheck = fMake2Dt(ldaFrom - 20,0).
/* take all */
ELSE ASSIGN
   ldFrom  = fMake2Dt(2/1/10,0)
   ldTo    = fMake2Dt(TODAY - 1,86399)
   ldCheck = ldFrom.


RequestLoop:
FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand     = gcBrand AND
         MsRequest.ReqType   = 9       AND
         MsRequest.ReqStatus = 2       AND
         MsRequest.ActStamp >= ldCheck AND
         MsRequest.DoneStamp >= ldFrom AND
         MsRequest.DoneStamp <= ldTo,
   FIRST DayCampaign NO-LOCK USE-INDEX DCEvent WHERE
         DayCampaign.Brand = gcBrand AND
         DayCampaign.DCEvent = MsRequest.ReqCParam3 AND
         DayCampaign.DCType = "5"
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   fSplitTS(MsRequest.ActStamp,
            OUTPUT ldaActDate,
            OUTPUT liTime).

   FIND FIRST DCCLI WHERE
              DCCLI.MsSeq    = MsRequest.MsSeq AND
              DCCLI.DCEvent  = MsRequest.ReqCParam3 AND
              DCCLI.TermDate = ldaActDate NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DCCLI THEN NEXT.
             
   /* rest of the unbilled fixed fees are converted into a single fee in
      termination */
   ldDebt = 0.    
   FIND FIRST SingleFee WHERE
              SingleFee.Brand     = gcBrand AND
              SingleFee.HostTable = "MobSub" AND
              SingleFee.KeyValue  = STRING(MsRequest.MsSeq) AND
              SingleFee.CalcObj   = "DC" + STRING(DCCLI.PerContractID)
   NO-LOCK NO-ERROR.           
   IF AVAILABLE SingleFee THEN ldDebt = SingleFee.Amt. 

   PUT STREAM sLog UNFORMATTED 
      "H"                             lcDelimiter  /*  1: line_type */
      MsRequest.CustNum               lcDelimiter  /*  2: identity  */
      MsRequest.MsSeq                 lcDelimiter  /*  3: subscription_id */
      ldDebt                          lcDelimiter  /*  4: amount */
      fDate2String(ldaActDate)        lcDelimiter  /*  5: cancel_date */
      SKIP.
 
   oiEvents = oiEvents + 1.
   IF NOT SESSION:BATCH AND oiEvents MOD 10 = 0 THEN DO:
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
