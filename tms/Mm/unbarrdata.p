/* -----------------------------------------------
  MODULE .......: unbarradata.p
  FUNCTION .....: Unbarr all data subscriptions
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 16.06.08
  MODIFIED .....: 
------------------------------------------------------ */

{commpaa.i}
ASSIGN
   katun   = "cron"
   gcBrand = "1".
   
{eventlog.i}
{timestamp.i}

DEF VAR liQty      AS INT  NO-UNDO.
DEF VAR ldtFrom    AS DATE NO-UNDO.
DEF VAR ldtTo      AS DATE NO-UNDO.
DEF VAR ldBegStamp AS DEC  NO-UNDO.
DEF VAR ldEndStamp AS DEC  NO-UNDO.
DEF VAR lcBarrPack AS CHAR NO-UNDO.
DEF VAR lcCLITypes AS CHAR NO-UNDO.
DEF VAR lcResult   AS CHAR NO-UNDO.
DEF VAR liRequest  AS INT  NO-UNDO.
DEF VAR lcFailed   AS CHAR NO-UNDO.
DEF VAR llDone     AS LOG  NO-UNDO.
DEF VAR ldCurrent  AS DEC  NO-UNDO.

DEF BUFFER bRequest FOR MsRequest.

ASSIGN
   ldtTo      = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
   ldtFrom    = DATE(MONTH(ldtTo),1,YEAR(ldtTo))
   ldBegStamp = fMake2Dt(ldtFrom,0)
   ldEndStamp = fMake2Dt(ldtTo,86399)
   lcBarrPack = fCParamC("UnbarrDataPackage")
   lcCLITypes = fCParamC("UnbarrDataCLITypes").


fELog("MONTHLY","UnbarrDataStarted").

/* find all barrings that were made during last month, and unbarr them */
FOR EACH MsRequest NO-LOCK USE-INDEX ReqType WHERE
         MsRequest.Brand      = gcBrand    AND
         MsRequest.ReqType    = 35         AND
         MsRequest.ReqStat    = 2          AND
         MsRequest.ActStamp  >= ldBegStamp AND
         MsRequest.ActStamp  <= ldEndStamp AND
         MsRequest.ReqCParam1 = lcBarrPack AND
         MsRequest.ReqSource = "5",
   FIRST MsOwner NO-LOCK WHERE
         MsOwner.MsSeq   = MsRequest.MsSeq    AND
         MsOwner.TsBeg  <= MsRequest.ActStamp AND
         MsOwner.TsEnd  >= MsRequest.ActStamp AND
         LOOKUP(MsOwner.CLIType,lcCLITypes) > 0:

   /* subscription already terminated -> no action needed */
   FIND FIRST MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN NEXT.

   /* current clitype is not checked; if clitype was changed and barrings were
      not released then, this takes care of that now */
      
   ASSIGN
      llDone    = FALSE
      ldCurrent = fMakeTS() + 0.0012.

   /* already unbarred */ 
   FOR FIRST bRequest NO-LOCK USE-INDEX MsSeq WHERE
             bRequest.MsSeq      = MsRequest.MsSeq        AND
             bRequest.ReqType    = 35                     AND
             /* pending or succesfully done */
             LOOKUP(STRING(bRequest.ReqStat),"0,1,2") > 0 AND
             bRequest.ActStamp   > MsRequest.ActStamp     AND
             bRequest.ActStamp  <= ldCurrent              AND
             bRequest.ReqCParam1 = "UN" + lcBarrPack:
      llDone = TRUE.
   END.
       
   IF llDone THEN NEXT.
         
   /* create unbarring request */   
   RUN barrengine (MsRequest.MsSeq,
                   "UN" + lcBarrPack,  /* package for unbarring */
                   "5",                /* source  */
                   "UnbarrData",       /* creator */
                   ldCurrent,          /* activate */
                   "",                 /* sms-text */
                   OUTPUT lcResult).

   liRequest = 0.
   liRequest = INTEGER(lcResult) NO-ERROR.
   
   /* unbarring request could not be created */
   IF liRequest = 0 THEN
      lcFailed = lcFailed + (IF lcFailed > "" THEN "," ELSE "") +
                 MsRequest.CLI + "/" + STRING(MsRequest.MsRequest).
                               
   ELSE DO:
      liQty = liQty + 1.
      
      /* mark barring request as father request? */
   END.
END.

DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "DataBarr"  
      ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                               STRING(MONTH(TODAY),"99")  +
                               STRING(DAY(TODAY),"99")
      ActionLog.ActionID     = "Unbarr"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                               MONTH(TODAY)
      ActionLog.ActionDec    = liQty
      ActionLog.ActionChar   = STRING(liQty) + 
                               " requests were created for unbarring. " + 
                               CHR(10) + 
                               (IF lcFailed > "" 
                                THEN "Failed: " + lcFailed
                                ELSE "")
      ActionLog.ActionStatus = 3
      ActionLog.UserCode     = "Cron"
      ActionLog.FromDate     = ldtFrom
      ActionLog.ToDate       = ldtTo.
      ActionLog.ActionTS     = fMakeTS().
END.

fELog("MONTHLY","UnbarrDataStopped:" + STRING(liQty)).

