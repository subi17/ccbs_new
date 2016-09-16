{commpaa.i}
{tmsconst.i}
{cparam2.i}
{email.i}
katun = "Qvantel".
gcBrand  = "1".

DEFINE VARIABLE liCnt         AS INTEGER   NO-UNDO.
DEFINE VARIABLE llDone        AS LOGICAl   NO-UNDO INIT TRUE.
DEFINE VARIABLE ldeStartStamp AS DECIMAL   NO-UNDO FORMAT "99999999.99999".
DEFINE VARIABLE lcConfDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDoneDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile     AS CHARACTER NO-UNDO.

DEFINE BUFFER bfMSRequest FOR MSRequest.
DEFINE BUFFER bfMSOwner   FOR MSOwner.

ASSIGN lcConfDir     = fCParamC("RepConfDir")
       ldeStartStamp = YEAR(TODAY) * 10000 +
                       MONTH(TODAY) * 100 +
                       DAY(TODAY).

FOR EACH MSRequest WHERE
         MSRequest.Brand      = gcBrand  AND
         MSRequest.ReqType    = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
         MSRequest.ActStamp   >= ldeStartStamp NO-LOCK:

   FIND FIRST MobSub WHERE 
              MobSub.MsSeq = MSRequest.MsSeq NO-LOCK NO-ERROR.
   IF AVAIL MobSub THEN
   DO:
      FIND FIRST MSOwner WHERE 
                 MSOwner.MsSeq = MobSub.MsSeq NO-LOCK NO-ERROR.
      IF AVAIL MSOwner THEN
         FIND FIRST bfMSOwner WHERE
                    bfMSOwner.MsSeq   = MobSub.MsSeq AND 
                    bfMSOwner.TsBegin < MSOwner.TsBegin NO-LOCK NO-ERROR.

         IF AVAIL bfMSOwner THEN
         DO:
            IF ((bfMSOwner.paytype = NO AND MSOwner.paytype = NO) OR
                (bfMSOwner.paytype = NO AND MSOwner.PayType = YES)) AND 
                 MSRequest.ReqStatus <> {&REQUEST_STATUS_DONE} AND
                 MSRequest.ReqStatus <> {&REQUEST_STATUS_REJECTED} THEN
               ASSIGN liCnt = liCnt + 1.
            ELSE IF ((bfMSOwner.paytype = YES AND MSOwner.paytype = NO) OR
                     (bfMSOwner.paytype = YES AND MSOwner.PayType = YES)) AND
                     MSRequest.ReqStatus <> {&REQUEST_STATUS_DONE} THEN
               ASSIGN llDone = FALSE.       
         END.
   END.
END.

IF llDone AND liCnt <= 5 THEN
DO:
   GetRecipients(lcConfDir + "pmdubrenewal_barredcases.email").

   ASSIGN
      lcDoneDir  = fCParam("PrepaidBundle","InProcDir").
   RUN pmdub_batch_in.p.
   
   FIND FIRST ActionLog USE-INDEX ActionID WHERE
              ActionLog.Brand        = gcBrand AND    
              ActionLog.ActionID     = "PMDUB_IN" AND
              ActionLog.TableName    = "Cron" AND
              ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS} NO-LOCK NO-ERROR.

   IF AVAIL ActionLog THEN
      ASSIGN lcLogFile = lcDoneDir + ActionLog.KeyValue + ".LOG".

   SendMail(lcLogFile,"").

   ASSIGN lcLogFile = "/apps/yoigo/tms_support/billing/log/stc_barrings_" +
                     STRING(YEAR(TODAY) * 100 + MONTH(TODAY)) + ".txt".
   RUN check_stc_barrings.p.

   SendMail(lcLogFile,"").

END.   

