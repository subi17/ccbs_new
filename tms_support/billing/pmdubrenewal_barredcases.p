{commpaa.i}
{tmsconst.i}
{cparam2.i}
{email.i}
katun = "Qvantel".
gcBrand  = "1".

DEFINE VARIABLE liCnt         AS INTEGER   NO-UNDO.
DEFINE VARIABLE llDone        AS LOGICAl   NO-UNDO INIT TRUE.
DEFINE VARIABLE ldeStartStamp AS DECIMAL   NO-UNDO FORMAT "99999999.99999".
DEFINE VARIABLE ldeEndStamp   AS DECIMAL   NO-UNDO FORMAT "99999999.99999".
DEFINE VARIABLE lcConfDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDoneDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile     AS CHARACTER NO-UNDO.

DEFINE BUFFER bfCliType FOR CliType.

ASSIGN lcConfDir      = fCParamC("RepConfDir")
       ldeStartStamp  = YEAR(TODAY) * 10000 +
                        MONTH(TODAY) * 100 +
                        DAY(TODAY)
       ldeEndStamp    = YEAR(TODAY) * 10000 +
                        MONTH(TODAY) * 100 +
                        DAY(TODAY + 1).
rep-blk:
REPEAT:
   ASSIGN liCnt  = 0
          llDone = TRUE.
   req-blk:       
   FOR EACH MSRequest WHERE
            MSRequest.Brand     = gcBrand  AND
            MSRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
            MSRequest.ActStamp  >= ldeStartStamp AND 
            MSRequest.ActStamp  <  ldeEndStamp NO-LOCK:

      FIND FIRST CliType WHERE 
                 Clitype.Brand     = gcBrand AND 
                 CliType.CliType   = MSRequest.ReqCParam1 NO-LOCK NO-ERROR.
      FIND FIRST bfCliType WHERE 
                 bfCliType.Brand   = gcBrand AND  
                 bfCliType.CliType = MSRequest.ReqCParam2 NO-LOCK NO-ERROR.

      IF ((CliType.PayType = 1 AND bfCliType.PayType = 1) OR
          (CliType.PayType = 1 AND bfCliType.PayType = 2)) AND 
          MSRequest.ReqStatus <> {&REQUEST_STATUS_DONE} AND
          MSRequest.ReqStatus <> {&REQUEST_STATUS_REJECTED} AND 
          MSRequest.ReqStatus <> {&REQUEST_STATUS_CANCELLED} THEN
         ASSIGN liCnt = liCnt + 1.

      ELSE IF (CliType.PayType = 2 AND bfCliType.PayType = 1) OR
              (CliType.PayType = 2 AND bfCliType.PayType = 2) THEN
      DO:
         IF (MSRequest.ReqStatus = {&REQUEST_STATUS_REJECTED} OR
             MSRequest.ReqStatus = {&REQUEST_STATUS_CANCELLED} ) THEN
            NEXT req-blk.    
         ELSE IF (MSRequest.ReqStatus <> {&REQUEST_STATUS_DONE}) THEN
         DO: 
            /* If this variable is already false then no need to assign */
            IF llDone THEN
               ASSIGN llDone = FALSE.
         END.   
      END.                
   END.
   IF liCnt <= 5 AND llDone THEN 
      LEAVE rep-blk.   
END.

GetRecipients(lcConfDir + "pmdubrenewal_barredcases.email").

ASSIGN
   lcDoneDir  = fCParam("PrepaidBundle","InProcDir").

RUN pmdub_batch_in.p.
   
FIND FIRST ActionLog USE-INDEX ActionID WHERE
           ActionLog.Brand        = gcBrand AND    
           ActionLog.ActionID     = "PMDUB_IN" AND
           ActionLog.TableName    = "Cron" AND
           ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS} 
           NO-LOCK NO-ERROR.

IF AVAIL ActionLog THEN
   ASSIGN lcLogFile = lcDoneDir + ActionLog.KeyValue + ".LOG".

SendMail(lcLogFile,"").

ASSIGN lcLogFile = "/apps/yoigo/tms_support/billing/log/stc_barrings_" +
                   STRING(YEAR(TODAY) * 100 + MONTH(TODAY)) + ".txt".
RUN check_stc_barrings.p.

SendMail(lcLogFile,"").
   

