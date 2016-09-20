{commpaa.i}
{tmsconst.i}
{cparam2.i}
{email.i}
katun = "Qvantel".
gcBrand  = "1".

DEFINE VARIABLE liCnt         AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldeStartStamp AS DECIMAL   NO-UNDO FORMAT "99999999.99999".
DEFINE VARIABLE ldeEndStamp   AS DECIMAL   NO-UNDO FORMAT "99999999.99999".
DEFINE VARIABLE lcConfDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDoneDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStatusList  AS CHARACTER NO-UNDO INITIAL "2,3,4,9,99". 

ASSIGN lcConfDir      = fCParamC("RepConfDir")
       ldeStartStamp  = YEAR(TODAY) * 10000 +
                        MONTH(TODAY) * 100 +
                        DAY(TODAY)
       ldeEndStamp    = YEAR(TODAY) * 10000 +
                        MONTH(TODAY) * 100 +
                        DAY(TODAY + 1).
rep-blk:
REPEAT:
   /* Prepaid to prepaid or prepaid to postpaid STC request with ongoing status */
   IF CAN-FIND(FIRST MSRequest WHERE
                     MSRequest.Brand     = gcBrand  AND
                     MSRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                     LOOKUP(STRING(MSRequest.ReqStatus),lcStatusList) = 0 AND
                     MSRequest.ActStamp  >= ldeStartStamp AND
                     MSRequest.ActStamp  <  ldeEndStamp AND 
                    (MsRequest.ReqCParam1 BEGINS "TARJ" AND
                     MsRequest.ReqCParam2 BEGINS "TARJ") OR
                    (MsRequest.ReqCParam1 BEGINS "TARJ"  AND
                     MsRequest.ReqCParam2 BEGINS "CONT") NO-LOCK
                     ) THEN
   DO:
      PAUSE 5.
      NEXT rep-blk.
   END.
   /* Postpaid to postpaid or postpaid to prepaid STC ongoing request */

   FOR EACH MSRequest WHERE
            MSRequest.Brand     = gcBrand  AND
            MSRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
            LOOKUP(STRING(MSRequest.ReqStatus),lcStatusList) = 0 AND
            MSRequest.ActStamp  >= ldeStartStamp AND 
            MSRequest.ActStamp  <  ldeEndStamp AND
           (MsRequest.ReqCParam1 BEGINS "CONT" AND
            MsRequest.ReqCParam2 BEGINS "CONT") OR
           (MsRequest.ReqCParam1 BEGINS "CONT" AND
            MsRequest.ReqCParam2 BEGINS "TARJ") NO-LOCK:

      ASSIGN liCnt = liCnt + 1.
      IF liCnt > 5 THEN
      DO:
         ASSIGN liCnt = 0.
         PAUSE 5.
         NEXT rep-blk.
      END.
   END.
   IF liCnt <= 5 THEN 
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

