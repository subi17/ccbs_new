/* ----------------------------------------------------------------------
  MODULE .......: bob_duplicateInvoice_for_last_invoices.p
  TASK .........: YDR-2348
                  To CREATE duplicate invoice request for inovice users 
                  with last invoices as electric/SMS.   
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: 21.11.16
  Version ......: Yoigo
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".

{tmsconst.i}
{fcreatereq.i}
{timestamp.i}
{cparam2.i}
{ftransdir.i}

DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum AS INT
   FIELD EndTS   AS DEC
   INDEX CustNum CustNum.

DEF VAR iCount              AS INT  NO-UNDO.
DEF VAR liOldDelType        AS INT  NO-UNDO.
DEF VAR liNewDelType        AS INT  NO-UNDO.
DEF VAR ldStartTime         AS DEC  NO-UNDO.
DEF VAR ldEndTime           AS DEC  NO-UNDO.
DEF VAR ldaStartDate        AS DATE NO-UNDO.
DEF VAR ldaEndDate          AS DATE NO-UNDO.
DEF VAR ldaActDate          AS DATE NO-UNDO.
DEF VAR liActTime           AS INT  NO-UNDO.
DEF VAR lcOutDir            AS CHAR NO-UNDO. 
DEF VAR lcSpoolDir          AS CHAR NO-UNDO. 
DEF VAR lcCollectionLogFile AS CHAR NO-UNDO. 
DEF VAR lcRequestLogFile    AS CHAR NO-UNDO. 
DEF VAR lcDateValue         AS CHAR NO-UNDO. 
DEF VAR ldeStartTime        AS DEC  NO-UNDO. 

DEF STREAM sCollectLog.
DEF STREAM sRequestLog.

ASSIGN
   lcSpoolDir          = fCParam("LastInvoices","SpoolDir")
   lcOutDir            = fCParam("LastInvoices","OutDir")
   ldaStartDate        = DATE(MONTH(TODAY), 1, YEAR(TODAY))
   ldaEndDate          = DATE(MONTH(TODAY), 3, YEAR(TODAY))
   ldStartTime         = fMake2Dt(ldaStartDate,0)
   ldEndTime           = fMake2Dt(ldaEndDate  ,86399)
   lcDateValue         = STRING(YEAR(TODAY),"9999") +
                         STRING(MONTH(TODAY),"99")  +
                         STRING(DAY(TODAY),"99") 
   lcCollectionLogFile = lcSpoolDir + "CollectionLog_" + lcDateValue + ".log"
   lcRequestLogFile    = lcSpoolDir + "RequestLog_" + lcDateValue + ".log".

OUTPUT STREAM sCollectLog TO VALUE(lcCollectionLogFile) APPEND.

PUT STREAM sCollectLog UNFORMATTED
   "CustomerNumber;EventDate;EventTime" SKIP.

FOR EACH EventLog NO-LOCK WHERE
         EventLog.UserCode  = "TermSub"                  AND
         EventLog.TableName = "Customer"                 AND
         EventLog.Action    = "Modify"                   AND
         EventLog.EventDate >= ldaStartDate              AND
         EventLog.EventDate <= ldaEndDate                AND
         LOOKUP("DelType",EventLog.Datavalues,CHR(255)) > 0:

   IF CAN-FIND(FIRST MobSub NO-LOCK WHERE
                     MobSub.Brand   = "1"               AND
                     MobSub.CustNum = INT(EventLog.Key) AND 
                     MobSub.PayType EQ FALSE)           THEN NEXT.

   ASSIGN
      liOldDelType = INT(ENTRY(2,EventLog.Datavalues,CHR(255)))
      liNewDelType = INT(ENTRY(3,EventLog.Datavalues,CHR(255))).
      
   IF (liOldDelType = 2 OR liOldDelType = 4) AND
      liNewDelType = 1 THEN
   DO:

      IF NOT CAN-FIND(FIRST ttCust WHERE
                            ttCust.CustNum = INT(EventLog.Key)) THEN
      DO:
         CREATE ttCust.
         ASSIGN ttCust.CustNum = INT(EventLog.Key) 
                ttCust.EndTS   = fHMS2TS(EventLog.EventDate, 
                                         EventLog.EventTime).
                                         
         PUT STREAM sCollectLog UNFORMATTED
            EventLog.Key                            ";"
            STRING(EventLog.EventDate,"99.99.9999") ";"
            EventLog.EventTime                      SKIP.

      END.
   END.

END.

FOR FIRST FuncRunQueue NO-LOCK WHERE 
          FuncRunQueue.FRQueueID = 3,  
    FIRST FuncRunQSchedule NO-LOCK WHERE 
          FuncRunQSchedule.FRQueueID = FuncRunQueue.FRQueueID AND 
          FuncRunQSchedule.StartTS  >= ldStartTime, 
    FIRST FuncRunExec NO-LOCK WHERE
          FuncRunExec.FRQScheduleID EQ FuncRunQSchedule.FRQScheduleID AND
          FuncRunExec.FRConfigID    EQ 5:             

    ldeStartTime = FuncRunExec.StartTS.

END.

IF ldeStartTime <= 0 THEN DO:
   PUT STREAM sCollectLog UNFORMATTED 
      "FuncRunExecution StartTime is not available".
   LEAVE.   
END.

OUTPUT STREAM sCollectLog CLOSE.

OUTPUT STREAM sRequestLog To VALUE(lcRequestLogFile) APPEND.

PUT STREAM sRequestLog UNFORMATTED 
   "CustomerNumber"    ";"
   "ExternalInvoiceID" ";"
   "InvoiceDate"       ";"
   "ChangeStamp"       ";"
   "CustomerEndStamp"  ";"
   "PrintState"        ";"
   "DeliveryType"      SKIP.

IF ldeStartTime > 0 THEN DO:

   FOR EACH ttCust:
      
      IF ttCust.EndTS < ldeStartTime THEN NEXT.
    
      FIND FIRST Invoice NO-LOCK WHERE
                 Invoice.Brand     = "1"            AND
                 Invoice.CustNum   = ttCust.CustNum AND
                 Invoice.InvDate   = ldaStartDate   AND
                 Invoice.InvType   = 1              AND
                 Invoice.ChgStamp <= ttCust.EndTS   NO-ERROR.
      IF AVAIL Invoice THEN DO:
         IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                           MsRequest.Brand      = "1"                                        AND
                           MsRequest.ReqType    = {&REQTYPE_DUPLICATE_INVOICE}               AND
                           MsRequest.CustNum    = ttCust.CustNum                             AND
                           MsRequest.ReqIParam1 = Invoice.InvNum                             AND
                           LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0) THEN NEXT.

         fCreateRequest({&REQTYPE_DUPLICATE_INVOICE},
                        fMake2Dt(11/10/2016,0),
                        katun,
                        FALSE,      /* fees     */
                        FALSE).     /* send sms */

         ASSIGN bCreaReq.CustNum    = Invoice.Custnum
                bCreaReq.ReqIParam1 = Invoice.InvNum
                bCreaReq.ReqCParam1 = Invoice.ExtInvId 
                bCreaReq.ReqSource  = {&REQUEST_SOURCE_SCRIPT}
                bCreaReq.Memo       = "YDR-2050".

         PUT STREAM sRequestLog UNFORMATTED
             Invoice.CustNum    ";"
             Invoice.ExtInvId   ";"
             Invoice.InvDate    ";"
             Invoice.ChgStamp   ";"
             ttCust.EndTs       ";"
             Invoice.PrintState ";"
             Invoice.DelType    SKIP.  
      END.
   END.

END.

OUTPUT STREAM sRequestLog CLOSE.

fMove2TransDir(lcCollectionLogFile, "", lcOutDir).
fMove2TransDir(lcRequestLogFile, "", lcOutDir).
