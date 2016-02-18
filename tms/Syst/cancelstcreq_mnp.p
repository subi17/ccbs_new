{commpaa.i}
katun = "cron".
gcBrand = "1".
{date.i}
{cparam2.i}
{timestamp.i}
{tmsconst.i}
{msreqfunc.i}

DEF VAR lcTodayStamp AS DECIMAL NO-UNDO.
DEF VAR ldtMNPDate   AS DATE    NO-UNDO.
DEF VAR ldtSTCDate   AS DATE    NO-UNDO.
DEF VAR llgMNPDate   AS LOG     NO-UNDO.
DEF VAR llgSTCDate   AS LOG     NO-UNDO.
DEF VAR lcLogDir     AS CHAR    NO-UNDO.
DEF VAR lcToday      AS CHAR    NO-UNDO. 

DEFINE BUFFER bMNPRequest FOR MsRequest.

DEFINE STREAM strout.

ASSIGN 
   lcToday    = STRING(YEAR(TODAY),"9999") +
                STRING(MONTH(TODAY),"99")  +
                STRING(DAY(TODAY),"99")
   lcTodayStamp = fMake2DT(TODAY,0)
   lcLogDir     = fCParam("CancelSTC","LogDir")
   lcLogDir     = lcLogDir + "CancelStcReq_MNP_" + lcToday + ".log".

OUTPUT STREAM strout TO VALUE(lcLogDir).

PUT STREAM strout UNFORMATTED 
   "MsSeq"    ";"
   "MSISDN"   ";" 
   "STC DATE" ";"
   "MNP DATE" ";"
   "Status" SKIP.

FOR EACH bMNPRequest NO-LOCK WHERE
         bMNPRequest.Brand      = gcBrand                                 AND
         bMNPRequest.ReqType    = {&REQTYPE_SUBSCRIPTION_TERMINATION}     AND
         bMNPRequest.ReqStatus  = INT({&REQ_ONGOING_STATUSES})            AND
         bMNPRequest.Reqcparam3 = STRING({&SUBSCRIPTION_TERM_REASON_MNP}) AND
         bMNPRequest.ActStamp  >= lcTodayStamp :

   llgMNPDate = fTS2Date(bMNPRequest.ActStamp,ldtMNPDate).

   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.Brand     = gcBrand                             AND
              MsRequest.MsSeq     = bMNPRequest.MsSeq                   AND
              MsRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
              MsRequest.ReqStatus = INT({&REQ_ONGOING_STATUSES})        AND
              MsRequest.ActStamp >= lcTodayStamp                        AND
              MsRequest.ActStamp <= bMNPRequest.ActStamp                NO-ERROR.

   IF NOT AVAIL MsRequest THEN NEXT.

   llgSTCDate = fTS2Date(MsRequest.ActStamp,ldtSTCDate).

   IF ldtMNPDate NE ldtSTCDate THEN NEXT.

   fReqStatus(4,"Manual status change from Ongoing staus->4 (YTS-8304)").

   PUT STREAM strout UNFORMATTED
      MsRequest.MsSeq ";"
      MsRequest.CLI   ";"
      ldtSTCDate      ";"
      ldtMNPDate      ";"
      "Status Changed" SKIP.

END.

OUTPUT STREAM strout CLOSE.

