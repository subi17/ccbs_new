{commpaa.i}
katun = "cron".
gcBrand = "1".
{date.i}
{cparam2.i}
{timestamp.i}
{tmsconst.i}
{msreqfunc.i}

DEF VAR ldeTodayStamp AS DECIMAL NO-UNDO.
DEF VAR ldtMNPDate   AS DATE    NO-UNDO.
DEF VAR ldtSTCDate   AS DATE    NO-UNDO.
DEF VAR lcLogDir     AS CHAR    NO-UNDO.
DEF VAR lcToday      AS CHAR    NO-UNDO. 

DEFINE BUFFER bMNPRequest FOR MsRequest.

DEFINE STREAM strout.

ASSIGN 
   lcToday    = STRING(YEAR(TODAY),"9999") +
                STRING(MONTH(TODAY),"99")  +
                STRING(DAY(TODAY),"99")
   ldeTodayStamp = fMake2DT(TODAY + 1,7200)
   lcLogDir     = fCParam("CancelSTC","LogDir")
   lcLogDir     = lcLogDir + "CancelStcReq_MNP_" + lcToday + ".log".

OUTPUT STREAM strout TO VALUE(lcLogDir).

PUT STREAM strout UNFORMATTED 
   "MsSeq"       ";"
   "MSISDN"      ";" 
   "STC DATE"    ";"
   "MNP DATE"    ";"
   "OLD SubType" ";"
   "New SubType" ";"
   "Status" SKIP.

FOR EACH bMNPRequest NO-LOCK WHERE
         bMNPRequest.Brand      = gcBrand                                 AND
         bMNPRequest.ReqType    = {&REQTYPE_SUBSCRIPTION_TERMINATION}     AND
         bMNPRequest.ReqStatus  = 0                                       AND
         bMNPRequest.Reqcparam3 = STRING({&SUBSCRIPTION_TERM_REASON_MNP}) AND
         bMNPRequest.ActStamp   = ldeTodayStamp :

   fTS2Date(bMNPRequest.ActStamp,ldtMNPDate).

   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.Brand     = gcBrand                             AND
              MsRequest.MsSeq     = bMNPRequest.MsSeq                   AND
              MsRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
              MsRequest.ReqStatus = 8                                   AND
              MsRequest.ActStamp  < bMNPRequest.ActStamp No-error.

   IF NOT AVAIL MsRequest THEN NEXT.

   FIND FIRST MobSub NO-LOCK where 
              MobSub.MsSeq eq MsRequest.MsSeq NO-ERROR.

   fTS2Date(MsRequest.ActStamp,ldtSTCDate).

   IF ldtMNPDate NE ldtSTCDate THEN NEXT.

   fReqStatus(4,"Manual status change from Ongoing staus->4 (YTS-8304)"). 

   PUT STREAM strout UNFORMATTED
      MsRequest.MsSeq ";"
      MsRequest.CLI   ";"
      ldtSTCDate      ";"
      ldtMNPDate      ";"
      (if MobSub.tariffbundle > "" then MobSub.tariffbundle
       else MobSub.clitype) ";"
      (if MsRequest.reqcparam5 > "" then MsRequest.reqcparam5 
       else MsRequest.reqcparam2) ";"
      "Status Changed" SKIP.

END.

OUTPUT STREAM strout CLOSE.

