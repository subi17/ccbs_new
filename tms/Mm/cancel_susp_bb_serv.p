/* ----------------------------------------------------------------------
  module .......: Mm/cancel_susp_bb_serv.p
  task .........: Cancel the BB service that has been in suspend mode
                  more than 90 days
  application ..: tms
  author .......: vikas
  created ......: 12.07.11
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}

DEF VAR liConfDays       AS INT  NO-UNDO.
DEF VAR liReq            AS INT  NO-UNDO.
DEF VAR lcError          AS CHAR NO-UNDO.
DEF VAR lcLogDir         AS CHAR NO-UNDO.
DEF VAR lcLogFile        AS CHAR NO-UNDO.
DEF VAR ldMsEndDate      AS DATE NO-UNDO.
DEF VAR liMsEndTime      AS INT  NO-UNDO.

DEF STREAM sout.

ASSIGN liConfDays = INT(fCParam("BlackBerry","BBCancelDays"))
       lcLogDir   = fCParam("BlackBerry","LogDir").

IF lcLogDir = "" OR lcLogDir = ? THEN lcLogDir = "/scratch/log/bb_cancel/".

IF liConfDays = 0 OR liConfDays = ? THEN liConfDays = 90.

lcLogFile = lcLogDir + "cancel_susp_bb_serv_" +
            STRING(YEAR(TODAY))       +
            STRING(MONTH(TODAY),"99") +
            STRING(DAY(TODAY),"99") + ".txt".

OUTPUT STREAM sout TO VALUE(lcLogFile).

PUT STREAM sout UNFORMATTED
                "MSISDN"   CHR(9)
                "Service"  CHR(9)
                "Status"   CHR(9)
                "ActStamp" CHR(9)
                "Remark"   SKIP.


FOR EACH  MobSub WHERE
          MobSub.Brand = gcBrand AND
          MobSub.ActivationDate < (TODAY - liConfDays) NO-LOCK:
    
    ASSIGN liReq       = 0
           lcError     = ""
           ldMsEndDate = ?
           liMsEndTime = 0.

    FOR EACH MsRequest WHERE
             MsRequest.MsSeq      = MobSub.MsSeq              AND
             MsRequest.ReqType    = {&REQTYPE_SERVICE_CHANGE} AND
             MsRequest.ReqCparam1 = "BB"                      AND
             MsRequest.ReqStatus  = {&REQUEST_STATUS_DONE} NO-LOCK
             BY MsRequest.UpdateStamp DESC:
        IF MsRequest.ReqIParam1 = 2 THEN DO:
           fSplitTS(MsRequest.ActStamp, ldMsEndDate, liMsEndTime).
           IF liConfDays >= (TODAY - ldMsEndDate) THEN LEAVE.

           liReq = fServiceRequest(INPUT MobSub.MsSeq,
                                   INPUT "BB",
                                   INPUT 0,                 /* deactivate  */
                                   INPUT "",
                                   INPUT fMakeTS(),
                                   INPUT "",                /* SalesMan */
                                   INPUT FALSE,             /* Set fees */
                                   INPUT FALSE,             /* SMS      */
                                   INPUT "",
                                   INPUT {&REQUEST_SOURCE_SCRIPT},
                                   INPUT 0,
                                   INPUT FALSE,
                                   OUTPUT lcError).

           PUT STREAM sout UNFORMATTED
                           MobSub.CLI           CHR(9)
                           MsRequest.ReqCparam1 CHR(9)
                           MsRequest.ReqIparam1 CHR(9)
                           MsRequest.ActStamp   CHR(9).
           IF liReq > 0 THEN
              PUT STREAM sout UNFORMATTED
                              "Request is created to deactivate the " +
                              "BB service" SKIP.
           ELSE
              PUT STREAM sout UNFORMATTED
                              "ERROR:Request is not created to " + "
                              deactivate the BB service: " lcError SKIP.
        END. /* IF MsRequest.ReqIParam1 = 2 THEN DO: */
        LEAVE.
    END. /* FOR EACH MsRequest WHERE */
END. /* FOR EACH  MobSub WHERE */

OUTPUT STREAM sout CLOSE.

