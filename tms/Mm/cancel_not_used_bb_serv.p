/* ----------------------------------------------------------------------
  module .......: Mm/cancel_not_used_bb_serv.p
  task .........: Cancel the BB service if subscription is specified in
                  the input file provided by DWH
  application ..: tms
  author .......: Vikas
  created ......: 08.11.12
  version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{timestamp.i}
{cparam2.i}
{tmsconst.i}
{fmakemsreq.i}
{ftransdir.i}

DEF VAR liReq            AS INT  NO-UNDO.
DEF VAR lcError          AS CHAR NO-UNDO.
DEF VAR lcLogDir         AS CHAR NO-UNDO.
DEF VAR lcIncomingDir    AS CHAR NO-UNDO.
DEF VAR lcDoneDir        AS CHAR NO-UNDO.
DEF VAR lcLogFile        AS CHAR NO-UNDO.
DEF VAR lcInputFileName  AS CHAR NO-UNDO.
DEF VAR lcLine           AS CHAR NO-UNDO.
DEF VAR liMsSeq          AS INT  NO-UNDO.

DEFINE TEMP-TABLE ttSubscription
   FIELD MsSeq    AS INT
   INDEX MsSeq IS PRIMARY MsSeq.

DEF STREAM sout.
DEF STREAM sFile.
DEF STREAM sInputFile.

ASSIGN lcLogDir      = fCParam("BlackBerry","LogDir")
       lcIncomingDir = fCParam("BlackBerry","InDir")
       lcDoneDir     = fCParam("BlackBerry","InProcDir").

IF NOT lcLogDir > "" OR NOT lcIncomingDir > "" OR
   NOT lcDoneDir > "" THEN RETURN.

INPUT STREAM sFile THROUGH VALUE("ls -1 " + lcIncomingDir + "/*.txt").

FILE_LOOP:
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcInputFileName.

   IF SEARCH(lcInputFileName) NE ? THEN DO:

      INPUT STREAM sInputFile FROM VALUE(lcInputFileName).

      REPEAT:
         IMPORT STREAM sInputFile UNFORMATTED lcLine.

          IF lcLine = "" OR lcLine = ? THEN NEXT.
          liMsSeq = INT(lcLine) NO-ERROR.
          IF liMsSeq = 0 OR liMsSeq = ? THEN NEXT.

          IF CAN-FIND (FIRST ttSubscription WHERE
                             ttSubscription.MsSeq = liMsSeq) THEN NEXT.

         CREATE ttSubscription.
                ttSubscription.MsSeq = liMsSeq.
      END. /* REPEAT: */

      INPUT STREAM sInputFile CLOSE.
   END. /* IF SEARCH(lcInputFileName) NE ? THEN DO: */

   /* Moved input file to processed directory */
   fTransDir(lcInputFileName,
             "",
             lcDoneDir).
END. /* REPEAT: */

lcLogFile = lcLogDir + "cancel_not_used_bb_serv_" +
            STRING(YEAR(TODAY))       +
            STRING(MONTH(TODAY),"99") +
            STRING(DAY(TODAY),"99") + ".txt".

OUTPUT STREAM sout TO VALUE(lcLogFile).

PUT STREAM sout UNFORMATTED
                "MsSeq"    CHR(9)
                "MSISDN"   CHR(9)
                "Service"  CHR(9)
                "Status"   CHR(9)
                "ActStamp" CHR(9)
                "Remark"   SKIP.

FOR EACH ttSubscription NO-LOCK,
    FIRST MobSub WHERE
          MobSub.MsSeq = ttSubscription.MsSeq NO-LOCK:
    
    ASSIGN liReq   = 0
           lcError = "".

    FOR EACH MsRequest WHERE
             MsRequest.MsSeq      = MobSub.MsSeq              AND
             MsRequest.ReqType    = {&REQTYPE_SERVICE_CHANGE} AND
             MsRequest.ReqCparam1 = "BB"                      AND
             MsRequest.ReqStatus  = {&REQUEST_STATUS_DONE} NO-LOCK
             BY MsRequest.UpdateStamp DESC:

        IF MsRequest.ReqIParam1 = 1 THEN DO:
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
                           MobSub.MsSeq         CHR(9)
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
        END. /* IF MsRequest.ReqIParam1 = 1 THEN DO: */
        LEAVE.
    END. /* FOR EACH MsRequest WHERE */
END. /* FOR EACH  MobSub WHERE */

OUTPUT STREAM sout CLOSE.
