/* ----------------------------------------------------------------------
  MODULE .......: sva_bob.p
  TASK .........: BOB / Cron tool for handling SVA status changes
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 06/2017
  Version ......: yoigo
---------------------------------------------------------------------- */
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/q25functions.i}
{Func/ftransdir.i}
{Syst/eventlog.i}
{Func/barrfunc.i}
{Func/msreqfunc.i}
{Func/profunc_request.i}

DEF VAR lcInDir AS CHAR NO-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.
DEF VAR lcInputFile AS CHAR NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcErrDir AS CHAR NO-UNDO.
DEF VAR lcOutDir AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC NO-UNDO.
DEF VAR lcErrorLog AS CHAR NO-UNDO.
DEF VAR lcLog AS CHAR NO-UNDO.
DEF VAR liReqType AS INT NO-UNDO.
DEF VAR liReqStatus AS INT NO-UNDO.
DEF VAR lcSpoolDir AS CHAR NO-UNDO.
DEF VAR lcInProcessedDir AS CHAR NO-UNDO.
DEF VAR llReqStatus AS LOGICAL NO-UNDO.
DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.
DEF STREAM sErr.

lcLogDir = "/tmp/".
lcErrDir =  "/tmp/".
lcInDir =  "/tmp/".
lcOutDir =  "/tmp/".


lcLogDir = fCParam("YPRO", "YPRO_SVA_log_base_dir").
lcErrDir = fCParam("YPRO", "YPRO_SVA_err_base_dir").
lcInDir = fCParam("YPRO", "YPRO_SVA_in_base_dir") + "incoming/".
lcOutDir = fCParam("YPRO", "YPRO_SVA_out_base_dir").
lcSpoolDir = fCParam("YPRO", "YPRO_SVA_in_base_dir") + "spool/".
lcInProcessedDir = fCParam("YPRO", "YPRO_SVA_in_base_dir") + "processed/".



/*File handling logic starts*/
/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcInDir ).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.

   lcInputFile = lcInDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:

      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.
      if lcfilename begins "sva" then message lcfilename VIEW-AS ALERT-BOX.

      /*Accept only activation files*/
      IF NOT lcFileName MATCHES "sva_*.txt" THEN NEXT.
      /*IF lcFileName MATCHES "*log*" THEN NEXT.*/

      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.
   
   lcErrorLog = lcSpoolDir + lcFileName + "_ERRORS.LOG".
   lcLog = lcSpoolDir + lcFileName + ".LOG".

   IF SESSION:BATCH THEN fBatchLog("START", lcInputFile).

   OUTPUT STREAM sLog TO VALUE(lcLog) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.
   OUTPUT STREAM sLog CLOSE.              

   RUN pReadFileData.

   fMove2TransDir(lcErrorLog, "", lcErrDir).
   fMove2TransDir(lcLog, "", lcLogDir).
   fMove2TransDir(lcInputFile, "", lcInProcessedDir).
   IF SESSION:BATCH AND lcInputFile NE "" THEN
      fBatchLog("FINISH", lcInputFile).
END.


INPUT STREAM sin CLOSE.

PROCEDURE pReadFileData:

   DEF VAR lcLine AS CHAR NO-UNDO.
   DEF VAR lcErrorCode AS CHAR NO-UNDO.
   DEF VAR liCustNum AS INT NO-UNDO.
   DEF VAR lcFixedNum AS CHAR NO-UNDO.
   DEF VAR liLineNum AS INT NO-UNDO.
   DEF VAR lcServiceCode AS CHAR NO-UNDO.
   DEF VAR lcSetStatus AS CHAR NO-UNDO.
   DEF VAR lcFaxToEmailNumber AS CHAR NO-UNDO.
   DEF VAR lcErrText AS CHAR NO-UNDO.
   DEF VAR lcEmailErr AS CHAR NO-UNDO.

   FILE_LINE:
   REPEAT TRANS:

      IMPORT STREAM sin UNFORMATTED lcLine.
      IF TRIM(lcLine) EQ "" THEN NEXT.
      liLineNum = liLineNum + 1.

      IF NOT SESSION:BATCH AND liLineNum MOD 10 = 0 THEN DO:
         disp "Reading data.. " lcFilename liLineNum with frame a.
         pause 0.
      END.

      assign
         lcErrText = ""
         lcFixedNum = entry(1,lcline,";")
         lcServiceCode = entry(2,lcline,";")
         lcSetStatus = entry(3,lcline,";").
         IF NUM-ENTRIES(lcline) > 3 THEN
            lcFaxtoEmailNumber = entry(4,lcLine,";").
         /*lcFaxtoEmailEmail = entry(5,lcLine,";")*/
/*      message lcfixednum VIEW-AS ALERT-BOX.
      message lcservicecode VIEW-AS ALERT-BOX.
      message lcsetstatus VIEW-AS ALERT-BOX.
      message lcfaxtoemailnumber VIEW-AS ALERT-BOX.*/
      
      FIND FIRST MobSub NO-LOCK WHERE 
                 MobSub.brand EQ Syst.Var:gcBrand AND
                 MobSub.Fixednumber EQ lcFixedNum AND
                 Mobsub.Paytype EQ FALSE NO-ERROR.
      IF NOT AVAIL Mobsub THEN DO:
         lcErrText = lcLine + ";ERROR: Subscription not found".
         OUTPUT STREAM sErr TO VALUE(lcErrorLog) append.
         PUT STREAM sErr UNFORMATTED lcErrText SKIP.
         OUTPUT STREAM sErr CLOSE.
         NEXT. /*next line*/
      END.
     
      /*Allowed state transitions:
      pending deactivation -> inactive
      pending activation -> active.*/
      IF lcSetStatus EQ "Active" THEN 
         liReqType = {&REQTYPE_CONTRACT_ACTIVATION}.
      ELSE IF lcSetStatus EQ "Inactive" THEN
         liReqType = {&REQTYPE_CONTRACT_TERMINATION}.
      ELSE IF lcSetStatus EQ "cancel_active" THEN
         liReqType = {&REQTYPE_CONTRACT_ACTIVATION}.
      ELSE IF lcSetStatus EQ "cancel_inactive" THEN
               liReqType = {&REQTYPE_CONTRACT_TERMINATION}.
      ELSE DO:
         lcErrText = lcLine + "Incorrect action code " + lcSetStatus.
         OUTPUT STREAM sErr TO VALUE(lcErrorLog) append.
         PUT STREAM sErr UNFORMATTED lcErrText SKIP.
         OUTPUT STREAM sErr CLOSE.     
         NEXT.
      END.
      liReqStatus = {&REQUEST_STATUS_CONFIRMATION_PENDING}.

      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq      EQ MobSub.MsSeq     AND
                 MsRequest.ReqType    EQ liReqType        AND
                 MsRequest.ReqStatus  EQ liReqStatus      AND
                 MsRequest.ReqCParam3 EQ lcServiceCode    NO-LOCK NO-ERROR.
      IF NOT AVAIL MsRequest THEN 
      DO:
         lcErrText = lcLine + ";" + "Action not allowed: Requested " + lcSetStatus.
         OUTPUT STREAM sErr TO VALUE(lcErrorLog) append.
         PUT STREAM sErr UNFORMATTED lcErrText SKIP.
         OUTPUT STREAM sErr CLOSE. 
         NEXT.
      END.
      /*Make actual status change for the request and create / remove fee*/

      IF lcSetStatus begins "cancel" THEN 
      DO:
         lcEmailErr = fSendEmailByRequest(MsRequest.MsRequest, "SVA_" + MsRequest.ReqCparam3).
         IF lcEmailErr NE "" THEN 
            MESSAGE "ERROR: " + lcEmailErr VIEW-AS ALERT-BOX.
         llReqStatus =  fReqStatus(4, "SVA operation cancelled ").           
      END.
      ELSE DO:
         llReqStatus = fReqStatus(6, "Execute SVA operation ").
         /* in case of incativation, inactivate its activation request */
         IF lcSetStatus EQ "Inactive" THEN 
         DO:
           FIND FIRST MsRequest WHERE
                      MsRequest.MsSeq      EQ MobSub.MsSeq                   AND
                      MsRequest.ReqType    EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
                      MsRequest.ReqStatus  EQ {&REQUEST_STATUS_DONE}         AND
                      MsRequest.ReqCParam3 EQ lcServiceCode                  NO-LOCK NO-ERROR.
           IF AVAIL MsRequest THEN
              llReqStatus = fReqStatus(9, "SVA operation, inactivated").
         END.
      END.

      IF llReqStatus = FALSE THEN DO: /* another process is handling same request */
         lcErrText = lcLine + ";" + "Changing status of request failed: Requested " + lcSetStatus.
         OUTPUT STREAM sErr TO VALUE(lcErrorLog) append.
         PUT STREAM sErr UNFORMATTED lcErrText SKIP.
         OUTPUT STREAM sErr CLOSE. 
         NEXT.
      END.   

      OUTPUT STREAM sLog TO VALUE(lcLog) append.
      PUT STREAM sLog UNFORMATTED "Operation done" SKIP.
      OUTPUT STREAM sLog CLOSE.

   END.
END PROCEDURE.
