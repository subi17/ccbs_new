/* ----------------------------------------------------------------------
  MODULE .......: sva_bob.p
  TASK .........: BOB / Cron tool for handling SVA status changes
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 06/2017
  Version ......: yoigo
---------------------------------------------------------------------- */
{Syst/commpaa.i}
gcbrand = "1".
{Syst/tmsconst.i}
{Func/q25functions.i}
{Func/ftransdir.i}
{Syst/eventlog.i}
{Func/fmakemsreq.i}
{Func/barrfunc.i}

DEF VAR lcProcessedFile AS CHAR NO-UNDO.
DEF VAR lcInDir AS CHAR NO-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.
DEF VAR lcInputFile AS CHAR NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcErrDir AS CHAR NO-UNDO.
DEF VAR lcOutDir AS CHAR NO-UNDO.
DEF VAR lcLogOutDir AS CHAR NO-UNDO.
DEF VAR lcTableName AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC NO-UNDO.
DEF VAR lcErrorLog AS CHAR NO-UNDO.
DEF VAR liReqType AS INT NO-UNDO.
DEF VAR liReqStatus AS INT NO-UNDO.

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.


lcLogDir = fCParam("YPRO", "YPRO_SVA_log_base_dir").
lcErrDir = fCParam("YPRO", "YPRO_SVA_err_base_dir").
lcInDir = fCParam("YPRO", "YPRO_SVA_in_base_dir").
lcOutDir = fCParam("YPRO", "YPRO_SVA_out_base_dir").

/*File handling logic starts*/
/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcInDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.

   lcInputFile = lcInDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:

      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.

      /*Accept only activation files*/
      IF NOT lcFileName BEGINS "sva_" THEN NEXT.

      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   lcErrorLog = lcLogDir + lcFileName + ".LOG".

   IF SESSION:BATCH THEN fBatchLog("START", lcInputFile).

   OUTPUT STREAM sLog TO VALUE(lcErrorLog) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.

   RUN pReadFileData.

   OUTPUT STREAM sLog CLOSE.
   fMove2TransDir(lcErrorLog, "", lcLogDir).
   /*fMove2TransDir(lcInputFile, "", lcProcDir).*/
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
         lcSetStatus = entry(3,lcline,";")
         lcFaxtoEmailNumber = entry(4,lcLine,";")
         /*lcFaxtoEmailEmail = entry(5,lcLine,";")*/ .
      
      FIND FIRST MobSub NO-LOCK WHERE 
                 MobSub.brand EQ gcBrand AND
                 MobSub.Fixednumber EQ lcFixedNum AND
                 Mobsub.Paytype EQ FALSE NO-ERROR.
      IF NOT AVAIL Mobsub THEN DO:
         lcErrText = lcLine + ";ERROR: Subscription not found".
         NEXT. /*next line*/
      END.
      /*Allowed state transitions:
      pending deactivation -> inactive
      pending activation -> active.*/
      IF lcSetStatus EQ "Active" THEN 
         liReqType = {&REQTYPE_CONTRACT_ACTIVATION}.
      ELSE IF lcSetStatus EQ "Inactive" THEN
         liReqType = {&REQTYPE_CONTRACT_TERMINATION}.
      ELSE DO:
         lcErrText = lcLine + "Incorrect action code " + lcSetStatus.
         NEXT.
      END.
      liReqStatus = {&REQUEST_STATUS_CONFIRMATION_PENDING}.

      FIND FIRST MsRequest WHERE
                 MsRequest.Brand EQ gcBrand AND
                 MsRequest.ReqType EQ liReqType AND
                 MsRequest.ReqStatus EQ liReqStatus.
      IF NOT AVAIL MsRequest THEN DO:
         lcErrText = lcLine + ";" + "Action not allowed: Reauested " + 
                     lcSetStatus.
         fReqSStatus(2, "Incorrect SVA action").            
         NEXT.
      END.
      /*Make actual status change for the request and create / remove fee*/

      /*fSendEmailByRequest*/
      fReqStatus(4, "Cancel SVA request ").

   END.
END PROCEDURE.
