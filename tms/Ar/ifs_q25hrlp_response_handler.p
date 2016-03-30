/* ----------------------------------------------------------------------
  MODULE .......: ifs_q25hrlp_response_reader.p
  TASK .........: Handle Q25 HRLP response file (sent by IFS)
  APPLICATION ..: tms
  AUTHOR .......: ilsavola & kariaika
  CREATED ......: 29.3.2016
  Version ......: yoigo
---------------------------------------------------------------------- */
{tmsconst.i}
{q25functions.i}

DEF VAR lcProcessedFile AS CHAR NO-UNDO.
DEF VAR lcIncDir AS CHAR NO-UNDO.
DEF VAR lcIncProcDir AS CHAR NO-UNDO.
DEF VAR lcRootDir AS CHAR NO-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.
DEF VAR lcInputFile AS CHAR NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcLogSpoolDir AS CHAR NO-UNDO.
DEF VAR lcLogOutDir AS CHAR NO-UNDO.
DEF VAR lcSummary AS CHAR NO-UNDO.

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

lcTableName = "HRLP".
lcActionID = {&Q25_HRLP_RESP_READER}.
ldCurrentTimeTS = fMakeTS().


DO TRANS:

   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      QUIT.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /*First execution stamp*/
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE ActionLog.
      RETURN. /*No reporting in first time.*/
   END.
   ELSE DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.

      RELEASE Actionlog.
   END.
END.

/*Kari, add file reading functionality here*/
/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.

   lcInputFile = lcIncDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:

      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.

      IF NOT lcFileName BEGINS "yoigocan" AND
         NOT lcFileName BEGINS "yoigoanu" THEN NEXT.

      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   ASSIGN
      liNumErr = 0
      liNumOK = 0
      lcTFBank = ""
      lcErrorLog = lcLogSpoolDir + lcFileName + ".LOG".

   IF SESSION:BATCH THEN fBatchLog("START", lcInputFile).

   lcTFBank = (IF INDEX(lcFileName,"SABADELL") > 0
               THEN {&TF_BANK_SABADELL}
               ELSE {&TF_BANK_UNOE}).

   OUTPUT STREAM sLog TO VALUE(lcErrorLog) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.

   EMPTY TEMP-TABLE ttTFCancel.

   RUN pReadFileData.
   INPUT STREAM sin CLOSE.

   fMove2TransDir(lcErrorLog, "", lcLogOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcIncProcDir).
   IF SESSION:BATCH AND lcProcessedFile NE "" THEN
      fBatchLog("FINISH", lcProcessedFile).




   DO TRANS:
      FIND FIRST ActionLog WHERE
                 ActionLog.Brand     EQ  gcBrand        AND
                 ActionLog.ActionID  EQ  lcActionID     AND
                 ActionLog.TableName EQ  lcTableName    AND
                 ActionLog.ActionStatus NE  {&ACTIONLOG_STATUS_SUCCESS}
      EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL ActionLog THEN DO:
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
      END.
      RELEASE ActionLog.
   END.

END.

PROCEDURE pReadFileData:

   DEF VAR lcLine AS CHAR NO-UNDO.
   DEF VAR lcOrgId AS CHAR NO-UNDO.
   DEF VAR liMonth AS INT NO-UNDO.
   DEF VAR liYear AS INT NO-UNDO.
   DEF VAR lcErrorCode AS CHAR NO-UNDO.
   DEF VAR ldeCancelAmt AS DEC NO-UNDO.

   DEF VAR liLineNum AS INT NO-UNDO.

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
         liCustNum = int(entry(1,lcline,";"))
         limsseq = int(entry(2,lcline,";"))
         liPeriod = int(entry(3,lcline,";"))
      
      FIND FIRST SingleFee USE-INDEX BillCode WHERE
                 SingleFee.Brand       EQ gcBrand AND
                 SingleFee.CustNum     EQ iiCustNum AND
                 SingleFee.HostTable   EQ "Mobsub" AND
                 SingleFee.Keyvalue    EQ STRING(limsseq) AND
                 SingleFee.BillPeriod  EQ liPeriod AND
                 SingleFee.Billcode    BEGINS "RVTERM" AND
                 SingleFee.SourceTable EQ "DCCLI" AND
                 SingleFee.CalcObj     EQ "RVTERM" NO-LOCK NO-ERROR.
      IF AVAIL SingleFee THEN DO:
         IF SingleFee.OrderId <= 0 THEN NEXT.   
         IF fisPostpaidMobsubReleased(liMsSeq) THEN DO:
            /* log mobsub released */
            NEXT.
         END.

         IF fisQ25RenewalDone(liMsSeq, /* from */) THEN DO:
            /*log renewal done */
            NEXT.
         END.

         IF fisQ25ExtensionDone(liMsSeq, 0) THEN DO:
            /* log extension done */
            NEXT.
         END.

         IF fisQ25TerminalReturned(/* SingleFee.orderId */) THEN DO:
            /* log terminal returned */
            NEXT.
         END.

         /* check barring */

      END.

END PROCEDURE.
