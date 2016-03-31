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
{ftransdir.i}
{eventlog.i}
{fmakemsreq.i}
{barrfunc.i}

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
DEF VAR lcTableName AS CHAR NO-UNDO.
DEF VAR lcActionID AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC NO-UNDO.
DEF VAR lcErrorLog AS CHAR NO-UNDO.

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

      /*Accept only activation files*/
      IF NOT lcFileName BEGINS "IFS_Q25HR_ACTIVE_" THEN NEXT.

      /*IF NOT lcFileName BEGINS "yoigocan" AND
         NOT lcFileName BEGINS "yoigoanu" THEN NEXT.*/

      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   lcErrorLog = lcLogSpoolDir + lcFileName + ".LOG".

   IF SESSION:BATCH THEN fBatchLog("START", lcInputFile).

   OUTPUT STREAM sLog TO VALUE(lcErrorLog) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.

   RUN pReadFileData.

   PUT STREAM sLog UNFORMATTED lcSummary SKIP.
   OUTPUT STREAM sLog CLOSE.
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

INPUT STREAM sin CLOSE.

PROCEDURE pMakeProdigyRequest:
   DEF INPUT PARAM iiMsSeq AS INT NO-UNDO.
   DEF INPUT-OUTPUT PARAM ocLine AS CHAR NO-UNDO.
   DEF VAR liReq AS INT NO-UNDO.
   DEF VAR lcError AS CHAR NO-UNDO.

   /* Create subrequests (set mandataory and orig request) */
   liReq = fServiceRequest (iiMsSeq,
                            "LP",
                            1,
                            "REDIRECTION_HIGHRISKCUSTOMER_1",
                            fSecOffSet(fMakeTS(),5),
                            "",                /* SalesMan */
                            FALSE,             /* Set fees */
                            FALSE,             /* SMS */
                            "",
                            "",
                            0,
                            FALSE,
                            OUTPUT lcError).

   /* Creation of subrequests failed, "fail" master request too */
   IF liReq = 0 OR liReq = ? THEN DO:
      fReqStatus(3,"ServiceRequest failure: " + lcError).
      ocLine = ocLine + " Error: ServiceRequest failure". 
      RETURN.
   END.
   ocLine = ocLine + " OK".

END.


PROCEDURE pReadFileData:

   DEF VAR lcLine AS CHAR NO-UNDO.
   DEF VAR lcOrgId AS CHAR NO-UNDO.
   DEF VAR lcErrorCode AS CHAR NO-UNDO.
   DEF VAR ldeCancelAmt AS DEC NO-UNDO.
   DEF VAR liCustNum AS INT NO-UNDO.
   DEF VAR liMsSeq AS INT NO-UNDO.
   DEF VAR liPeriod AS INT NO-UNDO.
   DEF VAR lcPeriod AS char NO-UNDO.
   DEF VAR liLineNum AS INT NO-UNDO.
   DEF VAR ldAmount AS DEC NO-UNDO.
   DEF VAR liMonth AS INT NO-UNDO.
   DEF VAR liYear AS INT NO-UNDO.
   DEF VAR ldaStartDate AS DATE NO-UNDO.
   DEF VAR ldaEndDate AS DATE NO-UNDO.

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
         lcPeriod = entry(3,lcline,";")
         liYear = INT(SUBSTRING(lcPeriod,1,4))
         liMonth = INT(SUBSTRING(lcPeriod,5,2))
         ldaStartDate = DATE(liMonth, 1, liYear) - 1
         ldaEndDate = fLastDayOfMonth(ldaStartdate + 1).
      
      FIND FIRST SingleFee USE-INDEX BillCode WHERE
                 SingleFee.Brand       EQ gcBrand AND
                 SingleFee.CustNum     EQ liCustNum AND
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
            PUT STREAM sLog UNFORMATTED
               lcLine +  " Error released." SKIP.
            NEXT.
         END.

         IF fisQ25ExtensionDone(liMsSeq, 0, ldAmount) THEN DO:
            /* log extension done */
            PUT STREAM sLog UNFORMATTED
               lcLine +  " Error: extension done." SKIP.
            NEXT.
         END.

         IF fisQ25TerminalReturned(SingleFee.orderId) THEN DO:
            /* log terminal returned */
            PUT STREAM sLog UNFORMATTED
               lcLine +  " Error: terminal returned." SKIP.
            NEXT.
         END.
         FIND FIRST DCCLI USE-INDEX PerContractId NO-LOCK WHERE
                    DCCLI.PerContractId = INT(Singlefee.sourcekey) AND
                    DCCLI.Brand   = gcBrand AND
                    DCCLI.DCEvent BEGINS "PAYTERM" AND
                    DCCLI.MsSeq   = liMsseq AND
                    DCCLI.ValidTo >= ldaStartDate AND
                    DCCLI.ValidTo <= ldaEndDate NO-ERROR.
         IF fisQ25RenewalDone(liMsSeq, DCCLI.Validfrom) THEN DO:
            /*log renewal done */
            PUT STREAM sLog UNFORMATTED
               lcLine +  " Error: renewal done." SKIP.
            NEXT.
         END.


         /* check barring statuses */
         IF fGetBarringStatus("Debt_HOTLP", 
                              liMsSeq) NE {&BARR_STATUS_INACTIVE} THEN DO:
            PUT STREAM sLog UNFORMATTED
               lcLine +  " Error: Debt_HOTLP barring status." SKIP.
            NEXT.
         END.
         
         IF fGetBarringStatus("Debt_LP", 
                              liMsSeq) NE {&BARR_STATUS_INACTIVE} THEN DO:
            PUT STREAM sLog UNFORMATTED
               lcLine +  " Error: Debr_LP barring status." SKIP.
            NEXT.
         END.
         RUN pMakeProdigyRequest(liMsSeq, lcLine).
            PUT STREAM sLog UNFORMATTED
               lcLine SKIP.
            NEXT.

      END.
   END.

END PROCEDURE.
