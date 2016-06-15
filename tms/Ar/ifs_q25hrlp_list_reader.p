/* ----------------------------------------------------------------------
  MODULE .......: ifs_q25hrlp_response_reader.p
  TASK .........: Handle Q25 HRLP response file (sent by IFS)
  APPLICATION ..: tms
  AUTHOR .......: ilsavola & kariaika
  CREATED ......: 29.3.2016
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
DEF VAR lcRootDir AS CHAR NO-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.
DEF VAR lcInputFile AS CHAR NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcLogOutDir AS CHAR NO-UNDO.
DEF VAR lcTableName AS CHAR NO-UNDO.
DEF VAR lcActionID AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC NO-UNDO.
DEF VAR lcErrorLog AS CHAR NO-UNDO.

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

ASSIGN 
   lcTableName = "HRLP"
   lcActionID = {&Q25_HRLP_RESP_READER}
   ldCurrentTimeTS = fMakeTS().

fInitHRLPParameters().

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
      QUIT. /*No reporting in first time.*/
   END.
   ELSE IF (liHRLPTestLevel EQ {&Q25_HRLP_NO_TEST}) THEN DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.

      RELEASE Actionlog.
   END.
END.

/*File handling logic starts*/
/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcHRLPListInDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.

   lcInputFile = lcHRLPListInDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:

      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.

      /*Accept only activation files*/
      IF NOT lcFileName BEGINS "IFS_Q25HR_ACTIVE_" THEN NEXT.

      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   lcErrorLog = lcHRLPSpoolDir + lcFileName + ".LOG".

   IF SESSION:BATCH THEN fBatchLog("START", lcInputFile).

   OUTPUT STREAM sLog TO VALUE(lcErrorLog) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.

   RUN pReadFileData.

   OUTPUT STREAM sLog CLOSE.
   fMove2TransDir(lcErrorLog, "", lcHRLPLogDir).
   fMove2TransDir(lcInputFile, "", lcHRLPInProcDir).
   IF SESSION:BATCH AND lcInputFile NE "" THEN
      fBatchLog("FINISH", lcInputFile).
END.

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

INPUT STREAM sin CLOSE.

PROCEDURE pReadFileData:

   DEF VAR lcLine AS CHAR NO-UNDO.
   DEF VAR lcOrgId AS CHAR NO-UNDO.
   DEF VAR lcErrorCode AS CHAR NO-UNDO.
   DEF VAR ldeCancelAmt AS DEC NO-UNDO.
   DEF VAR liCustNum AS INT NO-UNDO.
   DEF VAR liMsSeq AS INT NO-UNDO.
   DEF VAR lcMsisdn AS CHAR NO-UNDO.
   DEF VAR liPeriod AS INT NO-UNDO.
   DEF VAR lcDate AS char NO-UNDO.
   DEF VAR liLineNum AS INT NO-UNDO.
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
         lcMsisdn = entry(2,lcline,";")
         lcDate = entry(3,lcline,";")
         liPeriod = YEAR(DATE(lcDate) - 1) * 100 +
                    MONTH(DATE(lcDate) - 1)
         liYear = YEAR(DATE(lcDate))
         liMonth = MONTH(DATE(lcDate))
         ldaStartDate = DATE(liMonth - 1, 1, liYear) - 1
         ldaEndDate = DATE(lcDate).
      
      FIND FIRST MobSub NO-LOCK WHERE 
                 MobSub.brand EQ "1" AND
                 MobSub.Cli EQ lcMsisdn AND
                 Mobsub.Paytype EQ FALSE NO-ERROR.
      IF NOT AVAIL Mobsub THEN DO:
         /* log mobsub released */
         PUT STREAM sLog UNFORMATTED
            lcLine + {&Q25_HRLP_DELIM} + "Error released." SKIP.
         NEXT.
      END.
     
      liMsSeq = Mobsub.MsSeq.
      IF (liHRLPTestLevel EQ {&Q25_HRLP_FULL_TEST} AND 
         LOOKUP(STRING(liMsSeq),lcHRLPTestMSSeq) GT 0) THEN DO:
         fMakeProdigyRequest(liMsSeq, liCustNum, 
                             "REDIRECTION_HIGHRISKCUSTOMER_1",
                             INPUT-OUTPUT lcLine).
         PUT STREAM sLog UNFORMATTED
            lcLine + " TEST" SKIP.
      END.
      ELSE DO:
         FIND FIRST SingleFee WHERE
                    SingleFee.Brand       EQ gcBrand AND
                    SingleFee.CustNum     EQ liCustNum AND
                    SingleFee.HostTable   EQ "Mobsub" AND
                    SingleFee.Keyvalue    EQ STRING(Mobsub.msseq) AND
                    SingleFee.BillPeriod  EQ liPeriod AND
                    SingleFee.Billcode    BEGINS "RVTERM" AND
                    SingleFee.SourceTable EQ "DCCLI" AND
                    SingleFee.CalcObj     EQ "RVTERM" NO-LOCK NO-ERROR.
         IF AVAIL SingleFee THEN DO:
            IF SingleFee.OrderId <= 0 THEN NEXT.   
            IF (liHRLPTestLevel EQ {&Q25_HRLP_ONLY_PROV_TEST}) AND
               (LOOKUP(STRING(liMsSeq),lcHRLPTestMSSeq) EQ 0) THEN DO:
               MESSAGE "MsSeq is not in test list: " + STRING(liMsSeq) VIEW-AS 
                       ALERT-BOX.
               QUIT.
            END.
            IF fisQ25ExtensionDone(liMsSeq) THEN DO:
               /* log extension done */
               PUT STREAM sLog UNFORMATTED
                  lcLine + {&Q25_HRLP_DELIM} +  "Error: extension done." SKIP.
               NEXT.
            END.
            IF fisQ25PendingRequest(liMsSeq) THEN DO:
               /* log pending/ongoing q25 extension */
               PUT STREAM sLog UNFORMATTED
                  lcLine + {&Q25_HRLP_DELIM} +  "Error: pending Q25 request." SKIP.
               NEXT.
            END.
            IF fisQ25TerminalReturned(SingleFee.orderId) THEN DO:
               /* log terminal returned */
               PUT STREAM sLog UNFORMATTED
                  lcLine + {&Q25_HRLP_DELIM} + "Error: terminal returned." SKIP.
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
                  lcLine + {&Q25_HRLP_DELIM} + "Error: renewal done." SKIP.
               NEXT.
            END.


            /* check barring statuses */
            IF fGetBarringStatus("Debt_HOTLP", 
                                 liMsSeq) NE {&BARR_STATUS_INACTIVE} THEN DO:
               PUT STREAM sLog UNFORMATTED
                  lcLine + {&Q25_HRLP_DELIM} + 
                  "Error: Debt_HOTLP barring status." SKIP.
               NEXT.
            END.
            
            IF fGetBarringStatus("Debt_LP", 
                                 liMsSeq) NE {&BARR_STATUS_INACTIVE} THEN DO:
               PUT STREAM sLog UNFORMATTED
                  lcLine + {&Q25_HRLP_DELIM} + 
                  "Error: Debr_LP barring status." SKIP.
               NEXT.
            END.
            fMakeProdigyRequest(liMsSeq, liCustNum, 
                                "REDIRECTION_HIGHRISKCUSTOMER_1",
                                INPUT-OUTPUT lcLine).
            PUT STREAM sLog UNFORMATTED
               lcLine SKIP.
         END.
         ELSE
            PUT STREAM sLog UNFORMATTED
               lcLine + {&Q25_HRLP_DELIM} + "No Q25 Fee found" SKIP.
      END.
   END.

END PROCEDURE.
