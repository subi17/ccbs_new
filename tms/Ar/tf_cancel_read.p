/* ----------------------------------------------------------------------
  MODULE .......: tf_cancel_read.p
  TASK .........: Read terminal financing cancellation file response
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 11.11.2014
  Version ......: yoigo
---------------------------------------------------------------------- */
{commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{cparam2.i}
{timestamp.i}
{tmsconst.i}
{tsformat.i}
{ftransdir.i}
{eventlog.i}

DEF VAR lcProcessedFile AS CHAR NO-UNDO.
DEF VAR lcIncDir AS CHAR NO-UNDO. 
DEF VAR lcIncProcDir AS CHAR NO-UNDO. 
DEF VAR lcRootDir AS CHAR NO-UNDO. 
DEF VAR lcFileName AS CHAR NO-UNDO. 
DEF VAR lcInputFile AS CHAR NO-UNDO. 
DEF VAR liNumOk AS INT NO-UNDO. 
DEF VAR liNumErr AS INT NO-UNDO. 
DEF VAR lcErrorLog AS CHAR NO-UNDO. 
DEF VAR lcLogDir AS CHAR NO-UNDO. 
DEF VAR lcLogSpoolDir AS CHAR NO-UNDO. 
DEF VAR lcLogOutDir AS CHAR NO-UNDO. 
DEF VAR lcSummary AS CHAR NO-UNDO. 
DEF VAR lcTFBank AS CHAR NO-UNDO. 

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

lcRootDir = fCParam("TermFinance","CanInRoot").
IF lcRootDir EQ ? OR
   NOT lcRootDir > "" THEN RETURN "ERROR:Root directory not defined".
   
lcLogDir = fCParam("TermFinance","LogDir").
IF lcLogDir EQ ? OR
   NOT lcLogDir > "" THEN RETURN "ERROR:Log root directory not defined".

DEFINE TEMP-TABLE ttTFCancel NO-UNDO
   FIELD lineNum AS INT
   FIELD content AS char
   FIELD OrgId AS CHAR 
   FIELD BankMonth AS INT
   FIELD BankYear AS INT
   FIELD CancelAmt AS DEC
   FIELD ErrorCode AS CHAR
   FIELD OrderId AS INT
   FIELD CodFPago AS CHAR
INDEX linenum IS PRIMARY linenum.

ASSIGN
   lcLogSpoolDir = lcLogDir + "spool/"
   lcLogOutDir = lcLogDir + "outgoing/"
   lcIncDir = lcRootDir + "incoming/"
   lcIncProcDir = lcRootDir + "processed/".

FUNCTION fWriteLog RETURNS LOGIC
   (icLine AS CHAR,
    icMessage AS CHAR):
   
   IF icMessage BEGINS "ERROR" THEN liNumErr = liNumErr + 1.

   PUT STREAM sLog UNFORMATTED
      icLine ";"
      icMessage SKIP.
END FUNCTION.

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
               ELSE IF INDEX(lcFileName,"CETELEM") > 0
                  THEN {&TF_BANK_CETELEM}
               ELSE {&TF_BANK_UNOE}).
   
   OUTPUT STREAM sLog TO VALUE(lcErrorLog) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.

   EMPTY TEMP-TABLE ttTFCancel.
   
   RUN pReadFileData.
   INPUT STREAM sin CLOSE.
   RUN pProcessData.
   
   lcSummary = SUBST("input: &1, updated: &2, errors: &4",
                (liNumOK + liNumErr),
                liNumOK,
                liNumErr).
   
   PUT STREAM sLog UNFORMATTED lcSummary SKIP.
   OUTPUT STREAM sLog CLOSE.

   fMove2TransDir(lcErrorLog, "", lcLogOutDir). 
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcIncProcDir). 
   IF SESSION:BATCH AND lcProcessedFile NE "" THEN
      fBatchLog("FINISH", lcProcessedFile).

   DO TRANS:
      
      CREATE ActionLog.
      
      ASSIGN
         ActionLog.Brand        = gcBrand
         ActionLog.ActionID     = "TF_CREAD_" + lcTFBank
         ActionLog.ActionTS     = fMakeTS()
         ActionLog.TableName    = "Cron"
         ActionLog.KeyValue     = lcFilename
         ActionLog.UserCode     = katun
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
         ActionLog.ActionChar   = lcSummary.

      RELEASE ActionLog.   
   END.

END.

INPUT STREAM sFile CLOSE.

PROCEDURE pReadFileData:

   DEF VAR lcLine AS CHAR NO-UNDO. 
   DEF VAR lcOrgId AS CHAR NO-UNDO. 
   DEF VAR liMonth AS INT NO-UNDO. 
   DEF VAR liYear AS INT NO-UNDO. 
   DEF VAR liOrderId AS INT NO-UNDO.
   DEF VAR lcCodFPago AS CHAR NO-UNDO.
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

      IF LENGTH(lcLine) NE 210 THEN DO:
         fWriteLog(lcLine,"ERROR:Incorrect line length").
         NEXT FILE_LINE.
      END.

      ASSIGN 
         lcOrgId     = trim(substring(lcLine,1,9))
         ldeCancelAmt = int(substring(lcLine,14,11))
         liMonth     = int(substring(lcLine,25,2))
         liYear      = int(substring(lcLine,27,4))
         liOrderId   = int(substring(lcLine,31,8))
         lcCodFPago  = trim(substring(lcLine,39,4))
         ldeCancelAmt = ldeCancelAmt / 100.0
         lcErrorCode = substring(lcLine,167,2)
         NO-ERROR.
 
      IF ERROR-STATUS:ERROR THEN DO:
         fWriteLog(lcLine,"ERROR:Incorrect line syntax").
         NEXT FILE_LINE.
      END.

      IF liMonth < 1 OR liMonth > 12 THEN DO:
         fWriteLog(lcLine,"ERROR:Incorrect month syntax").
         NEXT FILE_LINE.
      END.
      
      IF liYear < 2000 THEN DO:
         fWriteLog(lcLine,"ERROR:Incorrect year syntax").
         NEXT FILE_LINE.
      END.
      
      CREATE ttTFCancel.
      ASSIGN
         ttTFCancel.LineNum = liLineNum
         ttTFCancel.Content = lcLine
         ttTFCancel.OrgId = lcOrgId
         ttTFCancel.BankMonth = liMonth
         ttTFCancel.BankYear = liYear
         ttTFCancel.OrderId = liOrderId
         ttTFCancel.CancelAmt = ldeCancelAmt
         ttTFCancel.ErrorCode = lcErrorCode
         ttTFCancel.CodFPago = lcCodFPago.
   END.
  
END PROCEDURE. 
   
PROCEDURE pProcessData:

   DEF BUFFER bttTFCancel FOR TEMP-TABLE ttTFCancel.

   LINE_LOOP:
   FOR EACH ttTFCancel:
      
      IF NOT SESSION:BATCH AND ttTFCancel.LineNum MOD 10 = 0 THEN DO:
         DISP "Processing data.. " lcFilename ttTFCancel.LineNum with frame a.
         PAUSE 0.
      END.
      
      IF ttTFCancel.OrderId > 0 THEN
         FIND FixedFeeTF EXCLUSIVE-LOCK WHERE
              FixedFeeTF.OrderId = ttTFCancel.OrderId AND
         YEAR(FixedFeeTF.BankDate) = ttTFCancel.BankYear AND
        MONTH(FixedFeeTF.BankDate) = ttTFCancel.BankMonth AND
              FixedFeeTF.Amount = ttTFCancel.CancelAmt AND
              FixedFeeTF.TFBank = lcTFBank NO-ERROR.
      ELSE 
         FIND FIRST FixedFeeTF EXCLUSIVE-LOCK WHERE
                    FixedFeeTF.OrgId = ttTFCancel.OrgId AND 
               YEAR(FixedFeeTF.BankDate) = ttTFCancel.BankYear AND
              MONTH(FixedFeeTF.BankDate) = ttTFCancel.BankMonth AND
                    FixedFeeTF.Amount = ttTFCancel.CancelAmt AND
                    FixedFeeTF.TFBank = lcTFBank AND
                    FixedFeeTF.CancelStatus = "SENT" NO-ERROR.
      
      IF AVAIL FixedFeeTF THEN DO:
               
         FixedFeeTF.CancelResp = ttTFCancel.Errorcode.
         
         IF FixedFeeTF.ResidualAmount > 0 AND
            NOT CAN-FIND(
               FIRST bttTFCancel NO-LOCK WHERE
                     bttTFCancel.OrgId = ttTFCancel.OrgId AND
                     bttTFCancel.CancelAmt = FixedFeeTF.ResidualAmount AND
                     bttTFCancel.BankYear = ttTFCancel.BankYear AND
                     bttTFCancel.BankMont = ttTFCancel.BankMonth AND
               ROWID(bttTFCancel) NE ROWID(ttTFCancel) AND
                     (IF ttTFCancel.OrderId > 0
                      THEN bttTFCancel.OrderId = ttTFCancel.OrderID
                      ELSE TRUE)) THEN DO:
            ASSIGN
               FixedFeeTF.CancelStatus = "ERROR_RESP"
               FixedFeeTF.CancelMemo = "ERROR:Response file does not contain related residual fee".
            fWriteLog(ttTFCancel.content,FixedFeeTF.CancelStatus).
            NEXT LINE_LOOP.
         END.
      END.
      ELSE DO:
         FIND FIRST FixedFeeTF EXCLUSIVE-LOCK WHERE
                    FixedFeeTF.OrgId = ttTFCancel.OrgId AND 
               YEAR(FixedFeeTF.BankDate) = ttTFCancel.BankYear AND
              MONTH(FixedFeeTF.BankDate) = ttTFCancel.BankMonth AND
                    FixedFeeTF.ResidualAmount = ttTFCancel.CancelAmt AND
                    FixedFeeTF.TFBank = lcTFBank AND
                    /* the PAYTERM fee should be handled in the previous loop,
                       so the cancel status is not SENT anymore */
                   (FixedFeeTF.CancelStatus NE "NEW" AND
                    FixedFeeTF.CancelStatus NE "") AND
                   (IF ttTFCancel.OrderId > 0
                    THEN FixedFeeTF.OrderId = ttTFCancel.OrderID
                    ELSE TRUE) NO-ERROR.

         IF NOT AVAIL FixedFeeTF THEN DO:
            fWriteLog(ttTFCancel.content, "ERROR:Installment fee not found").
            NEXT LINE_LOOP.
         END.
         
         FixedFeeTF.CancelResp = ttTFCancel.Errorcode.

         IF FixedFeeTF.Amount > 0 AND
            NOT CAN-FIND(
               FIRST bttTFCancel NO-LOCK WHERE
                     bttTFCancel.OrgId = ttTFCancel.OrgId AND
                     bttTFCancel.CancelAmt = FixedFeeTF.Amount AND
                     bttTFCancel.BankYear = ttTFCancel.BankYear AND
                     bttTFCancel.BankMont = ttTFCancel.BankMonth AND
               ROWID(bttTFCancel) NE ROWID(ttTFCancel) AND
                  (IF ttTFCancel.OrderId > 0
                   THEN bttTFCancel.OrderId = ttTFCancel.OrderID
                   ELSE TRUE)) THEN DO:

            ASSIGN
               FixedFeeTF.CancelStatus = "ERROR_RESP"
               FixedFeeTF.CancelMemo = "ERROR:Response file does not contain related PAYTERM fee".

            fWriteLog(ttTFCancel.content, FixedFeeTF.CancelStatus).
            NEXT LINE_LOOP.
         END.

         IF FixedfeeTF.CancelStatus NE ttTFCancel.ErrorCode THEN DO:
            FixedFeeTF.CancelStatus = "ERROR_RESP".
            FixedFeeTF.CancelMemo =
               SUBST("ERROR:Residual fee cancel response &1 does not match with PAYTERM fee cancel status &2",
                     ttTFCancel.ErrorCode, FixedFeeTF.CancelStatus).
            fWriteLog(ttTFCancel.content, FixedFeeTF.CancelMemo).
            NEXT LINE_LOOP.
         END.
         
      END.
      
      ASSIGN
         FixedFeeTF.CancelStatus = ttTFCancel.Errorcode
         FixedfeeTF.CancelMemo = ""
         liNumOK = liNumOK + 1.

      RELEASE FixedFeeTF.
   END.

END PROCEDURE. 
   
