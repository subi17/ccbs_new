/* ----------------------------------------------------------------------
  MODULE .......: brtestresult_analysis.p
  TASK .........: Analyse test results of a billrun
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 14.03.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{finvnum.i}
{funcrunprocess_update.i}
{date.i}
{tmsconst.i}
{ftaxdata.i}

DEF INPUT  PARAMETER iiBRTestQueueID  AS INT  NO-UNDO.
DEF INPUT  PARAMETER idaInvDate       AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiInvType        AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRExecID       AS INT  NO-UNDO. 
DEF INPUT  PARAMETER iiUpdateInterval AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiAnalysed       AS INT  NO-UNDO.

DEF VAR liCheck   AS INT  NO-UNDO.
DEF VAR liCustQty AS INT  NO-UNDO. 

DEF TEMP-TABLE ttResult NO-UNDO
   FIELD InvCust AS INT
   FIELD MsSeq   AS INT
   FIELD InvNum  AS INT 
   FIELD ExtInvID AS CHAR 
   FIELD BRTestCaseID AS INT
   FIELD ResultValue AS DEC
   FIELD TestResult AS CHAR
   INDEX BRTestCaseID BRTestCaseID InvCust MsSeq.

FORM
   oiAnalysed COLON 10 LABEL "Analysed"
WITH OVERLAY SIDE-LABELS ROW 10 CENTERED TITLE " Analyse Cases " FRAME fQty.


FUNCTION fErrorLog RETURNS LOGIC
   (iiCustNum AS INT,
    icError   AS CHAR):
   
   DO TRANS:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = gcBrand
             ErrorLog.ActionID  = "BRANALYSIS"
             ErrorLog.TableName = "BRTestCase"
             ErrorLog.KeyValue  = STRING(iiCustNum)
             ErrorLog.ErrorMsg  = icError
             ErrorLog.UserCode  = katun.
             ErrorLog.ActionTS  = fMakeTS().
   END.
   
END FUNCTION.


/***** Main start *****/

IF NOT SESSION:BATCH THEN DO:
   PAUSE 0.
   VIEW FRAME fQty.
END.

RUN pInitialize.

IF NOT RETURN-VALUE BEGINS "ERROR" THEN 
   RUN pAnalyseAnalysis(INPUT TABLE ttResult,
                        INPUT iiFRProcessID,
                        INPUT idaInvDate,
                        INPUT iiInvType,
                        INPUT iiUpdateInterval).

IF NOT RETURN-VALUE BEGINS "ERROR" THEN
   RUN pSaveAnalysisResults(INPUT TABLE ttResult,
                            INPUT iiFRExecID,
                            INPUT iiBRTestQueueID).
   
IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

RETURN RETURN-VALUE.

/***** Main end ******/

{funcrun_analysis_results.i}

PROCEDURE pInitialize:

   DEF VAR liLoop     AS INT  NO-UNDO.
   DEF VAR liInvCust  AS INT  NO-UNDO.
   DEF VAR liMsSeq    AS INT  NO-UNDO.
   DEF VAR liTestCase AS INT  NO-UNDO.
   DEF VAR liTestQty  AS INT  NO-UNDO.
 
   FIND FIRST BRTestQueue WHERE BRTestQueue.BRTestQueueID = iiBRTestQueueID
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BRTestQueue OR NOT BRTestQueue.Active THEN 
      RETURN "ERROR:Queue not available".
      
   EMPTY TEMP-TABLE ttResult.
   
   FIND FIRST FuncRunProcess WHERE FuncRunProcess.FRProcessID = iiFRProcessID 
      NO-LOCK.
   FIND FIRST FuncRunExec WHERE FuncRunExec.FRExecID = iiFRExecID 
      NO-LOCK.
 
   FOR EACH FuncRunResult NO-LOCK WHERE
            FuncRunResult.FRExecID    = FuncRunExec.FeedFromExecSeq AND
            FuncRunResult.FRResultSeq = FuncRunProcess.ProcSeq:

      DO liLoop = 1 TO NUM-ENTRIES(FuncRunResult.CharParam):
         
         ASSIGN 
            liInvCust  = FuncRunResult.IntParam
            liMsSeq    = 
               INT(ENTRY(1,ENTRY(liLoop,FuncRunResult.CharParam),":"))
            liTestCase = 
               INT(ENTRY(2,ENTRY(liLoop,FuncRunResult.CharParam),":"))
            NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
            fErrorLog(FuncRunResult.IntParam,
                      "ERROR:Invalid result from collection; " + 
                             ENTRY(liLoop,FuncRunResult.CharParam) + 
                             ", customer " + STRING(FuncRunResult.IntParam)).
            NEXT.
         END.
         
         CREATE ttResult.
         ASSIGN 
            ttResult.InvCust = liInvCust
            ttResult.MsSeq   = liMsSeq
            ttResult.BRTestCaseID = liTestCase.
      END.
   END.   

   /* are there cases with no entries */
   FOR FIRST BRTestQueue NO-LOCK WHERE
             BRTestQueue.BRTestQueueID = iiBRTestQueueID,
        EACH BRTestQRow OF BRTestQueue NO-LOCK WHERE
             BRTestQRow.Active = TRUE:
             
      liTestQty = 0.
      FOR EACH ttResult WHERE
               ttResult.BRTestCaseID = BRTestQRow.BRTestCaseID:
         liTestQty = liTestQty + 1.
      END.
      
      IF liTestQty < BRTestQRow.CaseQty THEN DO:
         CREATE ttResult.
         ASSIGN 
            ttResult.InvCust = 0
            ttResult.MsSeq   = 0
            ttResult.BRTestCaseID = BRTestQRow.BRTestCaseID
            ttResult.TestResult = STRING(BRTestQRow.CaseQty - liTestQty) +
                                  " cases were not found".
      END.
   END.

   RETURN "". 

END PROCEDURE.

PROCEDURE pAnalyse:
DEFINE INPUT PARAMETER TABLE FOR ttResult. 
DEFINE INPUT PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEFINE INPUT PARAMETER idaInvDate       AS DATE NO-UNDO.
DEFINE INPUT PARAMETER iiInvType        AS INT  NO-UNDO.
DEFINE INPUT PARAMETER iiUpdateInterval AS INT  NO-UNDO.

   DEF VAR liMatch  AS INT  NO-UNDO.
   DEF VAR ldRowAmt AS DEC  NO-UNDO.
   

   FOR EACH ttResult WHERE ttResult.InvCust > 0,
      FIRST BRTestCase NO-LOCK WHERE
            BRTestCase.BRTestCaseID = ttResult.BRTestCaseID:
   
      ttResult.TestResult = "Invoice not created".
      
      FOR EACH Invoice NO-LOCK USE-INDEX CustNum WHERE
               Invoice.Brand = gcBrand AND
               Invoice.CustNum = ttResult.InvCust AND
               Invoice.InvDate = idaInvDate AND
               Invoice.InvType = iiInvType,
         FIRST SubInvoice OF Invoice NO-LOCK WHERE
               SubInvoice.MsSeq = ttResult.MsSeq:

         oiAnalysed = oiAnalysed + 1.
         IF NOT SESSION:BATCH AND oiAnalysed MOD 100 = 0 THEN DO:
            PAUSE 0.
            DISP oiAnalysed WITH FRAME fQty.
         END.

         ASSIGN 
            ttResult.InvNum   = Invoice.InvNum
            ttResult.ExtInvID = Invoice.ExtInvID.
 
         liMatch = 0.
         FOR EACH InvRow OF Invoice NO-LOCK WHERE
                  InvRow.SubInvNum = SubInvoice.SubInvNum AND
                  InvRow.BillCode = BRTestCase.BillCode:
            
            ASSIGN 
               liMatch = 1
               ldRowAmt = InvRow.Amt.
               
            IF InvRow.Qty NE BRTestCase.ResultQty THEN liMatch = 3.
            ELSE 
            CASE BRTestCase.RelationalOperator:
            WHEN "EQ" THEN 
               IF BRTestCase.ResultValue =  ldRowAmt THEN liMatch = 2.
            WHEN "NE" THEN 
               IF BRTestCase.ResultValue NE ldRowAmt THEN liMatch = 2.
            WHEN "LT" THEN 
               IF BRTestCase.ResultValue >  ldRowAmt THEN liMatch = 2.
            WHEN "LE" THEN 
               IF BRTestCase.ResultValue >= ldRowAmt THEN liMatch = 2.
            WHEN "GT" THEN 
               IF BRTestCase.ResultValue <  ldRowAmt THEN liMatch = 2.
            WHEN "GE" THEN 
               IF BRTestCase.ResultValue <= ldRowAmt THEN liMatch = 2.
            END CASE.
                
            ttResult.ResultValue = ldRowAmt.
              
            IF liMatch >= 2 THEN LEAVE.      
         END.

         CASE liMatch:
         WHEN 0 THEN DO:
            /* the desired result was that nothing should be found */
            IF BRTestCase.ResultValue = ? THEN 
               ttResult.TestResult = "OK".
            ELSE ttResult.TestResult = "Result not found".
         END.   
         WHEN 1 THEN ttResult.TestResult = "NOK".
         WHEN 2 THEN ttResult.TestResult = "OK".
         WHEN 3 THEN ttResult.TestResult = "Quantity does not match".
         END CASE.

      END.
      
      IF iiUpdateInterval > 0 AND oiAnalysed MOD iiUpdateInterval = 0 
      THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiAnalysed) THEN
            RETURN "ERROR:Stopped".
      END.   
   END.
   
   RETURN "".
   
END PROCEDURE.

PROCEDURE pSaveResults:
DEFINE INPUT PARAMETER TABLE FOR ttResult. 
DEFINE INPUT PARAMETER iiFRExecID      AS INT NO-UNDO.
DEFINE INPUT PARAMETER iiBRTestQueueID AS INT NO-UNDO. 

   DEF VAR liResultID    AS INT  NO-UNDO.
   DEF VAR ldResultStamp AS DEC  NO-UNDO.

   ldResultStamp = fMakeTS().
   FOR FIRST FuncRunExec NO-LOCK WHERE
             FuncRunExec.FRExecID = iiFRExecID:
      ldResultStamp = FuncRunExec.StartTS.
   END.   

   REPEAT TRANS:
   
      FIND FIRST BRTestQResult WHERE 
         BRTestQResult.BRTestQueueID = iiBRTestQueueID AND
         BRTestQResult.TestRunStamp = ldResultStamp NO-LOCK NO-ERROR.
      IF AVAILABLE BRTestQResult THEN DO:
         liResultID = BRTestQResult.BRTestQResultID.
         LEAVE.
      END.
    
      /* lock BRTestQueue -> make sure that another process cannot do this */  
      FIND FIRST BRTestQueue WHERE BRTestQueue.BRTestQueueID = iiBRTestQueueID
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF LOCKED(BRTestQueue) THEN DO:
         PAUSE 2. 
         NEXT.
      END.   
      
      FIND LAST BRTestQResult USE-INDEX BRTestQResultID NO-LOCK NO-ERROR.
      IF AVAILABLE BRTestQResult THEN 
         liResultID = BRTestQResult.BRTestQResultID + 1.
      ELSE liResultID = 1.
      
      CREATE BRTestQResult.
      ASSIGN 
         BRTestQResult.BRTestQResultID = liResultID
         BRTestQResult.BRTestQueueID = iiBRTestQueueID
         BRTestQResult.TestRunStamp  = ldResultStamp
         BRTestQResult.UserCode = katun.

      RELEASE BRTestQueue.   
      RELEASE BRTestQResult.
      LEAVE.
   END.
   
   FOR EACH ttResult,
      FIRST BRTestQRow NO-LOCK WHERE
            BRTestQRow.BRTestQueueID = iiBRTestQueueID AND
            BRTestQRow.BRTestCaseID = ttResult.BRTestCaseID AND
            BRTestQRow.Active = TRUE TRANS:

      CREATE BRTestQResultRow.
      BUFFER-COPY ttResult TO BRTestQResultRow.
      ASSIGN 
         BRTestQResultRow.BRTestQResultID = liResultID
         BRTestQResultRow.BRTestQRowID    = BRTestQRow.BRTestQRowID
         BRTestQResultRow.QResultRowID    = NEXT-VALUE(QResultRowID).
   END.
   
END PROCEDURE.


