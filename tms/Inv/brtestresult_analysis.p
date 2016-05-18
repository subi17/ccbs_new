/* ----------------------------------------------------------------------
  MODULE .......: brtestresult_analysis.p
  TASK .........: Analyse test results of a billrun
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 14.03.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/finvnum.i}
{Syst/funcrunprocess_update.i}
{Func/date.i}
{Syst/tmsconst.i}
{Func/ftaxdata.i}

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
   RUN pAnalyseAnalysis(INPUT TABLE ttResult BY-REFERENCE,
                        INPUT iiFRProcessID,
                        INPUT idaInvDate,
                        INPUT iiInvType,
                        INPUT iiUpdateInterval).

IF NOT RETURN-VALUE BEGINS "ERROR" THEN
   RUN pSaveAnalysisResults(INPUT TABLE ttResult BY-REFERENCE,
                            INPUT iiFRExecID,
                            INPUT iiBRTestQueueID).
   
IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

RETURN RETURN-VALUE.

/***** Main end ******/

{Inv/funcrun_analysis_results.i}

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


