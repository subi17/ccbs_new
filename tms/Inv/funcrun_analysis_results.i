/* ----------------------------------------------------------------------
  MODULE .......: funcrun_analysis_results.i
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: 07.03.16
  Version ......: Yoigo
----------------------------------------------------------------------- */

PROCEDURE pAnalyseAnalysis:
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

PROCEDURE pSaveAnalysisResults:
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
