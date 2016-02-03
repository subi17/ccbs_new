{Syst/commpaa.i}
katun = "Qvantel".
gcBrand = "1".

DEFINE VARIABLE liBRTestQResultID AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcFile1 AS CHARACTER NO-UNDO. 
lcFile1 = "/apps/yoigo/tms_support/billing/brtestlog.txt".

UPDATE 
   liBRTestQResultID FORMAT ">>>>9" LABEL "BRTestQResultID: " SKIP
   lcFile1      FORMAT "X(50)" LABEL "Log File: " SKIP(1)
with overlay side-labels 1 column row 4 centered title "Generate BR Test result log file"
frame fcontrolrep.
hide frame fcontrolrep no-pause.

IF liBRTestQResultID = 0 OR 
   NOT lcFile1 > "" THEN RETURN.

FIND BRTestQResult NO-LOCK WHERE
     BRTestQResult.BRTestQResultID = liBRTestQResultID NO-ERROR.
IF NOT AVAIL BRTestQResult THEN DO:
   MESSAGE "BRTestQResultID" liBRTestQResultID "not found" VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

IF NOT AVAIL BRTestQResult THEN RETURN.

DEF STREAM SOUT.
OUTPUT STREAM SOUT TO VALUE(lcFile1).

DEFINE VARIABLE liTestCaseID AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcTestCase AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCustName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO. 

FUNCTION fTestCase RETURNS LOGIC
   (iiBRTestQRowID AS INT):

   ASSIGN
      lcTestCase = ""
      liTestCaseID = 0.
   FOR FIRST BRTestQRow NO-LOCK WHERE
             BRTestQRow.BRTestQRowID = iiBRTestQRowID,
       FIRST BRTestCase NO-LOCK WHERE
             BRTestCase.BRTestCaseID = BRTestQRow.BRTestCaseID:
      ASSIGN
         lcTestCase = BRTestCase.Description
         liTestCaseID = BRTestCase.BRTestCaseID.
   END.

   RETURN (liTestCaseID > 0).
   
END FUNCTION.

put stream sout unformatted "TestQueueResultID|QueueResultRowID|TestQueueRowID|TestCase|Description|InvoiceCustomer|InvoiceCustomerName|SubscriptionID|MSISDN|ExternalInvoiceID|InvoiceNumber|ResultValue|R~esult" SKIP.

FOR EACH BRTestQResultRow NO-LOCK WHERE
         BRTestQResultRow.BRTestQResultID = BRTestQResult.BRTestQResultID:

   fTestCase(BRTestQResultRow.BRTestQRowID).
   
   FIND FIRST Customer WHERE Customer.CustNum = BRTestQResultRow.InvCust
         NO-LOCK NO-ERROR.
   IF AVAILABLE Customer THEN
      lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                    BUFFER Customer).

   FIND FIRST MobSub WHERE MobSub.MsSeq = BRTestQResultRow.MsSeq
      NO-LOCK NO-ERROR.
   IF AVAILABLE MobSub THEN lcCLI = MobSub.CLI.
   ELSE DO:
      FIND FIRST TermMobSub WHERE TermMobSub.MsSeq = BRTestQResultRow.MsSeq
         NO-LOCK NO-ERROR.
      IF AVAILABLE TermMobSub THEN lcCLI = TermMobSub.CLI.
   END.

   put stream sout unformatted
      BRTestQResultRow.BRTestQResultID "|"
      BRTestQResultRow.QResultRowID    "|"
      BRTestQResultRow.BRTestQRowID    "|"
      liTestCaseID                     "|"
      lcTestCase                       "|"
      BRTestQResultRow.InvCust         "|"
      lcCustName                       "|"
      BRTestQResultRow.MsSeq           "|"
      lcCLI                            "|"
      BRTestQResultRow.ExtInvID        "|"
      BRTestQResultRow.InvNum          "|"
      BRTestQResultRow.ResultValue     "|"
      BRTestQResultRow.TestResult      SKIP.
END. 

MESSAGE "Log file created" lcFile1 VIEW-AS ALERT-BOX.
