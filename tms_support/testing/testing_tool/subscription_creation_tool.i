/* ----------------------------------------------------------------------
  Module .......: testing_tool.i
  Task .........: Include file
  Application ..: TMS
  Author .......: Vikas
  Created ......: 03.07.12
  Version ......: Yoigo
---------------------------------------------------------------------- */
{Syst/commpaa.i}
gcBrand = "1".
katun = "Qvantel".

{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/fmakemsreq.i}
{Func/msreqfunc.i}
{Func/service.i}
{Func/fdss.i}
{Func/fbtc.i}
{Func/ftransdir.i}
{Func/email.i}


DEFINE VARIABLE lcOutPutDir        AS CHAR NO-UNDO.
DEFINE VARIABLE lcIncomingDir      AS CHAR NO-UNDO.
DEFINE VARIABLE lcDoneDir          AS CHAR NO-UNDO.
DEFINE VARIABLE lcMasterDataDir    AS CHAR NO-UNDO.
DEFINE VARIABLE lcAnalyzerDataDir  AS CHAR NO-UNDO.

DEFINE VARIABLE lcOutOngoingDir    AS CHAR NO-UNDO.
DEFINE VARIABLE lcOutProcDir       AS CHAR NO-UNDO.
DEFINE VARIABLE lcInIncomingDir    AS CHAR NO-UNDO.
DEFINE VARIABLE lcInProcDir        AS CHAR NO-UNDO.
DEFINE VARIABLE lcAnalyzerInDir    AS CHAR NO-UNDO.
DEFINE VARIABLE lcAnalyzerSpoolDir AS CHAR NO-UNDO.

DEFINE VARIABLE lcLogFile          AS CHAR NO-UNDO.
DEFINE VARIABLE lcReportFile       AS CHAR NO-UNDO.
DEFINE VARIABLE lcResultFile       AS CHAR NO-UNDO.
DEFINE VARIABLE lcFileName         AS CHAR NO-UNDO.
DEFINE VARIABLE lcLine             AS CHAR NO-UNDO.
DEFINE VARIABLE lcLineType         AS CHAR NO-UNDO.
DEFINE VARIABLE lcDel              AS CHAR NO-UNDO INIT "|".
DEFINE VARIABLE liLine             AS INT  NO-UNDO.
DEFINE VARIABLE llError            AS LOG  NO-UNDO.
DEFINE VARIABLE ldThisRun          AS DEC  NO-UNDO.
DEFINE VARIABLE liCustNum          AS INT  NO-UNDO.
DEFINE VARIABLE lcCLI              AS CHAR NO-UNDO.
DEFINE VARIABLE lcError            AS CHAR NO-UNDO.
DEFINE VARIABLE lcBonoList         AS CHAR NO-UNDO.
DEFINE VARIABLE lcBono             AS CHAR NO-UNDO.
DEFINE VARIABLE lcResult           AS CHAR NO-UNDO.
DEFINE VARIABLE liBonoCount        AS INT  NO-UNDO.
DEFINE VARIABLE liBonoEntries      AS INT  NO-UNDO.
DEFINE VARIABLE llFileAvail        AS LOG  NO-UNDO.
DEFINE VARIABLE llKeep             AS LOG  NO-UNDO.
DEFINE VARIABLE lcCONTDContracts   AS CHAR NO-UNDO.
DEFINE VARIABLE lcIPLContracts     AS CHAR NO-UNDO.
DEFINE VARIABLE lcFLATContracts    AS CHAR NO-UNDO.
DEFINE VARIABLE lcCONTSContracts   AS CHAR NO-UNDO.
DEFINE VARIABLE lcCONTSFContracts  AS CHAR NO-UNDO.
DEFINE VARIABLE lcBundleCLITypes   AS CHAR NO-UNDO.
DEFINE VARIABLE lcBONOContracts    AS CHAR NO-UNDO.
DEFINE VARIABLE lcLockFile         AS CHAR NO-UNDO.

DEF STREAM sOrder.
DEF STREAM sOrderCust.
DEF STREAM sFile.
DEF STREAM sInputFile.
DEF STREAM sOutput.
DEF STREAM sReport.

DEFINE TEMP-TABLE ttBatchInputFile
   FIELD FileName  AS CHAR
   FIELD ttUserId  AS CHAR
   FIELD EmailId   AS CHAR
   FIELD Valid     AS LOG
   FIELD AnalyzerReport  AS LOG
   FIELD OutputFileName  AS CHAR
   FIELD DeliverFileName AS CHAR
   FIELD MsisdnStatus    AS INT
   FIELD SimIcc          AS CHAR
   FIELD UsedMSISDN      AS CHAR
   INDEX FileName IS PRIMARY UNIQUE FileName.

DEFINE TEMP-TABLE ttInputFileContent
   FIELD FileName   AS CHAR
   FIELD LineNo     AS INT
   FIELD InputLine  AS CHAR
   FIELD LineType   AS CHAR
   FIELD CustIDType AS CHAR
   FIELD TestList   AS CHAR
   FIELD Qty        AS INT
   FIELD ActDate    AS DATE
   INDEX FileNameNo FileName LineNo.

DEFINE TEMP-TABLE ttSubscription
   FIELD FileName   AS CHAR
   FIELD CLI        AS CHAR
   FIELD MsSeq      AS INT
   FIELD OrderId    AS INT
   FIELD CustNum    AS INT
   FIELD CustIdType AS CHAR
   FIELD CustId     AS CHAR
   FIELD CLIType    AS CHAR
   FIELD Handled    AS LOG
   FIELD EmailId    AS CHAR
   INDEX CLI IS PRIMARY UNIQUE CLI.

DEF TEMP-TABLE ttOrder    NO-UNDO LIKE Order.
DEF TEMP-TABLE ttOrderCustomer NO-UNDO LIKE OrderCustomer.
DEFINE BUFFER bttInputFileContent FOR ttInputFileContent.


FUNCTION fLogEntry RETURNS LOG (INPUT icLine AS CHAR,
                                INPUT icRemark AS CHAR):
   PUT STREAM sOutput UNFORMATTED icLine lcDel icRemark SKIP.
END FUNCTION.

FUNCTION fLoadOrder RETURNS LOG (INPUT icFileName AS CHAR):

   INPUT STREAM sOrder FROM VALUE(icFileName).
   REPEAT:
      CREATE ttOrder.
      IMPORT STREAM sOrder ttOrder.
   END.
   INPUT STREAM sOrder CLOSE.

   RETURN TRUE.
END FUNCTION.

FUNCTION fLoadOrderCustomer RETURNS LOG (INPUT icFileName AS CHAR):

   INPUT STREAM sOrderCust FROM VALUE(icFileName).
   REPEAT:
      CREATE ttOrderCustomer.
      IMPORT STREAM sOrderCust ttOrderCustomer.
   END.
   INPUT STREAM sOrderCust CLOSE.

   RETURN TRUE.
END FUNCTION.

FUNCTION fCheckMSISDN RETURNS LOG (INPUT iiStatus_MSISDN AS INT,
                                   INPUT icUsedMSISDN    AS CHAR ):

   IF icUsedMSISDN > "" THEN DO:
      FIND FIRST MSISDN EXCLUSIVE-LOCK WHERE
                 MSISDN.Brand = gcBrand AND
                 MSISDN.CLI   = icUsedMSISDN AND   /* Search with given MSISDN number */
                 MSISDN.ValidTo GE fMakeTS() AND
                 MSISDN.StatusCode EQ iiStatus_MSISDN NO-WAIT NO-ERROR. /* Normal or EMA */
   END.
   ELSE DO: /* Find first free */
      FIND FIRST MSISDN EXCLUSIVE-LOCK WHERE
                 MSISDN.Brand = gcBrand AND
                 MSISDN.ValidTo GE fMakeTS() AND
                 MSISDN.StatusCode EQ iiStatus_MSISDN NO-WAIT NO-ERROR. /* Normal or EMA */
   END.
   IF NOT AVAILABLE MSISDN THEN
      RETURN FALSE.
   ELSE RETURN TRUE.
END.

FUNCTION fCheckSIM RETURNS LOG (INPUT icSimIcc AS CHAR):

   DEFINE VARIABLE  llContinue  AS LOGICAL NO-UNDO INIT FALSE.

   IF icSimIcc > "" THEN DO:
      FIND FIRST SIM EXCLUSIVE-LOCK WHERE
                 SIM.ICC   EQ icSimIcc AND   /* Search with given ICC number */
                 SIM.Brand EQ gcBrand AND
                 SIM.SimStat EQ 1 NO-WAIT NO-ERROR.

      IF AVAILABLE SIM THEN   /* If stock not correct then ask confirmation */
         IF NOT (SIM.Stock EQ "TESTING" OR
                 SIM.Stock EQ "EMATESTING") THEN DO:
            MESSAGE "Incorrect SIM Stock (" + SIM.Stock + "). Change stock to Testing?" 
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            SET llContinue.
            IF NOT llContinue THEN DO:
               RELEASE SIM.
               RETURN FALSE.
            END.
            IF llContinue THEN ASSIGN SIM.Stock = "TESTING".
         END.
   END.
   ELSE DO:
      FIND FIRST SIM EXCLUSIVE-LOCK WHERE
                 SIM.Brand EQ gcBrand AND
                (SIM.Stock EQ "TESTING" OR
                 SIM.Stock EQ "EMATESTING") AND
                 SIM.SimStat EQ 1 NO-WAIT NO-ERROR.
   END.
   IF NOT AVAILABLE SIM THEN
      RETURN FALSE.
   ELSE RETURN TRUE.
END.

FUNCTION fUpdateMSISDN RETURNS LOGICAL:

   FIND CURRENT MSISDN EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

   ASSIGN MSISDN.StatusCode = 2
          MSISDN.OrderId    = Order.OrderId.

   RELEASE MSISDN.

   RETURN TRUE.
END.

FUNCTION fUpdateSIM RETURNS LOGICAL:

   FIND CURRENT SIM EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   SIM.simstat = 4.

   RELEASE SIM.

   RETURN TRUE.
END.

FUNCTION fCreateOrder RETURNS CHAR (INPUT icIdType       AS CHAR,
                                    INPUT icCLIType      AS CHAR,
                                    INPUT icUserId       AS CHAR,
                                    INPUT ilOwnCust      AS LOG,
                                    INPUT icOfferId      AS CHAR,
                                    INPUT iiMsisdnStatus AS INT,
                                    INPUT icSimIcc       AS CHAR,
                                    INPUT icMSISDN       AS CHAR,
                                    OUTPUT ocCLI         AS CHAR,
                                    OUTPUT oiOrderId     AS INT):

   DEFINE VARIABLE lcCLIType    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCustId     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcCustIdType AS CHARACTER NO-UNDO. 

   IF NOT ilOwnCust THEN DO:
      FIND FIRST ttOrderCustomer WHERE ttOrderCustomer.CustIdType = icIdType NO-ERROR.
      IF NOT AVAIL ttOrderCustomer THEN
         RETURN "Dummy OrderCustomer record not found for " + icIdType.
      ASSIGN
        lcCustidType = ttOrderCustomer.CustIdType
        lcCustId     = ttOrderCustomer.CustId.
   END.
   ELSE DO:
      ASSIGN
         lcCustIdType = Customer.CustIdType
         lcCustId     = Customer.OrgId.
   END.

   FIND FIRST ttOrder WHERE ttOrder.OrderId > 0 NO-ERROR.
   IF NOT AVAIL ttOrder THEN RETURN "Dummy Order record not found".

   IF LOOKUP(icCLIType,lcCONTDContracts) > 0       THEN lcCLIType = "CONTD".
   ELSE IF LOOKUP(icCLIType,lcIPLContracts) > 0    THEN lcCLIType = "CONTRD".
   ELSE IF LOOKUP(icCLIType,lcCONTSContracts) > 0  THEN lcCLIType = "CONTS".
   ELSE IF LOOKUP(icCLIType,lcFLATContracts) > 0   THEN lcCLIType = "CONTF".
   ELSE IF LOOKUP(icCLIType,lcCONTSFContracts) > 0 THEN lcCLIType = "CONTSF".
   ELSE IF LOOKUP(icCLIType,lcBONOContracts) > 0   THEN lcCLIType = "PMDUB".
   ELSE lcCLIType = icCLIType.

   FIND FIRST CLIType WHERE
              CLIType.CLIType = lcCLIType NO-LOCK NO-ERROR.
   IF NOT AVAIL CLIType THEN RETURN "Invalid CLIType is specified".

   IF NOT fCheckMSISDN(INPUT iiMsisdnStatus, INPUT icMSISDN) THEN RETURN "MSISDN is not available or free".
   IF NOT fCheckSIM(INPUT icSimIcc) THEN RETURN "SIM is not available or free".

   DO TRANS:
      CREATE Order.
      BUFFER-COPY ttOrder EXCEPT OrderId MsSeq Salesman TO Order.

      ASSIGN
         Order.Brand           = gcBrand
         Order.OrderId         = NEXT-VALUE(OrderId)
         Order.CrStamp         = fMakeTS()
         Order.StatusCode      = "1"
         Order.CLI             = MSISDN.CLI
         Order.CLIType         = lcCLIType
         Order.PayType         = (IF CLIType.PayType = 2 THEN TRUE ELSE FALSE)
         Order.ICC             = SIM.ICC
         Order.OrderChannel    = "vip"
         Order.MsSeq           = NEXT-VALUE(MobSub)
         Order.CustNum         = 0
         Order.OrderType       = 0
         Order.ContractID      = SUBSTRING(STRING(Order.OrderId),5,4)
         Order.Salesman        = icUserId
         Order.Offer           = icOfferId.

      ASSIGN oiOrderId = Order.OrderId
             ocCLI     = Order.CLI.

      IF NOT fUpdateMSISDN() THEN
         UNDO, RETURN "MSISDN is not available or free".
      IF NOT fUpdateSIM() THEN
         UNDO, RETURN "SIM is not available or free".
   END. /* DO TRANS: */

   IF icCLIType NE lcCLIType THEN
      fCreateOrderAction(Order.Orderid,"BundleItem",icCLIType,"").

   IF NOT CAN-FIND(FIRST ttSubscription WHERE
                         ttSubscription.CLI = Order.CLI) THEN DO:
      CREATE ttSubscription.
      ASSIGN ttSubscription.CLI        = Order.CLI
             ttSubscription.MsSeq      = Order.MsSeq
             ttSubscription.OrderId    = Order.OrderId
             ttSubscription.CLIType    = icCLIType
             ttSubscription.CustIdType = lcCustIdType
             ttSubscription.CustId     = lcCustId
             ttSubscription.FileName   = ttInputFileContent.FileName
             ttSubscription.EmailId    = ttBatchInputFile.EmailId.
   END. /* IF NOT CAN-FIND(FIRST ttSubscription WHERE */

   RETURN "".

END. /* FUNCTION fCreateOrder RETURNS LOGICAL: */

FUNCTION fCreateOrderCustomer RETURNS CHAR (INPUT iiOrderId     AS INT,
                                            INPUT icIdType      AS CHAR,
                                            INPUT ilOwnCustomer AS LOG):
   IF NOT ilOwnCustomer THEN DO:
      FIND FIRST ttOrderCustomer WHERE ttOrderCustomer.CustIdType = icIdType NO-ERROR.
      IF NOT AVAIL ttOrderCustomer THEN
         RETURN "Dummy OrderCustomer record not found for " + icIdType.

      CREATE OrderCustomer.
      BUFFER-COPY ttOrderCustomer EXCEPT OrderId CustNum TO OrderCustomer.
   END.

   ELSE DO:
      CREATE OrderCustomer.
      BUFFER-COPY Customer EXCEPT Language TO OrderCustomer.
      ASSIGN
         OrderCustomer.CustId = Customer.OrgId.
   END.

   ASSIGN
      OrderCustomer.Brand   = gcBrand
      OrderCustomer.OrderId = iiOrderId
      OrderCustomer.RowType = 1.

   RELEASE OrderCustomer.
   RELEASE Order.

   RETURN "".
END.

FUNCTION fCreateOrderTopup RETURNS LOGICAL:
   DEFINE VARIABLE lCreate AS LOGICAL NO-UNDO.
   lCreate = FALSE.
   CREATE OrderTopup.
   ASSIGN
      OrderTopup.Amount = 20.00
      OrderTopup.Brand = gcBrand
      OrderTopup.OrderId = Order.OrderId.
   lCreate = TRUE.
   RETURN lCreate.
END.



PROCEDURE pHandleOtherRows:

   DEF INPUT PARAMETER llFinal   AS LOG NO-UNDO.
   DEF INPUT PARAMETER llNewSubs AS LOG NO-UNDO.

   FILE_LOOP:
   FOR EACH ttBatchInputFile WHERE ttBatchInputFile.Valid = TRUE:

      lcLogFile = lcOutOngoingDir + "/" + ttBatchInputFile.FileName +
                  "_POST_" + STRING(ldThisRun) + ".LOG".
      OUTPUT STREAM sOutput TO VALUE(lcLogFile) APPEND.

      FILE_CONTENT_LOOP:
      FOR EACH ttInputFileContent WHERE
               ttInputFileContent.FileName = ttBatchInputFile.FileName AND
               ttInputFileContent.LineType <> "SUBSCRIPTION"
               BY ttInputFileContent.LineNo:

         /* Exclude Bono activation with new subscription
            because it is added by OrderAction */
         IF llNewSubs AND ttInputFileContent.LineType = "ACT_BONO"
         THEN NEXT FILE_CONTENT_LOOP.

         /* skip header line and no test case */
         IF ttInputFileContent.InputLine = "" OR
            ttInputFileContent.InputLine BEGINS "H" THEN NEXT FILE_CONTENT_LOOP.
         ELSE IF ttInputFileContent.TestList = "" THEN DO:
            PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.
            NEXT FILE_CONTENT_LOOP.
         END.

         CASE ttInputFileContent.LineType:
            WHEN "ACT_BONO" THEN RUN pActBono.
            WHEN "ACT_OTHER_BUNDLE" THEN RUN pActContract.
            WHEN "DEACT_OTHER_BUNDLE" OR WHEN "DEACT_BONO" THEN RUN pDeactContract.
            WHEN "ACT_SERVICE" THEN RUN pService(INPUT "Activation").
            WHEN "DEACT_SERVICE" THEN RUN pService(INPUT "Deactivation").
            WHEN "STC" THEN RUN pSTC.
            WHEN "BTC" THEN RUN pBTC.
            WHEN "SEGMENT_CODE" THEN RUN pUpdateSegment.
         END CASE.
      END. /* FOR EACH ttInputFileContent WHERE */

      /* Close Output log file */
      OUTPUT STREAM sOutput CLOSE.

      /* Moved output file to processed directory */
      IF llFinal THEN
         fTransDir(lcLogFile,
                   "",
                   lcOutProcDir).

      /* Delete all handled subscriptions */
      FOR EACH ttSubscription WHERE
               ttSubscription.FileName = ttBatchInputFile.FileName AND
               ttSubscription.Handled = TRUE:
         DELETE ttSubscription.
      END. /* FOR EACH ttSubscription WHERE */

   END. /* FOR EACH ttBatchInputFile NO-LOCK */

END PROCEDURE. /* PROCEDURE pHandleOtherRows: */

PROCEDURE pActBono:

   DEF VAR liContractEntries  AS INT  NO-UNDO.
   DEF VAR liRequest          AS INT  NO-UNDO.
   DEF VAR lcContractID       AS CHAR NO-UNDO.
   DEF VAR lcRemark           AS CHAR NO-UNDO.
   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR ldActTS            AS DEC  NO-UNDO.
   DEF VAR liSubCount         AS INT  NO-UNDO.
   DEF VAR liBonoCount        AS INT  NO-UNDO.

   DEF BUFFER bMsRequest      FOR MsRequest.

   liContractEntries = NUM-ENTRIES(ttInputFileContent.TestList).

   FOR EACH ttSubscription WHERE
            ttSubscription.FileName = ttInputFileContent.FileName AND
            ttSubscription.CustNum > 0,
      FIRST MobSub WHERE MobSub.MsSeq = ttSubscription.MsSeq NO-LOCK:

      ASSIGN ttSubscription.Handled = TRUE
             ldActTS  = fMakeTS()
             lcRemark = ""
             liSubCount = liSubCount + 1
             liBonoCount = liBonoCount + 1.

      IF liSubCount = 1 THEN
         PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.

      /* Bono Activation is not allowed for IPL */
      IF (ttSubscription.CLIType <> "CONTD"  AND
          ttSubscription.CLIType <> "CONTD9" AND
          ttSubscription.CLIType BEGINS "CONTD") THEN DO:
         fLogEntry(ttSubscription.CLI,"BONO is not allowed").
         NEXT.
      END. /* IF (ttSubscription.CLIType <> "CONTD"  AND */

      IF liContractEntries > 0 THEN DO:
         IF liBonoCount > liContractEntries THEN liBonoCount = 1.
            lcContractID = ENTRY(liBonoCount,ttInputFileContent.TestList).
      END. /* IF liBonoEntries > 0 THEN DO: */

      IF lcContractID > "" THEN DO:
         ASSIGN
            liRequest    = 0
            lcError      = ""
            lcRemark = lcRemark + "," + lcContractID.

         /* activate new periodical contract */
         liRequest = fPCActionRequest(ttSubscription.MsSeq,
                                      lcContractID, 
                                      "act",
                                      ldActTS,
                                      TRUE,   /* create fee */
                                      {&REQUEST_SOURCE_SCRIPT},
                                      "",
                                      0,
                                      FALSE,
                                      "",
                                      0, /* two_installment */
                                      0,
                                      OUTPUT lcError).
         IF liRequest = 0 THEN
            lcRemark = lcRemark + "-Activation failed " + lcError.
         ELSE
            lcRemark = lcRemark + "-Activation request created".
      END. /* IF lcContractID > "" THEN DO: */

      lcRemark = TRIM(lcRemark,",").
      fLogEntry(ttSubscription.CLI,lcRemark).

   END. /* FOR EACH ttSubscription WHERE */

END PROCEDURE. /* PROCEDURE pActBono: */

PROCEDURE pActContract:

   DEF VAR liContractCount    AS INT  NO-UNDO.
   DEF VAR liContractEntries  AS INT  NO-UNDO.
   DEF VAR liRequest          AS INT  NO-UNDO.
   DEF VAR lcContractID       AS CHAR NO-UNDO.
   DEF VAR lcRemark           AS CHAR NO-UNDO.
   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR ldActTS            AS DEC  NO-UNDO.
   DEF VAR liSubCount         AS INT  NO-UNDO.

   DEF BUFFER bMsRequest      FOR MsRequest.

   FOR EACH ttSubscription WHERE
            ttSubscription.FileName = ttInputFileContent.FileName AND
            ttSubscription.CustNum > 0,
      FIRST MobSub WHERE MobSub.MsSeq = ttSubscription.MsSeq NO-LOCK:

      ASSIGN ttSubscription.Handled = TRUE
             ldActTS  = fMakeTS()
             lcRemark = ""
             liContractCount = 0
             liSubCount = liSubCount + 1.

      IF liSubCount = 1 THEN
         PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.

      DO liContractCount = 1 TO NUM-ENTRIES(ttInputFileContent.TestList):
         ASSIGN
            liRequest    = 0
            lcError      = ""
            lcContractID = ENTRY(liContractCount,ttInputFileContent.TestList).

         IF lcContractID = "DSS" AND
            (fOngoingDSSAct(INPUT ttSubscription.CustNum) OR
             fIsDSSActive(INPUT ttSubscription.CustNum, INPUT ldActTS))
         THEN lcRemark = lcRemark + "," +
                         "DSS activation is ongoing or already active".
         ELSE DO:
            lcRemark = lcRemark + "," + lcContractID.

            /* activate new periodical contract */
            liRequest = fPCActionRequest(ttSubscription.MsSeq,
                                         lcContractID, 
                                         "act",
                                         ldActTS,
                                         TRUE,   /* create fee */
                                         {&REQUEST_SOURCE_SCRIPT},
                                         "",
                                         0,
                                         FALSE,
                                         "",
                                         0,
                                         0, /* two_installment */
                                         OUTPUT lcError).
            IF liRequest = 0 THEN
               lcRemark = lcRemark + "-Activation failed " + lcError.
            ELSE DO:
               lcRemark = lcRemark + "-Activation request created".

               IF lcContractID = "SPOTIFY" THEN DO:
                  FIND FIRST bMsRequest WHERE
                             bMsRequest.MsRequest = liRequest
                       EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAIL bMsRequest THEN
                     bMsRequest.ReqCparam4 = katun + "|" + STRING(TODAY,"99-99-9999").
               END. /* IF lcContractID = "SPOTIFY" THEN DO: */
            END. /* ELSE DO: */
         END. /* ELSE DO: */
      END. /* DO liContractCount = 1 TO */

      lcRemark = TRIM(lcRemark,",").
      fLogEntry(ttSubscription.CLI,lcRemark).

   END. /* FOR EACH ttSubscription WHERE */

END PROCEDURE. /* PROCEDURE pActContract: */

PROCEDURE pDeactContract:

   DEF VAR liContractCount    AS INT  NO-UNDO.
   DEF VAR liContractEntries  AS INT  NO-UNDO.
   DEF VAR liRequest          AS INT  NO-UNDO.
   DEF VAR lcContractID       AS CHAR NO-UNDO.
   DEF VAR lcRemark           AS CHAR NO-UNDO.
   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR llError            AS LOG  NO-UNDO.
   DEF VAR ldActTS            AS DEC  NO-UNDO.
   DEF VAR liSubCount         AS INT  NO-UNDO.

   FOR EACH ttSubscription WHERE
            ttSubscription.FileName = ttInputFileContent.FileName AND
            ttSubscription.CustNum > 0,
      FIRST MobSub WHERE MobSub.MsSeq = ttSubscription.MsSeq NO-LOCK:

      ASSIGN ttSubscription.Handled = TRUE
             ldActTS  = fSecOffSet(fMakeTS(),120) /* 2 mins gap */
             lcRemark = ""
             liSubCount = liSubCount + 1.

      IF liSubCount = 1 THEN
         PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.

      DO liContractCount = 1 TO NUM-ENTRIES(ttInputFileContent.TestList):
         ASSIGN
            liRequest    = 0
            llError      = FALSE
            lcError      = ""
            ldActTS  = fSecOffSet(fMakeTS(),120) /* 2 mins gap */
            lcContractID = ENTRY(liContractCount,ttInputFileContent.TestList).

         IF lcContractID = "BONO" THEN DO:
            lcContractID = fGetCurrentSpecificBundle(INPUT ttSubscription.MsSeq,
                                                     INPUT "BONO").

            ldActTS = fMake2Dt(fLastDayOfMonth(TODAY),86399).

            IF lcContractID = "" THEN DO:
               FIND FIRST OrderAction WHERE
                          OrderAction.Brand = gcBrand AND
                          OrderAction.OrderId = ttSubscription.OrderId AND
                          OrderAction.ItemType = "BundleItem" AND
                          OrderAction.ItemKey BEGINS "MDUB" NO-LOCK NO-ERROR.
               IF NOT AVAIL OrderAction THEN
                  ASSIGN llError = TRUE
                         lcRemark = lcRemark + "," +
                                    "Subscription does not have any active bono".
               ELSE lcContractID = OrderAction.ItemKey.
            END. /* IF lcContractID = "" THEN DO: */
         END. /* IF lcContractID = "BONO" THEN DO: */

         IF NOT llError THEN DO:
            lcRemark = lcRemark + "," + lcContractID.

            IF LOOKUP(lcContractID,"DSS,BONO_VOIP,SPOTIFY") > 0 THEN
               ldActTS = fMake2Dt(fLastDayOfMonth(TODAY),86399).

            /* terminate periodical contract */
            liRequest = fPCActionRequest(ttSubscription.MsSeq,
                                         lcContractID, 
                                         "term",
                                         ldActTS,
                                         TRUE,   /* create fee */
                                         {&REQUEST_SOURCE_SCRIPT},
                                         "",
                                         0,
                                         FALSE,
                                         "",
                                         0,
                                         0, /* two_installment */
                                         OUTPUT lcError).
            IF liRequest = 0 THEN
               lcRemark = lcRemark + "-Deactivation failed " + lcError.
            ELSE lcRemark = lcRemark + "-Deactivation request created".
         END. /* ELSE DO: */
      END. /* DO liContractCount = 1 TO */

      lcRemark = TRIM(lcRemark,",").
      fLogEntry(ttSubscription.CLI,lcRemark).

   END. /* FOR EACH ttSubscription WHERE */

END PROCEDURE. /* PROCEDURE pDeactContract: */

PROCEDURE pService:

   DEF INPUT PARAMETER icAction AS CHAR NO-UNDO.

   DEF VAR liContractCount    AS INT  NO-UNDO.
   DEF VAR liContractEntries  AS INT  NO-UNDO.
   DEF VAR liRequest          AS INT  NO-UNDO.
   DEF VAR lcContractID       AS CHAR NO-UNDO.
   DEF VAR lcRemark           AS CHAR NO-UNDO.
   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR ldActTS            AS DEC  NO-UNDO.
   DEF VAR liSubCount         AS INT  NO-UNDO.

   DEF BUFFER bMsRequest      FOR MsRequest.

   FOR EACH ttSubscription WHERE
            ttSubscription.FileName = ttInputFileContent.FileName AND
            ttSubscription.CustNum > 0,
      FIRST MobSub WHERE MobSub.MsSeq = ttSubscription.MsSeq NO-LOCK:

      ASSIGN ttSubscription.Handled = TRUE
             lcRemark = ""
             liSubCount = liSubCount + 1.

      IF icAction = "Activation" THEN
         ldActTS = fMakeTS().
      ELSE
         ldActTS = fSecOffSet(fMakeTS(),120). /* 2 mins gap */

      IF liSubCount = 1 THEN
         PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.
 
      DO liContractCount = 1 TO NUM-ENTRIES(ttInputFileContent.TestList):
         ASSIGN
            liRequest    = 0
            lcError      = ""
            lcContractID = ENTRY(liContractCount,ttInputFileContent.TestList).

         IF lcContractID <> "BB" THEN
            lcRemark = lcRemark + "," +
                       "No support for other services than BB".
         ELSE IF icAction = "Activation" THEN DO:
            FIND FIRST SubSer WHERE
                       SubSer.ServCom =  lcContractID AND
                       SubSer.MsSeq   =  ttSubscription.MsSeq AND
                       SubSer.SsDate  <= TODAY NO-LOCK NO-ERROR.
            IF AVAIL SubSer AND SubSer.SSstat = 1 THEN
               lcRemark = lcRemark + "," +
                          lcContractID + " service is already active".
         END. /* ELSE IF icAction = "Activation" THEN DO: */
         ELSE IF icAction = "Deactivation" THEN DO:
            FIND FIRST SubSer WHERE
                       SubSer.ServCom =  lcContractID AND
                       SubSer.MsSeq   =  ttSubscription.MsSeq AND
                       SubSer.SsDate  <= TODAY NO-LOCK NO-ERROR.
            IF AVAIL SubSer AND
               (SubSer.SSstat = 0 OR SubSer.SSstat = 2) THEN
               lcRemark = lcRemark + "," +
                          lcContractID + " service is already suspended".
            ELSE IF
               NOT CAN-FIND(FIRST bMsRequest WHERE
                            bMsRequest.MsSeq      = ttSubscription.MsSeq AND
                            bMsRequest.ReqType    = 1    AND
                            bMsRequest.ReqCParam1 = "BB" AND
                   LOOKUP(STRING(bMsRequest.ReqStatus),"2,4,9,99,3") = 0 AND
                            bMsRequest.ReqIparam1 = 1) THEN
               lcRemark = lcRemark + "," +
                          lcContractID + " service is not active".
         END. /* ELSE IF icAction = "Deactivation" THEN DO: */

         IF lcRemark = "" THEN DO:
            lcRemark = lcRemark + "," + lcContractID.

            /* activate BB service */
            liRequest = fServiceRequest(ttSubscription.MsSeq,
                                        lcContractID,
                                        (IF icAction = "Activation" THEN 1 ELSE 2),/* ON/Suspend */
                                        "",
                                        ldActTS,
                                        "",
                                        FALSE, /* fees */
                                        FALSE, /* sms */
                                        "",
                                        {&REQUEST_SOURCE_SCRIPT},
                                        0, /* father request */
                                        FALSE,
                                        OUTPUT lcError).
            IF liRequest = 0 THEN
               lcRemark = lcRemark + "-" + icAction + " failed " + lcError.
            ELSE lcRemark = lcRemark + "-" + icAction + " request created".
         END. /* IF lcRemark = "" THEN DO: */
      END. /* DO liContractCount = 1 TO */

      lcRemark = TRIM(lcRemark,",").
      fLogEntry(ttSubscription.CLI,lcRemark).

   END. /* FOR EACH ttSubscription WHERE */
END PROCEDURE. /* PROCEDURE pService: */

PROCEDURE pSTC:

   DEF VAR liRequest          AS INT  NO-UNDO.
   DEF VAR lcNewCLIType       AS CHAR NO-UNDO.
   DEF VAR lcRemark           AS CHAR NO-UNDO.
   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR ldActTS            AS DEC  NO-UNDO.
   DEF VAR liSubCount         AS INT  NO-UNDO.
   DEF VAR liSTCCount         AS INT  NO-UNDO.
   DEF VAR liSTCEntries       AS INT  NO-UNDO.
   DEF VAR lcDataBundleId     AS CHAR NO-UNDO.
   DEF VAR liCreditcheck      AS INT  NO-UNDO.

   DEF BUFFER NewCLIType      FOR CLIType.

   liSTCEntries = NUM-ENTRIES(ttInputFileContent.TestList).

   FOR EACH ttSubscription WHERE
            ttSubscription.FileName = ttInputFileContent.FileName AND
            ttSubscription.CustNum > 0,
      FIRST MobSub WHERE MobSub.MsSeq = ttSubscription.MsSeq NO-LOCK:

      ASSIGN ttSubscription.Handled = TRUE
             lcRemark = ""
             liSubCount = liSubCount + 1
             liSTCCount = liSTCCount + 1
             liRequest      = 0
             lcError        = ""
             lcDataBundleId = ""
             liCreditcheck  = 1.

      IF liSubCount = 1 THEN
         PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.

      FIND Customer NO-LOCK WHERE
           Customer.CustNum = ttSubscription.CustNum NO-ERROR.

      IF liSTCEntries > 0 THEN DO:
         IF liSTCCount > liSTCEntries THEN liSTCCount = 1.
            lcNewCLIType = ENTRY(liSTCCount,ttInputFileContent.TestList).
      END. /* IF liBonoEntries > 0 THEN DO: */

      IF LOOKUP(lcNewCLIType,lcCONTDContracts) > 0 THEN
         lcNewCLIType = "CONTD".
      ELSE IF LOOKUP(lcNewCLIType,lcIPLContracts) > 0 THEN
         ASSIGN lcDataBundleId = lcNewCLIType
                lcNewCLIType = "CONTRD".
      ELSE IF LOOKUP(lcNewCLIType,lcCONTSContracts) > 0 THEN
         ASSIGN lcDataBundleId = lcNewCLIType
                lcNewCLIType = "CONTS".
      ELSE IF LOOKUP(lcNewCLIType,lcFLATContracts) > 0 THEN
         ASSIGN lcDataBundleId = lcNewCLIType
                lcNewCLIType = "CONTF".
      ELSE IF LOOKUP(lcNewCLIType,lcCONTSFContracts) > 0 THEN
         ASSIGN lcDataBundleId = lcNewCLIType
                lcNewCLIType = "CONTSF".

      FIND FIRST NewCliType WHERE
                 NewCLIType.Brand   = gcBrand AND
                 NewCLIType.CLIType = lcNewCLIType NO-LOCK NO-ERROR.
      IF NOT AVAIL NewCLIType THEN
         lcRemark = lcRemark + "," + "Invalid CLIType specified " + lcNewCLIType.

      IF ttInputFileContent.ActDate = ? OR lcNewCLIType BEGINS "TARJ" OR
         MobSub.CLIType BEGINS "TARJ" THEN
         ldActTS = fMake2Dt((fLastDayOfMonth(TODAY) + 1),0).
      ELSE
         ldActTS = fMake2Dt(ttInputFileContent.ActDate,0).

      /* Set the katun to check correct barring */
      katun = "NewtonAd".
      /* Various validations */
      IF NOT fValidateMobTypeCh(MobSub.MsSeq,NewCLIType.CLIType,
                                ldActTS,FALSE,FALSE,0,"",OUTPUT lcError) THEN
      lcRemark = lcRemark + "," + lcError.

      /* Set the katun again with original username */
      katun = "Qvantel".
      IF fValidateNewCliType(NewCLIType.CLIType,lcDataBundleId,
                             TRUE,OUTPUT lcError) NE 0 THEN
        lcRemark = lcRemark + "," + lcError.

      IF NewCLIType.PayType = 2 OR Customer.CustIDType = "CIF" THEN
         liCreditcheck = 0.

      IF lcRemark = "" THEN DO:
         lcRemark = lcRemark + "," + NewCLIType.CLIType + lcDataBundleId.
         /* activate new periodical contract */
         liRequest = fCTChangeRequest(ttSubscription.MsSeq,
                                      NewCLIType.CLIType,
                                      lcDataBundleId,
                                      Customer.BankAcct,
                                      ldActTS,
                                      liCreditCheck,  /* 0 = Credit check ok */
                                      0, /*FALSE,*/
                                      "" /* pcSalesman */,
                                      FALSE,
                                      TRUE,
                                      katun,
                                      0,
                                      {&REQUEST_SOURCE_SCRIPT},
                                      0,
                                      0,
                                      "",
                                      OUTPUT lcError).
         IF liRequest = 0 THEN
            ASSIGN
               liSTCCount = liSTCCount - 1
               lcRemark = lcRemark + "-STC request creation failed " + lcError.
         ELSE lcRemark = lcRemark + "-STC request created".
      END. /* IF lcRemark = "" THEN DO: */

      lcRemark = TRIM(lcRemark,",").
      fLogEntry(ttSubscription.CLI,lcRemark).
   END. /* FOR EACH ttSubscription WHERE */
END PROCEDURE. /* PROCEDURE pSTC: */

PROCEDURE pBTC:

   DEF VAR liRequest          AS INT  NO-UNDO.
   DEF VAR lcRemark           AS CHAR NO-UNDO.
   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR lcErrorMsg         AS CHAR NO-UNDO.
   DEF VAR ldActTS            AS DEC  NO-UNDO.
   DEF VAR liSubCount         AS INT  NO-UNDO.
   DEF VAR lcNewBundle        AS CHAR NO-UNDO.
   DEF VAR lcOldBundle        AS CHAR NO-UNDO.
   DEF VAR lcBundleList       AS CHAR NO-UNDO.
   DEF VAR liBundleCount      AS INT  NO-UNDO.
   DEF VAR ldaActDate         AS DATE NO-UNDO.
   DEF VAR lcBundleType       AS CHAR NO-UNDO.
   DEF VAR liBTCCount         AS INT  NO-UNDO.
   DEF VAR liBTCEntries       AS INT  NO-UNDO.

   liBTCEntries = NUM-ENTRIES(ttInputFileContent.TestList).

   FOR EACH ttSubscription WHERE
            ttSubscription.FileName = ttInputFileContent.FileName AND
            ttSubscription.CustNum > 0,
      FIRST MobSub WHERE MobSub.MsSeq = ttSubscription.MsSeq NO-LOCK:

      ASSIGN ttSubscription.Handled = TRUE
             lcRemark = ""
             lcErrorMsg = ""
             liSubCount = liSubCount + 1
             liBTCCount = liBTCCount + 1.

      IF liSubCount = 1 THEN
         PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.

      IF liBTCEntries > 0 THEN DO:
         IF liBTCCount > liBTCEntries THEN liBTCCount = 1.
            lcBundleList = ENTRY(liBTCCount,ttInputFileContent.TestList).
      END. /* IF liBonoEntries > 0 THEN DO: */

      DO liBundleCount = 1 TO NUM-ENTRIES(lcBundleList,"+"):
         ASSIGN lcNewBundle = ENTRY(liBundleCount,lcBundleList,"+")
                liRequest   = 0
                lcOldBundle = ""
                lcError     = ""
                lcErrorMsg  = "".

         IF lcNewBundle = "" OR lcNewBundle = ? THEN
            ASSIGN lcErrorMsg = "New Bundle is not specified"
                   lcRemark   = lcRemark + "," + lcErrorMsg.

         IF LOOKUP(lcNewBundle,lcBONOContracts) > 0 THEN
            lcBundleType = "BONO".
         ELSE IF LOOKUP(lcNewBundle,lcIPLContracts) > 0 THEN
            lcBundleType = "CONTRD".
         ELSE IF LOOKUP(lcNewBundle,lcCONTSContracts) > 0 THEN
            lcBundleType = "CONTS".
         ELSE IF LOOKUP(lcNewBundle,lcCONTSFContracts) > 0 THEN
            lcBundleType = "CONTSF".
         ELSE IF LOOKUP(lcNewBundle,lcFLATContracts) > 0 THEN
            lcBundleType = "CONTF".
         ELSE
            ASSIGN lcErrorMsg = "Incorrect Bundle Id: " + lcNewBundle
                   lcRemark   = lcRemark + "," + lcErrorMsg.

         IF lcBundleType = "BONO" THEN
            lcOldBundle = fGetCurrentSpecificBundle(INPUT MobSub.MsSeq,
                                                    INPUT lcBundleType).
         ELSE
            lcOldBundle = MobSub.TariffBundle.

         IF lcOldBundle = "" OR lcOldBundle = ? THEN
            ASSIGN lcErrorMsg = "Old Bundle not found"
                   lcRemark   = lcRemark + "," + lcErrorMsg.

         IF ttInputFileContent.ActDate = ? OR lcBundleType = "BONO" THEN
            ldaActDate = (fLastDayOfMonth(TODAY) + 1).
         ELSE
            ldaActDate = ttInputFileContent.ActDate.

         ldActTS = fMake2Dt(ldaActDate,0).

         IF lcErrorMsg = ""  AND
            NOT fValidateBTC(MobSub.MsSeq,
                             lcOldBundle,
                             lcNewBundle,
                             ldaActDate,
                             MobSub.CLIType,
                             FALSE,
                             OUTPUT lcError) THEN
         ASSIGN lcErrorMsg = lcError
                lcRemark   = lcRemark + "," + lcErrorMsg.

         IF lcErrorMsg = "" THEN DO:
            lcRemark = lcRemark + "," + lcNewBundle.
            /* create BTC request */
            liRequest = fBundleChangeRequest(MobSub.MsSeq,
                                 lcOldBundle, 
                                 lcNewBundle,
                                 ldActTS,
                                 {&REQUEST_SOURCE_SCRIPT},
                                 "",    /* creator */
                                 FALSE, /* create fees */
                                 0,     /* orig. request */
                                 FALSE, /* mandatory */
                                 FALSE, /* Upgrade Upsell */
                                 0,
                                 "",
                                 OUTPUT lcError).

            IF liRequest = 0 THEN
               lcRemark = lcRemark + "-BTC request creation failed " + lcError.
            ELSE lcRemark = lcRemark + "-BTC request created".
         END. /* IF lcRemark = "" DO: */
      END. /* DO liBundleCount = 1 TO NUM-ENTRIES(lcBundleList,"+"): */

      lcRemark = TRIM(lcRemark,",").
      fLogEntry(ttSubscription.CLI,lcRemark).
   END. /* FOR EACH ttSubscription WHERE */
END PROCEDURE. /* PROCEDURE pBTC: */

PROCEDURE pUpdateSegment:

   DEF VAR liSegmentEntries   AS INT  NO-UNDO.
   DEF VAR lcSegmentCode      AS CHAR NO-UNDO.
   DEF VAR lcRemark           AS CHAR NO-UNDO.
   DEF VAR liSubCount         AS INT  NO-UNDO.
   DEF VAR liSegmentCount     AS INT  NO-UNDO.

   DEF BUFFER bMobSub FOR MobSub.

   liSegmentEntries = NUM-ENTRIES(ttInputFileContent.TestList).

   FOR EACH ttSubscription WHERE
            ttSubscription.FileName = ttInputFileContent.FileName AND
            ttSubscription.CustNum > 0,
      FIRST MobSub WHERE MobSub.MsSeq = ttSubscription.MsSeq NO-LOCK:

      ASSIGN ttSubscription.Handled = TRUE
             lcRemark = ""
             liSubCount = liSubCount + 1
             liSegmentCount = liSegmentCount + 1.

      IF liSubCount = 1 THEN
         PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.

      IF liSegmentEntries > 0 THEN DO:
         IF liSegmentCount > liSegmentEntries THEN liSegmentCount = 1.
            lcSegmentCode = ENTRY(liSegmentCount,ttInputFileContent.TestList).
      END. /* IF liBonoEntries > 0 THEN DO: */

      IF lcSegmentCode > "" THEN DO:
         lcRemark = lcRemark + "," + lcSegmentCode.
         FIND FIRST bMobSub WHERE
                    ROWID(bMobSub) = ROWID(MobSub)
              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF AVAIL bMobSub THEN
            ASSIGN bMobSub.SegmentOffer = lcSegmentCode
                   bMobSub.SegmentDate  = TODAY
                   lcRemark = lcRemark + "-Segment code is changed".
         RELEASE bMobSub.
      END. /* IF lcContractID > "" THEN DO: */

      lcRemark = TRIM(lcRemark,",").
      fLogEntry(ttSubscription.CLI,lcRemark).

   END. /* FOR EACH ttSubscription WHERE */

END PROCEDURE. /* PROCEDURE pUpdateSegment: */

