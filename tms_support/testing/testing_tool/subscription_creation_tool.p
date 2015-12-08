/* ----------------------------------------------------------------------
  Module .......: subscription_creation_ tool.p
  Task .........: STC/BTC and Contract Activation/Deactivation Testing Tool
                  to new subscription
  Application ..: TMS
  Author .......: Vikas
  Created ......: 03.07.12
  Version ......: Yoigo
---------------------------------------------------------------------- */

/* Include file */
{subscription_creation_tool.i}

{msisdn.i}
{forderstamp.i}
{orderfunc.i}

DEFINE VARIABLE liQty              AS INT  NO-UNDO.
DEFINE VARIABLE llSubsHeader       AS LOG  NO-UNDO.
DEFINE VARIABLE llDoubleCheck      AS LOG  NO-UNDO.
DEFINE VARIABLE liCount            AS INT  NO-UNDO.
DEFINE VARIABLE liActCount         AS INT  NO-UNDO.
DEFINE VARIABLE liOrderId          AS INT  NO-UNDO.
DEFINE VARIABLE lcMailSubject      AS CHAR NO-UNDO.
DEFINE VARIABLE lcEmailContent     AS CHAR NO-UNDO.
DEFINE VARIABLE liOrders           AS INT  NO-UNDO.
DEFINE VARIABLE liMobSubs          AS INT  NO-UNDO.
DEFINE VARIABLE liLoop             AS INT  NO-UNDO.
DEFINE VARIABLE liFiles            AS INT  NO-UNDO.
DEFINE VARIABLE lcTime             AS CHAR NO-UNDO.
DEFINE VARIABLE lcCustData         AS CHAR NO-UNDO.
DEFINE VARIABLE llOwnCustomer      AS LOG  NO-UNDO. 
DEFINE VARIABLE lcOfferId          AS CHAR NO-UNDO INIT "". 
DEFINE VARIABLE lcEmaMsisdn        AS CHAR NO-UNDO INIT "".


ASSIGN
   lcOutOngoingDir   = fCParam("TestingTool","OutOutgoingDir")
   lcOutProcDir      = fCParam("TestingTool","OutProcDir")
   lcInIncomingDir   = fCParam("TestingTool","InIncomingDir")
   lcInProcDir       = fCParam("TestingTool","InProcDir")
   lcMasterDataDir   = fCParam("TestingTool","InMasterDir")
   lcAnalyzerSpoolDir = fCParam("TestingTool","AnalyzerSpoolDir")
   lcAnalyzerInDir   = fCParam("TestingTool","AnalyzerInDir")
   xMailFrom         = fCparam("TestingTool","EmailFromAddress")
   xMailSubj         = fCparam("TestingTool","EmailSubject")
   lcLockFile        = fCparam("TestingTool","DaemonLockFile").

ASSIGN lcIPLContracts        = fCParamC("IPL_CONTRACTS")
       lcCONTDContracts      = fCParamC("CONTD_CONTRACTS")
       lcFLATContracts       = fCParamC("FLAT_CONTRACTS")
       lcCONTSContracts      = fCParamC("CONTS_CONTRACTS")
       lcCONTSFContracts     = fCParamC("CONTSF_CONTRACTS")
       lcBundleCLITypes      = fCParamC("BUNDLE_BASED_CLITYPES")
       lcBONOContracts       = fCParamC("BONO_CONTRACTS").

/* Main Block */

FOR FIRST InvText NO-LOCK WHERE
          InvText.Brand     = gcBrand       AND
          InvText.Target    = "EMAIL"       AND
          InvText.KeyValue  = "EmailConfTestingTool" AND
          InvText.Language  = 5             AND 
          InvText.FromDate <= TODAY         AND
          InvText.ToDate   >= TODAY:
   lcEmailContent = InvText.InvText.
END. /* FOR FIRST InvText NO-LOCK WHERE */

FORM
   SKIP(1)
   " Total Orders.......: " liOrders  FORMAT ">>>>>>>>>9" SKIP
   " Total Subscriptions: " liMobSubs FORMAT ">>>>>>>>>9" SKIP
   " Loops .............: " liLoop    FORMAT ">>>>>>>>>9" SKIP
   " Total Files Handled: " liFiles   FORMAT ">>>>>>>>>9" SKIP
   " Last Loop..........: " lcTime    FORMAT "X(20)"
   SKIP(1)
WITH
   CENTERED NO-LABELS TITLE " Test Subscription Creation Tool " WIDTH 50 ROW 8
FRAME frmMain.

/* Load Master data */
INPUT STREAM sFile THROUGH VALUE("ls -1 " + lcMasterDataDir + "/*.d").

FILE_LOOP:
REPEAT:
   IMPORT STREAM sFile UNFORMATTED lcResultFile.
   lcFileName = ENTRY(NUM-ENTRIES(lcResultFile,"/"),lcResultFile,"/").
   IF SEARCH(lcResultFile) NE ? THEN DO:
      IF lcFileName BEGINS "ordercustomer" THEN
         fLoadOrderCustomer(lcResultFile).
      ELSE IF lcFileName BEGINS "order" THEN
         fLoadOrder(lcResultFile).
      END. /* IF SEARCH(lcResultFile) NE ? THEN DO: */
END. /* REPEAT: */

INPUT STREAM sFile CLOSE.

IF lcLockFile = ? OR lcLockFile = "" THEN
   lcLockFile = "/tmp/test_subs_creation_daemon.lock".

UNIX SILENT VALUE("touch " + lcLockFile).

MAIN_LOOP:
DO WHILE TRUE
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

    IF RETRY THEN LEAVE.

    /* should I be running */
    FILE-INFO:FILE-NAME = lcLockFile.
    IF FILE-INFO:FILE-TYPE = ? THEN LEAVE.

    /* Empty Temp-table */
    EMPTY TEMP-TABLE ttBatchInputFile.
    EMPTY TEMP-TABLE ttInputFileContent.
    EMPTY TEMP-TABLE ttSubscription.

    ASSIGN liLoop      = liLoop + 1
           llFileAvail = FALSE
           ldThisRun   = fMakeTS().

    INPUT STREAM sFile THROUGH VALUE("ls -1 " + lcInIncomingDir + "/*.RPT").

    FILE_LOOP:
    REPEAT:

      IMPORT STREAM sFile UNFORMATTED lcResultFile.

      IF SEARCH(lcResultFile) NE ? THEN DO:
         ASSIGN liLine = 0
                llFileAvail = TRUE
                liFiles = liFiles + 1.

         CREATE ttBatchInputFile.
         ASSIGN
            ttBatchInputFile.FileName = REPLACE(ENTRY(NUM-ENTRIES(lcResultFile,"/"),
                                        lcResultFile,"/"),".RPT","")
             ttBatchInputFile.Valid    = TRUE
            ttBatchInputFile.MsisdnStatus = 99. /* Status code tells that this is for normal test MSISDN */.

         INPUT STREAM sInputFile FROM VALUE(lcResultFile).

         REPEAT:
            IMPORT STREAM sInputFile UNFORMATTED lcLine.
            liLine = liLine + 1.
            IF lcLine = "" THEN NEXT.

            CREATE ttInputFileContent.
            ASSIGN
               ttInputFileContent.LineNo    = liLine
               ttInputFileContent.InputLine = lcLine
               ttInputFileContent.FileName  = ttBatchInputFile.FileName.
         END. /* REPEAT: */
         INPUT STREAM sInputFile CLOSE.
      END. /* IF SEARCH(lcResultFile) NE ? THEN DO: */

      /* Moved input file to processed directory */
      fTransDir(lcResultFile,
                "",
                lcInProcDir).
    END. /* REPEAT: */

    INPUT STREAM sFile CLOSE.

    DISP
       liOrders
       liMobSubs
       liLoop
       liFiles
       STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"HH:MM:SS") @ lcTime
    WITH FRAME frmMain.
    PAUSE 0.

    IF NOT llFileAvail THEN DO:
       PAUSE 10 NO-MESSAGE.
       NEXT MAIN_LOOP.
    END. /* IF NOT llFileAvail THEN DO: */

    FILE_LOOP:
    FOR EACH ttBatchInputFile:

       lcLogFile = lcOutOngoingDir + "/" + ttBatchInputFile.FileName +
                   "_PRE_" + STRING(ldThisRun) + ".LOG".
       OUTPUT STREAM sOutput TO VALUE(lcLogFile).

       FILE_CONTENT_LOOP:
       FOR EACH ttInputFileContent WHERE
                ttInputFileContent.FileName = ttBatchInputFile.FileName
           BREAK BY ttInputFileContent.FileName
                 BY ttInputFileContent.LineNo:
          IF FIRST-OF(ttInputFileContent.FileName) THEN DO:
             ASSIGN llSubsHeader = TRUE
                    llError = FALSE
                    lcLineType = ""
                    liQty = 0
                    lcCustData = ""
                    llOwnCustomer = FALSE
                    lcOfferId = "".
          END. /* IF FIRST-OF(ttInputFileContent.FileName) THEN DO: */

          PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.

          /* skip header line */
          IF ttInputFileContent.InputLine = "" OR
             ttInputFileContent.InputLine BEGINS "H" THEN NEXT FILE_CONTENT_LOOP.

          IF NUM-ENTRIES(ttInputFileContent.InputLine,"|") >= 3 THEN
             lcLineType = ENTRY(2,ttInputFileContent.InputLine,"|").
          ELSE DO:
             llError = TRUE.
             fLogEntry(ttInputFileContent.InputLine,"Invalid input file format").
             LEAVE FILE_CONTENT_LOOP.
          END. /* ELSE DO: */

          IF llSubsHeader AND lcLineType <> "SUBSCRIPTION" THEN DO:
             llError = TRUE.
             fLogEntry(ttInputFileContent.InputLine,
                       "Invalid input file, first row must be subscription row").
             LEAVE FILE_CONTENT_LOOP.
          END. /* IF llSubsHeader AND lcLineType <> "SUBSCRIPTION" THEN DO: */

          CASE lcLineType:
             WHEN "SUBSCRIPTION" THEN DO:
                llSubsHeader = FALSE.
                IF NUM-ENTRIES(ttInputFileContent.InputLine,"|") < 5 THEN DO:
                   llError = TRUE.
                   fLogEntry(ttInputFileContent.InputLine,
                             "Invalid input file format, should be at least 5 enties").
                   LEAVE FILE_CONTENT_LOOP.
                END. /* IF NUM-ENTRIES(ttInputFileContent.InputLine,"|") < 5 */

                IF R-INDEX(ENTRY(3,ttInputFileContent.InputLine,"|"),"-") > 0 THEN DO:
                   ASSIGN
                     lcCustData = ENTRY(3,ttInputFileContent.InputLine,"|").
                     
                     FIND FIRST Customer NO-LOCK WHERE
                                Customer.OrgId = ENTRY(2,lcCustData,"-") NO-ERROR.
                     IF AVAIL Customer THEN DO:
                        ttInputFileContent.CustIDType = Customer.CustIdType.
                        llOwnCustomer = TRUE.
                     END.
                     ELSE ttInputFileContent.CustIDType = ENTRY(1,lcCustData,"-").
                END.
                ELSE DO:
                  ttInputFileContent.CustIDType = ENTRY(3,ttInputFileContent.InputLine,"|").
                  llOwnCustomer = FALSE.
                END.
                ASSIGN
                   ttInputFileContent.LineType   = lcLineType
                   ttInputFileContent.TestList   = ENTRY(4,ttInputFileContent.InputLine,"|")
                   ttInputFileContent.Qty        = INT(ENTRY(5,ttInputFileContent.InputLine,"|")).

                IF NUM-ENTRIES(ttInputFileContent.InputLine,"|") >= 6 THEN DO:
                  lcOfferId = ENTRY(6,ttInputFileContent.InputLine,"|").                  
                  IF lcOfferId > "" THEN DO:  /* Offer ID is not mandatory */
                     IF NOT CAN-FIND(FIRST Offer WHERE Offer.Offer = lcOfferId) THEN DO:
                        lcOfferId = "".
                        fLogEntry(ttInputFileContent.InputLine, "Invalid OfferId").
                        LEAVE FILE_CONTENT_LOOP.
                     END.
                  END.
                END.
                IF NUM-ENTRIES(ttInputFileContent.InputLine,"|") >= 7 THEN
                   ttBatchInputFile.ttUserId  = ENTRY(7,ttInputFileContent.InputLine,"|").
                IF NUM-ENTRIES(ttInputFileContent.InputLine,"|") >= 8 THEN
                   ttBatchInputFile.EmailId = ENTRY(8,ttInputFileContent.InputLine,"|").
                IF NUM-ENTRIES(ttInputFileContent.InputLine,"|") >= 9 THEN      /* For special MSISDN number */
                   ttBatchInputFile.UsedMSISDN = ENTRY(9,ttInputFileContent.InputLine,"|").
                IF NUM-ENTRIES(ttInputFileContent.InputLine,"|") >= 10 THEN      /* For special SIM number */
                   ttBatchInputFile.SimIcc = ENTRY(10,ttInputFileContent.InputLine,"|").
                IF NUM-ENTRIES(ttInputFileContent.InputLine,"|") >= 11 THEN DO:   /* For ADMIN user EMA testing only */
                   lcEmaMsisdn = ENTRY(11,ttInputFileContent.InputLine,"|").
                   IF lcEmaMsisdn > "" THEN DO:
                      IF lcEmaMsisdn NE "EMA" THEN DO:
                         llError = TRUE.
                         fLogEntry(ttBatchInputFile.FileName, lcEmaMsisdn + " Incorrect EMA format").
                         LEAVE FILE_CONTENT_LOOP.
                      END.
                      ELSE DO:
                         ttBatchInputFile.MsisdnStatus = 98.   /* Status code used for EMA subscribers */
                      END.
                   END.
                END.
             END. /* WHEN "SUBSCRIPTION" THEN DO: */
             WHEN "ACT_BONO" OR WHEN "DEACT_BONO" THEN DO:
                IF ENTRY(3,ttInputFileContent.InputLine,"|") > "" THEN
                   liQty = NUM-ENTRIES(ENTRY(3,ttInputFileContent.InputLine,"|"),",").

                FOR EACH bttInputFileContent WHERE
                         bttInputFileContent.FileName = ttInputFileContent.FileName AND
                         bttInputFileContent.LineType = "SUBSCRIPTION" AND
                         bttInputFileContent.Qty <= liQty:
                   bttInputFileContent.Qty = liQty.
                END. /* FOR EACH bttInputFileContent WHERE */

                ASSIGN ttInputFileContent.LineType = lcLineType
                       ttInputFileContent.TestList = ENTRY(3,ttInputFileContent.InputLine,"|").
             END.
             WHEN "STC" OR WHEN "BTC" THEN DO:
                IF ENTRY(3,ttInputFileContent.InputLine,"|") > "" THEN
                   liQty = NUM-ENTRIES(ENTRY(3,ttInputFileContent.InputLine,"|"),",").

                FOR EACH bttInputFileContent WHERE
                         bttInputFileContent.FileName = ttInputFileContent.FileName AND
                         bttInputFileContent.LineType = "SUBSCRIPTION" AND
                         bttInputFileContent.Qty <= liQty:
                   bttInputFileContent.Qty = liQty.
                END. /* FOR EACH bttInputFileContent WHERE */

                ASSIGN ttInputFileContent.LineType = lcLineType
                       ttInputFileContent.TestList = ENTRY(3,ttInputFileContent.InputLine,"|").

                IF NUM-ENTRIES(ttInputFileContent.InputLine,"|") >= 4 THEN
                   ttInputFileContent.ActDate = DATE(ENTRY(4,ttInputFileContent.InputLine,"|")) NO-ERROR.

             END.
             OTHERWISE DO:
                FOR EACH bttInputFileContent WHERE
                         bttInputFileContent.FileName = ttInputFileContent.FileName AND
                         bttInputFileContent.LineType = "SUBSCRIPTION" AND
                         bttInputFileContent.Qty = 0:
                   bttInputFileContent.Qty = 1.
                END. /* FOR EACH bttInputFileContent WHERE */

                ASSIGN ttInputFileContent.LineType = lcLineType
                       ttInputFileContent.TestList = ENTRY(3,ttInputFileContent.InputLine,"|").
             END.
          END CASE.
       END. /* FOR EACH ttInputFileContent WHERE */

       IF llError = FALSE THEN DO:
          IF NOT fCheckMSISDN(INPUT ttBatchInputFile.MsisdnStatus,
                              INPUT ttBatchInputFile.UsedMSISDN) THEN DO:
             fLogEntry(ttBatchInputFile.FileName,"MSISDN is not available or free").
             llError = TRUE.
          END.

          IF NOT fCheckSIM(INPUT ttBatchInputFile.SimIcc) THEN DO:
             fLogEntry(ttBatchInputFile.FileName,"SIM is not available or free").
             llError = TRUE.
          END.
       END.

       IF llError = TRUE THEN
          ASSIGN ttBatchInputFile.Valid = FALSE
                 ttBatchInputFile.DeliverFileName = lcOutProcDir + "/" + ttBatchInputFile.FileName +
                                                    "_PRE_" + STRING(ldThisRun) + ".LOG".

       /* Close Output log file */
       OUTPUT STREAM sOutput CLOSE.
       /* Moved output file to processed directory */
       fTransDir(lcLogFile,
                 "",
                 lcOutProcDir).

    END. /* FOR EACH ttBatchInputFile NO-LOCK */

    /* Now process actual data and create new orders */
    FILE_LOOP:
    FOR EACH ttBatchInputFile WHERE ttBatchInputFile.Valid = TRUE:

       ASSIGN
          liCount      = 0
          lcLogFile    = lcOutOngoingDir + "/" + ttBatchInputFile.FileName +
                         "_POST_" + STRING(ldThisRun) + ".LOG"
          lcReportFile = lcAnalyzerSpoolDir + "/" + ttBatchInputFile.FileName +
                         "_REPORT_" + STRING(ldThisRun) + ".LOG".

       OUTPUT STREAM sOutput TO VALUE(lcLogFile).
       OUTPUT STREAM sReport TO VALUE(lcReportFile).

       FILE_CONTENT_LOOP:
       FOR EACH ttInputFileContent WHERE
                ttInputFileContent.FileName = ttBatchInputFile.FileName AND
                ttInputFileContent.LineType = "SUBSCRIPTION"
                BY ttInputFileContent.LineNo:

          PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.

          ASSIGN lcBonoList    = ""
                 lcBono        = ""
                 liBonoCount   = 0
                 liBonoEntries = 0
                 liCount       = 0.

          /* skip header line */
          IF ttInputFileContent.InputLine = "" OR
             ttInputFileContent.InputLine BEGINS "H" THEN NEXT FILE_CONTENT_LOOP.

          /* Bono Activation is not allowed for IPL and Prepaid */
          IF NOT (LOOKUP(ttInputFileContent.TestList,lcIPLContracts) > 0) THEN DO:
                  /* ttInputFileContent.TestList BEGINS "TARJ") THEN DO: */
             FIND FIRST bttInputFileContent WHERE
                        bttInputFileContent.FileName = ttInputFileContent.FileName AND
                        bttInputFileContent.LineType = "ACT_BONO" NO-ERROR.
             IF AVAIL bttInputFileContent THEN DO:
                ASSIGN lcBonoList = bttInputFileContent.TestList
                       liBonoEntries = NUM-ENTRIES(bttInputFileContent.TestList).
                PUT STREAM sOutput UNFORMATTED bttInputFileContent.InputLine SKIP.
             END. /* IF AVAIL bttInputFileContent THEN DO: */
          END. /* IF NOT (ttInputFileContent.TestList BEGINS "CONTRD" */
          DO liCount = 1 TO ttInputFileContent.Qty:
             ASSIGN liOrderId = 0
                    lcCLI     = ""
                    lcResult  = ""
                    liBonoCount = liBonoCount + 1.

             IF liBonoEntries > 0 THEN DO:
                IF liBonoCount > liBonoEntries THEN liBonoCount = 1.
                lcBono = ENTRY(liBonoCount,lcBonoList).
             END. /* IF liBonoEntries > 0 THEN DO: */

             lcResult = fCreateOrder(INPUT ttInputFileContent.CustIDType,
                                     INPUT ttInputFileContent.TestList,
                                     INPUT ttBatchInputFile.ttUserId,
                                     INPUT llOwnCustomer,
                                     INPUT lcOfferId,
                                     INPUT ttBatchInputFile.MsisdnStatus,
                                     INPUT ttBatchInputFile.SimIcc,
                                     INPUT ttBatchInputFile.UsedMSISDN,
                                     OUTPUT lcCLI,
                                     OUTPUT liOrderId).
             IF liOrderId > 0 THEN DO:
                /* OrderAction for Bono Activation */
                IF lcBono > "" THEN DO:
                   fCreateOrderAction(liOrderId,"BundleItem",lcBono,"").
                   fLogEntry(STRING(liCount),lcCLI + "-" + lcBono).
                   
                   fCreateOrderTopup().
                END. /* IF lcBono > "" THEN DO: */
                ELSE
                   fLogEntry(STRING(liCount),lcCLI).
                
                /* Order released after creation */
                lcResult = fCreateOrderCustomer(INPUT liOrderId,
                                                INPUT ttInputFileContent.CustIDType,
                                                INPUT llOwnCustomer).
             END. /* IF liOrderId > 0 THEN DO: */
             ELSE DO:
                liBonoCount = liBonoCount - 1.
                fLogEntry(STRING(liCount),"Order creation failed: " + lcResult).
             END.
          END. /* DO liCount = 1 TO ttInputFileContent.Qty: */
       END. /* FOR EACH ttInputFileContent WHERE */

       /* Log into Report file for Analyser */
       PUT STREAM sReport UNFORMATTED "CustIdType|CustId|OrderId|CLI|MsSeq|CLIType|EmailId" SKIP.

       FOR EACH ttSubscription WHERE
                ttSubscription.FileName = ttBatchInputFile.FileName:
          ASSIGN liCount  = liCount + 1
                 liOrders = liOrders + 1.
          PUT STREAM sReport UNFORMATTED 
              ttSubscription.CustIdType    lcDel
              ttSubscription.CustId        lcDel
              ttSubscription.OrderId       lcDel
              ttSubscription.CLI           lcDel
              STRING(ttSubscription.MsSeq) lcDel
              ttSubscription.CLIType       lcDel
              ttSubscription.EmailId       SKIP.
       END. /* FOR EACH ttSubscription WHERE */

       /* Close Output log file */
       OUTPUT STREAM sOutput CLOSE.
       OUTPUT STREAM sReport CLOSE.

       IF liCount <= 0 THEN
          ASSIGN 
             ttBatchInputFile.OutputFileName  = lcLogFile
             ttBatchInputFile.DeliverFileName = lcLogFile
             ttBatchInputFile.AnalyzerReport  = FALSE.
       ELSE
          ASSIGN 
             ttBatchInputFile.OutputFileName  = lcLogFile
             ttBatchInputFile.DeliverFileName = lcReportFile
             ttBatchInputFile.AnalyzerReport  = TRUE.

    END. /* FOR EACH ttBatchInputFile NO-LOCK */

    /* Verify all the orders until those went through */
    SUB_LOOP:
    REPEAT:
       ASSIGN liCount = 0
              liActCount = 0.

       FOR EACH ttSubscription WHERE ttSubscription.CustNum = 0:
           liCount = liCount + 1.
           FIND FIRST MobSub WHERE
                      MobSub.CLI = ttSubscription.CLI NO-LOCK NO-ERROR.
           IF AVAIL MobSub THEN DO:

              IF LOOKUP(MobSub.CLIType,lcBundleCLITypes) > 0 AND
                 MobSub.TariffBundle = "" THEN NEXT.

              ASSIGN ttSubscription.CustNum = MobSub.CustNum
                     liActCount = liActCount + 1
                     liMobSubs  = liMobSubs + 1.
           END. /* IF AVAIL MobSub THEN DO: */
       END. /* FOR EACH ttSubscription NO-LOCK: */

       /* Leave the block if 80% subscriptions are ready for processing */
       /* Rest 20% subscription will be handled in 2nd round of process */
       IF liActCount > 0 AND
          TRUNC(liCount - (liCount * 0.20),0) <= liActCount THEN DO:
          IF liActCount < liCount THEN llDoubleCheck = TRUE.
          LEAVE SUB_LOOP.
       END.

       /* If there is no order then terminate the loop */
       IF liCount <= 0 THEN LEAVE SUB_LOOP.

       PAUSE 10 NO-MESSAGE.
    END.

    /* Now execute other details */
    IF liCount > 0 THEN DO:
       PAUSE 50 NO-MESSAGE.

       /* Double check for remaining subscriptions */
       IF llDoubleCheck THEN DO:
          RUN pHandleOtherRows(INPUT FALSE,INPUT TRUE).
          PAUSE 200 NO-MESSAGE.

          FOR EACH ttSubscription WHERE ttSubscription.CustNum = 0,
              FIRST MobSub WHERE
                    MobSub.CLI = ttSubscription.CLI NO-LOCK:
              ASSIGN ttSubscription.CustNum = MobSub.CustNum
                     liMobSubs = liMobSubs + 1.
          END. /* FOR EACH ttSubscription NO-LOCK: */

          RUN pHandleOtherRows(INPUT TRUE,INPUT TRUE).

       END. /* IF llDoubleCheck THEN DO: */
       ELSE
          RUN pHandleOtherRows(INPUT TRUE,INPUT TRUE).
    END. /* IF liCount > 0 THEN DO: */

    /* Trigger the Email and transfer Analyzer report */
    FOR EACH ttBatchInputFile WHERE
             ttBatchInputFile.DeliverFileName > "":

       /* Email Sending */
       IF ttBatchInputFile.EmailId > "" AND
          lcMailHost = "merak" THEN DO:
          xMailAddr = ttBatchInputFile.EmailId.
          SendMaileInvoice(lcEmailContent,ttBatchInputFile.DeliverFileName,"").
       END. /* IF lcMailHost = "merak" THEN DO: */

       /* Transfer Analyzer report */
       IF ttBatchInputFile.AnalyzerReport THEN
          fTransDir(ttBatchInputFile.DeliverFileName,
                    "",
                    lcAnalyzerInDir).

       /* Transfer POST report */
       IF ttBatchInputFile.Valid THEN
          fTransDir(ttBatchInputFile.OutputFileName,
                    "",
                    lcOutProcDir).
    END. /* FOR EACH ttBatchInputFile WHERE */

    PAUSE 10 NO-MESSAGE.

END. /* DO WHILE TRUE: */
