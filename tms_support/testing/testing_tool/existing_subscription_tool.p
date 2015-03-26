/* ----------------------------------------------------------------------
  Module .......: existing_subscription_tool.p
  Task .........: STC/BTC and Contract Activation/Deactivation Testing Tool
                  to existing subscription
  Application ..: TMS
  Author .......: Vikas
  Created ......: 08.08.12
  Version ......: Yoigo
---------------------------------------------------------------------- */

{commpaa.i}
gcBrand = "1".
katun = "Qvantel".
{timestamp.i}
{tmsconst.i}
{cparam2.i}
{fmakemsreq.i}
{msreqfunc.i}
{service.i}
{fdss.i}
{ftransdir.i}

DEFINE VARIABLE lcOutPutDir        AS CHAR NO-UNDO.
DEFINE VARIABLE lcIncomingDir      AS CHAR NO-UNDO.
DEFINE VARIABLE lcDoneDir          AS CHAR NO-UNDO.
DEFINE VARIABLE lcMasterDataDir    AS CHAR NO-UNDO.
DEFINE VARIABLE lcAnalyzerDataDir  AS CHAR NO-UNDO.

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

DEF STREAM sOrder.
DEF STREAM sOrderCust.
DEF STREAM sFile.
DEF STREAM sInputFile.
DEF STREAM sOutput.
DEF STREAM sReport.

DEFINE TEMP-TABLE ttBatchInputFile
   FIELD FileName  AS CHAR
   INDEX FileName IS PRIMARY UNIQUE FileName.

DEFINE TEMP-TABLE ttInputFileContent
   FIELD FileName   AS CHAR
   FIELD LineNo     AS INT
   FIELD InputLine  AS CHAR
   FIELD LineType   AS CHAR
   FIELD TestList   AS CHAR
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
   INDEX CLI IS PRIMARY UNIQUE CLI.

ASSIGN
   lcOutPutDir       = fCParam("TestingTool","OutDir")
   lcIncomingDir     = fCParam("TestingTool","InDir")
   lcDoneDir         = fCParam("TestingTool","InProcDir")
   lcMasterDataDir   = fCParam("TestingTool","InMasterDir")
   lcAnalyzerDataDir = fCParam("TestingTool","AnalyzerDir")
   ldThisRun         = fMakeTS().

DEF TEMP-TABLE ttOrder    NO-UNDO LIKE Order.
DEF TEMP-TABLE ttOrderCustomer NO-UNDO LIKE OrderCustomer.
DEFINE BUFFER bttInputFileContent FOR ttInputFileContent.

/* Include file */
{/apps/yoigo/tms_support/testing/testing_tool/subscription_creation_tool.i}

/* Main Block */

INPUT STREAM sFile THROUGH VALUE("ls -1 " + lcIncomingDir + "/*.RPT").

FILE_LOOP:
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcResultFile.

   IF SEARCH(lcResultFile) NE ? THEN DO:
      ASSIGN liLine = 0
             llFileAvail = TRUE.

      CREATE ttBatchInputFile.
      ttBatchInputFile.FileName = ENTRY(NUM-ENTRIES(lcResultFile,"/"),
                                        lcResultFile,"/").

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
             lcDoneDir).
END. /* REPEAT: */

INPUT STREAM sFile CLOSE.

IF NOT llFileAvail THEN QUIT.

FILE_LOOP:
FOR EACH ttBatchInputFile:

   lcLogFile = lcOutPutDir + "/" + ttBatchInputFile.FileName + "_PRE_" +
               STRING(ldThisRun) + ".LOG".
   OUTPUT STREAM sOutput TO VALUE(lcLogFile).

   FILE_CONTENT_LOOP:
   FOR EACH ttInputFileContent WHERE
            ttInputFileContent.FileName = ttBatchInputFile.FileName
       BREAK BY ttInputFileContent.FileName
             BY ttInputFileContent.LineNo:

      IF FIRST-OF(ttInputFileContent.FileName) THEN
         ASSIGN llError = FALSE
                llKeep  = FALSE.

      PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.

      /* skip header line */
      IF ttInputFileContent.InputLine = "" OR
         ttInputFileContent.InputLine BEGINS "H" THEN NEXT FILE_CONTENT_LOOP.

      IF NUM-ENTRIES(ttInputFileContent.InputLine,"|") <> 3 THEN DO:
         llError = TRUE.
         fLogEntry(ttInputFileContent.InputLine,"Invalid input file format").
         LEAVE FILE_CONTENT_LOOP.
      END. /* ELSE DO: */

      ASSIGN ttInputFileContent.LineType = ENTRY(2,ttInputFileContent.InputLine,"|")
             ttInputFileContent.TestList = ENTRY(3,ttInputFileContent.InputLine,"|").

      IF ttInputFileContent.LineType <> "Subscription" AND
         ttInputFileContent.TestList > "" THEN llKeep = TRUE.

      IF LAST-OF(ttInputFileContent.FileName) THEN DO:
         IF llKeep = FALSE THEN DO:
            llError = TRUE.
            fLogEntry(ttInputFileContent.InputLine,"No Test case is defined").
         END. /* IF llKeep = FALSE THEN DO: */
      END. /* IF LAST-OF(ttInputFileContent.FileName) THEN DO: */
   END. /* FOR EACH ttInputFileContent WHERE */

   IF llError = TRUE THEN DELETE ttBatchInputFile.

   /* Close Output log file */
   OUTPUT STREAM sOutput CLOSE.
   /* Moved output file to processed directory */
   fTransDir(lcLogFile,
             "",
             lcDoneDir).

END. /* FOR EACH ttBatchInputFile NO-LOCK */

/* Now process actual data and create new orders */
FILE_LOOP:
FOR EACH ttBatchInputFile:

   ASSIGN
      lcLogFile = lcOutPutDir + "/" + ttBatchInputFile.FileName + "_POST_" +
                  STRING(ldThisRun) + ".LOG"
      lcReportFile = lcOutPutDir + "/" + ttBatchInputFile.FileName +
                     "_ANALYZER_" + STRING(ldThisRun) + ".LOG".

   OUTPUT STREAM sOutput TO VALUE(lcLogFile).
   OUTPUT STREAM sReport TO VALUE(lcReportFile).

   FILE_CONTENT_LOOP:
   FOR EACH ttInputFileContent WHERE
            ttInputFileContent.FileName = ttBatchInputFile.FileName AND
            ttInputFileContent.LineType = "SUBSCRIPTION"
            BY ttInputFileContent.LineNo:

      PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.

      /* skip header line */
      IF ttInputFileContent.InputLine = "" OR
         ttInputFileContent.InputLine BEGINS "H" THEN NEXT FILE_CONTENT_LOOP.

      FIND FIRST MobSub WHERE
                 MobSub.CLI = ttInputFileContent.TestList NO-LOCK NO-ERROR.
      IF NOT AVAIL MobSub THEN DO:
         fLogEntry(ttInputFileContent.InputLine,"Subscription not found").
         NEXT FILE_CONTENT_LOOP.
      END. /* IF NOT AVAIL MobSub THEN DO: */

      FIND FIRST Customer WHERE
                 Customer.CustNum = MobSub.CustNum NO-LOCK NO-ERROR.
      IF NOT AVAIL Customer THEN DO:
         fLogEntry(ttInputFileContent.InputLine,"Customer not found").
         NEXT FILE_CONTENT_LOOP.
      END. /* IF NOT AVAIL Customer THEN DO: */

      CREATE ttSubscription.
      ASSIGN ttSubscription.CLI        = MobSub.CLI
             ttSubscription.MsSeq      = MobSub.MsSeq
             ttSubscription.CLIType    = MobSub.CLIType
             ttSubscription.CustNum    = Customer.CustNum
             ttSubscription.CustIdType = Customer.CustIdType
             ttSubscription.CustId     = Customer.OrgId
             ttSubscription.FileName   = ttInputFileContent.FileName.

   END. /* FOR EACH ttInputFileContent WHERE */

   /* Log into Report file for Analyser */
   FOR EACH ttSubscription WHERE
            ttSubscription.FileName = ttBatchInputFile.FileName:
      PUT STREAM sReport UNFORMATTED 
          ttSubscription.CustIdType lcDel
          ttSubscription.CustId     lcDel
          ttSubscription.CLI        lcDel
          ttSubscription.OrderId    lcDel
          ttSubscription.CLIType    SKIP.
   END. /* FOR EACH ttSubscription WHERE */

   /* Close Output log file */
   OUTPUT STREAM sOutput CLOSE.
   OUTPUT STREAM sReport CLOSE.

   /* Moved report file to analyzer directory for analyzing */
   fTransDir(lcReportFile,
             "",
             lcAnalyzerDataDir).
END. /* FOR EACH ttBatchInputFile NO-LOCK */


/* Now execute other details */
RUN pHandleOtherRows(INPUT TRUE,INPUT FALSE).


