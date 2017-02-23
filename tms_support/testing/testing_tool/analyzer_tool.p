/* ----------------------------------------------------------------------
  Module .......: analyzer_tool.p
  Task .........: Analyzer Tool based on input files
  Application ..: TMS
  Author .......: Vikas
  Created ......: 20.07.12
  Version ......: Yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
gcBrand = "1".
katun = "Qvantel".
{Func/date.i}
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/email.i}
{Func/multitenantfunc.i}
{../tms_support/testing/testing_tool/analyzer_tool.i}

DEFINE VARIABLE lcOutPutDir        AS CHAR NO-UNDO.
DEFINE VARIABLE lcDoneDir          AS CHAR NO-UNDO.
DEFINE VARIABLE lcMasterDataDir    AS CHAR NO-UNDO.
DEFINE VARIABLE lcAnalyzerInDir    AS CHAR NO-UNDO.
DEFINE VARIABLE lcAnalyzerProcDir  AS CHAR NO-UNDO.
DEFINE VARIABLE lcAnalyzerSpoolDir AS CHAR NO-UNDO.

DEFINE VARIABLE lcLogFile          AS CHAR NO-UNDO.
DEFINE VARIABLE lcReportFile       AS CHAR NO-UNDO.
DEFINE VARIABLE lcResultFile       AS CHAR NO-UNDO.
DEFINE VARIABLE lcFileName         AS CHAR NO-UNDO.
DEFINE VARIABLE lcLine             AS CHAR NO-UNDO.
DEFINE VARIABLE lcDel              AS CHAR NO-UNDO INIT "|".
DEFINE VARIABLE liLine             AS INT  NO-UNDO.
DEFINE VARIABLE liQty              AS INT  NO-UNDO.
DEFINE VARIABLE llError            AS LOG  NO-UNDO.
DEFINE VARIABLE ldThisRun          AS DEC  NO-UNDO.
DEFINE VARIABLE llFileAvail        AS LOG  NO-UNDO.
DEFINE VARIABLE liLoop             AS INT  NO-UNDO.
DEFINE VARIABLE liFiles            AS INT  NO-UNDO.
DEFINE VARIABLE lcTime             AS CHAR NO-UNDO.
DEFINE VARIABLE lcLockFile         AS CHAR NO-UNDO.
DEFINE VARIABLE lcMailSubject      AS CHAR NO-UNDO.
DEFINE VARIABLE lcEmailContent     AS CHAR NO-UNDO.
DEFINE VARIABLE lcInputFile        AS CHAR NO-UNDO.
DEFINE VARIABLE ldeFileTimeStamp   AS DEC  NO-UNDO.

DEF STREAM sFile.
DEF STREAM sInputFile.
DEF STREAM sOutput.

DEFINE TEMP-TABLE ttBatchInputFile
   FIELD FileName  AS CHAR
   FIELD EmailId   AS CHAR
   INDEX FileName IS PRIMARY UNIQUE FileName.

DEFINE TEMP-TABLE ttInputFileContent
   FIELD FileName   AS CHAR
   FIELD LineNo     AS INT
   FIELD InputLine  AS CHAR
   FIELD CLI        AS CHAR
   FIELD NewSubs    AS LOG
   FIELD Brand      AS CHAR
   INDEX FileNameNo FileName LineNo.

DEFINE TEMP-TABLE ttAnalyzerReport
   FIELD FileName    AS CHAR
   FIELD CLI         AS CHAR
   FIELD ActionType  AS CHAR
   FIELD ActionValue AS CHAR
   FIELD FromTS      AS DEC
   FIELD EndTS       AS DEC
   FIELD Limit       AS DEC
   FIELD FixedFee    AS DEC
   FIELD SingleFee   AS DEC
   FIELD Shaper      AS CHAR
   FIELD Remark      AS CHAR
   INDEX FileName FileName.

FORM
   SKIP(1)
   " Loops .............: " liLoop    FORMAT ">>>>>>>>>9" SKIP
   " Total Files Handled: " liFiles   FORMAT ">>>>>>>>>9" SKIP
   " Last Loop..........: " lcTime    FORMAT "X(20)"
   SKIP(1)
   WITH CENTERED NO-LABELS TITLE " Analyzer Tool " WIDTH 50 ROW 8 FRAME frmMain.

DO ON ERROR UNDO, THROW:
    
    /* Get where to read the incoming files from default tenant */
    fsetEffectiveTenantForAllDB("Default").

    ASSIGN
       lcAnalyzerSpoolDir = fCParam("TestingTool","AnalyzerSpoolDir")
       lcAnalyzerInDir    = fCParam("TestingTool","AnalyzerInDir")
       lcAnalyzerProcDir  = fCParam("TestingTool","AnalyzerProcDir")
       xMailFrom          = fCparam("TestingTool","EmailFromAddress")
       xMailSubj          = fCparam("TestingTool","AnalyzerEmailSubject")
       lcLockFile         = fCparam("TestingTool","AnalyzerDaemonLockFile").

    RUN pInitialize.
     
    RUN pReadFileAndProcess.
END.


PROCEDURE pReadFileAndProcess:
    
    DEFINE VARIABLE lcTenant AS CHARACTER NO-UNDO.

    IF lcLockFile = ? OR lcLockFile = "" THEN
       lcLockFile = "/tmp/test_analyzer_daemon.lock".

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
        EMPTY TEMP-TABLE ttAnalyzerReport.

        ASSIGN liLoop      = liLoop + 1
               llFileAvail = FALSE
               ldThisRun   = fMakeTS().

        DISP
           liLoop
           liFiles
           STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"HH:MM:SS") @ lcTime
           WITH FRAME frmMain.

        PAUSE 0.

        INPUT STREAM sFile THROUGH VALUE("ls -1 " + lcAnalyzerInDir + "/*.LOG").

        FILE_LOOP:
        REPEAT:
           IMPORT STREAM sFile UNFORMATTED lcResultFile.

           IF SEARCH(lcResultFile) NE ? THEN 
           DO:
              lcInputFile      = REPLACE(ENTRY(NUM-ENTRIES(lcResultFile,"/"),lcResultFile,"/"),".LOG","").
              ldeFileTimeStamp = DECIMAL(ENTRY(NUM-ENTRIES(lcInputFile,"_"),lcInputFile,"_")) NO-ERROR.

              /* Handle file after 1 hour so that all requests can be handled */
              IF ldThisRun < fOffSet(ldeFileTimeStamp,1) THEN 
              DO:
                 PAUSE 10 NO-MESSAGE.
                 NEXT MAIN_LOOP.
              END. /* IF ldThisRun < fOffSet(ldeFileTimeStamp,1) THEN DO: */

              ASSIGN liLine = 0
                     llFileAvail = TRUE
                     liFiles = liFiles + 1.

              CREATE ttBatchInputFile.
              ttBatchInputFile.FileName = lcInputFile.

              INPUT STREAM sInputFile FROM VALUE(lcResultFile).
              REPEAT:
                 IMPORT STREAM sInputFile UNFORMATTED lcLine.

                 liLine = liLine + 1.
                 
                 IF lcLine = "" OR lcLine BEGINS "CustIdType" OR NUM-ENTRIES(lcLine,"|") < 8 THEN 
                     NEXT.

                 CREATE ttInputFileContent.
                 ASSIGN
                    ttInputFileContent.LineNo    = liLine
                    ttInputFileContent.InputLine = lcLine
                    ttInputFileContent.FileName  = ttBatchInputFile.FileName
                    ttInputFileContent.CLI       = ENTRY(4,lcLine,"|")
                    ttBatchInputFile.EmailId     = ENTRY(7,lcLine,"|")
                    ttInputFileContent.Brand     = ENTRY(8,lcLine,"|").

                 IF ENTRY(3,lcLine,"|") = "" OR ENTRY(3,lcLine,"|") = "0" THEN
                    ttInputFileContent.NewSubs = FALSE.
                 ELSE
                    ttInputFileContent.NewSubs = TRUE.

              END. /* REPEAT: */
              INPUT STREAM sInputFile CLOSE.
           END. /* IF SEARCH(lcResultFile) NE ? THEN DO: */

           /* Moved input analyzer file to processed directory */
           fTransDir(lcResultFile,
                     "",
                     lcAnalyzerProcDir).
        END. /* REPEAT: */

        INPUT STREAM sFile CLOSE.

        DISP
           liLoop
           liFiles
           STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"HH:MM:SS") @ lcTime
           WITH FRAME frmMain.

        PAUSE 0.

        IF NOT llFileAvail THEN 
        DO:
           PAUSE 10 NO-MESSAGE.
           NEXT MAIN_LOOP.
        END. /* IF NOT llFileAvail THEN DO: */

        /* Now process analyzer file */
        FILE_LOOP:
        FOR EACH ttBatchInputFile:

           ASSIGN lcLogFile = lcAnalyzerSpoolDir + "/" + ttBatchInputFile.FileName + "_ANALYZER_" + STRING(ldThisRun) + ".LOG".

           OUTPUT STREAM sOutput TO VALUE(lcLogFile).

           EMPTY TEMP-TABLE ttAnalyzerReport.

           FILE_CONTENT_LOOP:
           FOR EACH ttInputFileContent WHERE ttInputFileContent.FileName = ttBatchInputFile.FileName NO-LOCK:

              /* skip header line */
              IF ttInputFileContent.InputLine = "" THEN NEXT FILE_CONTENT_LOOP.

              PUT STREAM sOutput UNFORMATTED ttInputFileContent.InputLine SKIP.

              ASSIGN lcTenant = fConvertBrandToTenant(ttInputFileContent.Brand).
              
              IF lcTenant = "" THEN
              DO:
                  PUT STREAM sOutput UNFORMATTED "IMPORTANT: Invalid Tenant" SKIP.
                  NEXT FILE_CONTENT_LOOP.
              END.
                    
              fsetEffectiveTenantForAllDB(lcTenant).
              
              FIND FIRST MobSub WHERE MobSub.CLI = ttInputFileContent.CLI NO-LOCK NO-ERROR.
              IF NOT AVAIL MobSub THEN 
              DO:
                 PUT STREAM sOutput UNFORMATTED "IMPORTANT: Subscription not found" SKIP.
                 NEXT FILE_CONTENT_LOOP.
              END.

              /* Add the header */
              PUT STREAM sOutput UNFORMATTED 
                         "CLI"          lcDel
                         "ActionType"   lcDel
                         "ActionValue"  lcDel
                         "FromTS"       lcDel
                         "EndTS"        lcDel
                         "BundleLimit"  lcDel
                         "FixedFee"     lcDel
                         "SingleFee"    lcDel
                         "Shaper"       lcDel
                         "Remark"       SKIP.

              RUN pContractActivation(INPUT MobSub.MsSeq).
              RUN pContractDeactivation(INPUT MobSub.MsSeq).
              RUN pServiceActivation(INPUT MobSub.MsSeq).
              RUN pServiceDeactivation(INPUT MobSub.MsSeq).
              RUN pSTC(INPUT MobSub.MsSeq).
              RUN pBTC(INPUT MobSub.MsSeq). 

              FOR EACH ttAnalyzerReport WHERE
                       ttAnalyzerReport.FileName = ttBatchInputFile.FileName AND
                       ttAnalyzerReport.CLI      = ttInputFileContent.CLI NO-LOCK:
                 fLogEntry().
              END. /* FOR EACH ttAnalyzerReport WHERE */
           END. /* FOR EACH ttInputFileContent WHERE */

           /* Close Output log file */
           OUTPUT STREAM sOutput CLOSE.

           /* Trigger the Email - don't send email to Yoigo domain */
           IF ttBatchInputFile.EmailId > "" AND lcMailHost = "merak" AND INDEX(ttBatchInputFile.EmailId,"yoigo.com") = 0 THEN 
           DO:
              xMailAddr = ttBatchInputFile.EmailId.
              SendMaileInvoice(lcEmailContent,lcLogFile,"").
           END. /* IF lcMailHost = "merak" THEN DO: */

           /* Moved debug file to processed directory */
           fTransDir(lcLogFile,
                     "",
                     lcAnalyzerProcDir).

        END. /* FOR EACH ttBatchInputFile NO-LOCK */

        PAUSE 10 NO-MESSAGE.

    END. /* DO WHILE TRUE: */

END PROCEDURE.


PROCEDURE pInitialize:

    FOR FIRST InvText NO-LOCK WHERE
              InvText.Brand     = gcBrand                 AND
              InvText.Target    = "EMAIL"                 AND
              InvText.KeyValue  = "EmailConfAnalyzerTool" AND
              InvText.Language  = 5                       AND 
              InvText.FromDate <= TODAY                   AND
              InvText.ToDate   >= TODAY:
       lcEmailContent = InvText.InvText.
    END. /* FOR FIRST InvText NO-LOCK WHERE */

END PROCEDURE.