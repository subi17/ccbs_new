/* Subscription and activation limits bob tool */

{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".

{Syst/tmsconst.i}
{Func/flimitreq.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/fcustdata.i}
{Func/multitenantfunc.i}

&SCOPED-DEFINE STAT_00 "00" /* Threshold successfully updated */
&SCOPED-DEFINE STAT_10 "10" /* ERROR: Client doesn.t exist */
&SCOPED-DEFINE STAT_11 "11" /* ERROR: Bad coded threshold */
&SCOPED-DEFINE STAT_12 "12" /* ERROR: Wrong value for threshold 1 */
&SCOPED-DEFINE STAT_13 "13" /* ERROR: Wrong value for threshold 2 */
&SCOPED-DEFINE STAT_14 "14" /* Threshold 2 should be greater than threshold 1 */
&SCOPED-DEFINE STAT_15 "15" /* ERROR: Bad record format */
&SCOPED-DEFINE STAT_16 "16" /* ERROR: Same values already exists for Limit 1 and 2"*/
&SCOPED-DEFINE STAT_99 "99" /* ERROR: Other error */

&SCOPED-DEFINE FS "|"

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.

DEFINE VARIABLE liCustnum AS INTEGER NO-UNDO. 
DEFINE VARIABLE liTMRuleSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldeLimit AS DECIMAL EXTENT 2 NO-UNDO. 
DEFINE VARIABLE ldeOldLimit AS DECIMAL EXTENT 2 NO-UNDO. 

DEFINE VARIABLE lcSep AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE lcError AS CHAR NO-UNDO. 
DEFINE VARIABLE lcThresholdCode AS CHAR NO-UNDO.
DEFINE VARIABLE llSame AS LOGICAL NO-UNDO.
DEFINE VARIABLE llNewLimit AS LOGICAL NO-UNDO. 
DEFINE VARIABLE llSubsLimit AS LOGICAL NO-UNDO.

/* counters */
DEFINE VARIABLE liCreated AS INTEGER NO-UNDO. 
DEFINE VARIABLE liUpdated AS INTEGER NO-UNDO. 
DEFINE VARIABLE liNoCustomer AS INTEGER NO-UNDO. 
DEFINE VARIABLE liNok AS INTEGER NO-UNDO. 

/* files and dirs */
DEFINE VARIABLE lcOutDir  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIncDir  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSpoolDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcStatFile AS CHARACTER NO-UNDO. 

/* final output file names */
DEFINE VARIABLE lcSummaryFileOut AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcTime AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liDate AS INT NO-UNDO. 
DEFINE VARIABLE lcToday AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llRecovery AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lcTenant AS CHARACTER NO-UNDO. 

lcToday =   STRING(YEAR(TODAY),"9999") + 
            STRING(MONTH(TODAY),"99") +
            STRING(DAY(TODAY),"99").

ASSIGN
   lcIncDir    = fCParam("tmlimitfile","IncDir") 
   lcProcDir   = fCParam("tmlimitfile","IncProcessedDir") 
   lcSpoolDir  = fCParam("tmlimitfile","OutSpoolDir")
   lcOutDir    = fCParam("tmlimitfile","OutDir").

DEF STREAM sin.
DEF STREAM sReport.
DEF STREAM sStatistics.
DEF STREAM sFile.

FUNCTION fRep RETURNS LOGICAL
(icStat AS CHAR):
   PUT STREAM sReport UNFORMATTED icStat {&FS}  
      liCustnum {&FS} lcThresholdCode {&FS}
      ldeLimit[1] {&FS} ldeLimit[2] {&FS}
      ldeOldLimit[1] {&FS} ldeOldLimit[2] SKIP. 
END FUNCTION. 


FUNCTION fUpdateSubsLimit RETURNS LOGICAL:

   DEF VAR liLimitType         AS INT  NO-UNDO.
   DEF VAR liMobsubLimit       AS INT  NO-UNDO.
   DEF VAR llDefaultSubsLimit  AS LOG  NO-UNDO.

   CASE lcThresholdCode:
      WHEN "SL" THEN liLimitType = {&LIMIT_TYPE_SUBQTY}.
      WHEN "AL" THEN liLimitType = {&LIMIT_TYPE_SUBACTQTY}.
      OTHERWISE RETURN FALSE.
   END. /* CASE lcThresholdCode: */

   IF ldeLimit[1] = ? OR ldeLimit[1] < 0 OR ldeLimit[1] > 999 THEN DO:
      liNok = liNok + 1.
      PUT STREAM sReport UNFORMATTED {&STAT_12} {&FS} lcLine SKIP.
      RETURN FALSE.
   END. /* IF ldeLimit[1] = ? OR ldeLimit[1] */

   /* Validate activation limit with subs. limit */
   IF liLimitType = {&LIMIT_TYPE_SUBACTQTY} THEN DO:
      liMobsubLimit = fGetMobsubLimit(INPUT Customer.Custnum,
                                      INPUT Customer.Category,
                                      OUTPUT llDefaultSubsLimit).
      IF ldeLimit[1] < liMobsubLimit THEN DO:
         liNok = liNok + 1.
         PUT STREAM sReport UNFORMATTED {&STAT_12} {&FS} lcLine SKIP.
         RETURN FALSE.
      END. /* IF ldeLimit[1] < liMobsubLimit THEN DO: */
   END. /* IF liLimitType = {&LIMIT_TYPE_SUBACTQTY} THEN DO: */

   ldeOldLimit[2] = ldeLimit[2].

   fGetLimit (Customer.Custnum, 0, liLimitType, 0, 0, TODAY).
   
   IF AVAIL Limit THEN DO:
      ldeOldLimit[1] = Limit.LimitAmt.

      IF ldeLimit[1] = ldeOldLimit[1] THEN DO:
         liNok = liNok + 1.
         fRep({&STAT_16}).
         RETURN FALSE.
      END. /* IF ldeLimit[1] = ldeOldLimit[1] THEN DO: */

      fSetLimit(ROWID(Limit), ldeLimit[1], FALSE, TODAY, 12/31/2049).
   END. /* IF AVAIL Limit THEN DO: */
   ELSE DO:
      llNewLimit = TRUE.
      fCreateLimit(Customer.Custnum,
                   0,
                   liLimitType,
                   ldeLimit[1],
                   0,
                   0,
                   TODAY,
                   12/31/2049).    
   END. /* ELSE DO: */

   IF llNewLimit THEN liCreated = liCreated + 1.
   ELSE liUpdated = liUpdated + 1.

   RETURN TRUE.

END FUNCTION. /* FUNCTION fUpdateSubsLimit: */


FUNCTION fUpdateTMRuleLimit RETURNS LOGICAL:

   i = 0.

   FIND TMRule NO-LOCK WHERE
        TMRule.TMRuleSeq = liTMRuleSeq NO-ERROR.
   IF NOT AVAIL TMRule THEN DO:
      liNok = liNok + 1.
      PUT STREAM sReport UNFORMATTED {&STAT_11} {&FS} lcLine SKIP.
      RETURN FALSE.
   END.

   LIMIT_LOOP:
   DO i = 1 TO 2:
         
      FIND TMRLimit WHERE
           TMRLimit.TMRuleSeq = TMRule.TMRuleSeq AND
           TMRLimit.LimitId   = i AND
           TMRLimit.ToDate   >= TODAY NO-LOCK NO-ERROR.
      IF NOT AVAIL TMRLimit THEN DO:
         liNok = liNok + 1.
         PUT STREAM sReport UNFORMATTED {&STAT_11} {&FS} lcLine SKIP.
         RETURN FALSE.
      END.
         
      ldeOldLimit[i] = TMRLimit.LimitAmt.
            
      fGetLimit(
            Customer.Custnum,
            0,
            {&LIMIT_TYPE_TMRLIMIT},
            TMRLimit.LimitId, 
            TMRule.TMRuleSeq,
            TODAY).
         
      IF AVAIL Limit THEN
         ASSIGN llNewLimit     = FALSE
                ldeOldLimit[i] = Limit.LimitAmt.
         
      IF ldeLimit[i] NE ldeOldLimit[i] THEN llSame = FALSE.
         
      IF ldeLimit[i] < TMRLimit.MinValue OR ldeLimit[i] > TMRLimit.MaxValue THEN
         lcError = (IF i = 1 THEN {&STAT_12} ELSE {&STAT_13}).
      
   END. /* DO i = 1 TO 2: */

   IF lcError NE "" THEN DO:
      fRep(lcError).  
      liNok = liNok + 1.
      RETURN FALSE.
   END.
      
   IF ldeLimit[1] > ldeLimit[2] THEN lcError = {&STAT_14}.
   IF llSame THEN lcError = {&STAT_16}.
      
   IF lcError NE "" THEN DO:
      fRep(lcError).  
      liNok = liNok + 1.
      RETURN FALSE.
   END.

   DO TRANSACTION:
      DO i = 1 TO EXTENT(ldeLimit):
         fCreateLimitHistory(
               Customer.Custnum,
               0,
               {&LIMIT_TYPE_TMRLIMIT},
               ldeLimit[i],
               i,
               TMRule.TMRuleSeq,
               FALSE,
               TODAY,
               12/31/2049). 
      END. /* DO i = 1 TO EXTENT(ldeLimit): */
         
      IF llNewLimit THEN liCreated = liCreated + 1.
      ELSE liUpdated = liUpdated + 1.
   END. /* DO TRANSACTION: */

   RETURN TRUE.

END FUNCTION. /* FUNCTION fUpdateTMRuleLimit: */


INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).

REPEAT:
      
   llRecovery = FALSE.

   IMPORT STREAM sFile UNFORMATTED lcFileName.
 
   lcInputFile = lcIncDir + lcFileName.
   
   IF SEARCH(lcInputFile) NE ? THEN
      INPUT STREAM sin FROM VALUE(lcInputFile).
   ELSE NEXT.
   /* Set effective tenant based on file name.
      If not recognised go to next file */
   lcTenant = ENTRY(1,lcFileName,"_").
   IF NOT fsetEffectiveTenantForAllDB(
      fConvertBrandToTenant(lcTenant)) THEN NEXT.  
 
   IF INDEX(lcFileName,"results") > 0 THEN DO:
      llRecovery = TRUE.
      liDate = INT(SUBSTRING(ENTRY(4,lcFileName,"_"),1,8)) NO-ERROR.
   END.
   ELSE liDate = INT(SUBSTRING(ENTRY(3,lcFileName,"_"),1,8)) NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN NEXT.
   IF INT(lcToday) < liDate THEN NEXT.
   
   lcTime = STRING(TIME,"HH:MM").
   lcOutputFile = lcSpoolDir + lcTenant + "_threshold_results_" + 
                  STRING(liDate) + ENTRY(1,lcTime,":") + ENTRY(2,lcTime,":") + 
                  ".txt".
   OUTPUT STREAM sReport TO VALUE(lcOutputFile).
   fBatchLog("START", lcOutputFile).

   lcStatFile = lcSpoolDir + lcTenant + "_threshold_summary_" + 
                STRING(liDate) + ".txt".
   OUTPUT STREAM sStatistics TO VALUE(lcStatFile).
   fBatchLog("START", lcStatFile).
   
   ASSIGN 
      liCreated = 0
      liUpdated = 0
      liNoCustomer = 0
      liNok = 0.

   LINE_LOOP:
   REPEAT:
      IMPORT STREAM sin UNFORMATTED lcLine.
   
      ASSIGN 
         ldeLimit    = 0
         ldeOldLimit = 0
         llSame      = TRUE
         lcError     = ""
         llNewLimit  = TRUE
         llSubsLimit = FALSE.
    
      IF llRecovery THEN DO:
         IF NOT lcLine BEGINS "00" THEN DO:
            liNok = liNok + 1.
            NEXT LINE_LOOP. 
         END.
         lcLine = SUBSTRING(lcLine, 4).
      END.

      liCustNum = INT(ENTRY(1,lcLine,lcSep)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN lcError = {&STAT_15}.
      
      lcThresholdCode = ENTRY(2,lcLine,lcSep).

      IF LOOKUP(lcThresholdCode, "SL,AL") > 0 THEN
         llSubsLimit = TRUE.

      IF NOT llSubsLimit THEN DO:
         liTMRuleSeq = INT(lcThresholdCode) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN lcError = {&STAT_15}.
      END. /* IF NOT llSubsLimit THEN DO: */
     
      IF llRecovery THEN ldeLimit[1] = DEC(ENTRY(5,lcLine,lcSep)) NO-ERROR.
      ELSE ldeLimit[1] = DEC(ENTRY(3,lcLine,lcSep)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN lcError = {&STAT_15}.
      
      IF llRecovery THEN ldeLimit[2] = DEC(ENTRY(6,lcLine,lcSep)) NO-ERROR.
      ELSE ldeLimit[2] = DEC(ENTRY(4,lcLine,lcSep)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN lcError = {&STAT_15}.
      /* check for additional chars */
     
      IF NOT llRecovery THEN DO:
         ENTRY(5,lcLine,lcSep) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN lcError = {&STAT_15}.
      END.

      IF lcError NE "" THEN DO:
         PUT STREAM sReport UNFORMATTED lcError {&FS} lcLine SKIP.
         liNok = liNok + 1.
         NEXT LINE_LOOP.
      END.
      
      /* Line parsing ends */

      FIND Customer NO-LOCK WHERE 
         Customer.Custnum = liCustNum NO-ERROR.
      IF NOT AVAIL Customer THEN DO:
         liNoCustomer = liNoCustomer + 1.
         PUT STREAM sReport UNFORMATTED {&STAT_10} {&FS} lcLine SKIP.
         NEXT LINE_LOOP.
      END. /* IF NOT AVAIL Customer THEN DO: */

      IF llSubsLimit THEN DO:
         IF NOT fUpdateSubsLimit() THEN NEXT LINE_LOOP.
      END. /* IF llSubsLimit THEN DO: */
      ELSE IF NOT fUpdateTMRuleLimit() THEN NEXT LINE_LOOP.
      
      fRep({&STAT_00}).  

   END.

   PUT STREAM sStatistics UNFORMATTED 
      "CREATED ....: " liCreated SKIP
      "UPDATED ....: " liUpdated SKIP
      "NO CUSTOMER : " liNoCustomer SKIP
      "NOT OK .....: " liNok.
   
   fTransDir(lcInputFile,
             "",
             lcProcDir).
   
   lcReportFileOut = fMove2TransDir(lcOutputFile, "", lcOutDir).
   lcSummaryFileOut = fMove2TransDir(lcStatFile, "", lcOutDir).
   
   IF lcReportFileOut NE "" THEN fBatchLog("FINISH", lcReportFileOut).
   IF lcSummaryFileOut NE "" THEN fBatchLog("FINISH", lcSummaryFileOut).
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sReport CLOSE.
   OUTPUT STREAM sStatistics CLOSE.

END.

