/* ----------------------------------------------------------------------
  MODULE .......: update_satisfactionvalue.p 
  TASK .........: Update satisfaction value (IAS) 
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{tmsconst.i}
{ftransdir.i}
{cparam2.i}
{eventlog.i}
{eventval.i}
{email.i}
{timestamp.i}

/* files and dirs */
DEF VAR lcLine AS CHAR NO-UNDO.
DEF VAR ldReadTS AS DEC NO-UNDO.
DEF VAR lcLogFile AS CHAR NO-UNDO. 
DEF VAR lcFileName AS CHAR NO-UNDO. 
DEF VAR lcIncDir  AS CHAR NO-UNDO. 
DEF VAR lcInputFile AS CHAR NO-UNDO. 
DEF VAR lcProcDir AS CHAR NO-UNDO. 
DEF VAR lcProcessedFile AS CHAR NO-UNDO. 
DEF VAR lcSpoolDir AS CHAR NO-UNDO. 
DEF VAR lcReportFileOut AS CHAR NO-UNDO. 
DEF VAR lcOutDir AS CHAR NO-UNDO. 
DEF VAR lcConfDir AS CHAR NO-UNDO. 
DEF VAR liNumOK AS INT NO-UNDO. 
DEF VAR liNumErr AS INT NO-UNDO. 
DEF VAR liYear AS INT NO-UNDO. 
DEF VAR liMonth AS INT NO-UNDO.
DEF VAR liDay AS INT NO-UNDO. 

ASSIGN
   lcIncDir    = fCParam("UpdateIAS","IncomingDir") 
   lcProcDir   = fCParam("UpdateIAS","IncProcDir")
   lcSpoolDir  = fCParam("UpdateIAS","OutSpoolDir")
   lcOutDir    = fCParam("UpdateIAS","OutDir")
   lcConfDir   = fCParamC("RepConfDir").

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine  "|"
      icMessage SKIP.    
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).
END FUNCTION.
 
/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName. 
   lcInputFile = lcIncDir + lcFileName.
   
   IF SEARCH(lcInputFile) NE ? THEN 
      INPUT STREAM sin FROM VALUE(lcInputFile).
   ELSE NEXT.

   /* extract date from filename */
   liYear  = INT(SUBSTRING(lcFileName,9,4)).
   liMonth = INT(SUBSTRING(lcFileName,13,2)).
   liDay   = INT(SUBSTRING(lcFileName,15,2)).
   ldReadTS = fHMS2TS(DATE(liMonth,liDay,liYear),"00:00:00").

   fBatchLog("START", lcInputFile).
   lcLogFile = lcSpoolDir + lcFileName + ".log".
   OUTPUT STREAM sLog TO VALUE(lcLogFile) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.
   
   LINE_LOOP:
   REPEAT:
      
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine EQ "" THEN NEXT.

      RUN pUpdateIAS (lcLine, ldReadTS).
      
      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         fError(ENTRY(2,RETURN-VALUE,":")).
         liNumErr = liNumErr + 1 .
      END.
      ELSE DO:
         liNumOK = liNumOK + 1 .
      END.
   END.
  
   PUT STREAM sLog UNFORMATTED 
       "input: " STRING(liNumOK + liNumErr) ", "
       "updated: " STRING(liNumOK) ", "
       "errors: " STRING(liNumErr) SKIP.

   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir). 
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   IF liNumErr > 0 THEN DO:
      /* mail recipients */
      GetRecipients(lcConfDir + "UpdateIAS" + ".email").
      /* send via mail */
      SendMail(lcReportFileOut,"").
   END.

END.

INPUT STREAM sFile CLOSE.


FUNCTION fFindIAS RETURNS LOGICAL 
   (pcHostTable AS CHAR,
    pcKeyValue AS CHAR,
    pdTimeStamp AS DEC):
   FIND FIRST PIndicator NO-LOCK WHERE
              PIndicator.Brand = gcBrand AND
              PIndicator.HostTable = pcHostTable AND 
              PIndicator.KeyValue = pcKeyValue AND
              PIndicator.IndicatorType = {&P_INDICATOR_TYPE_SATISFACTION_VALUE} AND
              PIndicator.TimeStamp = pdTimeStamp NO-ERROR.
    IF AVAIL PIndicator THEN RETURN TRUE.
    RETURN FALSE.
END FUNCTION.


FUNCTION fCreateIAS RETURNS LOGICAL 
   (pcHostTable AS CHAR,
    pcKeyValue AS CHAR,
    pdTimeStamp AS DEC):
    CREATE PIndicator.
    ASSIGN PIndicator.Brand = gcBrand 
           PIndicator.HostTable = pcHostTable
           PIndicator.KeyValue = pcKeyValue
           PIndicator.IndicatorType = {&P_INDICATOR_TYPE_SATISFACTION_VALUE}
           PIndicator.TimeStamp = pdTimeStamp . 
    RETURN TRUE.
END FUNCTION.

FUNCTION fUpdateCurrentIAS RETURNS LOGICAL 
   (pcIASvalue  AS CHAR,
    pcMemo AS CHAR):
    FIND CURRENT PIndicator EXCLUSIVE-LOCK NO-ERROR. 
    ASSIGN PIndicator.IndicatorValue = pcIASvalue
           PIndicator.Memo = pcMemo.
    RELEASE PIndicator.
    RETURN TRUE.
END FUNCTION.

FUNCTION fGetCurrentIASvalue RETURNS INTEGER :
    FIND CURRENT PIndicator NO-LOCK NO-ERROR. 
    RETURN INT(PIndicator.IndicatorValue) .
END FUNCTION.


PROCEDURE pUpdateIAS : 

DEFINE INPUT PARAMETER pcLine AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pdReadTS AS DECIMAL NO-UNDO.

/* local variables */
DEFINE VARIABLE lcCli AS CHAR NO-UNDO.
DEFINE VARIABLE liIASMobSub AS INT NO-UNDO.
DEFINE VARIABLE liIASCustomer AS INT NO-UNDO.
DEFINE VARIABLE lcMemo AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liIASvalue AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcSep AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE llExistIASCust AS LOGICAL NO-UNDO. 

ASSIGN
    lcCLi = ENTRY(1,pcLine,lcSep)
    liIASMobSub = INT(ENTRY(2,pcLine,lcSep))
    liIASCustomer = INT(ENTRY(3,pcLine,lcSep))
    lcMemo = ENTRY(4,pcLine,lcSep) NO-ERROR.

IF ERROR-STATUS:ERROR THEN RETURN "ERROR: Wrong file format".

/* validate mobsub */
FIND MobSub NO-LOCK WHERE
     MobSub.Brand = gcBrand AND
     Mobsub.Cli = lcCli NO-ERROR.
IF NOT AVAIL MobSub THEN RETURN "ERROR:Subscription not found".

/* validate active customer */
FIND Customer NO-LOCK WHERE
     Customer.CustNum = MobSub.CustNum NO-ERROR. 
IF NOT AVAIL Customer THEN RETURN "ERROR:Customer not found".

/* update MobSub IAS */
IF NOT fFindIAS("MobSub",STRING(MobSub.MsSeq),pdReadTS) THEN 
   fCreateIAS("MobSub",STRING(MobSub.MsSeq),pdReadTS).
fUpdateCurrentIAS(STRING(liIASMobSub), lcMemo).

/* update Customer IAS */
IF NOT fFindIAS("Customer",STRING(Customer.CustNum),pdReadTS) THEN DO:
   fCreateIAS("Customer",STRING(Customer.CustNum),pdReadTS). 
   fUpdateCurrentIAS(STRING(liIASCustomer),"").
END.
ELSE DO:
   liIASvalue = fGetCurrentIASvalue().
   IF liIASvalue > liIASCustomer THEN 
      fUpdateCurrentIAS(STRING(liIASCustomer),"").
END.

RETURN "".

END PROCEDURE.  
