/* ----------------------------------------------------------------------
  MODULE .......: stc_massive_bob.p
  TASK .........: Back door tools: Automate Massive STC 
                  RES-1683 Massive change of tariffs Yoigo.
  APPLICATION ..: TMS
  AUTHOR .......: ravalupa
  CREATED ......: 03.07.18
  Version ......: Yoigo
----------------------------------------------------------------------- */

Syst.Var:katun    = "Cron".
Syst.Var:gcBrand  = "1".

{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Syst/eventval.i}
{Func/fmakemsreq.i}

DEFINE STREAM sin.
DEFINE STREAM sout.
DEFINE VARIABLE lcLine          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSep           AS CHARACTER NO-UNDO INIT ";".
DEFINE VARIABLE liNumOK         AS INTEGER   NO-UNDO. 
DEFINE VARIABLE liNumErr        AS INTEGER   NO-UNDO. 

/* files and dirs */
DEFINE VARIABLE lcLogFile       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFileName      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIncDir        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInputFile     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcDir       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSpoolDir      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutDir        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcRootDir       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liEntries       AS INTEGER   NO-UNDO. 

/* field variables */
DEFINE VARIABLE lcCustomer      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMsisdn        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOldCLIType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcNewCLIType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBankAccount   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liRequest       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcinfo          AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeFee          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liCreditcheck   AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldeTimeStamp    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lcBONOContracts AS CHARACTER NO-UNDO.
 

DEFINE BUFFER new_CLIType FOR CLIType.
DEFINE VARIABLE not_used AS CHAR.

ASSIGN
   lcRootDir         = fCParam("MassiveSTC","RootDir")
   lcBONOContracts   = fCParamC("BONO_CONTRACTS")
   .

IF NOT lcRootDir > "" THEN RETURN.

ASSIGN
   lcIncDir   = lcRootDir + "incoming/" 
   lcProcDir  = lcRootDir + "processed/"
   lcSpoolDir = lcRootDir + "spool/"
   lcOutDir   = lcRootDir + "outgoing/"
   .

DEFINE STREAM sin.
DEFINE STREAM sFile.
DEFINE STREAM sLog.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHARACTER):

   PUT STREAM sLog UNFORMATTED
      lcLine lcSep 
      icMessage SKIP.
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHARACTER):

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

   ASSIGN
      liNumOk  = 0
      liNumErr = 0.
   
   fBatchLog("START", lcInputFile).
   lcLogFile = lcSpoolDir + lcFileName + ".LOG".
   OUTPUT STREAM sLog TO VALUE(lcLogFile) append.

   PUT STREAM sLog UNFORMATTED
      lcFilename  " "
      STRING(TODAY,"99.99.99") " "
      STRING(TIME,"hh:mm:ss") SKIP.
  
   LINE_LOOP:
   REPEAT:
      
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF TRIM(lcLine) EQ "" THEN NEXT.
      
      ASSIGN 
         liEntries      = NUM-ENTRIES(lcLine,lcSep)
         lcCustomer     = TRIM(ENTRY(1,lcLine,lcSep))
         lcMsisdn       = TRIM(ENTRY(2,lcLine,lcSep))
         lcOldCLIType   = TRIM(ENTRY(3,lcLine,lcSep))
         lcNewCLIType   = TRIM(ENTRY(4,lcLine,lcSep))
         lcSMSText      = TRIM(ENTRY(5,lcLine,lcSep))
         ldtSTCDate     = TRIM(ENTRY(6,lcLine,lcSep))
         NO-ERROR.

      IF ERROR-STATUS:ERROR OR liEntries NE 5 THEN 
      DO:
         fError("Incorrect input data format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.


      RUN pSTC(lcMsisdn,
               lcOldCLIType,
               lcNewCLIType,
               lcSMSText,
               ldtSTCDate
              ).

      IF RETURN-VALUE BEGINS "ERROR" THEN 
      DO:
         fError(ENTRY(2,RETURN-VALUE,":")).
         liNumErr = liNumErr + 1 .
      END.
      ELSE liNumOK = liNumOK + 1 .
   END.
  
   PUT STREAM sLog UNFORMATTED 
      "input: " STRING(liNumOK + liNumErr) ", "
      "updated: " STRING(liNumOK) ", "
      "errors: " STRING(liNumErr) SKIP.
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir).  
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).

END.

INPUT STREAM sFile CLOSE.

/*
   Procedure updates subscription level STC.
*/
PROCEDURE pSTC:

   DEFINE INPUT PARAMETER pcMsisdn AS CHARACTER NO-UNDO.
   lcSMSText
   DEFINE INPUT PARAMETER pcNewProfile AS CHARACTER NO-UNDO.

   DEFINE VARIABLE lcError AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liReq   AS INTEGER   NO-UNDO.

   FIND  MobSub NO-LOCK WHERE
         MobSub.CLI = pcMsisdn NO-ERROR.
   IF NOT AVAILABLE MobSub THEN 
   DO:
      PUT STREAM sout UNFORMATTED lcline lcSep "ERROR:MobSub not found" SKIP.
      NEXT.
   END.

   FIND  Customer NO-LOCK WHERE
         Customer.CustNum = MobSub.CustNum NO-ERROR.
   IF NOT AVAILABLE Customer THEN 
   DO:
      PUT STREAM sout UNFORMATTED lcline lcSep "ERROR:customer not found" SKIP.
      NEXT.
   END.

   IF Customer.CustIDType = "CIF" THEN liCreditcheck = 0.

   FIND FIRST  CLIType WHERE
               CLIType.Clitype = MobSub.CliType  AND
               CliType.Brand   = Syst.Var:gcBrand NO-LOCK NO-ERROR.
   IF NOT AVAILABLE CLIType THEN 
   DO:
      PUT STREAM sout UNFORMATTED lcline lcSep "ERROR:Current CliType not found" SKIP.
      NEXT.
   END.
   
   IF CLIType.Clitype <> {&CLITYPE_TARIFFTYPE_MOBILEONLY} THEN 
   DO:
      PUT STREAM sout UNFORMATTED lcline lcSep "ERROR:Current CliType not Mobile" SKIP.
      NEXT.
   END.

   FIND FIRST  new_CLIType WHERE
               new_CLIType.Clitype = lcNewCLIType AND
               new_CLIType.Brand   = Syst.Var:gcBrand NO-LOCK NO-ERROR.
   IF NOT AVAILABLE new_CLIType THEN 
   DO:
      PUT STREAM sout UNFORMATTED lcline lcSep "ERROR:New CliType not found" SKIP.
      NEXT.
   END.

   IF CLIType.TariffType <> {&CLITYPE_TARIFFTYPE_MOBILEONLY} THEN 
   DO:
      PUT STREAM sout UNFORMATTED lcline lcSep "ERROR:Current CliType is Not a Mobile Tariff" SKIP.
      NEXT.
   END.
   
   IF new_CLIType.Clitype = MobSub.CliType THEN 
   DO:
      PUT STREAM sout UNFORMATTED lcline lcSep "ERROR:New CliType is same as current clitype" SKIP.
      NEXT.
   END.

   IF new_CLIType.TariffType <> {&CLITYPE_TARIFFTYPE_MOBILEONLY} THEN 
   DO:
      PUT STREAM sout UNFORMATTED lcline lcSep "ERROR:Current CliType is Not a Mobile Tariff" SKIP.
      NEXT.
   END.

   IF (lcOldCLIType > "" AND lcOldCLIType NE MobSub.CliType) THEN 
   DO:
      PUT STREAM sout UNFORMATTED lcline lcSep "ERROR:Incorrect Old CLI type: "  MobSub.CliType SKIP.
      NEXT.
   END.

   
   FIND FIRST  MsRequest WHERE
               MsRequest.MsSeq = MobSub.MsSeq AND
               MsRequest.Reqtype = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
               LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9,99") = 0 NO-LOCK NO-ERROR.
   IF AVAIL MsRequest THEN 
   DO:
      PUT STREAM sout UNFORMATTED lcline lcSep "ERROR:Already pending STC request to " MsRequest.ReqCparam2 SKIP.
      NEXT.
   END.

   FIND FIRST  MsRequest WHERE
               MsRequest.MsSeq = MobSub.MsSeq AND
               MsRequest.Reqtype = {&REQTYPE_BUNDLE_CHANGE} AND
               LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9,99") = 0 AND
               LOOKUP(MsRequest.ReqCparam2,lcBONOContracts) = 0 NO-LOCK NO-ERROR.
   IF AVAIL MsRequest THEN 
   DO:
      PUT STREAM sout UNFORMATTED lcline lcSep "ERROR:Already pending Bundle change request " MsRequest.ReqCparam2 SKIP.
      NEXT.
   END.

   IF new_CLIType.PayType = 2 THEN liCreditcheck = 0.


   /* CREATE SubSer */
   liRequest =  fCTChangeRequest(MobSub.MsSeq,
                                 new_CLIType.Clitype,
                                 "",
                                 lcBankAccount,
                                 ldeTimeStamp,
                                 liCreditCheck,  /* 0 = Credit check ok */
                                 0, /* 0=no extend_term_contract, 1=extend_term_contract, 2=exclude_term_penalty */
                                 "",
                                 (ldeFee > 0),
                                 FALSE, /* Send SMS */
                                 Syst.Var:katun,
                                 ldeFee,
                                 {&REQUEST_SOURCE_SCRIPT},
                                 0,
                                 0,    /* Father request id */
                                 "",   /* For DMS usage contract_id, channel */
                                 OUTPUT lcInfo).

   IF liRequest = 0 THEN 
   DO:
      PUT STREAM sout UNFORMATTED lcline lcSep "ERROR:Request creation failed: " + lcInfo SKIP.
      NEXT.
   END.

   RETURN "OK".
END.
