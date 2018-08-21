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
{Func/fcustpl.i}
{Func/penaltyfee.i}
{Func/fmakesms.i}

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
DEFINE VARIABLE lcCustIDType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCustID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMsisdn        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOldCLIType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcNewCLIType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSMSText2      AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldtSTCDate      AS DATE      NO-UNDO.
DEFINE VARIABLE lcError         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcinfo          AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldtValidSTCDates AS DATE     NO-UNDO EXTENT 2.

DEFINE BUFFER old_CLIType FOR CLIType.
DEFINE BUFFER new_CLIType FOR CLIType.

FUNCTION fSTC RETURNS CHARACTER 
   (pcCustIDType     AS CHARACTER,
    pcCustID         AS CHARACTER,
    pcMsisdn         AS CHARACTER,
    pcOldCLIType     AS CHARACTER,
    pcNewCLIType     AS CHARACTER,
    pcSMSText        AS CHARACTER,
    pdtSTCDate       AS DATE)
    FORWARD.

ASSIGN
   lcRootDir            = fCParam("StcMassiveBob", "RootDir")
   lcIncDir             = fCParam("StcMassiveBob", "IncDir")
   lcProcDir            = fCParam("StcMassiveBob", "IncProcessDir")
   lcSpoolDir           = fCParam("StcMassiveBob", "OutSpoolDir")
   lcOutDir             = fCParam("StcMassiveBob", "OutDir")
   ldtValidSTCDates[1]  = TODAY + 1
   ldtValidSTCDates[2]  = Func.Common:mFirstDayOfNextMonth(TODAY)
   .

IF NOT (lcRootDir > "" AND lcIncDir > "" AND lcProcDir > "" AND lcSpoolDir > "" AND lcOutDir > "")
THEN RETURN.

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

   ENTRY(6, lcLine, lcSep) = "" NO-ERROR. /* Remove Sms text in Error Cases */
   fLogLine("KO:" + icMessage).
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
         lcCustIDType   = TRIM(ENTRY(1,lcLine,lcSep))
         lcCustID       = TRIM(ENTRY(2,lcLine,lcSep))
         lcMsisdn       = TRIM(ENTRY(3,lcLine,lcSep))
         lcOldCLIType   = TRIM(ENTRY(4,lcLine,lcSep))
         lcNewCLIType   = TRIM(ENTRY(5,lcLine,lcSep))
         lcSMSText2     = TRIM(ENTRY(6,lcLine,lcSep))
         ldtSTCDate     = DATE(TRIM(ENTRY(7,lcLine,lcSep)))
         NO-ERROR.

      IF ERROR-STATUS:ERROR OR liEntries NE 7 THEN 
      DO:
         fError("Incorrect input data format. " + STRING(liEntries) + lcSep + STRING(ERROR-STATUS:ERROR) ).
         liNumErr = liNumErr + 1 .
         NEXT.
      END.


      lcError =    fSTC(lcCustIDType,
                        lcCustID,
                        lcMsisdn,
                        lcOldCLIType,
                        lcNewCLIType,
                        lcSMSText2,
                        ldtSTCDate
                        ) NO-ERROR.

      IF lcError = "OK" THEN DO:
         fLogLine("OK").
         liNumOK = liNumOK + 1.
      END.
      ELSE DO:
         fError(lcError).
         liNumErr = liNumErr + 1 .
      END.
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
FUNCTION fSTC RETURN CHARACTER
   (pcCustIDType     AS CHARACTER,
    pcCustID         AS CHARACTER,
    pcMsisdn         AS CHARACTER,
    pcOldCLIType     AS CHARACTER,
    pcNewCLIType     AS CHARACTER,
    pcSMSText        AS CHARACTER,
    pdtSTCDate       AS DATE
   ):

   DEFINE VARIABLE liCreditcheck    AS INTEGER   NO-UNDO INIT 1.
   DEFINE VARIABLE liRequest        AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcPenaltyMsg     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldeSTCStamp      AS DECIMAL   NO-UNDO.

   FIND  Customer  NO-LOCK WHERE
         Customer.Brand       = Syst.Var:gcBrand   AND
         Customer.CustIDType  = pcCustIDType       AND
         Customer.OrgId       = pcCustID     NO-ERROR.
   IF NOT AVAILABLE Customer THEN 
      RETURN "Customer not found with given CustIDType and CustID".
   
   FIND  MobSub NO-LOCK WHERE
         MobSub.CLI = pcMsisdn NO-ERROR.
   IF NOT AVAILABLE MobSub THEN 
      RETURN "MobSub not found".

   IF Customer.CustNum <> MobSub.CustNum THEN 
      RETURN "Customer given doesn't match with Mobile Subscription's Customer".

   IF Customer.CustIDType = "CIF" THEN liCreditcheck = 0.

   FIND FIRST  old_CLIType WHERE
               old_CLIType.Clitype = pcOldCLIType  AND
               old_CLIType.Brand   = Syst.Var:gcBrand NO-LOCK NO-ERROR.
   IF NOT AVAILABLE old_CLIType THEN 
      RETURN "Current CliType not found".
   
   IF old_CLIType.TariffType <> {&CLITYPE_TARIFFTYPE_MOBILEONLY} THEN 
      RETURN "Current CliType not Mobile".

   IF (pcOldCLIType > "" AND pcOldCLIType NE MobSub.CliType) THEN 
      RETURN "Incorrect Old CLI type: "  + MobSub.CliType .

   FIND FIRST  new_CLIType WHERE
               new_CLIType.Clitype = pcNewCLIType AND
               new_CLIType.Brand   = Syst.Var:gcBrand NO-LOCK NO-ERROR.
   IF NOT AVAILABLE new_CLIType THEN 
      RETURN "New CliType not found".

   IF new_CLIType.Clitype = MobSub.CliType THEN 
      RETURN "New CliType is same as current clitype".

   IF new_CLIType.TariffType <> {&CLITYPE_TARIFFTYPE_MOBILEONLY} THEN 
      RETURN "New CliType is Not a Mobile Tariff".

   IF CAN-FIND (FIRST   MsRequest WHERE
                        MsRequest.MsSeq   = MobSub.MsSeq AND
                        MsRequest.Reqtype = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                        LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0)
   THEN
      RETURN "Already pending STC Request".

   IF CAN-FIND (FIRST   MsRequest WHERE
                        MsRequest.MsSeq   = MobSub.Msseq AND
                        MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                        LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0)
   THEN
      RETURN "Already pending Termination Request".

   IF new_CLIType.PayType = 2 THEN liCreditcheck = 0.

   IF pcSMSText = "" THEN
      RETURN "SMS text is Blank".

   IF pdtSTCDate = ? OR NOT (pdtSTCDate = ldtValidSTCDates[1] OR pdtSTCDate = ldtValidSTCDates[2]) THEN
      RETURN "Not a Valid STC Date". 

   lcPenaltyMsg = fCalcPenalty(MobSub.MsSeq, new_CLIType.CliType).
   IF lcPenaltyMsg <> "" THEN
      RETURN "STC not done for penalties:" + lcPenaltyMsg.
   
   ldeSTCStamp = Func.Common:mDate2TS(pdtSTCDate).
   /* CREATE SubSer */
   liRequest =  fCTChangeRequest(MobSub.MsSeq,
                                 new_CLIType.Clitype,
                                 "",
                                 "",/* Bank Account */
                                 ldeSTCStamp,
                                 liCreditCheck,  /* 0 = Credit check ok */
                                 0, /* 0=no extend_term_contract, 1=extend_term_contract, 2=exclude_term_penalty */
                                 "",
                                 FALSE, /* (ldeFee > 0) */
                                 FALSE, /* Send SMS */
                                 Syst.Var:katun,
                                 0, /* ldeFee */
                                 {&REQUEST_SOURCE_SCRIPT},
                                 0,
                                 0,    /* Father request id */
                                 "",   /* For DMS usage contract_id, channel */
                                 OUTPUT lcInfo).

   IF liRequest = 0 THEN 
      RETURN "Request creation failed: " + lcInfo.
   ELSE DO:

      Func.Common:mWriteMemoWithType("Mobsub",
                                     STRING(MobSub.msseq),
                                     MobSub.Custnum,
                                     "Cambio de tarifa masivo",
                                     old_CLIType.CliName + " --> " + new_CLIType.CliName + " - " + STRING(pdtSTCDate,"99-99-9999"),
                                     "MobSub",
                                     "SYSTEM"
                                    ).

      fMakeSchedSMS2(MobSub.CustNum,
                     MobSub.CLI,
                     {&SMSTYPE_STC},
                     pcSMSText,
                     ldeSTCStamp,
                     {&STC_SMS_SENDER},
                     "").

   END.

   RETURN "OK".
END FUNCTION.