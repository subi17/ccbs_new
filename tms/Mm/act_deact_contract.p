/* ----------------------------------------------------------------------
  module .......: Mm/act_deact_contract.p
  task .........: Activate/Deactivate contracts from Backdoor tool
  application ..: tms
  author .......: vikas
  created ......: 06.09.12
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/fmakemsreq.i}
{Func/mdub.i}
{Func/service.i}
{Func/fprepaidfee.i}
{Mm/active_bundle.i}
{Mm/fbundle.i}

/* files and dirs */
DEF VAR lcLine           AS CHAR NO-UNDO.
DEF VAR lcLogFile        AS CHAR NO-UNDO. 
DEF VAR lcFileName       AS CHAR NO-UNDO. 
DEF VAR lcIncDir         AS CHAR NO-UNDO. 
DEF VAR lcInputFile      AS CHAR NO-UNDO. 
DEF VAR lcProcDir        AS CHAR NO-UNDO. 
DEF VAR lcProcessedFile  AS CHAR NO-UNDO. 
DEF VAR lcSpoolDir       AS CHAR NO-UNDO. 
DEF VAR lcReportFileOut  AS CHAR NO-UNDO. 
DEF VAR lcOutDir         AS CHAR NO-UNDO. 
DEF VAR lcToday          AS CHAR NO-UNDO.
DEF VAR lcTime           AS CHAR NO-UNDO.
DEF VAR lcSep            AS CHAR NO-UNDO INIT ";".
DEF VAR ldEndStamp       AS DEC  NO-UNDO.
DEF VAR ldaActDate       AS DATE NO-UNDO.
DEF VAR ldePMDUBFee      AS DEC  NO-UNDO.
DEF VAR lcBONOContracts  AS CHAR NO-UNDO.
DEF VAR liRead           AS INT NO-UNDO. 
DEF VAR liErrors         AS INT NO-UNDO. 

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

ASSIGN
   lcIncDir    = fCParam("ContractBackTool","IncDir") 
   lcProcDir   = fCParam("ContractBackTool","IncProcDir")
   lcSpoolDir  = fCParam("ContractBackTool","OutSpoolDir")
   lcOutDir    = fCParam("ContractBackTool","OutDir")
   ldePMDUBFee = fgetPrepaidFeeAmount("PMDUB", TODAY)
   lcToday     = STRING(YEAR(TODAY),"9999") + 
                 STRING(MONTH(TODAY),"99")  +
                 STRING(DAY(TODAY),"99")
   lcTime      = REPLACE(STRING(TIME,"hh:mm:ss"),":","")
   lcBONOContracts = fCParamC("BONO_CONTRACTS").


FUNCTION fLogLine RETURNS LOG(icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine lcSep 
      icMessage SKIP.

END FUNCTION.


FUNCTION fHandleContract RETURNS CHAR(INPUT icContract   AS CHAR,
                                      INPUT icAction     AS CHAR):

   DEF VAR liRequest     AS INT  NO-UNDO.
   DEF VAR lcResult      AS CHAR NO-UNDO.
   DEF VAR llResult      AS LOG  NO-UNDO.
   DEF VAR liReturnValue AS INT  NO-UNDO.
   DEF VAR ldeFirstSecond AS DEC NO-UNDO. 
   DEF VAR ldEndDate AS DATE NO-UNDO.
   DEF VAR ldeActStamp      AS DEC  NO-UNDO.

   DEF BUFFER bMsRequest FOR MsRequest.

   /* Check if subscription type is not compatible with bundle */
   CASE icAction:
      WHEN "1" THEN DO:

         ldeActStamp = Func.Common:mMakeTS().

         IF fMatrixAnalyse(Syst.Var:gcBrand,
                           "PERCONTR",
                           "PerContract;SubsTypeTo",
                           icContract + ";" + MobSub.CLIType,
                           OUTPUT lcResult) NE 1 THEN
            RETURN "ERROR:Contract is not allowed for this subscription type".

         IF LOOKUP(icContract,lcBONOContracts) > 0 AND
            NOT fAllowMDUBActivation("") THEN
            RETURN "ERROR:Contract activation is not allowed".

         /* DATA200_UPSELL and DSS200_UPSELL can't be 
            activate from TMS CUI, Vista/VFR */
         IF icContract = "DATA200_UPSELL" OR
            icContract = "DSS200_UPSELL" THEN
            RETURN SUBST("&1 activation is not allowed", icContract).

         /* YPR */
         IF icContract EQ "VOICE3000" THEN DO:
            
            ldeFirstSecond = Func.Common:mMake2DT(DATE(MONTH(TODAY),1,YEAR(TODAY)),0).

            FIND FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE
                       MsOwner.MsSeq = Mobsub.MsSeq NO-ERROR.
            IF MsOwner.TsBegin > ldeFirstSecond THEN
               ldeActStamp = MsOwner.TSBegin.
            ELSE ldeActStamp = ldeFirstSecond.
         END.

         /* Validate Prepaid Balance before making PMDUB activation request */
         IF icContract = {&PMDUB} THEN DO:
            RUN pEnoughBalance(INPUT MobSub.CLI,
                               INPUT ldePMDUBFee,
                               OUTPUT llResult).
            IF llResult = FALSE THEN
               RETURN "ERROR:Contract activation is not allowed due to not enough balance".
         END. /* IF icContract = {&PMDUB} THEN DO: */
      END. /* WHEN "1" THEN DO: */
      WHEN "0" THEN DO:
         IF LOOKUP(icContract,lcBONOContracts) > 0 THEN DO:
            IF NOT fAllowMDUBTermination("") THEN
               RETURN "ERROR:Contract termination is not allowed".
            /* Ongoing BTC with upgrade upsell */
            ELSE IF fOngoingBTC(INPUT MobSub.MsSeq,
                                INPUT icContract,
                                INPUT TRUE) THEN
               RETURN "ERROR:Bundle termination is not allowed since " +
                      "subscription has ongoing BTC with upgrade upsell".
         END. /* IF LOOKUP(icContract,lcBONOContracts) > 0 THEN DO: */
   
         ASSIGN ldEndDate   = Func.Common:mLastDayOfMonth(TODAY)
                ldeActStamp = Func.Common:mMake2DT(ldEndDate,86399).

      END. /* WHEN "0" THEN DO: */
   END CASE. /* CASE icAction: */

   liRequest = fPCActionRequest(MobSub.MsSeq,
                                icContract,
                                IF icAction = "1" THEN "act" ELSE "term",
                                ldeActStamp,
                                TRUE,    /* fees */
                                {&REQUEST_SOURCE_SCRIPT},
                                "",   /* creator */
                                0,    /* no father request */
                                FALSE,
                                "",
                                0,
                                0,
                                "",
                                OUTPUT lcResult).
   
   IF liRequest = 0 THEN
      RETURN "ERROR:Contract request not created: " + lcResult.
   ELSE IF icContract EQ "VOICE3000" AND icAction = "1" THEN DO:
      FIND FIRST bMsRequest WHERE
                 bMsRequest.MsRequest = liRequest EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bMsRequest THEN
         bMsRequest.ReqIParam4 = 1. /* force rerate */
      RELEASE bMsRequest.
   END.

   RETURN "OK".
   
END FUNCTION.

FUNCTION fHandleService RETURNS CHAR(INPUT icService AS CHAR,
                                     INPUT icAction  AS CHAR):

   DEF VAR liValue     AS INT  NO-UNDO.
   DEF VAR liReq       AS INT  NO-UNDO.
   DEF VAR lcError     AS CHAR NO-UNDO.
   DEF VAR lcParam     AS CHAR NO-UNDO.

   DEFINE VARIABLE ldeActStamp AS DECIMAL NO-UNDO. 
   ldeActStamp = Func.Common:mMakeTS().

   CASE icAction:
      WHEN "1" THEN DO:
         FIND FIRST SubSer USE-INDEX ServCom WHERE
                    SubSer.MsSeq   = MobSub.MsSeq AND
                    SubSer.ServCom = icService NO-LOCK NO-ERROR.

         IF AVAIL SubSer AND SubSer.SSStat = 1 AND
            NOT (lcParam > "" AND SubSer.SSParam NE lcParam) THEN
            RETURN "ERROR:Service is already active".

         IF icService EQ "BB" THEN DO:
            IF MobSub.CLIType = "TARJRD1" THEN
               RETURN "ERROR:Service is not allowed for this subscription type".

            IF NOT fIsBBAllowed(Mobsub.MsSeq,ldeActStamp) THEN
               RETURN "ERROR:Service can not be activated since " +
                      "subscription does not have active data bundle".

            IF AVAIL SubSer AND SubSer.SSStat = 2 THEN lcParam = "3".

         END. /* IF icService EQ "BB" THEN DO: */

         liValue = 1.

      END. /* WHEN "1" THEN DO: */
      WHEN "0" THEN DO:
         liValue = 0.
      
         FIND FIRST SubSer USE-INDEX ServCom WHERE
                    SubSer.MsSeq   = MobSub.MsSeq AND
                    SubSer.ServCom = icService NO-LOCK NO-ERROR.
         IF NOT AVAIL SubSer OR SubSer.SSStat = 0 OR SubSer.SSStat = 2 THEN
            RETURN "ERROR:Service is already inactive".

         IF icService EQ "BB" THEN liValue = 2.

      END. /* WHEN "0" THEN DO: */
   END CASE.

   /* Check ongoing service requests */
   IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq      = MobSub.MsSeq AND
                     MsRequest.ReqType    = ({&REQTYPE_SERVICE_CHANGE}) AND
                     MsRequest.ReqCParam1 = icService AND
               LOOKUP(STRING(MsRequest.ReqStat),{&REQ_INACTIVE_STATUSES}) = 0)
   THEN RETURN "ERROR:Service can not be changed due to ongoing network command".

   liReq = fServiceRequest(MobSub.MsSeq,
                           icService,
                           liValue,
                           lcParam,
                           ldeActStamp,
                           "",
                           TRUE,       /* fees */
                           FALSE,      /* sms */
                           "",
                           {&REQUEST_SOURCE_SCRIPT},
                           0, /* father request */
                           FALSE, /* mandatory for father request */
                           OUTPUT lcError).
         
   IF liReq = 0 THEN
      RETURN "ERROR:Service request not created: " + lcError.

   RETURN "OK".
   
END FUNCTION.


/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName. 
   lcInputFile = lcIncDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:
      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.
   
      /* To prevent duplicate file handling (YTS-5280) */
      IF CAN-FIND (FIRST ActionLog NO-LOCK WHERE
                         ActionLog.Brand = Syst.Var:gcBrand AND
                         ActionLog.TableName = "Cron" AND
                         ActionLog.KeyValue = lcFileName AND
                         ActionLog.ActionID = "ContractBOB" AND
                         ActionLog.ActionStatus = 0) THEN NEXT.

      DO TRANS:
         CREATE ActionLog.
         ASSIGN 
            ActionLog.Brand        = Syst.Var:gcBrand   
            ActionLog.TableName    = "Cron"  
            ActionLog.KeyValue     = lcFileName
            ActionLog.ActionID     = "ContractBOB"
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                     MONTH(TODAY)
            ActionLog.ActionStatus = 0
            ActionLog.ActionTS     = Func.Common:mMakeTS().
      END.

      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   lcLogFile = lcSpoolDir + "act_deact_contract_" +
               lcToday + "_" + lcTime + ".log".
   OUTPUT STREAM sLog TO VALUE(lcLogFile).
   fBatchLog("START", lcLogFile).

   LINE_LOOP:
   REPEAT:
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine = "" OR lcLine = ? THEN NEXT LINE_LOOP.

      RUN pCheckContract(INPUT lcLine).
   
      IF RETURN-VALUE BEGINS "ERROR" THEN liErrors = liErrors + 1.
      liRead = liRead + 1.
      
      fLogLine(RETURN-VALUE).

   END. /* REPEAT: LINE_LOOP: */
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.
   
   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir). 
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
   
   DO TRANS:
      ASSIGN 
         ActionLog.ActionDec    = liRead
         ActionLog.ActionChar   = "Read: " + STRING(liRead) + 
                                  " Errors: " + STRING(liErrors) + 
                                  " Succesful: " + STRING(liRead - liErrors) + 
                                  CHR(10) + "Finished: " + Func.Common:mTS2HMS(Func.Common:mMakeTS())
         ActionLog.ActionStatus = 3.
   END.
   
END. /* REPEAT: */

INPUT STREAM sFile CLOSE.


PROCEDURE pCheckContract: 

   DEF INPUT PARAMETER pcLine AS CHAR NO-UNDO. 

   /* local variables */
   DEF VAR lcCLI              AS CHAR NO-UNDO.
   DEF VAR lcContract         AS CHAR NO-UNDO.
   DEF VAR lcInputAction      AS CHAR NO-UNDO.
   DEF VAR lcError            AS CHAR NO-UNDO.

   IF NUM-ENTRIES(pcLine,lcSep) <> 5 THEN
      RETURN "ERROR:Wrong file format".

   ASSIGN
      lcCLI          = TRIM(ENTRY(1,pcLine,lcSep))
      lcContract     = TRIM(ENTRY(2,pcLine,lcSep))
      lcInputAction  = TRIM(ENTRY(3,pcLine,lcSep)).

   IF lcInputAction = ? OR
      LOOKUP(lcInputAction,"0,1") = 0 THEN
      RETURN "ERROR:Invalid action value".

   /* check invoice */
   FIND MobSub WHERE 
        MobSub.Brand = Syst.Var:gcBrand AND
        MobSub.CLI   = lcCLI NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN "ERROR:Invalid MSISDN".

   FIND Customer WHERE
        Customer.CustNum = MobSub.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN RETURN "ERROR:Customer not found".

   IF LOOKUP(lcContract,lcBONOContracts +
             ",VOICE3000,VOICE100,VOICE200,VOICE200B") > 0 THEN DO:
      lcError = fHandleContract(lcContract,lcInputAction).
   END. /* IF LOOKUP(lcContract,lcBONOContracts */
   ELSE IF LOOKUP(lcContract,"BB,LTE") > 0 THEN
      lcError = fHandleService(lcContract,lcInputAction).
   ELSE lcError = "ERROR:Invalid contract type".

   IF lcError <> "OK" THEN RETURN lcError.

   RETURN "OK".

END PROCEDURE.

