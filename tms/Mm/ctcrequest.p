/* ----------------------------------------------------------------------
  MODULE .......: ctcrequest.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: 
  CREATED ......: 
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msreqfunc.i}
{Func/fmakemsreq.i}
{Syst/tmsconst.i}
{Func/fmakesms.i}
{Mm/fbundle.i}
{Mm/active_bundle.i}
{Func/add_lines_request.i}

DEFINE INPUT PARAMETER iiReqId AS INTEGER   NO-UNDO.
DEF BUFFER OldCliType FOR CliType.
DEF BUFFER NewCliType FOR CliType.

DEF VAR lcBankNumber  AS CHAR NO-UNDO.
DEF VAR liCreditCheck AS INT NO-UNDO.
DEF VAR ldeActStamp   AS DEC NO-UNDO FORMAT "99999999.99999".
DEF VAR liReq         AS INT NO-UNDO.
DEF VAR ocResult      AS CHAR NO-UNDO.
DEF VAR ldeSMSTime    AS DEC NO-UNDO. 
DEF VAR ldeEndStamp   AS DEC NO-UNDO. 
DEF VAR lcSender      AS CHAR NO-UNDO. 
DEF VAR llBBActive    AS LOG  NO-UNDO.
DEF VAR lcContract    AS CHAR NO-UNDO.
DEF VAR lcBundleName  AS CHAR NO-UNDO.

DEF VAR lcPostpaidVoiceTariffs AS CHAR NO-UNDO.
DEF VAR lcPrepaidVoiceTariffs  AS CHAR NO-UNDO.
DEF VAR lcOnlyVoiceContracts   AS CHAR NO-UNDO.
DEF VAR lcDataBundleCLITypes   AS CHAR NO-UNDO.
DEF VAR lcBONOContracts        AS CHAR NO-UNDO.

DEF BUFFER lbMobSub     FOR MobSub.
DEF BUFFER bMobSubCust  FOR MobSub.

FIND FIRST MsRequest WHERE MsRequest.MsRequest = iiReqId NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 0 THEN 
   RETURN "ERROR:Invalid request ID".

IF MsRequest.ReqStatus NE 0 THEN RETURN.

/* Check if there is ongoing reactivation request */
IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                   MsRequest.MsSeq EQ MsRequest.MsSeq AND
                   MsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_REACTIVATION} AND
                   LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0)
   THEN RETURN.

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

/* Validate request parameter entries */
FIND FIRST Mobsub WHERE 
           Mobsub.MSSeq = MSRequest.MSSEq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN DO:
   fReqError("MobSub not found").
   RETURN.
END.

/* nothing to do */
IF MsRequest.ReqCParam1 = MsRequest.ReqCParam2 OR
   MsRequest.ReqCParam1 = ""                   OR
   MsRequest.ReqCParam2 = "" THEN DO:

   fReqError("CLITypes are not valid").
   RETURN.
END.
   
/* check if request's CLIType is consistent with current one */
IF MsRequest.ReqCParam1 NE MobSub.CLIType THEN DO:
   fReqError("CLIType not consistent").
   RETURN.
END. 

FIND FIRST OldCliType WHERE 
           OldCliType.Brand   = Syst.Var:gcBrand AND 
           OldCliType.CliType = MSRequest.ReqCParam1 NO-LOCK NO-ERROR.
           
IF NOT AVAIL OldCliType THEN DO:
   fReqError("ERROR: Unknown old clitype " + MSRequest.ReqCParam1).
   RETURN.
END.

FIND FIRST NewCliType WHERE
           NewCliType.Brand   = Syst.Var:gcBrand AND
           NewCliType.CliType = MSRequest.ReqCParam2 NO-LOCK NO-ERROR.

IF NOT AVAIL NewCliType THEN DO:
   fReqError ("ERROR: Unknown new clitype " + MSRequest.ReqCParam2).
   RETURN.
END.

FIND Customer WHERE
     Customer.Custnum = MsRequest.Custnum NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN DO:
   fReqError("ERROR: Unknown customer " + STRING(MSRequest.Custnum)).
   RETURN.
END.

IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_FUSION_ORDER} AND
   MsRequest.ReqIParam2 > 0 THEN DO:
   FIND OrderFusion NO-LOCK WHERE
        OrderFusion.Brand = Syst.Var:gcBrand AND
        OrderFusion.OrderId = MsRequest.ReqIParam2 NO-ERROR.

   IF NOT AVAIL OrderFusion THEN DO:
      fReqError(SUBST("Order not found: &1", MsRequest.ReqIParam2)).
      RETURN.
   END.

   IF NOT OrderFusion.FixedNumber > "" THEN DO:
      fReqError("Fixed number is empty").
      RETURN.
   END.
END.


ASSIGN 
   lcBankNumber  = MSRequest.ReqCparam3 
   ldeActStamp   = MSrequest.ReqDParam1
   liCreditCheck = Msrequest.ReqIParam1.

IF lcBankNumber ne "" AND
   OldCliType.PayType = 2 AND
   NewCliType.PayType = 1 AND
   NOT CAN-FIND(FIRST bMobSubCust WHERE
                      bMobSubCust.Brand     = Syst.Var:gcBrand AND
                      bMobSubCust.MsSeq    <> MobSub.MsSeq AND
                      bMobSubCust.CustNum   = Customer.CustNum AND
                      bMobSubCust.PayType   = FALSE) THEN DO:

   liReq = fSubRequest
           (INPUT  MSRequest.MSSeq,
                   MSRequest.CLI,
                   MSRequest.CustNum,
                   FALSE,               /* Fees */
                   FALSE,               /* SendSMS */ 
                   MSRequest.UserCode,
                   Func.Common:mMakeTS(),                    
                   "BANKNUMBER",
                   lcBankNumber,
                   24,
                   MSRequest.MSrequest,
                   1,                   /* 1 = MAndatory */ 
           OUTPUT ocResult ).

   IF liReq EQ 0 THEN
      /* write possible error to an order memo */
      Func.Common:mWriteMemo("MobSub",
        STRING(MobSub.MsSeq),
        MobSub.Custnum,
        "BANK ACCOUNT CHANGE REQUEST FAILED",
        ocResult).
END.

/* Create possible Credit Check request, YDR-323 */
IF liCreditCheck EQ 1 AND
   liReq = 0 AND /* check that bank request with credit scoring was not created */
   MsRequest.ReqSource NE {&REQUEST_SOURCE_NEWTON} AND
   MsRequest.ReqSource NE {&REQUEST_SOURCE_EXTERNAL_API} AND
   MsRequest.ReqSource NE {&REQUEST_SOURCE_FUSION_ORDER}
   THEN DO:

   liReq = fSubRequest
   (INPUT  MSRequest.MSSeq,
           MSRequest.CLI,
           MSRequest.CustNum,
           FALSE,               /* Fees */
           FALSE,               /* SendSMS */
           MSRequest.UserCode,
           Func.Common:mMakeTS(),
           "CREDITCHECK",
           "",
           33,
           MSRequest.MSrequest,
           1,                   /* 1 = MAndatory */
    OUTPUT ocResult ).
   
   IF liReq EQ 0 THEN
      /* write possible error to an order memo */
      Func.Common:mWriteMemo("MobSub",
        STRING(MobSub.MsSeq),
        MobSub.custnum,
        "CREDIT CHECK REQUEST FAILED",
        ocResult).
END.

FIND MsRequest WHERE
     MsRequest.MsRequest = iiReqId  AND
     MsRequest.Brand     = Syst.Var:gcBrand NO-LOCK NO-ERROR.

/* Mark request as handled */
IF NOT fReqStatus(7,"") THEN DO:
   fReqError("ERROR: St. update failed.").  
   RETURN.
END.

/* Set new activation ts for clitype change */     
IF MSREquest.ReqDParam1 > MSRequest.ActStamp OR
   MSRequest.ReqSource = {&REQUEST_SOURCE_FUSION_ORDER} OR
   MSRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION} OR
   MSRequest.ReqSource = {&REQUEST_SOURCE_MAIN_LINE_DEACTIVATION} THEN
     ldeActStamp = MsRequest.ReqDParam1.
ELSE ldeActStamp = MsRequest.ActStamp.

IF NOT (MsRequest.ReqSource = "5" AND MsRequest.UserCode begins "Qvantel") THEN DO:

/* Send SMS if BB is being deactivated during STC */
FIND FIRST SubSer WHERE
           SubSer.ServCom = "BB"         AND
           SubSer.MsSeq   = MobSub.MsSeq AND
           SubSer.SsDate <= TODAY NO-LOCK NO-ERROR.
IF AVAILABLE SubSer AND SubSer.SSStat = 1 THEN llBBActive = TRUE.

/* Fetch active bundles */
lcContract = fGetActiveSpecificBundle(MsRequest.MsSeq,ldeActStamp,"BONO").

IF lcContract > "" THEN DO:

   IF LOOKUP(lcContract, fCParamC("ALLOWED_BONO_STC_CONTRACTS")) = 0 OR
      (fMatrixAnalyse(Syst.Var:gcBrand,
                     "PERCONTR",
                     "PerContract;SubsTypeTo",
                     lcContract + ";" + MsRequest.ReqCparam2,
                     OUTPUT lcReqChar) NE 1 AND
      ENTRY(1,lcReqChar,";") NE "?") THEN DO:
   
      lcSMSText = fGetSMSTxt("BundleTerminate",
                             TODAY,
                             Customer.Language,
                             OUTPUT ldeSMSTime).

      IF fConvBundleToCommName(INPUT lcContract,
                               OUTPUT lcBundleName,
                               OUTPUT lcSender) THEN
         lcSMSText = REPLACE(lcSMSText,"#BUNDLE",lcBundleName).
      ELSE lcSMSText = "".
         
      /* both to agreement cust and user */
      IF lcSMSText > "" THEN
         fMakeSchedSMS2(MsRequest.CustNum,
                        MsRequest.CLI,
                        {&SMSTYPE_INFO},
                        lcSMSText,
                        ldeSMSTime,
                        lcSender,
                        "").
   END.
END.

/* Send Pre-BB Deactivation SMS */
IF llBBActive THEN DO:

   ASSIGN 
      lcPostpaidVoiceTariffs = fCParamC("POSTPAID_VOICE_TARIFFS")
      lcPrepaidVoiceTariffs  = fCParamC("PREPAID_VOICE_TARIFFS")
      lcDataBundleCLITypes   = fCParamC("DATA_BUNDLE_BASED_CLITYPES")
      lcOnlyVoiceContracts   = fCParamC("ONLY_VOICE_CONTRACTS")
      lcBONOContracts        = fCParamC("BONO_CONTRACTS").

   IF LOOKUP(MsRequest.ReqCParam1,lcPrepaidVoiceTariffs) = 0 AND
      LOOKUP(MsRequest.ReqCParam2,lcPrepaidVoiceTariffs) > 0 AND
      MsRequest.ReqCParam2 <> "TARJ6" THEN /* BB allowed for TARJ6 */
      lcSMSText = "BBDeActSTCPreV_1".
   ELSE IF LOOKUP(MsRequest.ReqCParam1,"TARJ6,TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 THEN
      lcSMSText = "BBDeActSTCPreV_1".
   ELSE IF MsRequest.ReqCParam2 = "TARJRD1" THEN
      lcSMSText = "BBDeActSTCPreD_1".
   ELSE IF LOOKUP(MsRequest.ReqCParam2,lcPostpaidVoiceTariffs) > 0 THEN DO:
      IF LOOKUP(lcContract,lcBONOContracts) = 0 AND
         LOOKUP(MsRequest.ReqCParam2,lcDataBundleCLITypes) = 0 THEN DO:
         IF MsRequest.ReqCParam5 = "" OR
            LOOKUP(MsRequest.ReqCParam5,lcOnlyVoiceContracts) > 0 THEN
            lcSMSText = "BBDeActSTCPostV1".
      END. /* IF LOOKUP(lcContract,lcBONOContracts) = 0 AND */
   END. /*  ELSE IF LOOKUP(MsRequest.ReqCParam2,lcPostpaidVoiceTariffs) */
   ELSE lcSMSText = "".

   IF lcSMSText > "" THEN DO:
      
      lcSMSText = fGetSMSTxt(lcSMSText,
                             TODAY,
                             Customer.Language,
                             ldeSMSTime).
      IF lcSMSText > "" THEN
         fMakeSchedSMS2(MsRequest.CustNum,
                        MsRequest.CLI,
                        11,
                        lcSMSText,
                        ldeSMSTime,
                        {&BB_SMS_SENDER},
                        "").
   END.
END. /* IF llBBActive THEN DO: */

/* SMS to Primary line */
IF MobSub.MultiSimID > 0 AND
   MobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} THEN DO:
   FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
              lbMobSub.Brand  = Syst.Var:gcBrand AND
              lbMobSub.MultiSimID = MobSub.MultiSimID AND
              lbMobSub.MultiSimType NE MobSub.MultiSimType AND
              lbMobSub.Custnum = MobSub.Custnum NO-ERROR.
   IF AVAIL lbMobSub THEN DO:
      lcSMSText = fGetSMSTxt("MultiSIMSecondarySTC",
                             TODAY,
                             Customer.Language,
                             OUTPUT ldeSMSTime).

      IF lcSMSText > "" THEN
         fMakeSchedSMS2(lbMobSub.CustNum,
                        lbMobSub.CLI,
                        {&SMSTYPE_INFO},
                        lcSMSText,
                        ldeSMSTime,
                        "22622",
                        "").
   END. /* IF AVAIL lbMobSub THEN DO: */
END. /* IF MobSub.MultiSimID > 0 AND */
END.

FIND MsRequest WHERE
     MsRequest.MsRequest = iiReqId  AND
     MsRequest.Brand     = Syst.Var:gcBrand EXCLUSIVE-LOCK NO-ERROR.

/* Set new activation ts for clitype change */     
IF MSREquest.ReqDParam1 > MSRequest.ActStamp OR 
   MSRequest.ReqSource = {&REQUEST_SOURCE_FUSION_ORDER} OR
   MSRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION} OR
   MSRequest.ReqSource = {&REQUEST_SOURCE_MAIN_LINE_DEACTIVATION} THEN
   MSRequest.ActStamp =   MSREquest.ReqDParam1.

/* Check sub-requests */      
IF fChkSubRequest(MSrequest.MSRequest) THEN  fReqStatus(8,"").          

/* YDR-1847 */
fAdditionalLineSTC(iiReqId,
                   MSRequest.ActStamp,
                   "STC").
