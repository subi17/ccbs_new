&IF "{&fbtc}" NE "YES" 
&THEN

&GLOBAL-DEFINE fbtc YES

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/fcreatereq.i}
{Func/cparam2.i}
{Mm/fbundle.i}
{Func/stc_extension.i}
{Func/fctchange.i}

FUNCTION fBundleChangeRequest RETURNS INTEGER
   (INPUT iiMsSeq         AS INT,    /* subscription */
    INPUT icOldBundle     AS CHAR,   /* old (current) bundle */ 
    INPUT icNewBundle     AS CHAR,   /* new bundle */
    INPUT idActStamp      AS DEC,    /* when request should be handled */
    INPUT icSource        AS CHAR,
    INPUT icCreator       AS CHAR,   
    INPUT ilCreateFees    AS LOG,    
    INPUT iiOrigRequest   AS INT,    /* main request */
    INPUT ilMandatory     AS LOG,    /* is subrequest mandatory */
    INPUT ilUpgradeUpsell AS LOG,    /* is upgrade upsell */
    INPUT ilExtendTerm    AS INT,    /* extend terminal contract */
    INPUT icDMSInfo       AS CHAR,   /* For DMS usage, contract_id*/
    OUTPUT ocResult       AS CHAR):
 
   DEF VAR liReqCreated    AS INT  NO-UNDO.

   DEF BUFFER bMobSub   FOR MobSub.

   FIND FIRST bMobSub WHERE
              bMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL bMobSub THEN DO:
      ocResult = "MobSub not found".
      RETURN 0.
   END. /* IF NOT AVAIL bMobSub THEN DO: */

   ocResult = fChkRequest(iiMsSeq,
                          {&REQTYPE_BUNDLE_CHANGE},
                          icOldBundle,
                          icCreator).
   IF ocResult > "" THEN RETURN 0.

   /* Pre-check for IPL -> IPL BTC/STC */
   RUN Mm/requestaction_check.p(INPUT {&REQTYPE_BUNDLE_CHANGE},
                           INPUT bMobSub.CLIType,
                           INPUT bMobsub.MsSeq,
                           INPUT icSource,
                           OUTPUT ocResult).
   IF ocResult > "" THEN RETURN 0.

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = Func.Common:mMakeTS().

   /* double check (duplicate RPC call) */
   ocResult = fChkRequest(iiMsSeq,
                          {&REQTYPE_BUNDLE_CHANGE},
                          icOldBundle,
                          icCreator).
   IF ocResult > "" THEN RETURN 0.

   fCreateRequest({&REQTYPE_BUNDLE_CHANGE},
                  idActStamp,
                  icCreator,
                  ilCreateFees, 
                  FALSE).   /* sms */

   ASSIGN
      bCreaReq.MsSeq       = iiMsSeq 
      bCreaReq.ReqCParam1  = icOldBundle
      bCreaReq.ReqCParam2  = icNewBundle
      bCreaReq.ReqSource   = icSource
      bCreaReq.OrigRequest = iiOrigRequest
      bCreaReq.Mandatory   = INTEGER(ilMandatory)
      bCreaReq.ReqIParam5  = ilExtendTerm
      liReqCreated         = bCreaReq.MsRequest
      bCreaReq.ReqCParam5  = (icOldBundle + "TO" + icNewBundle)
                             WHEN ilUpgradeUpsell = TRUE
      bCreaReq.ReqCparam6  = icDMSInfo.

   RELEASE bCreaReq.

   /* initial actions */
   RUN Mm/requestaction_init.p (liReqCreated).
   
   /* Send the SMS using Request Action Rules for BTC */
   RUN Mm/requestaction_sms.p(INPUT liReqCreated,
                           INPUT bMobSub.CLIType,
                           INPUT icSource).

   RETURN liReqCreated.
     
END FUNCTION.


FUNCTION fIsBTCAllowed RETURNS LOGIC
   (iiMsSeq   AS INT,
    icDCEvent AS CHAR,
    OUTPUT ocInfo AS CHAR):

   DEF VAR lcResult         AS CHAR NO-UNDO.
   DEF VAR lcBONOContracts  AS CHAR NO-UNDO.
   
   DEF BUFFER bSTC FOR MsRequest.

   lcBONOContracts = fCParamC("BONO_CONTRACTS").

   /* BONO BTC request is not allowed if have STC request for VOICE to OTHER */
   IF LOOKUP(icDCEvent,lcBONOContracts) > 0 THEN
   FOR FIRST bSTC NO-LOCK WHERE
             bSTC.MsSeq   = iiMsSeq AND
             bSTC.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
             LOOKUP(STRING(bSTC.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0:
             
      IF fMatrixAnalyse(Syst.Var:gcBrand,
                        "BTC-DENY",
                        "SubsTypeFrom;SubsTypeTo",
                        bSTC.ReqCParam1 + ";" + bSTC.ReqCParam2,
                        OUTPUT lcResult) = 0 THEN DO:
         ocInfo = "Subscription has a pending STC. Bundle change not allowed".
         RETURN FALSE.
      END.
   END.

   RETURN TRUE.

END FUNCTION.

FUNCTION fIsBTCBundleAllowed RETURNS LOGIC
   (piMsSeq        AS INT,
    pcCLIType      AS CHAR,
    pcOldBundle    AS CHAR,
    pcNewBundle    AS CHAR,
    pdaActDate     AS DATE,
    OUTPUT ocError AS CHAR):

   DEF VAR lcResult         AS CHAR NO-UNDO.
   DEF VAR lcActiveBundles  AS CHAR NO-UNDO.
   DEF VAR ldActStamp       AS DEC  NO-UNDO.
   DEF VAR lcIPLContracts   AS CHAR NO-UNDO.
   DEF VAR lcFLATContracts  AS CHAR NO-UNDO.
   DEF VAR lcBONOContracts  AS CHAR NO-UNDO.
   DEF VAR lcCONTSContracts AS CHAR NO-UNDO.
   DEF VAR lcCONTSFContracts AS CHAR NO-UNDO.

   ldActStamp = Func.Common:mMake2DT(pdaActDate,0).
  
   IF pcOldBundle = "" OR pcNewBundle = "" OR 
      pcOldBundle = pcNewBundle THEN DO:
      ocError = "Nothing to do".
      RETURN FALSE.
   END.

   IF pcOldBundle = "DUB" OR pcNewBundle = "DUB" THEN DO:
      ocError = "DUB is not considered as exchangable bundle".
      RETURN FALSE.
   END. /* IF pcOldBundle = "DUB" THEN DO: */

   lcActiveBundles = fGetActiveBundle(piMsSeq,ldActStamp).

   IF lcActiveBundles = "" THEN DO:
      ocError = "No active bundles for subscription".
      RETURN FALSE.
   END.

   IF LOOKUP(pcOldBundle,lcActiveBundles) = 0 THEN DO:
      ocError = "Bundle is not currently active".
      RETURN FALSE.
   END.
   
   IF NOT fIsBundle(pcNewBundle) OR
      NOT fIsBundle(pcOldBundle)THEN DO:
      ocError = "Not a bundle".
      RETURN FALSE.
   END.
 
   /* is the new bundle allowed */
   IF fMatrixAnalyse(Syst.Var:gcBrand,
                     "PERCONTR",
                     "PerContract;SubsTypeTo",
                     pcNewBundle + ";" + pcCLIType,
                     OUTPUT lcResult) NE 1 AND
      ENTRY(1,lcResult,";") NE "?"  
   THEN DO:
      ocError = "Bundle is not allowed for this subscription type " + pcNewBundle + " " + pcCLIType.
      RETURN FALSE.
   END.

   ASSIGN lcIPLContracts   = fCParamC("IPL_CONTRACTS")
          lcFLATContracts  = fCParamC("FLAT_CONTRACTS")
          lcCONTSContracts = fCParamC("CONTS_CONTRACTS")
          lcBONOContracts  = fCParamC("BONO_CONTRACTS")
          lcCONTSFContracts = fCParamC("CONTSF_CONTRACTS").

   IF LOOKUP(pcOldBundle,lcCONTSFContracts) > 0 AND
      LOOKUP(pcNewBundle,lcCONTSFContracts) > 0 THEN DO:
      ocError = "BTC is not allowed for fusion tariff".
      RETURN FALSE.
   END. /* IF LOOKUP(pcOldBundle,lcCONTSFContracts) > 0 AND */

   IF NOT ((LOOKUP(pcOldBundle,lcFLATContracts) > 0 AND
       LOOKUP(pcNewBundle,lcFLATContracts) > 0) OR
      (LOOKUP(pcOldBundle,lcIPLContracts) > 0 AND
       LOOKUP(pcNewBundle,lcIPLContracts) > 0) OR
      (LOOKUP(pcOldBundle,lcCONTSContracts) > 0 AND
       LOOKUP(pcNewBundle,lcCONTSContracts) > 0) OR
      (LOOKUP(pcOldBundle,lcBONOContracts) > 0 AND
       LOOKUP(pcNewBundle,lcBONOContracts) > 0)) THEN DO:
      ocError = "Old and new bundle type are not compatible for BTC".
      RETURN FALSE.
   END.

   IF DAY(pdaActDate) <> 1 AND
      LOOKUP(pcNewBundle,lcBONOContracts) = 0 AND
      NOT fIsiSTCAllowed(INPUT Mobsub.MsSeq) THEN DO:
      ocError = "Multiple immediate BTC is not allowed in same month due to business rules!".
      RETURN FALSE.
   END.

   RETURN TRUE.

END FUNCTION.

FUNCTION fOngoingBTC RETURNS LOGICAL
   (iiMsSeq           AS INT,
    icCurrentContract AS CHAR,
    ilCheckUpgrade    AS LOG):
   
   DEF VAR llExist AS LOG NO-UNDO INIT FALSE.
   
   FIND FIRST MsRequest NO-LOCK WHERE
               MsRequest.MsSeq   = iiMsSeq   AND
               MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
               MsRequest.ReqCparam1 = icCurrentContract AND
               LOOKUP(STRING(MsRequest.ReqStatus),
                      {&REQ_INACTIVE_STATUSES}) = 0 NO-ERROR.
   IF AVAILABLE MsRequest THEN DO:
      IF ilCheckUpgrade THEN DO:
         IF MsRequest.ReqCparam5 > "" THEN llExist = TRUE.
         ELSE llExist = FALSE.
      END. /* IF ilCheckUpgrade THEN DO: */
      ELSE llExist = TRUE.
   END. /* IF AVAILABLE MsRequest THEN DO: */

   RETURN llExist.
   
END FUNCTION.

FUNCTION fMainAdditionalLine RETURNS LOGICAL
   (iiMsSeq        AS INT,
    iiCustNum      AS INT,
    pcNewBundle    AS CHAR,
    OUTPUT ocError AS CHAR):

   DEF BUFFER lbMobSub      FOR MobSub.
   DEF BUFFER CLIType       FOR CLIType.

   FIND FIRST CLIType NO-LOCK WHERE
              CLIType.Brand = Syst.Var:gcBrand AND
              CLIType.CLIType = pcNewBundle AND
              CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL} NO-ERROR.
   IF NOT AVAIL CLIType THEN RETURN TRUE.
   
   MOBSUB_LOOP:
   FOR EACH lbMobSub NO-LOCK WHERE
            lbMobSub.Brand   = Syst.Var:gcBrand AND
            lbMobSub.InvCust = iiCustNum AND
            lbMobSub.MsSeq NE  iiMsSeq AND
            lbMobSub.PayType = FALSE,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = Syst.Var:gcBrand AND
            CLIType.CLIType = lbMobSub.TariffBundle AND
            CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}:

      IF fHasPendingRequests
         (lbMobSub.MsSeq,
          lbMobSub.CLI,
          CLIType.LineType) THEN NEXT.

      RETURN TRUE.
   END.

   ocError = "BTC is not allowed to additional line because main line is not active".
   RETURN FALSE.
   
END FUNCTION.

FUNCTION fValidateBTC RETURNS LOGICAL
   (piMsSeq AS INT,
    pcOldBundle AS CHAR,
    pcNewBundle AS CHAR,
    pdaActDate AS DATE,
    pcCliType AS CHAR,
    ilExtendContract AS LOG,
    OUTPUT ocError AS CHAR):

   DEF BUFFER MobSub FOR MobSub.
   DEF VAR lcMNP AS CHAR NO-UNDO. 
   DEF VAR ldaDueDate AS DATE NO-UNDO. 
   DEF VAR lcBONOContracts  AS CHAR NO-UNDO.

   lcBONOContracts = fCParamC("BONO_CONTRACTS").

   FIND MobSub NO-LOCK WHERE
        MobSub.MsSeq = piMsSeq NO-ERROR.
   IF NOT AVAIL MobSub THEN DO:
      ocError = "Subscription not found".
      RETURN False.
   END.

   /* MNP OUT validation for Tariff BTC, reason YTS-6053 */
   IF LOOKUP(pcnewBundle,lcBONOContracts) = 0 THEN DO:
      lcMNP = Mnp.MNPOutGoing:mGetMNPOutOngoing(MobSub.CLI).
      IF lcMNP NE "" THEN DO: 
         
         FIND MNPProcess WHERE
              MNPProcess.PortRequest = lcMNP NO-LOCK NO-ERROR.

         IF AVAIL MNPProcess THEN DO:

            IF 1 > fMNPPeriods(
               input Func.Common:mMakeTS(),
               input MNPProcess.PortingTime,
               INPUT 0,
               OUTPUT ldaDueDate) THEN DO: 
               ocError = "Ongoing MNP OUT request. Too late for BTC".
               RETURN FALSE.
            END.
         END.
      END.
   END.

   IF pdaActDate <= TODAY THEN DO:
      ocError = "Invalid change date".
      RETURN False.
   END.

   IF DAY(pdaActDate) NE 1 AND
      (pcNewBundle BEGINS "MDUB" OR pcNewBundle BEGINS "DATA") THEN DO:
      ocError = "Invalid change date".
      RETURN False.
   END.

   IF fOngoingBTC(piMsSeq,pcOldBundle,FALSE) THEN DO:
      ocError = "Ongoing BTC request".
      RETURN False.
   END.

   IF NOT fIsBTCBundleAllowed(piMsSeq,
                              pcCLIType,
                              pcOldBundle,
                              pcNewBundle,
                              pdaActDate,
                              OUTPUT ocError) THEN RETURN False.

   IF NOT fMainAdditionalLine(MobSub.MsSeq,
                              MobSub.CustNum,  
                              pcNewBundle,
                              OUTPUT ocError) THEN RETURN False.

   IF NOT fIsBTCAllowed(piMsSeq,
                        pcOldBundle,
                        OUTPUT ocError) THEN RETURN False.

   IF ilExtendContract AND NOT
      fCanExtendTerminalContract(
          BUFFER MobSub,
          pdaActDate,
          pcCLIType,
          OUTPUT ocError) THEN RETURN FALSE.

   RETURN True.
END.


&ENDIF
