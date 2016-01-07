/* requestaction_exec.i    10.09.08/aam 
*/
&IF "{&frequestaction}" NE "YES"
&THEN
&GLOBAL-DEFINE frequestaction YES

{commali.i}
{timestamp.i}
{tmsconst.i}
{cparam2.i}
{fbundle.i}

DEF TEMP-TABLE ttAction NO-UNDO
   FIELD ActionType AS CHAR
   FIELD ActionKey  AS CHAR
   FIELD ActionID   AS INT
   FIELD Action     AS INT
   INDEX ActionType ActionType ActionKey Action.

FUNCTION fGetPerContractActivation RETURNS DEC
   (iiMsSeq      AS INT,
    icDCEvent    AS CHAR,
    idaEventDate AS DATE):

   DEF VAR ldActivated AS DEC  NO-UNDO.
   DEF VAR lcReqParam2 AS CHAR NO-UNDO. 

   DEF BUFFER bActRequest FOR MsRequest.
   DEF BUFFER bDCCLI FOR DCCLI.
   DEF BUFFER MobSub FOR MobSub.

   FIND FIRST bDCCLI WHERE
              bDCCLI.Brand   = gcBrand   AND
              bDCCLI.DCEvent = icDCEvent AND
              bDCCLI.MsSeq   = iiMsSeq   AND
              bDCCLI.ValidTo   >= idaEventDate AND
              bDCCLI.ValidFrom <= idaEventDate NO-LOCK NO-ERROR. 
   IF NOT AVAIL bDCCLI THEN RETURN 0.0.

   /* YDR-2046 */
   /* If we have Renewaldate (Extended permanency) THEN renewal dated
      clitype is considered as reference tariff */
   IF bDCCLI.RenewalDate <> ? THEN 
      ASSIGN 
         ldActivated = fMake2Dt(bDCCLI.RenewalDate,0)
         lcReqParam2 = "update".
   ELSE     
      ASSIGN 
         ldActivated = fMake2Dt(bDCCLI.ValidFrom,0)
         lcReqParam2 = "act,recreate".

   /* must use request handling time instead of contract begin time, YBU-991 */
   FIND FIRST bActRequest WHERE
      bActRequest.MsSeq   = iiMsseq AND
      bActRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} AND
      bActRequest.ReqStatus  = {&REQUEST_STATUS_DONE} AND
      bActRequest.DoneStamp >= ldActivated AND
      bActRequest.ReqCParam3 = bDCCLI.DCEvent AND
      LOOKUP(bActRequest.ReqCparam2,lcReqParam2) > 0
   NO-LOCK USE-INDEX MsSeq NO-ERROR.

   IF AVAIL bActRequest THEN DO: 
      ldActivated = bActRequest.DoneStamp.
      
      /* YDR-2046 */
      /* If we have any renewal order THEN before STC clitye is 
         considered as reference tariff */
        
      IF bActRequest.ReqSource = {&REQUEST_SOURCE_RENEWAL} THEN 
         ldActivated = fMake2Dt(idaEventDate - 1,0).

   END. 

   RETURN ldActivated. 

END FUNCTION.

FUNCTION fGetOriginalCLIType RETURNS CHAR
  (iiMsSeq      AS INT,
   icDCEvent    AS CHAR,
   idaValidDate AS DATE,
   OUTPUT ldaActivated AS DATE):
   
   DEF VAR ldActivated   AS DEC  NO-UNDO.
   DEF VAR lcOrigCLIType AS CHAR NO-UNDO. 
   DEF VAR liTime        AS INT  NO-UNDO.
   
   DEF BUFFER bOrigOwner FOR MsOwner.
   
   /* when was contract for penalty fee activated */
   ldActivated = fGetPerContractActivation(iiMsSeq,
                                           icDCEvent,
                                           idaValidDate).
   IF ldActivated = 0 THEN RETURN "".

   FIND FIRST bOrigOwner WHERE
              bOrigOwner.MsSeq = iiMsSeq AND 
              bOrigOwner.TSBegin <= ldActivated NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE bOrigOwner THEN 
      FIND LAST bOrigOwner WHERE
                bOrigOwner.MsSeq = iiMsSeq AND 
                bOrigOwner.TSBegin > ldActivated NO-LOCK NO-ERROR. 

   IF AVAIL bOrigOwner THEN DO:
      lcOrigCLIType = bOrigOwner.CLIType.
      fSplitTS(bOrigOwner.TSBeg,
               OUTPUT ldaActivated,
               OUTPUT liTime).
   END.
               
   RETURN lcOrigCLIType.
   
END FUNCTION.

FUNCTION fGetOriginalBundle RETURNS CHAR
   (iiMsSeq      AS INT,
    icDCEvent    AS CHAR,
    idaValidDate AS DATE,
    OUTPUT odaActivated AS DATE):

   DEF VAR ldaActivated     AS DATE NO-UNDO.  
   DEF VAR ldActivated      AS DEC  NO-UNDO.
   DEF VAR ldActEnd         AS DEC  NO-UNDO.
   DEF VAR liTime           AS INT  NO-UNDO.
   DEF VAR liOffSet         AS INT  NO-UNDO.
   DEF VAR lcOrigCLIType    AS CHAR NO-UNDO.
   DEF VAR lcIPLContracts   AS CHAR NO-UNDO.
   DEF VAR lcCONTDContracts AS CHAR NO-UNDO.
   DEF VAR lcFLATContracts  AS CHAR NO-UNDO.
   DEF VAR lcCONTSContracts AS CHAR NO-UNDO.
   DEF VAR lcCONTSFContracts AS CHAR NO-UNDO.
   DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.

   ASSIGN lcIPLContracts   = fCParamC("IPL_CONTRACTS")
          lcCONTDContracts = fCParamC("CONTD_CONTRACTS")
          lcFLATContracts  = fCParamC("FLAT_CONTRACTS")
          lcCONTSContracts = fCParamC("CONTS_CONTRACTS")
          lcCONTSFContracts = fCParamC("CONTSF_CONTRACTS")
          lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").
   
   DEF BUFFER bBundleRequest FOR MsRequest.
                                                          
   lcOrigCLIType = fGetOriginalCLIType(iiMsSeq, 
                                       icDCEvent,
                                       idaValidDate,
                                       OUTPUT ldaActivated). 
                                                             
   IF LOOKUP(lcOrigCLIType,lcBundleCLITypes) = 0 THEN RETURN "".

   /* when was contract for penalty fee activated */
   ldActivated = fGetPerContractActivation(iiMsSeq,
                                           icDCEvent,
                                           idaValidDate).
   
   IF ldActivated = 0 THEN RETURN "". 

   IF ldActivated = TRUNCATE(ldActivated,0) THEN liOffSet = 24.
   ELSE liOffSet = 3.

   /* activated during the first hours after subscription activation */
   ldActEnd    = fOffSet(ldActivated,liOffSet).
      
   /* Note: It should return only IPL or FLAT Tariff Basic Bundle */
   FOR EACH bBundleRequest NO-LOCK WHERE
        bBundleRequest.MsSeq   = iiMsSeq AND
        bBundleRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} AND
        bBundleRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
        bBundleRequest.ActStamp <= ldActEnd AND
        LOOKUP(bBundleRequest.ReqCParam3,
               lcIPLContracts + "," + 
               lcCONTDContracts + "," + 
               lcFlatContracts + "," +
               lcCONTSContracts + "," +
               lcCONTSFContracts) > 0
      BY bBundleRequest.DoneStamp DESC:
      fSplitTS(bBundleRequest.DoneStamp,
               OUTPUT odaActivated,
               OUTPUT liTime).
      RETURN bBundleRequest.ReqCParam3.
   END.
 
   RETURN "".
      
END FUNCTION.

PROCEDURE pCollectRequestActions:

   DEF INPUT PARAMETER iiMsSeq      AS INT  NO-UNDO.
   DEF INPUT PARAMETER ihRequest    AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER icCLIType    AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiPayType    AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiReqType    AS INT  NO-UNDO.
   DEF INPUT PARAMETER idaReqDate   AS DATE NO-UNDO.
   DEF INPUT PARAMETER icActionList AS CHAR NO-UNDO.
   
   DEF VAR llAllowed AS LOG  NO-UNDO.
   
   DEF BUFFER bAction FOR ttAction.
   DEF BUFFER bContract FOR DayCampaign.
   
   EMPTY TEMP-TABLE ttAction.
   
   /* clitype level overrides paytype level, so check clitypes first */
   FOR EACH RequestAction NO-LOCK USE-INDEX CLIType WHERE
            RequestAction.Brand      = gcBrand     AND
            RequestAction.CLIType    = icCLIType   AND
            RequestAction.ReqType    = iiReqType   AND
            RequestAction.ValidTo   >= idaReqDate  AND
            RequestAction.ValidFrom <= idaReqDate  AND
            LOOKUP(STRING(RequestAction.Action),icActionList) > 0:

      /* additional rules defined */
      RUN pDoRulesAllow(iiMsSeq,
                        icCLIType,
                        ihRequest,
                        idaReqDate,
                        RequestAction.RequestActionID,
                        RequestAction.ActionType,
                        RequestAction.ActionKey,
                        OUTPUT llAllowed).
      IF NOT llAllowed THEN NEXT.
 
      FIND FIRST ttAction WHERE 
                 ttAction.ActionType = RequestAction.ActionType AND
                 ttAction.ActionKey  = RequestAction.ActionKey AND
                 ttAction.Action     = RequestAction.Action NO-ERROR.
      IF NOT AVAILABLE ttAction THEN DO:
         CREATE ttAction.
         ASSIGN 
            ttAction.ActionType = RequestAction.ActionType
            ttAction.ActionKey  = RequestAction.ActionKey 
            ttAction.ActionID   = RequestAction.RequestActionID
            ttAction.Action     = RequestAction.Action.
      END.      
   END.

   /* paytype level */
   FOR EACH RequestAction NO-LOCK USE-INDEX PayType WHERE
            RequestAction.Brand      = gcBrand     AND
            RequestAction.PayType    = iiPayType   AND
            RequestAction.ReqType    = iiReqType   AND
            RequestAction.ValidTo   >= idaReqDate  AND
            RequestAction.ValidFrom <= idaReqDate  AND
            RequestAction.CLIType    = ""          AND
            LOOKUP(STRING(RequestAction.Action),icActionList) > 0:

      /* additional rules defined */
      RUN pDoRulesAllow(iiMsSeq,
                        icCLIType,
                        ihRequest,
                        idaReqDate,
                        RequestAction.RequestActionID,
                        RequestAction.ActionType,
                        RequestAction.ActionKey,
                        OUTPUT llAllowed).
      IF NOT llAllowed THEN NEXT.
 
      FIND FIRST ttAction WHERE 
                 ttAction.ActionType = RequestAction.ActionType AND
                 ttAction.ActionKey  = RequestAction.ActionKey AND
                 ttAction.Action     = RequestAction.Action NO-ERROR.
      IF NOT AVAILABLE ttAction THEN DO:
         CREATE ttAction.
         ASSIGN 
            ttAction.ActionType = RequestAction.ActionType
            ttAction.ActionKey  = RequestAction.ActionKey
            ttAction.ActionID   = RequestAction.RequestActionID
            ttAction.Action     = RequestAction.Action.
      END.      
   END.

   /* all */
   FOR EACH RequestAction NO-LOCK USE-INDEX CLIType WHERE
            RequestAction.Brand      = gcBrand     AND
            RequestAction.CLIType    = "*"         AND
            RequestAction.ReqType    = iiReqType   AND
            RequestAction.ValidTo   >= idaReqDate  AND
            RequestAction.ValidFrom <= idaReqDate  AND
            LOOKUP(STRING(RequestAction.Action),icActionList) > 0:

      /* additional rules defined */
      RUN pDoRulesAllow(iiMsSeq,
                        icCLIType,
                        ihRequest,
                        idaReqDate,
                        RequestAction.RequestActionID,
                        RequestAction.ActionType,
                        RequestAction.ActionKey,
                        OUTPUT llAllowed).
      IF NOT llAllowed THEN NEXT.
 
      FIND FIRST ttAction WHERE 
                 ttAction.ActionType = RequestAction.ActionType AND
                 ttAction.ActionKey  = RequestAction.ActionKey AND 
                 ttAction.Action     = RequestAction.Action NO-ERROR.
      IF NOT AVAILABLE ttAction THEN DO:
         CREATE ttAction.
         ASSIGN 
            ttAction.ActionType = RequestAction.ActionType
            ttAction.ActionKey  = RequestAction.ActionKey 
            ttAction.ActionID   = RequestAction.RequestActionID
            ttAction.Action     = RequestAction.Action.
      END.      
   END.

   /* convert periodical contract types to actual contract ids */
   FOR EACH ttAction WHERE
            ttAction.ActionType = "DCType":

      FOR EACH bContract NO-LOCK WHERE
               bContract.Brand  = gcBrand AND
               bContract.DCType = ttAction.ActionKey:
               /* validfrom/to not checked, subscription may have old
                  contracts active */
   
         /* contract level rules override type level ones */
         IF CAN-FIND(FIRST bAction WHERE
                           bAction.ActionType = "DayCampaign" AND
                           bAction.ActionKey  = bContract.DCEvent AND
                           bAction.Action     = ttAction.Action)
         THEN NEXT.
         
         CREATE bAction.
         ASSIGN 
            bAction.ActionType = "DayCampaign"
            bAction.ActionKey  = bContract.DCEvent
            bAction.ActionID   = ttAction.ActionID
            bAction.Action     = ttAction.Action.
      END.
      
      DELETE ttAction.
   END.
            
END PROCEDURE.

PROCEDURE pDoRulesAllow:

   DEF INPUT  PARAMETER iiMsSeq           AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiCLIType         AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER ihRequest         AS HANDLE NO-UNDO.
   DEF INPUT  PARAMETER idaReqDate        AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiRequestActionID AS INT  NO-UNDO.
   DEF INPUT  PARAMETER icActionType      AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icActionKey       AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER olMatch           AS LOG  NO-UNDO.
   
   DEF VAR liSTC         AS INT  NO-UNDO.
   DEF VAR llChain       AS LOG  NO-UNDO.
   DEF VAR llInstallment AS LOG  NO-UNDO.
   DEF VAR liCount       AS INT  NO-UNDO.
   DEF VAR lhParam       AS HANDLE NO-UNDO.
   DEF VAR lcReturnValue AS CHAR NO-UNDO.
   DEF VAR lcParamValue AS CHAR NO-UNDO. 
   DEF VAR llCheckOtherRule AS LOG NO-UNDO. 

   DEF BUFFER bSTCRequest FOR MsRequest.   

   /* if no rules defined then all accepted */
   olMatch = TRUE.
   
   IF NOT VALID-HANDLE(ihRequest) THEN RETURN.
   
   CheckActionRules:
   FOR EACH RequestActionRule NO-LOCK WHERE
         RequestActionRule.RequestActionID = iiRequestActionID AND
         RequestActionRule.ToDate   >= idaReqDate AND
         RequestActionRule.FromDate <= idaReqDate:
      
      /* if included rules have been defined then atleast one of them 
         must match */
      IF RequestActionRule.ParamValue > "" THEN olMatch = FALSE.
      
      IF RequestActionRule.ExclParamValue > "" THEN DO:
         RUN pCheckInstallmentRule(RequestActionRule.ExclParamValue, 
                                   iiMsSeq, 
                                   OUTPUT llInstallment).
         IF llInstallment THEN NEXT CheckActionRules.
      END.
      
      IF RequestActionRule.ParamField BEGINS "#" THEN DO:

         CASE RequestActionRule.ParamField:
         WHEN "#FEECOMPARE" THEN DO:
            /* not yet converted to per.contract level */
            IF icActionType = "DcType" THEN DO:
               olMatch = TRUE.
               RETURN.
            END.

            IF RequestActionRule.ParamValue BEGINS "+," THEN
               lcParamValue = SUBSTRING(RequestActionRule.ParamValue,3).
            ELSE lcParamValue = RequestActionRule.ParamValue.

            RUN pFeeComparison(iiMsSeq,
                               idaReqDate,
                               icActionKey,
                               ihRequest,
                               lcParamValue,
                               RequestActionRule.ExclParamValue,
                               OUTPUT olMatch).
         END. /* WHEN "#FEECOMPARE" THEN DO: */
         END CASE.
         
         IF olMatch = FALSE AND
            LOOKUP("+", RequestActionRule.ParamValue) > 0 THEN RETURN.

         IF olMatch = TRUE AND
            NOT RequestActionRule.ParamValue BEGINS "+," THEN RETURN.
         NEXT CheckActionRules.
      END.

      lhParam = ihRequest:BUFFER-FIELD(RequestActionRule.ParamField) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT.

      IF RequestActionRule.ParamValue > "" THEN DO:
         
         /* chain of stcs should be found */
         IF ihRequest::ReqType = 0 AND 
            INDEX(RequestActionRule.ParamValue,"<") > 0 AND
            INDEX(RequestActionRule.ParamValue,",") = 0 AND 
            lhParam:BUFFER-VALUE = ENTRY(1,RequestActionRule.ParamValue,"<") 
         THEN DO:
            
            ASSIGN
               llChain = FALSE
               liSTC   = 1. 
            
            FOR EACH bSTCRequest NO-LOCK WHERE
                     bSTCRequest.MsSeq   = iiMsSeq AND
                     bSTCRequest.ReqType = 0 AND
                     bSTCRequest.ReqStat = 2 AND
                     bSTCRequest.ActStamp < ihRequest::ActStamp
            BY bSTCRequest.ActStamp DESC:
            
               liSTC = liSTC + 1.
               IF liSTC > NUM-ENTRIES(RequestActionRule.ParamValue,"<") THEN
                  LEAVE.

               IF liSTC = 2 THEN llChain = TRUE.   
               IF ENTRY(liSTC,RequestActionRule.ParamValue,"<") NE
                  bSTCRequest.ReqCParam1 THEN llChain = FALSE.
            END.
            
            IF llChain THEN DO:
               olMatch = TRUE.
               RETURN.
            END.
            ELSE NEXT CheckActionRules. 
         END.
         
         /* old CLIType or bundle affects terminal discount */
         ELSE IF (ihRequest::ReqType = 0 OR ihRequest::ReqType = 81) AND
                 INDEX(RequestActionRule.ParamValue,">") > 0 AND
                 INDEX(RequestActionRule.ParamValue,",") = 0 AND 
                 lhParam:BUFFER-VALUE = 
                     ENTRY(2,RequestActionRule.ParamValue,">") 
         THEN DO:
            /* not yet converted to per.contract level */
            IF icActionType = "DcType" THEN llChain = TRUE.
            
            ELSE IF ENTRY(1,RequestActionRule.ParamValue,">") BEGINS "BUNDLE:" 
            THEN RUN pCheckOriginalBundle(
                        iiMsSeq,
                        idaReqDate,
                        icActionKey,
                        ENTRY(2,ENTRY(1,RequestActionRule.ParamValue,">"),":"),
                        OUTPUT llChain).

            ELSE RUN pCheckOriginalCLIType(
                        iiMsSeq,
                        idaReqDate,
                        icActionKey,
                        ENTRY(1,RequestActionRule.ParamValue,">"),
                        OUTPUT llChain).

            IF llChain THEN DO:
               IF ihRequest::ReqType = 0 AND
                  RequestActionRule.ParamField = "ReqCParam5" AND
                  lhParam:BUFFER-VALUE <> ENTRY(2,RequestActionRule.ParamValue,">")
               THEN llChain = FALSE.

               IF ihRequest::ReqType = 81 AND
                  RequestActionRule.ParamField = "ReqCParam2" AND
                  lhParam:BUFFER-VALUE <> ENTRY(2,RequestActionRule.ParamValue,">")
               THEN llChain = FALSE.
            END. /* IF llChain THEN DO: */

            IF llChain THEN DO:
               olMatch = TRUE.
               RETURN.
            END.
            ELSE NEXT CheckActionRules.
         END.
         
         ELSE IF LOOKUP(lhParam:BUFFER-VALUE,RequestActionRule.ParamValue) > 0 
         THEN DO:
            olMatch = TRUE.
            /* Special handling to differentiate with other rules with     */
            /* plus sign (eg '+') it means all the rules must be satisfied */
            IF RequestActionRule.ParamValue BEGINS "+," THEN DO:
               /* Check exclude params to verify correct bundle/clitype */
               IF RequestActionRule.ExclParamValue > "" THEN DO:
                  IF LOOKUP("BUNDLE",RequestActionRule.ExclParamValue) > 0 THEN
                     lcReturnValue = fGetCurrentSpecificBundle(iiMsSeq,iiCLIType).

                  IF LOOKUP(lcReturnValue,RequestActionRule.ExclParamValue) > 0
                  THEN DO:
                     olMatch = FALSE.
                     RETURN.
                  END. /* IF LOOKUP(lcReturnValue,RequestActionRule */
                  ELSE NEXT CheckActionRules.
               END. /* IF RequestActionRule.ExclParamValue > "" THEN DO: */
               ELSE NEXT CheckActionRules.
            END. /* IF LOOKUP("+", RequestActionRule.ParamValue) > 0 THEN */
            RETURN.
         END.   

         ELSE DO liCount = 1 TO NUM-ENTRIES(RequestActionRule.ParamValue):
            /* make sure that only intended range matches are checked, i.e. 
               entries with wildcards */
            IF INDEX(ENTRY(liCount,RequestActionRule.ParamValue),"*") > 0 AND
               lhParam:BUFFER-VALUE MATCHES 
                  ENTRY(liCount,RequestActionRule.ParamValue) 
            THEN DO:
               olMatch = TRUE.
               IF RequestActionRule.ParamValue BEGINS "+," THEN 
                  NEXT CheckActionRules.
               RETURN.
            END.   
         END.
      END.
          
      IF RequestActionRule.ExclParamValue > "" THEN DO: 
         IF LOOKUP(lhParam:BUFFER-VALUE,RequestActionRule.ExclParamValue) > 0 
         THEN DO:
            olMatch = FALSE.
            RETURN.
         END.   
         ELSE DO liCount = 1 TO NUM-ENTRIES(RequestActionRule.ExclParamValue):
            IF INDEX(ENTRY(liCount,RequestActionRule.ExclParamValue),"*") > 0 
                  AND
               lhParam:BUFFER-VALUE MATCHES 
                  ENTRY(liCount,RequestActionRule.ExclParamValue) 
            THEN DO:
               olMatch = FALSE.
               RETURN.
            END.   
         END.
      END.
      /* Special handling to differentiate with other rules with     */
      /* plus sign (eg '+') it means all the rules must be satisfied */
      IF olMatch = FALSE AND
         LOOKUP("+", RequestActionRule.ParamValue) > 0 THEN RETURN.
   END.

END PROCEDURE.

PROCEDURE pCheckOriginalCLIType:
  
   DEF INPUT  PARAMETER iiMsSeq      AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idaReqDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER icDCEvent    AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icCLIType    AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER olMatch      AS LOG  NO-UNDO.

   DEF VAR ldTS          AS DEC  NO-UNDO.
   DEF VAR lcOrigCLIType AS CHAR NO-UNDO. 
   DEF VAR ldaActivated  AS DATE NO-UNDO.

   lcOrigCLIType = fGetOriginalCLIType(iiMsSeq,
                                       icDCEvent,
                                       idaReqDate,
                                       OUTPUT ldaActivated).
                                       
   olMatch = (lcOrigCLIType = icCLIType).
 
END PROCEDURE.

PROCEDURE pCheckOriginalBundle:

   DEF INPUT  PARAMETER iiMsSeq      AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idaReqDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER icDCEvent    AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icBundle     AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER olMatch      AS LOG  NO-UNDO.

   DEF VAR lcOriginalBundle AS CHAR  NO-UNDO.
   DEF VAR ldaActivated     AS DATE NO-UNDO.
   
   lcOriginalBundle = fGetOriginalBundle(iiMsSeq,
                                         icDCEvent,
                                         idaReqDate,
                                         OUTPUT ldaActivated).

   olMatch = (lcOriginalBundle = icBundle).
      
END PROCEDURE.
         
PROCEDURE pCheckInstallmentRule:
   
   DEF INPUT PARAMETER  icRule  AS CHAR NO-UNDO.
   DEF INPUT PARAMETER  iiMsSeq AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER olInstallment AS LOG  NO-UNDO.

   DEF BUFFER bDCCLI FOR DCCLI.
   DEF BUFFER bContract FOR DayCampaign.
         
   olInstallment = FALSE.

   /* YTS-2259 */
   IF icRule EQ "INSTALLMENT" THEN DO:
      
      FOR EACH bContract NO-LOCK WHERE
               bContract.Brand = gcBrand AND
               bContract.DCType = "5",
         FIRST bDCCLI NO-LOCK WHERE
               bDCCLI.MsSeq = iiMsSeq AND
               bDCCLI.DCEvent = bContract.DCEvent AND
               bDCCLI.ValidTo >= TODAY:
         olInstallment = TRUE.
         RETURN.
      END.
   END.
   
END PROCEDURE.

PROCEDURE pFeeComparison:

   DEF INPUT  PARAMETER iiMsSeq      AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idaReqDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER icDCEvent    AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER ihRequest    AS HANDLE NO-UNDO.
   DEF INPUT  PARAMETER icRule       AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icExclRule   AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER olMatch      AS LOG  NO-UNDO.

   DEF VAR ldOriginalFee             AS DEC  NO-UNDO.
   DEF VAR ldCurrFee                 AS DEC  NO-UNDO.
   DEF VAR ldNewFee                  AS DEC  NO-UNDO.
   DEF VAR lcOrigCLIType             AS CHAR NO-UNDO.
   DEF VAR ldaActivated              AS DATE NO-UNDO.
   DEF VAR ldCompareFee              AS DEC  NO-UNDO EXTENT 2.
   DEF VAR liComp                    AS INT  NO-UNDO.
   DEF VAR lcBONOContracts           AS CHAR NO-UNDO.
   DEF VAR lcNewCLIType              AS CHAR NO-UNDO. 

   DEF BUFFER bCLIType FOR CLIType.
   DEF BUFFER bMobSub  FOR MobSub.
    
   ASSIGN olMatch = FALSE
          lcBONOContracts = fCParamC("BONO_CONTRACTS").
   
   /* compare original fee (when penalty fee was activated), current fee and/or
      new fee (after STC/BTC) with each other. Fee can be minimum consumption
      or monthly fee */
   IF LOOKUP(STRING(ihRequest::ReqType),"0,81") = 0 OR
      (ihRequest::ReqType = 81 AND LOOKUP(ihRequest::ReqCParam1,lcBONOContracts) > 0)
   THEN RETURN.

   /* if penalty fee doesn't exist then no need to check further */
   IF NOT CAN-FIND(FIRST DCCLI WHERE 
                         DCCLI.MsSeq = iiMsSeq AND
                         DCCLI.DCEvent = icDCEvent) THEN RETURN.
    
   IF INDEX(icRule + icExclRule,"ORIGINAL") > 0 THEN DO:
      lcOrigCLIType = fGetOriginalCLIType(iiMsSeq,
                                          icDCEvent,
                                          idaReqDate,
                                          OUTPUT ldaActivated).
      IF lcOrigCLIType = "" THEN RETURN. 
                    
      FIND FIRST bCLIType WHERE
                 bCLIType.Brand = gcBrand AND
                 bCLIType.CLIType = lcOrigCLIType NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bCLIType THEN RETURN.

      IF bCLIType.CompareFee > 0 THEN ldOriginalFee = bCLIType.CompareFee.
      ELSE DO:
         lcOrigCLIType = fGetOriginalBundle(iiMsSeq,
                                            icDCEvent,
                                            idaReqDate,
                                            OUTPUT ldaActivated).
         FIND FIRST bCLIType WHERE
                    bCLIType.Brand = gcBrand AND
                    bCLIType.CLIType = lcOrigCLIType NO-LOCK NO-ERROR.
         IF AVAIL bCLIType THEN ldOriginalFee = bCLIType.CompareFee.
      END. /* ELSE DO: */

      IF ldOriginalFee = 0 THEN RETURN.

   END. /* IF INDEX(icRule + icExclRule,"ORIGINAL") > 0 THEN DO: */

   IF INDEX(icRule + icExclRule,"CURRENT") > 0 THEN DO:
      FIND FIRST bCLIType WHERE 
                 bCLIType.Brand   = gcBrand AND
                 bCLIType.CLIType = ihRequest::ReqCParam1 NO-LOCK NO-ERROR.
      IF AVAIL bCLIType THEN ldNewFee = bCLIType.CompareFee.
   END.
        
   IF INDEX(icRule + icExclRule,"NEW") > 0 THEN DO:
      FIND FIRST bCLIType WHERE 
                 bCLIType.Brand   = gcBrand AND
                 bCLIType.CLIType = ihRequest::ReqCParam2 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bCLIType OR bCLIType.CompareFee = 0 THEN
         FIND FIRST bCLIType WHERE 
                    bCLIType.Brand   = gcBrand AND
                    bCLIType.CLIType = ihRequest::ReqCParam5 NO-LOCK NO-ERROR.

      IF AVAIL bCLIType THEN ASSIGN
         ldNewFee = bCLIType.CompareFee
         lcNewCLIType = bCLIType.CLIType.
   END. /* IF INDEX(icRule + icExclRule,"NEW") > 0 THEN DO: */
          
   IF icRule > "" THEN DO:
      IF INDEX(icRule,">") > 0 THEN DO:
         ldCompareFee = 0.   
         DO liComp = 1 TO 2:
            CASE ENTRY(liComp,icRule,">"):
            WHEN "ORIGINAL" THEN ldCompareFee[liComp] = ldOriginalFee.
            WHEN "CURRENT"  THEN ldCompareFee[liComp] = ldCurrFee.
            WHEN "NEW"      THEN ldCompareFee[liComp] = ldNewFee.
            END CASE.
         END.
         
         olMatch = (ldCompareFee[1] > ldCompareFee[2]).
         
         /* YDR-1105 */
         IF lcNewCLIType EQ "CONT8" AND
            LOOKUP(lcOrigCLIType,"CONT7,CONTD9") > 0 THEN olMatch = TRUE.
         
         /* Temp code, do not apply penalty for STC/BTC requests created
            before the deployment */
         
         IF ihRequest::CreStamp < 20130905.18000 AND
            LOOKUP(icDCEvent,"TERM18,TERM18-50,TERM24,TERM24-50") > 0
         THEN olMatch = FALSE.
         
         /* YDR-1137 - Exclude termination/extension if request is originating 
            from Fusion order (STC) fallback */
         IF ihRequest::ReqSource EQ {&REQUEST_SOURCE_FUSION_ORDER_FALLBACK} 
            THEN olMatch = FALSE.
      END.
   END.

   IF icExclRule > "" THEN DO:
      IF INDEX(icExclRule,">") > 0 THEN DO:
         ldCompareFee = 0.   
         DO liComp = 1 TO 2:
            CASE ENTRY(liComp,icExclRule,">"):
            WHEN "ORIGINAL" THEN ldCompareFee[liComp] = ldOriginalFee.
            WHEN "CURRENT"  THEN ldCompareFee[liComp] = ldCurrFee.
            WHEN "NEW"      THEN ldCompareFee[liComp] = ldNewFee.
            END CASE.
         END.
         
         olMatch = NOT (ldCompareFee[1] > ldCompareFee[2]).
      END.
   END.

END PROCEDURE.

&ENDIF
