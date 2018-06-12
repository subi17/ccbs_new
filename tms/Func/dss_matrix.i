&IF "{&dss_matrix_i}" NE "YES" 
&THEN

&GLOBAL-DEFINE dss_matrix_i YES

{Func/fdss.i}
{Func/extralinefunc.i}
{Func/matrix.i}
{Syst/tmsconst.i}
{Syst/eventval.i}
{Func/create_eventlog.i}

FUNCTION fIsDSSAllowedForCustomer RETURNS LOG
   (INPUT  iiCustnum            AS INT,
    INPUT  ideActStamp          AS DEC,
    INPUT  icBundleId           AS CHAR,
    INPUT  icReturnParamType    AS CHAR,
    INPUT  ilNewSubscription    AS LOG,
    OUTPUT odeCurrMonthLimit    AS DEC,
    OUTPUT odeConsumedData      AS DEC,
    OUTPUT odeOtherMonthLimit   AS DEC,
    OUTPUT ocResult             AS CHAR):

   DEF VAR liPeriod               AS INT   NO-UNDO.
   DEF VAR liMobSubCount          AS INT   NO-UNDO.
   DEF VAR liTotalDSSLimit        AS INT64 NO-UNDO.
   DEF VAR lcHSDPASubsList        AS CHAR  NO-UNDO.
   DEF VAR lcAllSubsList          AS CHAR  NO-UNDO.
   DEF VAR ldeServiceLCounterAmt  AS DEC   NO-UNDO.
   DEF VAR ldeBundleLimit         AS DEC   NO-UNDO.
   DEF VAR lcExcludeBundles       AS CHAR  NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType  AS CHAR  NO-UNDO.
   DEF VAR lcAllowedDSS4SubsType  AS CHAR  NO-UNDO.
   DEF VAR lcDSSRelatedSubsType   AS CHAR  NO-UNDO. 
   DEF VAR llDSSPrimaryAvail      AS LOG   NO-UNDO.
   DEF VAR lcDSSBundleId          AS CHAR  NO-UNDO. 
   DEF VAR lcDSS4CommLine         AS CHAR  NO-UNDO. 

   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bMserviceLPool   FOR MserviceLPool.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.
   DEF BUFFER bMobSub          FOR MobSub.
   DEF BUFFER bDayCampaign     FOR DayCampaign.
   DEF BUFFER lbShaperConf     FOR ShaperConf.

   ASSIGN liPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
          lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES").

   IF fIsDSSActive(INPUT iiCustnum,INPUT ideActStamp) OR
      (ilNewSubscription = TRUE AND
       fOngoingDSSAct(INPUT iiCustnum)) THEN DO:
      ocResult = "dss_already_active".
      RETURN FALSE.
   END. /* IF fIsDSSActive(INPUT iiCustnum) THEN DO: */

   IF icBundleId = {&DSS2} THEN
      ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
             lcDSSRelatedSubsType  = fCParamC("DSS2_PRIMARY_SUBS_TYPE").

   IF icBundleId = {&DSS4} THEN 
      ASSIGN lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE")
             lcDSSRelatedSubsType  = fCParamC("DSS4_PRIMARY_SUBS_TYPE").

   FOR EACH bMobSub NO-LOCK WHERE
            bMobSub.Brand   EQ Syst.Var:gcBrand   AND
            bMobSub.InvCust EQ iiCustnum          AND
            NOT bMobSub.PayType:

      IF icBundleId = {&DSS2} AND
         LOOKUP(bMobSub.CLIType,lcAllowedDSS2SubsType) = 0 THEN NEXT.
           
      IF icBundleId = {&DSS4} AND
         LOOKUP(bMobSub.CLIType,lcAllowedDSS4SubsType) = 0 THEN NEXT.

      IF (icBundleId EQ {&DSS2} OR icBundleId EQ {&DSS4}) AND
        (fCLITypeIsExtraLine(bMobSub.CLIType) OR 
         fCLITypeIsMainLine(bMobSub.CLIType))             THEN 
         IF NOT fCheckActiveExtraLinePair(bMobSub.MsSeq,
                                          bMobSub.CLIType,
                                          OUTPUT lcDSSBundleId) THEN NEXT.
      
      ASSIGN liMobSubCount = liMobSubCount + 1
             lcALLSubsList = lcALLSubsList + ";34" + bMobSub.CLI.
      
      FOR EACH bMServiceLimit NO-LOCK WHERE
               bMServiceLimit.MsSeq    EQ bMobSub.MsSeq     AND
               bMServiceLimit.DialType EQ {&DIAL_TYPE_GPRS} AND
               bMServiceLimit.FromTS   <= ideActStamp       AND
               bMServiceLimit.EndTS    >= ideActStamp,
         FIRST bServiceLimit NO-LOCK WHERE
               bServiceLimit.SLSeq EQ bMServiceLimit.SLSeq,
         FIRST bDayCampaign NO-LOCK WHERE
               bDayCampaign.Brand   EQ Syst.Var:gcBrand    AND
               bDayCampaign.DCEvent EQ bServiceLimit.GroupCode:

         /* might happen with ACC if the old customer has still active DSS */
         IF bDayCampaign.DCEvent BEGINS "DSS" THEN NEXT.

         IF LOOKUP(STRING(bDayCampaign.DCType),
                   {&PERCONTRACT_RATING_PACKAGE}) = 0 AND
            bDayCampaign.DCType NE {&DCTYPE_POOL_RATING} THEN NEXT.

         IF LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 THEN NEXT.

         /* pending termination request */
         IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                           MsRequest.MsSeq      EQ bMServiceLimit.MsSeq            AND
                           MsRequest.ReqType    EQ {&REQTYPE_CONTRACT_TERMINATION} AND
                           MsRequest.ReqCParam3 EQ bServiceLimit.GroupCode         AND
             LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) EQ 0     AND
                     MsRequest.ActStamp <= ideActStamp)                            THEN NEXT.

         IF (icBundleId EQ {&DSS2}  OR 
             icBundleId EQ {&DSS4}) THEN DO:
            IF (LOOKUP(bDayCampaign.DCEvent,lcDSSRelatedSubsType) > 0) OR
               (LOOKUP(bMobSub.CLIType,lcDSSRelatedSubsType)      > 0  AND
                 CAN-FIND(FIRST CLIType NO-LOCK WHERE
                                CLIType.Brand      EQ Syst.Var:gcBrand       AND
                                CLIType.CLIType    EQ bMobSub.CLIType        AND
                                CLIType.BaseBundle EQ bDayCampaign.DCEvent)) THEN
            llDSSPrimaryAvail = TRUE.
         END.

         ldeBundleLimit = 0.
               
         /* All bundle + UPSELL limits for first month DSS */
         IF bDayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO:
            FIND FIRST bMServiceLPool NO-LOCK WHERE
                       bMserviceLPool.MsSeq  EQ bMServiceLimit.MsSeq AND
                       bMserviceLPool.SLSeq  EQ bMServiceLimit.SLSeq AND
                       bMserviceLPool.EndTS  >= ideActStamp          AND
                       bMserviceLPool.FromTS <= ideActStamp          NO-ERROR.
            IF AVAILABLE bMserviceLPool THEN
               ASSIGN odeCurrMonthLimit = odeCurrMonthLimit + bMserviceLPool.LimitAmt
                      ldeBundleLimit    = bMserviceLPool.LimitAmt.
         END. /* IF bDayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO: */
         /* Accumulate only Bundle limit for DSS other months */
         ELSE DO:
            ASSIGN odeCurrMonthLimit  = odeCurrMonthLimit + bMServiceLimit.InclAmt
                   odeOtherMonthLimit = odeOtherMonthLimit + bMServiceLimit.InclAmt
                   ldeBundleLimit     = bMServiceLimit.InclAmt.

            IF LOOKUP("34" + bMobSub.CLI,lcHSDPASubsList,";") = 0 THEN
               lcHSDPASubsList = lcHSDPASubsList + ";34" + bMobSub.CLI.
         END. /* ELSE DO: */

         FIND FIRST bServiceLCounter WHERE
                    bServiceLCounter.MSSeq  = bMobSub.MsSeq       AND
                    bServiceLCounter.SLSeq  = bServiceLimit.SlSeq AND
                    bServiceLCounter.Period = liPeriod NO-LOCK NO-ERROR.
         IF AVAILABLE bServiceLCounter THEN DO:
            ldeServiceLCounterAmt = (bServiceLCounter.Amt / 1024 / 1024).
            IF ldeBundleLimit < ldeServiceLCounterAmt THEN
               odeConsumedData = odeConsumedData + ldeBundleLimit.
            ELSE
               odeConsumedData = odeConsumedData + ldeServiceLCounterAmt.
         END. /* IF AVAILABLE bServiceLCounter THEN DO: */
      END. /* FOR EACH bMServiceLimit WHERE */
   END. /* FOR EACH bMobSub NO-LOCK WHERE */

   ASSIGN lcHSDPASubsList = TRIM(lcHSDPASubsList,";")
          lcALLSubsList   = TRIM(lcALLSubsList,";").

   IF (icBundleId = {&DSS2} OR icBundleId = {&DSS4}) AND 
      NOT llDSSPrimaryAvail THEN DO:
      ocResult = "no_primary_subscription " + icBundleId.
      RETURN FALSE.
   END.

   IF odeOtherMonthLimit <= 0 THEN DO:
      ocResult = "dss_no_data_bundle".
      RETURN FALSE.
   END. /* IF odeOtherMonthLimit <= 0 THEN DO: */

   IF (ilNewSubscription EQ TRUE  AND liMobSubCount < 1) OR
      (ilNewSubscription EQ FALSE AND liMobSubCount < 2) THEN DO:
      ocResult = "dss_no_postpaid_subscription".
      RETURN FALSE.
   END. /* IF liMobSubCount < 2 THEN DO: */

   ASSIGN liTotalDSSLimit = (odeOtherMonthLimit * 1024 * 1024).

   IF icBundleId EQ {&DSS4} THEN DO:

      FIND FIRST lbShaperConf NO-LOCK WHERE
                 lbShaperConf.Brand        EQ Syst.Var:gcBrand AND
                 lbShaperConf.ShaperConfID EQ {&DSS4SHAPERID}  NO-ERROR.

      IF NOT AVAIL lbShaperConf THEN 
         RETURN FALSE.

      ASSIGN odeCurrMonthLimit  = (lbShaperConf.LimitUnshaped / 1024 / 1024)
             odeOtherMonthLimit = (lbShaperConf.LimitUnshaped / 1024 / 1024)
             lcDSS4CommLine     = "DSS-ACCOUNT="    + STRING(iiCustnum)                  + "," +
                                  "TEMPLATE="       + lbShaperConf.Template              + "," +
                                  "TARIFF_TYPE="    + lbShaperConf.TariffType            + "," +
                                  "TARIFF="         + icBundleId                         + "," +
                                  "LIMIT_UNSHAPED=" + STRING(lbShaperConf.LimitUnshaped) + "," +
                                  "LIMIT_SHAPED="   + STRING(lbShaperConf.LimitShaped)   + "," +
                                  "MSISDNS="        + lcALLSubsList.
   END.

   /* If it blanks then return only create param list otherwise specific */
   IF icReturnParamType = "HSDPA_MSISDN" THEN
      ocResult = "MSISDNS=" + lcHSDPASubsList.
   ELSE DO:
      
      IF icBundleId = {&DSS4} THEN 
         ocResult = lcDSS4CommLine.
      ELSE 
         ocResult = "DSS-ACCOUNT="    + STRING(iiCustnum)       + "," +
                    "TEMPLATE=DSS_MONTHLY"                      + "," +
                    "TARIFF_TYPE=DSS"                           + "," +
                    "TARIFF="         + icBundleId              + "," +
                    "LIMIT_UNSHAPED=" + STRING(liTotalDSSLimit) + "," +
                    "LIMIT_SHAPED="   + STRING({&PL_LIMIT_SHAPED}) + "," +
                    "MSISDNS="        + lcALLSubsList.

   END.              
  
   RETURN TRUE.

END FUNCTION. /* FUNCTION fIsDSSAllowed RETURNS LOG */

FUNCTION fIsDSSAllowed RETURNS LOG
   (INPUT  iiCustnum            AS INT,
    INPUT  iiMsSeq              AS INT,
    INPUT  ideActStamp          AS DEC,
    INPUT  icBundleId           AS CHAR,
    INPUT  icReturnParamType    AS CHAR,
    OUTPUT odeCurrMonthLimit    AS DEC,
    OUTPUT odeConsumedData      AS DEC,
    OUTPUT odeOtherMonthLimit   AS DEC,
    OUTPUT ocResult             AS CHAR):
   
   FIND FIRST MobSub WHERE
              MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN DO:
      ocResult = "MobSub not found".
      RETURN FALSE.
   END. /* IF NOT AVAILABLE MobSub THEN DO: */

   IF fMatrixAnalyse(Syst.Var:gcBrand,
                     "PERCONTR",
                     "PerContract;SubsTypeTo",
                     icBundleId + ";" + MobSub.CLIType,
                     OUTPUT ocResult) NE 1 THEN DO:
      ocResult = "ERROR:Contract is not allowed for this subscription type".
      RETURN FALSE.
   END. /* IF fMatrixAnalyse(Syst.Var:gcBrand */

   RETURN fIsDSSAllowedForCustomer(INPUT iiCustnum,
                                   INPUT ideActStamp,
                                   INPUT icBundleId,
                                   INPUT icReturnParamType,
                                   INPUT FALSE, /* is new subscription */
                                   OUTPUT odeCurrMonthLimit,
                                   OUTPUT odeConsumedData,
                                   OUTPUT odeOtherMonthLimit,
                                   OUTPUT ocResult).
END.

FUNCTION fIsDSSActivationAllowed RETURNS LOG
   (INPUT  iiCustnum      AS INT,
    INPUT  iiMsSeq        AS INT,
    INPUT  ideActStamp    AS DEC,
    INPUT  icBundleId     AS CHAR,
    OUTPUT oiDSSPriMsSeq  AS INT, 
    OUTPUT ocResult       AS CHAR):

   DEF VAR liMobSubCount           AS INT  NO-UNDO.
   DEF VAR lcExcludeBundles        AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType   AS CHAR NO-UNDO.
   DEF VAR lcDSS2PrimarySubsType   AS CHAR NO-UNDO.
   DEF VAR llDSSPrimaryAvail       AS LOG  NO-UNDO.
   DEF VAR lcAllowedDSS4SubsType   AS CHAR NO-UNDO.
   DEF VAR lcDSS4PrimarySubsType   AS CHAR NO-UNDO.
   DEF VAR llDSS4PrimaryAvail      AS LOG  NO-UNDO.
   DEF VAR lcDSSRelatedSubsType    AS CHAR NO-UNDO. 
   DEF VAR lcDSSBundleId           AS CHAR NO-UNDO. 

   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.
   DEF BUFFER bMobSub          FOR MobSub.
   DEF BUFFER bDayCampaign     FOR DayCampaign.

   ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
          lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE")
          lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE")
          lcDSS4PrimarySubsType = fCParamC("DSS4_PRIMARY_SUBS_TYPE")
          lcExcludeBundles      = fCParamC("EXCLUDE_BUNDLES")
          lcDSSRelatedSubsType  = "".

   IF icBundleId EQ {&DSS4} THEN 
      lcDSSRelatedSubsType = lcDSS4PrimarySubsType.
   ELSE IF icBundleId EQ {&DSS2} THEN 
      lcDSSRelatedSubsType = lcDSS2PrimarySubsType.

   IF iiMsSeq > 0 THEN DO:

      FIND FIRST bMobSub NO-LOCK WHERE
                 bMobSub.MsSeq EQ iiMsSeq NO-ERROR.
   
      IF NOT AVAILABLE bMobSub THEN DO:
         ocResult = "MobSub not found".
         RETURN FALSE.
      END. /* IF NOT AVAILABLE bMobSub THEN DO: */

      IF fMatrixAnalyse(Syst.Var:gcBrand,
                        "PERCONTR",
                        "PerContract;SubsTypeTo",
                        icBundleId + ";" + bMobSub.CLIType,
                        OUTPUT ocResult) NE 1 THEN DO:
         ocResult = "ERROR:Contract is not allowed for this subscription type".
         RETURN FALSE.
      END. /* IF fMatrixAnalyse(Syst.Var:gcBrand */

      IF LOOKUP(bMobSub.CLIType,lcDSSRelatedSubsType) > 0 AND 
        (fCLITypeIsExtraLine(bMobSub.CLIType) OR
         fCLITypeIsMainLine(bMobSub.CLIType))             THEN
         IF NOT fCheckActiveExtraLinePair(bMobSub.MsSeq,
                                          bMobSub.CLIType,
                                          OUTPUT lcDSSBundleId) THEN DO:
            ocResult = "Primary manline or Extraline is not hard associated".
            RETURN FALSE.
         END. 

   END. /* IF iiMsSeq > 0 THEN DO: */

   MOBSUB_LOOP:
   FOR EACH bMobSub WHERE
            bMobSub.Brand   EQ Syst.Var:gcBrand AND
            bMobSub.InvCust EQ iiCustnum        AND
            NOT bMobSub.PayType NO-LOCK:

      /* Exclude current operated subs and not allowed list */
      IF bMobSub.MsSeq EQ iiMsSeq OR
         LOOKUP(bMobSub.CLIType,lcDSSRelatedSubsType) EQ 0 THEN NEXT.

      /* Extraline hard association subscription check */
      IF LOOKUP(bMobSub.CLIType,lcDSSRelatedSubsType) > 0 AND
         (fCLITypeIsExtraLine(bMobSub.CLIType) OR
          fCLITypeIsMainLine(bMobSub.CLIType))            THEN
          IF NOT fCheckActiveExtraLinePair(bMobSub.MsSeq,
                                           bMobSub.CLIType,
                                           OUTPUT lcDSSBundleId) THEN NEXT. 
      
      /* Exclude subs. if termination request is ongoing */
      IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                         MsRequest.MsSeq    EQ bMobSub.MsSeq                           AND
                         MsRequest.ReqType  EQ {&REQTYPE_SUBSCRIPTION_TERMINATION}     AND
                         MsRequest.ActStamp <= ideActStamp                             AND
           LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3,19") = 0) THEN NEXT.

      /* Exclude subs. if STC request is ongoing */
      IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                         MsRequest.MsSeq    EQ bMobSub.MsSeq                          AND
                         MsRequest.ReqType  EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}    AND
                         MsRequest.ActStamp <= ideActStamp                            AND
           LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3,19") = 0 AND
                  LOOKUP(MsRequest.ReqCParam2,lcAllowedDSS2SubsType) = 0)             THEN NEXT.
      
      /* Exclude subs. if ACC request is ongoing */
      IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                         MsRequest.MsSeq    EQ bMobSub.MsSeq                        AND
                         MsRequest.ReqType  EQ {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE} AND
                         MsRequest.ActStamp <= ideActStamp                          AND
          LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") = 0)  THEN NEXT.

      liMobSubCount = liMobSubCount + 1.
      
      FOR EACH bMServiceLimit WHERE
               bMServiceLimit.MsSeq    EQ bMobSub.MsSeq     AND
               bMServiceLimit.DialType EQ {&DIAL_TYPE_GPRS} AND
               bMServiceLimit.FromTS   <= ideActStamp       AND
               bMServiceLimit.EndTS    >= ideActStamp       NO-LOCK,
         FIRST bServiceLimit NO-LOCK WHERE
               bServiceLimit.SLSeq EQ bMServiceLimit.SLSeq,
         FIRST bDayCampaign NO-LOCK WHERE
               bDayCampaign.Brand   EQ Syst.Var:gcBrand        AND
               bDayCampaign.DCEvent EQ bServiceLimit.GroupCode AND
               LOOKUP(bDayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) GT 0:

         /* Should not count DSS/UPSELL */
         IF bDayCampaign.DCEvent BEGINS {&DSS} OR
            LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 THEN NEXT.

         /* pending termination request */
         IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                           MsRequest.MsSeq      EQ bMServiceLimit.MsSeq            AND
                           MsRequest.ReqType    EQ {&REQTYPE_CONTRACT_TERMINATION} AND
                           MsRequest.ReqCParam3 EQ bServiceLimit.GroupCode         AND
             LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) EQ 0     AND
                           MsRequest.ActStamp <= ideActStamp)                      THEN NEXT.

         IF NOT llDSSPrimaryAvail THEN DO:
            IF (LOOKUP(bDayCampaign.DCEvent,lcDSSRelatedSubsType) GT 0) OR
               (LOOKUP(bMobSub.CLIType,lcDSSRelatedSubsType)      GT 0  AND 
                CAN-FIND(FIRST CLIType NO-LOCK WHERE 
                               CLIType.Brand      EQ Syst.Var:gcBrand       AND 
                               CLIType.CLIType    EQ bMobSub.CLIType        AND 
                               CLIType.BaseBundle EQ bDayCampaign.DCEvent)) THEN DO:
               ASSIGN oiDSSPriMsSeq     = bMobSub.MsSeq
                      llDSSPrimaryAvail = TRUE.
               LEAVE.
            END.
         END.

      END. /* FOR EACH bMServiceLimit WHERE */

      IF llDSSPrimaryAvail AND
         ((iiMsSeq > 0 AND liMobSubCount >= 1) OR
          (iiMsSeq = 0 AND liMobSubCount >= 2)) THEN LEAVE MOBSUB_LOOP.

   END. /* FOR EACH bMobSub NO-LOCK WHERE */

   IF NOT llDSSPrimaryAvail THEN DO:
      ocResult = "Customer does not have any active primary subscription".
      RETURN FALSE.
   END.

   IF (iiMsSeq > 0 AND liMobSubCount < 1) OR
      (iiMsSeq = 0 AND liMobSubCount < 2) THEN DO:
      ocResult = "Customer should have at least 2 active postpaid subscriptions.".
      RETURN FALSE.
   END. /* IF (iiMsSeq > 0 AND liMobSubCount < 1) OR */
 
   RETURN TRUE.

END.

FUNCTION fgetFlexUpsellBundle RETURNS CHAR
     (INPUT  iiCustnum      AS INT,
      INPUT  iiMsSeq        AS INT,
      INPUT  icDSSId        AS CHAR,
      INPUT  icBundle       AS CHAR,
      INPUT  ideActStamp    AS DEC):
   /* DSS related variables */
   DEF VAR lcResult              AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.
   DEF VAR llDSSneeded           AS LOG  NO-UNDO.
   DEF VAR lcDSSBundleId         AS CHAR NO-UNDO. 

   DEF BUFFER Mobsub FOR Mobsub.

   lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

   IF icDSSId BEGINS "DSS" THEN
      llDSSNeeded = TRUE.
   IF icDSSId EQ "DSS" THEN DO: 
      IF NOT fIsDSSActive(INPUT iiCustnum,INPUT Func.Common:mMakeTS()) THEN
         llDSSNeeded = FALSE.
   END.
   ELSE IF icDSSId EQ "DSS2" THEN DO:
      
      FIND FIRST MobSub WHERE Mobsub.msseq EQ iiMsseq NO-LOCK NO-ERROR.
      
      IF AVAIL MobSub THEN DO:
         IF fMatrixAnalyse(Syst.Var:gcBrand,
                           "PERCONTR",
                           "PerContract;SubsTypeTo",
                           "DSS2" + ";" + MobSub.CLIType,
                           OUTPUT lcResult) NE 1 THEN
            llDSSNeeded = FALSE.
         
         ELSE IF LOOKUP(Mobsub.CLIType,lcAllowedDSS2SubsType)   > 0  AND
           (fCLITypeIsMainLine(Mobsub.CLIType) OR
            fCLITypeIsExtraLine(Mobsub.CLIType)) THEN
            IF NOT fCheckActiveExtraLinePair(Mobsub.MsSeq,
                                             Mobsub.CLIType,
                                             OUTPUT lcDSSBundleId) THEN
            llDSSNeeded = FALSE.
      END.
      ELSE
         llDSSNeeded = FALSE. /* Should not ever come here */
   END.
   ELSE /* not known/supported DSS */
      llDSSNeeded = FALSE.
   IF llDSSNeeded THEN DO: 
      IF icBundle MATCHES "*FLEX_500MB_UPSELL" THEN DO:
         RETURN "DSS_FLEX_500MB_UPSELL".
      END.
      ELSE IF icBundle MATCHES "*FLEX_5GB_UPSELL" THEN
         RETURN "DSS_FLEX_5GB_UPSELL".
   END.
   ELSE
      IF icBundle MATCHES "*FLEX_500MB_UPSELL" THEN DO:
         RETURN "FLEX_500MB_UPSELL".
      END.
      ELSE IF icBundle MATCHES "*FLEX_5GB_UPSELL" THEN
         RETURN "FLEX_5GB_UPSELL".
END.

FUNCTION fCanDSSKeepActive RETURNS LOG
   (INPUT  iiCustnum      AS INT,
    INPUT  iiMsSeq        AS INT,
    INPUT  ideActStamp    AS DEC,
    INPUT  icBundleId     AS CHAR,
    OUTPUT ocResult       AS CHAR):

   DEF VAR liMobSubCount         AS INT   NO-UNDO.
   DEF VAR llDataBundleActive    AS LOG   NO-UNDO.
   DEF VAR lcExcludeBundles      AS CHAR  NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType AS CHAR  NO-UNDO.
   DEF VAR lcDSS2PrimarySubsType AS CHAR  NO-UNDO.
   DEF VAR llDSSPrimaryAvail     AS LOG   NO-UNDO.
   DEF VAR lcDSS4PrimarySubsType AS CHAR  NO-UNDO. 
   DEF VAR lcAllowedDSS4SubsType AS CHAR  NO-UNDO. 
   DEF VAR lcDSSRelatedSubsType  AS CHAR  NO-UNDO. 

   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.
   DEF BUFFER bMobSub          FOR MobSub.
   DEF BUFFER bDayCampaign     FOR DayCampaign.
   DEF BUFFER MsRequest        FOR MsRequest.

   lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES").

   ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
          lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE")
          lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE")
          lcDSS4PrimarySubsType = fCParamC("DSS4_PRIMARY_SUBS_TYPE").

   IF icBundleId EQ {&DSS4} THEN
      lcDSSRelatedSubsType = lcAllowedDSS4SubsType.
   ELSE IF icBundleId EQ {&DSS2} THEN
      lcDSSRelatedSubsType = lcAllowedDSS2SubsType.     

   MOBSUB_LOOP:
   FOR EACH bMobSub NO-LOCK WHERE
            bMobSub.Brand   EQ Syst.Var:gcBrand AND
            bMobSub.InvCust EQ iiCustnum        AND
            NOT bMobSub.PayType:

      /* Exclude current operated subs. to check further DSS valid */
      IF bMobSub.MsSeq = iiMsSeq OR
         ((icBundleId = {&DSS2} OR icBundleId EQ {&DSS4}) AND
          LOOKUP(bMobSub.CLIType,lcDSSRelatedSubsType) = 0) THEN NEXT.

      /* Exclude subs. if termination request is ongoing */
      IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                         MsRequest.MsSeq    EQ bMobSub.MsSeq                       AND
                         MsRequest.ReqType  EQ {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                         MsRequest.ActStamp <= ideActStamp                         AND
           LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3,19") = 0) THEN NEXT.

      /* Exclude subs. if STC request is ongoing */
      IF (icBundleId EQ {&DSS2} OR icBundleId EQ {&DSS4}) AND
         CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                         MsRequest.MsSeq    EQ bMobSub.MsSeq                          AND
                         MsRequest.ReqType  EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}    AND
                         MsRequest.ActStamp <= ideActStamp                            AND
           LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3,19") = 0 AND
                  LOOKUP(MsRequest.ReqCParam2,lcAllowedDSS2SubsType) = 0)             THEN NEXT.

      IF icBundleId = "DSS" THEN DO:
         FOR EACH MsRequest NO-LOCK WHERE
                  MsRequest.MsSeq    EQ bMobSub.MsSeq                       AND
                  MsRequest.ReqType  EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                  MsRequest.ActStamp <= ideActStamp                         AND
                  LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") = 0:

             IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                               CLIType.CLIType = MsRequest.ReqCparam2 AND
                               CLIType.PayType = {&CLITYPE_PAYTYPE_PREPAID})
               THEN NEXT MOBSUB_LOOP.
         END.
      END.

      /* Exclude subs. if ACC request is ongoing */
      IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                         MsRequest.MsSeq    EQ bMobSub.MsSeq                        AND
                         MsRequest.ReqType  EQ {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE} AND
                         MsRequest.ActStamp <= ideActStamp                          AND
                   LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") = 0) THEN NEXT.

      liMobSubCount = liMobSubCount + 1.

      FOR EACH bMServiceLimit NO-LOCK WHERE
               bMServiceLimit.MsSeq    EQ bMobSub.MsSeq     AND
               bMServiceLimit.DialType EQ {&DIAL_TYPE_GPRS} AND
               bMServiceLimit.FromTS   <= ideActStamp       AND
               bMServiceLimit.EndTS    >= ideActStamp,
         FIRST bServiceLimit NO-LOCK WHERE
               bServiceLimit.SLSeq EQ bMServiceLimit.SLSeq,
         FIRST bDayCampaign NO-LOCK WHERE
               bDayCampaign.Brand   EQ Syst.Var:gcBrand        AND
               bDayCampaign.DCEvent Eq bServiceLimit.GroupCode AND
               LOOKUP(bDayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0:

         /* Should not count DSS/UPSELL */
         IF bDayCampaign.DCEvent BEGINS {&DSS} OR
            LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 THEN NEXT.

         /* pending termination request */
         IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                           MsRequest.MsSeq      EQ bMServiceLimit.MsSeq            AND
                           MsRequest.ReqType    EQ {&REQTYPE_CONTRACT_TERMINATION} AND
                           MsRequest.ReqCParam3 EQ bServiceLimit.GroupCode         AND
             LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0      AND
                           MsRequest.ActStamp   <= ideActStamp)                    THEN NEXT.

         IF (icBundleId EQ {&DSS2} OR icBundleId EQ {&DSS4}) AND
            ((LOOKUP(bDayCampaign.DCEvent,lcDSSRelatedSubsType) GT 0) OR
             (LOOKUP(bMobSub.CLIType,lcDSSRelatedSubsType)      GT 0  AND
                CAN-FIND(FIRST CLIType NO-LOCK WHERE
                               CLIType.Brand      EQ Syst.Var:gcBrand AND
                               CLIType.CLIType    EQ bMobSub.CLIType  AND
                               CLIType.BaseBundle EQ bDayCampaign.DCEvent))) THEN
            llDSSPrimaryAvail = TRUE.

         llDataBundleActive = TRUE.

      END. /* FOR EACH bMServiceLimit WHERE */

   END. /* FOR EACH bMobSub NO-LOCK WHERE */

   IF (icBundleId EQ {&DSS2} OR icBundleId EQ {&DSS4}) 
      AND NOT llDSSPrimaryAvail THEN DO:
      ocResult = "Customer does not have any active primary subscription".
      RETURN FALSE.
   END.

   IF liMobSubCount < 2 THEN DO:
      ocResult = "Customer should have at least 2 active postpaid subscriptions.".
      RETURN FALSE.
   END. /* IF liMobSubCount < 2 THEN DO: */

   IF NOT llDataBundleActive THEN DO:
      ocResult = "Customer does not have any active data bundle.".
      RETURN FALSE.
   END. /* IF NOT llDataBundleActive THEN DO: */
 
   RETURN TRUE.

END FUNCTION. /* FUNCTION fCanDSSKeepActive RETURNS LOG */


FUNCTION fIsDSSTransferAllowed RETURNS LOG
   (INPUT  icCurrentCLI         AS CHAR,
    INPUT  iiCustnum            AS INT,
    INPUT  ideCheckTS           AS DEC,
    INPUT  icBundleId           AS CHAR,
    OUTPUT oiDSSTransToMsSeq    AS INT,
    OUTPUT ocError              AS CHAR):

   DEF VAR liMobSubCount           AS INT  NO-UNDO.
   DEF VAR llFirstActBundle        AS LOG  NO-UNDO INIT TRUE.
   DEF VAR llActOtherBundle        AS LOG  NO-UNDO.
   DEF VAR lcExcludeBundles        AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType   AS CHAR NO-UNDO.
   DEF VAR lcDSS2PrimarySubsType   AS CHAR NO-UNDO.

   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bMobSub          FOR MobSub.
   DEF BUFFER bDayCampaign     FOR DayCampaign.

   lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES").

   IF icBundleId = "DSS2" THEN
      ASSIGN lcAllowedDSS2SubsType   = fCParamC("DSS2_SUBS_TYPE")
             lcDSS2PrimarySubsType   = fCParamC("DSS2_PRIMARY_SUBS_TYPE").

   FOR EACH bMobSub WHERE
            bMobSub.Brand   = Syst.Var:gcBrand      AND
            bMobSub.InvCust = iiCustnum    AND
            NOT bMobSub.PayType NO-LOCK BY ActivationDate:

      IF bMobSub.CLI = icCurrentCLI OR
         (icBundleId = "DSS2" AND
          LOOKUP(bMobSub.CLIType,lcAllowedDSS2SubsType) = 0) THEN NEXT.

      /* Exclude subs. if termination request is ongoing */
      IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                   MsRequest.MsSeq   = bMobSub.MsSeq AND
                   MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                   MsRequest.ActStamp <= ideCheckTS  AND
                   LOOKUP(STRING(MsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES} + ",3") = 0) THEN NEXT.

      liMobSubCount = liMobSubCount + 1.

      FOR EACH bMServiceLimit WHERE
               bMServiceLimit.MsSeq    = bMobSub.MsSeq  AND
               bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
               bMServiceLimit.FromTS  <= ideCheckTS     AND
               bMServiceLimit.EndTS   >= ideCheckTS NO-LOCK,
         FIRST bServiceLimit NO-LOCK WHERE
               bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
         FIRST bDayCampaign WHERE
               bDayCampaign.Brand = Syst.Var:gcBrand AND
               bDayCampaign.DCEvent = bServiceLimit.GroupCode AND
               LOOKUP(STRING(bDayCampaign.DCType),
                      {&PERCONTRACT_RATING_PACKAGE}) > 0 NO-LOCK:

             IF LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 THEN NEXT.

             /* pending termination request */
             IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                     MsRequest.MsSeq = bMServiceLimit.MsSeq AND
                     MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                     MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                     LOOKUP(STRING(MsRequest.ReqStatus),
                                   {&REQ_INACTIVE_STATUSES}) = 0 AND
                     MsRequest.ActStamp <= ideCheckTS) THEN NEXT.

             IF llFirstActBundle THEN DO:
                IF icBundleId = "DSS2" THEN DO:
                   IF LOOKUP(bDayCampaign.DCEvent,lcDSS2PrimarySubsType) > 0 THEN
                      ASSIGN oiDSSTransToMsSeq = bMobSub.MsSeq
                             llFirstActBundle  = FALSE.
                END.
                ELSE
                   ASSIGN oiDSSTransToMsSeq = bMobSub.MsSeq
                          llFirstActBundle  = FALSE.
             END. /* IF llFirstActBundle THEN DO: */

             llActOtherBundle = TRUE.

      END. /* FOR EACH bMServiceLimit WHERE */
   END. /* FOR EACH bMobSub WHERE */

   IF icBundleId = "DSS2" AND oiDSSTransToMsSeq = 0 THEN DO:
      ocError = "Customer does not have any active primary subscription".
      RETURN FALSE.
   END.

   IF liMobSubCount < 2 THEN DO:
      ocError = "Customer should have at least 2 active postpaid subscriptions".
      RETURN FALSE.
   END. /* IF liMobSubCount < 2 THEN DO: */

   IF NOT llActOtherBundle THEN DO:
      ocError = "Customer does not have any active data bundle".
      RETURN FALSE.
   END. /* IF NOT llActOtherBundle THEN DO: */
  
   RETURN TRUE.

END FUNCTION. /* FUNCTION fIsDSSTransferAllowed RETURNS LOG */


FUNCTION fTransferDSS RETURNS LOG
   (INPUT  iiCurrentDSSMsSeq  AS INT,
    INPUT  iiDSSTransToMsSeq  AS INT,
    INPUT  idActionDate       AS DATE,
    INPUT  icUserCode         AS CHAR,
    INPUT  icProcess          AS CHAR,
    OUTPUT ocError            AS CHAR):

   DEF VAR llTransferDSS       AS LOG  NO-UNDO.
   DEF VAR ldMonthEndDate      AS DATE NO-UNDO.
   DEF VAR ldNextMonthFromDate AS DATE NO-UNDO.
   DEF VAR ldEndDate           AS DATE NO-UNDO.
   DEF VAR ldReqAmt            AS DEC  NO-UNDO.
   DEF VAR liPeriod            AS INT  NO-UNDO.
   
   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bMobSub          FOR MobSub.

   ASSIGN ldMonthEndDate      = Func.Common:mLastDayOfMonth(idActionDate)
          ldNextMonthFromDate = ldMonthEndDate + 1
          liPeriod            = YEAR(idActionDate) * 100 + MONTH(idActionDate).

   FIND FIRST bMobSub WHERE
              bMobSub.MsSeq = iiDSSTransToMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL bMobSub THEN DO:
      ocError = "MobSub not found for Subs.Id:" + STRING(iiDSSTransToMsSeq).
      RETURN FALSE.
   END. /* IF NOT AVAIL bMobSub THEN DO: */

   DO TRANSACTION:
      FIND FIRST FixedFee WHERE
                 FixedFee.Brand     = Syst.Var:gcBrand   AND
                 FixedFee.HostTable = "MobSub"  AND
                 FixedFee.KeyValue  = STRING(iiCurrentDSSMsSeq) AND
                 FixedFee.CalcObj   = {&DSS}    AND
                 FixedFee.EndPeriod >= liPeriod
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL FixedFee THEN DO:
         /* Close Fee from current subscription */
         RUN Mc/closefee.p(FixedFee.FFNum,
                      ldMonthEndDate,
                      FALSE,
                      FALSE,
                      iiCurrentDSSMsSeq,
                      "",
                      icUserCode,
                      icProcess, /* fee_memo */
                      0,
                      FALSE, /* Full monthly fee */
                      OUTPUT ldReqAmt).

         /* Update First month fee based on the number of days if not billed */
         IF DAY(FixedFee.BegDate) > 1 THEN DO:
            FIND FIRST FFItem OF FixedFee EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE FFItem AND NOT FFItem.BILLED THEN DO:
               ldEndDate = fInt2Date(FixedFee.BegPeriod,2).
               ASSIGN
                  FFItem.Amt = FixedFee.Amt * (ldEndDate - fixedfee.begdate + 1) /
                               DAY(ldEndDate)
                  FixedFee.ServiceLimitGroup = "PMF:DSS:" + STRING(FFItem.Amt).
            END. /* IF AVAILABLE FFItem AND NOT FFItem.BILLED THEN DO: */
         END. /* IF DAY(FixedFee.BegPeriod) > 1 AND FMItem.FirstMonthBR = 0 */

         /* Open fee on new DSS subs. from next month */
         RUN Mc/creasfee.p(bMobSub.CustNum,
                      bMobSub.MsSeq,
                      ldNextMonthFromDate,
                      "FeeModel",
                      "DSSMF",
                      9,
                      ?,
                      "DSS" + " created " + 
                      STRING(ldNextMonthFromDate,"99.99.9999") +  /* memo */
                      "¤" + "DSS",    /* calcobject */
                      FALSE,              /* no messages to screen */
                      icUserCode,
                      icProcess,
                      0,
                      "",
                      "",
                      OUTPUT ocError).

         IF ocError BEGINS "ERROR:" OR ocError BEGINS "0" THEN DO:
            ocError = "Fee creation (DSSMF) failed for new contract: " +
                      ocError.
            RETURN FALSE.
         END. /* IF ocError BEGINS "ERROR:" */
      END. /* IF AVAIL FixedFee THEN DO: */

      FOR EACH bServiceLimit WHERE
               bServiceLimit.GroupCode BEGINS {&DSS} NO-LOCK,
          EACH bMServiceLimit WHERE
               bMServiceLimit.MsSeq  = iiCurrentDSSMsSeq   AND
               bMServiceLimit.SLSeq  = bServiceLimit.SLSeq AND
               bMServiceLimit.EndTS  >= Func.Common:mMakeTS() EXCLUSIVE-LOCK:
         ASSIGN bMServiceLimit.MsSeq = bMobSub.MsSeq
                llTransferDSS        = TRUE.
      END. /* FOR EACH bServiceLimit WHERE */
   END. /* DO TRANSACTION: */

   IF llTransferDSS THEN RETURN TRUE.
   ELSE RETURN FALSE.

END FUNCTION. /* FUNCTION fTransferDSS RETURNS LOG */


FUNCTION fGetActiveBundleLimit RETURNS DEC
   (iiMsSeq      AS INT,
    ideActStamp AS DEC):

   DEF VAR ldeDataBundleLimits AS DEC  NO-UNDO.
   DEF VAR lcExcludeBundles    AS CHAR NO-UNDO.

   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bMsRequest       FOR MsRequest.

   lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES").

   FOR EACH bMServiceLimit NO-LOCK WHERE
            bMServiceLimit.MsSeq   = iiMsSeq AND
            bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
            bMServiceLimit.FromTS <= ideActStamp AND
            bMServiceLimit.EndTS  >= ideActStamp, 
      FIRST bServiceLimit NO-LOCK WHERE
            bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
      FIRST DayCampaign NO-LOCK WHERE 
            DayCampaign.Brand   = Syst.Var:gcBrand AND
            DayCampaign.DCEvent = bServiceLimit.GroupCode AND
            LOOKUP(STRING(DayCampaign.DCType),
                   {&PERCONTRACT_RATING_PACKAGE}) > 0:

      /* Should not return DSS in active bundle list */
      IF bServiceLimit.GroupCode BEGINS {&DSS} OR
         LOOKUP(bServiceLimit.GroupCode,lcExcludeBundles) > 0
      THEN NEXT.

      /* pending termination request */
      IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                  MsRequest.MsSeq = bMServiceLimit.MsSeq AND
                  MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                  MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                  LOOKUP(STRING(MsRequest.ReqStatus),
                                {&REQ_INACTIVE_STATUSES}) = 0 AND
                  MsRequest.ActStamp <= ideActStamp) THEN NEXT.
      
      ldeDataBundleLimits = ldeDataBundleLimits + bMServiceLimit.InclAmt.
   END. /* FOR EACH bMServiceLimit NO-LOCK WHERE */

   RETURN ldeDataBundleLimits.
    
END FUNCTION. /* FUNCTION fGetActiveBundleLimit */

FUNCTION fGetActiveBonoLimit RETURNS DEC
   (iiMsSeq      AS INT,
    ideActStamp AS DEC):

   DEF VAR ldeDataBundleLimit  AS DEC  NO-UNDO.
   DEF VAR lcBonoContracts     AS CHAR NO-UNDO.

   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.

   lcBonoContracts = fCParamC("BONO_CONTRACTS").

   FOR EACH bMServiceLimit NO-LOCK WHERE
            bMServiceLimit.MsSeq   = iiMsSeq AND
            bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
            bMServiceLimit.FromTS <= ideActStamp AND
            bMServiceLimit.EndTS  >= ideActStamp, 
      FIRST bServiceLimit NO-LOCK WHERE
            bServiceLimit.SLSeq = bMServiceLimit.SLSeq AND
            LOOKUP(bServiceLimit.GroupCode,lcBonoContracts) > 0:

      /* pending termination request */
      IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                  MsRequest.MsSeq = bMServiceLimit.MsSeq AND
                  MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                  MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                  LOOKUP(STRING(MsRequest.ReqStatus),
                                {&REQ_INACTIVE_STATUSES}) = 0 AND
                  MsRequest.ActStamp <= ideActStamp) THEN NEXT.
      
      ldeDataBundleLimit = bMServiceLimit.InclAmt.
      LEAVE.
   END. /* FOR EACH bMServiceLimit NO-LOCK WHERE */

   RETURN ldeDataBundleLimit.
    
END FUNCTION. /* FUNCTION fGetActiveBundleLimit */

FUNCTION fMakeDSSCommLine RETURNS CHAR
(INPUT iiSolog     AS INT,
 INPUT iiMsRequest AS INT).

   DEF VAR lcReturn            AS CHAR  NO-UNDO.
   DEF VAR lcAction            AS CHAR  NO-UNDO.
   DEF VAR liDSSMsSeq          AS INT   NO-UNDO.
   DEF VAR ldeCurrentDSSLimit  AS DEC   NO-UNDO.
   DEF VAR liDSSLimit          AS INT64 NO-UNDO.
   DEF VAR lcDSSBundleId       AS CHAR  NO-UNDO.

   DEF BUFFER ProvSolog        FOR Solog.
   DEF BUFFER ProvMsRequest    FOR MsRequest.
   DEF BUFFER lbShaperConf     FOR ShaperConf.   

   FIND FIRST ProvSolog WHERE
              ProvSolog.Solog = iiSolog NO-LOCK NO-ERROR.

   FIND FIRST ProvMsRequest WHERE 
              ProvMsRequest.Msrequest = iiMsRequest NO-LOCK NO-ERROR.

   /* Get latest DSS Limit to send to Network */
   IF ProvMsRequest.ReqCparam1 = "MODIFY" AND
      (ProvMsRequest.ReqCparam2 = "" OR
       INDEX(ProvMsRequest.ReqCparam2,"LIMIT") > 0) THEN
      fGetDSSMsSeqLimit(INPUT ProvMsRequest.CustNum,
                        INPUT (IF ProvMsRequest.ActStamp > Func.Common:mMakeTS() THEN
                               ProvMsRequest.ActStamp ELSE Func.Common:mMakeTS()),
                        OUTPUT liDSSMsSeq,
                        OUTPUT ldeCurrentDSSLimit,
                        OUTPUT lcDSSBundleId).

   IF ldeCurrentDSSLimit > 0 THEN DO TRANSACTION:
      ASSIGN liDSSLimit = (ldeCurrentDSSLimit * 1024 * 1024).
         
      FIND CURRENT ProvMsRequest EXCLUSIVE-LOCK.

      IF AVAILABLE ProvMsRequest THEN DO:
         
         IF ProvMSRequest.ReqCparam3 EQ {&DSS4} THEN DO: 
            FIND FIRST lbShaperConf NO-LOCK WHERE
                       lbShaperConf.Brand        EQ Syst.Var:gcBrand AND
                       lbShaperConf.ShaperConfID EQ {&DSS4SHAPERID}  NO-ERROR.

            IF NOT AVAIL lbShaperConf THEN
               RETURN lcReturn.

            ProvMSRequest.ReqCparam2 = "DSS-ACCOUNT="    + STRING(ProvMSRequest.CustNum)      + "," +
                                       "TEMPLATE="       + lbShaperConf.Template              + "," +
                                       "TARIFF_TYPE="    + lbShaperConf.TariffType            + "," +
                                       "TARIFF="         + ProvMSRequest.ReqCparam3           + "," +
                                       "LIMIT_UNSHAPED=" + STRING(lbShaperConf.LimitUnshaped) + "," +
                                       "LIMIT_SHAPED="   + STRING(lbShaperConf.LimitShaped).
         END.
         ELSE    
            ProvMSRequest.ReqCparam2 = "DSS-ACCOUNT=" + STRING(ProvMSRequest.CustNum)  + "," +
                                       "TEMPLATE=DSS_MONTHLY"                          + "," +
                                       "TARIFF_TYPE=DSS"                               + "," +
                                       "TARIFF="          + ProvMsRequest.ReqCparam3   + "," +
                                       "LIMIT_UNSHAPED="  + STRING(liDSSLimit)         + "," +
                                       "LIMIT_SHAPED="    + STRING({&PL_LIMIT_SHAPED}).

      END.

      FIND CURRENT ProvMSRequest NO-LOCK.
   END. /* IF ProvMSRequest.ReqCparam1 = "MODIFY" THEN DO: */

   IF ProvMsRequest.ReqCparam1 = "ADD" THEN
      lcAction = "MODIFY".
   ELSE IF ProvMsRequest.ReqCparam1 = "REMOVE" THEN
      lcAction = "DELETE".
   ELSE lcAction = ProvMsRequest.ReqCparam1.

   lcReturn = STRING(ProvSolog.Solog)  + " " +
              lcAction                 + "," + /* Action  */
              ProvMsRequest.ReqCparam2 + "," +
              "OPERATOR=YOIGO".
  
   RETURN lcReturn.

END. /* FUNCTION fMakeDSSCommLine RETURNS CHAR */

PROCEDURE pUpdateDSSLimit:

   DEF INPUT PARAMETER iiCustNum    AS INT  NO-UNDO.
   DEF INPUT PARAMETER icType       AS CHAR NO-UNDO.
   DEF INPUT PARAMETER ideAmt       AS DEC  NO-UNDO.
   DEF INPUT PARAMETER ideConsAmt   AS DEC  NO-UNDO.
   DEF INPUT PARAMETER ideFromTS    AS DEC  NO-UNDO.
   DEF OUTPUT PARAMETER odeDSSLimit AS DEC  NO-UNDO.

   DEF BUFFER MDSSServiceLimit      FOR MServiceLimit.
   DEF BUFFER bServiceLimit         FOR ServiceLimit.
   DEF BUFFER bServiceLCounter      FOR ServiceLCounter.

   DEF VAR liPeriod                 AS INT NO-UNDO.
   DEF VAR liCount                  AS INT NO-UNDO.
   DEF VAR liTime AS INT NO-UNDO. 
   DEF VAR ldaDate AS DATE NO-UNDO. 
   DEF VAR ldePoolFromTS AS DEC NO-UNDO. 
   DEF VAR ldePoolAmt AS DEC NO-UNDO. 
   DEF VAR ldeLastSecondOfMonth AS DEC NO-UNDO. 
   DEF VAR ldeFirstSecondOfMonth AS DEC NO-UNDO. 
         
   liPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).

   FOR EACH bServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "bServiceLimit.GroupCode"},
       EACH MDSSServiceLimit EXCLUSIVE-LOCK WHERE
            MDSSServiceLimit.CustNum = iiCustNum           AND
            MDSSServiceLimit.DialType = bServiceLimit.DialType AND
            MDSSServiceLimit.SlSeq   = bServiceLimit.SlSeq AND
            MDSSServiceLimit.EndTS  >= ideFromTS:
      
      IF icType EQ "UPDATE" AND
         MDSSServiceLimit.EndTS >= ideFromTS AND
         MDSSServiceLimit.FromTS <= ideFromTS THEN DO:
            
         Func.Common:mSplitTS(ideFromTS, output ldaDate, output litime).

         ASSIGN
            ldePoolFromTS = ?
            ldeFirstSecondOfMonth = Func.Common:mHMS2TS(DATE(MONTH(ldaDate),1,
                                                 YEAR(ldaDate)), "00:00:00").
            ldeLastSecondOfMonth = Func.Common:mHMS2TS(Func.Common:mLastDayOfMonth(ldaDate),"23:59:59").
         
            /* should not normally happen, means that other bundle has already
               been added to future (older bundle has gone to error) */
            FOR EACH MserviceLPool WHERE
                     MserviceLPool.custnum = iiCustNum AND
                     MserviceLPool.slseq = MDSSServiceLimit.SlSeq AND
                     MserviceLPool.EndTS >= ideFromTS EXCLUSIVE-LOCK:
               
               IF MServiceLPool.FromTS <= ideFromTS THEN ASSIGN
                  ldePoolFromTS = MserviceLPool.FromTS
                  ldePoolAmt = MserviceLPool.LimitAmt
                  MserviceLPool.FromTS = ideFromTS.

               MserviceLPool.LimitAmt = MserviceLPool.LimitAmt + ideAmt.
            END.

            IF ldePoolFromTS EQ ? THEN DO:

               FIND FIRST MserviceLPool NO-LOCK WHERE
                          MserviceLPool.Custnum = iiCustNum AND
                          MserviceLPool.SLSeq = MDSSServiceLimit.SLSeq AND
                          MserviceLPool.EndTs < ideFromTS AND
                          MserviceLPool.FromTS >= MDSSServiceLimit.FromTs
               USE-INDEX Custnum NO-ERROR.
               
               ASSIGN
                  ldePoolAmt = MDSSServiceLimit.InclAmt
                  ldePoolFromTS = (IF AVAIL MserviceLPool
                                   THEN Func.Common:mSecOffSet(MserviceLPool.EndTs,1)
                                   ELSE MDSSServiceLimit.FromTS).
            END.

            FIND FIRST MserviceLPool WHERE
                       MserviceLPool.Custnum = MDSSServiceLimit.Custnum AND
                       MserviceLPool.SLSeq = MDSSServiceLimit.SLSeq AND
                       MserviceLPool.EndTS = Func.Common:mSecOffSet(ideFromTS,-1) 
            NO-LOCK NO-ERROR.
            IF NOT AVAIL MserviceLPool THEN DO:
               CREATE MServiceLPool.
               ASSIGN
                  MServiceLPool.MsSeq   = MDSSServiceLimit.MsSeq
                  MServiceLPool.Custnum = MDSSServiceLimit.Custnum
                  MserviceLPool.SLSeq   = MDSSServiceLimit.SlSeq
                  MserviceLPool.FromTS  = ldePoolFromTS
                  MserviceLPool.EndTS   = Func.Common:mSecOffSet(ideFromTS,-1)
                  MserviceLPool.LimitAmt = ldePoolAmt.

               IF MserviceLPool.EndTS < MserviceLPool.FromTS THEN
                  MserviceLPool.EndTS = MserviceLPool.FromTS.

               IF llDoEvent THEN 
                  fMakeCreateEvent((BUFFER MserviceLPool:HANDLE),
                                   "",
                                   Syst.Var:katun,
                                   "").

               
            END.
      END.

       liCount = liCount + 1.

       IF icType = "ADD" OR icType BEGINS "UPDATE" THEN DO:

          IF icType BEGINS "UPDATE" THEN
             MDSSServiceLimit.InclAmt = MDSSServiceLimit.InclAmt + ideAmt.

          IF icType = "ADD" AND ideConsAmt > 0 AND
             NOT CAN-FIND(FIRST ServiceLCounter WHERE
                    ServiceLCounter.Custnum  = MDSSServiceLimit.CustNum AND
                    ServiceLCounter.SLSeq    = bServiceLimit.SlSeq      AND
                    ServiceLCounter.Period   = liPeriod) THEN DO:
             CREATE bServiceLCounter.
             ASSIGN bServiceLCounter.msseq   = 0
                    bServiceLCounter.Custnum = MDSSServiceLimit.CustNum
                    bServiceLCounter.slseq   = bServiceLimit.SlSeq
                    bServiceLCounter.Period  = liPeriod 
                    bServiceLCounter.MSID    = MDSSServiceLimit.MSID
                    bServiceLCounter.amt     = (ideConsAmt * 1024 * 1024).
          END. /* IF CAN-FIND(FIRST bServiceLCounter WHERE */

       END. /* IF icType = "Add" THEN DO: */
       ELSE IF icType = "REMOVE" THEN DO:
         
         IF MDSSServiceLimit.EndTS >= ideFromTS AND
            MDSSServiceLimit.FromTS <= ideFromTS THEN DO:
            
            FIND FIRST MserviceLPool NO-LOCK WHERE
                       MserviceLPool.Custnum = iiCustNum AND
                       MserviceLPool.SLSeq = MDSSServiceLimit.SLSeq AND
                       MserviceLPool.EndTs < ideFromTS AND
                       MserviceLPool.FromTs >= MDSSServiceLimit.FromTS
            USE-INDEX Custnum NO-ERROR.
                  
            ldePoolFromTS = (IF AVAIL MserviceLPool
                             THEN Func.Common:mSecOffSet(MserviceLPool.EndTs,1)
                             ELSE MDSSServiceLimit.FromTS).
                  
            FIND FIRST MserviceLPool WHERE
                       MserviceLPool.Custnum = MDSSServiceLimit.Custnum AND
                       MserviceLPool.SLSeq = MDSSServiceLimit.SLSeq AND
                       MserviceLPool.EndTS = ideFromTS 
            NO-LOCK NO-ERROR.

            IF NOT AVAIL MserviceLPool THEN DO:
               CREATE MServiceLPool.
               ASSIGN
                  MServiceLPool.MsSeq   = MDSSServiceLimit.MsSeq
                  MServiceLPool.Custnum = MDSSServiceLimit.Custnum
                  MserviceLPool.SLSeq   = MDSSServiceLimit.SlSeq
                  MserviceLPool.FromTS  = ldePoolFromTS
                  MserviceLPool.EndTS   = ideFromTS
                  MserviceLPool.LimitAmt = MDSSServiceLimit.InclAmt.
               
               IF llDoEvent THEN 
                  fMakeCreateEvent((BUFFER MserviceLPool:HANDLE),
                                   "",
                                   Syst.Var:katun,
                                   "").
            END.
          END.

          MDSSServiceLimit.InclAmt = MDSSServiceLimit.InclAmt - ideAmt.

          IF MDSSServiceLimit.InclAmt < 0 THEN
             MDSSServiceLimit.InclAmt = 0.
       END. /* ELSE IF icType = "Remove" THEN DO: */

       /* DSS MServiceLimit could be multiple so we have to return     */
       /* first MServiceLimit Amt because index is in descending order */ 
       IF liCount = 1 THEN
          odeDSSLimit = MDSSServiceLimit.InclAmt.

   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

   RELEASE MDSSServiceLimit.
   RELEASE bServiceLCounter.
   RELEASE MServiceLPool.

END PROCEDURE. /* PROCEDURE pUpdateDSSLimit: */

&ENDIF
