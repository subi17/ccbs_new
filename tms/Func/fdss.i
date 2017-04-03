/* ----------------------------------------------------------------------
  module .......: Func/fdss.i
  task .........: DSS related functions and procedures
  application ..: tms
  author .......: vikas
  created ......: 18.08.11
  version ......: yoigo
---------------------------------------------------------------------- */

&IF "{&fDSS}" NE "YES" 
&THEN

&GLOBAL-DEFINE fDSS YES

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/date.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/fmakemsreq.i}
{Func/fparse.i}
{Func/fixedfee.i}
{Func/matrix.i}
{Syst/eventval.i}
{Func/create_eventlog.i}

&GLOBAL-DEFINE PL_LIMIT_SHAPED 20971520

FUNCTION fDSSRequest RETURNS INTEGER
   (INPUT iiMsSeq        AS INT,    /* subscription */
    INPUT iiCustNum      AS INT,    /* customer */
    INPUT icAction       AS CHAR,   /* Action: Mandatory */
    INPUT icParams       AS CHAR,   /* Parameters: Optional */
    INPUT icBundle       AS CHAR,   /* bundle: Optional */
    INPUT idActStamp     AS DEC,    /* when request should be handled */
    INPUT icSource       AS CHAR,
    INPUT icCreator      AS CHAR,   
    INPUT ilCreateFees   AS LOG,    
    INPUT iiOrigRequest  AS INT,    /* main request */
    INPUT ilMandatory    AS LOG,    /* is subrequest mandatory */
    OUTPUT ocResult      AS CHAR):
 
   DEF VAR liReqCreated     AS INT  NO-UNDO.
   DEF VAR lcCLI            AS CHAR NO-UNDO.
   DEF VAR lcCLIType        AS CHAR NO-UNDO.

   DEF BUFFER bMobSub       FOR MobSub.
   DEF BUFFER bTermMobSub   FOR TermMobSub.

   FIND FIRST bMobSub WHERE
              bMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL bMobSub THEN DO:
      FIND FIRST bTermMobSub WHERE
                 bTermMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAIL bTermMobSub THEN DO:
         ocResult = "MobSub not found".
         RETURN 0.
      END. /* IF NOT AVAIL bTermMobSub THEN DO: */
      ASSIGN lcCLI = bTermMobSub.CLI
             lcCLIType = bTermMobSub.CLIType.
   END. /* IF NOT AVAIL bMobSub THEN DO: */
   ELSE ASSIGN lcCLI = bMobSub.CLI
               lcCLIType = bMobSub.CLIType.

   ocResult = fChkRequest(iiCustNum,
                          {&REQTYPE_DSS},
                          icAction,
                          icCreator).

   IF ocResult > "" THEN RETURN 0.

   /* Pre-check only for CREATE, there is no ongoing ACC request */
   IF icAction = "CREATE" THEN DO:
      RUN Mm/requestaction_check.p(INPUT {&REQTYPE_DSS},
                                INPUT lcCLIType,
                                INPUT iiMsSeq,
                                INPUT icSource,
                                OUTPUT ocResult).
      IF ocResult > "" THEN RETURN 0.
   END. /* IF icAction = "CREATE" THEN DO: */

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = fMakeTS().

   fCreateRequest({&REQTYPE_DSS},
                  idActStamp,
                  icCreator,
                  ilCreateFees, 
                  FALSE).   /* sms */

   ASSIGN
      bCreaReq.MsSeq       = iiMsSeq
      bCreaReq.CLI         = lcCLI
      bCreaReq.CustNum     = iiCustNum /* it can be different in case of ACC */
      bCreaReq.ReqCParam1  = icAction
      bCreaReq.ReqCParam2  = icParams
      bCreaReq.ReqCParam3  = icBundle
      bCreaReq.ReqSource   = icSource
      bCreaReq.OrigRequest = iiOrigRequest
      bCreaReq.Mandatory   = INTEGER(ilMandatory)
      liReqCreated         = bCreaReq.MsRequest.
 
   RELEASE bCreaReq.

   /* Send the SMS using Request Action Rules for DSS */
   RUN Mm/requestaction_sms.p(INPUT liReqCreated,
                           INPUT lcCLIType,
                           INPUT icSource).
  
   RETURN liReqCreated.
     
END FUNCTION.


FUNCTION fOngoingDSSAct RETURNS LOG (INPUT iiCustnum AS INT):
   
   DEF VAR llExist AS LOG NO-UNDO.
   
   llExist = CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                       MsRequest.Brand = gcBrand            AND
                       MsRequest.ReqType = {&REQTYPE_DSS}   AND
                       MsRequest.Custnum = iiCustnum        AND
                       MsRequest.ReqCParam1 = "CREATE"      AND
                       LOOKUP(STRING(MsRequest.ReqStatus),
                              {&REQ_INACTIVE_STATUSES}) = 0).

   RETURN llExist.
   
END FUNCTION. /* FUNCTION fOngoingDSSAct */

FUNCTION fOngoingDSSTerm RETURNS LOG (INPUT iiCustnum   AS INT,
                                      INPUT ideActStamp AS DEC):
   
   DEF VAR llExist AS LOG NO-UNDO.
   
   llExist = CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                       MsRequest.Brand = gcBrand           AND
                       MsRequest.ReqType = {&REQTYPE_DSS}  AND
                       MsRequest.Custnum = iiCustnum       AND
                       MsRequest.ReqCParam1 = "DELETE"     AND
                       MsRequest.ActStamp  <= ideActStamp  AND
                       LOOKUP(STRING(MsRequest.ReqStatus),
                              {&REQ_INACTIVE_STATUSES}) = 0).

   RETURN llExist.
   
END FUNCTION. /* FUNCTION fOngoingDSSTerm */

FUNCTION fIsDSSActive RETURNS LOG (INPUT iiCustNum    AS INT,
                                   INPUT ideActStamp  AS DEC):

   DEF BUFFER ServiceLimit FOR ServiceLimit.

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"}:
       IF CAN-FIND (FIRST MServiceLimit NO-LOCK WHERE
                          MServiceLimit.CustNum = iiCustNum          AND
                          MServiceLimit.DialType = ServiceLimit.DialType AND
                          MServiceLimit.SlSeq   = ServiceLimit.SlSeq AND
                          MServiceLimit.FromTS <= ideActStamp        AND
                          MServiceLimit.EndTS  >= ideActStamp) THEN
       RETURN TRUE.
   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

   RETURN FALSE.

END FUNCTION.

FUNCTION fGetDSSMsSeqLimit RETURNS LOG (INPUT  iiCustNum   AS INT,
                                        INPUT  ideActStamp AS DEC,
                                        OUTPUT oiDSSMsSeq  AS INT,
                                        OUTPUT odeDSSLimit AS DEC,
                                        OUTPUT ocBundleId  AS CHAR):

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"},
      FIRST MServiceLimit NO-LOCK WHERE
            MServiceLimit.CustNum  = iiCustNum             AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
            MServiceLimit.EndTS   >= ideActStamp:

      ASSIGN oiDSSMsSeq  = MServiceLimit.MsSeq
             odeDSSLimit = MServiceLimit.InclAmt
             ocBundleId  = ServiceLimit.GroupCode.

      RETURN TRUE.
   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

   RETURN FALSE.
END FUNCTION.

/* used with DSS/DSS2 compatible data bundle terminations (YTS-6383) */
FUNCTION fGetDSSMsSeqLimitTerm RETURNS LOG (INPUT  iiCustNum   AS INT,
                                        INPUT  ideActStamp AS DEC,
                                        INPUT  ideBundleTermStamp AS DEC,
                                        OUTPUT oiDSSMsSeq  AS INT,
                                        OUTPUT odeDSSLimit AS DEC,
                                        OUTPUT ocBundleId  AS CHAR):

   DEF BUFFER ServiceLimit FOR ServiceLimit.
   DEF BUFFER MServiceLimit FOR MServiceLimit.
   DEF BUFFER bMServiceLimit FOR MServiceLimit.

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"},
       EACH MServiceLimit NO-LOCK WHERE
            MServiceLimit.CustNum  = iiCustNum             AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
            MServiceLimit.EndTS   >= ideActStamp:

      IF MServiceLimit.FromTs <= ideBundleTermStamp OR
         (MServiceLimit.FromTs EQ TRUNC(MServiceLimit.FromTs,0) AND
          CAN-FIND(FIRST bMServiceLimit NO-LOCK WHERE
                         bMServiceLimit.CustNum  = MServiceLimit.Custnum AND
                         bMServiceLimit.DialType = MServiceLimit.DialType AND
                         bMServiceLimit.SlSeq    = MServiceLimit.SlSeq    AND
                         bMServiceLimit.EndTS    = fSecOffSet(MServiceLimit.FromTs,-1) AND
                         bMServiceLimit.FromTs  <= ideBundleTermStamp)) THEN DO:

         ASSIGN oiDSSMsSeq  = MServiceLimit.MsSeq
                odeDSSLimit = MServiceLimit.InclAmt
                ocBundleId  = ServiceLimit.GroupCode.

         RETURN TRUE.
      END.
   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

   RETURN FALSE.
END FUNCTION.

FUNCTION fGetActiveDSSId RETURNS CHAR (INPUT iiCustNum   AS INT,
                                       INPUT ideActStamp AS DEC):

   DEF BUFFER ServiceLimit FOR ServiceLimit.
   DEF BUFFER MServiceLimit FOR MServiceLimit.

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"},
      FIRST MServiceLimit NO-LOCK WHERE
            MServiceLimit.CustNum  = iiCustNum             AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
            MServiceLimit.EndTS   >= ideActStamp:

      RETURN ServiceLimit.GroupCode.
   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

   RETURN "".
END FUNCTION.

FUNCTION fGetDSSId RETURNS CHAR (
   INPUT iiCustNum   AS INT,
   INPUT ideActStamp AS DEC):

   DEF BUFFER ServiceLimit FOR ServiceLimit.
   DEF BUFFER MServiceLimit FOR MServiceLimit.

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"},
      FIRST MServiceLimit NO-LOCK WHERE
            MServiceLimit.CustNum  = iiCustNum             AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
            MServiceLimit.EndTS   >= ideActStamp           AND
            MServiceLimit.FromTs  <= ideActStamp:

      RETURN ServiceLimit.GroupCode.
   END.

   RETURN "".
END FUNCTION.

FUNCTION fGetDSSUsage RETURNS DEC (INPUT iiCustNum    AS INT,
                                   INPUT idActDate    AS DATE,
                                   OUTPUT odeDSSLimit AS DEC):

   DEF VAR liPeriod    AS INT NO-UNDO.
   DEF VAR ldeActStamp AS DEC NO-UNDO.
   DEF VAR ldeAmount   AS DEC NO-UNDO.

   liPeriod = YEAR(idActDate) * 100 + MONTH(idActDate).
   ldeActStamp = fHMS2TS(fLastDayOfMonth(idActDate),"23:59:59").

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"},
      FIRST MServiceLimit NO-LOCK WHERE
            MServiceLimit.CustNum = iiCustNum          AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq   = ServiceLimit.SlSeq AND
            MServiceLimit.FromTS <= ldeActStamp        AND
            MServiceLimit.EndTS  >= ldeActStamp:

      odeDSSLimit = odeDSSLimit + MServiceLimit.InclAmt.
       
      FIND FIRST ServiceLCounter WHERE
                 ServiceLCounter.CustNum = MServiceLimit.CustNum AND
                 ServiceLCounter.SLSeq   = ServiceLimit.SlSeq    AND
                 ServiceLCounter.Period  = liPeriod NO-LOCK NO-ERROR.
      IF AVAIL ServiceLCounter THEN ldeAmount = ServiceLCounter.Amt.
      ELSE ldeAmount = 0.

      RETURN ldeAmount.
   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

   RETURN 0.

END FUNCTION.

FUNCTION fGetTotalDSSUsage RETURNS LOG (INPUT iiCustNum    AS INT,
                                        INPUT idActDate    AS DATE,
                                        OUTPUT ocBundleId  AS CHAR,
                                        OUTPUT odeDSSLimit AS DEC,
                                        OUTPUT odeDSSUsage AS DEC):

   DEF VAR liPeriod    AS INT  NO-UNDO.
   DEF VAR ldeActStamp AS DEC  NO-UNDO.
   DEF VAR ldaFromDate AS DATE NO-UNDO.
   DEF VAR ldeFromTS   AS DEC  NO-UNDO.

   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.

   ASSIGN liPeriod = YEAR(idActDate) * 100 + MONTH(idActDate)
          ldeActStamp = fHMS2TS(fLastDayOfMonth(idActDate),"23:59:59")
          ldaFromDate = DATE(MONTH(idActDate),1,YEAR(idActDate))
          ldeFromTS   = fHMS2TS(ldaFromDate,"00:00:00").

   FOR EACH ServiceLimit NO-LOCK WHERE
            {Func/dss_search.i "ServiceLimit.GroupCode"},
      FIRST MServiceLimit NO-LOCK WHERE
            MServiceLimit.CustNum  = iiCustNum          AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
            MServiceLimit.FromTS  <= ldeActStamp        AND
            MServiceLimit.EndTS   >= ldeActStamp:

      ASSIGN odeDSSLimit = MServiceLimit.InclAmt
             ocBundleId  = ServiceLimit.GroupCode.
       
      FIND FIRST ServiceLCounter WHERE
                 ServiceLCounter.CustNum = MServiceLimit.CustNum AND
                 ServiceLCounter.SLSeq   = ServiceLimit.SlSeq    AND
                 ServiceLCounter.Period  = liPeriod NO-LOCK NO-ERROR.
      IF AVAIL ServiceLCounter THEN
         odeDSSUsage = ServiceLCounter.Amt.

      FOR EACH bServiceLimit NO-LOCK WHERE
                bServiceLimit.GroupCode = "DSS200_UPSELL" OR
                bServiceLimit.GroupCode = ServiceLimit.Groupcode + "_UPSELL",
          FIRST bMServiceLimit NO-LOCK WHERE
                bMServiceLimit.CustNum  = iiCustNum          AND
                bMServiceLimit.DialType = bServiceLimit.DialType AND
                bMServiceLimit.SlSeq    = bServiceLimit.SlSeq AND
                bMServiceLimit.FromTS  <= ldeActStamp        AND
                bMServiceLimit.EndTS   >= ldeActStamp:

         FIND FIRST MServiceLPool WHERE
                    MserviceLPool.MsSeq   = bMServiceLimit.MsSeq   AND
                    MserviceLPool.SLSeq   = bMServiceLimit.SLSeq   AND
                    MserviceLPool.FromTS <= ldeActStamp            AND
                    MserviceLPool.EndTS  >= ldeFromTS NO-LOCK NO-ERROR.
         IF AVAILABLE MserviceLPool THEN
            odeDSSLimit = odeDSSLimit + MserviceLPool.LimitAmt.

         FIND FIRST bServiceLCounter WHERE
                    bServiceLCounter.CustNum = bMServiceLimit.CustNum AND
                    bServiceLCounter.SLSeq   = bServiceLimit.SlSeq    AND
                    bServiceLCounter.Period  = liPeriod NO-LOCK NO-ERROR.
         IF AVAIL bServiceLCounter THEN
            odeDSSUsage = odeDSSUsage + bServiceLCounter.Amt.
      END.

      odeDSSUsage = odeDSSUsage / 1024 / 1024.

      RETURN TRUE.
   END. /* FOR EACH ServiceLimit NO-LOCK WHERE */

   RETURN FALSE.

END FUNCTION.


FUNCTION fGetOtherBundleUsages RETURNS DEC (INPUT iiCustNum   AS INT,
                                            INPUT iiPeriod    AS INT):

   DEF VAR ldeConsumedData        AS DEC  NO-UNDO.
   DEF VAR ldFromDate             AS DATE NO-UNDO.
   DEF VAR ldToDate               AS DATE NO-UNDO.
   DEF VAR ldeFromTS              AS DEC  NO-UNDO.
   DEF VAR ldeEndTS               AS DEC  NO-UNDO.
   DEF VAR ldeServiceLCounterAmt  AS DEC  NO-UNDO.
   DEF VAR lcExcludeBundles       AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType  AS CHAR NO-UNDO.
   DEF VAR lcBundleId             AS CHAR NO-UNDO.
  
   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.
   DEF BUFFER bMsOwner         FOR MsOwner.
   DEF BUFFER bDayCampaign     FOR DayCampaign.

   ASSIGN ldFromDate = fInt2Date(iiPeriod,1)
          ldToDate   = fInt2Date(iiPeriod,2)
          ldeFromTS  = fMake2Dt(ldFromDate,0)
          ldeEndTS   = fMake2Dt(ldToDate,86399)
          lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES").

   lcBundleId = fGetActiveDSSId(iiCustNum,ldeEndTS).
   IF lcBundleId = "DSS2" THEN
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

   FOR EACH bMsOwner NO-LOCK WHERE
            bMsOwner.InvCust  = iiCustnum  AND
            bMsOwner.TsBegin <= ldeEndTS   AND
            bMsOwner.TsEnd   >= ldeFromTS  AND
            NOT bMsOwner.PayType
      BREAK BY bMsOwner.MsSeq:
      IF FIRST-OF(bMsOwner.MsSeq) THEN DO:

         IF lcBundleId = "DSS2" AND
            LOOKUP(bMsOwner.CLIType,lcAllowedDSS2SubsType) = 0 THEN NEXT.

         FOR EACH bMServiceLimit WHERE
                  bMServiceLimit.MsSeq   = bMsOwner.MsSeq AND
                  bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
                  bMServiceLimit.FromTS <= ldeEndTS      AND
                  bMServiceLimit.EndTS  >= ldeEndTS  NO-LOCK,
            FIRST bServiceLimit NO-LOCK WHERE
                  bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
            FIRST bDayCampaign WHERE
                  bDayCampaign.Brand = gcBrand AND
                  bDayCampaign.DCEvent = bServiceLimit.GroupCode NO-LOCK:

            IF LOOKUP(STRING(bDayCampaign.DCType),
                      {&PERCONTRACT_RATING_PACKAGE}) = 0 AND
               bDayCampaign.DCType NE {&DCTYPE_POOL_RATING} THEN NEXT.

            IF bDayCampaign.DCEvent BEGINS {&DSS} OR
               LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 THEN NEXT.

            FIND FIRST bServiceLCounter WHERE
                       bServiceLCounter.MSSeq  = bMsOwner.MsSeq      AND
                       bServiceLCounter.SLSeq  = bServiceLimit.SlSeq AND
                       bServiceLCounter.Period = iiPeriod NO-LOCK NO-ERROR.
            IF AVAILABLE bServiceLCounter THEN DO:
               ldeServiceLCounterAmt = (bServiceLCounter.Amt / 1024 / 1024).
               IF bMServiceLimit.InclAmt < ldeServiceLCounterAmt THEN
                  ldeConsumedData = ldeConsumedData + bMServiceLimit.InclAmt.
               ELSE
                  ldeConsumedData = ldeConsumedData + ldeServiceLCounterAmt.
            END. /* IF AVAILABLE bServiceLCounter THEN DO: */
         END. /* FOR EACH bMServiceLimit WHERE */
      END. /* IF FIRST-OF(bMsOwner.MsSeq) THEN DO: */
   END. /* FOR EACH bMsOwner NO-LOCK WHERE */

   RETURN ldeConsumedData.

END FUNCTION.

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
   DEF VAR lcDSS2PrimarySubsType  AS CHAR  NO-UNDO.
   DEF VAR llDSS2PrimaryAvail     AS LOG   NO-UNDO.
   
   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bMserviceLPool   FOR MserviceLPool.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.
   DEF BUFFER bMobSub          FOR MobSub.
   DEF BUFFER bDayCampaign     FOR DayCampaign.

   ASSIGN liPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
          lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES").

   IF fIsDSSActive(INPUT iiCustnum,INPUT ideActStamp) OR
      (ilNewSubscription = TRUE AND
       fOngoingDSSAct(INPUT iiCustnum)) THEN DO:
      ocResult = "dss_already_active".
      RETURN FALSE.
   END. /* IF fIsDSSActive(INPUT iiCustnum) THEN DO: */

   IF icBundleId = "DSS2" THEN
      ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
             lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE").

   FOR EACH bMobSub WHERE
            bMobSub.Brand   = gcBrand   AND
            bMobSub.InvCust = iiCustnum AND
            NOT bMobSub.PayType NO-LOCK:

      IF icBundleId = "DSS2" AND
         LOOKUP(bMobSub.CLIType,lcAllowedDSS2SubsType) = 0 THEN NEXT.

      ASSIGN liMobSubCount = liMobSubCount + 1
             lcALLSubsList = lcALLSubsList + ";34" + bMobSub.CLI.

      FOR EACH bMServiceLimit WHERE
               bMServiceLimit.MsSeq   = bMobSub.MsSeq AND
               bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
               bMServiceLimit.FromTS <= ideActStamp   AND
               bMServiceLimit.EndTS  >= ideActStamp NO-LOCK,
         FIRST bServiceLimit NO-LOCK WHERE
               bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
         FIRST bDayCampaign WHERE
               bDayCampaign.Brand = gcBrand AND
               bDayCampaign.DCEvent = bServiceLimit.GroupCode NO-LOCK:

         /* might happen with ACC if the old customer has still active DSS */
         IF bDayCampaign.DCEvent BEGINS "DSS" THEN NEXT.

         IF LOOKUP(STRING(bDayCampaign.DCType),
                   {&PERCONTRACT_RATING_PACKAGE}) = 0 AND
            bDayCampaign.DCType NE {&DCTYPE_POOL_RATING} THEN NEXT.

         IF LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 THEN NEXT.

         /* pending termination request */
         IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                     MsRequest.MsSeq = bMServiceLimit.MsSeq AND
                     MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                     MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                     LOOKUP(STRING(MsRequest.ReqStatus),
                                   {&REQ_INACTIVE_STATUSES}) = 0 AND
                     MsRequest.ActStamp <= ideActStamp) THEN NEXT.

         IF icBundleId = "DSS2" AND
            LOOKUP(bDayCampaign.DCEvent,lcDSS2PrimarySubsType) > 0 THEN
            llDSS2PrimaryAvail = TRUE.

         ldeBundleLimit = 0.
               
         /* All bundle + UPSELL limits for first month DSS */
         IF bDayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO:
            FIND FIRST bMServiceLPool WHERE
                       bMserviceLPool.MsSeq   = bMServiceLimit.MsSeq  AND
                       bMserviceLPool.SLSeq   = bMServiceLimit.SLSeq  AND
                       bMserviceLPool.EndTS  >= ideActStamp           AND
                       bMserviceLPool.FromTS <= ideActStamp NO-LOCK NO-ERROR.
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

   IF icBundleId = "DSS2" AND NOT llDSS2PrimaryAvail THEN DO:
      ocResult = "dss2_no_primary_subscription".
      RETURN FALSE.
   END.

   IF odeOtherMonthLimit <= 0 THEN DO:
      ocResult = "dss_no_data_bundle".
      RETURN FALSE.
   END. /* IF odeOtherMonthLimit <= 0 THEN DO: */

   IF (ilNewSubscription EQ TRUE AND liMobSubCount < 1) OR
      (ilNewSubscription EQ FALSE AND liMobSubCount < 2) THEN DO:
      ocResult = "dss_no_postpaid_subscription".
      RETURN FALSE.
   END. /* IF liMobSubCount < 2 THEN DO: */

   ASSIGN liTotalDSSLimit = (odeOtherMonthLimit * 1024 * 1024).

   /* If it blanks then return only create param list otherwise specific */
   IF icReturnParamType = "HSDPA_MSISDN" THEN
      ocResult = "MSISDNS=" + lcHSDPASubsList.
   ELSE
      ocResult = "DSS-ACCOUNT="    + STRING(iiCustnum)       + "," +
                 "TEMPLATE=DSS_MONTHLY"                      + "," +
                 "TARIFF_TYPE=DSS"                           + "," +
                 "TARIFF="         + icBundleId              + "," +
                 "LIMIT_UNSHAPED=" + STRING(liTotalDSSLimit) + "," +
                 "LIMIT_SHAPED="   + STRING({&PL_LIMIT_SHAPED}) + "," +
                 "MSISDNS="        + lcALLSubsList.
  
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

   DEF VAR liSubCount AS INT NO-UNDO.

   DEFINE BUFFER lbMobSub FOR MobSub.

   IF iiMsSeq > 0 THEN DO:
      FIND FIRST MobSub WHERE
                 MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MobSub THEN DO:
         ocResult = "MobSub not found".
         RETURN FALSE.
      END. /* IF NOT AVAILABLE MobSub THEN DO: */

      IF fMatrixAnalyse(gcBrand,
                        "PERCONTR",
                        "PerContract;SubsTypeTo",
                        icBundleId + ";" + MobSub.CLIType,
                        OUTPUT ocResult) NE 1 THEN DO:
         ocResult = "ERROR:Contract is not allowed for this subscription type".
         RETURN FALSE.
      END. /* IF fMatrixAnalyse(gcBrand */
   END.

   FIND FIRST Customer NO-LOCK WHERE
              Customer.CustNum = iiCustNum NO-ERROR.
   IF AVAILABLE Customer THEN DO:

      FOR EACH lbMobSub NO-LOCK WHERE
               lbMobSub.Brand     = gcBrand          AND
               lbMobSub.CustNum   = Customer.CustNum AND
               lbMobSub.PayType   = FALSE            AND
               lbMobSub.SalesMan NE "GIFT":
         liSubCount = liSubCount + 1.
      END.

      IF NOT fCheckExistConvergentOR2P(Customer.CustIDType, Customer.OrgID) AND
         liSubCount < 2 THEN
         RETURN FALSE.
         
   END.

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

FUNCTION fIsDSS2Allowed RETURNS LOG
   (INPUT  iiCustnum      AS INT,
    INPUT  iiMsSeq        AS INT,
    INPUT  ideActStamp    AS DEC,
    OUTPUT oiDSS2PriMsSeq AS INT, 
    OUTPUT ocResult       AS CHAR):

   DEF VAR liMobSubCount         AS INT   NO-UNDO.
   DEF VAR lcExcludeBundles      AS CHAR  NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType AS CHAR  NO-UNDO.
   DEF VAR lcDSS2PrimarySubsType AS CHAR  NO-UNDO.
   DEF VAR llDSS2PrimaryAvail    AS LOG   NO-UNDO.
   
   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.
   DEF BUFFER bMobSub          FOR MobSub.
   DEF BUFFER bDayCampaign     FOR DayCampaign.

   ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
          lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE")
          lcExcludeBundles      = fCParamC("EXCLUDE_BUNDLES").

   IF iiMsSeq > 0 THEN DO:
      FIND FIRST bMobSub WHERE
                 bMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bMobSub THEN DO:
         ocResult = "MobSub not found".
         RETURN FALSE.
      END. /* IF NOT AVAILABLE bMobSub THEN DO: */

      IF fMatrixAnalyse(gcBrand,
                        "PERCONTR",
                        "PerContract;SubsTypeTo",
                        "DSS2" + ";" + bMobSub.CLIType,
                        OUTPUT ocResult) NE 1 THEN DO:
         ocResult = "ERROR:Contract is not allowed for this subscription type".
         RETURN FALSE.
      END. /* IF fMatrixAnalyse(gcBrand */
   END. /* IF iiMsSeq > 0 THEN DO: */

   MOBSUB_LOOP:
   FOR EACH bMobSub WHERE
            bMobSub.Brand   = gcBrand   AND
            bMobSub.InvCust = iiCustnum AND
            NOT bMobSub.PayType NO-LOCK:

      /* Exclude current operated subs and not allowed list */
      IF bMobSub.MsSeq = iiMsSeq OR
         LOOKUP(bMobSub.CLIType,lcAllowedDSS2SubsType) = 0 THEN NEXT.

      /* Exclude subs. if termination request is ongoing */
      IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                   MsRequest.MsSeq   = bMobSub.MsSeq AND
                   MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                   MsRequest.ActStamp <= ideActStamp AND
                   LOOKUP(STRING(MsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES} + ",3") = 0) THEN NEXT.

      /* Exclude subs. if STC request is ongoing */
      IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                   MsRequest.MsSeq   = bMobSub.MsSeq AND
                   MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                   MsRequest.ActStamp <= ideActStamp AND
                   LOOKUP(STRING(MsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES} + ",3") = 0 AND
                   LOOKUP(MsRequest.ReqCParam2,lcAllowedDSS2SubsType) = 0) THEN NEXT.
      
      /* Exclude subs. if ACC request is ongoing */
      IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                   MsRequest.MsSeq   = bMobSub.MsSeq AND
                   MsRequest.ReqType = {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE} AND
                   MsRequest.ActStamp <= ideActStamp AND
                   LOOKUP(STRING(MsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES} + ",3") = 0) THEN NEXT.

      liMobSubCount = liMobSubCount + 1.

      FOR EACH bMServiceLimit WHERE
               bMServiceLimit.MsSeq    = bMobSub.MsSeq AND
               bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
               bMServiceLimit.FromTS  <= ideActStamp   AND
               bMServiceLimit.EndTS   >= ideActStamp NO-LOCK,
         FIRST bServiceLimit NO-LOCK WHERE
               bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
         FIRST bDayCampaign NO-LOCK WHERE
               bDayCampaign.Brand = gcBrand AND
               bDayCampaign.DCEvent = bServiceLimit.GroupCode AND
               LOOKUP(bDayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0:

         /* Should not count DSS/UPSELL */
         IF bDayCampaign.DCEvent BEGINS {&DSS} OR
            LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 THEN NEXT.

         /* pending termination request */
         IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                     MsRequest.MsSeq = bMServiceLimit.MsSeq AND
                     MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                     MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                     LOOKUP(STRING(MsRequest.ReqStatus),
                                   {&REQ_INACTIVE_STATUSES}) = 0 AND
                     MsRequest.ActStamp <= ideActStamp) THEN NEXT.

         IF NOT llDSS2PrimaryAvail AND
            LOOKUP(bDayCampaign.DCEvent,lcDSS2PrimarySubsType) > 0 THEN DO:
            ASSIGN oiDSS2PriMsSeq     = bMobSub.MsSeq
                   llDSS2PrimaryAvail = TRUE.
            LEAVE.
         END.
      END. /* FOR EACH bMServiceLimit WHERE */

      IF llDSS2PrimaryAvail AND
         ((iiMsSeq > 0 AND liMobSubCount >= 1) OR
          (iiMsSeq = 0 AND liMobSubCount >= 2)) THEN LEAVE MOBSUB_LOOP.

   END. /* FOR EACH bMobSub NO-LOCK WHERE */

   IF NOT llDSS2PrimaryAvail THEN DO:
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
   DEF VAR llDSS2PrimaryAvail    AS LOG   NO-UNDO.
   
   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.
   DEF BUFFER bMobSub          FOR MobSub.
   DEF BUFFER bDayCampaign     FOR DayCampaign.

   lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES").

   IF icBundleId = "DSS2" THEN
      ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
             lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE").

   FOR EACH bMobSub WHERE
            bMobSub.Brand   = gcBrand   AND
            bMobSub.InvCust = iiCustnum AND
            NOT bMobSub.PayType NO-LOCK:

      /* Exclude current operated subs. to check further DSS valid */
      IF bMobSub.MsSeq = iiMsSeq OR
         (icBundleId = "DSS2" AND
          LOOKUP(bMobSub.CLIType,lcAllowedDSS2SubsType) = 0) THEN NEXT.

      /* Exclude subs. if termination request is ongoing */
      IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                   MsRequest.MsSeq   = bMobSub.MsSeq AND
                   MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                   MsRequest.ActStamp <= ideActStamp AND
                   LOOKUP(STRING(MsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES} + ",3") = 0) THEN NEXT.

      /* Exclude subs. if STC request is ongoing */
      IF icBundleId = "DSS2" AND
         CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                   MsRequest.MsSeq   = bMobSub.MsSeq AND
                   MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                   MsRequest.ActStamp <= ideActStamp AND
                   LOOKUP(STRING(MsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES} + ",3") = 0 AND
                   LOOKUP(MsRequest.ReqCParam2,lcAllowedDSS2SubsType) = 0)
      THEN NEXT.

      IF icBundleId = "DSS" AND
         CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                   MsRequest.MsSeq   = bMobSub.MsSeq AND
                   MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                   MsRequest.ActStamp <= ideActStamp AND
                   LOOKUP(STRING(MsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES} + ",3") = 0 AND
                   MsRequest.ReqCParam2 BEGINS "TARJ") THEN NEXT.

      /* Exclude subs. if ACC request is ongoing */
      IF CAN-FIND (FIRST MsRequest NO-LOCK WHERE
                   MsRequest.MsSeq   = bMobSub.MsSeq AND
                   MsRequest.ReqType = {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE} AND
                   MsRequest.ActStamp <= ideActStamp AND
                   LOOKUP(STRING(MsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES} + ",3") = 0) THEN NEXT.

      liMobSubCount = liMobSubCount + 1.

      FOR EACH bMServiceLimit WHERE
               bMServiceLimit.MsSeq   = bMobSub.MsSeq AND
               bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
               bMServiceLimit.FromTS <= ideActStamp   AND
               bMServiceLimit.EndTS  >= ideActStamp NO-LOCK,
         FIRST bServiceLimit NO-LOCK WHERE
               bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
         FIRST bDayCampaign NO-LOCK WHERE
               bDayCampaign.Brand = gcBrand AND
               bDayCampaign.DCEvent = bServiceLimit.GroupCode AND
               LOOKUP(bDayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0:

         /* Should not count DSS/UPSELL */
         IF bDayCampaign.DCEvent BEGINS {&DSS} OR
            LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 THEN NEXT.

         /* pending termination request */
         IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                     MsRequest.MsSeq = bMServiceLimit.MsSeq AND
                     MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                     MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                     LOOKUP(STRING(MsRequest.ReqStatus),
                                   {&REQ_INACTIVE_STATUSES}) = 0 AND
                     MsRequest.ActStamp <= ideActStamp) THEN NEXT.

         IF icBundleId = "DSS2" AND
            LOOKUP(bDayCampaign.DCEvent,lcDSS2PrimarySubsType) > 0 THEN
            llDSS2PrimaryAvail = TRUE.
               
         llDataBundleActive = TRUE.

      END. /* FOR EACH bMServiceLimit WHERE */
   END. /* FOR EACH bMobSub NO-LOCK WHERE */

   IF icBundleId = "DSS2" AND NOT llDSS2PrimaryAvail THEN DO:
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

   DEF VAR liMobSubCount         AS INT  NO-UNDO.
   DEF VAR llFirstActBundle      AS LOG  NO-UNDO INIT TRUE.
   DEF VAR llActOtherBundle      AS LOG  NO-UNDO.
   DEF VAR lcExcludeBundles      AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.
   DEF VAR lcDSS2PrimarySubsType AS CHAR NO-UNDO.
   
   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bMobSub          FOR MobSub.
   DEF BUFFER bDayCampaign     FOR DayCampaign.

   lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES").

   IF icBundleId = "DSS2" THEN
      ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
             lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE").

   FOR EACH bMobSub WHERE
            bMobSub.Brand   = gcBrand      AND
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
               bDayCampaign.Brand = gcBrand AND
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

   ASSIGN ldMonthEndDate      = fLastDayOfMonth(idActionDate)
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
                 FixedFee.Brand     = gcBrand   AND
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
               bMServiceLimit.EndTS  >= fMakeTS() EXCLUSIVE-LOCK:
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
            DayCampaign.Brand   = gcBrand AND
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
   
   FIND FIRST ProvSolog WHERE
              ProvSolog.Solog = iiSolog NO-LOCK NO-ERROR.

   FIND FIRST ProvMsRequest WHERE 
              ProvMsRequest.Msrequest = iiMsRequest NO-LOCK NO-ERROR.

   /* Get latest DSS Limit to send to Network */
   IF ProvMsRequest.ReqCparam1 = "MODIFY" AND
      (ProvMsRequest.ReqCparam2 = "" OR
       INDEX(ProvMsRequest.ReqCparam2,"LIMIT") > 0) THEN
      fGetDSSMsSeqLimit(INPUT ProvMsRequest.CustNum,
                        INPUT (IF ProvMsRequest.ActStamp > fMakeTS() THEN
                               ProvMsRequest.ActStamp ELSE fMakeTS()),
                        OUTPUT liDSSMsSeq,
                        OUTPUT ldeCurrentDSSLimit,
                        OUTPUT lcDSSBundleId).

   IF ldeCurrentDSSLimit > 0 THEN DO TRANSACTION:
      ASSIGN liDSSLimit      = (ldeCurrentDSSLimit * 1024 * 1024).
         
      FIND CURRENT ProvMsRequest EXCLUSIVE-LOCK.
      IF AVAILABLE ProvMsRequest THEN
         ProvMSRequest.ReqCparam2 =
         "DSS-ACCOUNT=" + STRING(ProvMSRequest.CustNum)  + "," +
         "TEMPLATE=DSS_MONTHLY"                          + "," +
         "TARIFF_TYPE=DSS"                               + "," +
         "TARIFF="          + ProvMsRequest.ReqCparam3   + "," +
         "LIMIT_UNSHAPED="  + STRING(liDSSLimit)         + "," +
         "LIMIT_SHAPED="    + STRING({&PL_LIMIT_SHAPED}).
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
            
         fSplitTS(ideFromTS, output ldaDate, output litime).

         ASSIGN
            ldePoolFromTS = ?
            ldeFirstSecondOfMonth = fHMS2TS(DATE(MONTH(ldaDate),1,
                                                 YEAR(ldaDate)), "00:00:00").
            ldeLastSecondOfMonth = fHMS2TS(fLastDayOfMonth(ldaDate),"23:59:59").
         
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
                                   THEN fSecOffSet(MserviceLPool.EndTs,1)
                                   ELSE MDSSServiceLimit.FromTS).
            END.

            FIND FIRST MserviceLPool WHERE
                       MserviceLPool.Custnum = MDSSServiceLimit.Custnum AND
                       MserviceLPool.SLSeq = MDSSServiceLimit.SLSeq AND
                       MserviceLPool.EndTS = fSecOffSet(ideFromTS,-1) 
            NO-LOCK NO-ERROR.
            IF NOT AVAIL MserviceLPool THEN DO:
               CREATE MServiceLPool.
               ASSIGN
                  MServiceLPool.MsSeq   = MDSSServiceLimit.MsSeq
                  MServiceLPool.Custnum = MDSSServiceLimit.Custnum
                  MserviceLPool.SLSeq   = MDSSServiceLimit.SlSeq
                  MserviceLPool.FromTS  = ldePoolFromTS
                  MserviceLPool.EndTS   = fSecOffSet(ideFromTS,-1)
                  MserviceLPool.LimitAmt = ldePoolAmt.

               IF MserviceLPool.EndTS < MserviceLPool.FromTS THEN
                  MserviceLPool.EndTS = MserviceLPool.FromTS.

               IF llDoEvent THEN 
                  fMakeCreateEvent((BUFFER MserviceLPool:HANDLE),
                                   "",
                                   katun,
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
                             THEN fSecOffSet(MserviceLPool.EndTs,1)
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
                                   katun,
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


PROCEDURE pUpdateDSSNetworkLimit:
   DEF INPUT PARAMETER iiMsSeq          AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiCustNum        AS INT  NO-UNDO.
   DEF INPUT PARAMETER ideLimit         AS DEC  NO-UNDO.
   DEF INPUT PARAMETER icActionType     AS CHAR NO-UNDO.
   DEF INPUT PARAMETER ilCheckDSSLimit  AS LOG  NO-UNDO.
   DEF INPUT PARAMETER iiMainRequest    AS INT  NO-UNDO.
   DEF INPUT PARAMETER ideActStamp      AS DEC  NO-UNDO.
   DEF INPUT PARAMETER icSource         AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icBundleId       AS CHAR NO-UNDO.
   
   DEF VAR liCreated       AS INT   NO-UNDO.
   DEF VAR lcError         AS CHAR  NO-UNDO.
   DEF VAR lcParam         AS CHAR  NO-UNDO.
   DEF VAR liLimit         AS INT64 NO-UNDO.

   DEF BUFFER bMsRequest   FOR MsRequest.

   ASSIGN liLimit      = ideLimit * 1024 * 1024.

   /* Compare latest DSS limit with limit specified in the "CREATE" request */
   IF ilCheckDSSLimit THEN DO:
      FIND LAST bMsRequest WHERE
                bMsRequest.MsSeq      = iiMsSeq        AND
                bMsRequest.ReqType    = {&REQTYPE_DSS} AND
                bMsRequest.ReqCparam1 = "CREATE"       AND
                bMsRequest.ActStamp  <= ideActStamp NO-LOCK NO-ERROR.
      IF AVAILABLE bMsRequest AND
         STRING(liLimit) = fParseKVP("LIMIT_UNSHAPED",bMsRequest.ReqCparam2,",")
      THEN RETURN.
   END. /* IF ilCheckDSSLimit THEN DO: */

   IF icActionType = "QUOTA" THEN
      lcParam = "DSS-ACCOUNT" + "=" + STRING(iiCustNum) + "," +
                "TEMPLATE=DSS"                          + "," +
                icActionType  + "=" + STRING(liLimit)   + "," +
                "GRACE"       + "=" + STRING(0).
   ELSE
      lcParam = "DSS-ACCOUNT="    + STRING(iiCustNum)       + "," +
                "TEMPLATE=DSS_MONTHLY"                      + "," +
                "TARIFF_TYPE=DSS"                           + "," +
                "TARIFF="         + icBundleId              + "," +
                "LIMIT_UNSHAPED=" + STRING(liLimit)         + "," +
                "LIMIT_SHAPED="   + STRING({&PL_LIMIT_SHAPED}).

   liCreated = fDSSRequest(iiMsSeq,
                           iiCustNum,
                           "MODIFY",
                           lcParam,
                           icBundleId,
                           ideActStamp,
                           icSource,
                           "",
                           FALSE,
                           iiMainRequest,
                           FALSE,
                           OUTPUT lcError).

   IF liCreated = 0 THEN
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                 "Customer",
                 STRING(iiCustNum),
                 iiCustNum,
                 "Update DSS Network Limit/Quota",
                 "Failed Update DSS Network Limit/Quota. " + lcError).

END PROCEDURE.

PROCEDURE pUpdateDSSNetwork:
   DEF INPUT PARAMETER iiMsSeq          AS INT  NO-UNDO.
   DEF INPUT PARAMETER icCLI            AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiCustNum        AS INT  NO-UNDO.
   DEF INPUT PARAMETER icAction         AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icParamList      AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiMainRequest    AS INT  NO-UNDO.
   DEF INPUT PARAMETER ideActStamp      AS DEC  NO-UNDO.
   DEF INPUT PARAMETER icSource         AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icBundleId       AS CHAR NO-UNDO.
   
   DEF VAR liCreated                    AS INT   NO-UNDO.
   DEF VAR lcError                      AS CHAR  NO-UNDO.
   DEF VAR lcParam                      AS CHAR  NO-UNDO.

   CASE icAction:
      /* Delete DSS Account */
      WHEN "DELETE" THEN .
      /* Update HSDPA MSISDN LIST */
      WHEN "MODIFY" THEN
         lcParam = "TEMPLATE=DSS," + icParamList.
      /* Add VoIP provision */
      WHEN "VOIPVIDEO_ADD" THEN
         ASSIGN icAction = "MODIFY"
                lcParam  = "VOIPVIDEO=1".
      OTHERWISE
         lcParam = "MSISDN=34" + icCLI.
   END.
   
   lcParam = "DSS-ACCOUNT=" + STRING(iiCustNum) + "," + lcParam.

   liCreated = fDSSRequest(iiMsSeq,
                           iiCustNum,
                           icAction, 
                           lcParam,
                           icBundleId,
                           ideActStamp,
                           icSource,
                           "",
                           FALSE,
                           iiMainRequest,
                           FALSE,
                           OUTPUT lcError).

   IF liCreated = 0 THEN
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "Customer",
                       STRING(iiCustNum),
                       iiCustNum,
                       "Unable to create " + icAction + " request",
                       "Unable to create " + icAction + " request. " +
                       lcError).

END PROCEDURE.

PROCEDURE pUpdateDSSConsumption:

   DEF INPUT PARAMETER iiMainRequest AS INT  NO-UNDO.
   DEF INPUT PARAMETER icSource      AS CHAR NO-UNDO.

   DEF VAR ldeDSSLimit               AS DEC  NO-UNDO.
   DEF VAR ldeDSSUsage               AS DEC  NO-UNDO.
   DEF VAR lcBundleId                AS CHAR NO-UNDO.
   DEF VAR liRequest                 AS INT  NO-UNDO.
   DEF VAR lcError                   AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType     AS CHAR NO-UNDO.
   DEF VAR ldaActDate                AS DATE NO-UNDO.
   DEF VAR liActTime                 AS INT  NO-UNDO.

   DEF BUFFER bMsRequest FOR MsRequest.

   FIND FIRST bMsRequest WHERE
              bMsRequest.MsRequest = iiMainRequest NO-LOCK NO-ERROR.
   IF NOT AVAIL bMsRequest THEN RETURN.

   fSplitTS(bMsRequest.ActStamp,OUTPUT ldaActDate,OUTPUT liActTime).

   /* Check whether DSS bundle is active or not for this customer */
   IF NOT fGetTotalDSSUsage(INPUT bMsRequest.CustNum,INPUT ldaActDate,
                            OUTPUT lcBundleId,OUTPUT ldeDSSLimit,
                            OUTPUT ldeDSSUsage) THEN RETURN.

   IF lcBundleId = "DSS2" THEN
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

   /* Adjust DSS counter if counter is more than usage */
   IF ldeDSSUsage > ldeDSSLimit THEN DO:
   
      /* YTS-6595 */
      IF fOngoingDSSTerm (bMsRequest.Custnum,
                          bMsRequest.ActStamp) THEN RETURN.

      RUN pUpdateDSSNetworkLimit(INPUT bMsRequest.MsSeq,
                                 INPUT bMsRequest.CustNum,
                                 INPUT (ldeDSSUsage - ldeDSSLimit),
                                 INPUT "QUOTA",
                                 INPUT FALSE,
                                 INPUT bMsRequest.MsRequest,
                                 INPUT bMsRequest.ActStamp,
                                 INPUT icSource,
                                 INPUT lcBundleId).
   END. /* IF ldeDSSUsage > ldeDSSLimit THEN DO: */

   /* Reset subscription based consumption counter if new tariff is not
      compatible with DSS2 */
   IF lcBundleId = "DSS2" AND
      LOOKUP(bMsRequest.ReqCparam1,lcAllowedDSS2SubsType) > 0 AND
      LOOKUP(bMsRequest.ReqCparam2,lcAllowedDSS2SubsType) = 0 THEN DO:
      liRequest = fServiceRequest(
                        bMsRequest.MsSeq,
                        "SHAPER",
                        1, /* on */
                        "0,GRACE=0,TEMPLATE=HSPA_SET",
                        bMsRequest.ActStamp,
                        "", /* salesman */
                        FALSE, /* fees */
                        FALSE, /* sms */
                        "", /* creator */
                        icSource,
                        bMsRequest.MsRequest, /* father request */
                        FALSE, /* mandatory for father request */
                        OUTPUT lcError).
      IF liRequest = 0 THEN
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "MobSub",
                          STRING(bMsRequest.MsSeq),
                          bMsRequest.Custnum,
                          "Contract consumption adjustment failed;",
                          lcError).
   END. /* IF lcBundleId = "DSS2" AND */

END PROCEDURE.

&ENDIF
