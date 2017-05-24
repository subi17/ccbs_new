/* fmakeservlimit.i     2004/jp

   changes:             31.12.04/aam input idActStamp 

*/

&IF "{&fmakeservlimit}" NE "YES" 
&THEN

&GLOBAL-DEFINE fmakeservlimit YES

{Syst/commali.i}
{Func/date.i}
{Func/timestamp.i}
{Syst/eventval.i}
{Func/fdss.i}

/* somehow have to prevent calling fCleanEventObjects */
DEF VAR llCleanServLimitEventLog AS LOGICAL NO-UNDO INIT TRUE.

/* DSS related variables */
DEF VAR ldeCurrMonthLimit   AS DEC  NO-UNDO.
DEF VAR ldeOtherMonthLimit  AS DEC  NO-UNDO.
DEF VAR ldeConsumedData     AS DEC  NO-UNDO.
DEF VAR ldeDSSTotalLimit    AS DEC  NO-UNDO.
DEF VAR ldeDSSUpsellLimit   AS DEC  NO-UNDO.
DEF VAR liDSSMsSeq          AS INT  NO-UNDO.
DEF VAR lcDSSBundleId       AS CHAR NO-UNDO.

IF llDoEvent THEN DO:

   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

    DEFINE VARIABLE lhBufMSLimit     AS HANDLE    NO-UNDO.
    lhBufMSLimit = BUFFER MServiceLimit:HANDLE.

END.

FUNCTION fMakeServLimit RETURN LOGICAL
   (INPUT icServiceLimitGroup   AS CHAR,
    INPUT iiMSSeq               AS INT,
    INPUT iiCustNum             AS INT,
    INPUT idActStamp            AS DEC,
    INPUT idEndStamp            AS DEC,
    OUTPUT ocResult             AS CHAR):

   DEF VAR ldtDate    AS DATE NO-UNDO.
   DEF VAR liTime     AS INT  NO-UNDO.
   DEF VAR ldBegin    AS DEC  NO-UNDO EXTENT 2.
   DEF VAR ldEnd      AS DEC  NO-UNDO EXTENT 2.
   DEF VAR liCount    AS INT  NO-UNDO.
   DEF VAR liDays     AS INT  NO-UNDO.
   DEF VAR ldFactor   AS DEC  NO-UNDO EXTENT 2.
   DEF VAR ldaBegDate AS DATE NO-UNDO.
   DEF VAR llCreated  AS LOG  NO-UNDO INIT FALSE.

   DEF VAR llCreateDSS          AS LOG  NO-UNDO.
   DEF VAR llUpdateDSSLimit     AS LOG  NO-UNDO.
   DEF VAR ldeUpdateDSSLimit    AS DEC  NO-UNDO.
   DEF VAR lcError              AS CHAR NO-UNDO.
   DEF VAR ldeCheckTS           AS DEC  NO-UNDO.

   DEF VAR lcPostpaidDataBundles AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.

   DEF BUFFER bContract FOR DayCampaign.
   DEF BUFFER bMobSub   FOR MobSub.
   DEF BUFFER MDSSServiceLimit FOR MServiceLimit.
   
   IF idActStamp = 0 OR idActStamp = ? THEN idActStamp = fMakeTS(). 
   IF idEndStamp = 0 OR idEndStamp = ? THEN idEndStamp = 99999999.99999.

   lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS").

   /* Use current time stamp if DSS activation stamp is less than current */
   /* because request could be rejected and DSS usage could be wrong      */
   IF LOOKUP(icServiceLimitGroup,{&DSS_BUNDLES}) > 0 AND
      idActStamp < fMakeTS() THEN idActStamp = fMakeTS().

   IF idActStamp < fMakeTS() THEN ldeCheckTS = fMakeTS().
   ELSE ldeCheckTS = idActStamp.

   fSplitTS(idActStamp,
            OUTPUT ldtDate,
            OUTPUT liTime).

   IF ldtDate = ? THEN DO:
      ocResult = SUBST("Corrupted activation stamp: &1", idActStamp).
      RETURN FALSE.
   END.

   FIND FIRST bMobSub WHERE bMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bMobSub THEN DO:
      ocResult = "MobSub not found".
      RETURN FALSE.
   END. /* IF NOT AVAILABLE bMobSub THEN DO: */

   /* Check whether DSS is already active or not or DSS allowed */
   IF LOOKUP(icServiceLimitGroup,{&DSS_BUNDLES}) > 0 THEN DO:
      IF NOT fIsDSSAllowed(INPUT  iiCustNum,
                           INPUT  iiMsSeq,
                           INPUT  ldeCheckTS,
                           INPUT  icServiceLimitGroup,
                           INPUT  "HSDPA_MSISDN",
                           OUTPUT ldeCurrMonthLimit,
                           OUTPUT ldeConsumedData,
                           OUTPUT ldeOtherMonthLimit,
                           OUTPUT ocResult) THEN RETURN FALSE.
      ELSE ASSIGN
         lcDSSBundleId = icServiceLimitGroup
         llUpdateDSSLimit = TRUE
         llCreateDSS = TRUE.
   END. /* IF lcDCEvent = {&DSS} AND */
   /* Link DSS upsell to the Main DSS subscription */
   ELSE IF icServiceLimitGroup BEGINS "DSS" AND
           INDEX(icServiceLimitGroup,"UPSELL") > 0 AND
           fGetDSSMsSeqLimit(INPUT  iiCustNum,
                             INPUT  ldeCheckTS,
                             OUTPUT liDSSMsSeq,
                             OUTPUT ldeDSSTotalLimit,
                             OUTPUT lcDSSBundleId) THEN
      ASSIGN iiMSSeq = liDSSMsSeq
             llCreateDSS = TRUE
             llUpdateDSSLimit = FALSE.
   /* Exclude Prepaid bundle and data Upsells from update DSS Limit */
   ELSE IF NOT bMobSub.PayType AND
           fIsDSSActive(iiCustNum,ldeCheckTS) AND
           LOOKUP(icServiceLimitGroup,lcPostpaidDataBundles) > 0 THEN DO:
      lcDSSBundleId = fGetActiveDSSId(INPUT bMobSub.CustNum,INPUT ldeCheckTS).
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

      IF lcDSSBundleId = {&DSS} OR (lcDSSBundleId = "DSS2" AND
         LOOKUP(bMobSub.CLIType,lcAllowedDSS2SubsType) > 0) THEN
         llUpdateDSSLimit = TRUE.
   END.

   /* Activate */ 

   FOR EACH ServiceLimitGroup NO-LOCK WHERE 
            ServiceLimitGroup.Brand     = gcBrand AND
            ServiceLimitGroup.GroupCode = icServiceLimitGroup,
      FIRST bContract WHERE
            bContract.Brand = gcBrand AND
            bContract.DCEvent = ServiceLimitGroup.GroupCode,
       EACH ServiceLimit NO-LOCK WHERE 
            ServiceLimit.GroupCode  = icServiceLimitGroup AND 
            ServiceLimit.ValidFrom <= ldtDate             AND 
            ServiceLimit.ValidTo   >= ldtDate:

      IF bContract.DCType EQ "8" OR bContract.InstanceLimit > 1 THEN
      DO WHILE TRUE:
         FIND LAST MServiceLimit NO-LOCK WHERE
                   MServiceLimit.MsSeq    = iiMsSeq AND
                   MServiceLimit.DialType = ServiceLimit.DialType AND
                   MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
                   MServiceLimit.EndTS    = idEndStamp
         USE-INDEX MsSeq NO-ERROR.
         IF AVAIL MServiceLimit
         THEN idEndStamp = fSecOffSet(MServiceLimit.EndTS,-1).
         ELSE LEAVE.
      END.

      ASSIGN
         ldBegin[1] = idActStamp
         ldEnd[1]   = idEndStamp
         ldFactor   = 1.
         
      /* Create MServiceLimit with Relative limit */
      IF (ServiceLimit.FirstMonthCalc = 1 OR bContract.FirstMonthCalc = 1) AND
         DAY(ldtDate) > 1 THEN DO:
         IF MONTH(ldtDate) = 12 THEN ASSIGN 
            ldaBegDate = DATE(1,1,YEAR(ldtDate) + 1)
            liDays     = 31.
         ELSE ASSIGN 
            ldaBegDate = DATE(MONTH(ldtDate) + 1,1,YEAR(ldtDate))
            liDays     = DAY(ldaBegDate - 1).
      
         ASSIGN 
            ldFactor[1] = (liDays - DAY(ldtDate) + 1) / liDays
            ldEnd[1]    = fMake2DT(ldaBegDate - 1,86399)
            ldBegin[2]  = fMake2DT(ldaBegDate,0)
            ldEnd[2]    = idEndStamp.
      END. /* IF (ServiceLimit.FirstMonthCalc = 1 */

      /* Note: If DSS is being added in middle of month then two          */
      /* MserviceLimit record will be created                             */
      /* 1. For First month - DSS limit will include other data           */
      /* bundles and upsells limit. It will only be valid for 1st Month   */
      /* 2. For other month(s) - DSS limit will only include other data   */
      /* bundles limit. It will be valid for other months.                */

      IF LOOKUP(icServiceLimitGroup,{&DSS_BUNDLES}) > 0 AND
         ldeCurrMonthLimit <> ldeOtherMonthLimit THEN
         ASSIGN ldaBegDate = fLastDayOfMonth(ldtDate)
                ldEnd[1]   = fMake2DT(ldaBegDate,86399)
                ldBegin[2] = fMake2DT(ldaBegDate + 1,0)
                ldEnd[2]   = idEndStamp.

      DO liCount = 1 TO 2:
      
         IF ldBegin[liCount] = 0 OR ldEnd[liCount] = 0 THEN NEXT. 
         
         IF llCreateDSS THEN DO:
            IF liCount = 1 AND
               CAN-FIND(FIRST MServiceLimit WHERE 
                        MServiceLimit.Custnum  = iiCustnum             AND
                        MServiceLimit.DialType = ServiceLimit.DialType AND
                        MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND 
                        MServiceLimit.FromTS  <= ldBegin[liCount]      AND
                        MServiceLimit.EndTS   >= ldBegin[liCount]) 
            THEN NEXT.
         END. /* IF llCreateDSS THEN DO: */
         ELSE IF bContract.DCType NE "8" AND
                 bContract.InstanceLimit <= 1 AND
            CAN-FIND(FIRST MServiceLimit WHERE 
                           MServiceLimit.MSSeq    = iiMSSeq               AND
                           MServiceLimit.DialType = ServiceLimit.DialType AND
                           MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND 
                           MServiceLimit.FromTS  <= ldBegin[liCount]      AND
                           MServiceLimit.EndTS   >= ldBegin[liCount]) 
         THEN NEXT.

         IF llDoEvent THEN DO:
            RUN StarEventInitialize( lhBufMSLimit ).
         END.

         CREATE mServiceLimit.
         ASSIGN
            mServiceLimit.MSID      = NEXT-VALUE(mServiceLimit)
            mServiceLimit.SLSeq     = ServiceLimit.SLSeq          
            mServiceLimit.MSSeq     = iiMsseq       
            mServiceLimit.Custnum   = (IF llCreateDSS THEN iiCustnum ELSE 0)
            mServiceLimit.DialType  = ServiceLimit.DialType          
            mServiceLimit.InclUnit  = ServiceLimit.InclUnit
            mServiceLimit.FromTS    = ldBegin[liCount]
            mServiceLimit.EndTS     = ldEnd[liCount]
            llCreated               = True.

         /* Upgrade Upsell */
         IF AVAILABLE MsRequest AND MsRequest.ReqDParam1 > 0 AND
            ServiceLimit.DialType EQ {&DIAL_TYPE_GPRS} THEN
            MServiceLimit.InclAmt = ROUND(MsRequest.ReqDParam1 * ldFactor[liCount],2).
         ELSE
            MServiceLimit.InclAmt = ROUND(ServiceLimit.InclAmt * ldFactor[liCount],2).

         /* Update DSS Limit with bundle(s) HSDPA limit */
         IF llUpdateDSSLimit THEN DO:
            /* 1. If new DSS bundle is being added then update DSS limit */
            /*  with consolidated existing bundles/upsell limit          */
            /* 2. Create additional DSS MServiceLimit record for next    */
            /* month with only active bundle limits                      */

            IF llCreateDSS THEN DO:
               IF liCount = 1 THEN
                  ldeUpdateDSSLimit = ldeCurrMonthLimit.
               ELSE IF liCount = 2 THEN
                  ASSIGN ldeUpdateDSSLimit = ldeOtherMonthLimit
                         ldeConsumedData   = 0.
            END. /* llCreateDSS */
            /* 3. Update DSS limit with newly added bundle limit */
            ELSE IF MServiceLimit.DialType = {&DIAL_TYPE_GPRS} THEN
               ldeUpdateDSSLimit = MServiceLimit.InclAmt.

            /* Update DSS limit when DialType is GPRS (eg: 7) */
            IF MServiceLimit.DialType = {&DIAL_TYPE_GPRS} THEN DO:
               IF LOOKUP(icServiceLimitGroup,{&DSS_BUNDLES}) > 0 THEN
                  MServiceLimit.InclAmt = INT(ldeUpdateDSSLimit).

               RUN pUpdateDSSLimit(INPUT iiCustnum,
                                (IF icServiceLimitGroup BEGINS {&DSS} THEN "Add"
                                 ELSE "Update"),
                                INPUT ldeUpdateDSSLimit,
                                INPUT ldeConsumedData,
                                INPUT idActStamp,
                                OUTPUT ldeDSSTotalLimit).
            END. /* IF MServiceLimit.DialType = {&DIAL_TYPE_GPRS} THEN DO: */
         END. /* IF llUpdateDSSLimit THEN DO: */

         IF llDoEvent THEN DO:
            RUN StarEventMakeCreateEvent( lhBufMSLimit ).
            IF llCleanServLimitEventLog THEN fCleanEventObjects().
         END.
      END.
   
   END.

   RETURN llCreated.
   
END.

FUNCTION fMakeServLPool RETURN LOGICAL
   (INPUT icServiceLimitGroup   AS CHAR,
    INPUT iiMSSeq               AS INT,
    INPUT iiCustNum             AS INT,
    INPUT idActStamp            AS DEC,
    INPUT idEndStamp            AS DEC,
   OUTPUT ocError               AS CHAR):
   
   DEF VAR ldtDate              AS DATE NO-UNDO.
   DEF VAR liTime               AS INT  NO-UNDO.
   DEF VAR ldeMonthBegin AS DEC NO-UNDO. 
   DEF VAR ldeMonthEnd AS DEC NO-UNDO. 
   DEF VAR ldeOldEndStamp AS DEC NO-UNDO. 
   DEF VAR liUpSellCount AS INT NO-UNDO. 
   DEF VAR ldeLimitAmt AS DEC NO-UNDO. 
   DEF VAR ldaLastDay AS DATE NO-UNDO. 

   DEF BUFFER MServiceLPool FOR MServiceLPool.
   DEF BUFFER bMServiceLPool FOR MServiceLPool.
   DEF BUFFER ServiceLimitGroup FOR ServiceLimitGroup.
   DEF BUFFER DayCampaign FOR DayCampaign.
   DEF BUFFER ServiceLimit FOR ServiceLimit.
   DEF BUFFER MServiceLimit FOR MServiceLimit.
   
   IF idActStamp = 0 OR idActStamp = ? THEN idActStamp = fMakeTS(). 

   fSplitTS(idActStamp, OUTPUT ldtDate, OUTPUT liTime).
   ldaLastDay = fLastDayOfMonth(ldtDate).
   ldeMonthEnd = fMake2Dt(ldaLastDay, 86399).
   ldeMonthBegin = fMake2Dt(DATE(MONTH(ldtDate),1,YEAR(ldtDate)),0).
   
   IF idEndStamp = 0 OR idEndStamp = ? THEN idEndStamp = ldeMonthEnd.

   fSplitTS(idActStamp,
            OUTPUT ldtDate,
            OUTPUT liTime).

   IF ldtDate = ? THEN DO:
      ocError = SUBST("Corrupted activation stamp: &1", idActStamp).
      RETURN FALSE.
   END.
   
   FOR FIRST ServiceLimitGroup NO-LOCK WHERE 
             ServiceLimitGroup.Brand     = gcBrand AND
             ServiceLimitGroup.GroupCode = icServiceLimitGroup,
       FIRST DayCampaign WHERE
             DayCampaign.Brand = gcBrand AND
             DayCampaign.DCEvent = ServiceLimitGroup.GroupCode,
        EACH ServiceLimit NO-LOCK WHERE 
             ServiceLimit.GroupCode  = icServiceLimitGroup AND 
             ServiceLimit.ValidFrom <= ldtDate             AND 
             ServiceLimit.ValidTo   >= ldtDate:
      
      /* when upsell is created 1st time create upsell contract*/
      fMakeServLimit(icServiceLimitGroup,     
                     iiMsSeq,
                     iiCustNum,
                     idActStamp,
                     idEndStamp,
              OUTPUT ocError).

      IF icServiceLimitGroup BEGINS "DSS" AND
         INDEX(icServiceLimitGroup,"UPSELL") > 0 THEN
         FIND FIRST MServiceLimit WHERE 
                    MServiceLimit.Custnum  = iiCustnum             AND
                    MServiceLimit.DialType = ServiceLimit.DialType AND
                    MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND 
                    MServiceLimit.FromTS  <= idActStamp            AND
                    MServiceLimit.EndTS   >= idActStamp NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST MServiceLimit WHERE 
                    MServiceLimit.MSSeq    = iiMSSeq               AND
                    MServiceLimit.DialType = ServiceLimit.DialType AND
                    MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND 
                    MServiceLimit.FromTS  <= idActStamp            AND
                    MServiceLimit.EndTS   >= idActStamp NO-LOCK NO-ERROR.
      
      IF NOT AVAIL MServiceLimit THEN DO:
         ocError = "Upsell contract not found".
         RETURN FALSE.
      END.

      ldeLimitAmt = ServiceLimit.InclAmt.

      /* check monthly limit */
      FOR EACH MServiceLPool WHERE
               MserviceLPool.MsSeq = MServiceLimit.MsSeq AND
               MserviceLPool.SLSeq = MServiceLimit.SLSeq AND
               MserviceLPool.EndTS <= ldeMonthEnd AND
               MserviceLPool.FromTS >= ldeMonthBegin NO-LOCK:
         liUpSellCount = liUpSellCount + 1.
      END.
      IF liUpSellCount >= DayCampaign.InstanceLimit THEN DO:
         ocError = SUBST("Upsell limit &1 exceeded",DayCampaign.InstanceLimit).
         RETURN FALSE.
      END.

      IF icServiceLimitGroup BEGINS {&DSS} + "_UPSELL" OR
         icServiceLimitGroup = "DSS200_UPSELL" OR
         icServiceLimitGroup = "DSS2_UPSELL" THEN
         ldeDSSUpsellLimit = ldeLimitAmt.

      FIND FIRST MServiceLPool WHERE
                 MserviceLPool.MsSeq = MServiceLimit.MsSeq AND
                 MserviceLPool.SLSeq = MServiceLimit.SLSeq AND
                 MserviceLPool.EndTS >= idActStamp AND
                 MserviceLPool.FromTS <= idActStamp NO-LOCK NO-ERROR.

      /* close existing pool */
      IF AVAIL MserviceLPool THEN DO:

         ldeOldEndStamp = fSecOffSet(idActStamp, -1).
   
         FIND CURRENT MserviceLPool EXCLUSIVE-LOCK.

         IF llDoEvent THEN DO:
            RUN StarEventInitialize((BUFFER MServiceLPool:HANDLE)).
            RUN StarEventSetOldBuffer((BUFFER MServiceLPool:HANDLE)).
         END.
         
         MserviceLPool.EndTs = ldeOldEndStamp.
         ldeLimitAmt = ldeLimitAmt + MserviceLPool.LimitAmt.
       
         IF llDoEvent THEN DO:
            RUN StarEventMakeModifyEvent((BUFFER MServiceLPool:HANDLE)).
            IF llCleanServLimitEventLog THEN fCleanEventObjects().
         END.

      END.
         
      IF llDoEvent THEN DO:
         RUN StarEventInitialize((BUFFER bMServiceLPool:HANDLE)).
      END.
      
      CREATE bMServiceLPool.
      ASSIGN
         bMServiceLPool.MsSeq    = MServiceLimit.MsSeq
         bMServiceLPool.Custnum  = MServiceLimit.Custnum
         bMserviceLPool.SLSeq    = MServiceLimit.SlSeq
         bMserviceLPool.FromTS   = idActStamp
         bMserviceLPool.EndTS    = idEndStamp 
         bMserviceLPool.LimitAmt = ldeLimitAmt.
                
      IF llDoEvent THEN DO:
         RUN StarEventMakeCreateEvent((BUFFER bMServiceLPool:HANDLE)).
         IF llCleanServLimitEventLog THEN fCleanEventObjects().
      END.

   END.

   RETURN TRUE.

END.

&ENDIF
