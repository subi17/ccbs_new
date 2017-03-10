/* ----------------------------------------------------------------------
  MODULE .......: dss_bundle_first_month_fee.p
  TASK .........: Calculate first month's fixed fee for DSS and related bundles
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 06.09.11
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

DISABLE TRIGGERS FOR LOAD OF FixedFee.

{Syst/commali.i}
{Func/cparam2.i}
{Syst/funcrunprocess_update.i}

DEF INPUT  PARAMETER idaFromDate      AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaToDate        AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiInvCust        AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdateInterval AS INT  NO-UNDO.
DEF INPUT  PARAMETER icRunMode        AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiHandled        AS INT  NO-UNDO.

DEF VAR ldPeriodFrom    AS DEC  NO-UNDO.
DEF VAR ldPeriodTo      AS DEC  NO-UNDO.
DEF VAR liPeriod        AS INT  NO-UNDO.
DEF VAR lcIPLContracts  AS CHAR NO-UNDO.
DEF VAR lcBONOContracts AS CHAR NO-UNDO.

DEF VAR lcAllowedDSS2SubsType         AS CHAR NO-UNDO.
DEF VAR lcExcludeBundles              AS CHAR NO-UNDO.
DEF VAR lcFirstMonthUsageBasedBundles AS CHAR  NO-UNDO.

{Mm/dss_bundle_first_month_fee.i}

/******** Main start ********/

ASSIGN
   ldPeriodFrom    = fMake2Dt(idaFromDate,0)
   ldPeriodTo      = fMake2Dt(idaToDate,86399)
   liPeriod        = YEAR(idaToDate) * 100 + MONTH(idaToDate)
   lcIPLContracts  = fCParamC("IPL_CONTRACTS")
   lcBONOContracts = fCParamC("BONO_CONTRACTS")
   lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
   lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES")
   lcFirstMonthUsageBasedBundles = fCParamC("FIRST_MONTH_USAGE_BASED_BUNDLES").

IF iiInvCust > 0 THEN DO:
   FIND FIRST Customer WHERE
              Customer.Brand   = gcBrand AND
              Customer.CustNum = iiInvCust NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN RETURN "Customer not found".

   RUN pGetCustomerSubscriptions(INPUT Customer.Custnum).

END. /* IF iiInvCust > 0 THEN DO: */
ELSE
   RUN pGetAllCustomersSubscriptions.

IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.

IF oiHandled > 0 AND iiInvCust = 0 THEN DO TRANS:
   CREATE ActionLog.
   ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "FixedFee"  
         ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") +
                                  STRING(MONTH(TODAY),"99")  +
                                  STRING(DAY(TODAY),"99")
         ActionLog.ActionID     = "BUNDLEFEE" +
                                  (IF icRunMode eq "test" THEN "TEST" ELSE "")
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                  MONTH(TODAY)
         ActionLog.ActionDec    = oiHandled
         ActionLog.ActionChar   = STRING(oiHandled) + 
                                  " first month fees were updated"
         ActionLog.ActionStatus = 3
         ActionLog.UserCode     = katun
         ActionLog.FromDate     = idaFromDate
         ActionLog.ToDate       = idaToDate.
         ActionLog.ActionTS     = fMakeTS().
END.

RETURN "".

FINALLY:
   EMPTY TEMP-TABLE ttSub NO-ERROR.
END.

/******** Main end ********/

PROCEDURE pGetAllCustomersSubscriptions:

   FOR EACH Customer NO-LOCK:

      RUN pGetCustomerSubscriptions(INPUT Customer.Custnum).

   END. /* FOR EACH Customer NO-LOCK: */

   RETURN "".

END PROCEDURE.


PROCEDURE pGetCustomerSubscriptions:

   DEF INPUT PARAMETER iiInvCust    AS INT  NO-UNDO.

   DEF VAR ldeDSSUsage              AS DEC  NO-UNDO.
   DEF VAR ldeBundleAmt             AS DEC  NO-UNDO.
   DEF VAR ldeDSSLimit              AS DEC  NO-UNDO.
   DEF VAR llFullMonth              AS LOG  NO-UNDO.
   DEF VAR ldFFItemStartDate        AS DATE NO-UNDO.
   DEF VAR liFFItemStartTime        AS INT  NO-UNDO.
   DEF VAR ldFeeAmount              AS DEC  NO-UNDO.
   DEF VAR lcBundleId               AS CHAR NO-UNDO.

   DEF BUFFER bMServiceLimit        FOR MServiceLimit.
   DEF BUFFER bServiceLimit         FOR ServiceLimit.
   DEF BUFFER bDayCampaign          FOR DayCampaign.
   DEF BUFFER bMSRequest            FOR MsRequest.
   DEF BUFFER MSRequest             FOR MsRequest.
   DEF BUFFER b2MSRequest           FOR MsRequest.
   DEF BUFFER bFixedFee             FOR FixedFee.

   EMPTY TEMP-TABLE ttSub NO-ERROR.
   EMPTY TEMP-TABLE ttMsOwner NO-ERROR.

   /* Check wheather customer is linked with DSS service or not */
   lcBundleId = fGetDSSId(iiInvCust,ldPeriodTo).
   IF lcBundleId = "" THEN RETURN.

   ldeDSSUsage = fGetDSSUsage(INPUT iiInvCust,
                              INPUT idaFromDate,
                              OUTPUT ldeDSSLimit).

   fGetMsOwnerTempTable(iiInvCust,idaFromDate,idaToDate,FALSE,FALSE).

   FOR EACH ttMsOwner BREAK BY ttMsOwner.MsSeq:

      IF lcBundleId = "DSS2" AND
         LOOKUP(ttMsOwner.CLIType,lcAllowedDSS2SubsType) = 0 THEN NEXT.

      DATA_BUNDLE_LOOP:
      FOR EACH bMServiceLimit WHERE
               bMServiceLimit.MsSeq    = ttMsOwner.MsSeq AND
               bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
               bMServiceLimit.FromTS  <= ttMsOwner.PeriodTo AND
               bMServiceLimit.EndTS   >= ttMsOwner.PeriodFrom NO-LOCK,
         FIRST bServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
               bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
         FIRST bDayCampaign NO-LOCK WHERE
               bDayCampaign.Brand = gcBrand AND
               bDayCampaign.DCEvent = bServiceLimit.GroupCode AND
               LOOKUP(bDayCampaign.DCType,
                     {&PERCONTRACT_RATING_PACKAGE}) > 0:

         IF LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 OR
            bDayCampaign.DCEvent BEGINS {&DSS}
         THEN NEXT DATA_BUNDLE_LOOP.

         IF CAN-FIND(FIRST ttSub WHERE
                           ttSub.MsSeq    = bMServiceLimit.MsSeq AND
                           ttSub.BundleId = bServiceLimit.GroupCode)
         THEN NEXT DATA_BUNDLE_LOOP.

         ASSIGN ldeBundleAmt = (bMServiceLimit.InclAmt * 1024 * 1024)
                llFullMonth  = FALSE.

         /* If ongoing bundle termination request OR NOT FIRST  */
         /* month then reduce data amount from total DSS limit  */
         /* because bundle will be charged as full month amt    */

         /* First and last month is always usage based          */
         IF LOOKUP(bDayCampaign.DCEvent,lcFirstMonthUsageBasedBundles) > 0 AND
            (bMServiceLimit.FromTs >= ldPeriodFrom OR
             bMServiceLimit.EndTs < ldPeriodTo) THEN .

         ELSE IF bMServiceLimit.FromTS < ldPeriodFrom THEN
            llFullMonth = TRUE.

         /* If bundle is terminated because of BTC in first month */
         /* then bundle will be charged based on the usage        */
         ELSE IF CAN-FIND(FIRST MSRequest WHERE
            MSRequest.MsSeq = bMServiceLimit.MsSeq AND
            MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
            MsRequest.ActStamp >= ldPeriodFrom AND
            MsRequest.ActStamp <= ldPeriodTo AND
            MsRequest.ReqCParam3 = bDayCampaign.DCEvent AND
            MsRequest.ReqSource <> {&REQUEST_SOURCE_BTC} AND
            LOOKUP(STRING(MsRequest.ReqStat),"4,99") = 0 USE-INDEX MsSeq) OR
            CAN-FIND(FIRST MSRequest WHERE
            MSRequest.MsSeq = bMServiceLimit.MsSeq AND
            MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
            MsRequest.ActStamp >= ldPeriodFrom AND
            MsRequest.ActStamp <= ldPeriodTo AND
            MsRequest.ReqCParam3 = bDayCampaign.DCEvent AND
            LOOKUP(MsRequest.ReqCParam3,lcBONOContracts) = 0 AND
            LOOKUP(MsRequest.ReqCParam3,lcIPLContracts)  = 0 AND
            MsRequest.ReqSource  = {&REQUEST_SOURCE_BTC} USE-INDEX MsSeq)
         THEN llFullMonth = TRUE.
            
         /* if bundle is originate from stc or btc then it as full month */
         IF llFullMonth = FALSE AND
            bMServiceLimit.FromTs >= ldPeriodFrom THEN
            FOR EACH MsRequest NO-LOCK WHERE
                     MsRequest.MsSeq = ttMsOwner.MsSeq AND
                     MsRequest.ReqType = 8 AND
                     MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                     MsRequest.ActStamp >= ldPeriodFrom AND
                     MsRequest.ActStamp <= ldPeriodTo AND
                     MsRequest.OrigRequest > 0 AND
                LOOKUP(STRING(MsRequest.ReqStatus),"2,9") > 0 USE-INDEX MsSeq,
               FIRST bMsRequest NO-LOCK WHERE
                     bMsRequest.MsRequest = MsRequest.OrigRequest:
               IF (bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR 
                   bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}) AND
                   bMsRequest.ActStamp = ldPeriodFrom THEN DO:
                  llFullMonth = TRUE.
                  LEAVE.
               END. /* IF bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} */
            END. /* FOR EACH MsRequest NO-LOCK WHERE */

         /* YTS-6939  CONT15, Activation >= 6.5.2015
             Termination month = activation month
             DSS active, CONT15 data counter not full */
         IF llFullMonth = FALSE AND 
            (bMServiceLimit.FromTs >= ldPeriodFrom OR
             bMServiceLimit.EndTs <= ldPeriodTo) AND
            (bDayCampaign.DCType = {&DCTYPE_SERVICE_PACKAGE} OR
             bDayCampaign.DCType = {&DCTYPE_BUNDLE}) THEN DO:

            FIND FIRST MSRequest NO-LOCK WHERE
                       MSRequest.MsSeq = ttMsOwner.MsSeq AND
                       MSRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                       MsRequest.ActStamp >= ldPeriodFrom AND
                       MsRequest.ActStamp <= ldPeriodTo AND
                       MSRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                       MsRequest.ReqSource  = {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION} AND
                   LOOKUP(STRING(MsRequest.ReqStatus),"2,9") > 0 USE-INDEX MsSeq NO-ERROR.
            IF AVAIL MsRequest THEN DO:
            
               FIND FIRST MobSub NO-LOCK WHERE
                          MobSub.MsSeq = MsRequest.MsSeq NO-ERROR.
               IF AVAILABLE MobSub THEN
                  llFullMonth = (MobSub.ActivationDate >= 5/6/2015).
               ELSE DO:
                  FIND FIRST TermMobSub NO-LOCK WHERE
                             TermMobSub.MsSeq = MsRequest.MsSeq NO-ERROR.
                  IF AVAILABLE TermMobSub THEN
                     llFullMonth= (TermMobSub.ActivationDate >= 5/6/2015).
               END.
            END.
         END.

         /* if the data bundle contract is terminated from STC/BTC
            in middle of month then skip first month fee calculation */
         IF llFullMonth = TRUE AND
            bMServiceLimit.EndTs < ldPeriodTo THEN
            FOR EACH MsRequest NO-LOCK WHERE
                     MsRequest.MsSeq = ttMsOwner.MsSeq AND
                     MsRequest.ReqType = 9 AND
                     MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                     MsRequest.ActStamp >= ldPeriodFrom AND
                     MsRequest.ActStamp < ldPeriodTo AND
                     MsRequest.OrigRequest > 0 AND
                     LOOKUP(STRING(MsRequest.ReqStatus),"2,9") > 0
                     USE-INDEX MsSeq,
               FIRST bMsRequest NO-LOCK WHERE
                     bMsRequest.MsRequest = MsRequest.OrigRequest:
               IF (bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR
                   bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}) AND
                   bMsRequest.ActStamp < ldPeriodTo THEN DO:
                  llFullMonth = FALSE.
                  LEAVE.
               END.
            END.


         IF llFullMonth THEN DO:
            IF ldeDSSUsage >= ldeBundleAmt THEN
               ldeDSSUsage = (ldeDSSUsage - ldeBundleAmt).
            ELSE ldeDSSUsage = 0.

            NEXT DATA_BUNDLE_LOOP.
         END. /* IF llFullMonth THEN DO: */

         CREATE ttSub.
         ASSIGN ttSub.MsSeq            = bMServiceLimit.MsSeq
                ttSub.CustNum         = ttMsOwner.CustNum
                ttSub.BundleId        = bServiceLimit.GroupCode
                ttSub.BundleLimit     = ldeBundleAmt
                ttSub.BundleLimitInMB = bMServiceLimit.InclAmt
                ttSub.InclUnit        = bServiceLimit.InclUnit
                ttSub.Priority        = bDayCampaign.DSSPriority.

      END. /* FOR EACH bMServiceLimit WHERE */
   END. /* FOR EACH ttMsOwner NO-LOCK WHERE */


   /* Calculate first month fee */
   FOR EACH ttSub WHERE
            ttSub.CustNum = iiInvCust
       BY ttSub.Priority:

      ldFeeAmount = 0.

      FOR FIRST DayCampaign NO-LOCK WHERE
                DayCampaign.Brand   = gcBrand AND
                DayCampaign.DCEvent = ttSub.BundleId,
          FIRST FixedFee NO-LOCK USE-INDEX HostTable WHERE
                FixedFee.Brand     = gcBrand AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue  = STRING(ttSub.MsSeq) AND
                FixedFee.FeeModel  = DayCampaign.FeeModel AND
                FixedFee.CalcObj   = DayCampaign.DCEvent AND
                FixedFee.InUse     = TRUE AND
                FixedFee.BegDate  <= idaToDate AND
                FixedFee.EndPer   >= liPeriod,
          FIRST FMItem NO-LOCK WHERE
                FMItem.Brand     = gcBrand AND
                FMItem.FeeModel  = FixedFee.FeeModel AND
                FMItem.FromDate <= FixedFee.BegDate AND
                FMItem.ToDate   >= FixedFee.BegDate AND
                FMItem.FirstMonthBr = 2:

          /* already billed */
          FIND FIRST FFItem OF FixedFee WHERE
                     FFItem.BillPeriod = liPeriod NO-LOCK NO-ERROR.
          IF NOT AVAILABLE FFItem OR 
            (FFItem.Billed AND icRunMode NE "test") THEN NEXT.

          /* % of fee, based on usage */
          IF ldeDSSUsage > 0 THEN
             ldFeeAmount = fCalculateProportionalFee(
                                         (IF ldeDSSUsage < ttSub.BundleLimit
                                          THEN ldeDSSUsage
                                          ELSE ttSub.BundleLimit),
                                          ttSub.InclUnit,
                                          ttSub.BundleLimitInMB,
                                          FixedFee.Amt).
                        
          ELSE ldFeeAmount = 0.
      
          IF icRunMode EQ "test" THEN oiHandled = oiHandled + 1.
          ELSE DO TRANS:
               
             IF FFItem.Billed THEN NEXT.

             FIND CURRENT FFItem EXCLUSIVE-LOCK.

             FFItem.Amt = ldFeeAmount.

             FIND FIRST bFixedFee WHERE
                        ROWID(bFixedFee) = ROWID(FixedFee) EXCLUSIVE-LOCK.
             ASSIGN 
                /* pmf: partial monthly fee */
                bFixedFee.ServiceLimitGroup = "PMF:" + ttSub.BundleId + ":" +
                                              STRING(ldFeeAmount)
                oiHandled = oiHandled + 1.
          END. /* DO TRANS: */

          RELEASE FFItem.
          RELEASE FixedFee.
      
          IF iiUpdateInterval > 0 AND oiHandled MOD iiUpdateInterval = 0 THEN DO:
             IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiHandled) THEN
                RETURN "ERROR:Stopped".
          END.
      END. /* FOR FIRST DayCampaign NO-LOCK WHERE */

      IF ldeDSSUsage > 0 THEN
         ldeDSSUsage = (ldeDSSUsage - ttSub.BundleLimit).

   END. /* FOR EACH ttSub BREAK BY ttSub.CustNum: */

   RETURN "".

END PROCEDURE.


