/* ----------------------------------------------------------------------
  MODULE .......: dss_bundle_first_month_fee.i
  TASK .........: Calculate first month's fixed fee for DSS and related bundles
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 06.09.11
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&IF "{&dss_bundle_first_month_fee}" NE "YES" 
&THEN

&GLOBAL-DEFINE dss_bundle_first_month_fee YES

{commali.i}
{timestamp.i}
{tmsconst.i}
{fdss.i}
{bundle_first_month_fee.i}
{istc.i}

DEF TEMP-TABLE ttSub NO-UNDO
   FIELD MsSeq            AS INT
   FIELD CustNum          AS INT
   FIELD BundleId         AS CHAR
   FIELD BundleLimit      AS DEC
   FIELD BundleLimitinMB  AS DEC
   FIELD UsedDSSLimit     AS DEC
   FIELD InclUnit         AS INT
   FIELD Priority         AS INT
   INDEX MsSeqBun MsSeq BundleId
   INDEX CustNumPri CustNum Priority.

PROCEDURE pGetDSSLinkedSubsFee:

   DEF INPUT PARAMETER iiInvCust    AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiMsSeq      AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiPeriod     AS INT  NO-UNDO.
   DEF INPUT PARAMETER icBundleId   AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icDSSBundle  AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER odeFeeAmt   AS DEC  NO-UNDO.

   DEF VAR ldeDSSUsage              AS DEC  NO-UNDO.
   DEF VAR ldeBundleAmt             AS DEC  NO-UNDO.
   DEF VAR ldPeriodFrom             AS DEC  NO-UNDO.
   DEF VAR ldPeriodTo               AS DEC  NO-UNDO.
   DEF VAR ldFromDate               AS DATE NO-UNDO.
   DEF VAR ldToDate                 AS DATE NO-UNDO.
   DEF VAR llFullMonth              AS LOG  NO-UNDO.
   DEF VAR liCount                  AS INT  NO-UNDO.
   DEF VAR ldeDSSLimit              AS DEC  NO-UNDO.

   DEF BUFFER bMServiceLimit        FOR MServiceLimit.
   DEF BUFFER bServiceLimit         FOR ServiceLimit.
   DEF BUFFER bDayCampaign          FOR DayCampaign.
   DEF BUFFER bMsRequest            FOR MsRequest.

   ASSIGN ldFromDate   = fInt2Date(iiPeriod,1)
          ldToDate     = fInt2Date(iiPeriod,2)
          ldPeriodFrom = fMake2Dt(ldFromDate,0)
          ldPeriodTo   = fMake2Dt(ldToDate,86399).
          
   ldeDSSUsage = fGetDSSUsage(INPUT iiInvCust,
                              INPUT ldFromDate,
                              OUTPUT ldeDSSLimit).

   FOR EACH ttMsOwner BREAK BY ttMsOwner.MsSeq:

      IF icDSSBundle = "DSS2" AND
         LOOKUP(ttMsOwner.CLIType,lcAllowedDSS2SubsType) = 0 THEN NEXT.

         FOR EACH bMServiceLimit WHERE
                  bMServiceLimit.MsSeq   = ttMsOwner.MsSeq AND
                  bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
                  bMServiceLimit.FromTS <= ttMsOwner.PeriodTo     AND
                  bMServiceLimit.EndTS  >= ttMsOwner.PeriodFrom NO-LOCK,
             FIRST bServiceLimit NO-LOCK WHERE
                   bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
             FIRST bDayCampaign NO-LOCK WHERE
                   bDayCampaign.Brand = gcBrand AND
                   bDayCampaign.DCEvent = bServiceLimit.GroupCode AND
                   LOOKUP(bDayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0:

             /* Only Subs. level postpaid bundles */
             IF bServiceLimit.GroupCode BEGINS {&DSS} OR
                LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 OR
                CAN-FIND(FIRST ttSub WHERE
                               ttSub.MsSeq    = bMServiceLimit.MsSeq AND
                               ttSub.BundleId = bDayCampaign.DCEvent)
             THEN NEXT.

             ASSIGN ldeBundleAmt = (bMServiceLimit.InclAmt * 1024 * 1024)
                    llFullMonth  = FALSE.

             /* If ongoing bundle termination request except BTC Request   */
             /* OR NOT FIRST month then reduce data amount from total DSS  */
             /* limit  because bundle will be charged as full month amt    */
             IF bMServiceLimit.FromTS < ldPeriodFrom THEN
                llFullMonth = TRUE.
             /* First month always usage based */
             ELSE IF LOOKUP(bDayCampaign.DCEvent,
                            lcFirstMonthUsageBasedBundles) > 0 THEN .
             ELSE IF
                CAN-FIND(FIRST MSRequest WHERE
                               MSRequest.MsSeq = bMServiceLimit.MsSeq AND
                               MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                               MsRequest.ActStamp = ldPeriodTo AND
                               MsRequest.ReqCParam3 = bDayCampaign.DCEvent AND
                               MsRequest.ReqSource <> {&REQUEST_SOURCE_BTC} AND
                               LOOKUP(STRING(MsRequest.ReqStat),"4,99") = 0
                               USE-INDEX MsSeq) OR
                CAN-FIND(FIRST MSRequest WHERE
                               MSRequest.MsSeq = bMServiceLimit.MsSeq AND
                               MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                               MsRequest.ActStamp = ldPeriodTo AND
                               MsRequest.ReqCParam3 = bDayCampaign.DCEvent AND
                               LOOKUP(MsRequest.ReqCParam3,lcBONOContracts) = 0 AND
                               LOOKUP(MsRequest.ReqCParam3,lcIPLContracts)  = 0 AND
                               MsRequest.ReqSource  = {&REQUEST_SOURCE_BTC}
                               USE-INDEX MsSeq)
             THEN llFullMonth = TRUE.

             /* if the data bundle contract is originating from STC/BTC
                then skip first month fee calculation */
             IF llFullMonth = FALSE AND
                bMServiceLimit.FromTs >= ldPeriodFrom THEN
             FOR EACH MsRequest NO-LOCK WHERE
                      MsRequest.MsSeq = ttMsOwner.MsSeq AND
                      MsRequest.ReqType = 8 AND
                      MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                      MsRequest.ActStamp >= ldPeriodFrom AND
                      MsRequest.ActStamp <= ldPeriodTo AND
                      MsRequest.OrigRequest > 0 AND
                      LOOKUP(STRING(MsRequest.ReqStatus),"2,9") > 0
                      USE-INDEX MsSeq,
                FIRST bMsRequest NO-LOCK WHERE
                      bMsRequest.MsRequest = MsRequest.OrigRequest:
                IF (bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR
                    bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}) AND
                   bMsRequest.ActStamp = ldPeriodFrom THEN DO:
                   llFullMonth = TRUE.
                   LEAVE.
                END.
             END.

             /* if the data bundle contract is terminated from STC/BTC
                in middle of month then charge based on last month prorated */
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
             END. /* IF llFullMonth THEN DO: */

             /* If we have to charge full amount for mentioned subs. */
             /* then just return bundle fee amount                   */
             IF llFullMonth AND
                ttMsOwner.MsSeq = iiMsSeq AND
                bDayCampaign.DCEvent = icBundleId THEN DO:
                /* for each used because there might exist  
                   same type of fixed fee in the past */
                FOR EACH  FixedFee NO-LOCK USE-INDEX HostTable WHERE
                          FixedFee.Brand     = gcBrand AND
                          FixedFee.HostTable = "MobSub" AND
                          FixedFee.KeyValue  = STRING(ttMsOwner.MsSeq) AND
                          FixedFee.FeeModel  = bDayCampaign.FeeModel AND
                          FixedFee.CalcObj   = bDayCampaign.DCEvent AND
                          FixedFee.InUse     = TRUE AND
                          FixedFee.BegDate  <= ldToDate,
                    FIRST FFItem NO-LOCK WHERE
                          FFItem.FFNum = FixedFee.FFNum AND
                          FFItem.BillPeriod = iiPeriod:
                    odeFeeAmt = FFItem.Amt.
                    LEAVE.
                END. /* FOR FIRST DayCampaign NO-LOCK WHERE*/

                RETURN.
             END. /* IF llFullMonth THEN DO: */

             Create ttSub.
             ASSIGN ttSub.MsSeq           = bMServiceLimit.MsSeq
                    ttSub.CustNum         = ttMsOwner.CustNum
                    ttSub.BundleId        = bServiceLimit.GroupCode
                    ttSub.UsedDSSLimit    = ldeDSSUsage
                    ttSub.BundleLimit     = ldeBundleAmt
                    ttSub.BundleLimitInMB = bMServiceLimit.InclAmt
                    ttSub.InclUnit        = bServiceLimit.InclUnit
                    ttSub.Priority        = bDayCampaign.DSSPriority.

         END. /* FOR EACH bMServiceLimit WHERE */
   END. /* FOR EACH ttMsOwner NO-LOCK WHERE */

   FOR EACH ttSub BY ttSub.Priority:

      ASSIGN liCount   = liCount + 1
             odeFeeAmt = 0.

      IF liCount = 1 THEN ldeDSSUsage = ttSub.UsedDSSLimit.

      /* There is no Fixed fee linked with DUB */
      IF ttSub.BundleId = "DUB" AND ldeDSSUsage > 0 THEN DO:
         ldeDSSUsage = (ldeDSSUsage - ttSub.BundleLimit).
         NEXT.
      END. /* IF ttSub.BundleId = "DUB" AND ldeDSSUsage > 0 */

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
                FixedFee.BegDate  <= ldToDate   AND
                FixedFee.EndPer   >= iiPeriod,
          FIRST FMItem NO-LOCK WHERE
                FMItem.Brand     = gcBrand AND
                FMItem.FeeModel  = FixedFee.FeeModel AND
                FMItem.FromDate <= FixedFee.BegDate  AND
                FMItem.ToDate   >= FixedFee.BegDate:

          /* already billed */
          FIND FIRST FFItem OF FixedFee WHERE
                     FFItem.BillPeriod = iiPeriod NO-LOCK NO-ERROR.
          IF NOT AVAILABLE FFItem THEN NEXT.

          /* If fee is billed by Test invoice then display billed amount */
          IF FFItem.Billed THEN DO:
             ASSIGN odeFeeAmt   = FFItem.Amt
                    ldeDSSUsage = (ldeDSSUsage - ttSub.BundleLimit).
             NEXT.
          END. /* IF FFItem.Billed THEN DO: */
          /* % of fee, based on usage */
          IF FMItem.FirstMonthBr = 2 THEN DO:
             IF ldeDSSUsage > 0 THEN
                odeFeeAmt = fCalculateProportionalFee(
                                    (IF ldeDSSUsage < ttSub.BundleLimit
                                     THEN ldeDSSUsage
                                     ELSE ttSub.BundleLimit),
                                    ttSub.InclUnit,
                                    ttSub.BundleLimitInMB,
                                    FixedFee.Amt).
             ELSE odeFeeAmt = 0.
          END. /* IF FMItem.FirstMonthBr = 2 THEN DO: */
          ELSE odeFeeAmt = FFItem.Amt.

          ldeDSSUsage = (ldeDSSUsage - ttSub.BundleLimit).

      END. /* FOR FIRST DayCampaign NO-LOCK WHERE */

      /* Just return amount if it just need to display in RPC */
      IF ttSub.MsSeq = iiMsSeq AND icBundleId = ttSub.BundleId THEN RETURN.
   END. /* FOR EACH ttDSSInfo */

END PROCEDURE.

&ENDIF

