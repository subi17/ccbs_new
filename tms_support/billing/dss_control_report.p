/* ----------------------------------------------------------------------
  MODULE .......: dss_control_report.p
  TASK .........: Generate DSS Control Report
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 20.10.11
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
gcBrand = "1".
katun   = "Qvantel".
{Func/cparam2.i}
{Func/timestamp.i}
{Mm/dss_bundle_first_month_fee.i}

DEF VAR ldPeriodFrom    AS DEC  NO-UNDO.
DEF VAR ldPeriodTo      AS DEC  NO-UNDO.
DEF VAR ldaFromDate     AS DATE NO-UNDO.
DEF VAR ldaToDate       AS DATE NO-UNDO.
DEF VAR lcPeriod        AS CHAR NO-UNDO FORMAT "X(6)".
DEF VAR lcDelimiter     AS CHAR NO-UNDO.
DEF VAR lcErrOutputFile AS CHAR NO-UNDO.
DEF VAR lcCLIList       AS CHAR NO-UNDO.
DEF VAR lcOutputFile    AS CHAR NO-UNDO FORMAT "X(256)" VIEW-AS FILL-IN SIZE 45 BY 1.
DEF VAR liPeriod        AS INT  NO-UNDO.
DEF VAR ldeTotalBundleLimit AS DEC NO-UNDO.
DEF VAR ldeTotalDSSLimit    AS DEC NO-UNDO.
DEF VAR ldeTotalBundleUsage AS DEC NO-UNDO.
DEF VAR ldeTotalDSSUsage    AS DEC NO-UNDO.
DEF VAR lcDSSFromTS         AS CHAR NO-UNDO.
DEF VAR liTotalPostSubs     AS INT  NO-UNDO.
DEF VAR lcALLHSDPABDest     AS CHAR NO-UNDO.
DEF VAR lcMobSubActTS       AS CHAR NO-UNDO.
DEF VAR lcMobSubEndTS       AS CHAR NO-UNDO.
DEF VAR lcBundleFromTS      AS CHAR NO-UNDO.
DEF VAR lcBundleEndTS       AS CHAR NO-UNDO.
DEF VAR lcReportingMode     AS CHAR NO-UNDO INIT "Normal".

DEF STREAM s_out.
DEF STREAM s_error.

lcDelimiter = CHR(9).

DEF TEMP-TABLE ttDSSInfo NO-UNDO
   FIELD CustNum          AS INT
   FIELD MsSeq            AS INT
   FIELD CLI              AS CHAR
   FIELD CLIType          AS CHAR
   FIELD SubActTS         AS DEC
   FIELD SubEndTS         AS DEC
   FIELD BundleId         AS CHAR
   FIELD BundleStatus     AS CHAR
   FIELD BundleLimit      AS DEC
   FIELD BundleLimitinMB  AS DEC
   FIELD BundleFee        AS DEC
   FIELD BundleFromTS     AS DEC
   FIELD BundleEndTS      AS DEC
   FIELD BeforeUsageCounter AS DEC
   FIELD AfterUsageCounter  AS DEC
   FIELD BeforeUsageCalls AS DEC
   FIELD AfterUsageCalls  AS DEC
   FIELD BundleFeeCalc    AS LOG
   FIELD DataAllocated    AS DEC
   FIELD BilledBundleFee  AS DEC
   FIELD InclUnit         AS INT
   FIELD Priority         AS INT
   INDEX MsSeqBun MsSeq BundleId
   INDEX CustNum CustNum.

FORM
   SKIP
   "Enter Billing Period (eg: YYYYMM):" lcPeriod SKIP
   "Reporting Mode (eg: Full/Normal) :" lcReportingMode SKIP
   "Enter Output File Path:" lcOutputFile
   WITH OVERLAY CENTERED ROW 10 TITLE " DSS Control Report " NO-LABELS
   FRAME finvrow.

UPDATE lcPeriod
       lcReportingMode
       lcOutputFile with FRAME finvrow.

IF lcPeriod = "" THEN DO:
   MESSAGE "Please Enter Billing Period" VIEW-AS ALERT-BOX.
   RETURN.
END.

IF lcReportingMode = "" THEN DO:
   MESSAGE "Please Enter Reporting Mode" VIEW-AS ALERT-BOX.
   RETURN.
END.

IF lcOutputFile = "" THEN DO:
   MESSAGE "Please Enter Output File Path" VIEW-AS ALERT-BOX.
   RETURN.
END. /* IF lcOutputFile = "" THEN DO: */

ASSIGN liPeriod = INT(lcPeriod)
       lcErrOutputFile = REPLACE(lcOutputFile,".xls","") + "_err.xls".

/******** Main start ********/

ASSIGN
   ldaFromDate    = fInt2Date(liPeriod,1)
   ldaToDate      = fInt2Date(liPeriod,2)
   ldPeriodFrom   = fMake2Dt(ldaFromDate,0)
   ldPeriodTo     = fMake2Dt(ldaToDate,86399)
   lcALLHSDPABDest = fCParamC("ALL_HSDPA_BDEST").

RUN pGetAllCustomersSubscriptions.

OUTPUT STREAM s_out TO VALUE(lcOutputFile).

OUTPUT STREAM s_error TO VALUE(lcErrOutputFile).

/* HEADER */
PUT STREAM s_out UNFORMATTED
    "CustNum"           lcDelimiter
    "CLI"               lcDelimiter
    "Sub.Id"            lcDelimiter
    "Sub.ActTS"         lcDelimiter
    "Sub.EndTS"         lcDelimiter
    "Sub.Type"          lcDelimiter
    "Bundle"            lcDelimiter
    "FromTS"            lcDelimiter
    "EndTS"             lcDelimiter
    "Status"            lcDelimiter
    "Limit(MB)"         lcDelimiter
    "Before Usage Counter(MB)" lcDelimiter
    "Before Usage Calls(MB)"   lcDelimiter
    "After Usage Counter(MB)"  lcDelimiter
    "After Usage Calls(MB)"    lcDelimiter
    "Alloc.(MB)"        lcDelimiter
    "Priority"          lcDelimiter
    "Invoiced Fee"      lcDelimiter
    "Possible Fee"      SKIP.

/* HEADER */
PUT STREAM s_error UNFORMATTED
    "CustNum"           lcDelimiter
    "CLI"               lcDelimiter
    "DSS ActStamp"      lcDelimiter
    "DSS Limit/Usage/Invoiced Fee"     lcDelimiter
    "Bundles Limit/Usage/Possible Fee" lcDelimiter
    "Error Case"        lcDelimiter
    "Remark"            SKIP.

FOR EACH ttDSSInfo NO-LOCK BREAK BY ttDSSInfo.CustNum:

   IF FIRST-OF(ttDSSInfo.CustNum) THEN
      ASSIGN ldeTotalDSSLimit    = 0
             ldeTotalBundleLimit = 0
             ldeTotalDSSUsage    = 0
             ldeTotalBundleUsage = 0
             lcCLIList           = ""
             liTotalPostSubs     = 0.

   ASSIGN lcMobSubActTS  = fTS2HMS(ttDSSInfo.SubActTS)
          lcMobSubEndTS  = fTS2HMS(ttDSSInfo.SubEndTS)
          lcBundleFromTS = fTS2HMS(ttDSSInfo.BundleFromTS).

     IF ttDSSInfo.BundleEndTS = 99999999.99999 THEN
        lcBundleEndTS  = fTS2HMS(20491231.86399).
     ELSE
        lcBundleEndTS  = fTS2HMS(ttDSSInfo.BundleEndTS).

   IF ttDSSInfo.BundleId = {&DSS} THEN
      ASSIGN ldeTotalDSSLimit = ttDSSInfo.BundleLimitinMB
             ldeTotalDSSUsage = ttDSSInfo.AfterUsageCounter
             lcDSSFromTS      = lcBundleFromTS.
   ELSE IF NOT (ttDSSInfo.BundleId BEGINS {&DSS} + "_UPSELL") THEN
      ldeTotalBundleLimit = ldeTotalBundleLimit + ttDSSInfo.BundleLimitinMB.

   IF NOT (ttDSSInfo.BundleId BEGINS {&DSS} + "_UPSELL") THEN DO:
      IF LOOKUP(ttDSSInfo.CLI,lcCLIList) = 0 THEN DO:
         IF ttDSSInfo.BundleId <> {&DSS} THEN
            ldeTotalBundleUsage = ldeTotalBundleUsage +
                                  ttDSSInfo.BeforeUsageCounter +
                                  ttDSSInfo.AfterUsageCalls.
         liTotalPostSubs = liTotalPostSubs + 1.
      END. /* IF LOOKUP(ttDSSInfo.CLI,lcCLIList) = 0 THEN DO: */
      lcCLIList = lcCLIList + "," + ttDSSInfo.CLI.
   END. /* IF NOT (ttDSSInfo.BundleId BEGINS {&DSS} + "_UPSELL") THEN DO: */

   IF LAST-OF(ttDSSInfo.CustNum) THEN DO:
      IF liTotalPostSubs < 2 THEN
         PUT STREAM s_error UNFORMATTED
             STRING(ttDSSInfo.CustNum)         lcDelimiter
             ""                                lcDelimiter
             lcDSSFromTS                       lcDelimiter
             STRING(ldeTotalDSSLimit)          lcDelimiter
             STRING(ldeTotalBundleLimit)       lcDelimiter
             "Yes"                             lcDelimiter
             "CRITICAL ERROR-Not enough postpaid subs." SKIP.

      IF ldeTotalDSSLimit <> ldeTotalBundleLimit THEN
         PUT STREAM s_error UNFORMATTED
             STRING(ttDSSInfo.CustNum)         lcDelimiter
             ""                                lcDelimiter
             lcDSSFromTS                       lcDelimiter
             STRING(ldeTotalDSSLimit)          lcDelimiter
             STRING(ldeTotalBundleLimit)       lcDelimiter
             "Yes"                             lcDelimiter
             "Mismatch DSS Limit"              SKIP.
      /* Validate Usage if Reporting Mode is full */
      IF lcReportingMode = "Full" AND
         ROUND(ldeTotalDSSUsage,2) <> ROUND(ldeTotalBundleUsage,2) AND
         (ROUND(ldeTotalBundleUsage,2) - ROUND(ldeTotalDSSUsage,2)) >= 15 AND
         ldeTotalBundleUsage < ldeTotalDSSLimit THEN DO:
         PUT STREAM s_error UNFORMATTED
             STRING(ttDSSInfo.CustNum)         lcDelimiter
             ""                                lcDelimiter
             lcDSSFromTS                       lcDelimiter
             STRING(ldeTotalDSSUsage)          lcDelimiter
             STRING(ldeTotalBundleUsage)       lcDelimiter
             "Yes"                             lcDelimiter
             "Mismatch DSS Usage"              SKIP.
      END. /* IF round(ldeTotalDSSUsage,2) <> round(ldeTotalBundleUsage,2) */
   END. /* IF LAST-OF(ttDSSInfo.CustNum) THEN DO: */

   IF ttDSSInfo.BilledBundleFee > 0 AND
      ROUND(ttDSSInfo.BilledBundleFee,2) <> ROUND(ttDSSInfo.BundleFee,2) THEN
      PUT STREAM s_error UNFORMATTED
       STRING(ttDSSInfo.CustNum)         lcDelimiter
       ttDSSInfo.CLI                     lcDelimiter
       lcDSSFromTS                       lcDelimiter
       STRING(ttDSSInfo.BilledBundleFee) lcDelimiter
       STRING(ttDSSInfo.BundleFee)       lcDelimiter
       "Yes"                             lcDelimiter
       "Mismatch Bundle Fee"             SKIP.

   PUT STREAM s_out UNFORMATTED
       STRING(ttDSSInfo.CustNum)         lcDelimiter
       ttDSSInfo.CLI                     lcDelimiter
       STRING(ttDSSInfo.MsSeq)           lcDelimiter
       lcMobSubActTS                     lcDelimiter
       lcMobSubEndTS                     lcDelimiter
       ttDSSInfo.CLIType                 lcDelimiter
       ttDSSInfo.BundleId                lcDelimiter
       lcBundleFromTS                    lcDelimiter
       lcBundleENDTS                     lcDelimiter
       ttDSSInfo.BundleStatus            lcDelimiter
       STRING(ttDSSInfo.BundleLimitinMB) lcDelimiter
       STRING(ttDSSInfo.BeforeUsageCounter) lcDelimiter
       STRING(ttDSSInfo.BeforeUsageCalls)   lcDelimiter
       STRING(ttDSSInfo.AfterUsageCounter)  lcDelimiter
       STRING(ttDSSInfo.AfterUsageCalls)    lcDelimiter
       STRING(ttDSSInfo.DataAllocated)   lcDelimiter
       STRING(ttDSSInfo.Priority)        lcDelimiter
       STRING(ttDSSInfo.BilledBundleFee) lcDelimiter
       STRING(ttDSSInfo.BundleFee)       SKIP.
END. /* FOR EACH ttDSSInfo NO-LOCK: */

OUTPUT STREAM s_out CLOSE.
OUTPUT STREAM s_error CLOSE.

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
   DEF VAR ldeDataAllocated         AS DEC  NO-UNDO.
   DEF VAR llFullMonth              AS LOG  NO-UNDO.
   DEF VAR ldeFeeAmt                AS DEC  NO-UNDO.
   DEF VAR ldeBundleInclAmt         AS DEC  NO-UNDO.
   DEF VAR ldFFItemStartDate        AS DATE NO-UNDO.
   DEF VAR liFFItemStartTime        AS INT  NO-UNDO.
   DEF VAR liErrorCodeOut           AS INT  NO-UNDO.
   DEF VAR ldeTotalDSSCallUsage     AS DEC  NO-UNDO.
   DEF VAR ldeTotalOtherBundleCallUsage AS DEC  NO-UNDO.

   DEF BUFFER bFixedFee             FOR FixedFee.
   DEF BUFFER bMServiceLimit        FOR MServiceLimit.
   DEF BUFFER bServiceLimit         FOR ServiceLimit.
   DEF BUFFER bDayCampaign          FOR DayCampaign.
   DEF BUFFER bMsRequest            FOR MsRequest.

   /* Check wheather customer is linked with DSS service or not */
   IF NOT fIsDSSActive(INPUT iiInvCust,
                       INPUT ldPeriodTo) THEN RETURN.

   ldeDSSUsage = fGetDSSUsage(INPUT iiInvCust,
                              INPUT ldaFromDate,
                              OUTPUT ldeDSSLimit).

   FOR EACH MsOwner NO-LOCK WHERE
            MsOwner.InvCust  = iiInvCust     AND
            MsOwner.TsBegin <= ldPeriodTo    AND
            MsOwner.TsEnd   >= ldPeriodFrom  AND
            NOT MsOwner.PayType
      BREAK BY MsOwner.MsSeq:
      IF FIRST-OF(MsOwner.MsSeq) THEN DO:

         STATUS DEFAULT MsOwner.CLI.

         FOR EACH bMServiceLimit WHERE
                  bMServiceLimit.MsSeq   = MsOwner.MsSeq  AND
                  bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
                  bMServiceLimit.FromTS <= ldPeriodTo     AND
                  bMServiceLimit.EndTS  >= ldPeriodTo NO-LOCK,
             FIRST bServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
                   bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
             FIRST bDayCampaign NO-LOCK WHERE
                   bDayCampaign.Brand = gcBrand AND
                   bDayCampaign.DCEvent = bServiceLimit.GroupCode:

             IF CAN-FIND(FIRST ttDSSInfo WHERE
                               ttDSSInfo.MsSeq    = bMServiceLimit.MsSeq AND
                               ttDSSInfo.BundleId = bServiceLimit.GroupCode) OR
                LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0
             THEN NEXT.

             ldeBundleInclAmt = bMServiceLimit.InclAmt.

             IF bDayCampaign.DCEvent = {&DSS} THEN DO:
                FIND FIRST MServiceLPool WHERE
                           MserviceLPool.MsSeq   = bMServiceLimit.MsSeq   AND
                           MserviceLPool.SLSeq   = bMServiceLimit.SLSeq   AND
                           MserviceLPool.FromTS <= ldPeriodTo             AND
                           MserviceLPool.EndTS  >= ldPeriodTo NO-LOCK NO-ERROR.
                IF AVAILABLE MserviceLPool THEN
                   ldeBundleInclAmt = MserviceLPool.LimitAmt.
             END. /* IF bDayCampaign.DCType = {&DSS} THEN DO: */

             ASSIGN ldeBundleAmt = (ldeBundleInclAmt * 1024 * 1024)
                    llFullMonth  = FALSE
                    ldeDataAllocated = 0.

             Create ttDSSInfo.
             ASSIGN ttDSSInfo.MsSeq           = bMServiceLimit.MsSeq
                    ttDSSInfo.CustNum         = MsOwner.CustNum
                    ttDSSInfo.CLIType         = MsOwner.CLIType
                    ttDSSInfo.CLI             = MsOwner.CLI
                    ttDSSInfo.BundleId        = bServiceLimit.GroupCode
                    ttDSSInfo.BundleFromTS    = bMServiceLimit.FromTS
                    ttDSSInfo.BundleEndTS     = bMServiceLimit.EndTS
                    ttDSSInfo.BundleLimit     = ldeBundleAmt
                    ttDSSInfo.BundleLimitInMB = ldeBundleInclAmt
                    ttDSSInfo.InclUnit        = bServiceLimit.InclUnit
                    ttDSSInfo.BundleStatus    = "Active".

             /* DSS/Upsell usage */
             IF bServiceLimit.GroupCode BEGINS {&DSS} THEN DO:
                FIND FIRST ServiceLCounter WHERE
                           ServiceLCounter.CustNum = bMServiceLimit.CustNum AND
                           ServiceLCounter.SLSeq   = bServiceLimit.SlSeq  AND
                           ServiceLCounter.Period  = liPeriod NO-LOCK NO-ERROR.
                IF AVAIL ServiceLCounter THEN
                   ttDSSInfo.AfterUsageCounter = (ServiceLCounter.Amt / 1024 / 1024).
             END. /* IF bServiceLimit.GroupCode BEGINS {&DSS} THEN DO: */
             /* Get Subs. level usage before DSS activation */
             ELSE DO:
                FIND FIRST ServiceLCounter WHERE
                           ServiceLCounter.MsSeq  = bMServiceLimit.MsSeq AND
                           ServiceLCounter.SLSeq  = bServiceLimit.SlSeq  AND
                           ServiceLCounter.Period = liPeriod NO-LOCK NO-ERROR.
                IF AVAIL ServiceLCounter THEN
                   ttDSSInfo.BeforeUsageCounter = (ServiceLCounter.Amt / 1024 / 1024).

                IF bMServiceLimit.InclAmt < ttDSSInfo.BeforeUsageCounter THEN
                   ttDSSInfo.BeforeUsageCounter = bMServiceLimit.InclAmt.
             END. /* ELSE DO: */

             IF MsOwner.TsEnd < ldPeriodTo THEN DO:
                FIND FIRST TermMobSub WHERE
                           TermMobSub.MsSeq = MsOwner.MsSeq
                     NO-LOCK NO-ERROR.
                IF AVAILABLE TermMobSub THEN
                   ASSIGN ttDSSInfo.SubActTS = TermMobSub.ActivationTS
                          ttDSSInfo.SubEndTS = MsOwner.TsEnd.
             END. /* IF MsOwner.TsEnd < ldPeriodTo THEN DO: */
             ELSE DO:
                FIND FIRST MobSub WHERE
                           MobSub.MsSeq = MsOwner.MsSeq
                     NO-LOCK NO-ERROR.
                IF AVAILABLE MobSub THEN
                   ttDSSInfo.SubActTS = MobSub.ActivationTS.
             END. /* ELSE DO: */

             /* Don't execute Upsells further */
             IF bDayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO:
                FIND FIRST MServiceLPool WHERE
                           MserviceLPool.MsSeq   = bMServiceLimit.MsSeq   AND
                           MserviceLPool.SLSeq   = bMServiceLimit.SLSeq   AND
                           MserviceLPool.FromTS <= ldPeriodTo             AND
                           MserviceLPool.EndTS  >= ldPeriodFrom NO-LOCK NO-ERROR.
                IF AVAILABLE MserviceLPool THEN
                   ttDSSInfo.BundleLimitInMB = MserviceLPool.LimitAmt.
                NEXT.
             END. /* IF bDayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO: */

             /* If ongoing bundle termination request except BTC Request   */
             /* OR NOT FIRST month then reduce data amount from total DSS  */
             /* limit  because bundle will be charged as full month amt    */
             IF CAN-FIND(FIRST MSRequest WHERE
                         MSRequest.MsSeq = bMServiceLimit.MsSeq AND
                         MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                         MsRequest.ActStamp = ldPeriodTo AND
                         MsRequest.ReqCParam3 = bDayCampaign.DCEvent AND
                         MsRequest.ReqSource <> {&REQUEST_SOURCE_BTC} AND
                         LOOKUP(STRING(MsRequest.ReqStat),
                                {&REQ_INACTIVE_STATUSES}) = 0
                         USE-INDEX MsSeq) OR
                (bServiceLimit.GroupCode = {&DSS} AND
                 fOngoingDSSTerm(bMServiceLimit.CustNum,ldPeriodTo)) THEN DO:
                /* First month always usage based */
                IF LOOKUP(bDayCampaign.DCEvent,
                          lcFirstMonthUsageBasedBundles) > 0 THEN
                   llFullMonth = FALSE.
                ELSE llFullMonth = TRUE.
                ttDSSInfo.BundleStatus = "Termination Ongoing".
             END. /* IF CAN-FIND(FIRST MSRequest WHERE */

             IF bMServiceLimit.FromTS < ldPeriodFrom THEN
                llFullMonth = TRUE.

             /* if the data bundle contract is originating from STC/BTC
                then skip first month fee calculation */
             IF llFullMonth = FALSE AND
                bMServiceLimit.FromTs >= ldPeriodFrom THEN
             FOR EACH MsRequest NO-LOCK WHERE
                      MsRequest.MsSeq = MsOwner.MsSeq AND
                      MsRequest.ReqType = 8 AND
                      MsRequest.ReqCParam3 = bServiceLimit.GroupCode AND
                      MsRequest.ActStamp >= ldPeriodFrom AND
                      MsRequest.ActStamp <= ldPeriodTo AND
                      MsRequest.OrigRequest > 0 AND
                      LOOKUP(STRING(MsRequest.ReqStatus),"2,9") > 0
                      USE-INDEX MsSeq,
                FIRST bMsRequest NO-LOCK WHERE
                      bMsRequest.MsRequest = MsRequest.OrigRequest:
                IF bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR 
                   bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE} THEN DO:
                   llFullMonth = TRUE.
                   LEAVE.
                END.
             END.

             /* First month always usage based */
             IF llFullMonth = FALSE AND
                bMServiceLimit.FromTs >= ldPeriodFrom AND
                LOOKUP(bDayCampaign.DCEvent,lcFirstMonthUsageBasedBundles) > 0
             THEN DO:
                IF bMServiceLimit.EndTS = ldPeriodTo THEN
                   ttDSSInfo.BundleStatus = "Terminated".
             END. /* IF llFullMonth = FALSE AND */
             /* If Termination request is part of BTC */
             ELSE IF llFullMonth = FALSE AND
                bMServiceLimit.FromTs >= ldPeriodFrom AND
                CAN-FIND(FIRST MSRequest WHERE
                         MSRequest.MsSeq = bMServiceLimit.MsSeq AND
                         MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                         MsRequest.ActStamp = ldPeriodTo AND
                         MsRequest.ReqCParam3 = bDayCampaign.DCEvent AND
                        (LOOKUP(MsRequest.ReqCParam3,lcBONOContracts) > 0  OR
                         LOOKUP(MsRequest.ReqCParam3,lcIPLContracts)  > 0) AND
                         MsRequest.ReqSource = {&REQUEST_SOURCE_BTC}
                         USE-INDEX MsSeq) THEN
                ASSIGN llFullMonth = FALSE
                       ttDSSInfo.BundleStatus = "BTC-First Month Usage Based".
             ELSE IF bMServiceLimit.EndTS = ldPeriodTo THEN
                ASSIGN llFullMonth = TRUE
                       ttDSSInfo.BundleStatus = "Terminated".

             IF llFullMonth AND bDayCampaign.DCEvent <> {&DSS} THEN DO:
                IF ldeDSSUsage >= ldeBundleAmt THEN
                   ASSIGN ldeDSSUsage = (ldeDSSUsage - ldeBundleAmt)
                          ldeDataAllocated = ldeBundleAmt.
                ELSE
                  ASSIGN ldeDataAllocated = ldeDSSUsage
                         ldeDSSUsage = 0.
             END. /* IF CAN-FIND(FIRST MSRequest WHERE */

             ASSIGN ttDSSInfo.DataAllocated   = (ldeDataAllocated / 1024 / 1024)
                    ttDSSInfo.BundleFeeCalc   = (NOT llFullMonth)
                    ttDSSInfo.Priority = fGetBundlePriority(bServiceLimit.GroupCode).

             IF llFullMonth THEN DO:
                /* for each used because there might exist  
                   same type of fixed fee in the past */
                FOR EACH FixedFee NO-LOCK USE-INDEX HostTable WHERE
                          FixedFee.Brand     = gcBrand AND
                          FixedFee.HostTable = "MobSub" AND
                          FixedFee.KeyValue  = STRING(ttDSSInfo.MsSeq) AND
                          FixedFee.FeeModel  = bDayCampaign.FeeModel AND
                          FixedFee.CalcObj   = bDayCampaign.DCEvent AND
                          FixedFee.InUse     = TRUE AND
                          FixedFee.BegDate  <= ldaToDate,
                    FIRST FFItem NO-LOCK WHERE
                          FFItem.FFNum = FixedFee.FFNum AND
                          FFItem.BillPeriod = liPeriod:

                    ASSIGN ttDSSInfo.BundleFee = FFItem.Amt
                           ttDSSInfo.BilledBundleFee = FFItem.Amt.
                    LEAVE.
                END. /* FOR FIRST DayCampaign NO-LOCK WHERE*/
             END. /* IF llFullMonth THEN DO: */
         END. /* FOR EACH bMServiceLimit WHERE */

         /* Make entry for a subscription without any bundle */
         IF NOT CAN-FIND(FIRST ttDSSInfo WHERE
                               ttDSSInfo.MsSeq = MsOwner.MsSeq) THEN DO:
             CREATE ttDSSInfo.
             ASSIGN ttDSSInfo.MsSeq           = MsOwner.MsSeq
                    ttDSSInfo.CustNum         = MsOwner.CustNum
                    ttDSSInfo.CLIType         = MsOwner.CLIType
                    ttDSSInfo.CLI             = MsOwner.CLI
                    ttDSSInfo.BundleFeeCalc   = False
                    ttDSSInfo.BundleStatus    = "N/A".
         END. /* IF NOT CAN-FIND(FIRST ttDSSInfo WHERE */

         /* If reporting mode is full then only count usage from MobCDR */
         IF lcReportingMode = "Full" THEN DO:
            ASSIGN ldeTotalDSSCallUsage = 0
                   ldeTotalOtherBundleCallUsage = 0.

            FOR EACH MobCDR WHERE
                     MobCDR.CLI     = MsOwner.CLI AND
                     MobCDR.DateSt >= ldaFromDate AND
                     MobCDR.DateSt <= ldaToDate   NO-LOCK:
               IF MobCDR.EventType = "GPRS" AND MobCDR.ErrorCode = 0 THEN DO:
                  IF MobCDR.bDest = "GPRSDSS_A" AND MobCDR.DCEvent = {&DSS} THEN
                     ldeTotalDSSCallUsage = ldeTotalDSSCallUsage +
                                            MobCDR.DataIn + MobCDR.DataOut.
                  ELSE IF LOOKUP(MobCDR.bDest,lcALLHSDPABDest) > 0 THEN
                     ldeTotalOtherBundleCallUsage = ldeTotalOtherBundleCallUsage +
                                                    MobCDR.DataIn + MobCDR.DataOut.
               END. /* IF MobCDR.EventType = "GPRS" AND MobCDR.ErrorCode = 0 */
            END. /* FOR EACH MobCDR WHERE */

            FOR EACH ttDSSInfo WHERE
                     ttDSSInfo.MsSeq = MsOwner.MsSeq:
               IF ttDSSInfo.BundleId BEGINS "DSS" THEN
                  ttDSSInfo.AfterUsageCalls = ttDSSInfo.AfterUsageCalls +
                                              (ldeTotalDSSCallUsage / 1024 / 1024).
               ELSE
                  ASSIGN
                     ttDSSInfo.AfterUsageCalls = ttDSSInfo.AfterUsageCalls +
                                     (ldeTotalDSSCallUsage / 1024 / 1024)
                     ttDSSInfo.BeforeUsageCalls = ttDSSInfo.BeforeUsageCalls +
                                     (ldeTotalOtherBundleCallUsage / 1024 / 1024).
            END. /* FOR EACH ttDSSInfo WHERE */
         END. /* IF lcReportingMode = "Full" THEN DO: */
      END. /* IF FIRST-OF(MsOwner.MsSeq) THEN DO: */
   END. /* FOR EACH MsOwner NO-LOCK WHERE */


   /* Calculate first month fee */
   FOR EACH ttDSSInfo WHERE
            ttDSSInfo.CustNum       = iiInvCust AND
            ttDSSInfo.BundleFeeCalc = TRUE
            BY ttDSSInfo.Priority:

      ASSIGN ldeFeeAmt = 0
             ldeDataAllocated = 0.

      FOR FIRST DayCampaign NO-LOCK WHERE
                DayCampaign.Brand   = gcBrand AND
                DayCampaign.DCEvent = ttDSSInfo.BundleId,
          FIRST FixedFee NO-LOCK USE-INDEX HostTable WHERE
                FixedFee.Brand     = gcBrand AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue  = STRING(ttDSSInfo.MsSeq) AND
                FixedFee.FeeModel  = DayCampaign.FeeModel AND
                FixedFee.CalcObj   = DayCampaign.DCEvent AND
                FixedFee.InUse     = TRUE AND
                FixedFee.BegDate  >= ldaFromDate AND
                FixedFee.BegDate  <= ldaToDate   AND
                FixedFee.EndPer   >= liPeriod,
          FIRST FMItem NO-LOCK WHERE
                FMItem.Brand     = gcBrand AND
                FMItem.FeeModel  = FixedFee.FeeModel AND
                FMItem.FromDate <= FixedFee.BegDate  AND
                FMItem.ToDate   >= FixedFee.BegDate:

          /* already billed */
          FIND FIRST FFItem OF FixedFee WHERE
                     FFItem.BillPeriod = liPeriod NO-LOCK NO-ERROR.
          IF NOT AVAILABLE FFItem THEN NEXT.

          /* If fee is billed by Test invoice then display billed amount */
          IF FFItem.Billed OR FixedFee.ServiceLimitGroup BEGINS "PMF:" THEN
             ttDSSInfo.BilledBundleFee = FFItem.Amt.

          IF ttDSSInfo.BundleId = {&DSS} THEN DO:
             fSplitTS(DEC(FFItem.Concerns[1]), OUTPUT ldFFItemStartDate,
                      OUTPUT liFFItemStartTime).
             ldeFeeAmt = fCalcProportionalFeeDSS(INPUT FixedFee.Amt,
                                                 INPUT ldFFItemStartDate,
                                                 INPUT ldaFromDate,
                                                 INPUT ldaToDate).
          END. /* IF ttDSSInfo.BundleId = {&DSS} THEN DO: */
          /* % of fee, based on usage */
          ELSE IF FMItem.FirstMonthBr = 2 THEN DO:
             IF ldeDSSUsage > 0 THEN
                ldeFeeAmt = fCalculateProportionalFee(
                                    (IF ldeDSSUsage < ttDSSInfo.BundleLimit
                                     THEN ldeDSSUsage
                                     ELSE ttDSSInfo.BundleLimit),
                                    ttDSSInfo.InclUnit,
                                    ttDSSInfo.BundleLimitInMB,
                                    FixedFee.Amt).
             ELSE ldeFeeAmt = 0.
          END. /* ELSE IF FMItem.FirstMonthBr = 2 THEN DO: */
          ELSE ldeFeeAmt = FFItem.Amt.
      END. /* FOR FIRST DayCampaign NO-LOCK WHERE */

      IF ttDSSInfo.BundleId <> {&DSS} AND ldeDSSUsage > 0 THEN DO:
         IF ldeDSSUsage > ttDSSInfo.BundleLimit THEN 
            ttDSSInfo.DataAllocated = (ttDSSInfo.BundleLimit / 1024 / 1024).
         ELSE ttDSSInfo.DataAllocated = (ldeDSSUsage / 1024 / 1024).

         ldeDSSUsage = (ldeDSSUsage - ttDSSInfo.BundleLimit).
      END. /* IF ttDSSInfo.BundleId <> {&DSS} AND ldeDSSUsage > 0 THEN DO: */

      /* Update Bundle Fee amount */
      ttDSSInfo.BundleFee = ldeFeeAmt.
   END. /* FOR EACH ttDSSInfo */

END PROCEDURE.
