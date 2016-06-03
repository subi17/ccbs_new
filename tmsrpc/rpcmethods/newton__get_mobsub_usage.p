/**
 * Subscription usage for the current month.
 *
 * @input     msseq;int;mandatory;Mobile subscription identifier
 * @output    gprs_in;double;mandatory;monthly incoming GPRS usage in MB (for old MiYoigo)
              gprs_out;double;mandatory;monthly outgoing GPRS usage in MB (for old MiYoigo)
              dss_limit;double;mandatory;monthly DSS limit in MB
              dss_usage;double;mandatory;monthly DSS usage in MB
              dss_upsell;int;mandatory;monthly count of DSS upsells
              dss2_bono_count;int;optional;number of Bono contracts linked to DSS2 (returned id DSS2 active)
              data_bundle_limit;double;mandatory;monthly HSPA data limit in MB
              data_bundle_usage;double;mandatory;monthly HSPA data usage in MB
              data_bundle_upsell;int;optional;count of monthly upsells
              bono_count;int;mandatory;number of active Bono contracts (0/1)
              data_bundle_upsell_daily;int;optional;count of daily TARJ6_UPSELL upsells
              data_bundle_usage_day;double;optional;used and mandatory with TARJ6
              roam_data_bundle_upsell;int;mandatory;count of monthly int. roaming data upsells
              roam_data_bundle_usage;double;mandatory;daily int. roaming data bundle usage in MB
              roam_data_row_usage;double;mandatory;monthly int. roaming usage (outside eu) in MB
              voice_bundle_limit;double;mandatory;monthly voice bundle limit in minutes
              voice_bundle_usage;double;mandatory;monthly voice bundle usage in minutes
              voice_bdest_limit;int;mandatory;monthly voice bundle bdest limit
              voice_bdest_usage;int;mandatory;count of monthly used bdests
 */
{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{commpaa.i}
gcBrand = "1".
{callquery.i}
{tmsconst.i}
{timestamp.i}
{cparam2.i}
{fbundle.i}
{upsellbundle.i}
{tarj6.i}
{istc.i}

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.

/* Output parameters */
DEF VAR first_level_struct  AS CHARACTER NO-UNDO.

/* Local variables */
DEF VAR ldGprsTrafficIn  AS DECIMAL INITIAL 0.0 NO-UNDO.
DEF VAR ldGprsTrafficOut AS DECIMAL INITIAL 0.0 NO-UNDO.
DEF VAR ldbalance        AS DECIMAL INITIAL 0.0 NO-UNDO.
DEF VAR first_of_month   AS DATE NO-UNDO.
DEF VAR liPeriod         AS INT NO-UNDO.
DEF VAR liDayPeriod      AS INT NO-UNDO.
DEF VAR tthCDR           AS HANDLE  NO-UNDO.
DEF VAR liErrorCodeOut   AS INTEGER NO-UNDO.
DEF VAR ldPeriodFrom     AS DEC     NO-UNDO.
DEF VAR ldPeriodTo       AS DEC     NO-UNDO.
DEF VAR ldaLastDay       AS DATE    NO-UNDO.
DEF VAR ldeCurrentTS     AS DECIMAL NO-UNDO.
DEF VAR liUpsellCount    AS INT     NO-UNDO.
DEF VAR lcError          AS CHAR    NO-UNDO.
DEF VAR ldeDSSTotalLimit AS DEC     NO-UNDO.
DEF VAR ldeDSSDataUsed   AS DEC     NO-UNDO.
DEF VAR llDSSActive      AS LOG     NO-UNDO.
DEF VAR lcDSSBundleId    AS CHAR    NO-UNDO.
DEF VAR ldeDataBundleLimit  AS DEC  NO-UNDO.
DEF VAR ldeDataBundleUsage  AS DEC  NO-UNDO.
DEF VAR ldeVoiceBundleLimit AS DEC  NO-UNDO.
DEF VAR ldeVoiceBundleUsage AS DEC  NO-UNDO.
DEF VAR ldeTotalDataBundleLimit AS DEC  NO-UNDO.
DEF VAR ldeTotalDataBundleUsage AS DEC  NO-UNDO.
DEF VAR ldeDSSDataBundleUsage   AS DEC  NO-UNDO.
DEF VAR ldeRoamBundleUsage AS DEC  NO-UNDO.
DEF VAR liRoamUpsellCount AS INT     NO-UNDO.
DEF VAR liDSSUpsellCount        AS INT  NO-UNDO.
DEF VAR liVoiceBDestLimit       AS INT  NO-UNDO.
DEF VAR liVoiceBDestUsage       AS INT  NO-UNDO.
DEF VAR ldeIntRoamUsage AS DEC NO-UNDO. 
DEF VAR liDialTypes AS INT NO-UNDO EXTENT 3.
DEF VAR liLoop AS INTEGER NO-UNDO. 
DEF VAR ldaDate AS DATE NO-UNDO. 
DEF VAR liTime AS INT NO-UNDO. 
DEF VAR ldePackageFromTS AS DECIMAL NO-UNDO.
DEF VAR liUpSellCountDay AS INT NO-UNDO.
DEF VAR liGPRSTMRuleSeq AS INT NO-UNDO.
DEF VAR ldeTARJ6DailyChargeMonth AS DEC NO-UNDO. 
DEF VAR ldeTARJ6DailyChargeDay AS DEC NO-UNDO. 
DEF VAR ldeTARJ6UpsellChargeMonth AS DEC NO-UNDO. 
DEF VAR ldeTARJ6UpsellChargeDay AS DEC NO-UNDO. 
DEF VAR ldeTARJ6DataUsageMonthly AS DEC NO-UNDO INIT 0.0. 
DEF VAR ldeTARJ6DataUsageDaily AS DEC NO-UNDO INIT 0.0.
DEF VAR lcBONOContracts       AS CHAR NO-UNDO.
DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.
DEF VAR lcExcludeBundles      AS CHAR NO-UNDO.
DEF VAR liBonoCount AS INT NO-UNDO. 
DEF VAR liDSS2BonoCount AS INT NO-UNDO. 
DEF VAR ldePrepDataUsageMonthly AS DEC NO-UNDO.
DEF VAR ldePrepVoiceUsageMonthly AS DEC NO-UNDO.
DEF VAR ldaPrepRenewal AS DATE NO-UNDO. 
DEF VAR ldePrepDataLimit AS DEC NO-UNDO.
DEF VAR ldePrepVoiceimit AS DEC NO-UNDO.
DEF VAR liPrepRenewal AS INT NO-UNDO. 
DEF VAR ldaActDate AS DATE NO-UNDO. 
DEF VAR ldaCDRCollectFrom AS DATE NO-UNDO. 
DEF VAR ldaiSTCDate AS DATE NO-UNDO.
DEF VAR liiSTCTime  AS INT  NO-UNDO.
DEF VAR lliSTC      AS LOG  NO-UNDO.
DEF VAR lcUpSellBundle AS CHAR NO-UNDO.
DEF VAR liData200Count AS INT NO-UNDO.
DEF VAR liDSS200Count AS INT NO-UNDO.
DEF VAR lcData200Bundle AS CHAR NO-UNDO.
DEF VAR lcUpsellId AS CHAR NO-UNDO. 
DEF VAR liCount AS INT NO-UNDO.
DEF VAR llAccumulatorFound AS LOG NO-UNDO.
DEF BUFFER bServiceLimit FOR ServiceLimit.

DEF TEMP-TABLE ttCDR NO-UNDO LIKE MobCDR.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.

piMsSeq = get_pos_int(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST MobSub NO-LOCK WHERE
           MobSub.MsSeq = piMsSeq NO-ERROR.
IF NOT AVAIL MobSub THEN
   RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

IF NOT MobSub.PayType THEN
   lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

ASSIGN lcBONOContracts  = fCParamC("BONO_CONTRACTS")
       lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES").

tthCDR = TEMP-TABLE ttCDR:HANDLE.

ASSIGN
   first_of_month = DATE(MONTH(TODAY), 1, YEAR(TODAY))
   ldaLastDay     = fLastDayOfMonth(TODAY)
   liPeriod       = YEAR(TODAY) * 100 + MONTH(TODAY)
   ldPeriodFrom   = fMake2Dt(first_of_month,0)
   ldPeriodTo     = fMake2Dt(ldaLastDay,86399)
   ldeCurrentTS   = fMakeTS()
   liDayPeriod    = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY).

first_level_struct = add_struct(response_toplevel_id, "").
   
IF MobSub.CliType = "TARJ7" OR MobSub.CliType = "TARJ9" THEN DO:
   FOR EACH ServiceLimit NO-LOCK WHERE
            ServiceLimit.GroupCode = MobSub.CliType,
       FIRST MServiceLimit WHERE
             MServiceLimit.MsSeq = Mobsub.MsSeq AND
             MServiceLimit.DialType = ServiceLimit.DialType AND
             MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
             MServiceLimit.EndTS >= ldeCurrentTS NO-LOCK:

         fSplitTS(MServiceLimit.FromTS,OUTPUT ldaActDate, OUTPUT liTime).
                  
         IF ldaActDate >= DATE(MONTH(TODAY),1,YEAR(TODAY)) THEN
            ldaPrepRenewal = ldaActDate.
         ELSE DO:
         
            IF TODAY EQ fLastDayOfMonth(TODAY) AND
               DAY(ldaActDate) >= DAY(TODAY) THEN ldaPrepRenewal = TODAY.
            ELSE IF DAY(ldaActDate) > DAY(TODAY)THEN DO:
               ldaPrepRenewal = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
               IF DAY(ldaPrepRenewal) > DAY(ldaActDate) THEN
                  ldaPrepRenewal = DATE(MONTH(ldaPrepRenewal),
                                         DAY(ldaActDate),
                                         YEAR(ldaPrepRenewal)).
            END.
            ELSE ldaPrepRenewal = DATE(MONTH(TODAY),
                                       DAY(ldaActDate),
                                       YEAR(TODAY)).
               
            IF DAY(ldaPrepRenewal) EQ DAY(ldaActDate) OR
               ldaPrepRenewal EQ fLastDayOfMonth(ldaPrepRenewal) THEN DO:
               FIND FIRST PrepEDR NO-LOCK WHERE
                          PrepEDR.MsSeq       = MobSub.Msseq AND
                          PrepEDR.DateST      = ldaPrepRenewal AND
                          PrepEDR.SuccessCode = 1 AND
                          PrepEDR.CLIType     = MobSub.CliType AND
                          PrepEDR.ErrorCode   = 0 NO-ERROR.
               /* YTS-9086 - Added counter reset check */
               IF AVAIL PrepEDR AND
                  ldaPrepRenewal NE ldaActDate THEN DO:
                  FIND FIRST MsRequest NO-LOCK WHERE
                             MsRequest.MsSeq = PrepEDR.MsSeq AND
                             MsRequest.ReqType = {&REQTYPE_SERVICE_CHANGE} AND
                             MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
                             MsRequest.ActStamp >= fHMS2TS(TODAY,"00:00:00") AND
                             MsRequest.ReqCParam2 = "LADEL1_PRE_PLUS_RESET"
                             NO-ERROR.
                  IF AVAIL MsRequest THEN
                     liPrepRenewal = MsRequest.ActStamp.
                  ELSE
                     liPrepRenewal = PrepEDR.TimeStart.
               END.
            END.
         END.

         IF MServiceLimit.DialType = {&DIAL_TYPE_GPRS} THEN DO:
            
            ldePrepDataLimit = MServiceLimit.InclAmt.

            /* Check if there is any active prepaid upsell */
            liUpsellCount = fGetUpSellCount(INPUT "TARJ7_UPSELL",
                                            INPUT Mobsub.MsSeq,
                                            INPUT Mobsub.Custnum,
                                            OUTPUT lcError).
            IF liUpsellCount > 0 THEN DO:
               FIND FIRST bServiceLimit NO-LOCK WHERE
                          bServiceLimit.GroupCode = "TARJ7_UPSELL" NO-ERROR.
               IF AVAIL bServiceLimit THEN
                  ldePrepDataLimit = ldePrepDataLimit +
                                       (bServiceLimit.InclAmt * liUpsellCount).
            END. /* IF liUpsellCount > 0 THEN DO: */
         END.
            
         IF MServiceLimit.DialType = {&DIAL_TYPE_VOICE} THEN
            ldePrepVoiceimit = MServiceLimit.InclAmt.

   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */
END. /* ELSE IF MobSub.CliType = "TARJ7" THEN DO: */

EMPTY TEMP-TABLE ttCDR.

ldaCDRCollectFrom = (IF (MobSub.CliType EQ "TARJ7" OR 
                         MobSub.CliType EQ "TARJ9") AND
                         ldaPrepRenewal < first_of_month THEN ldaPrepRenewal
                     ELSE first_of_month).

IF NOT MobSub.PayType THEN DO:
   FIND FIRST MSOwner WHERE
              MSOwner.MsSeq    = MobSub.MsSeq AND
              MsOwner.TsBegin <= ldPeriodTo    AND
              MsOwner.TsEnd   >= ldPeriodFrom  AND
              MsOwner.PayType  = FALSE         AND
              MsOwner.CLIEvent BEGINS "iS" NO-LOCK NO-ERROR.
   IF AVAIL MSOwner AND MsOwner.TsBeg >= ldPeriodFrom AND
      MsOwner.TsBeg <= ldPeriodTo THEN DO:
      fSplitTS(MsOwner.TsBeg,OUTPUT ldaiSTCDate,OUTPUT liiSTCTime).
      lliSTC = TRUE.
   END.
END.

fMobCDRCollect(INPUT TRIM(STRING(MobSub.PayType,"pre/post")),
               INPUT gcBrand,
               INPUT "rpc",
               INPUT ldaCDRCollectFrom,
               INPUT TODAY,
               INPUT 0,
               INPUT "",
               INPUT MobSub.CLI,
               INPUT 0,
               INPUT 0,
               INPUT "",
               INPUT "",
               INPUT "",
               INPUT 0,
               INPUT-OUTPUT liErrorCodeOut,
               INPUT-OUTPUT tthCDR).

/* for solving situation at first month when there is not yeat received
   new CDRs with accumulator field. This can be removed one month after 
   YDR-1965 deployment */
IF MobSub.CLIType EQ "TARJ9" AND CAN-FIND(FIRST ttCDR WHERE 
   ttCDR.CLIType EQ "TARJ9" AND ttCDR.EventType EQ "CALL" AND 
   ttCDR.Accumulator > 0) THEN
   llAccumulatorFound = TRUE.
   
FOR EACH ttCDR NO-LOCK USE-INDEX date:

   IF ttCDR.ErrorCode NE 0 THEN NEXT.

   /* Only Package data once TARJ7 and TARJ9 is activated */
   IF MobSub.CLIType EQ "TARJ7" OR
      MobSub.CLIType EQ "TARJ9" THEN DO:

      IF ((ttCDR.CLIType EQ "TARJ7" AND MobSub.CLIType EQ "TARJ7") OR
         (ttCDR.CLIType EQ "TARJ9" AND MobSub.CLIType EQ "TARJ9")) AND
         ttCDR.DateSt >= ldaPrepRenewal THEN DO:

         IF ttCDR.DateST NE ldaPrepRenewal OR
            ttCDR.TimeStart >= liPrepRenewal THEN DO:

            IF ttCDR.EventType EQ "GPRS" THEN DO:
               IF ttCDR.Charge EQ 0 THEN DO:
                  ldePrepDataUsageMonthly  = ldePrepDataUsageMonthly +
                                             ttCDR.DataIn + ttCDR.DataOut.
               END.
            END.
            ELSE IF ttCDR.EventType EQ "CALL" AND 
                    ttCDR.CLIType EQ "TARJ9" THEN DO:  
               IF llAccumulatorFound THEN DO:
                  IF ttCDR.Accumulator > 0 THEN
                     ldePrepVoiceUsageMonthly = ttCDR.Accumulator.
               END.      
               ELSE DO:
                  IF ttCDR.Charge EQ 0 AND
                  LOOKUP(ttCDR.GsmBnr,{&YOIGO_FREE_NUMBERS}) = 0 THEN DO:
                     ldePrepVoiceUsageMonthly = ldePrepVoiceUsageMonthly + 
                                                ttCDR.BillDur.
                     IF ldePrepVoiceUsageMonthly > 1200 THEN
                        ldePrepVoiceUsageMonthly = 1200.
                  END.
               END.
            END.
         END.
      END.

      IF ttCDR.DateSt < first_of_month THEN NEXT.
   END.

   IF ttCDR.EventType = "GPRS" THEN DO:
   
      /* international GPRS Roaming */
      IF ttCDR.RateCCN = 90 THEN DO:
         IF ttCDR.BDest = "ROAMGPRS" THEN
            ldeIntRoamUsage = ldeIntRoamUsage + ttCDR.datain + ttCDR.dataout.
      END.
      /* national data */
      ELSE DO:
         IF MobSub.CLIType EQ "TARJ6" AND ttCDR.CLIType EQ "TARJ6" THEN
            ASSIGN ldeTARJ6DataUsageMonthly = ldeTARJ6DataUsageMonthly + 
                                              ttCDR.DataIn + ttCDR.DataOut
                   ldeTARJ6DataUsageDaily   = ldeTARJ6DataUsageDaily +
                                              ttCDR.DataIn + ttCDR.DataOut
                                              WHEN ttCDR.DateST EQ TODAY.

         IF lliSTC AND ttCDR.DateSt < ldaiSTCDate THEN NEXT.

         ASSIGN 
            ldGprsTrafficIn  = ldGprsTrafficIn + ttCDR.DataIn
            ldGprsTrafficOut = ldGprsTrafficOut + ttCDR.DataOut.

          /* Only track high speed DSS calls */
          IF ttCDR.bDest = "GPRSDSS_A" OR ttCDR.bDest = "GPRSDSS2_A" THEN
             ldeDSSDataBundleUsage = ldeDSSDataBundleUsage +
                                     ttCDR.DataIn + ttCDR.DataOut.
      END.
   END.
END.

/* prepaid data is in kiloBytes, convert to Bytes */
IF MobSub.PayType THEN ASSIGN
   ldGprsTrafficIn = ldGprsTrafficIn * 1024
   ldGprsTrafficOut = ldGprsTrafficOut * 1024
   ldeIntRoamUsage = ldeIntRoamUsage / 1024
   ldeTARJ6DataUsageMonthly = ldeTARJ6DataUsageMonthly / 1024
   ldeTARJ6DataUsageDaily = ldeTARJ6DataUsageDaily / 1024.
ELSE ASSIGN
   ldeIntRoamUsage = ldeIntRoamUsage / 1024 / 1024.

/* yoigo-yoigo counter */
IF MobSub.CLIType EQ "CONT" THEN DO:
   FIND FIRST DCCli NO-LOCK WHERE
              DCCli.Brand = gcBrand AND
              DCCli.dcevent = "YOIGOYOIGO" AND
              DCCli.msseq = mobsub.msseq AND
              DCCli.validto >= TODAY NO-ERROR.
   IF AVAIL DCCli THEN DO: 
      FIND FIRST DCCounter NO-LOCK WHERE
          DCCounter.msseq = mobsub.msseq AND
          DCCounter.dcdate = TODAY AND
          DCCounter.dcevent = "YOIGOYOIGO" NO-ERROR.
      add_string(
         first_level_struct,
         "yoigo_call_counter",
         (IF AVAIL DCCounter THEN 
            string(int(DCCounter.Amount), "hh:mm:ss") ELSE string(0, "hh:mm:ss"))).
   END.   
END.

/* Return DSS/UPSELL data volume and corresponding usage */
IF NOT MobSub.PayType THEN DO:
   lcDSSBundleId = fGetActiveDSSId(MobSub.CustNum,ldeCurrentTS).
   IF lcDSSBundleId = "DSS2" THEN
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

   IF lcDSSBundleId = {&DSS} OR
      (lcDSSBundleId = "DSS2" AND
       LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0) THEN
   FOR EACH ServiceLimit WHERE
            ServiceLimit.GroupCode BEGINS {&DSS} NO-LOCK,
      FIRST MServiceLimit WHERE
            MServiceLimit.Custnum = Mobsub.Custnum     AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SlSeq   = ServiceLimit.SlSeq AND
            MServiceLimit.FromTS <= ldeCurrentTS       AND
            MServiceLimit.EndTS  >= ldeCurrentTS NO-LOCK:

      llDSSActive = TRUE.

      IF LOOKUP(ServiceLimit.GroupCode,{&DSS_BUNDLES}) > 0 THEN DO:
         FIND FIRST MServiceLPool WHERE
                    MserviceLPool.Custnum = MServiceLimit.Custnum AND
                    MserviceLPool.SLSeq   = MServiceLimit.SLSeq   AND
                    MserviceLPool.FromTS <= ldPeriodTo            AND
                    MserviceLPool.EndTS  >= ldPeriodTo NO-LOCK NO-ERROR.
         IF AVAILABLE MserviceLPool THEN
            ldeDSSTotalLimit = ldeDSSTotalLimit + MserviceLPool.LimitAmt.
         ELSE
            ldeDSSTotalLimit = ldeDSSTotalLimit + MServiceLimit.InclAmt.
      END. /* IF ServiceLimit.GroupCode = {&DSS} THEN DO: */
      ELSE DO:
         FIND FIRST MServiceLPool WHERE
                    MserviceLPool.Custnum = MServiceLimit.Custnum AND
                    MserviceLPool.SLSeq   = MServiceLimit.SLSeq   AND
                    MserviceLPool.EndTS  >= ldeCurrentTS          AND
                    MserviceLPool.FromTS <= ldeCurrentTS NO-LOCK NO-ERROR.
         IF AVAILABLE MserviceLPool THEN
            ldeDSSTotalLimit = ldeDSSTotalLimit + MserviceLPool.LimitAmt.
      END. /* ELSE DO: */

      FIND FIRST ServiceLCounter WHERE
                 ServiceLCounter.Custnum = MServiceLimit.Custnum AND
                 ServiceLCounter.SlSeq   = MServiceLimit.SlSeq   AND
                 ServiceLCounter.Period  = liPeriod NO-LOCK NO-ERROR.
      IF AVAILABLE ServiceLCounter THEN
         ldeDSSDataUsed = ldeDSSDataUsed  + ServiceLCounter.amt.
   END. /* FOR EACH ServiceLimit WHERE */
   
   ASSIGN ldeDSSDataUsed = (ldeDSSDataUsed / 1024 / 1024)
          ldeDSSDataBundleUsage = (ldeDSSDataBundleUsage / 1024 / 1024).

   /* Check if there is any active DSS upsell */
   IF llDSSActive THEN
   FOR FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand = gcBrand AND
             DayCampaign.DCEvent = lcDSSBundleId:
      
      DO liLoop = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell):

         lcUpsellId = ENTRY(liLoop,DayCampaign.BundleUpsell).

         liCount = fGetUpSellCount(INPUT lcUpsellId,
                                   INPUT Mobsub.MsSeq,
                                   INPUT Mobsub.Custnum,
                                   OUTPUT lcError).
         IF lcUpsellId EQ "DSS200_UPSELL" THEN
            liDSS200Count = liCount.
         ELSE liDssUpsellCount = liDssUpsellCount + liCount.
      END.
      IF lcDSSBundleId EQ "DSS2" THEN DO:

         fGetMsOwnerTempTable(MobSub.Custnum,first_of_month,
                              ldaLastDay,FALSE,FALSE).

         FOR EACH ttMsOwner BREAK BY ttMsOwner.MsSeq:

            IF FIRST-OF(ttMsOwner.MsSeq) THEN DO:
               IF LOOKUP(ttMsOwner.CLIType,lcAllowedDSS2SubsType) = 0 THEN NEXT.
         
               FOR EACH MServiceLimit WHERE
                        MServiceLimit.MsSeq    = ttMsOwner.MsSeq AND
                        MServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
                        MServiceLimit.FromTS  <= ttMsOwner.PeriodTo AND
                        MServiceLimit.EndTS   >= ttMsOwner.PeriodFrom NO-LOCK,
                  FIRST ServiceLimit NO-LOCK WHERE
                        ServiceLimit.SLSeq = MServiceLimit.SLSeq:

                  IF LOOKUP(ServiceLimit.GroupCode,lcBONOContracts) > 0 THEN
                     liDSS2BonoCount = liDSS2BonoCount + 1.
               END. /* FOR EACH MServiceLimit WHERE */
            END. /* IF FIRST-OF(ttMsOwner.MsSeq) THEN DO: */
         END. /* FOR EACH ttMsOwner BREAK BY ttMsOwner.MsSeq */
      END. /* IF lcDSSBundleId EQ "DSS2" THEN DO: */
   END. /* IF llDSSActive THEN DO: */

END. /* IF NOT MobSub.PayType THEN DO: */

FOR FIRST ServiceLimit NO-LOCK WHERE
          ServiceLimit.GroupCode = "HSPA_ROAM_EU",
    EACH MServiceLimit NO-LOCK WHERE
         MServiceLimit.MsSeq   = MobSub.MsSeq AND
         MServiceLimit.DialType = ServiceLimit.DialType AND 
         MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
         MServiceLimit.EndTS  >= ldeCurrentTS AND
         MServiceLimit.FromTs <= ldeCurrentTS AND
         MServiceLimit.EndTS >= MServiceLimit.FromTS:

   FIND FIRST ServiceLCounter NO-LOCK WHERE
              ServiceLCounter.MsSeq  = MServiceLimit.MsSeq  AND
              ServiceLCounter.Period = liDayPeriod AND
              ServiceLCounter.SlSeq  = MServiceLimit.SlSeq  AND
              ServiceLCounter.MSID   = MServiceLimit.MSID NO-ERROR.
   IF AVAIL ServiceLCounter THEN
       ldeRoamBundleUsage = ldeRoamBundleUsage + 
                            (ServiceLCounter.Amt / 1024 / 1024).
   
   /* filter out base bundle (activated from consumption). YTS-7114 */
   IF INT(MServiceLimit.FromTs) EQ MServiceLimit.FromTS THEN NEXT.
    
   liRoamUpsellCount = liRoamUpsellCount + 1.

END.

/* Return all data/voice bundles limit and usage */
ASSIGN
   liDialTypes[1] = 0
   liDialTypes[2] = 4
   liDialTypes[3] = 7.

DO liLoop = 1 TO 3:
   FOR EACH MServiceLimit WHERE
            MServiceLimit.MsSeq   = MobSub.MsSeq AND
            MServiceLimit.DialType = liDialTypes[liLoop] AND
            MServiceLimit.FromTS <= ldPeriodTo   AND
            MServiceLimit.EndTS  >= ldeCurrentTS NO-LOCK,
      FIRST ServiceLimit NO-LOCK WHERE
            ServiceLimit.SLSeq = MServiceLimit.SLSeq,
      FIRST DayCampaign WHERE
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = ServiceLimit.GroupCode NO-LOCK:

      IF LOOKUP(STRING(DayCampaign.DCType),
                {&PERCONTRACT_RATING_PACKAGE}) = 0 AND
         DayCampaign.DCType NE {&DCTYPE_POOL_RATING} THEN NEXT.

      /* Prepaid data limit needs to return */
      IF NOT DayCampaign.DCEvent BEGINS {&PMDUB} THEN DO:
         IF DayCampaign.DCEvent BEGINS {&DSS} OR
            LOOKUP(DayCampaign.DCEvent,lcExcludeBundles) > 0 THEN NEXT.
      END. /* IF NOT DayCampaign.DCEvent BEGINS {&PMDUB} THEN DO: */

      ASSIGN ldeDataBundleLimit = 0
             ldeDataBundleUsage = 0.

      IF MServiceLimit.DialType = {&DIAL_TYPE_GPRS} THEN DO:
         /* Check DSS Upgrade upsell limit */
         FIND FIRST MsRequest NO-LOCK WHERE
                    MsRequest.MsSeq      = MobSub.MsSeq AND
                    MsRequest.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION} AND
                    MsRequest.ReqStatus  = {&REQUEST_STATUS_DONE} AND
                    MsRequest.ActStamp  >= ldPeriodFrom AND
                    MsRequest.ActStamp  <= ldPeriodTo   AND
                    MsRequest.ReqCParam3 = "DSS_UPSELL_UPGRADE" NO-ERROR.
         IF AVAILABLE MsRequest THEN
            ldeTotalDataBundleLimit = ldeTotalDataBundleLimit +
                                       MsRequest.ReqDparam1.

         /* All data bundle + UPSELL limits */
         IF DayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO:
            FIND FIRST MServiceLPool WHERE
                       MserviceLPool.MsSeq   = MServiceLimit.MsSeq  AND
                       MserviceLPool.SLSeq   = MServiceLimit.SLSeq  AND
                       MserviceLPool.EndTS  >= ldPeriodFrom         AND
                       MserviceLPool.FromTS <= ldPeriodTo NO-LOCK NO-ERROR.
            IF AVAILABLE MserviceLPool THEN DO:
               ldeDataBundleLimit = MserviceLPool.LimitAmt.
               ldeTotalDataBundleLimit = ldeTotalDataBundleLimit +
                                         MserviceLPool.LimitAmt.
               IF DayCampaign.DCEvent EQ "DATA200_UPSELL" THEN
                  lcData200Bundle= DayCampaign.DCEvent.
               ELSE   
                  lcUpSellBundle = DayCampaign.DCEvent.
            END.           
         END. /* IF bDayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO: */
         ELSE
            ASSIGN ldeDataBundleLimit = MServiceLimit.InclAmt
                   ldeTotalDataBundleLimit = ldeTotalDataBundleLimit +
                                             MServiceLimit.InclAmt
                   liBonoCount = liBonoCount + 1 WHEN 
                   LOOKUP(DayCampaign.DCEvent,lcBONOContracts) > 0.

         /* YTS-4626 - use FOR EACH mainly because of PMDUB,
            with those it's usual to have more than one counter
            on the same period because of automatic renewal process */
         FOR EACH ServiceLCounter NO-LOCK WHERE
                  ServiceLCounter.MsSeq  = MServiceLimit.MsSeq  AND
                  ServiceLCounter.SlSeq  = MServiceLimit.SlSeq  AND
                  ServiceLCounter.Period = liPeriod:
            ldeTotalDataBundleUsage = ldeTotalDataBundleUsage +
                                      (ServiceLCounter.Amt / 1024 / 1024).
         END. /* IF AVAILABLE ServiceLCounter THEN DO: */
      END. /* IF MServiceLimit.DialType = {&DIAL_TYPE_GPRS} THEN DO: */

      ELSE IF MServiceLimit.DialType = {&DIAL_TYPE_VOICE} THEN DO:
         /* All Voice bundle + UPSELL limits */
         IF DayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO:
            FIND FIRST MServiceLPool WHERE
                       MserviceLPool.MsSeq   = MServiceLimit.MsSeq  AND
                       MserviceLPool.SLSeq   = MServiceLimit.SLSeq  AND
                       MserviceLPool.EndTS  >= ldPeriodFrom         AND
                       MserviceLPool.FromTS <= ldPeriodTo NO-LOCK NO-ERROR.
            IF AVAILABLE MserviceLPool THEN
               ldeVoiceBundleLimit = ldeVoiceBundleLimit +
                                     MserviceLPool.LimitAmt.
         END. /* IF bDayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO: */
         ELSE ldeVoiceBundleLimit = ldeVoiceBundleLimit +
                                    MServiceLimit.InclAmt.

         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.MsSeq  = MServiceLimit.MsSeq  AND
                    ServiceLCounter.SlSeq  = MServiceLimit.SlSeq  AND
                    ServiceLCounter.Period = liPeriod NO-LOCK NO-ERROR.
         IF AVAILABLE ServiceLCounter THEN
            ldeVoiceBundleUsage = ldeVoiceBundleUsage + 
                                  (ServiceLCounter.Amt / 60).
      END. /* IF MServiceLimit.DialType = {&DIAL_TYPE_VOICE} THEN DO: */

      /* Return Voice BDestination limit/usage */
      IF ((ServiceLimit.GroupCode BEGINS "CONTF"       OR
           ServiceLimit.GroupCode = "VOICE100"         OR 
           ServiceLimit.GroupCode = "FREE100MINUTES")  AND
           ServiceLimit.DialType = {&DIAL_TYPE_VOICE}) OR
           ServiceLimit.DialType = 0 THEN DO:
         IF ServiceLimit.DialType = {&DIAL_TYPE_VOICE} THEN
            liVoiceBDestLimit = ServiceLimit.BDestLimit.
         ELSE
            liVoiceBDestLimit = INT(ServiceLimit.InclAmt).

         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.MsSeq  = MServiceLimit.MsSeq AND
                    ServiceLCounter.SLSeq  = MServiceLimit.SlSeq AND
                    ServiceLCounter.Period = liPeriod NO-LOCK NO-ERROR.
         IF AVAIL ServiceLCounter THEN DO:
            IF ServiceLimit.DialType = {&DIAL_TYPE_VOICE} THEN
               liVoiceBDestUsage = ServiceLCounter.limit.
            ELSE
               liVoiceBDestUsage = INT(ServiceLCounter.Amt).
         END. /* IF AVAIL ServiceLCounter THEN DO: */
      END. /* IF (ServiceLimit.GroupCode BEGINS "CONTF" AND */
   END. /* FOR EACH MServiceLimit WHERE */
END. /* DO liDialType = 4 to 7 BY 3 */

/* Count Upsell */
IF lcUpSellBundle > "" THEN
   liUpsellCount = fGetUpSellCount(lcUpSellBundle,
                                   MobSub.MsSeq,
                                   MobSub.Custnum,
                                   OUTPUT lcError).
IF lcData200Bundle > "" THEN
   liData200Count = fGetUpSellCount(lcData200Bundle,
                                   MobSub.MsSeq,
                                   MobSub.Custnum,
                                   OUTPUT lcError).
/*ilkka add 200 countesr*/
IF MobSub.CliType = "TARJ6" THEN DO:
   FOR FIRST ServiceLimit NO-LOCK WHERE
             ServiceLimit.GroupCode = {&TARJ_UPSELL} AND
             ServiceLimit.ValidFrom <= TODAY  AND
             ServiceLimit.ValidTo   >= TODAY.
      liUpsellCount = fGetUpSellCount({&TARJ_UPSELL},
                                      MobSub.MsSeq,
                                      MobSub.Custnum,
                                      OUTPUT lcError).
      /* there can be overlapping bundles, only use the latest one */
      liUpSellCountDay = 0.
      FOR EACH MServiceLimit NO-LOCK WHERE
               MServiceLimit.MsSeq    = piMsSeq AND
               MServiceLimit.DialType = ServiceLimit.DialType AND
               MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
               MServiceLimit.EndTS   >= ldeCurrentTs AND
               MServiceLimit.FromTs  <= ldeCurrentTs:
         liUpSellCountDay = liUpSellCountDay + 1.
      END.

      add_int(first_level_struct, "data_bundle_upsell_daily", liUpSellCountDay).
      ldeTotalDataBundleUsage = 0.

      IF Mobsub.PayType THEN liGPRSTMRuleSeq = fCParamI("TMQueueGPRSPrepaid").
      IF liGPRSTMRuleSeq > 0 THEN DO:
         FIND FIRST TMCounter NO-LOCK WHERE
                    TMCounter.MsSeq     = Mobsub.MsSeq AND
                    TMCounter.TMRuleSeq = liGPRSTMRuleSeq AND
                    TMCounter.Todate    = ldaLastDay NO-ERROR.
         IF AVAIL TMCounter THEN
            ldeTotalDataBundleUsage = TMCounter.Amount.
      END. /* IF liGPRSTMRuleSeq > 0 THEN DO: */
   END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

   fGetTARJ6DataLimitAndCharges
         (INPUT MobSub.MsSeq,
          OUTPUT ldeTotalDataBundleLimit,
          OUTPUT ldeTARJ6UpsellChargeMonth,
          OUTPUT ldeTARJ6UpsellChargeDay,
          OUTPUT ldeTARJ6DailyChargeMonth,
          OUTPUT ldeTARJ6DailyChargeDay).

    add_double(first_level_struct,"data_bundle_usage_day",
               ldeTARJ6DataUsageDaily).
END. /* IF MobSub.CliType = "TARJ6" THEN DO: */
ELSE IF MobSub.CLIType EQ "TARJ7" OR
        MobSub.CLIType EQ "TARJ9" THEN
   ASSIGN
      ldeTotalDataBundleUsage = ldePrepDataUsageMonthly / 1024
      ldeTotalDataBundleLimit = ldePrepDataLimit
      ldeVoiceBundleLimit     = ldePrepVoiceimit
      ldeVoiceBundleUsage     = ldePrepVoiceUsageMonthly / 60.

ELSE IF liBonoCount = 0 AND
   fGetActiveSpecificBundle(Mobsub.MsSeq,ldPeriodFrom,"BONO") > "" THEN
   liBonoCount = 1.

ldeTotalDataBundleUsage = ldeTotalDataBundleUsage + ldeDSSDataBundleUsage.

add_double(first_level_struct, "gprs_in", ldGprsTrafficIn).
add_double(first_level_struct, "gprs_out", ldGprsTrafficOut).
add_double(first_level_struct, "dss_limit", ldeDSSTotalLimit).
add_double(first_level_struct, "dss_usage", ldeDSSDataUsed).
add_int(first_level_struct,    "dss_upsell",liDSSUpsellCount).
add_int(first_level_struct,   "dss2_bono_count", liDSS2BonoCount).
add_double(first_level_struct, "data_bundle_limit",  ldeTotalDataBundleLimit).
add_double(first_level_struct, "data_bundle_usage", ldeTotalDataBundleUsage).
add_int(first_level_struct,    "roam_data_bundle_upsell", liRoamUpsellCount).
add_double(first_level_struct, "roam_data_bundle_usage", ldeRoamBundleUsage).
add_double(first_level_struct, "roam_data_row_usage", ldeIntRoamUsage).
add_double(first_level_struct, "voice_bundle_limit", ldeVoiceBundleLimit).
add_double(first_level_struct, "voice_bundle_usage", ldeVoiceBundleUsage).
add_int(first_level_struct,    "voice_bdest_limit", liVoiceBDestLimit).
add_int(first_level_struct,    "voice_bdest_usage", liVoiceBDestUsage).
add_int(first_level_struct,    "bono_count", liBonoCount).
add_int(first_level_struct,    "data_bundle_upsell", liUpsellCount).
add_int(first_level_struct,    "data200_upsell_count", liData200Count).
add_int(first_level_struct,    "dss200_upsell_count", liDSS200Count).

FINALLY:
   EMPTY TEMP-TABLE ttCDR.
   EMPTY TEMP-TABLE ttMsOwner.
   IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.

