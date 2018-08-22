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
              data200_upsell_count;int;mandatory;Data 200 upsell count
              dss200_upsell_count;int;mandatory;DSS 200 upsell count
              fixed_bundle_limit;double;mandatory;fixed line bundle limit
              fixed_bundle_usage;double;mandatory;fixed line bundle usage
              fixed_bdest_limit;int;mandatory;fixed line bundle destination limit
              fixed_bdest_usage;int;mandatory;fixed line bundle destination usage
              flex_500mb_upsell;int;mandatory;Mobile Data Usage Flex Upsell 500mb
              flex_5gb_upsell;int;mandatory;Mobile Data Usage Flex Upsell 5gb

 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Func/callquery.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{Mm/active_bundle.i}
{Func/upsellbundle.i}
{Func/tarj6.i}
{Func/istc.i}

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.

/* Output parameters */
DEF VAR first_level_struct  AS CHARACTER NO-UNDO.

/* Local variables */
DEF VAR ldGprsTrafficIn            AS DECIMAL INITIAL 0.0 NO-UNDO.
DEF VAR ldGprsTrafficOut           AS DECIMAL INITIAL 0.0 NO-UNDO.
DEF VAR ldbalance                  AS DECIMAL INITIAL 0.0 NO-UNDO.
DEF VAR first_of_month             AS DATE    NO-UNDO.
DEF VAR liPeriod                   AS INT     NO-UNDO.
DEF VAR liDayPeriod                AS INT     NO-UNDO.
DEF VAR tthCDR                     AS HANDLE  NO-UNDO.
DEF VAR liErrorCodeOut             AS INTEGER NO-UNDO.
DEF VAR ldPeriodFrom               AS DEC     NO-UNDO.
DEF VAR ldPeriodTo                 AS DEC     NO-UNDO.
DEF VAR ldaLastDay                 AS DATE    NO-UNDO.
DEF VAR ldeCurrentTS               AS DECIMAL NO-UNDO.
DEF VAR liUpsellCount              AS INT     NO-UNDO.
DEF VAR lcError                    AS CHAR    NO-UNDO.
DEF VAR ldeDSSTotalLimit           AS DEC     NO-UNDO.
DEF VAR ldeDSSDataUsed             AS DEC     NO-UNDO.
DEF VAR llDSSActive                AS LOG     NO-UNDO.
DEF VAR lcDSSBundleId              AS CHAR    NO-UNDO.
DEF VAR ldeDataBundleLimit         AS DEC     NO-UNDO.
DEF VAR ldeDataBundleUsage         AS DEC     NO-UNDO.
DEF VAR ldeVoiceBundleLimit        AS DEC     NO-UNDO.
DEF VAR ldeVoiceBundleUsage        AS DEC     NO-UNDO.
DEF VAR ldeTotalDataBundleLimit    AS DEC     NO-UNDO.
DEF VAR ldeTotalDataBundleUsage    AS DEC     NO-UNDO.
DEF VAR ldeDSSDataBundleUsage      AS DEC     NO-UNDO.
DEF VAR ldeRoamBundleUsage         AS DEC     NO-UNDO.
DEF VAR liRoamUpsellCount          AS INT     NO-UNDO.
DEF VAR liDSSUpsellCount           AS INT     NO-UNDO.
DEF VAR liVoiceBDestLimit          AS INT     NO-UNDO.
DEF VAR liVoiceBDestUsage          AS INT     NO-UNDO.
DEF VAR liFixedBdestLimit          AS INT     NO-UNDO.
DEF VAR liFixedBdestUsage          AS INT     NO-UNDO.
DEF VAR ldeFixedBundleLimit        AS DEC     NO-UNDO.
DEF VAR ldeFixedBundleUsage        AS DEC     NO-UNDO.
DEF VAR ldeIntRoamUsage            AS DEC     NO-UNDO. 
DEF VAR liDialTypes                AS INT     NO-UNDO EXTENT 5.
DEF VAR liLoop                     AS INTEGER NO-UNDO. 
DEF VAR ldaDate                    AS DATE    NO-UNDO. 
DEF VAR liTime                     AS INT     NO-UNDO. 
DEF VAR ldePackageFromTS           AS DECIMAL NO-UNDO.
DEF VAR liUpSellCountDay           AS INT     NO-UNDO.
DEF VAR liGPRSTMRuleSeq            AS INT     NO-UNDO.
DEF VAR ldeTARJ6DailyChargeMonth   AS DEC     NO-UNDO. 
DEF VAR ldeTARJ6DailyChargeDay     AS DEC     NO-UNDO. 
DEF VAR ldeTARJ6UpsellChargeMonth  AS DEC     NO-UNDO. 
DEF VAR ldeTARJ6UpsellChargeDay    AS DEC     NO-UNDO. 
DEF VAR ldeTARJ6DataUsageMonthly   AS DEC     NO-UNDO INIT 0.0. 
DEF VAR ldeTARJ6DataUsageDaily     AS DEC     NO-UNDO INIT 0.0.
DEF VAR lcBONOContracts            AS CHAR    NO-UNDO.
DEF VAR lcAllowedDSS2SubsType      AS CHAR    NO-UNDO.
DEF VAR lcExcludeBundles           AS CHAR    NO-UNDO.
DEF VAR liBonoCount                AS INT     NO-UNDO. 
DEF VAR liDSS2BonoCount            AS INT     NO-UNDO. 
DEF VAR ldePrepDataUsageMonthly    AS DEC     NO-UNDO.
DEF VAR ldePrepVoiceUsageMonthly   AS DEC     NO-UNDO.
DEF VAR ldaPrepRenewal             AS DATE    NO-UNDO. 
DEF VAR ldePrepDataLimit           AS DEC     NO-UNDO.
DEF VAR ldePrepVoiceimit           AS DEC     NO-UNDO.
DEF VAR liPrepRenewal              AS INT     NO-UNDO. 
DEF VAR ldaActDate                 AS DATE    NO-UNDO. 
DEF VAR ldaCDRCollectFrom          AS DATE    NO-UNDO. 
DEF VAR ldaiSTCDate                AS DATE    NO-UNDO.
DEF VAR liiSTCTime                 AS INT     NO-UNDO.
DEF VAR lliSTC                     AS LOG     NO-UNDO.
DEF VAR lcUpSellBundle             AS CHAR    NO-UNDO.
DEF VAR liData200Count             AS INT     NO-UNDO.
DEF VAR liDSS200Count              AS INT     NO-UNDO.
DEF VAR lcData200Bundle            AS CHAR    NO-UNDO.
DEF VAR lcUpsellId                 AS CHAR    NO-UNDO. 
DEF VAR liCount                    AS INT     NO-UNDO.
DEF VAR liRstTime                  AS INT     NO-UNDO. 
DEF VAR ldaRstDate                 AS DATE    NO-UNDO.
DEF VAR lcFlexData500MB            AS CHAR    NO-UNDO. 
DEF VAR lcFlexData5GB              AS CHAR    NO-UNDO. 
DEF VAR liFlexData500MB            AS INT     NO-UNDO. 
DEF VAR liFlexData5GB              AS INT     NO-UNDO. 
DEF VAR lcBundleUpsell             AS CHAR NO-UNDO. 

DEF BUFFER bServiceLimit FOR ServiceLimit.

DEF TEMP-TABLE ttCDR NO-UNDO LIKE MobCDR.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.

piMsSeq = get_pos_int(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO ordercanal MobSub MsSeq piMsSeq}

IF NOT MobSub.PayType THEN
   lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

ASSIGN lcBONOContracts  = fCParamC("BONO_CONTRACTS")
       lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES").

tthCDR = TEMP-TABLE ttCDR:HANDLE.

ASSIGN
   first_of_month = DATE(MONTH(TODAY), 1, YEAR(TODAY))
   ldaLastDay     = Func.Common:mLastDayOfMonth(TODAY)
   liPeriod       = YEAR(TODAY) * 100 + MONTH(TODAY)
   ldPeriodFrom   = Func.Common:mMake2DT(first_of_month,0)
   ldPeriodTo     = Func.Common:mMake2DT(ldaLastDay,86399)
   ldeCurrentTS   = Func.Common:mMakeTS()
   liDayPeriod    = YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY).

first_level_struct = add_struct(response_toplevel_id, "").

IF LOOKUP(MobSub.CliType,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 THEN DO:  
   FOR EACH ServiceLimit NO-LOCK WHERE
            ServiceLimit.GroupCode = MobSub.CliType,
       FIRST MServiceLimit WHERE
             MServiceLimit.MsSeq = Mobsub.MsSeq AND
             MServiceLimit.DialType = ServiceLimit.DialType AND
             MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
             MServiceLimit.EndTS >= ldeCurrentTS NO-LOCK:

         Func.Common:mSplitTS(MServiceLimit.FromTS,OUTPUT ldaActDate, OUTPUT liTime).
                  
         IF ldaActDate >= DATE(MONTH(TODAY),1,YEAR(TODAY)) THEN
            ldaPrepRenewal = ldaActDate.
         ELSE DO:
         
            IF TODAY EQ Func.Common:mLastDayOfMonth(TODAY) AND
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
               
            /* YTS-9086 - TARJ7 does not have voice bundle thus only other prepaid check. 
               TMS Counter reset request used for period start time. */
            RELEASE MsRequest.

            IF LOOKUP(MobSub.CliType,"TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 THEN DO:
               
               FIND FIRST MsRequest NO-LOCK USE-INDEX MsActStamp WHERE
                          MsRequest.MsSeq = MobSub.MsSeq AND
                          MsRequest.ActStamp >= Func.Common:mHMS2TS(ldaPrepRenewal,"00:00:00") AND
                          MsRequest.ActStamp <= Func.Common:mHMS2TS(ldaPrepRenewal,"23:59:59") AND
                          MsRequest.ReqType = {&REQTYPE_SERVICE_CHANGE} AND
                          MsRequest.ReqStatus <= {&REQUEST_STATUS_DONE} AND
                          MsRequest.ReqCParam2 = "LADEL1_PRE_PLUS_RESET"
                          NO-ERROR.
               IF AVAIL MsRequest THEN DO:
                  Func.Common:mSplitTS(MsRequest.ActStamp,OUTPUT ldaRstDate, OUTPUT liRstTime).
                  liPrepRenewal = liRstTime.
               END.
            END.
            IF NOT AVAIL MsRequest THEN DO:
               /* PrepEDR charge moment used for period start time. */
               FIND FIRST PrepEDR NO-LOCK WHERE
                          PrepEDR.MsSeq       = MobSub.Msseq AND
                          PrepEDR.DateST      = ldaPrepRenewal AND
                          PrepEDR.SuccessCode = 1 AND
                          PrepEDR.CLIType     = MobSub.CliType AND
                          PrepEDR.ErrorCode   = 0 NO-ERROR.
               IF AVAIL PrepEDR THEN
                  liPrepRenewal = PrepEDR.TimeStart.
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

ldaCDRCollectFrom = (IF LOOKUP(MobSub.CliType,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 AND
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
      Func.Common:mSplitTS(MsOwner.TsBeg,OUTPUT ldaiSTCDate,OUTPUT liiSTCTime).
      lliSTC = TRUE.
   END.
END.

fMobCDRCollect(INPUT TRIM(STRING(MobSub.PayType,"pre/post")),
               INPUT Syst.Var:gcBrand,
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

FOR EACH ttCDR NO-LOCK USE-INDEX date:

   IF ttCDR.ErrorCode NE 0 THEN NEXT.

   /* Only Package data once TARJ7-13 is activated */
   IF LOOKUP(MobSub.CliType,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 THEN DO:

      IF ttCDR.CLIType EQ MobSub.CLIType AND
         ttCDR.DateSt >= ldaPrepRenewal THEN DO:

         IF ttCDR.DateST NE ldaPrepRenewal OR
            ttCDR.TimeStart >= liPrepRenewal THEN DO:

            IF ttCDR.EventType EQ "GPRS" THEN DO:
               IF ttCDR.Charge EQ 0 AND
                  ttCDR.Accumulator > 0 THEN DO:
                  ldePrepDataUsageMonthly  = ttCDR.Accumulator.
               END.
            END.
            ELSE IF ttCDR.EventType EQ "CALL" AND
                    LOOKUP(ttCDR.CLIType,"TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 AND
                    ttCDR.Accumulator > 0 THEN DO:  
               ldePrepVoiceUsageMonthly = ttCDR.Accumulator.
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
              DCCli.Brand = Syst.Var:gcBrand AND
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
             DayCampaign.Brand = Syst.Var:gcBrand AND
             DayCampaign.DCEvent = lcDSSBundleId:
      
      lcBundleUpsell  =  fGetDayCampaignUpsells(DayCampaign.DCEvent).           
      
      DO liLoop = 1 TO NUM-ENTRIES(lcBundleUpsell):

         lcUpsellId = ENTRY(liLoop,lcBundleUpsell).

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

/* Return all data/voice bundles limit and usage */
ASSIGN
   liDialTypes[1] = {&DIAL_TYPE_GENERAL}
   liDialTypes[2] = {&DIAL_TYPE_FIXED_VOICE}
   liDialTypes[3] = {&DIAL_TYPE_VOICE}
   liDialTypes[4] = {&DIAL_TYPE_GPRS}
   liDialTypes[5] = {&DIAL_TYPE_FIXED_VOICE_BDEST}. 

DO liLoop = 1 TO 5:
   FOR EACH MServiceLimit WHERE
            MServiceLimit.MsSeq   = MobSub.MsSeq AND
            MServiceLimit.DialType = liDialTypes[liLoop] AND
            MServiceLimit.FromTS <= ldPeriodTo   AND
            MServiceLimit.EndTS  >= ldeCurrentTS NO-LOCK,
      FIRST ServiceLimit NO-LOCK WHERE
            ServiceLimit.SLSeq = MServiceLimit.SLSeq,
      FIRST DayCampaign WHERE
            DayCampaign.Brand = Syst.Var:gcBrand AND
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
               CASE DayCampaign.DCEvent:
                  WHEN "DATA200_UPSELL" THEN 
                     lcData200Bundle= DayCampaign.DCEvent.
                  WHEN "FLEX_500MB_UPSELL" THEN 
                     lcFlexData500MB = DayCampaign.DCEvent.
                  WHEN "FLEX_5GB_UPSELL" THEN 
                     lcFlexData5GB = DayCampaign.DCEvent.
                  OTHERWISE 
                     lcUpSellBundle = DayCampaign.DCEvent.
               END CASE.
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

      ELSE IF MServiceLimit.DialType = {&DIAL_TYPE_VOICE} OR
              MServiceLimit.DialType = {&DIAL_TYPE_FIXED_VOICE}  THEN DO:
         /* All Voice bundle + UPSELL limits */
         IF MServiceLimit.DialType = {&DIAL_TYPE_VOICE} AND
            DayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO:
            FIND FIRST MServiceLPool WHERE
                       MserviceLPool.MsSeq   = MServiceLimit.MsSeq  AND
                       MserviceLPool.SLSeq   = MServiceLimit.SLSeq  AND
                       MserviceLPool.EndTS  >= ldPeriodFrom         AND
                       MserviceLPool.FromTS <= ldPeriodTo NO-LOCK NO-ERROR.
            IF AVAILABLE MserviceLPool THEN
               ldeVoiceBundleLimit = ldeVoiceBundleLimit +
                                     MserviceLPool.LimitAmt.
         END. /* IF bDayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO: */
         ELSE IF MServiceLimit.DialType = {&DIAL_TYPE_FIXED_VOICE} THEN
            ldeFixedBundleLimit = ldeFixedBundleLimit +
                                     MServiceLimit.InclAmt.
         ELSE ldeVoiceBundleLimit = ldeVoiceBundleLimit +
                                    MServiceLimit.InclAmt.

         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.MsSeq  = MServiceLimit.MsSeq  AND
                    ServiceLCounter.SlSeq  = MServiceLimit.SlSeq  AND
                    ServiceLCounter.Period = liPeriod NO-LOCK NO-ERROR.
         IF AVAILABLE ServiceLCounter THEN DO:
            IF MServiceLimit.DialType = {&DIAL_TYPE_FIXED_VOICE} THEN
               ldeFixedBundleUsage = ldeFixedBundleUsage + 
                                  (ServiceLCounter.Amt / 60).
            ELSE ldeVoiceBundleUsage = ldeVoiceBundleUsage + 
                                  (ServiceLCounter.Amt / 60).
         END.
      END. /* IF MServiceLimit.DialType = {&DIAL_TYPE_VOICE} THEN DO: */
      
      /*YDR-2284 removed the condition*/
      /* Return Voice BDestination limit/usage */
      IF (ServiceLimit.DialType = {&DIAL_TYPE_VOICE} AND
          ServiceLimit.BDestLimit > 0) OR
         ServiceLimit.DialType = {&DIAL_TYPE_GENERAL} OR 
         ServiceLimit.DialType = {&DIAL_TYPE_FIXED_VOICE_BDEST} THEN DO:
         IF ServiceLimit.DialType = {&DIAL_TYPE_VOICE} THEN
            liVoiceBDestLimit = ServiceLimit.BDestLimit.
         ELSE IF ServiceLimit.DialType = {&DIAL_TYPE_FIXED_VOICE_BDEST} THEN
            liFixedBdestLimit = INT(ServiceLimit.InclAmt).
         ELSE
            liVoiceBDestLimit = INT(ServiceLimit.InclAmt).  

         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.MsSeq  = MServiceLimit.MsSeq AND
                    ServiceLCounter.SLSeq  = MServiceLimit.SlSeq AND
                    ServiceLCounter.Period = liPeriod NO-LOCK NO-ERROR.
         IF AVAIL ServiceLCounter THEN DO:
            IF ServiceLimit.DialType = {&DIAL_TYPE_VOICE} THEN
               liVoiceBDestUsage = ServiceLCounter.limit.
            ELSE IF ServiceLimit.DialType = {&DIAL_TYPE_FIXED_VOICE_BDEST} THEN
               liFixedBdestUsage = ServiceLCounter.limit.
            ELSE
               liVoiceBDestUsage = INT(ServiceLCounter.Amt).
         END. /* IF AVAIL ServiceLCounter THEN DO: */
      END.
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
IF lcFlexData500MB > "" THEN
   liFlexData500MB = fGetUpSellCount(lcFlexData500MB,
                                     MobSub.MsSeq,
                                     MobSub.Custnum,
                                     OUTPUT lcError).
IF lcFlexData5GB > "" THEN
   liFlexData5GB = fGetUpSellCount(lcFlexData5GB,
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
ELSE IF LOOKUP(MobSub.CLIType,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 THEN
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
add_double(first_level_struct, "fixed_bundle_limit", ldeFixedBundleLimit).
add_double(first_level_struct, "fixed_bundle_usage", ldeFixedBundleUsage).
add_int(first_level_struct,    "fixed_bdest_limit", liFixedBdestLimit).
add_int(first_level_struct,    "fixed_bdest_usage", liFixedBdestUsage).
add_int(first_level_struct,    "flex_500mb_upsell", liFlexData500MB).
add_int(first_level_struct,    "flex_5gb_upsell", liFlexData5GB).

FINALLY:
   EMPTY TEMP-TABLE ttCDR.
   EMPTY TEMP-TABLE ttMsOwner.
   IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.
   END.

