/**
 * A mobsub object
 *
 * @input       msseq;int;mandatory;subscription id
                cli_type;string;mandatory;new subscription type
                tariff_bundle;string;optional;subscription based bundle for new tariff
 * @output      struct with penalties array and warning array;
 * @penalty_struct 
                stc_date;datetime;mandatory;
                penalty_contract;string;optional;(TERM or PAYTERM+TERM)
                penalty_amount;double;optional;penalty amount
                extension;boolean;optional;TRUE if extension is allowed
 */
{xmlrpc/xmlrpc_access.i}

{commpaa.i}
katun = "Newton".
gcBrand = "1".

{tmsconst.i}
{date.i}
{timestamp.i}
{cparam2.i}
{fixedfee.i}
{fbundle.i}
{fcustpl.i}
{penaltyfee.i}
{fctchange.i}
{main_add_lines.i}
{fdss.i}

/* Input parameters */
DEF VAR piMsSeq            AS INT  NO-UNDO.
DEF VAR pcNewCLIType       AS CHAR NO-UNDO.
DEF VAR pcTariffBundle     AS CHAR NO-UNDO.

/* Output parameters */
DEF VAR top_struct         AS CHAR NO-UNDO.
DEF VAR result_array       AS CHAR NO-UNDO.
DEF VAR penalty_array      AS CHAR NO-UNDO.
DEF VAR penalty_struct     AS CHAR NO-UNDO.
DEF VAR sub_struct         AS CHAR NO-UNDO.
DEF VAR warning_array      AS CHAR NO-UNDO.

DEF VAR ldePendingFee      AS DEC  NO-UNDO.
DEF VAR ldePeriodFee       AS DEC  NO-UNDO.
DEF VAR lderesidualFee     AS DEC  NO-UNDO.
DEF VAR ldeTermPendingFee  AS DEC  NO-UNDO EXTENT 2.
DEF VAR ldeTotalPenaltyFee AS DEC  NO-UNDO EXTENT 2.
DEF VAR liCount            AS INT  NO-UNDO.
DEF VAR liTotalPeriods     AS INT  NO-UNDO.
DEF VAR lcPenaltyCode      AS CHAR NO-UNDO.
DEF VAR lcInstallmentCode  AS CHAR NO-UNDO.
DEF VAR lcPriceList        AS CHAR NO-UNDO.
DEF VAR lcOrigBundle       AS CHAR NO-UNDO.
DEF VAR lcOrigCLIType      AS CHAR NO-UNDO.
DEF VAR llRenewal          AS LOG  NO-UNDO.
DEF VAR lcCLIType          AS CHAR NO-UNDO.
DEF VAR lcFinancedInfo AS CHAR NO-UNDO. 
DEF VAR ldaSTCDates AS DATE NO-UNDO EXTENT 2.
DEF VAR liLengthAfterExtension AS INT NO-UNDO EXTENT 2.
DEF VAR liLoop AS INT NO-UNDO. 
DEF VAR ldePenaltyFactor AS DEC NO-UNDO. 
DEF VAR ldtFrom AS DATE NO-UNDO. 
DEF VAR ldtTo AS DATE NO-UNDO. 
DEF VAR liMonths AS INT NO-UNDO. 
DEF VAR lcIPLContracts   AS CHAR NO-UNDO.
DEF VAR lcFLATContracts  AS CHAR NO-UNDO.
DEF VAR lcCONTDContracts AS CHAR NO-UNDO.
DEF VAR lcCONTSContracts AS CHAR NO-UNDO.
DEF VAR lcCONTSFContracts AS CHAR NO-UNDO.
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.
DEF VAR lcFusionTariffs  AS CHAR NO-UNDO.
DEF VAR lliSTCAllowed    AS LOG  NO-UNDO.
DEF VAR lcDSS2PrimarySubsType AS CHAR NO-UNDO. 
DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO. 
DEF VAR llAdditionalLines AS LOG NO-UNDO. 
DEF VAR lcResult AS CHAR NO-UNDO. 
DEF VAR lcBono AS CHAR NO-UNDO. 
DEF VAR lcAllowedBONOSTCContracts AS CHAR NO-UNDO.
DEF VAR ldeNextMonthTS    AS DEC NO-UNDO.
DEF VAR ldeEndTS          AS DEC NO-UNDO.
DEF VAR llDSS1Term        AS LOG NO-UNDO.
DEF VAR llDSS2Term        AS LOG NO-UNDO.
DEF VAR lcError           AS CHAR NO-UNDO.
DEF VAR lcDSSBundleId     AS CHAR NO-UNDO.
DEF VAR lcPostpaidDataBundles  AS CHAR NO-UNDO.
DEF VAR lcDataBundleCLITypes   AS CHAR NO-UNDO.
DEF VAR liFeePeriod       AS INT  NO-UNDO.
DEF VAR liOrderId AS INT NO-UNDO. 

DEF BUFFER bCLIType        FOR CLIType.
DEF BUFFER OldCLIType      FOR CLIType.
DEF BUFFER lbMobSub        FOR MobSub.

IF validate_request(param_toplevel_id, "int,string,string") EQ ? THEN RETURN.
piMsSeq = get_pos_int(param_toplevel_id,"0").
pcNewCLIType = get_string(param_toplevel_id,"1").
pcTariffBundle = get_string(param_toplevel_id,"2").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST MobSub NO-LOCK WHERE
           MobSub.MsSeq = piMsSeq NO-ERROR.
IF NOT AVAILABLE MobSub THEN
   RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

IF pcNewCLIType = "" OR pcNewCLIType = ? THEN
   RETURN appl_err("Invalid New CLIType").

FIND FIRST CLIType WHERE
           CLIType.CLIType = pcNewCLIType NO-LOCK NO-ERROR.
IF NOT AVAILABLE CLIType THEN
   RETURN appl_err(SUBST("New CLIType entry &1 not found", pcNewCLIType)).

lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

IF LOOKUP(pcNewCLIType,lcBundleCLITypes) > 0 THEN DO:
   IF pcTariffBundle = "" THEN
      RETURN appl_err("Missing tariff bundle").

   FIND FIRST CLIType WHERE
              CLIType.CLIType = pcTariffBundle NO-LOCK NO-ERROR.
   IF NOT AVAILABLE CLIType THEN
      RETURN appl_err(SUBST("New CLIType entry &1 not found", pcTariffBundle)).
END.
ELSE pcTariffBundle = "".

ASSIGN
   lcOrigCLIType  = MobSub.CLIType
   ldaSTCDates[1] = TODAY + 1
   ldaSTCDates[2] = fLastDayOfMonth(TODAY) + 1
   ldeNextMonthTS = fMake2DT(ldaSTCDates[2],0)
   ldeEndTS       = fMake2DT(fLastDayOfMonth(TODAY),86399).

IF MobSub.PayType = FALSE THEN DO:
   ASSIGN
      lcIPLContracts        = fCParamC("IPL_CONTRACTS")
      lcCONTDContracts      = fCParamC("CONTD_CONTRACTS")
      lcFLATContracts       = fCParamC("FLAT_CONTRACTS")
      lcCONTSContracts      = fCParamC("CONTS_CONTRACTS")
      lcCONTSFContracts     = fCParamC("CONTSF_CONTRACTS")
      lcFusionTariffs       = fCParamC("FUSION_SUBS_TYPE")
      lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE")
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
      lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS")
      lcDataBundleCLITypes  = fCParamC("DATA_BUNDLE_BASED_CLITYPES").

   lliSTCAllowed = fIsiSTCAllowed(INPUT Mobsub.MsSeq).
END.

ASSIGN
   lcAllowedBONOSTCContracts = fCParamC("ALLOWED_BONO_STC_CONTRACTS").

FUNCTION fAddWarningStruct RETURNS LOGICAL:

   warning_array = add_array(sub_struct,"warnings").

   lcBono = fGetCurrentSpecificBundle(Mobsub.MsSeq,"BONO").

   IF lcBono > "" THEN DO:
      IF fMatrixAnalyse(gcBrand,
                        "PERCONTR",
                        "PerContract;SubsTypeTo",
                        lcBono + ";" + pcNewCLIType,
                        OUTPUT lcError) NE 1 OR
         LOOKUP(lcBono,lcAllowedBONOSTCContracts) = 0 THEN
         add_string(warning_array,"","LEGACY_AUTOMATIC_TERMINATION").
      ELSE IF LOOKUP(lcBono,lcAllowedBONOSTCContracts) > 0 THEN
         add_string(warning_array,"","BONO_TRANSFER").
   END. /* IF lcBono > "" THEN DO: */

   IF llAdditionalLines THEN
      add_string(warning_array,"","STC_HAS_ADDITIONAL_LINES").

   IF llDSS1Term THEN
      add_string(warning_array,"","DSS1_TERMINATION").
   IF llDSS2Term THEN
      add_string(warning_array,"","DSS2_TERMINATION").

END FUNCTION.

FUNCTION fAddCLITypeStruct RETURNS LOGICAL:

   DEF VAR liCount AS INT NO-UNDO.

   IF ldaSTCDates[1] = ldaSTCDates[2] OR
      MobSub.PayType = TRUE OR CLIType.PayType = 2 OR
      NOT lliSTCAllowed THEN
      liCount = 2.
   ELSE
      liCount = 1.

   sub_struct = add_struct(response_toplevel_id, "").
   penalty_array = add_array(sub_struct,"penalties").

   DO liLoop = liCount TO EXTENT(ldaSTCDates):
           
      penalty_struct = add_struct(penalty_array,"").
      add_datetime(penalty_struct, "stc_date", ldaSTCDates[liLoop]).

      IF CLIType.PayType = {&CLITYPE_PAYTYPE_PREPAID} THEN DO:

         IF ldeTotalPenaltyFee[liLoop] <= 0 THEN NEXT.
         add_string(penalty_struct, "penalty_contract", lcPenaltyCode).
         add_double(penalty_struct,"penalty_amount",ldeTotalPenaltyFee[liLoop]).
      END. /* IF CLIType.PayType = 2 THEN DO: */
      /* postpaid */
      ELSE DO:

         IF ldeTermPendingFee[liLoop] <= 0 THEN NEXT.
         IF OldCLIType.CompareFee <= CLIType.CompareFee AND
            NOT (LOOKUP(OldCLIType.CliType,"CONT7,CONTD9") > 0 
                 AND CLIType.CLIType EQ "CONT8")
            THEN NEXT.

         IF LOOKUP(MobSub.CLIType,lcBundleCLITypes) > 0 THEN DO:
            IF MobSub.TariffBundle EQ CLIType.CLIType THEN NEXT.
         END.
         ELSE IF MobSub.CLIType EQ CLIType.CLIType THEN NEXT.

         /* special handling for Fusion tariffs, YDR-1137 */
         add_string(penalty_struct, "penalty_contract",
           (IF LOOKUP(CLIType.CLIType,lcFusionTariffs) > 0 OR
               LOOKUP(CLIType.CLIType,lcCONTSFContracts) > 0
            THEN lcPenaltyCode ELSE "TERM")).
         add_double(penalty_struct,"penalty_amount",
            ldeTermPendingFee[liLoop]).
         IF liLengthAfterExtension[liLoop] <=
            (IF LOOKUP(CLIType.CLIType,lcFusionTariffs) > 0 OR
                LOOKUP(CLIType.CLIType,lcCONTSFContracts) > 0
            THEN 36 ELSE 30)
            THEN add_boolean(penalty_struct,"extension", True).
      END.
   END.

   fAddWarningStruct().

END FUNCTION. 

FUNCTION fGetOrigBundle RETURNS CHAR
   (iiMsSeq      AS INT,
    icDCEvent    AS CHAR,
    idaValidDate AS DATE):

   DEF VAR ldActivated AS DEC  NO-UNDO.
   DEF VAR ldActEnd    AS DEC  NO-UNDO.
   DEF VAR liTime      AS INT  NO-UNDO.
   DEF VAR liOffSet    AS INT  NO-UNDO.

   DEF BUFFER bOrigOwner FOR MsOwner.
   DEF BUFFER bBundleRequest FOR MsRequest.
   DEF BUFFER bufMsRequest FOR MsRequest .
   
   /* fgetpercontractactivation */
   ldActivated = fHMS2TS(idaValidDate,"00:00:00").
   FIND FIRST bufMsRequest WHERE
              bufMsRequest.MsSeq = iiMsseq AND
              bufMsRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} AND
              bufMsRequest.ReqStatus =  {&REQUEST_STATUS_DONE} AND
              bufMsRequest.DoneStamp >= ldActivated AND
              bufMsRequest.ReqCParam3 = icDCEvent AND
       LOOKUP(bufMsRequest.ReqCparam2,"act,recreate") > 0
   NO-LOCK USE-INDEX MsSeq NO-ERROR.

   IF AVAIL bufMsRequest THEN 
      ldActivated = bufMsRequest.DoneStamp.
   
   /* fgetpercontractactivation */
   
   IF ldActivated = TRUNCATE(ldActivated,0) THEN liOffSet = 24.
   ELSE liOffSet = 3.

   ASSIGN
      /* activated during the first hours after subscription activation */
      ldActEnd    = fOffSet(ldActivated,liOffSet)
      ldActivated = fSecOffSet(ldActivated,-180).

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
               lcCONTSFContracts) > 0,
      FIRST DayCampaign NO-LOCK WHERE
            DayCampaign.Brand   = gcBrand AND
            DayCampaign.DCEvent = bBundleRequest.ReqCParam3 AND
            DayCampaign.FeeModel > "" AND
            LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0
      BY bBundleRequest.DoneStamp DESC:
         
         FIND FIRST bOrigOwner WHERE
                    bOrigOwner.MsSeq = iiMsSeq AND
                    bOrigOwner.TSBegin <= ldActivated NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bOrigOwner THEN
            FIND LAST bOrigOwner WHERE
                      bOrigOwner.MsSeq = iiMsSeq AND
                      bOrigOwner.TSBegin > ldActivated NO-LOCK NO-ERROR.
         IF NOT AVAIL bOrigOwner THEN RETURN "".
         RETURN DayCampaign.DCEvent.
   END.
   
   RETURN "".

END FUNCTION.

FUNCTION fGetOrigCLIType RETURNS CHARACTER
         (INPUT pcDCEvent AS CHAR,
          INPUT pdtValidFrom AS DATE,
          INPUT piMsSeq AS INT):

   DEF VAR ldTS AS DEC NO-UNDO.
   DEF VAR lcCLIType AS CHAR NO-UNDO. 
   DEF BUFFER bufMsRequest FOR MsRequest .

   ldTS = fHMS2TS(pdtValidFrom,"00:00:00").
   FIND FIRST bufMsRequest WHERE
              bufMsRequest.MsSeq = piMsseq AND
              bufMsRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} AND
              bufMsRequest.ReqStatus =  {&REQUEST_STATUS_DONE} AND
              bufMsRequest.DoneStamp >= ldTS AND
              bufMsRequest.ReqCParam3 = pcDCEvent AND
       LOOKUP(bufMsRequest.ReqCparam2,"act,recreate") > 0
   NO-LOCK USE-INDEX MsSeq NO-ERROR.

   IF AVAIL bufMsRequest THEN 
      ldTS = bufMsRequest.DoneStamp.

   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = piMsSeq AND 
              MsOwner.TSBegin <= ldTS NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE MsOwner THEN 
      FIND LAST MsOwner WHERE
                MsOwner.MsSeq = piMsSeq AND 
                MsOwner.TSBegin > ldTS NO-LOCK NO-ERROR. 
   IF AVAIL MsOwner THEN
           lcCLIType = MsOwner.CLIType.

   RETURN lcCLIType.  
END FUNCTION.


IF NOT MobSub.PayType THEN DO:

   /* If postpaid to prepaid only */
   IF CLIType.PayType = {&CLITYPE_PAYTYPE_PREPAID} THEN DO:
      FIND FIRST DCCLI WHERE
                 DCCLI.MsSeq = MobSub.MsSeq     AND
                 DCCLI.DCEvent BEGINS "PAYTERM" AND
                 DCCLI.ValidTo >= ldaSTCDates[1] NO-LOCK NO-ERROR.
      IF AVAIL DCCLI THEN DO:

         DO liLoop = 1 TO EXTENT(ldaSTCDates):
            fGetFixedFeeInfo(MobSub.MsSeq,
                             MobSub.CustNum,
                             "", /* collect all active payterms */
                             0,
                             ldaSTCDates[liLoop],
                             OUTPUT ldePendingFee,
                             OUTPUT liTotalPeriods,
                             OUTPUT ldePeriodFee,
                             OUTPUT lderesidualFee,
                             OUTPUT lcFinancedInfo,
                             OUTPUT liOrderId).
            IF ldePendingFee > 0 THEN
               ASSIGN ldeTotalPenaltyFee[liLoop] = ldePendingFee
                      lcPenaltyCode = "PAYTERM".
         END.
      END.

      /* Residual Amount SingleFee */
      DO liLoop = 1 TO EXTENT(ldaSTCDates):

         ASSIGN ldePendingFee = 0
                liFeePeriod = YEAR(ldaSTCDates[liLoop]) * 100 +
                              MONTH(ldaSTCDates[liLoop]).

         FOR EACH SingleFee USE-INDEX Custnum WHERE
                  SingleFee.Brand       = gcBrand AND
                  SingleFee.Custnum     = Mobsub.InvCust AND
                  SingleFee.HostTable   = "Mobsub" AND
                  SingleFee.KeyValue    = STRING(Mobsub.MsSeq) AND
                  SingleFee.BillPeriod >= liFeePeriod AND
                  SingleFee.CalcObj     = "RVTERM" NO-LOCK:
            ldePendingFee = ldePendingFee + SingleFee.Amt.
         END.

         IF ldePendingFee > 0 THEN
            ASSIGN ldeTotalPenaltyFee[liLoop] = ldeTotalPenaltyFee[liLoop] +
                                                ldePendingFee
                   lcPenaltyCode = "PAYTERM".
      END.
   END.

   /* Count possible penalty fee for contract termination */
   liCount = 0.
   CONTRACT_LOOP:
   FOR EACH DCCLI NO-LOCK WHERE
            DCCLI.MsSeq = MobSub.MsSeq AND
            DCCLI.ValidFrom <= ldaSTCDates[1] AND
            DCCLI.ValidTo   >= ldaSTCDates[1] AND
            DCCLI.CreateFees = TRUE,
      FIRST DayCampaign WHERE
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = DCCLI.DCEvent AND
            DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
            DayCampaign.TermFeeModel NE "" AND
            DayCampaign.TermFeeCalc > 0 NO-LOCK BY DCCLI.ValidFrom DESC:
   
      liCount = liCount + 1.
      IF liCount > 1 THEN LEAVE CONTRACT_LOOP.
  
      IF DCCLI.Amount NE ? THEN ldePeriodFee = DCCLI.Amount.
      ELSE DO:
         lcPriceList = fFeeModelPriceList(MobSub.Custnum,
                                          MobSub.BillTarget,
                                          DayCampaign.TermFeeModel,
                                          TODAY).
         FIND FIRST FMItem NO-LOCK WHERE
                    FMItem.Brand     = gcBrand       AND
                    FMItem.FeeModel  = DayCampaign.TermFeeModel AND
                    FMItem.PriceList = lcPriceList AND
                    FMItem.FromDate <= TODAY     AND
                    FMItem.ToDate   >= TODAY NO-ERROR.
         IF AVAIL FMItem THEN ldePeriodFee = FMItem.Amount.
      END.

      IF ldePeriodFee EQ 0 THEN NEXT.
         
      ldtTo = DATETIME(DCCLI.ValidTo,0).

      /* calculate a factor for the fee (full / proportional) */
      DO liLoop = 1 TO EXTENT(ldaSTCDates):
         ldePenaltyFactor = fCalculateFactor(DCCLI.ValidFrom,
                                              DCCLI.RenewalDate,
                                              DCCLI.ValidTo,
                                              DCCLI.ValidToOrig,
                                              ldaSTCDates[liLoop],
                                              DayCampaign.TermFeeCalc).
         
         IF ldePenaltyFactor EQ 0 THEN NEXT.
          
         ASSIGN ldeTermPendingFee[liLoop] = 
            TRUNCATE(ldePenaltyFactor * ldePeriodFee,0)
            ldeTotalPenaltyFee[liLoop] = ldeTotalPenaltyFee[liLoop]
               + ldeTermPendingFee[liLoop]
            lcPenaltyCode = lcPenaltyCode + "+TERM" WHEN liLoop EQ 1
            ldtFrom = DATETIME(ldaSTCDates[liLoop],0)
            liLengthAfterExtension[liLoop] =
              INTERVAL(ldtTo,ldtFrom,"months") + 13.
      END.

      lcOrigCLIType = fGetOrigCLIType(DCCLI.DCEvent,
                                      DCCLI.ValidFrom,
                                      MobSub.MsSeq).

      IF LOOKUP(lcOrigCLIType,lcBundleCLITypes) > 0 THEN DO:
         lcOrigBundle = fGetOrigBundle(MobSub.MsSeq,
                                       DCCLI.DCEvent,
                                       DCCLI.ValidFrom).

         lcOrigCLIType = fConvBundleToCLIType(lcOrigBundle).
      END.

   END. /* FOR EACH DCCLI NO-LOCK WHERE */

   lcPenaltyCode = TRIM(lcPenaltyCode,"+").

   /* Check Additional Line */
   IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                     CLIType.Brand = gcBrand AND
                     CLIType.CLIType = MobSub.TariffBundle AND
                     CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) AND
      NOT CAN-FIND(FIRST CLIType NO-LOCK WHERE
                         CLIType.Brand = gcBrand AND
                         CLIType.CLIType = pcTariffBundle AND
                         CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN DO:

      MOBSUB_LOOP:
      FOR EACH lbMobSub NO-LOCK WHERE
               lbMobSub.Brand   = gcBrand AND
               lbMobSub.InvCust = Mobsub.CustNum AND
               lbMobSub.PayType = FALSE AND
               lbMobSub.MsSeq NE Mobsub.MsSeq,
         FIRST bCLIType NO-LOCK WHERE
               bCLIType.Brand = gcBrand AND
               bCLIType.CLIType = (IF lbMobsub.TariffBundle > ""
                                  THEN lbMobsub.TariffBundle
                                  ELSE lbMobsub.CLIType) AND
               bCLIType.LineType > 0:

         IF fHasPendingRequests(
            lbMobSub.MsSeq,
            lbMobSub.CLI,
            bCLIType.LineType) THEN NEXT.

         IF bCLIType.LineType = {&CLITYPE_LINETYPE_MAIN} THEN DO:
            llAdditionalLines = FALSE.
            LEAVE MOBSUB_LOOP.
         END.
         llAdditionalLines = TRUE.

      END. /* FOR EACH lbMobSub NO-LOCK WHERE */

   END.

   /* Check DSS availability */
   lcDSSBundleId = fGetActiveDSSId(MobSub.CustNum,fMakeTS()).
   IF lcDSSBundleId > "" AND
      NOT fOngoingDSSTerm(MobSub.CustNum,ldeEndTS) THEN DO:
      IF lcDSSBundleId = "DSS2" THEN DO:
         IF LOOKUP(pcNewCLIType,lcAllowedDSS2SubsType) = 0 AND
            (LOOKUP(pcTariffBundle,lcDSS2PrimarySubsType) = 0 AND
             LOOKUP(pcNewCLIType, lcDSS2PrimarySubsType) = 0) AND
            NOT fCanDSSKeepActive(MobSub.CustNum,MobSub.MsSeq,ldeNextMonthTS,
                                  lcDSSBundleId,OUTPUT lcError) THEN
            llDSS2Term = TRUE.
         ELSE IF LOOKUP(pcNewCLIType,lcAllowedDSS2SubsType) > 0 AND
                (LOOKUP(pcTariffBundle,lcDSS2PrimarySubsType) = 0 AND
                 LOOKUP(pcNewCLIType,lcDSS2PrimarySubsType) = 0) AND
            NOT fCanDSSKeepActive(MobSub.CustNum,0,ldeNextMonthTS,
                                  lcDSSBundleId,OUTPUT lcError) THEN
            llDSS2Term = TRUE.
      END.
      ELSE IF lcDSSBundleId = "DSS" AND
         LOOKUP(pcNewCLIType,lcDataBundleCLITypes) = 0 AND
         LOOKUP(pcTariffBundle,lcPostpaidDataBundles) = 0 AND
         NOT fCanDSSKeepActive(MobSub.CustNum,MobSub.MsSeq,ldeNextMonthTS,
                               lcDSSBundleId,OUTPUT lcError) THEN
         llDSS1Term = TRUE.
   END. /* IF lcDSSBundleId > "" AND */

END. /* IF NOT MobSub.PayType THEN DO: */

FIND FIRST OldCLIType WHERE
           OldCLIType.CLIType = lcOrigCLIType NO-LOCK NO-ERROR.
IF NOT AVAILABLE OldCLIType THEN
   RETURN appl_err(SUBST("Current CLIType entry &1 not found", lcOrigCLIType)).

fAddCLITypeStruct().

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.

