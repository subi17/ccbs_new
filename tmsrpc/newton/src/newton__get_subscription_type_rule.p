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
                warnings;array;optional;Contract related warnings
 */

 /*
   17.09.2015 hugo.lujan YPR-2524 [Q25] - TMS - STC from Postpaid to Prepaid
   AC1: Modify get_subscription_type_rule TMSRPC to:
   * fetch Quota 25 refinance remaining amount and
   * add into total penalty fee.
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:katun = "Newton".
Syst.Var:gcBrand = "1".

{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/fixedfee.i}
{Mm/fbundle.i}
{Func/fcustpl.i}
{Func/penaltyfee.i}
{Func/fctchange.i}
{Func/main_add_lines.i}
{Func/dss_matrix.i}
{Func/fixedlinefunc.i}

/* Input parameters */
DEF VAR piMsSeq            AS INT     NO-UNDO.
DEF VAR pcNewCLIType       AS CHAR    NO-UNDO.
DEF VAR pcTariffBundle     AS CHAR    NO-UNDO.

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
DEF VAR ldeTotalPenaltyFee AS DEC  NO-UNDO EXTENT 2.
DEF VAR liTotalPeriods     AS INT  NO-UNDO.
DEF VAR lcPenaltyCode      AS CHAR NO-UNDO.
DEF VAR lcPriceList        AS CHAR NO-UNDO.
DEF VAR lcOrigCLIType      AS CHAR NO-UNDO.
DEF VAR lcFinancedInfo AS CHAR NO-UNDO. 
DEF VAR ldaSTCDates AS DATE NO-UNDO EXTENT 2.
DEF VAR liLengthAfterExtension AS INT NO-UNDO EXTENT 2.
DEF VAR liLoop AS INT NO-UNDO. 
DEF VAR ldePenaltyFactor AS DEC NO-UNDO. 
DEF VAR ldtFrom AS DATE NO-UNDO. 
DEF VAR ldtTo AS DATE NO-UNDO. 
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
/* Additional line mobile only ALFMO-53 */
DEF VAR llAddline50Disc AS LOGICAL NO-UNDO.
DEF VAR llAddline20Disc AS LOGICAL NO-UNDO.

/* q25refinance_remaining Quota 25 refinance remaining amount */
DEF VAR ldeQ25RefiRemain AS DECIMAL NO-UNDO.

DEF VAR ldOriginalFee AS DECIMAL NO-UNDO.
DEF VAR ldNewFee      AS DECIMAL NO-UNDO.

DEF BUFFER bCLIType        FOR CLIType.
DEF BUFFER OldCLIType      FOR CLIType.
DEF BUFFER lbMobSub        FOR MobSub.

IF validate_request(param_toplevel_id, "int,string,string") EQ ? THEN RETURN.
piMsSeq         = get_pos_int(param_toplevel_id,"0").
pcNewCLIType    = get_string(param_toplevel_id,"1").
pcTariffBundle  = get_string(param_toplevel_id,"2").  /* tariff_bundle */

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO OrderCanal MobSub MsSeq piMsSeq}

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
   ldaSTCDates[1] = TODAY + 1
   ldaSTCDates[2] = Func.Common:mLastDayOfMonth(TODAY) + 1
   ldeNextMonthTS = Func.Common:mMake2DT(ldaSTCDates[2],0)
   ldeEndTS       = Func.Common:mMake2DT(Func.Common:mLastDayOfMonth(TODAY),86399).

lliSTCAllowed = fIsiSTCAllowed(INPUT Mobsub.MsSeq).

IF MobSub.PayType = FALSE THEN DO:
   ASSIGN
      lcCONTSFContracts     = fCParamC("CONTSF_CONTRACTS")
      lcFusionTariffs       = fCParamC("FUSION_SUBS_TYPE")
      lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE")
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
      lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS")
      lcDataBundleCLITypes  = fCParamC("DATA_BUNDLE_BASED_CLITYPES").
END.

ASSIGN
   lcAllowedBONOSTCContracts = fCParamC("ALLOWED_BONO_STC_CONTRACTS").

FUNCTION fAddWarningStruct RETURNS LOGICAL:

   DEFINE VARIABLE lcParentValue AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcChildValue  AS CHARACTER NO-UNDO.
   DEFINE BUFFER lELMobSub FOR MobSub.

   warning_array = add_array(sub_struct,"warnings").

   lcBono = fGetCurrentSpecificBundle(Mobsub.MsSeq,"BONO").

   IF fCLITypeIsExtraLine(pcNewCLIType)   AND               
      NOT fValidateExtraLineSTC(MobSub.CustNum, pcNewCLIType)
   THEN DO:
      add_string(warning_array,"","STC_EXTRALINE_ERROR").
      RETURN FALSE.
   END.

   IF lcBono > "" THEN DO:
      IF fMatrixAnalyse(Syst.Var:gcBrand,
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

   /* ALFMO-53 */
   IF llAddline50Disc THEN
      add_string(warning_array,"","STC_HAS_50_PER_ADDLINE").
   IF llAddline20Disc THEN
      add_string(warning_array,"","STC_HAS_20_PER_ADDLINE").

   IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                     CLIType.CLIType = MobSub.CLIType AND
                     CLIType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT})
   THEN DO:
      lcParentValue = "CONVERGENT".
      /* If the old CLIType can have extraline and the extraline actually exists */
      IF fCLITypeIsMainLine(MobSub.CLIType) AND
         CAN-FIND(FIRST lELMobSub USE-INDEX MultiSimID NO-LOCK WHERE
                        lELMobSub.Brand        EQ Syst.Var:gcBrand AND
                        lELMobSub.MultiSimId   EQ MobSub.MsSeq               AND
                        lELMobSub.MultiSimtype EQ {&MULTISIMTYPE_EXTRALINE}  AND
                        lELMobSub.Custnum      EQ Mobsub.Custnum AND
                       (lELMobSub.MsStatus     EQ {&MSSTATUS_ACTIVE} OR
                        lELMobSub.MsStatus     EQ {&MSSTATUS_BARRED}))
      THEN lcParentValue = fExtraLineForMainLine(MobSub.CLIType).

      IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                        CLIType.CLIType = pcNewCLIType AND
                        CLIType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT})
      THEN DO:
         lcChildValue = "CONVERGENT".

         /* The new CLIType can have extraline. In this case we don't check
            the activity status of the extraline as the old CLIType extraline will
            be used (if available) */
         IF fCLITypeIsMainLine(pcNewCLIType)
         THEN lcChildValue = fExtraLineForMainLine(pcNewCLIType).
      END.
      ELSE IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                             CLIType.CLIType = pcNewCLIType AND
                             CLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY})
      THEN lcChildValue = "MOBILEONLY".

      FOR EACH TMSRelation NO-LOCK USE-INDEX ChildValue WHERE
               TMSRelation.TableName   = "CLIType"           AND
               TMSRelation.KeyType     = "STCWarningMessage" AND
               TMSRelation.ChildValue  = lcChildValue        AND
               TMSRelation.ParentValue = lcParentValue:

         IF TMSRelation.ToTime NE ? AND
            TMSRelation.ToTime < NOW
         THEN LEAVE.

         IF TMSRelation.FromTime NE ? AND
            TMSRelation.FromTime > NOW
         THEN NEXT.

         add_string(warning_array,"",TMSRelation.RelationType).
      END.
   END.

END FUNCTION.

FUNCTION fAddCLITypeStruct RETURNS LOGICAL:

   DEF VAR liCount AS INT NO-UNDO.

   IF (NOT(MobSub.PayType = FALSE OR CLIType.PayType = 1)  OR
       NOT(MobSub.PayType = TRUE  OR CLIType.PayType = 2)) AND
          ldaSTCDates[1] <> ldaSTCDates[2]                 AND
          lliSTCAllowed                                    THEN
      liCount = 1.  /* collect both STC dates (iSTC and normal) */
   ELSE
      liCount = 2.  /* collect the 2nd STC date only (normal) */

   sub_struct = add_struct(response_toplevel_id, "").
   penalty_array = add_array(sub_struct,"penalties").

   DO liLoop = liCount TO EXTENT(ldaSTCDates):
           
      penalty_struct = add_struct(penalty_array,"").
      add_datetime(penalty_struct, "stc_date", ldaSTCDates[liLoop]).
      
      IF ldeTotalPenaltyFee[liLoop] <= 0 THEN NEXT.

      IF CLIType.PayType = {&CLITYPE_PAYTYPE_PREPAID} THEN DO:

         add_string(penalty_struct, "penalty_contract", lcPenaltyCode).
         add_double(penalty_struct,"penalty_amount",ldeTotalPenaltyFee[liLoop]).
      END. /* IF CLIType.PayType = 2 THEN DO: */
      /* postpaid */
      ELSE DO:

         IF LOOKUP(MobSub.CLIType,lcBundleCLITypes) > 0 THEN DO:
            IF MobSub.TariffBundle EQ CLIType.CLIType THEN NEXT.
         END.
         ELSE IF MobSub.CLIType EQ CLIType.CLIType THEN NEXT.

         add_string(penalty_struct, "penalty_contract", lcPenaltyCode).
         add_double(penalty_struct,"penalty_amount",
            ldeTotalPenaltyFee[liLoop]).

         IF liLengthAfterExtension[liLoop] NE 0 AND
            liLengthAfterExtension[liLoop] <=
            (IF LOOKUP(CLIType.CLIType,lcFusionTariffs) > 0 OR
                LOOKUP(CLIType.CLIType,lcCONTSFContracts) > 0
            THEN 36 ELSE 30)
            THEN add_boolean(penalty_struct,"extension", True).
      END.
   END.

   fAddWarningStruct().

END FUNCTION. 

/* Moved to Func/penaltyfee.i
FUNCTION fGetReferenceTariff RETURNS CHARACTER
         (INPUT pcDCEvent      AS CHAR,
          INPUT pdtValidFrom   AS DATE,
          INPUT pdtExtentDate AS DATE,
          INPUT piMsSeq        AS INT):

   DEF VAR ldTS        AS DEC  NO-UNDO.
   DEF VAR lcCLIType   AS CHAR NO-UNDO.
   DEF VAR lcReqParam2 AS CHAR NO-UNDO.

   DEF BUFFER bufMsRequest FOR MsRequest .

   IF pdtExtentDate <> ? THEN
      ASSIGN
         ldTS        = Func.Common:mHMS2TS(pdtExtentDate,"00:00:00")
         lcReqParam2 = "update".
   ELSE
      ASSIGN
         ldTS        = Func.Common:mHMS2TS(pdtValidFrom,"00:00:00")
         lcReqParam2 = "act,recreate".

   FIND FIRST bufMsRequest WHERE
              bufMsRequest.MsSeq      = piMsseq                        AND
              bufMsRequest.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION} AND
              bufMsRequest.ReqStatus  = {&REQUEST_STATUS_DONE}         AND
              bufMsRequest.DoneStamp >= ldTS                           AND
              bufMsRequest.ReqCParam3 = pcDCEvent                      AND
       LOOKUP(bufMsRequest.ReqCparam2,lcReqParam2) > 0
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
   
   IF AVAIL MsOwner THEN DO:
      lcCLIType = MsOwner.CLIType.

      IF MsOwner.TariffBundle <> "" THEN 
         lcCLIType = MsOwner.TariffBundle.
   END.

   RETURN lcCLIType.
END FUNCTION.
*/

/*
   Purpose: Fetch Quota 25 refinance remaining amount
   YPR-2524 - [Q25] - TMS - STC from Postpaid to Prepaid
   Author: Hugo Alberto Lujan Chavez hugo.lujan
   25.sep.2015
*/
FUNCTION fGetQ25RefRemainingAmt RETURNS DECIMAL
         (INPUT iMsSeq       AS INTEGER,
          INPUT iiCustnum    AS INTEGER,
          INPUT icCalcObj    AS CHARACTER,          
          INPUT idaCountFrom AS DATE):

   /* Quota 25 refinance remaining amount */
   DEF VAR ldePendingFee AS DECIMAL NO-UNDO.
   DEF VAR liPeriodFrom  AS INTEGER NO-UNDO.
   DEF VAR liPeriod      AS INTEGER NO-UNDO. 
   DEF VAR ldaDate       AS DATE    NO-UNDO.

   DEF BUFFER FixedFee  FOR FixedFee.
   DEF BUFFER FFItem    FOR FFItem.

   ASSIGN
      ldaDate       = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
      liPeriod      = YEAR(ldaDate) * 100 + MONTH(ldaDate)
      ldePendingFee = 0.

   IF idaCountFrom NE ? THEN
      ASSIGN liPeriodFrom  = YEAR(idaCountFrom) * 10000 + 
                             MONTH(idaCountFrom) * 100  +
                             DAY(idaCountFrom).

   FOR EACH FixedFee NO-LOCK USE-INDEX HostTable WHERE
            FixedFee.Brand     EQ Syst.Var:gcBrand        AND 
            FixedFee.Custnum   EQ iiCustnum      AND
            FixedFee.HostTable EQ "MobSub"       AND
            FixedFee.KeyValue  EQ STRING(iMsSeq) AND
            FixedFee.EndPeriod >= liPeriod       AND
            FixedFee.BillCode  BEGINS "RVTERM"   AND
            FixedFee.InUse:

      IF icCalcObj > "" AND
         FixedFee.CalcObj <> icCalcObj THEN NEXT.      

      FOR EACH FFItem OF FixedFee NO-LOCK:
         IF idaCountFrom <> ? AND
            FFItem.Concerns[1] < liPeriodFrom THEN NEXT.
        
         IF FFItem.Billed THEN NEXT.
         ldePendingFee = ldePendingFee + FFItem.Amt.
      END. /* FOR EACH FFItem OF FixedFee */      
   END. /* FOR EACH FixedFee */

   RETURN ldePendingFee.
END FUNCTION. /* FUNCTION fGetQ25RefRemainingAmt */


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
         END. /* liLoop = 1 TO EXTENT(ldaSTCDates) */
      END.
      
      /* Q25 */
      IF CAN-FIND(FIRST DCCLI NO-LOCK WHERE
                 DCCLI.MsSeq = MobSub.MsSeq     AND
                 DCCLI.DCEvent EQ "RVTERM12" AND
                 DCCLI.ValidTo >= ldaSTCDates[1]) THEN DO:

         DO liLoop = 1 TO EXTENT(ldaSTCDates):
            ldeQ25RefiRemain = 
               fGetQ25RefRemainingAmt(MobSub.MsSeq,
                                      MobSub.Custnum,
                                      "RVTERM12",
                                      ldaSTCDates[liLoop]).
            IF ldeQ25RefiRemain > 0 THEN
               ASSIGN ldeTotalPenaltyFee[liLoop] = ldeTotalPenaltyFee[liLoop] + 
                                                   ldeQ25RefiRemain
                      lcPenaltyCode = "PAYTERM".
         END. /* liLoop = 1 TO EXTENT(ldaSTCDates) */
      END.

      /* Residual Amount SingleFee */
      DO liLoop = 1 TO EXTENT(ldaSTCDates):

         ASSIGN ldePendingFee = 0
                liFeePeriod = YEAR(ldaSTCDates[liLoop]) * 100 +
                              MONTH(ldaSTCDates[liLoop]).

         FOR EACH SingleFee USE-INDEX Custnum WHERE
                  SingleFee.Brand       = Syst.Var:gcBrand AND
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
   CONTRACT_LOOP:
   FOR EACH DCCLI NO-LOCK WHERE
            DCCLI.MsSeq = MobSub.MsSeq AND
            DCCLI.ValidFrom <= ldaSTCDates[1] AND
            DCCLI.ValidTo   >= ldaSTCDates[1] AND
            DCCLI.CreateFees = TRUE,
      FIRST DayCampaign WHERE
            DayCampaign.Brand = Syst.Var:gcBrand AND
            DayCampaign.DCEvent = DCCLI.DCEvent AND
            DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
            DayCampaign.TermFeeModel NE "" AND
            DayCampaign.TermFeeCalc > 0 NO-LOCK BY DCCLI.ValidFrom DESC:

      ASSIGN ldNewFee = 0 ldOriginalFee = 0 ldePeriodFee = 0.
   
      IF DCCLI.Amount NE ? THEN ldePeriodFee = DCCLI.Amount.
      ELSE DO:
         lcPriceList = fFeeModelPriceList(MobSub.Custnum,
                                          MobSub.BillTarget,
                                          DayCampaign.TermFeeModel,
                                          TODAY).
         FIND FIRST FMItem NO-LOCK WHERE
                    FMItem.Brand     = Syst.Var:gcBrand       AND
                    FMItem.FeeModel  = DayCampaign.TermFeeModel AND
                    FMItem.PriceList = lcPriceList AND
                    FMItem.FromDate <= TODAY     AND
                    FMItem.ToDate   >= TODAY NO-ERROR.
         IF AVAIL FMItem THEN ldePeriodFee = FMItem.Amount.
      END.

      IF ldePeriodFee EQ 0 THEN NEXT.
         
      IF CLIType.PayType EQ {&CLITYPE_PAYTYPE_POSTPAID} THEN DO:

         lcOrigCLIType = fGetReferenceTariff(DCCLI.DCEvent,
                                             DCCLI.ValidFrom,
                                             DCCLI.RenewalDate,
                                             MobSub.MsSeq).
         
         FIND FIRST OldCLIType NO-LOCK WHERE
                    OldCLIType.CLIType = lcOrigCLIType NO-ERROR.
         IF NOT AVAILABLE OldCLIType THEN
            RETURN appl_err(SUBST("Current CLIType entry &1 not found",
                                  lcOrigCLIType)).

         IF DCCLI.DCEvent BEGINS "TERM" THEN 
         DO:
            IF fIsConvergentORFixedOnly(CLIType.CLIType) OR fIsConvergentORFixedOnly(OldCLIType.CLIType) THEN 
            DO:
                ASSIGN 
                    ldOriginalFee = fGetMobileLineCompareFee(OldCLIType.CLIType, OldCLIType.BaseBundle, DCCLI.ValidFrom)
                    ldNewFee      = fGetMobileLineCompareFee(CLIType.CLIType   , CLIType.BaseBundle   , TODAY).

                IF ldOriginalFee <= ldNewFee THEN
                    NEXT.
            END.
            ELSE
            DO:
                IF OldCLIType.CompareFee <= CLIType.CompareFee AND
                   NOT (LOOKUP(OldCLIType.CliType,"CONT7,CONTD9") > 0 
                        AND CLIType.CLIType EQ "CONT8")
                   THEN NEXT.
            END.   
         END.
         /* When STCed between convergent tariffs we exclude FTERM and TVTERM from termination */
         ELSE IF (DCCLI.DCEvent BEGINS "FTERM" OR DCCLI.DCEvent BEGINS "TVTERM") AND
                 fIsConvergentORFixedOnly(CLIType.CLIType) THEN NEXT.
      END.
         
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
         IF TRUNCATE(ldePenaltyFactor * ldePeriodFee,0) EQ 0 THEN NEXT.
          
         ASSIGN
            ldeTotalPenaltyFee[liLoop] = ldeTotalPenaltyFee[liLoop]
               + TRUNCATE(ldePenaltyFactor * ldePeriodFee,0) 
            lcPenaltyCode = lcPenaltyCode + "+TERM" WHEN
               INDEX(lcPenaltyCode,"+TERM") = 0
            ldtFrom = DATETIME(ldaSTCDates[liLoop],0)
            liLengthAfterExtension[liLoop] =
              INTERVAL(ldtTo,ldtFrom,"months") + 13
              WHEN DCCLI.DCEvent BEGINS "TERM".
      END.


   END. /* FOR EACH DCCLI NO-LOCK WHERE */

   lcPenaltyCode = TRIM(lcPenaltyCode,"+").

   /* Check 50% and 20% additional line discount */

   FIND FIRST Customer WHERE
              Customer.CustNum = MobSub.AgrCust NO-LOCK NO-ERROR.

   /* Mobile only additional line ALFMO-53 20% */
   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = Syst.Var:gcBrand AND
              DiscountPlan.DPRuleID = ENTRY(LOOKUP(MobSub.CliType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS_20}) NO-LOCK NO-ERROR.

   IF AVAIL DiscountPlan THEN 
   DO: 
      IF AVAIL Customer AND 
      LOOKUP(CLIType.Clitype, {&ADDLINE_CLITYPES}) = 0 AND
      CAN-FIND(FIRST DPMember WHERE
                     DPMember.DPId      = DiscountPlan.DPId AND
                     DPMember.HostTable = "MobSub" AND
                     DPMember.KeyValue  = STRING(MobSub.MsSeq) AND
                     DPMember.ValidTo   >= TODAY) THEN
      ASSIGN llAddline20Disc = TRUE.   
   END.

   /* Mobile only additional line ALFMO-53 50% */

   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = Syst.Var:gcBrand AND
              DiscountPlan.DPRuleID = ENTRY(LOOKUP(MobSub.CliType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS_HM}) NO-LOCK NO-ERROR.

   IF AVAIL DiscountPlan THEN 
   DO: 
      IF AVAIL Customer AND
      CAN-FIND(FIRST DPMember WHERE
                     DPMember.DPId      = DiscountPlan.DPId AND
                     DPMember.HostTable = "MobSub" AND
                     DPMember.KeyValue  = STRING(MobSub.MsSeq) AND
                     DPMember.ValidTo   >= TODAY) THEN
      DO:
         IF LOOKUP(CLIType.Clitype, {&ADDLINE_CLITYPES}) = 0 OR
            NOT fCheckExistingMobileOnly(Customer.CustIDType,Customer.OrgID,CLIType.CLIType) THEN
            ASSIGN llAddline50Disc = TRUE.
      END.
   END.

   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = Syst.Var:gcBrand AND
              DiscountPlan.DPRuleID = ENTRY(LOOKUP(MobSub.CliType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS}) NO-LOCK NO-ERROR.

   IF AVAIL DiscountPlan THEN 
   DO: 
      IF AVAIL Customer AND
      CAN-FIND(FIRST DPMember WHERE
                     DPMember.DPId      = DiscountPlan.DPId AND
                     DPMember.HostTable = "MobSub" AND
                     DPMember.KeyValue  = STRING(MobSub.MsSeq) AND
                     DPMember.ValidTo   >= TODAY) THEN
      DO:
         IF LOOKUP(CLIType.Clitype, {&ADDLINE_CLITYPES}) = 0 OR
            NOT fCheckExistingConvergent(Customer.CustIDType,Customer.OrgID,CLIType.CLIType) THEN
            ASSIGN llAddline50Disc = TRUE.
      END.
   END.

   /* Check Additional Line */
   IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                     CLIType.Brand = Syst.Var:gcBrand AND
                     CLIType.CLIType = MobSub.TariffBundle AND
                     CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) AND
      NOT CAN-FIND(FIRST CLIType NO-LOCK WHERE
                         CLIType.Brand = Syst.Var:gcBrand AND
                         CLIType.CLIType = pcTariffBundle AND
                         CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN DO:

      MOBSUB_LOOP:
      FOR EACH lbMobSub NO-LOCK WHERE
               lbMobSub.Brand   = Syst.Var:gcBrand AND
               lbMobSub.InvCust = Mobsub.CustNum AND
               lbMobSub.PayType = FALSE AND
               lbMobSub.MsSeq NE Mobsub.MsSeq,
         FIRST bCLIType NO-LOCK WHERE
               bCLIType.Brand = Syst.Var:gcBrand AND
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
   lcDSSBundleId = fGetActiveDSSId(MobSub.CustNum,Func.Common:mMakeTS()).
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

fAddCLITypeStruct().

FINALLY:
   END.
