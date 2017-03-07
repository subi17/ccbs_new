/**
 * Subscription charges and balances for the current month.
 *
 * @input     msseq;int;mandatory;Mobile subscription identifier
 * @output    total_balance;double;mandatory;
              clitype_struct;structs;mandatory;
 * @clitype_struct
              billing_groups;array of structs;mandatory;billing item group data
 * @billing_groups tms_id;string;mandatory;Name of billitem group
              balance;double;mandatory;Billing group Balance
              balance_day;double;optional;Billing item group daily balance
              billing_items;array of structs;mandatory;billing item data
 * @billing_items billing_code;double;mandatory;Billing group Balance
              balance;double;mandatory;Billing item monthly balance
              balance_day;double;optional;Billing item daily balance (used with TARJ6)
 */
{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{Syst/commpaa.i}
gcBrand = "1".
{Func/callquery.i}
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/upsellbundle.i}
{Func/tarj6.i}
{Func/fprepaidfee.i}

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.

/* Output parameters */
DEF VAR main_level_struct   AS CHARACTER NO-UNDO.
DEF VAR first_level_struct  AS CHARACTER NO-UNDO.
DEF VAR second_level_array  AS CHARACTER NO-UNDO.
DEF VAR second_level_struct AS CHARACTER NO-UNDO.
DEF VAR third_level_array   AS CHARACTER NO-UNDO.
DEF VAR third_level_struct  AS CHARACTER NO-UNDO.
DEF VAR forth_level_array   AS CHARACTER NO-UNDO.
DEF VAR forth_level_struct  AS CHARACTER NO-UNDO.

/* Local variables */
DEF VAR ldbalance        AS DECIMAL INITIAL 0.0 NO-UNDO.
DEF VAR first_of_month   AS DATE NO-UNDO FORMAT "99.99.9999".
DEF VAR liPeriod         AS INT NO-UNDO.
DEF VAR liDayPeriod      AS INT NO-UNDO.
DEF VAR ldeGroupBalance  AS DECIMAL NO-UNDO.
DEF VAR ldeGroupDailyBalance AS DECIMAL NO-UNDO.
DEF VAR tthCDR           AS HANDLE  NO-UNDO.
DEF VAR liErrorCodeOut   AS INTEGER NO-UNDO INIT 0.
DEF VAR ldPeriodFrom     AS DEC     NO-UNDO.
DEF VAR ldPeriodTo       AS DEC     NO-UNDO.
DEF VAR ldaLastDay       AS DATE    NO-UNDO.
DEF VAR ldDataFee        AS DEC     NO-UNDO.
DEF VAR ldeBundleFee     AS DECIMAL NO-UNDO.
DEF VAR ldeUpsellFee     AS DECIMAL NO-UNDO.
DEF VAR ldeCurrentTS     AS DECIMAL NO-UNDO.
DEF VAR liUpsellCount    AS INT     NO-UNDO.
DEF VAR lcError          AS CHAR    NO-UNDO.
DEF VAR lcGroupCode      AS CHAR    NO-UNDO.
DEF VAR lcDSSBundleId    AS CHAR    NO-UNDO.
DEF VAR lcFirstMonthFeeCalcObj AS CHAR NO-UNDO.
DEF VAR ldeTARJ6DailyChargeMonth AS DEC NO-UNDO. 
DEF VAR ldeTARJ6DailyChargeDay AS DEC NO-UNDO. 
DEF VAR ldeTARJ6UpsellChargeMonth AS DEC NO-UNDO. 
DEF VAR ldeTARJ6UpsellChargeDay AS DEC NO-UNDO. 
DEF VAR lcBONOContracts       AS CHAR NO-UNDO.
DEF VAR lcIPLContracts        AS CHAR NO-UNDO.
DEF VAR lcFLATContracts       AS CHAR NO-UNDO.
DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.
DEF VAR lcExcludeBundles      AS CHAR NO-UNDO.
DEF VAR lcFirstMonthUsageBasedBundles AS CHAR NO-UNDO.
DEF VAR ldaiSTCDate AS DATE NO-UNDO.
DEF VAR liiSTCTime  AS INT  NO-UNDO.
DEF VAR lliSTC      AS LOG  NO-UNDO.
DEF VAR ldeTotalBalance AS DEC NO-UNDO.
DEF VAR lcCLIType   AS CHAR NO-UNDO.
DEF VAR lcTariffBundle   AS CHAR NO-UNDO.
DEF VAR ldaItemFromDate AS DATE NO-UNDO.
DEF VAR ldaItemToDate AS DATE NO-UNDO.
DEF VAR ldeTotalDataBundleLimit AS DEC NO-UNDO.
DEF VAR ldeCDRts AS DEC NO-UNDO.
DEF VAR lcYear AS CHAR NO-UNDO.
DEF VAR lcMonth AS CHAR No-UNDO.
DEF VAR ldaToDate AS DATE NO-UNDO.
DEF VAR ldaFromDate AS DATE NO-UNDO.
DEF VAR liLoop AS INT NO-UNDO.
DEF VAR lcDiscounts AS CHAR NO-UNDO.
DEF VAR ldDiscountAmt AS DEC NO-UNDO.
DEF VAR llExtFound AS LOGICAL NO-UNDO.

DEF BUFFER bMsRequest FOR MSRequest.

{Mm/dss_bundle_first_month_fee.i}

DEF TEMP-TABLE ttCDR NO-UNDO LIKE MobCDR.

/* Temp-table for CDR data */
DEFINE TEMP-TABLE ttCall NO-UNDO
  FIELD CLIType      AS CHARACTER
  FIELD TariffBundle AS CHARACTER
  FIELD Bigroup      AS CHARACTER
  FIELD BillCode     AS CHARACTER
  FIELD Price        AS DECIMAL
  FIELD BalanceType  AS CHARACTER
  INDEX Bigroup CLIType TariffBundle Bigroup billcode
  INDEX CliTypeBillCode CLIType TariffBundle billCode BalanceType.

FUNCTION fCollectBalance RETURNS LOGIC
   (icCLIType      AS CHAR,
    icTariffBundle AS CHAR,
    icBillCode     AS CHAR,
    idAmount       AS DEC,
    icBalanceType  AS CHAR):

   FIND FIRST ttCall WHERE
              ttCall.CLIType      = icCLIType AND
              ttCall.TariffBundle = icTariffBundle AND
              ttCall.BillCode     = icBillCode AND
              ttCall.BalanceType  = icBalanceType NO-ERROR.

   IF NOT AVAILABLE ttCall THEN DO:
      CREATE ttCall.
      ASSIGN ttCall.BillCode     = icBillCode
             ttCall.CLIType      = icCLIType
             ttCall.TariffBundle = icTariffBundle
             ttCall.BalanceType  = icBalanceType.

      FOR FIRST BillItem NO-LOCK WHERE
                BillItem.Brand = gcBrand AND
                BillItem.BillCode = icBillCode:
         ttCall.BIGroup = BillItem.BIGroup.
      END.
   END.

   ttCall.Price = ttCall.Price + idAmount.
   /* Only Monthly balance will go to total balance because
      daily already counted as part of monthly */
   IF icBalanceType = "balance" THEN
      ldBalance = ldBalance + idAmount.
    
END FUNCTION.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.

piMsSeq = get_pos_int(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST MobSub NO-LOCK WHERE
           MobSub.MsSeq = piMsSeq NO-ERROR.
IF NOT AVAIL MobSub THEN
   RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

IF NOT MobSub.PayType THEN
   ASSIGN lcIPLContracts   = fCParamC("IPL_CONTRACTS")
          lcBONOContracts  = fCParamC("BONO_CONTRACTS")
          lcFLATContracts  = fCParamC("FLAT_CONTRACTS")
          lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES")
          lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")        
          lcFirstMonthUsageBasedBundles = fCParamC("FIRST_MONTH_USAGE_BASED_BUNDLES").

tthCDR = TEMP-TABLE ttCDR:HANDLE.

ASSIGN
   first_of_month = DATE(MONTH(TODAY),1,YEAR(TODAY))
   ldaLastDay     = fLastDayOfMonth(TODAY)
   liPeriod       = YEAR(TODAY) * 100 + MONTH(TODAY)
   ldPeriodFrom   = fMake2Dt(first_of_month,0)
   ldPeriodTo     = fMake2Dt(ldaLastDay,86399)
   ldeCurrentTS   = fMakeTS().

main_level_struct = add_struct(response_toplevel_id, "").
   
EMPTY TEMP-TABLE ttCDR.

fGetMsOwnerTempTable(MobSub.Custnum,first_of_month,ldaLastDay,
                     TRUE,MobSub.PayType).

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
               INPUT first_of_month,
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

IF liErrorCodeOut = 0 AND MobSub.FixedNumber > "" THEN 
fMobCDRCollect(INPUT TRIM(STRING(MobSub.PayType,"pre/post")),
               INPUT gcBrand,
               INPUT "rpc",
               INPUT first_of_month,
               INPUT TODAY,
               INPUT 0,
               INPUT "",
               INPUT MobSub.FixedNumber,
               INPUT 0,
               INPUT 0,
               INPUT "",
               INPUT "",
               INPUT "",
               INPUT 0,
               INPUT-OUTPUT liErrorCodeOut,
               INPUT-OUTPUT tthCDR).

FOR EACH ttCDR NO-LOCK WHERE
         ttCDR.ErrorCode = 0,
   FIRST ttMsOwner WHERE
         ttMsOwner.CustNum   = ttCDR.CustNum AND
         ttMsOwner.MsSeq     = ttCDR.MsSeq   AND
         ttMsOwner.FromDate <= ttCDR.DateSt  AND
         ttMsOwner.ToDate   >= ttCDR.DateSt NO-LOCK:

   IF ttMsOwner.PayType EQ TRUE THEN DO:
      ldeCDRts = fMake2Dt(ttCDR.DateSt,ttCDR.TimeStart).

      IF NOT ttMsOwner.PeriodFrom <= ldeCDRts AND 
             ttMsOwner.PeriodTo   >= ldeCDRts THEN
      NEXT.       
   END.
 
   fCollectBalance(ttMsOwner.CLIType,
                   ttMsOwner.TariffBundle,
                   ttCDR.BillCode,
                   IF MobSub.PayType = FALSE 
                   THEN ttCDR.Amount
                   ELSE ttCDR.Charge,
                   "balance").
END.


IF MobSub.PayType EQ {&MOBSUB_PAYTYPE_POSTPAID} THEN DO:

   lcDSSBundleId = fGetActiveDSSId(MobSub.CustNum,ldeCurrentTS).

   FOR EACH ttMsOwner WHERE
            ttMsOwner.CustNum = MobSub.CustNum AND
            ttMsOwner.MsSeq   = MobSub.MsSeq NO-LOCK:

      ASSIGN lcCLIType = ttMsOwner.CLIType
             lcTariffBundle = ttMsOwner.TariffBundle.

      /* Get First Month Active Bundles */
      FIRST_MONTH_LOOP:
      FOR EACH MServiceLimit NO-LOCK WHERE
               MServiceLimit.MsSeq    = ttMsOwner.MsSeq  AND
               MServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
               MServiceLimit.FromTS  <= ttMsOwner.PeriodTo  AND
               MServiceLimit.EndTS   >= ttMsOwner.PeriodFrom,
         FIRST ServiceLimit NO-LOCK WHERE
               ServiceLimit.SlSeq   = MServiceLimit.SlSeq,
         FIRST DayCampaign NO-LOCK WHERE
               DayCampaign.Brand   = gcBrand AND
               DayCampaign.DCEvent = ServiceLimit.GroupCode AND
               LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0,
         FIRST FixedFee NO-LOCK USE-INDEX HostTable WHERE
               FixedFee.Brand     = gcBrand AND
               FixedFee.HostTable = "MobSub" AND
               FixedFee.KeyValue  = STRING(ttMsOwner.MsSeq) AND
               FixedFee.FeeModel  = DayCampaign.FeeModel AND
               FixedFee.CalcObj   = DayCampaign.DCEvent  AND
               FixedFee.InUse     = TRUE AND
               FixedFee.BegDate  <= ldaLastDay AND
               FixedFee.EndPer   >= liPeriod,
         FIRST FMItem NO-LOCK WHERE
               FMItem.Brand     = gcBrand AND
               FMItem.FeeModel  = FixedFee.FeeModel AND
               FMItem.FromDate <= FixedFee.BegDate  AND
               FMItem.ToDate   >= FixedFee.BegDate:
      
         IF LOOKUP(DayCampaign.DCEvent,lcExcludeBundles) > 0 OR
            DayCampaign.DCEvent = "DSS2" THEN NEXT.

         IF (FMItem.BrokenRental = 0 AND MServiceLimit.EndTS  < ldPeriodTo) OR
            (FMItem.FirstMonthBr = 0 AND MServiceLimit.FromTS > ldPeriodFrom) OR
            (MServiceLimit.FromTS <= ldPeriodFrom AND
             MServiceLimit.EndTS  >= ldPeriodTo) THEN
            NEXT FIRST_MONTH_LOOP.

         /* skip first month fee calculation for contracts originating
            from stc/btc. YBU-1326  */
         IF NOT lliSTC THEN
         FOR EACH MsRequest NO-LOCK WHERE
                  MsRequest.MsSeq   = ttMsOwner.MsSeq AND
                  MsRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} AND
                  MsRequest.ReqCParam3 = ServiceLimit.GroupCode AND
                  MsRequest.ActStamp >= ldPeriodFrom AND
                  MsRequest.ActStamp <= ldPeriodTo AND
                  MsRequest.OrigRequest > 0 USE-INDEX MsSeq,
            FIRST bMsRequest NO-LOCK WHERE
                  bMsRequest.MsRequest = MsRequest.OrigRequest:
            IF (bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR 
                bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}) AND
               bMsRequest.ActStamp = ldPeriodFrom THEN
               NEXT FIRST_MONTH_LOOP.
         END. /* FOR EACH MsRequest NO-LOCK WHERE */

         ASSIGN ldDataFee   = 0
                lcGroupCode = ServiceLimit.GroupCode.

         /* If data dundle is linked with DSS */
         IF lcDSSBundleId > "" THEN DO:
            IF ServiceLimit.GroupCode = {&DSS} THEN DO:
               /* Full Monthly fee will be charged */
               IF MServiceLimit.EndTS = ldPeriodTo OR
                  MServiceLimit.FromTS < ldPeriodFrom
               THEN NEXT FIRST_MONTH_LOOP.

               /* Charge full amount - Ongoing DSS termination request */
               IF fOngoingDSSTerm(MServiceLimit.CustNum,ldPeriodTo) THEN
                  ldDataFee = FixedFee.Amt.

            END. /* IF ServiceLimit.GroupCode = {&DSS} THEN DO: */
            ELSE IF lcDSSBundleId = "DSS2" AND
               LOOKUP(lcCLIType,lcAllowedDSS2SubsType) = 0 THEN DO:
               IF MServiceLimit.EndTS < ldPeriodTo THEN .
               ELSE
                  RUN pGetBundleFirstMonthFee(INPUT  MobSub.MsSeq,
                                              INPUT  lcGroupCode,
                                              BUFFER MServiceLimit).
            END. /* ELSE IF lcDSSBundleId = "DSS2" AND */
            ELSE DO:
               RUN pGetDSSLinkedSubsFee(INPUT  MobSub.CustNum,
                                        INPUT  MobSub.MsSeq,
                                        INPUT  liPeriod,
                                        INPUT  lcGroupCode,
                                        INPUT  lcDSSBundleId,
                                        OUTPUT ldDataFee).

               fCollectBalance(lcCLIType,lcTariffBundle,
                               FMItem.BillCode,ldDataFee,
                               "balance").

               lcFirstMonthFeeCalcObj = lcFirstMonthFeeCalcObj +
                                       (IF lcFirstMonthFeeCalcObj > "" THEN ","
                                        ELSE "") + lcGroupCode.
            END.
         END. /* IF lcDSSBundleId > "" THEN DO: */
         ELSE DO:
            IF MServiceLimit.EndTS < ldPeriodTo THEN .
            ELSE
               RUN pGetBundleFirstMonthFee(INPUT  MobSub.MsSeq,
                                           INPUT  lcGroupCode,
                                           BUFFER MServiceLimit).
         END. /* ELSE DO: */
      END. /* FOR EACH MServiceLimit NO-LOCK WHERE */
   END. /* FOR EACH ttMsOwner NO-LOCK: */

   /* Rest all fixed fee excluding first month fee */
   FOR EACH FixedFee USE-INDEX HostTable WHERE 
            FixedFee.Brand      = gcBrand AND 
            FixedFee.CustNum    = MobSub.InvCust AND 
            FixedFee.HostTable  = "Mobsub" AND
            FixedFee.KeyValue   = STRING(Mobsub.MsSeq) AND
            FixedFee.EndPeriod >= liPeriod AND
            FixedFee.InUse      = TRUE AND
            LOOKUP(FixedFee.CalcObj,lcFirstMonthFeeCalcObj) = 0 NO-LOCK,
      FIRST FFItem OF FixedFee NO-LOCK WHERE
            FFItem.BillPeriod = liPeriod:

      ASSIGN ldaItemFromDate = fInt2Date(FFItem.Concerns[1],0)
             ldaItemToDate   = fInt2Date(FFItem.Concerns[2],0).

      FIND FIRST ttMsOwner WHERE
                 ttMsOwner.CustNum   = MobSub.CustNum AND
                 ttMsOwner.MsSeq     = MobSub.MsSeq   AND
                 ttMsOwner.FromDate <= ldaItemToDate  AND
                 ttMsOwner.ToDate   >= ldaItemFromDate NO-LOCK NO-ERROR.
      IF AVAIL ttMsOwner THEN
         fCollectBalance(ttMsOwner.CLIType,ttMsOwner.TariffBundle,
                         FFItem.BillCode,FFItem.Amt,"balance").
      ELSE
         fCollectBalance(MobSub.CLIType,MobSub.TariffBundle,
                         FFItem.BillCode,FFItem.Amt,"balance").
   END.

   /* data upsell single fees */
   FOR EACH SingleFee USE-INDEX Custnum WHERE
            SingleFee.Brand = gcBrand AND
            SingleFee.Custnum = Mobsub.InvCust AND
            SingleFee.HostTable = "Mobsub" AND
            SingleFee.KeyValue = STRING(Mobsub.MsSeq) AND
            SingleFee.BillPeriod = liPeriod NO-LOCK:

      ldaItemFromDate = fInt2Date(SingleFee.Concerns[1],0).
      ldDiscountAmt = 0.
      llExtFound = FALSE.

      /* YPR-3507 Q25 consumption Vista visibility */
      IF SingleFee.Billcode BEGINS "RVTERM" AND
         SingleFee.CalcObj EQ "RVTERM" AND
         SingleFee.SourceTable = "DCCLI" THEN DO:
         ASSIGN         
            ldaToDate = fLastDayOfMonth(TODAY)
            ldaFromDate = date(month(ldaToDate),1,year(ldaToDate)).

         IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                           MsRequest.msSeq EQ mobsub.msseq AND
                           MsRequest.reqtype EQ
                              {&REQTYPE_CONTRACT_ACTIVATION} AND
                           MsRequest.reqStatus EQ
                              {&REQUEST_STATUS_NEW} AND
                           MsRequest.ReqCParam3 EQ "RVTERM12" AND
                           MsRequest.ReqIParam3 EQ INT(SingleFee.sourcekey)) 
            THEN llExtFound = TRUE. /* ongoin request found */
         IF llExtFound THEN NEXT.
         lcDiscounts = "RVTERMDT1DISC,RVTERMDT2DISC,RVTERMDT3DISC,RVTERMDT4DISC".
         DiscountsLoop:
         DO liLoop = 1 TO NUM-ENTRIES(lcDiscounts): 
            FIND FIRST DiscountPlan WHERE Discountplan.dpruleid EQ 
                                          ENTRY(liLoop,lcDiscounts) 
                                          NO-LOCK NO-ERROR.
            IF AVAIL DiscountPlan THEN DO:
               FIND FIRST DPMember WHERE
                          DPMember.DPId EQ Discountplan.DPId AND
                          DPMember.HostTable EQ "MobSub" AND
                          DPMember.KeyValue EQ SingleFee.KeyValue AND
                          DPMember.OrderId EQ SingleFee.orderId AND
                          DPMember.ValidFrom LE ldaToDate AND
                          DPMemBer.ValidTo GE ldaFromDate AND
                          DPMemBer.ValidTo GE DPMember.ValidFrom
                          NO-LOCK NO-ERROR.
               IF AVAIL DPMember THEN DO: 
                  /* only one discount should be found */
                  ldDiscountAmt = ldDiscountAmt + DPMember.discValue.
               END.
            END.
         END.
      END.   
      /* 0 row is not needed to show */
      IF (SingleFee.Amt - ldDiscountAmt LE 0) THEN NEXT.
      FIND FIRST ttMsOwner WHERE
                 ttMsOwner.CustNum   = MobSub.CustNum AND
                 ttMsOwner.MsSeq     = MobSub.MsSeq AND
                 ttMsOwner.FromDate <= ldaItemFromDate AND
                 ttMsOwner.ToDate   >= ldaItemFromDate NO-LOCK NO-ERROR.
      IF AVAIL ttMsOwner THEN
         fCollectBalance(ttMsOwner.CLIType,ttMsOwner.TariffBundle,
                         SingleFee.BillCode,
                         SingleFee.Amt - ldDiscountAmt,
                         "balance").
      ELSE
         fCollectBalance(MobSub.CLIType,MobSub.TariffBundle,
                         SingleFee.BillCode,
                         SingleFee.Amt - ldDiscountAmt,
                         "balance").
   END.

END. /* IF NOT MobSub.PayType THEN DO: */
ELSE DO:
   IF MobSub.CLIType EQ "TARJ6" THEN DO:
      fGetTARJ6DataLimitAndCharges
         (MobSub.MsSeq,
          OUTPUT ldeTotalDataBundleLimit,
          OUTPUT ldeTARJ6UpsellChargeMonth,
          OUTPUT ldeTARJ6UpsellChargeDay,
          OUTPUT ldeTARJ6DailyChargeMonth,
          OUTPUT ldeTARJ6DailyChargeDay).

      fCollectBalance(MobSub.CLIType,MobSub.TariffBundle,
                      "PRETARJ6DBUNDLE",ldeTARJ6DailyChargeMonth,"balance").
      fCollectBalance(MobSub.CLIType,MobSub.TariffBundle,
                      "PRETARJ6DBUNDLE",ldeTARJ6DailyChargeDay,"balance_day").
      fCollectBalance(MobSub.CLIType,MobSub.TariffBundle,
                      "PRETARJ6UPSELL",ldeTARJ6UpsellChargeMonth,"balance").
      fCollectBalance(MobSub.CLIType,MobSub.TariffBundle,
                      "PRETARJ6UPSELL",ldeTARJ6UpsellChargeDay,"balance_day").
   END.
   ELSE IF MobSub.CliType = "TARJ7" OR MobSub.CliType = "TARJ9" THEN DO:
      FOR FIRST ServiceLimit NO-LOCK WHERE
                ServiceLimit.GroupCode = MobSub.CliType:
         IF CAN-FIND (FIRST MServiceLimit WHERE
                            MServiceLimit.MsSeq = Mobsub.MsSeq AND
                            MServiceLimit.DialType = ServiceLimit.DialType AND
                            MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
                            MServiceLimit.EndTS >= ldeCurrentTS NO-LOCK)
         THEN DO:
            ASSIGN ldeBundleFee = fgetPrepaidFeeAmount(MobSub.CliType, TODAY).

            fCollectBalance(MobSub.CLIType,MobSub.TariffBundle,
                            "PRE" + MobSub.CliType + "BUNDLE",ldeBundleFee,"balance").

            /* Check if there is any active prepaid upsell */
            liUpsellCount = fGetUpSellCount(INPUT "TARJ7_UPSELL",
                                            INPUT Mobsub.MsSeq,
                                            INPUT Mobsub.Custnum,
                                            OUTPUT lcError).
            IF liUpsellCount > 0 THEN DO:
               ldeUpsellFee = fgetPrepaidFeeAmount("TARJ7_UPSELL", TODAY).
               ldeUpsellFee = ldeUpsellFee * liUpsellCount.
               fCollectBalance(MobSub.CLIType,MobSub.TariffBundle,
                               "PRETARJ7UPSELL",ldeUpsellFee,"balance").
            END. /* IF liUpsellCount > 0 THEN DO: */
         END. /* IF CAN-FIND (FIRST MServiceLimit WHERE */
      END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */
   END. /* ELSE IF MobSub.CliType = "TARJ7" THEN DO: */
   ELSE DO:
      FOR FIRST ServiceLimit NO-LOCK WHERE
                ServiceLimit.GroupCode = {&PMDUB}:
         IF CAN-FIND (FIRST MServiceLimit WHERE
                            MServiceLimit.MsSeq = Mobsub.MsSeq AND
                            MServiceLimit.DialType = ServiceLimit.DialType AND
                            MServiceLimit.SLSeq = ServiceLimit.SLSeq AND
                            MServiceLimit.EndTS >= ldeCurrentTS NO-LOCK)
         THEN DO:
            ASSIGN ldeBundleFee = fgetPrepaidFeeAmount("PMDUB", TODAY).

            fCollectBalance(MobSub.CLIType,MobSub.TariffBundle,
                            "PREMDUB",ldeBundleFee,"balance").

            /* Check if there is any active prepaid upsell */
            liUpsellCount = fGetUpSellCount(INPUT {&PMDUB} + "_UPSELL",
                                            INPUT Mobsub.MsSeq,
                                            INPUT Mobsub.Custnum,
                                            OUTPUT lcError).
            IF liUpsellCount > 0 THEN DO:
               ldeUpsellFee = fgetPrepaidFeeAmount("PMDUB_UPSELL", TODAY).
               ldeUpsellFee = ldeUpsellFee * liUpsellCount.
               fCollectBalance(MobSub.CLIType,MobSub.TariffBundle,
                               "PREMDUB",ldeUpsellFee,"balance").
            END. /* IF liUpsellCount > 0 THEN DO: */
         END. /* IF CAN-FIND(FIRST MServiceLimit WHERE */
      END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */
   END.
END. /* ELSE DO: */

FOR EACH ttMsOwner WHERE
         ttMsOwner.CustNum = MobSub.CustNum AND
         ttMsOwner.MsSeq   = MobSub.MsSeq NO-LOCK:

    IF ttMsOwner.CLIType = MobSub.CLIType AND
       ttMsOwner.TariffBundle = MobSub.TariffBundle THEN
       first_level_struct = add_struct(main_level_struct,"subscription").
    ELSE
       first_level_struct = add_struct(main_level_struct,"old_subscription").

    ASSIGN lcCLIType = ttMsOwner.CLIType + ttMsOwner.TariffBundle
           ldeTotalBalance = 0.

    second_level_struct = add_struct(first_level_struct,lcCLIType).
    second_level_array = add_array(second_level_struct, "billing_groups").

    FOR EACH ttCall NO-LOCK WHERE
             ttCall.CLIType      = ttMsOwner.CLIType AND
             ttCall.TariffBundle = ttMsOwner.TariffBundle
       BREAK BY ttCall.BIGroup
             BY ttCall.BillCode:

       IF FIRST-OF(ttCall.BIGroup) THEN DO:
          ldeGroupBalance = 0.
          third_level_struct = add_struct(second_level_array, "").
          add_string(third_level_struct, "tms_id", ttCall.BIGroup).
          third_level_array = add_array(third_level_struct, "billing_items").
       END.

       forth_level_struct = add_struct(third_level_array, "").
       add_string(forth_level_struct, "billing_code", ttCall.BillCode).
       add_double(forth_level_struct, ttCall.BalanceType, ttCall.Price).

       /* Only Monthly balance will go to total balance because
          daily already counted as part of monthly */
       IF ttCall.BalanceType = "balance" THEN
          ASSIGN ldeGroupBalance = ldeGroupBalance + ttCall.Price
                 ldeTotalBalance = ldeTotalBalance + ttCall.Price.
       ELSE
          ldeGroupDailyBalance = ldeGroupDailyBalance + ttCall.Price.

       IF LAST-OF(ttCall.BIGroup) THEN DO:
          add_double(third_level_struct, "balance", ldeGroupBalance).
          IF ldeGroupDailyBalance > 0 THEN
             add_double(third_level_struct, "balance_day", ldeGroupDailyBalance).
       END.
    END.
    add_double(second_level_struct, "balance", ldeTotalBalance).
END.

add_double(main_level_struct, "total_balance", ldBalance).

FINALLY:
   EMPTY TEMP-TABLE ttCDR.
   EMPTY TEMP-TABLE ttCall.
   EMPTY TEMP-TABLE ttSub.
   EMPTY TEMP-TABLE ttMsOwner.
   IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.

PROCEDURE pGetBundleFirstMonthFee:

   DEF INPUT PARAMETER iiMsSeq         AS INT  NO-UNDO.
   DEF INPUT PARAMETER icDcEvent       AS CHAR NO-UNDO.
   DEF PARAMETER BUFFER bMServiceLimit FOR MServiceLimit.

   DEF VAR ldeUsedData                 AS DEC  NO-UNDO.

   /* first month's fixed fee is calculated based on usage */
   IF AVAILABLE bMServiceLimit THEN DO:

      FOR FIRST DayCampaign NO-LOCK WHERE
                DayCampaign.Brand   = gcBrand AND 
                DayCampaign.DCEvent = icDcEvent,
          FIRST FixedFee NO-LOCK USE-INDEX HostTable WHERE
                FixedFee.Brand     = gcBrand AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue  = STRING(MobSub.MsSeq) AND
                FixedFee.FeeModel  = DayCampaign.FeeModel AND
                FixedFee.CalcObj   = DayCampaign.DCEvent AND
                FixedFee.InUse     = TRUE AND
                FixedFee.BegDate  >= first_of_month AND
                FixedFee.BegDate  <= ldaLastDay AND
                FixedFee.EndPer   >= liPeriod,
          FIRST FFItem OF FixedFee NO-LOCK WHERE
                FFItem.BillPeriod = liPeriod,
          FIRST FMItem NO-LOCK WHERE
                FMItem.Brand     = gcBrand AND
                FMItem.FeeModel  = FixedFee.FeeModel AND
                FMItem.FromDate <= FixedFee.BegDate  AND
                FMItem.ToDate   >= FixedFee.BegDate  AND
                FMItem.FirstMonthBr = 2: /* Usage based */
             
         ASSIGN ldDataFee   = FFItem.Amt
                ldeUsedData = 0.

         /* First month always usage based */
         IF LOOKUP(icDcEvent,lcFirstMonthUsageBasedBundles) > 0 THEN .

         /* Ongoing bundle termination request except BONO/IPL BTC Request */
         ELSE IF CAN-FIND(FIRST MSRequest WHERE
                     MSRequest.MsSeq = bMServiceLimit.MsSeq AND
                     MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                     MsRequest.ActStamp = ldPeriodTo AND
                     MsRequest.ReqCParam3 = icDcEvent AND
                     MsRequest.ReqSource <> {&REQUEST_SOURCE_BTC} AND
                     LOOKUP(STRING(MsRequest.ReqStat),"4,99") = 0
                     USE-INDEX MsSeq) OR
            CAN-FIND(FIRST MSRequest WHERE
                     MSRequest.MsSeq = bMServiceLimit.MsSeq AND
                     MsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                     MsRequest.ActStamp = ldPeriodTo AND
                     MsRequest.ReqCParam3 = icDcEvent AND
                     LOOKUP(MsRequest.ReqCParam3,lcBONOContracts) = 0 AND
                     LOOKUP(MsRequest.ReqCParam3,lcIPLContracts)  = 0 AND
                     MsRequest.ReqSource = {&REQUEST_SOURCE_BTC}
                     USE-INDEX MsSeq)
         THEN DO:
            fCollectBalance(lcCLIType,lcTariffBundle,
                            FFItem.BillCode,ldDataFee,"balance").
            lcFirstMonthFeeCalcObj = lcFirstMonthFeeCalcObj + 
                                     (IF lcFirstMonthFeeCalcObj > "" THEN ","
                                      ELSE "") + FixedFee.CalcObj.
            RETURN.
         END. /* IF CAN-FIND(FIRST MSRequest WHERE */

         FIND FIRST ServiceLCounter NO-LOCK WHERE
                    ServiceLCounter.MsSeq  = Mobsub.MsSeq AND
                    ServiceLCounter.SlSeq  = bMServiceLimit.SlSeq AND
                    ServiceLCounter.Period = liPeriod NO-ERROR.
         IF AVAILABLE ServiceLCounter THEN
            ldeUsedData = ServiceLCounter.Amt.
         
         ldDataFee = fCalculateProportionalFee(ldeUsedData,
                                               bMServiceLimit.InclUnit,
                                               bMServiceLimit.InclAmt,
                                               FixedFee.Amt).
      
         fCollectBalance(lcCLIType,lcTariffBundle,
                         FFItem.BillCode,ldDataFee,"balance").
         lcFirstMonthFeeCalcObj = lcFirstMonthFeeCalcObj + 
                                  (IF lcFirstMonthFeeCalcObj > "" THEN ","
                                   ELSE "") + FixedFee.CalcObj.

      END. /* FOR FIRST DayCampaign NO-LOCK WHERE */
   END. /* IF AVAILABLE bMServiceLimit AND */

END PROCEDURE. /* PROCEDURE pGetBundleFirstMonthFee: */
