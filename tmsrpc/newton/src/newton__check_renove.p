/**
 * Initial search for a subscription for renove orders
 *
 * @input: brand;string;mandatory;Tenant to check for subscription
           msisdn;str;mandatory;Subscription msisdn 
           id_type;str;optional;Customer id type (Representative person if company_id is given) 
           person_id;str;optional;Customer id (Representative person if company_id is given)
           company_id;str;optional;Company id
           channel;str;mandatory;order channel
           bypass;boolean;optional;skip some business rules (YDR-90)
           offer_id;str;optional;offer id
 * @output: data;struct 
 * @data  subscription_type;string;mandatory;subscription type (eg. CONT2)
          corporate_customer;bool;mandatory;true <=> corporate customer
          contract_days_remain;int;optional;remaining contract days
          contract_penalty;double;optional;a possible penalty in euros
          person_id;string;mandatory;person id of orderer
          id_type;string;mandatory;person id type of orderer
          has_terminal;bool;optional;terminal has been ordered before
          number_type;string;optional;new/mnp (original order type)
          allowed_terminal_financing_amount;double;optional;
          pending_stc;boolean;optional;true
          liQtyTFs;int;mandatory;number of installments
          subscription_bundle;string;mandatory;tariff bundle of mobsub
          additional_line_discount_50;boolean;optional;
 * @installment;array of installment structs;two fields insidde struct
 * @installment;struct per_contract_id;int;installment periodical contract id
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:katun = "NewtonRPC".
Syst.Var:gcBrand = "1".
{Func/penaltyfee.i}
{Func/orderchk.i}
{Func/fcustpl.i}
{Func/fixedfee.i}
{Func/cparam2.i}
{Func/barrfunc.i}

DEF VAR lcPostpaidVoiceTariffs AS CHAR NO-UNDO.
DEF VAR lcPrepaidVoiceTariffs  AS CHAR NO-UNDO.

FUNCTION fMatchOfferCriterias RETURN LOGICAL 
         (INPUT pcPayType AS CHARACTER,
          INPUT pcCLIType AS CHARACTER,
          INPUT pcOfferId AS CHARACTER,
          INPUT pcRenewalSegment AS CHAR,
          OUTPUT ocError AS CHARACTER):

         DEFINE VARIABLE llAllowed AS LOGICAL NO-UNDO INITIAL FALSE.
         DEF VAR ldeNow AS DEC NO-UNDO. 
         ldeNow = Func.Common:mMakeTS().

         FIND Offer WHERE 
              Offer.Brand = Syst.Var:gcBrand AND 
              Offer.Offer = pcOfferId AND 
              Offer.FromDate <= TODAY AND 
              Offer.ToDate >= TODAY NO-LOCK NO-ERROR. 
         IF NOT AVAIL Offer THEN DO:
            ocError = "Offer not found".
            RETURN FALSE.
         END.

         FOR EACH OfferCriteria NO-LOCK WHERE
                  OfferCriteria.Brand = Syst.Var:gcBrand AND 
                  OfferCriteria.Offer = Offer.Offer AND
                  OfferCriteria.BeginStamp <= ldeNow AND 
                  OfferCriteria.EndStamp >= ldeNow AND 
                  LOOKUP(OfferCriteria.CriteriaType,
                         "RenewalSegment,PayType,CLIType") > 0 :

                  IF OfferCriteria.CriteriaType = "PayType" AND 
                     LOOKUP(pcPayType,OfferCriteria.IncludedValue) = 0
                  THEN DO:
                     IF pcPayType = "2" THEN 
                        ocError = "prepaid_tries_postpaid".
                     ELSE ocError = "postpaid_tries_prepaid".
                     RETURN FALSE.
                  END.

                  IF OfferCriteria.CriteriaType = "RenewalSegment" AND 
                     LOOKUP(pcRenewalSegment,OfferCriteria.IncludedValue) = 0 
                  THEN DO:
                    ocError = "invalid_subscription_renewal_segment".
                    RETURN FALSE. 
                  END.

                  IF OfferCriteria.CriteriaType = "CLIType" THEN DO:
               
                     if OfferCriteria.includedvalue eq "ALL_VOICE" THEN DO:
                        if lookup(pcCliType,lcPostpaidVoiceTariffs + "," +
                                            lcPrepaidVoiceTariffs) = 0 then do:
                           ocError = "invalid_subscription_type".
                           RETURN FALSE.
                        END.
                     end.
                     else if LOOKUP(pcCLIType,OfferCriteria.IncludedValue) = 0 
                     THEN DO:
                       ocError = "invalid_subscription_type".
                       RETURN FALSE. 
                     END.
                  END.

                  llAllowed = TRUE.
         END.

         RETURN llAllowed. 

END FUNCTION.


/* Input parameters */
DEF VAR pcStruct AS CHARACTER NO-UNDO. 
DEF VAR pcTenant AS CHAR NO-UNDO.
DEF VAR pcCLI AS CHAR NO-UNDO.
DEF VAR pcIdType AS CHAR NO-UNDO INIT ?.
DEF VAR pcPersonId AS CHAR NO-UNDO INIT ?.
DEF VAR pcCIF AS CHAR NO-UNDO INIT ?.
DEF VAR pcChannel AS CHARACTER NO-UNDO. 
DEF VAR plBypass AS LOGICAL NO-UNDO.
DEF VAR pcOfferId AS CHARACTER NO-UNDO. 
/* Output parameters */
DEF VAR top_struct AS CHAR NO-UNDO.
DEF VAR result_array AS CHAR NO-UNDO.
DEF VAR sub_struct AS CHAR NO-UNDO.
/* Local variables */
DEF VAR lcFields AS CHARACTER NO-UNDO. 
DEF VAR ldeCurrPen AS DEC NO-UNDO.
DEF VAR ldaLastTerminal AS DATE NO-UNDO INIT ?. 
DEF VAR liTime AS INT NO-UNDO. 
DEF VAR ldePrice AS DEC NO-UNDO.
DEF VAR lcPriceList AS CHAR NO-UNDO.
DEF VAR liRemPeriod AS INT NO-UNDO.
DEF VAR llPreactivated AS LOGICAL NO-UNDO.
DEF VAR llBarrings AS LOGICAL NO-UNDO.
DEF VAR lcBarrStatus AS CHARACTER NO-UNDO. 
DEF VAR llPrerenove AS LOGICAL NO-UNDO INIT FALSE.
DEF VAR lcError AS CHAR NO-UNDO.
DEF VAR ldePendingFee AS DECIMAL NO-UNDO. 
DEF VAR lderesidualFee AS DECIMAL NO-UNDO. 
DEF VAR liTotalPeriods AS INTEGER NO-UNDO. 
DEF VAR ldePeriodFee AS DECIMAL NO-UNDO. 
DEF VAR liConfigDays AS INT NO-UNDO.
DEF VAR llCancelledPrerenove AS LOG NO-UNDO.
DEF VAR lcFinancedInfo AS CHAR NO-UNDO. 
DEF VAR liOrderId AS INT NO-UNDO. 
DEF VAR liQtyTFs AS INT NO-UNDO. 

DEF VAR installment_array AS CHAR NO-UNDO.
DEF VAR installment_struct AS CHAR NO-UNDO. 

DEF VAR llDefBarring       AS LOG NO-UNDO. 
DEF VAR ldtFirstDay        AS DATE NO-UNDO.
DEF VAR ldePendingFees AS DECIMAL NO-UNDO.
DEF VAR liPeriod AS INT NO-UNDO. 
DEF VAR ldaDate AS DATE NO-UNDO. 

DEF BUFFER bServiceRequest FOR MSRequest.
DEF BUFFER bMobSub FOR MobSub.

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.

pcstruct = get_struct(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcFields = validate_request(pcstruct,
   "brand!,msisdn!,person_id,id_type,company_id,channel!,offer_id,bypass").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   pcTenant = get_string(pcStruct, "brand") 
   pcCLI = get_string(pcStruct, "msisdn")
   pcIdType = get_string(pcStruct, "id_type") WHEN LOOKUP ("id_type", lcFields) > 0
   pcPersonId = get_string(pcStruct, "person_id") WHEN LOOKUP ("person_id", lcFields) > 0
   pcCIF = get_string(pcStruct, "company_id") WHEN LOOKUP ("company_id", lcFields) > 0
   pcChannel = get_string(pcStruct, "channel")
   plBypass = get_bool(pcStruct, "bypass") WHEN LOOKUP("bypass", lcFields) > 0
   pcOfferId = get_string(pcStruct, "offer_id") WHEN LOOKUP("offer_id",lcFields) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant} 

ASSIGN lcPostpaidVoiceTariffs           = fCParamC("POSTPAID_VOICE_TARIFFS")
       lcPrepaidVoiceTariffs            = fCParamC("PREPAID_VOICE_TARIFFS").

FIND Mobsub WHERE 
     Mobsub.Brand = Syst.Var:gcBrand AND
     Mobsub.CLI = pcCLI NO-LOCK NO-ERROR.

IF NOT AVAIL Mobsub THEN
   RETURN appl_err("number_not_valid").

FIND FIRST CliType WHERE CliType.Brand = Syst.Var:gcBrand AND CliType.CliType = MobSub.CliType NO-LOCK NO-ERROR.
IF AVAIL CliType AND CliType.TariffType = {&CLITYPE_TARIFFTYPE_FIXEDONLY} THEN  
    RETURN appl_err("renewal_not_allowed_for_fixed_only").

FIND FIRST Segmentation NO-LOCK WHERE
           Segmentation.MsSeq = MobSub.MsSeq NO-ERROR.

IF NOT AVAIL Segmentation THEN RETURN appl_err("general").

IF NOT pcChannel BEGINS "retention" AND
   Mnp.MNPOutGoing:mIsMNPOutOngoing(mobsub.cli) THEN RETURN appl_err("ongoing_mnp_out_request").

IF pcOfferId > "" AND
   NOT fMatchOfferCriterias((IF MobSub.PayType = TRUE THEN "2" ELSE "1"),
                             MobSub.CLIType,
                             pcOfferId,
                             Segmentation.SegmentOffer,
                             OUTPUT lcError) THEN DO:
   RETURN appl_err(lcError).
END.


IF Mobsub.PayType = FALSE AND
   LOOKUP(pcChannel,"renewal_pos_stc,retention_stc") > 0 THEN
   RETURN appl_err("general").
   
FIND Customer WHERE
     Customer.Brand   = Syst.Var:gcBrand AND
     Customer.Custnum = Mobsub.Custnum NO-LOCK NO-ERROR.

IF NOT AVAIL Customer THEN
   RETURN appl_err("customer_not_found").

&SCOPED-DEFINE ERR_RENOVE "no_match"

/* renewal pos check */
IF pcChannel BEGINS "renewal_pos" THEN DO:
   
   IF pcCIF EQ ? THEN DO:
      IF Customer.CustIdType NE pcIdType OR 
         Customer.OrgId NE pcPersonId THEN DO:
         RETURN appl_err({&ERR_RENOVE}).
      END.
   END.
   ELSE DO:
      
      IF Customer.CustIdType NE "CIF" OR
         Customer.OrgId NE pcCIF THEN DO:
         RETURN appl_err({&ERR_RENOVE}).
      END.
      
      IF Customer.AuthCustIdType NE pcIdType OR
         Customer.AuthCustId NE pcPersonId THEN DO:
         RETURN appl_err({&ERR_RENOVE}).
      END.
   END.
END.
   
/* ongoing ACC prevents renove orders */
IF CAN-FIND(FIRST MsRequest WHERE
                  MsRequest.MsSeq = MobSub.MsSeq AND
                  MsRequest.ReqType = 10 AND
                  LOOKUP(STRING(MsRequest.ReqStat),"2,4,9") = 0)
THEN RETURN appl_err("ongoing_acc").

/* ongoing renove contract request */
FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.MsSeq = MobSub.MsSeq AND
         MsRequest.ReqType = 46,
    EACH bServiceRequest NO-LOCK WHERE
         bServiceRequest.OrigRequest = MsRequest.MsRequest AND
         LOOKUP(STRING(bServiceRequest.ReqStat),"2,4,9") = 0:
   RETURN appl_err("general").
END.

/* NEW or MNP number */
FIND FIRST Order NO-LOCK WHERE 
   Order.MsSeq = Mobsub.MsSeq AND
   Order.StatusCode EQ {&ORDER_STATUS_DELIVERED} AND
   Order.OrderType < 2 NO-ERROR.
IF NOT AVAIL Order THEN
   RETURN appl_err("order_not_found").

/* Check barrings */
IF Mobsub.PayType EQ FALSE AND NOT plBypass THEN DO:
    FOR EACH bMobsub NO-LOCK WHERE
             bMobsub.Brand = Syst.Var:gcBrand AND
             bMobsub.AgrCust = Customer.AgrCust AND
             bMobsub.PayType = FALSE AND
             bMobsub.MsStatus = 8:
      lcBarrStatus = fCheckStatus(bMobsub.MsSeq).
      llBarrings = (LOOKUP(lcBarrStatus, {&FRAUD_BARR_CODES}) > 0 ).
      /* Here is used old error code, to avoid hassle elsewhere(TMS/WEB) */
      IF llBarrings THEN RETURN appl_err("pos_stc_barrings").
   
    END.

END.

/* Check if customer has any debt invoice  */

ldtFirstDay = DATE(MONTH(ADD-INTERVAL(TODAY,-12,"months") + 1),
                         1,
                    YEAR(ADD-INTERVAL(TODAY,-12,"months") + 1)).      
INVSEARCH_LOOP:
FOR EACH Invoice NO-LOCK WHERE
         Invoice.Brand    = Syst.Var:gcBrand          AND
         Invoice.Custnum  = Customer.Custnum AND
         Invoice.InvType  = 1                AND 
         Invoice.InvDate <= TODAY            AND 
         Invoice.InvDate >= ldtFirstDay      AND
         Invoice.DueDate  < TODAY - 2:
   IF Invoice.PaymState NE 2            AND
      Invoice.InvAmt     > 0            THEN DO:
      llDefBarring = TRUE. /* unpaid invoice */
      LEAVE INVSEARCH_LOOP.
   END.
   llDefBarring = FALSE. /* invoice(s) found */
END.

IF llDefBarring THEN RETURN appl_err("pos_stc_barrings").
      
/* check preactivated subscription */
IF Mobsub.CliType = "TARJ3" 
   OR fIsPreactivatedCustomer(Customer.Custnum) THEN DO:
   llPreactivated = TRUE.
END.


IF llPreactivated OR
   fOngoingOrders(MobSub.Cli,(IF pcChannel BEGINS "retention"
                              THEN "retention"
                              ELSE "renewal")) THEN
   RETURN appl_err("general").

/* validations ends */

top_struct = add_struct(response_toplevel_id, "").

/* Check ongoing STC request */
FIND FIRST MsRequest WHERE
           MsRequest.MsSeq = MobSub.MsSeq AND
           MsRequest.ReqType = 0 AND
           LOOKUP(STRING(MsRequest.ReqStat),"2,4,9") = 0 NO-LOCK NO-ERROR.
IF AVAIL MsRequest THEN DO:
   IF LOOKUP(pcChannel,"renewal_pos_stc,retention_stc") > 0 THEN
      add_boolean(top_struct, "pending_stc", True).
END. /* IF AVAIL MsRequest THEN DO: */
   
/* get the latest purchased terminal (phone) */
FIND FIRST SubsTerminal WHERE
           SubsTerminal.MsSeq = MobSub.MsSeq AND
           SubsTerminal.TerminalType = ({&TERMINAL_TYPE_PHONE})
   NO-LOCK USE-INDEX MsSeq NO-ERROR.

/* yot-903 */
IF AVAIL SubsTerminal THEN DO:

   IF Mobsub.PayType = True AND
      SubsTerminal.OrderID > 0 AND
      CAN-FIND(FIRST Order NO-LOCK WHERE
                     Order.Brand = Syst.Var:gcBrand AND
                     Order.OrderID = SubsTerminal.OrderId AND
                     Order.OrderType = 2) THEN DO:
      llPrerenove = TRUE.

      /* Check cancelled same renewal order */
      IF CAN-FIND(FIRST MsRequest WHERE
                        MsRequest.MsSeq = Mobsub.MsSeq AND
                        MsRequest.ReqType = {&REQTYPE_REVERT_RENEWAL_ORDER} AND
                        MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
                        MsRequest.ReqIParam1 = SubsTerminal.OrderID NO-LOCK)
      THEN llCancelledPrerenove = TRUE.
   END. /* IF Mobsub.PayType = True AND */
   
   Func.Common:mSplitTS(SubsTerminal.PurchaseTS, OUTPUT ldaLastTerminal, OUTPUT liTime).

   /* YTS-3465 */
   IF SubsTerminal.PerContractID > 0 THEN DO:
      FIND FIRST DCCLI WHERE
                 DCCLI.MsSeq = Mobsub.MsSeq AND
                 DCCLI.PerContractID = SubsTerminal.PerContractID
      NO-LOCK NO-ERROR.
      IF NOT AVAIL DCCLI OR DCCLI.ValidTo < TODAY THEN ldaLastTerminal = ?.
   END.
END.

add_string(top_struct,"number_type",STRING(Order.MNPStatus EQ 0, "new/mnp")).
add_boolean(top_struct, "has_terminal", (AVAIL Substerminal)).

/* Search active terminal contract with penalty fee */
CONTRACT_LOOP:
FOR EACH DCCLI WHERE
         DCCLI.Brand = Syst.Var:gcBrand AND
         DCCLI.MsSeq = Mobsub.Msseq AND
         DCCLI.ValidTo >= TODAY AND
         DCCLI.ValidFrom <= TODAY AND
         DCCLI.CreateFees = TRUE NO-LOCK,
   FIRST DayCampaign WHERE
         DayCampaign.Brand = Syst.Var:gcBrand AND
         DayCampaign.DCEvent = DCCLI.DCEvent AND
         DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
         DayCampaign.TermFeeCalc > 0 NO-LOCK:
   
   IF DayCampaign.DCEvent BEGINS "FTERM" THEN NEXT.
   /* Count possible penalty fee for contract termination */
   IF DCCLI.Amount NE ? THEN ldePrice = DCCLI.Amount.
   ELSE DO:
      lcPriceList = fFeeModelPriceList(MobSub.Custnum,
                                       MobSub.BillTarget,
                                       DayCampaign.TermFeeModel,
                                       TODAY).

      FIND FIRST FMItem NO-LOCK  WHERE
                 FMItem.Brand     = Syst.Var:gcBrand       AND
                 FMItem.FeeModel  = DayCampaign.TermFeeModel AND
                 FMItem.PriceList = lcPriceList AND
                 FMItem.FromDate <= TODAY     AND
                 FMItem.ToDate   >= TODAY NO-ERROR.

      IF AVAIL fmitem THEN ldePrice = fmitem.amount.
   END.

   liRemPeriod = DCCLI.ValidTo - TODAY + 1.
   IF liRemPeriod < 0 THEN liRemPeriod = 0.

   /* calculate a factor for the fee (full / proportional) */
   ldeCurrPen = fCalculateFactor(DCCLI.ValidFrom,
                                 DCCLI.RenewalDate,
                                 DCCLI.ValidTo,
                                 DCCLI.ValidToOrig,
                                 TODAY - 1,
                                 DayCampaign.TermFeeCalc).

   ldeCurrPen = TRUNCATE(ldeCurrPen * ldePrice,0).

   /* only one terminal contract should be active */
   LEAVE CONTRACT_LOOP.
   
END.

/* YDR-532 - New Renewal Rules */
IF ldaLastTerminal NE ? THEN DO:
   liConfigDays = 180.

   IF MobSub.PayType THEN DO:
      IF pcChannel <> "renewal_pos_stc" THEN liConfigDays = 360.

      /* If renewal order is cancelled then it should be allowed */
      IF llPrerenove AND llCancelledPrerenove THEN .
      ELSE IF (TODAY - ldaLastTerminal) < liConfigDays AND NOT plBypass
      THEN DO:
         IF liConfigDays = 180 THEN
            RETURN appl_err("minimum_time_not_exceeded|6").
         ELSE
            RETURN appl_err("minimum_time_not_exceeded|12").
      END. /* IF (TODAY - ldaLastTerminal) < liConfigDays AND NOT plBypass */
   END. /* IF MobSub.PayType THEN DO: */
   ELSE DO:
      IF (TODAY - ldaLastTerminal) < liConfigDays AND NOT plBypass THEN
         RETURN appl_err("minimum_time_not_exceeded|6").
   END. /* ELSE DO: */
END. /* IF ldaLastTerminal NE ? THEN DO: */
/* SIM Only */
ELSE DO:
   IF MobSub.PayType THEN liConfigDays = 180.
   ELSE liConfigDays = 90.

   /* Validate prepaid/postpaid SIM Only config date */
   IF (TODAY - MobSub.ActivationDate) < liConfigDays AND NOT plBypass THEN DO:
      IF liConfigDays = 180 THEN
         RETURN appl_err("minimum_time_not_exceeded|6").
      ELSE
         RETURN appl_err("minimum_time_not_exceeded|3").
   END. /* IF (TODAY - MobSub.ActivationDate) < liConfigDays AND */
END. /* ELSE DO: */

IF Customer.CustIdType EQ "CIF" THEN DO:
   add_string(top_struct, "person_id", Customer.AuthCustId).
   add_string(top_struct, "id_type",   Customer.AuthCustIdType).
END.
ELSE DO:
   add_string(top_struct, "person_id", Customer.OrgId).
   add_string(top_struct, "id_type", Customer.CustIdType).
END.

add_string(top_struct, "subscription_type", Mobsub.CliType).
add_boolean(top_struct, "corporate_customer", (Customer.CustIdType EQ "CIF")).
IF liRemperiod > 0 THEN DO:
   add_int(top_struct, "contract_days_remain", liRemPeriod).
   add_double(top_struct, "contract_penalty", ldeCurrPen).
END.

/* q25 - Calculate allowed_terminal_financing_amount if risk limit is configured 
   and collect all non invoiced installments and ongoing orders and return 
   the difference of risk limit and pending all installment fee.*/
ASSIGN
   ldaDate = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
   liPeriod = YEAR(ldaDate) * 100 + MONTH(ldaDate).

FIND FIRST Limit NO-LOCK WHERE
           Limit.CustNum   = Customer.Custnum AND
           Limit.LimitType = {&LIMIT_TYPE_RISKLIMIT} AND
           Limit.ToDate   >= TODAY NO-ERROR.
IF AVAILABLE Limit THEN DO:

   MOBSUB_LOOP:
   FOR EACH bMobSub NO-LOCK WHERE
            bMobSub.Brand = Syst.Var:gcBrand AND
            bMobSub.CustNum = Customer.CustNum,
      EACH DCCLI NO-LOCK WHERE
           DCCLI.MsSeq = bMobSub.MsSeq AND
           DCCLI.ValidTo >= ldaDate AND
          (DCCLI.DCEvent BEGINS "PAYTERM" OR
           DCCLI.DCEvent BEGINS "RVTERM"):

      FOR FIRST FixedFee NO-LOCK WHERE
                FixedFee.Brand = Syst.Var:gcBrand AND
                FixedFee.Custnum = Customer.Custnum AND
                FixedFee.HostTable = "MobSub" AND
                FixedFee.KeyValue = STRING(bMobSub.MsSeq) AND
                FixedFee.EndPeriod >= liPeriod AND
                FixedFee.SourceTable = "DCCLI" AND
                FixedFee.SourceKey = STRING(DCCLI.PerContractID) AND
               (FixedFee.BillCode BEGINS "PAYTERM" OR
                FixedFee.BillCode BEGINS "RVTERM"):

         FOR EACH FFItem OF FixedFee NO-LOCK:
            IF FFItem.Billed AND
              (FFItem.BillPeriod <= liPeriod OR
               CAN-FIND(FIRST Invoice WHERE
                              Invoice.InvNum = FFItem.InvNum AND
                              Invoice.InvType = 1)) THEN NEXT.
            ldePendingFees = ldePendingFees + FFItem.Amt.
         END.

         IF FixedFee.BillCode BEGINS "PAYTERM" THEN
         FOR FIRST SingleFee NO-LOCK WHERE
                   SingleFee.Brand       = Syst.Var:gcBrand AND
                   SingleFee.Custnum     = FixedFee.CustNum AND
                   SingleFee.HostTable   = FixedFee.HostTable AND
                   SingleFee.KeyValue    = FixedFee.KeyValue AND
                   SingleFee.SourceTable = FixedFee.SourceTable AND
                   SingleFee.SourceKey   = FixedFee.SourceKey AND
                   SingleFee.CalcObj     = "RVTERM":
            IF SingleFee.Billed = TRUE AND
               CAN-FIND (FIRST Invoice NO-LOCK WHERE
                               Invoice.InvNum  = SingleFee.InvNum AND
                               Invoice.InvType = 1) THEN NEXT.
               ldePendingFees = ldePendingFees + SingleFee.Amt.
         END.
      END.
   END.

   FOR EACH Order NO-LOCK WHERE
            Order.Brand = Syst.Var:gcBrand AND
            Order.CustNum = Customer.CustNum and
      LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0,
      FIRST OfferItem NO-LOCK WHERE
            OfferItem.Brand       = Syst.Var:gcBrand AND
            OfferItem.Offer       = Order.Offer AND
            OfferItem.ItemType    = "PerContract" AND
            OfferItem.ItemKey     BEGINS "PAYTERM" AND
            OfferItem.EndStamp   >= Order.CrStamp  AND
            OfferItem.BeginStamp <= Order.CrStamp:

      ldePendingFees = ldePendingFees + OfferItem.Amount.

      FOR FIRST FMItem NO-LOCK WHERE
                FMItem.Brand     = Syst.Var:gcBrand AND
                FMItem.FeeModel  = OfferItem.ItemKey  AND
                FMItem.ToDate   >= TODAY AND
                FMItem.FromDate <= TODAY:
          ldePendingFees = ldePendingFees + (FMItem.Amount * FMItem.FFItemQty).
      END.

   END.

   add_double(top_struct, "allowed_terminal_financing_amount", 
             (Limit.LimitAmt - ldePendingFees)).
END.

installment_array = add_array(top_struct, "installment").

/* active payterm periodical contract(s) */
FOR EACH  DCCLI WHERE
          DCCLI.MsSeq = MobSub.MsSeq AND
          DCCLI.DCEvent BEGINS "PAYTERM" AND
          DCCLI.ValidTo >= TODAY NO-LOCK:

   IF fGetFixedFeeInfo(MobSub.MsSeq,
                       MobSub.Custnum,
                    DCCLI.DCEvent,
                    DCCLI.PercontractId,
                    ?,
                    OUTPUT ldePendingFee,
                    OUTPUT liTotalPeriods,
                    OUTPUT ldePeriodFee,
                    OUTPUT lderesidualFee,
                    OUTPUT lcFinancedInfo,
                    OUTPUT liOrderId) THEN DO:

      installment_struct = add_struct(installment_array,"").
      add_int(installment_struct,"per_contract_id", DCCLI.PercontractId).
      liQtyTFs = liQtyTFs + 1.
   END.
END.
add_int(top_struct,"TF_quantity",liQtyTFs).

/* Return Subs. based bundle */
add_string(top_struct, "subscription_bundle", MobSub.TariffBundle).

/* if subscription has one of 50% additional line discount active 
   ALFMO12_addline_mobile_only added the mobile only discounts as well*/
IF LOOKUP(MobSub.CliType, {&ADDLINE_CLITYPES}) > 0 THEN DO:
   FOR EACH DiscountPlan NO-LOCK WHERE
            DiscountPlan.Brand    = Syst.Var:gcBrand AND
     LOOKUP(DiscountPlan.DPRuleID, {&ADDLINE_DISCOUNTS} + "," + {&ADDLINE_DISCOUNTS_HM}) > 0 AND
            DiscountPlan.ValidTo >= TODAY,
      FIRST DPMember NO-LOCK WHERE
            DPMember.DPID       = DiscountPlan.DPID AND
            DPMember.HostTable  = "MobSub" AND
            DPMember.KeyValue   = STRING(MobSub.MsSeq) AND
            DPMember.ValidTo   >= TODAY AND
            DPMember.ValidFrom <= DPMember.ValidTo:
      add_boolean(top_struct, "additional_line_discount_50", TRUE).
   END.
END.

FINALLY:
   END.
