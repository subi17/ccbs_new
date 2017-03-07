/**
 * Information about subscription and customer (by msisdn)
 *
 * @input  transaction_id;string;mandatory;transaction id
           msisdn;string;mandatory;subscription msisdn number
 * @output   struct;mandatory;response struct
 * @response1 EBM, Spotify, Upsell Process, Roaming process Applications
           transaction_id;string;transaction id
           msisdn;string;mandatory;subscription msisdn number
           subscription_type;string;mandatory;Subscription type
 * @response2 MiYoigo Web/Mobile Applications
           transaction_id;string;transaction id
           msisdn;string;mandatory;subscription msisdn number
           subscription_type;string;mandatory;Subscription type
           multisim_id;int;optional;MultiSim ID (returned if MultiSIM subscription pair is active)
           permanent_contract_valid_to;date;mandatory;
           permanent_contract_length;int;mandatory;terminal contract original length in months
 * @response3 Other Applications (IVR, LBR)
           transaction_id;string;transaction id
           msisdn;string;mandatory;subscription msisdn number
           custnum;int;mandatory;customer number
           msseq;int;mandatory;subscription number
           subscription_name;string;mandatory;Subscription translation name
           subscription_type;string;mandatory;Subscription type
           subscription_actstamp;double;mandatory;Subscription Activation Stamp
           tariff_activation_date;date;mandatory;Current tariff activation date
           zipcode;string;mandatory;Zip Code
           region;string;mandatory;Region Code
           custid;string;mandatory;Customer Id
           language;int;mandatory;Customer Language
           paytype;boolean;mandatory;Payment type (true/false)
           pending_penalty_fee;double;pending terminal contract price
           permanent_contract_valid_to;date;mandatory;
           installment_contract;struct;optional;installment contract details
 * @installment_contract number_of_payments;int;total periods included to contract
                monthly_fee;double;payterm monthly fee
                pending_fee;double;how much is unpaid
 * @exceptions 1;Subscription not found
               2;Customer not found
               5;Application Id does not match
 */

{xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.
{Syst/commpaa.i}
katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId.
gcBrand = "1".
{Func/timestamp.i}
{Syst/tmsconst.i}
{Mm/fbundle.i}
{Func/fixedfee.i}
{Func/fcustpl.i}
{Func/penaltyfee.i}
{Func/fexternalapi.i}

/* Input parameters */
DEF VAR pcCLI               AS CHAR NO-UNDO.
DEF VAR pcTransId           AS CHAR NO-UNDO.
/* Output parameters */
DEF VAR top_struct          AS CHAR NO-UNDO.
DEF VAR payterm_struct      AS CHAR NO-UNDO.

DEF VAR lcSubscriptionType  AS CHAR NO-UNDO.
DEF VAR lcSubscriptionName  AS CHAR NO-UNDO.
DEF VAR lcPriceList         AS CHAR NO-UNDO.
DEF VAR liCount             AS INT  NO-UNDO.
DEF VAR ldePendingFee       AS DEC  NO-UNDO. 
DEF VAR liTotalPeriods      AS INT  NO-UNDO. 
DEF VAR ldePeriodFee        AS DEC  NO-UNDO. 
DEF VAR lderesidualFee      AS DEC  NO-UNDO. 
DEF VAR liRemPeriod         AS INT  NO-UNDO.
DEF VAR ldePrice            AS DEC  NO-UNDO.
DEF VAR ldaTariffActDate    AS DATE NO-UNDO. 
DEF VAR lcAppId             AS CHAR NO-UNDO.
DEF VAR lcFinancedInfo      AS CHAR NO-UNDO. 
DEF VAR ldtFrom             AS DATE NO-UNDO. 
DEF VAR ldtTo               AS DATE NO-UNDO. 
DEF VAR liMonths            AS INT NO-UNDO. 
DEF VAR liOrderId           AS INT  NO-UNDO.

DEF BUFFER lbMobsub FOR Mobsub.

IF validate_request(param_toplevel_id, "string,string") EQ ? THEN RETURN.

ASSIGN pcTransId = get_string(param_toplevel_id, "0")
       pcCLI     = get_string(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcAppId = substring(pcTransId,1,3).

IF NOT fchkTMSCodeValues(gbAuthLog.UserName,lcAppId) THEN
   RETURN appl_err("Application Id does not match").

FIND FIRST Mobsub NO-LOCK WHERE
           Mobsub.CLI = pcCLI NO-ERROR.
IF NOT AVAILABLE MobSub THEN
   RETURN appl_err("Subscription not found").
  
FIND FIRST customer NO-LOCK WHERE
           customer.custnum = MobSub.Custnum NO-ERROR.
IF NOT AVAILABLE customer THEN
   RETURN appl_err("Customer not found").

top_struct = add_struct(response_toplevel_id, "").

add_string(top_struct, "transaction_id", pcTransId).
add_string(top_struct, "msisdn", Mobsub.CLI).

/* Get correct bundle to return correct CLIType */
lcSubscriptionType = fGetCurrentTariff(BUFFER Mobsub,
                                       OUTPUT ldaTariffActDate).
IF MobSub.CLIType EQ "CONTRD" THEN
   lcSubscriptionType = fConvBundleToCLIType(lcSubscriptionType).

add_string(top_struct, "subscription_type", lcSubscriptionType).

/* Return if application ids EBM,Spotify,Upsell Process,Roaming process */
IF LOOKUP(lcAppId,"503,504,505,506") > 0 THEN RETURN.

/* Return if application id is MiYoigo/Web */
IF LOOKUP(lcAppId,"501,502") > 0 THEN DO:

   IF MobSub.MultiSIMType > 0 AND
      MobSub.MultiSIMID > 0 THEN DO:

      FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSIMID WHERE
                 lbMobSub.Brand = gcBrand AND
                 lbMobSub.MultiSimID = MobSub.MultiSimID AND
                 lbMobSub.MultiSimType NE MobSub.MultiSimType AND
                 lbMobSub.Custnum = MobSub.Custnum NO-ERROR.
      IF AVAIL lbMobSub THEN
         add_int(top_struct,"multisim_id",MobSub.MultiSimID) .
   END. /* IF MobSub.MultiSIMType > 0 AND */

   /* Return if prepaid subscription */
   IF Mobsub.PayType THEN RETURN.

END. /* IF LOOKUP(lcAppId,"501,502") > 0 THEN DO: */

/* Otherwise return everything */
ELSE DO:
   add_int(top_struct, "msseq", Mobsub.MsSeq).
   add_int(top_struct, "custnum", Customer.Custnum).
   add_string(top_struct, "zipcode", Customer.ZipCode).
   add_string(top_struct, "region", Customer.Region).
   add_string(top_struct, "custid", Customer.OrgId).
   add_int(top_struct, "language", Customer.Language).
   add_boolean(top_struct, "paytype", MobSub.PayType).

   lcSubscriptionName = fGetItemName(gcBrand,
                                     "CLIType",
                                     lcSubscriptionType,
                                     Customer.Language,
                                     Today).
   IF lcSubscriptionName = ? THEN lcSubscriptionName = "".

   add_string(top_struct, "subscription_name", lcSubscriptionName).
   add_double(top_struct,"subscription_actstamp", Mobsub.ActivationTS).
   add_datetime(top_struct,"tariff_activation_date",ldaTariffActDate).

   /* Return if prepaid subscription */
   IF Mobsub.PayType THEN RETURN.

   /* active payterm periodical contract */
   /* NOTE: it is supposed that there is only one active installment contract */
   FIND FIRST DCCLI WHERE
              DCCLI.MsSeq = MobSub.MsSeq AND
              DCCLI.DCEvent BEGINS "PAYTERM" AND
              DCCLI.ValidTo >= TODAY NO-LOCK NO-ERROR.
   IF AVAIL DCCLI AND
      fGetFixedFeeInfo(MobSub.MsSeq,
                       Mobsub.Custnum,
                       DCCLI.DCEvent,
                       DCCLI.PerContractId,
                       ?,
                       OUTPUT ldePendingFee,
                       OUTPUT liTotalPeriods,
                       OUTPUT ldePeriodFee,
                       OUTPUT lderesidualFee,
                       OUTPUT lcFinancedInfo,
                       OUTPUT liOrderId)
   THEN DO:
      payterm_struct = add_struct(top_struct,"installment_contract").
      add_int(payterm_struct,"number_of_payments",liTotalPeriods).
      add_double(payterm_struct,"monthly_fee", ldePeriodFee).
      add_double(payterm_struct,"pending_fee", ldePendingFee).
   END. /* IF AVAIL DCCLI AND */

END. /* ELSE DO: */

/* Count possible penalty fee for contract termination */
liCount = 0.
CONTRACT_LOOP:
FOR EACH DCCLI NO-LOCK WHERE
         DCCLI.MsSeq = MobSub.MsSeq AND
         DCCLI.ValidFrom <= TODAY AND
         DCCLI.ValidTo   >= TODAY AND
         DCCLI.CreateFees = TRUE,
   FIRST DayCampaign WHERE
         DayCampaign.Brand = gcBrand AND
         DayCampaign.DCEvent = DCCLI.DCEvent AND
         DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
         DayCampaign.TermFeeModel NE "" AND
         DayCampaign.TermFeeCalc > 0 NO-LOCK BY DCCLI.ValidFrom DESC:
   
   liCount = liCount + 1.
   IF liCount > 1 THEN LEAVE CONTRACT_LOOP.
  
   IF DCCLI.TermDate NE ? THEN
      add_datetime(top_struct,"permanent_contract_valid_to", DCCLI.Termdate).
   ELSE
      add_datetime(top_struct,"permanent_contract_valid_to", DCCLI.ValidTo).

   IF LOOKUP(lcAppId,"501,502") > 0 THEN DO:
      IF DCCLI.RenewalDate EQ ? THEN
         liMonths = DayCampaign.DurMonths.
      ELSE ASSIGN
         ldtFrom = DATETIME(DCCLI.ValidFrom,0)
         ldtTo = DATETIME(DCCLI.ValidTo,0)
         liMonths = INTERVAL(ldtTo,ldtFrom,"months") + 1.
      add_int(top_struct,"permanent_contract_length",liMonths).
   END.
   ELSE DO:

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

      /* calculate a factor for the fee (full / proportional) */
      ldePendingFee = fCalculateFactor(DCCLI.ValidFrom,
                                       DCCLI.RenewalDate,
                                       DCCLI.ValidTo,
                                       DCCLI.ValidToOrig,
                                       TODAY,
                                       DayCampaign.TermFeeCalc).

      ldePendingFee = TRUNCATE(ldePendingFee * ldePeriodFee,0).
      add_double(top_struct,"pending_penalty_fee",ldePendingFee).
   END. /* ELSE DO: */

END. /* FOR EACH DCCLI NO-LOCK WHERE */

FINALLY:
   /* Store the transaction id */
   gbAuthLog.TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.

