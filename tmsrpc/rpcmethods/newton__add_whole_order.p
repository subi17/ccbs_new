/**
 * Create an order
 * YI-006
 *
 * @input   order_data;struct;mandatory
            customer_data;struct;mandatory
            address_data;struct;optional
            device_data;struct;optional
            contact_data;struct;optional
            fusion_data;struct;optional
            q25_data;struct;optional
 * @order_data salesman;string;optional;id of the seller
               reseller;string;mandatory;reseller id
               channel;string;mandatory;controller name
               orderer_ip;string;mandatory;IP that submits the order
               billing_data;string;optional;bank account
               payment_method;string;optional;on_delivery or credit_card
               payment_reference;string;optional;
               authorization_number;string;optional;
               bin_number;string;optional;
               campaign_code;string;optional;
               campaign_type;string;optional;
               memo;string;optional;memo for order
               CLI;string;mandatory;phone number
               number_type;string;mandatory;new/mnp/renewal/stc
               old_operator;string;optional;mandatory for mnp
               old_icc;string;optional;mandatory for mnp
               old_billing_category;string;optional;mandatory for mnp
               mnp_porting_date;date;optional;only for mnp from CC channel
               ICC;string;optional;
               subscription_type;string;mandatory;
               contractid;string;mandatory;
               topup;float;optional;amount of added topup
               fat;float;optional;amount of added free air time
               fatgroup;float;optional;free air time group
               check;boolean;optional;Order check required
               send_sms;boolean;optional;Send SMS
               referee;str;optional;referee's MSISDN
               offer_id;string;optional;
               price_selection_time;timestamp;optional;time when user accepted order price in web
               order_inspection_result;string;mandatory;
               order_inspection_description;string;optional;
               order_inspection_level;string;optional;
               order_inspection_rule_id;string;optional;
               order_inspection_risk_code;string;optional;
               additional_bundle;string;optional;optional bundle 
               subscription_bundle;string;optional;mandatory base bundle for bundle based subscription types
               dss;boolean;optional;activate dss
               bono_voip;boolean;optional;activate bono voip
               delivery_channel;string;optional;paper/email/sms/no delivery
               bypass_rules;boolean;optional;skips subscription/actinvation limit check
               discount_plan_id;string;optional
               discount_plan_amount;double;optional
               discount_valid_periods;int;optional
               sim_type;string;optional;sim types (eg: regular/micro/nano/universal)
               multisim_id;int;optional;order group id
               multisim_type;int;optional;group member (1,2,..), mandatory if multisim_id is passed
               exclude_term_penalty;boolean;optional;yes/no
               extend_term_contract;boolean;optional;only applied if number_type=stc
               usage_type;string;optional;VOICE/DATA (should be used with CONT8)
               Mandate;string;optional
               delivery_type;int;optional;
               delivery_secure;int;optional;
               send_offer;boolean;optional;
               resignation_period;boolean;optional;
               tarj7_promo;boolean;optional;
               keep_installment;boolean;optional;
               multiorder;boolean;optional;
 * @customer_data fname;string;optional;
                  lname;string;optional;
                  lname2;string;optional;
                  site_name;string;optional;
                  title;string;optional;
                  region;string;optional;
                  street;string;mandatory;
                  street_number;string;mandatory;
                  additional_address;string;mandatory;
                  zip;string;mandatory;
                  city;string;mandatory;
                  country;string;optional;
                  street_code:string;optional;
                  city_code;string;optional;
                  municipality_code;string;optional;
                  nationality;string;optional;
                  birthday;datetime;optional;
                  email;string;optional;
                  sms_number;string;optional;
                  phone_number;string;optional;
                  customer_name;string;optional;
                  mark_post;boolean;optional;
                  mark_sms;boolean;optional;
                  mark_email;boolean;optional;
                  mark_post_3rd;boolean;optional;
                  mark_sms_3rd;boolean;optional;
                  mark_email_3rd;boolean;optional;
                  mark_bank_3rd;boolean;optional;
                  language;string;optional;
                  latitude;string;optional;
                  longitude;string;optional;
                  \[person|company\]id;string;optional;id of the owner
                  id_type;string;optional;NIF,NIE,CIF or Passport
                  profession;string;optional;
                  retrieved;boolean;optional;
                  identified_cust_sms_number;string;optional;
 * @address_data  fname;string;optional;
                  lname;string;optional;
                  lname2;string;optional;
                  site_name;string;optional;Company/Kiala point name
                  title;string;optional;
                  region;string;optional;
                  street;string;mandatory;
                  street_number;string;mandatory;
                  additional_address;string;mandatory;
                  zip;string;mandatory;
                  city;string;mandatory;
                  country;string;optional;
                  street_code;string;optional;
                  city_code;string;optional;
                  municipality_code;string;optional;
                  nationality;string;optional;
                  birthday;datetime;optional;
                  email;string;optional;
                  sms_number;string;optional;
                  phone_number;string;optional;
                  customer_name;string;optional;
                  mark_post;boolean;optional;
                  mark_sms;boolean;optional;
                  mark_email;boolean;optional;
                  mark_post_3rd;boolean;optional;
                  mark_sms_3rd;boolean;optional;
                  mark_email_3rd;boolean;optional;
                  mark_bank_3rd;boolean;optional;
                  language;string;optional;
                  latitude;string;optional;
                  longitude;string;optional;
                  kiala_code;string;optional;
                  ups_hours;string;optional;
 * @device_data IMEI;int;optional;optional;
                discount;int;optional;discount value
                manufacturer;string;optional;terminal manufacturer (IMEI register)
                model;string;optional;terminal model (IMEI register)
                color;string;optional;terminal color (IMEI register)
                serial_number;optional;laptop serial number
                hard_book;int;mandatory;0 - No hard booking, 1 - Pending hard booking
 * @contact_data fname;string;optional;
                 lname;string;optional;
                 lname2;string;optional;
                 site_name;string;optional;
                 title;string;optional;
                 street_code;string;optional;
                 city_code;string;optional;
                 municipality_code;string;optional;
                 region;string;optional;
                 street;string;mandatory;
                 street_number;string;mandatory;
                 additional_address;string;mandatory;
                 zip;string;mandatory;
                 city;string;mandatory;
                 country;string;optional;
                 nationality;string;optional;
                 birthday;datetime;optional;
                 email;string;optional;
                 sms_number;string;optional;
                 phone_number;string;optional;
                 customer_name;string;optional;
                 mark_post;boolean;optional;
                 mark_sms;boolean;optional;
                 mark_email;boolean;optional;
                 mark_post_3rd;boolean;optional;
                 mark_sms_3rd;boolean;optional;
                 mark_email_3rd;boolean;optional;
                 mark_bank_3rd;boolean;optional;
                 language;string;optional;
                 latitude;string;optional;
                 longitude;string;optional;
                 person_id;string;optional;contact person id
                 id_type;string;optional;NIF,NIE,CIF or Passport
 * @fusion_data  fixed_line_number_type;string;mandatory;NEW/MNP
                 fixed_line_number;string;optional;
                 fixed_line_mnp_old_operator;string;optional;
                 fixed_line_mnp_old_operator_name;string;optional;
                 fixed_line_mnp_old_operator_code;string;optional;
                 fixed_line_serial_number;string;optional;
                 fixed_line_mnp_time_of_change;string;optional;
                 fixed_line_product;string;mandatory;fusion order product code
                 customer_type;string;mandatory;customer type
                 phone_book;boolean;optional;
                 contractid;string;optional;
                 install_address;struct;mandatory;
                 billing_address;struct;optional;
  @install_address fname;string;optional
                    lname;string;optional;
                    lname2;string;optional;
                    phone_number;string;optional;
                    street;string;optional;
                    additional_address;string;optional
                    city;string;mandatory;
                    zip;string;mandatory;
                    street_number;string;optional;
                    region;string;optional;
                    profession;string;optional;
                    email_address;string;optional;
                    gescal;string;optional;
  @billing_address address;string;optional;
                    additional_address;string;optional
                    city;string;mandatory;
                    zip;string;mandatory;
                    street_number;string;optional;
                    region;string;optional;
 * @q25_data   q25_extension;boolean;optional;Extension of the Quota 25
               q25_discount;double;optional;Discount amount over the Quota 25
               per_contract_id;int;mandatory;installment contract id (related to q25)
               q25_contract_id;string;optional;Quota 25 Contract ID generated by WEB
 
 *
 * @output  orderid;int;handle for order
 */

{xmlrpc/xmlrpc_access.i}

{commpaa.i}
gcBrand = "1".
katun = "NewtonRPC".
{date.i}
{orderchk.i}
{order.i}
{tmsconst.i}
{fbundle.i}
{mnpoutchk.i}
{create_eventlog.i}
{fmakemsreq.i}
{main_add_lines.i}
{msisdn.i}
{forderstamp.i}
{email.i}
{ftransdir.i}

{cparam2.i}
/*{utumaa.i new }
{edefine.i new}*/
{order_data.i}
{smsmessage.i}

DEF VAR top_struct       AS CHAR NO-UNDO.
DEF VAR top_struct_fields AS CHAR NO-UNDO.
/* Input parameters for order */
DEF VAR pcOrderStruct   AS CHAR NO-UNDO.
DEF VAR lcOrderStruct   AS CHAR NO-UNDO.

DEF VAR pcDeviceStruct  AS CHAR NO-UNDO.
DEF VAR lcDeviceStruct  AS CHAR NO-UNDO.

/* Input parameters for customer */
DEF VAR pcCustomerStruct AS CHAR NO-UNDO.

DEF VAR pcAddressStruct AS CHAR NO-UNDO.
DEF VAR lcAddressStruct AS CHAR NO-UNDO.
DEF VAR pcContactStruct AS CHAR NO-UNDO.
DEF VAR lcContactStruct AS CHAR NO-UNDO.
DEF VAR pcFusionStruct  AS CHAR NO-UNDO. 
DEF VAR lcFusionStructFields AS CHAR NO-UNDO. 

DEF VAR gcOrderStructFields AS CHARACTER NO-UNDO.  
DEF VAR gcCustomerStructFields AS CHARACTER NO-UNDO.
DEF VAR gcCustomerStructStringFields AS CHARACTER NO-UNDO. 

DEF VAR pcSalesman      AS CHAR INITIAL "selforder" NO-UNDO.
DEF VAR pcReseller      AS CHAR NO-UNDO.
DEF VAR pcChannel       AS CHAR NO-UNDO.
DEF VAR pcIP            AS CHAR NO-UNDO.
/*DEF VAR pcOrderer       AS CHAR NO-UNDO.*/
DEF VAR pcAccount       AS CHAR NO-UNDO.
DEF VAR piPaymentMethod AS INT NO-UNDO.
DEF VAR pcCreditCardRefNum AS CHAR NO-UNDO.
DEF VAR pcCampaignCode  AS CHAR NO-UNDO.
DEF VAR pcCampaignType  AS CHAR NO-UNDO.
DEF VAR pcMemo          AS CHAR NO-UNDO.
DEF VAR pcAuthNumber    AS CHAR NO-UNDO.
DEF VAR pcBinNumber     AS CHAR NO-UNDO.

DEF VAR pcCLI           AS CHAR NO-UNDO.
DEF VAR pcSubType       AS CHAR NO-UNDO.
DEF VAR pcNumberType    AS CHAR NO-UNDO.
DEF VAR pcOldIcc        AS CHAR NO-UNDO.
DEF VAR pcOldOperator   AS CHAR NO-UNDO.
DEF VAR pcOldBillCat    AS CHAR NO-UNDO.
DEF VAR pcMNPPortingDate AS DATE NO-UNDO.
DEF VAR pcIcc           AS CHAR NO-UNDO INIT "".
DEF VAR pcContractId    AS CHAR NO-UNDO.
DEF VAR pfTopup         AS DEC NO-UNDO.
DEF VAR pfFAT           AS DECIMAL NO-UNDO.
DEF VAR pcFATGrp        AS CHAR NO-UNDO.
DEF VAR plCheck         AS LOG NO-UNDO INIT FALSE.
DEF VAR plSendSMS       AS LOG NO-UNDO INIT TRUE.
DEF VAR pcReferee       AS CHAR NO-UNDO.
DEF VAR pcOfferId  AS CHAR NO-UNDO.
DEF VAR pdePriceSelTime AS DEC NO-UNDO.
DEF VAR liTermOfferItemID AS INTEGER NO-UNDO.
DEF VAR pcDiscountPlanId  AS CHAR NO-UNDO. 
DEF VAR pdeDiscountPlanAmount AS DEC NO-UNDO. 
DEF VAR piDiscountValidPeriod AS INT NO-UNDO.
DEF VAR piMultiSimID AS INT NO-UNDO. 
DEF VAR piMultiSimType AS INT NO-UNDO. 
DEF VAR plExcTermPenalty AS LOG NO-UNDO.
DEF VAR plExtendTermContract AS LOG NO-UNDO.
DEF VAR pcMandateId AS CHAR NO-UNDO. 
DEF VAR piDeliveryType AS INT NO-UNDO. 
DEF VAR piDeliverySecure AS INT NO-UNDO. 
DEF VAR plKeepInstallment AS LOG NO-UNDO. 
DEF VAR pcUpsHours AS CHAR NO-UNDO. 
DEF VAR plCustDataRetr AS LOGICAL NO-UNDO.
DEF VAR pcIdentifiedSmsNumber AS CHAR NO-UNDO.
DEF VAR plMultiOrder AS LOGICAL NO-UNDO.
DEF VAR pcGescal AS CHAR NO-UNDO. 

/* Real Order Inspection parameters */
DEF VAR pcROIresult      AS CHAR NO-UNDO.
DEF VAR pcROIdescription AS CHAR NO-UNDO.
DEF VAR pcROIlevel       AS CHAR NO-UNDO.
DEf VAR pcROIruleId      AS CHAR NO-UNDO.
DEF VAR pcROIriskcode    AS CHAR NO-UNDO.
DEF VAR lcErrors AS CHARACTER NO-UNDO. 
DEF VAR lcPayType AS CHAR NO-UNDO. 
DEF VAR lcOldPayType AS CHAR NO-UNDO. 
DEF VAR lcOfferOrderChannel  AS CHAR NO-UNDO.

DEF VAR pcDataBundleType AS CHAR NO-UNDO. 
DEF VAR pcMobsubBundleType AS CHAR NO-UNDO. 
DEF VAR plDSSActivate    AS LOG NO-UNDO. 
DEF VAR plBonoVoipActivate AS LOG NO-UNDO.
DEF VAR plByPassRules AS LOG NO-UNDO.
DEF VAR lcdelivery_channel AS CHAR NO-UNDO.
DEF VAR pcUsageType AS CHAR NO-UNDO. 

DEF VAR lcPostpaidVoiceTariffs AS CHAR NO-UNDO.
DEF VAR lcPrepaidVoiceTariffs  AS CHAR NO-UNDO.
DEF VAR lcOnlyVoiceContracts   AS CHAR NO-UNDO.
DEF VAR lcBONOContracts        AS CHAR NO-UNDO.
DEF VAR lcIPLContracts         AS CHAR NO-UNDO.
DEF VAR lcFLATContracts        AS CHAR NO-UNDO.
DEF VAR lcCONTDContracts       AS CHAR NO-UNDO.
DEF VAR lcCONTSContracts       AS CHAR NO-UNDO.
DEF VAR lcCONTSFContracts      AS CHAR NO-UNDO.
DEF VAR lcBundleCLITypes       AS CHAR NO-UNDO.
DEF VAR lcRenoveSMSText        AS CHAR NO-UNDO. 
DEF VAR lcSTCSMSText           AS CHAR NO-UNDO. 
DEF VAR lcOfferSMSText         AS CHAR NO-UNDO. 
DEF VAR lcOrderSMSText         AS CHAR NO-UNDO.
DEF VAR ldeSMSStamp            AS DEC  NO-UNDO. 
DEF VAR lcMobileNumber         AS CHAR NO-UNDO. 
   
DEF VAR lcSMSKey AS CHAR NO-UNDO. 

/* Local variables for order */
DEF VAR liOrderId       AS INT  NO-UNDO.
DEF VAR lcSimType       AS CHAR NO-UNDO.

/* Local variables for customer */

/* Input parameters for device */
DEF VAR pcIMEI          AS CHAR NO-UNDO.
DEF VAR piDeviceDiscount AS INT NO-UNDO.
DEF VAR pcDeviceManufacturer AS CHAR NO-UNDO.
DEF VAR pcDeviceModel   AS CHAR NO-UNDO.
DEF VAR pcDeviceColor   AS CHAR NO-UNDO.
DEF VAR pcDeviceID   AS CHAR NO-UNDO.
DEF VAR pcLaptopSerial  AS CHAR NO-UNDO.
DEF VAR piHardBook      AS INT  NO-UNDO.

DEF VAR lccTemp AS CHARACTER NO-UNDO. 
DEF VAR lcError AS CHARACTER NO-UNDO. 
DEF VAR liRequest AS INTEGER NO-UNDO.
DEF VAR ldaOrderDate AS DATE NO-UNDO.
DEF VAR liOrderTime  AS INT  NO-UNDO.

DEF VAR lcIdType AS CHARACTER NO-UNDO.  
DEF VAR lcContactId AS CHARACTER NO-UNDO. 
DEF VAR lcContactIdType AS CHARACTER NO-UNDO. 
DEF VAR lcId AS CHARACTER NO-UNDO. 
DEF VAR llPendingMainLineOrder AS LOG NO-UNDO. 
   
DEF VAR pcFixedInstallAddress AS CHAR NO-UNDO. 
DEF VAR pcFixedBillingAddress AS CHAR NO-UNDO. 
DEF VAR lcFixedLineNumberType AS CHAR NO-UNDO. 
DEF VAR lcFixedLineNumber AS CHAR NO-UNDO. 
DEF VAR lcFixedLineMNPOldOper AS CHAR NO-UNDO. 
DEF VAR lcFixedLineMNPOldOperName AS CHAR NO-UNDO.
DEF VAR lcFixedLineMNPOldOperCode AS CHAR NO-UNDO.
DEF VAR lcFixedLineSerialNbr AS CHAR NO-UNDO.
DEF VAR lcFixedLineMNPTime AS CHAR NO-UNDO. 
DEF VAR lcFixedLineProduct AS CHAR NO-UNDO. 
DEF VAR lcFixedLineCustomerType AS CHAR NO-UNDO. 
DEF VAR llPhoneBook AS LOG NO-UNDO. 
DEF VAR lcFixedContractId AS CHAR NO-UNDO. 
DEF VAR plSendOffer AS LOG NO-UNDO. 
DEF VAR plResignationPeriod AS LOG NO-UNDO. 
DEF VAR plPromotion AS LOG NO-UNDO.
DEF VAR llROIClose AS LOG NO-UNDO. 

DEF VAR lcPayment AS CHAR NO-UNDO.
DEF VAR pcPaypalPayerid AS CHAR NO-UNDO.
DEF VAR liLanguage AS INTEGER NO-UNDO.

/* q25_data */
DEF VAR llq25_extension   AS LOGICAL NO-UNDO. /* Quota 25 extension */
DEF VAR ldeq25_discount   AS DECIMAL NO-UNDO. /* Discount amount over Quota 25 */
DEF VAR liper_contract_id AS INTEGER NO-UNDO. /* installment contract id - Quota 25 */
DEF VAR lcq25_contract_id AS CHAR    NO-UNDO. /* Quota 25 Contract ID generated by WEB */
DEF VAR pcQ25Struct       AS CHAR    NO-UNDO. /* Quota 25 input struct */
DEF VAR lcQ25Struct       AS CHAR    NO-UNDO. /* Quota 25 input struct */
/*parameter s and variables for accessories*/

DEF VAR pcAccessory AS CHAR NO-UNDO.
DEF VAR pcAccessoryStruct AS CHAR NO-UNDO.
DEF VAR lcAccessoryStruct AS CHAR NO-UNDO.

/* Prevent duplicate orders YTS-2166 */
DEF BUFFER lbOrder FOR Order.   

/* YBP-514 */
FUNCTION fGetOrderFields RETURNS LOGICAL :
   
   IF LOOKUP("salesman", lcOrderStruct) GT 0 THEN
       pcSalesman = get_string(pcOrderStruct, "salesman").
   pcReseller = get_string(pcOrderStruct, "reseller").
   pcChannel = get_string(pcOrderStruct, "channel").
   pcChannel = REPLACE(pcChannel, "order", "").
   pcIp = get_string(pcOrderStruct, "orderer_ip").
   IF LOOKUP("delivery_type", lcOrderStruct) GT 0 THEN
      piDeliveryType = get_int(pcOrderStruct, "delivery_type").
   IF LOOKUP("billing_data", lcOrderStruct) GT 0 THEN
       pcAccount = get_string(pcOrderStruct, "billing_data").
   IF LOOKUP("payment_method", lcOrderStruct) GT 0 THEN DO:
      lcPayment = get_string(pcOrderStruct, "payment_method").
      IF lcPayment EQ "on_delivery" THEN 
         piPaymentMethod = {&ORDERPAYMENT_M_POD}.
      ELSE IF lcPayment EQ "credit_card" THEN
         piPaymentMethod = {&ORDERPAYMENT_M_CREDIT_CARD}.
      ELSE IF lcPayment EQ "paypal" THEN
         piPaymentMethod = {&ORDERPAYMENT_M_PAYPAL}.
      ELSE 
         piPaymentMethod = 0.
   END.
   IF LOOKUP("payer_id", lcOrderStruct) GT 0 THEN
      pcPaypalPayerid = get_string(pcOrderStruct, "payer_id").
   IF LOOKUP("payment_reference", lcOrderStruct) GT 0 THEN
       pcCreditCardRefNum = get_string(pcOrderStruct, "payment_reference").
   IF LOOKUP("authorization_number", lcOrderStruct) GT 0 THEN
       pcAuthNumber = get_string(pcOrderStruct, "authorization_number").
   IF LOOKUP("bin_number", lcOrderStruct) GT 0 THEN
       pcBinNumber = get_string(pcOrderStruct, "bin_number").
   IF LOOKUP('campaign_code', lcOrderStruct) GT 0 THEN
       pcCampaignCode = get_string(pcOrderStruct, "campaign_code").
   IF LOOKUP('campaign_type', lcOrderStruct) GT 0 THEN
       pcCampaignType = get_string(pcOrderStruct, "campaign_type").
   IF LOOKUP('memo', lcOrderStruct) GT 0 THEN
       pcMemo = get_string(pcOrderStruct, "memo").

   pcCLI = get_string(pcOrderStruct, "CLI").
   pcSubType = get_string(pcOrderStruct, "subscription_type").
   pcNumberType = get_string(pcOrderStruct, "number_type").
   IF LOOKUP("old_operator", lcOrderStruct) GT 0 THEN 
   DO:
       pcOldIcc = get_string(pcOrderStruct, "old_icc").
       pcOldOperator = get_string(pcOrderStruct, "old_operator").
       pcOldBillCat = get_string(pcOrderStruct, "old_billing_category").

       IF LOOKUP("mnp_porting_date", lcOrderStruct) GT 0 THEN
          pcMNPPortingDate = get_date(pcOrderStruct, "mnp_porting_date").
   END.
   IF LOOKUP('ICC', lcOrderStruct) GT 0 THEN
       pcIcc = get_string(pcOrderStruct, "ICC").

   pcContractId = get_string(pcOrderStruct, "contractid").
   IF LOOKUP('send_sms', lcOrderStruct) GT 0 THEN
       plSendSMS = get_bool(pcOrderStruct, "send_sms").
   IF LOOKUP('topup', lcOrderStruct) GT 0 THEN
       pfTopup = get_double(pcOrderStruct, "topup").
   IF LOOKUP('fat', lcOrderStruct) GT 0 THEN
       pfFAT = get_double(pcOrderStruct, "fat").
   IF LOOKUP('fatgroup', lcOrderStruct) GT 0 THEN
       pcFATGrp = get_string(pcOrderStruct, "fatgroup").

   IF LOOKUP('check', lcOrderStruct) GT 0 THEN
       plCheck = get_bool(pcOrderStruct, "check").
   
   IF LOOKUP('referee', lcOrderStruct) GT 0 THEN
      pcReferee = get_string(pcOrderStruct, "referee").
  
   IF LOOKUP('offer_id', lcOrderStruct) GT 0 THEN 
      pcOfferId = get_string(pcOrderStruct, "offer_id").

   IF LOOKUP('price_selection_time', lcOrderStruct) GT 0 THEN
       pdePriceSelTime = get_timestamp(pcOrderStruct, "price_selection_time").
   ELSE pdePriceSelTime = {&nowTS}.
   

   pcROIresult =  get_string(pcOrderStruct, "order_inspection_result").
   IF LOOKUP('order_inspection_description', lcOrderStruct) GT 0 THEN 
      pcROIdescription = get_string(pcOrderStruct, "order_inspection_description").
   IF LOOKUP('order_inspection_level', lcOrderStruct) GT 0 THEN 
      pcROIlevel = get_string(pcOrderStruct, "order_inspection_level").
   IF LOOKUP('order_inspection_rule_id', lcOrderStruct) GT 0 THEN 
      pcROIruleId = get_string(pcOrderStruct, "order_inspection_rule_id").
   IF LOOKUP('order_inspection_risk_code', lcOrderStruct) GT 0 THEN 
      pcROIriskcode = get_string(pcOrderStruct, "order_inspection_risk_code").

   IF LOOKUP("additional_bundle",lcOrderStruct) > 0 THEN
      pcDataBundleType = get_string(pcOrderStruct,"additional_bundle").
   
   IF LOOKUP("subscription_bundle",lcOrderStruct) > 0 THEN
      pcMobsubBundleType = get_string(pcOrderStruct,"subscription_bundle").

   IF LOOKUP('dss', lcOrderStruct) GT 0 THEN
      plDSSActivate = get_bool(pcOrderStruct,"dss").

   IF LOOKUP('delivery_channel', lcOrderStruct) > 0 THEN
      lcdelivery_channel = get_string(pcOrderStruct,"delivery_channel").
    
   IF LOOKUP('bono_voip', lcOrderStruct) GT 0 THEN
      plBonoVoipActivate = get_bool(pcOrderStruct,"bono_voip").
   
   IF LOOKUP('bypass_rules', lcOrderStruct) GT 0 THEN
      plBypassRules = get_bool(pcOrderStruct,"bypass_rules").

   IF LOOKUP('discount_plan_id', lcOrderStruct) GT 0 THEN
      pcDiscountPlanId = get_string(pcOrderStruct,"discount_plan_id").

   IF LOOKUP('sim_type', lcOrderStruct) > 0 THEN
      lcSimType = get_string(pcOrderStruct,"sim_type").

   IF LOOKUP('discount_plan_amount', lcOrderStruct) GT 0 THEN
      pdeDiscountPlanAmount = get_double(pcOrderStruct,"discount_plan_amount").

   IF LOOKUP('discount_valid_periods', lcOrderStruct) GT 0 THEN
      piDiscountValidPeriod = get_int(pcOrderStruct,"discount_valid_periods").
   
   IF LOOKUP('multisim_id', lcOrderStruct) GT 0 THEN
      piMultiSimID = get_pos_int(pcOrderStruct,"multisim_id").
   
   IF LOOKUP('multisim_type', lcOrderStruct) GT 0 THEN
      piMultiSimType = get_pos_int(pcOrderStruct,"multisim_type").

   IF LOOKUP('exclude_term_penalty', lcOrderStruct) GT 0 THEN
      plExcTermPenalty = get_bool(pcOrderStruct,"exclude_term_penalty").
   
   IF LOOKUP('extend_term_contract', lcOrderStruct) GT 0 THEN
      plExtendTermContract = get_bool(pcOrderStruct,"extend_term_contract").
   
   IF LOOKUP('usage_type', lcOrderStruct) GT 0 THEN
      pcUsageType = get_string(pcOrderStruct,"usage_type").

   IF LOOKUP('mandate', lcOrderStruct) GT 0 THEN
      pcMandateId = get_string(pcOrderStruct,'mandate').
   
   IF LOOKUP('delivery_secure', lcOrderStruct) GT 0 THEN
      piDeliverySecure = get_int(pcOrderStruct,'delivery_secure').
   
   IF LOOKUP('send_offer', lcOrderStruct) GT 0 THEN
       plSendOffer = get_bool(pcOrderStruct, "send_offer").
   
   IF LOOKUP('resignation_period', lcOrderStruct) GT 0 THEN
       plResignationPeriod = get_bool(pcOrderStruct, "resignation_period").
       
   IF LOOKUP('tarj7_promo', lcOrderStruct) GT 0 THEN
       plPromotion = get_bool(pcOrderStruct, "tarj7_promo").    

   IF LOOKUP('keep_installment', lcOrderStruct) GT 0 THEN
      plKeepInstallment = get_bool(pcOrderStruct,"keep_installment").
   IF LOOKUP('multiorder', lcOrderStruct) GT 0 THEN
         plMultiOrder = get_bool(pcOrderStruct,"multiorder").
   llROIClose = (pcROIresult EQ "risk" AND LOOKUP(pcROIlevel,"7,8") > 0).

   RETURN TRUE.
END.


FUNCTION fCreateMemo RETURNS LOGICAL (INPUT pcTitle AS CHARACTER,
   INPUT pcText  AS CHAR, INPUT pcCreUser AS CHAR):

   CREATE Memo.
   ASSIGN
      Memo.CreStamp  = {&nowTS}
      Memo.Brand     = gcBrand 
      Memo.HostTable = "Order" 
      Memo.KeyValue  = STRING(Order.OrderId) 
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = pcCreUser 
      Memo.MemoTitle = pcTitle
      Memo.MemoText  = pcText.
END.


/* YBP-531 */
FUNCTION fCreateOrderCustomer RETURNS CHARACTER 
     (INPUT pcStructId AS CHARACTER,
      INPUT pcStructFields AS CHARACTER,
      INPUT piRowType AS INTEGER,
      INPUT plUpdate  AS LOGICAL):

   DEF VAR lcFError AS CHAR NO-UNDO. 
   DEF VAR iData AS INTEGER NO-UNDO. 
   DEF VAR lcc   AS CHARACTER NO-UNDO. 
   DEF VAR lii   AS INTEGER NO-UNDO. 
   DEF VAR lcField AS CHARACTER NO-UNDO. 
   DEF VAR liFieldIndex AS INTEGER NO-UNDO. 
   DEF VAR lcMarkOut    AS CHARACTER NO-UNDO. 
   DEF VAR lcMarketing  AS CHARACTER NO-UNDO. 
   DEF VAR ldBirthDay   AS DATE NO-UNDO. 
   DEF VAR llSelfEmployed AS LOGICAL NO-UNDO. 
   DEF VAR ldFoundationDate AS DATE NO-UNDO. 
   DEF VAR data            AS CHAR EXTENT 29 NO-UNDO.
   DEF VAR lcIdOrderCustomer AS CHARACTER NO-UNDO. 
   DEF VAR lcIdTypeOrderCustomer AS CHARACTER NO-UNDO. 
   DEF VAR liSubLimit AS INT NO-UNDO. 
   DEF VAR liSubs AS INT NO-UNDO. 
   DEF VAR liDelType  AS INT NO-UNDO.
   DEF VAR liActLimit AS INT NO-UNDO.
   DEF VAR liActs AS INT NO-UNDO.

   data[LOOKUP("country", gcCustomerStructStringFields)] = "ES".
   data[LOOKUP("nationality", gcCustomerStructStringFields)] = "ES".

   /* YBP-533 */
   lcc = validate_request(pcStructId, pcStructFields).
   DO lii = 1 TO NUM-ENTRIES(lcc):
      lcField = ENTRY(lii, lcc).
      liFieldIndex = LOOKUP(lcField, gcCustomerStructStringFields).
      IF lcField BEGINS "mark_" THEN 
      DO:
         IF NOT get_bool(pcStructId, lcField) THEN
            NEXT.
         lcField = SUBSTRING(lcField, 6).
         IF INDEX(lcField, "3rd") GT 0 THEN
            lcMarkOut = TRIM(lcMarkOut + "|" + REPLACE(lcField, "_3rd", ""), "|").
         ELSE
            lcMarketing = TRIM(lcMarketing + "|" + lcField, "|").
      END. /* IF lcField BEGINS "mark_" ... */
      ELSE IF lcField EQ "birthday" THEN 
      DO:
         ldBirthday = get_date(pcStructId, lcField).
      END. /* IF lcField EQ "ldBirthDay" ... */
      ELSE IF lcField EQ "self_employed" THEN 
      DO:
         llSelfEmployed = get_bool(pcStructId, lcField).
      END. /* IF lcField EQ "self_employed" ... */
      ELSE IF lcField EQ "foundation_date" THEN 
      DO:
         ldFoundationDate = get_date(pcStructId, lcField).
     /* END. ELSE IF lcField EQ "coname" THEN DO:
         data[2] = get_string(pcCustomerStruct, lcField). */
      END. /* IF lcField EQ "foundation_date" ... */
      ELSE IF lcField EQ "language" THEN 
      DO:
         liLanguage = LOOKUP(get_string(pcStructId, lcField),
                                 "es_ES,es_CA,es_EU,es_GA,en").
      END. /* IF lcField EQ "language" ... */
      ELSE IF lcField EQ "retrieved" THEN
      DO:
         plCustDataRetr = get_bool(pcStructId, lcField).
      END.
      ELSE IF lcField EQ "identified_cust_sms_number" THEN
         DO:
             pcIdentifiedSmsNumber = get_string(pcStructId, lcField).
      END.
      ELSE IF liFieldIndex EQ 0 THEN
         lcFError = SUBST("Unknown data field `&1`", lcField).
      ELSE
         data[liFieldIndex] = get_string(pcStructId, lcField).
   END. /* DO lii = */

   IF lcFError eq "" THEN
   DO:
      lcIdtypeOrderCustomer = data[LOOKUP("id_type", gcCustomerStructStringFields)].
      IF data[LOOKUP("person_id", gcCustomerStructStringFields)] ne "" THEN
          lcIdOrderCustomer = data[LOOKUP("person_id", gcCustomerStructStringFields)].
      IF data[LOOKUP("company_id", gcCustomerStructStringFields)] ne "" THEN 
      DO:
          lcContactId = lcIdOrderCustomer.
          lcIdOrderCustomer = data[LOOKUP("company_id", gcCustomerStructStringFields)].
          lcContactIdType = lcIdtypeOrderCustomer.
          lcIdtypeOrderCustomer = "CIF".
      END. /* data[LOOKUP("company_id", ... */
      
      IF piRowType EQ 4 THEN
         pcUpsHours = data[LOOKUP("ups_hours", gcCustomerStructStringFields)].
      
      IF lcIdOrderCustomer EQ "" AND piRowType = 1 THEN
          lcFError = "Expected either person_id or company_id".

      /* YTS-2453 */
      IF NOT plBypassRules AND
         lcFError = "" AND 
         piRowType = 1 AND
         LOOKUP(pcNumberType,"new,mnp") > 0 AND
         NOT plUpdate AND
         piMultiSimType NE {&MULTISIMTYPE_SECONDARY} AND
         NOT fSubscriptionLimitCheck(
         lcIdOrderCustomer,
         lcIdTypeOrderCustomer,
         llSelfEmployed,
         1,
         OUTPUT lcFError,
         OUTPUT liSubLimit,
         OUTPUT liSubs,
         OUTPUT liSubLimit,
         OUTPUT liActs) THEN .

      CASE lcdelivery_channel:
         WHEN "PAPER" THEN liDelType = {&INV_DEL_TYPE_PAPER}.
         WHEN "EMAIL" THEN liDelType = {&INV_DEL_TYPE_EMAIL}.
         WHEN "SMS"   THEN liDelType = {&INV_DEL_TYPE_SMS}.
         WHEN "No delivery" THEN liDelType = {&INV_DEL_TYPE_NO_DELIVERY}.
         WHEN "" THEN .
         OTHERWISE lcFError = "Invalid Invoice Delivery Type".
      END CASE. /* CASE lcdelivery_channel: */

   END. /* lcFError = "" */

   IF plUpdate AND lcFError eq "" THEN
   DO:
      /* YBP-535 */
      CREATE OrderCustomer.
      ASSIGN
         OrderCustomer.Brand           = gcBrand 
         OrderCustomer.OrderId         = liOrderId
         OrderCustomer.CustId          = lcIdOrderCustomer 
         OrderCustomer.CustIdType      = lcIdtypeOrderCustomer
         OrderCustomer.RowType         = piRowType
         OrderCustomer.BankCode        = pcAccount
         OrderCustomer.DataChecked     = ?
      .
      
      /* ordercustomer handling */
      ASSIGN
         OrderCustomer.FirstName          = 
            data[LOOKUP("fname", gcCustomerStructStringFields)]
         OrderCustomer.Surname1           = 
            data[LOOKUP("lname", gcCustomerStructStringFields)]
         OrderCustomer.Surname2           = 
            data[LOOKUP("lname2", gcCustomerStructStringFields)]
         OrderCustomer.Company            = 
            data[LOOKUP("site_name", gcCustomerStructStringFields)]
         OrderCustomer.Profession         = 
            data[LOOKUP("profession", gcCustomerStructStringFields)]
         OrderCustomer.Region             = 
            data[LOOKUP("region", gcCustomerStructStringFields)]
         OrderCustomer.ZipCode            = 
            data[LOOKUP("zip", gcCustomerStructStringFields)]
         OrderCustomer.PostOffice         = 
            data[LOOKUP("city", gcCustomerStructStringFields)]
         OrderCustomer.Country            = 
            data[LOOKUP("country", gcCustomerStructStringFields)]
         OrderCustomer.Nationality        = 
            data[LOOKUP("nationality", gcCustomerStructStringFields)]
         OrderCustomer.Email              = 
            data[LOOKUP("email", gcCustomerStructStringFields)]
         OrderCustomer.custtitle          = 
            data[LOOKUP("title", gcCustomerStructStringFields)]
         OrderCustomer.MobileNumber       = 
            data[LOOKUP("sms_number", gcCustomerStructStringFields)]
         OrderCustomer.FixedNumber        = 
            data[LOOKUP("phone_number", gcCustomerStructStringFields)]
         OrderCustomer.AddressCodC        = 
            data[LOOKUP("street_code", gcCustomerStructStringFields)]
         OrderCustomer.AddressCodP        = 
            data[LOOKUP("city_code", gcCustomerStructStringFields)]
         OrderCustomer.AddressCodM        = 
            data[LOOKUP("municipality_code", gcCustomerStructStringFields)] 
         OrderCustomer.Street             =
            data[LOOKUP("street", gcCustomerStructStringFields)]
         OrderCustomer.BuildingNum        = 
            data[LOOKUP("street_number", gcCustomerStructStringFields)]
         OrderCustomer.AddressCompl       =
            data[LOOKUP("additional_address", gcCustomerStructStringFields)]
         OrderCustomer.Latitude = 
            data[LOOKUP("latitude", gcCustomerStructStringFields)] 
         OrderCustomer.Longitude = 
            data[LOOKUP("longitude", gcCustomerStructStringFields)] 
         OrderCustomer.KialaCode = 
            data[LOOKUP("kiala_code", gcCustomerStructStringFields)] 
         OrderCustomer.Gescal = 
            data[LOOKUP("gescal", gcCustomerStructStringFields)] 
         OrderCustomer.SelfEmployed       = llSelfEmployed 
         OrderCustomer.FoundationDate     = ldFoundationDate
         OrderCustomer.Birthday           = ldBirthday
         OrderCustomer.Language           = STRING(liLanguage)
         OrderCustomer.OperSMSMarketing   = (LOOKUP("SMS", lcMarketing, "|") NE 0)
         OrderCustomer.OperEmailMarketing = (LOOKUP("Email", lcMarketing, "|") NE 0)
         OrderCustomer.OperPostMarketing  = (LOOKUP("Post", lcMarketing, "|") NE 0)
         OrderCustomer.OutSMSMarketing    = (LOOKUP("SMS", lcMarkOut, "|") NE 0)
         OrderCustomer.OutEMailMarketing  = (LOOKUP("Email", lcMarkOut, "|") NE 0)
         OrderCustomer.OutPostMarketing   = (LOOKUP("Post", lcMarkOut, "|") NE 0)
         OrderCustomer.OutBankMarketing   = (LOOKUP("Bank", lcMarkOut, "|") NE 0)
         OrderCustomer.OperAllMarketing   = lcMarketing NE ""
         OrderCustomer.ExtInvRef          = 
            data[LOOKUP("invoice_ref", gcCustomerStructStringFields)]
 
         OrderCustomer.Address = OrderCustomer.Street 
         OrderCustomer.CustDataRetr = plCustdataRetr
         OrderCustomer.MSISDNForIdent = pcIdentifiedSmsNumber.

         IF OrderCustomer.BuildingNum NE "" THEN 
            OrderCustomer.Address = OrderCustomer.Address + " " +
                                    OrderCustomer.BuildingNum.         
         IF OrderCustomer.AddressCompl NE "" THEN 
            OrderCustomer.Address = OrderCustomer.Address + " " + 
                                    OrderCustomer.AddressCompl. 

         IF liDelType > 0 THEN OrderCustomer.DelType = liDelType.
   END.



   IF piRowType = 1 THEN
   DO:
      lcId      = lcIdOrderCustomer.
      lcIdType  = lcIdTypeOrderCustomer.
   END.

   RETURN lcFError.
END.


FUNCTION fGetAndValidateDeviceFields RETURNS CHARACTER:
   
   DEF VAR lcError AS CHAR NO-UNDO INIT "". 

   IF LOOKUP("IMEI", lcDeviceStruct) GT 0 THEN 
      pcIMEI = get_string(pcDeviceStruct, "IMEI").
   
   IF LOOKUP('discount', lcDeviceStruct) GT 0 THEN
      piDeviceDiscount = get_int(pcDeviceStruct, "discount").
  
   /* IMEI register data */
   IF LOOKUP('manufacturer', lcDeviceStruct) GT 0 THEN
      pcDeviceManufacturer = get_string(pcDeviceStruct, "manufacturer").
   IF LOOKUP('model', lcDeviceStruct) GT 0 THEN
      pcDeviceModel = get_string(pcDeviceStruct, "model").
   IF LOOKUP('color', lcDeviceStruct) GT 0 THEN
      pcDeviceColor = get_string(pcDeviceStruct, "color").
   IF LOOKUP('device_id', lcDeviceStruct) GT 0 THEN
      pcDeviceID = get_string(pcDeviceStruct, "device_id").
   IF LOOKUP('serial_number', lcDeviceStruct) GT 0 THEN
      pcLaptopSerial = get_string(pcDeviceStruct, "serial_number").
   IF LOOKUP('hard_book', lcDeviceStruct) GT 0 THEN
      piHardBook = get_int(pcDeviceStruct, "hard_book").
   RETURN lcError.
END.


FUNCTION fCheckMSISDN RETURNS CHARACTER:
   
   DEF VAR lcError AS CHAR NO-UNDO INIT "".

   IF pcNumberType EQ "new" THEN DO:

      FIND FIRST MSISDN NO-LOCK
         WHERE msisdn.brand EQ gcBrand 
         AND MSISDN.ValidTo GE {&nowts}
         AND msisdn.cli EQ pcCLI
         AND msisdn.statuscode EQ 1 
         NO-ERROR.

      IF NOT AVAILABLE MSISDN THEN
         lcError = SUBST("Cli &1 not found or not free", pcCLI).
   END.
   /* YDR-491 */
   ELSE IF pcNumberType EQ "MNP" THEN DO:
      FIND FIRST MobSub WHERE
                 MobSub.Brand = gcBrand AND
                 MobSub.CLI = pcCLI NO-LOCK NO-ERROR. 
      IF AVAIL MobSub THEN
         lcError = "Subscription already exists with MSISDN " + pcCLI.
   END.
   RETURN lcError.
END.


/* YBP-528 */
FUNCTION fCheckSIM RETURNS CHARACTER:
   DEF VAR lcError AS CHAR NO-UNDO. 
   lcError = "".

   IF lcSimType = "Regular" THEN lcSimType = "Plug_IN".

   IF pcIcc NE '' THEN 
   DO:
       FIND FIRST SIM EXCLUSIVE-LOCK
          WHERE SIM.brand EQ gcBrand 
          AND SIM.ICC EQ pcIcc
          AND SIM.simstat EQ 1 
          NO-ERROR.
       IF NOT AVAILABLE SIM THEN
          lcError = SUBST("SIM with ICC &1 not found or not free", pcIcc).

       ELSE IF lcSimType > "" AND lcSimType <> SIM.SimArt THEN
          lcError = SUBST("SIM with ICC &1 not match with SIM Type &2", pcIcc, lcSimType).
   END.
   ELSE DO:
      IF lcSimType > "" AND LOOKUP(lcSimType,"Plug_IN,Micro,Nano,Universal") = 0 THEN
         RETURN "Invalid SIM Type specified".

      IF LOOKUP(pcNumberType,"new,mnp") > 0 AND lcSimType = "" THEN
         RETURN "sim_type is missing".
   END. /* ELSE DO: */

   RETURN lcError.
END.


/* YBP-584 */ 
FUNCTION fUpdateSIM RETURNS LOGICAL:
   IF pcIcc NE '' THEN 
   DO:
       IF pcNumberType = "renewal" THEN /* Reserved for ICC change */
          SIM.simstat = 13.
       ELSE
          SIM.simstat = 4.
   END.
   RETURN TRUE.
END.


/* YBP-531 */
FUNCTION fCreateOrder RETURNS LOGICAL:
   CREATE Order.
   ASSIGN
      Order.Brand           = gcBrand 
      Order.OrderId         = liOrderId
      Order.Salesman        = pcSalesman
      Order.Reseller        = pcReseller
      Order.Source          = "newton"
      Order.OrderChannel    = pcChannel
      Order.OrdererIP       = pcIP
      Order.CrStamp         = pdePriceSelTime 
      Order.InvCustRole     = 1
      Order.UserRole        = 1
      Order.StatusCode      = "1"
      Order.OrdererId       = lcContactId
      Order.OrdererIDType   = lcContactIdType 
      Order.CLI        = pcCLI
      Order.CLIType    = pcSubType
      Order.mnpstatus  = INT(pcNumberType EQ "mnp")
      Order.paytype    = (IF CLIType.PayType = 2 THEN TRUE ELSE FALSE)  
      Order.oldpaytype = (pcOldBillCat EQ "prepaid")
      Order.ICC        = pcIcc
      Order.OldIcc     = pcOldIcc
      Order.CurrOper   = pcOldOperator
      Order.PortingDate = pcMNPPortingDate
      Order.ContractId = pcContractId
      Order.FAT        = pfFAT WHEN pfFAT NE 0
      Order.FTGrp    = pcFATGrp
      Order.SMSType  = INT(plSendSMS)
      Order.Campaign = pcCampaignCode
      Order.CampaignType = pcCampaignType 
      Order.Referee  = pcReferee
      Order.Offer = pcOfferId 
      Order.ROIResult = pcROIresult
      Order.RiskCode = pcROIriskcode
      Order.MultiSimID = piMultiSimID
      Order.UsageType = pcUsageType
      Order.DeliveryType = piDeliveryType
      Order.DeliverySecure = piDeliverySecure
      Order.SendOffer = plSendOffer
      Order.ResignationPeriod  = plResignationPeriod 
      Order.RoiLevel = INT(pcRoilevel)
      Order.MultiSimType = piMultiSimType
      Order.MsSeq = (IF LOOKUP(pcNumberType,"new,mnp") > 0 
                     THEN NEXT-VALUE(MobSub)
                     ELSE MobSub.MsSeq).
      Order.Multiorder = plMultiOrder.               
      
END.

/* YBP-569 */
FUNCTION fHandleCorporateCustomer RETURNS LOGICAL:
   /*
   The Cases where CIF order should go to automated order management

   CASE 1: If the customer has any active subs. then order should go to 21
   CASE 2: If the customer has not any active subs. then order should go to 20
   */

   FIND FIRST Customer WHERE
              Customer.Brand      = gcBrand  AND
              Customer.OrgId      = lcId     AND
              Customer.CustIdType = lcIdType AND
              Customer.Roles NE "inactive" NO-LOCK NO-ERROR. 
   IF AVAIL Customer THEN DO:
      FIND FIRST MobSub WHERE
                 MobSub.Brand   = gcBrand AND
                 MobSub.AgrCust = Customer.CustNum
           NO-LOCK NO-ERROR.
      IF NOT AVAIL MobSub THEN Order.StatusCode = "20".
      ELSE Order.StatusCode = "21".
   END. /* IF AVAIL Customer THEN DO: */
   ELSE Order.StatusCode = "20".

END. /* FUNCTION fHandleCorporateCustomer RETURNS LOGICAL: */

/* YBP-570 */ 
FUNCTION fCreateOrderTopup RETURNS LOGICAL:
   DEFINE VARIABLE lCreate AS LOGICAL NO-UNDO. 
   lCreate = FALSE.
   IF pfTopup NE 0 THEN 
   DO:
      CREATE OrderTopup.
      ASSIGN
         OrderTopup.Amount = pfTopup
         OrderTopup.Brand = gcBrand 
         OrderTopup.OrderId = Order.OrderId
         /*OrderTopup.VatAmount = */
      .
      lCreate = TRUE.
   END.
   RETURN lCreate.
END.

FUNCTION fCreateAccessory RETURNS LOGICAL:

   IF pcAccessory NE "" THEN DO:
      CREATE OrderAccessory.
      ASSIGN
         OrderAccessory.OrderId     = Order.OrderId
         OrderAccessory.TerminalType = {&TERMINAL_TYPE_ACCESSORY}
         OrderAccessory.brand       = gcBrand
         OrderAccessory.ProductCode = pcAccessory. /*deviceid - billingitem*/
   END.

   RETURN TRUE.
END.


/* YBP-571 */ 
FUNCTION fCreateOrderAccessory RETURNS LOGICAL:

   DEFINE VARIABLE liLaptopItemID AS INTEGER NO-UNDO. 
   
   IF liTermOfferItemID > 0 OR pcIMEI NE "" THEN DO:
      
      IF liTermOfferItemID > 0 THEN
         FIND OfferItem WHERE
              OfferItem.OfferItemId = liTermOfferItemID NO-LOCK.
      
      CREATE OrderAccessory.
      ASSIGN
         OrderAccessory.OrderId       = Order.OrderId
         OrderAccessory.TerminalType  = {&TERMINAL_TYPE_PHONE} 
         OrderAccessory.brand         = gcBrand 
         OrderAccessory.IMEI          = pcIMEI
         OrderAccessory.discount      = piDeviceDiscount WHEN piDeviceDiscount NE 0
         OrderAccessory.Model         = pcDeviceModel
         OrderAccessory.Manufacturer  = pcDeviceManufacturer
         OrderAccessory.ModelColor    = pcDeviceColor
         OrderAccessory.ProductCode   = OfferItem.ItemKey WHEN AVAIL OfferItem
         OrderAccessory.Amount        = OfferItem.Amount WHEN AVAIL OfferItem
         OrderAccessory.HardBook      = piHardBook.

      RELEASE OrderAccessory.
      RELEASE OfferItem.
   END.
   
   IF pcLaptopSerial NE "" THEN DO:

      liLaptopItemID = fGetTerminalOfferItemId(Offer.Offer, {&BITEM_GRP_LAPTOP}, pdePriceSelTime).
   
      IF liLaptopItemID > 0 THEN
         FIND OfferItem WHERE
              OfferItem.OfferItemId = liLaptopItemID NO-LOCK.

      CREATE OrderAccessory.
      ASSIGN
         OrderAccessory.OrderId     = Order.OrderId
         OrderAccessory.TerminalType = {&TERMINAL_TYPE_LAPTOP} 
         OrderAccessory.brand       = gcBrand 
         OrderAccessory.IMEI        = pcLaptopSerial
         OrderAccessory.ProductCode = OfferItem.ItemKey WHEN AVAIL OfferItem.
   END.

   RETURN TRUE.
END.


/* YBP-572 */ 
FUNCTION fCreateOrderPayment RETURNS LOGICAL:
   CREATE OrderPayment.
   ASSIGN
      OrderPayment.Brand         = gcBrand
      OrderPayment.OrderId       = liOrderId
      OrderPayment.Method        = piPaymentMethod
      OrderPayment.CCReference   = pcCreditCardRefNum
      OrderPayment.AuthNumber    = pcAuthNumber
      OrderPayment.BinNumber     = pcBinNumber.
      
      IF piPaymentMethod EQ {&ORDERPAYMENT_M_PAYPAL} THEN
         OrderPayment.CCNumber = pcPaypalPayerid.

   RETURN TRUE.
END.

FUNCTION fCreateOrderFusion RETURNS LOGICAL:

   CREATE OrderFusion.
   ASSIGN
      OrderFusion.Brand             = gcBrand
      OrderFusion.OrderId           = liOrderId
      OrderFusion.FusionStatus      = {&FUSION_ORDER_STATUS_NEW}
         WHEN LOOKUP(Order.StatusCode,{&ORDER_ROI_STATUSES}) = 0 AND
         NOT llROIClose
      OrderFusion.OrderDate         = ldaOrderDate
      OrderFusion.Salesman          = pcSalesman
      OrderFusion.FixedNumberType   = lcFixedLineNumberType
      OrderFusion.FixedNumber       = lcFixedLineNumber
      OrderFusion.Product           = lcFixedLineProduct
      OrderFusion.CustomerType      = lcFixedLineCustomerType
      OrderFusion.FixedContractID   = lcFixedContractId
      OrderFusion.FixedMNPTime      = lcFixedLineMNPTime
      OrderFusion.PhoneBook         = llPhoneBook
      OrderFusion.FixedCurrOper     = lcFixedLineMNPOldOper WHEN lcFixedLineMNPOldOper > ""
      OrderFusion.FixedCurrOper     = lcFixedLineMNPOldOperName WHEN lcFixedLineMNPOldOper = ""
      OrderFusion.UpdateTS          = fMakeTS()
      OrderFusion.FixedCurrOperCode = lcFixedLineMNPOldOperCode
      OrderFusion.SerialNumber      = lcFixedLineSerialNbr.

   RETURN TRUE.

END.

/* Input variables for address data */

gcOrderStructFields = "billing_data," +
                      "campaign_code," +
                      "channel!," +
                      "check," +
                      "CLI!," +
                      "contractid!," +
                      "fat," +
                      "fatgroup," +
                      "ICC," +
                      "memo," +
                      "mnp_process_bypass," +
                      "network_bypass," +
                      "no_charge," +
                      "number_type!," +
                      "old_billing_category," +
                      "old_icc," +
                      "old_operator," +
                      "mnp_porting_date," +
                      "orderer_ip!," +
                      "payment_method," +
                      "payer_id," +
                      "payment_reference," +
                      "authorization_number," +
                      "bin_number," +
                      "salesman," +
                      "reseller!," +
                      "subscription_type!," +
                      "topup," +
                      "user_fname," +
                      "user_lname," +
                      "user_lname2," +
                      "send_sms," +
                      "referee," +
                      "offer_id," +
                      "price_selection_time," +
                      "order_inspection_result!," +
                      "order_inspection_description," +
                      "order_inspection_level," +
                      "order_inspection_rule_id," +
                      "order_inspection_risk_code," + 
                      "additional_bundle," +
                      "subscription_bundle," +
                      "dss," +
                      "bono_voip," +
                      "bypass_rules," +
                      "delivery_channel," +
                      "discount_plan_id," +
                      "discount_plan_amount," +
                      "discount_valid_periods," +
                      "sim_type," + 
                      "multisim_id," +
                      "multisim_type," +
                      "exclude_term_penalty," +
                      "extend_term_contract," +
                      "usage_type," +
                      "mandate," + 
                      "delivery_type," + 
                      "delivery_secure," + 
                      "campaign_type," +
                      "send_offer," +
                      "resignation_period," +
                      "tarj7_promo," +
                      "keep_installment," +
                      "multiorder".

gcCustomerStructFields = "birthday," +
                         "city!," +
                         "city_code," +
                         "street_code," +
                         "municipality_code," +
                         "company_id," +
                         "country," +
                         "customer_name," +
                         "email," +
                         "fname," +
                         "foundation_date," +
                         "id_type," +
                         "invoice_ref," +
                         "language," +
                         "lname," +
                         "lname2," +
                         "mark_email," +
                         "mark_email_3rd," +
                         "mark_post," +
                         "mark_post_3rd," +
                         "mark_sms," +
                         "mark_sms_3rd," +
                         "mark_bank_3rd," +
                         "nationality," +
                         "person_id," +
                         "phone_number," +
                         "region," +
                         "self_employed," +
                         "site_name," +
                         "sms_number," +
                         "street!," +
                         "street_number,"+
                         "additional_address,"+
                         "title," +
                         "zip!," +
                         "latitude," + 
                         "longitude," +
                         "profession," +
                         "kiala_code," + 
                         "ups_hours," +
                         "retrieved," +
                         "identified_cust_sms_number," +
                         "gescal".

/* note: check that data variable has correct EXTENT value */
gcCustomerStructStringFields = "city," +
                               "city_code," +
                               "street_code," +
                               "municipality_code," +
                               "company_id," +
                               "country," +
                               "customer_name," +
                               "email," +
                               "fname," +
                               "id_type," +
                               "invoice_ref," +
                               "lname," +
                               "lname2," +
                               "nationality," +
                               "person_id," +
                               "phone_number," +
                               "region," +
                               "site_name," +
                               "sms_number," +
                               "street," +
                               "street_number,"+
                               "additional_address,"+
                               "title," +
                               "zip," +
                               "latitude," + 
                               "longitude," +
                               "profession," + 
                               "kiala_code," +
                               "ups_hours," +
                               "gescal".

/* common validation */
/* YBP-513 */
IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
top_struct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

top_struct_fields = validate_request(top_struct, 
   "order_data!,customer_data!,address_data,device_data,contact_data,fusion_data,q25_data,order_inspection_data,accessory_data").
IF top_struct_fields EQ ? THEN RETURN.

ASSIGN
pcOrderStruct     = get_struct(top_struct, "order_data")
pcCustomerStruct  = get_struct(top_struct, "customer_data")
pcAddressStruct   = get_struct(top_struct, "address_data") WHEN
                       LOOKUP("address_data",top_struct_fields) > 0
pcDeviceStruct    = get_struct(top_struct, "device_data") WHEN
                       LOOKUP("device_data",top_struct_fields) > 0 
pcContactStruct   = get_struct(top_struct, "contact_data") WHEN
                       LOOKUP("contact_data",top_struct_fields) > 0
pcFusionStruct    = get_struct(top_struct, "fusion_data") WHEN
                       LOOKUP("fusion_data",top_struct_fields) > 0
pcAccessoryStruct = get_struct(top_struct, "accessory_data") WHEN
                       LOOKUP("accessory_data",top_struct_fields) > 0
pcQ25Struct      = get_struct(top_struct, "q25_data") WHEN
                      LOOKUP("q25_data",top_struct_fields) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

/* order validation */
/* YPB-514 */
lcOrderStruct = validate_request(pcOrderStruct, gcOrderStructFields).
IF lcOrderStruct EQ ? THEN RETURN.
fGetOrderFields().
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF LOOKUP(pcNumberType,"new,mnp,renewal,stc") = 0 THEN
   RETURN appl_err(SUBST("Unknown number_type &1", pcNumberType)).   

/* YPB-515 */
IF pcDiscountPlanId > "" THEN DO:
   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = gcBrand AND
              DiscountPlan.DPRuleID = pcDiscountPlanId AND
              DiscountPlan.ValidFrom <= TODAY AND
              DiscountPlan.ValidTo   >= TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL DiscountPlan THEN
      RETURN appl_Err(SUBST("Incorrect discount plan id: &1", pcDiscountPlanId)).
END.

/* YBP-516 */
ASSIGN lcPostpaidVoiceTariffs = fCParamC("POSTPAID_VOICE_TARIFFS")
       lcPrepaidVoiceTariffs  = fCParamC("PREPAID_VOICE_TARIFFS")
       lcBundleCLITypes       = fCParamC("BUNDLE_BASED_CLITYPES").

/* YBP-517 */
IF LOOKUP(pcSubType,lcBundleCLITypes) > 0 THEN
   ASSIGN lcIPLContracts    = fCParamC("IPL_CONTRACTS")
          lcCONTDContracts  = fCParamC("CONTD_CONTRACTS")
          lcCONTSContracts  = fCParamC("CONTS_CONTRACTS")
          lcFLATContracts   = fCParamC("FLAT_CONTRACTS")
          lcCONTSFContracts = fCParamC("CONTSF_CONTRACTS").

IF pcMobsubBundleType > "" THEN DO:
   IF LOOKUP(pcMobsubBundleType,lcIPLContracts)    = 0 AND
      LOOKUP(pcMobsubBundleType,lcCONTDContracts)  = 0 AND
      LOOKUP(pcMobsubBundleType,lcFLATContracts)   = 0 AND
      LOOKUP(pcMobsubBundleType,lcCONTSContracts)  = 0 AND
      LOOKUP(pcMobsubBundleType,lcCONTSFContracts) = 0 THEN RETURN
      appl_err(SUBST("Incorrect data bundle type: &1", pcMobsubBundleType)).   

   IF NOT fIsBundleAllowed
      (pcSubType,
       pcMobsubBundleType,
       OUTPUT lcError) THEN RETURN appl_err(lcError).
END.
ELSE IF LOOKUP(pcSubType,lcBundleCLITypes) > 0 AND
   pcNumberType <> "renewal" THEN
   RETURN appl_err("Subscription based data bundle is missing").

IF pcDataBundleType > "" THEN DO:
   lcBONOContracts = fCParamC("BONO_CONTRACTS").
   IF LOOKUP(pcDataBundleType,lcBONOContracts) = 0 THEN RETURN
      appl_err(SUBST("Incorrect data bundle type: &1", pcDataBundleType)).   

   IF NOT fIsBundleAllowed
      (pcSubType,
       pcDataBundleType,
       OUTPUT lcError) THEN RETURN appl_err(lcError).
END.

FIND CLIType NO-LOCK WHERE
     CLIType.Brand = gcBrand AND
     CLIType.CliType = pcSubType NO-ERROR.
IF NOT AVAIL CLIType THEN DO:
   RETURN appl_err(SUBST("Unknown CLIType &1", pcSubType)).   
END.

/* YBP-518 */
/* device validation */
IF pcDeviceStruct > "" THEN DO:
   lcDeviceStruct = validate_request(pcDeviceStruct, 
      "IMEI,discount,model,manufacturer,color,serial_number,device_id,hard_book").
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   lcError = fGetAndValidateDeviceFields().
   IF lcError <> "" THEN appl_err(lcError).
   IF gi_xmlrpc_error NE 0 THEN RETURN.

END.

/*YPR-2478*/
IF pcAccessoryStruct > "" THEN DO:
   lcAccessoryStruct = validate_request(pcAccessoryStruct,
      "device_model_id").
   IF gi_xmlrpc_error NE 0 THEN RETURN.
      IF LOOKUP('device_model_id', lcAccessoryStruct) GT 0 THEN
      pcAccessory = get_string(pcAccessoryStruct, "device_model_id").
END.

IF pcOfferId NE "" THEN DO:

   /* YBP-519 */
   FIND Offer WHERE 
        Offer.Brand = gcBrand AND 
        Offer.Offer = pcOfferId NO-LOCK NO-ERROR.
   IF NOT AVAIL Offer THEN
      RETURN appl_err("Offer " + pcOfferId + " is not defined").

   /* YBP-520 */
   liTermOfferItemID = fGetTerminalOfferItemId(Offer.Offer,
                                               {&BITEM_GRP_TERMINAL},
                                               pdePriceSelTime).
   
   IF pcChannel NE "renewal_pos_stc" AND
      pcChannel NE "retention_stc"   THEN DO:
   
      /* YBP-521 */
      lcErrors = "".
      lcPayType = (IF CLIType.PayType = 2 THEN "2" ELSE "1").
      lcOldPayType = (IF pcOldBillCat EQ "prepaid" THEN "2" ELSE "1").
      
      IF (liTermOfferItemID > 0) NE (pcDeviceID NE "") THEN DO:
         lcErrors = lcErrors + "Terminal is missing or not in offer;".
      END.
      
      IF liTermOfferItemID > 0 THEN DO:
         
         FIND OfferItem WHERE
              OfferItem.OfferItemId = liTermOfferItemID NO-LOCK.
         
         IF OfferItem.ItemKey NE pcDeviceID THEN
            lcErrors = lcErrors + "Terminal code mismatch; Offer terminal: " + OfferItem.ItemKey + " Order terminal: " + pcDeviceID.
      END.
      
      /* YBP-521 */
      FOR EACH offercriteria where
               offercriteria.brand = gcBrand and
               offercriteria.offer = offer.offer and
               offercriteria.beginstamp < pdePriceSelTime and
               offercriteria.endstamp > pdePriceSelTime NO-LOCK:

         case offercriteria.criteriatype:
            when "clitype" then do:
               if offercriteria.includedvalue eq "ALL_VOICE" THEN DO:
                  if lookup(pcSubType,lcPostpaidVoiceTariffs + "," +
                                      lcPrepaidVoiceTariffs) = 0 then
                  lcErrors = lcErrors + "CLIType " + pcSubType + " not in " + offercriteria.includedvalue + ";".
               end.
               else if lookup(pcSubType, offercriteria.includedvalue) = 0 then do:
                  lcErrors = lcErrors + "CLIType " + pcSubType + " not in " + offercriteria.includedvalue + ";".
               end.
            end.
            when "paytype" then do:
               if lookup(lcpaytype,offercriteria.includedvalue) = 0 then do:
                  lcErrors = lcErrors + "PayType " + lcPayType + " not in " + offercriteria.includedvalue + ";".
               end.
            end.
            when "oldpaytype" then do:
               if lookup(lcOldPayType,offercriteria.includedvalue) = 0 then do:
                  lcErrors = lcErrors + "OldPayType " + lcOldPayType + " not in " + offercriteria.includedvalue + ";".
               end.
            end.
            when "orderchannel" then do:
               lcOfferOrderChannel = 
                  (IF LOOKUP(pcChannel,"telesales,emission") > 0 THEN "cc"
                   ELSE IF LOOKUP(pcChannel,"fusion_telesales,fusion_emission,fusion_cc") > 0
                   THEN "fusion_telesales" ELSE pcChannel).
               if lookup(lcOfferOrderChannel, offercriteria.includedvalue) = 0 then do:
                  lcErrors = lcErrors + "OrderChannel " + lcOfferOrderChannel + " not in " + offercriteria.includedvalue + ";".
               end.
            end.
            when "numbertype" then do:
               /* renewal offers have currently numbertype value New */
               if pcNumberType NE "renewal" AND
                  lookup(pcNumberType, offercriteria.includedvalue) = 0 then do:
                  lcErrors = lcErrors  + "NumberType " + pcNumberType + " not in " + offercriteria.includedvalue + ";".
               end.
            end.
            when "reseller" then do:
               if offercriteria.includedvalue > "" AND
                  LOOKUP(pcReseller,offercriteria.includedvalue) = 0 THEN DO:
                  lcErrors = lcErrors  + "Reseller " + 
                    pcReseller + " not in " + offercriteria.includedvalue + ";".
               end.
            end.
         end.
      end.

      IF lcErrors NE "" THEN RETURN appl_err("Offer criteria don't match: " + lcErrors). 

   end.
   liTermOfferItemID = fGetTerminalOfferItemId(Offer.Offer, {&BITEM_GRP_TERMINAL}, pdePriceSelTime).
END.

/* YBP-523 */
/* validate ROI risk values */
IF pcROIresult = "risk" AND 
   LOOKUP(pcROIlevel,"1,2,3,4,7,8") = 0 THEN
   RETURN appl_err(SUBST("Unsupported Real Time Inspection Risk Level: &1", pcROIlevel)).
 
/* YBP-524 */
/* validate MSISDN */
lcError = fCheckMSISDN().
IF lcError <> "" THEN appl_err(lcError).
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* YBP-525 */
/* Customer, Address and Contact validation */
lccTemp = validate_request(pcCustomerStruct, gcCustomerStructFields).
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF pcAddressStruct > "" THEN
DO:
   lccTemp = validate_request(pcAddressStruct, gcCustomerStructFields).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.
IF pcContactStruct > "" THEN
DO:
   lccTemp = validate_request(pcContactStruct, gcCustomerStructFields).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.
 
/* YBP-536 */ 
lcError = fCreateOrderCustomer(pcCustomerStruct, gcCustomerStructFields, 1, FALSE).
IF lcError <> "" THEN appl_err(lcError).
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* YBP-537 */ 
IF pcAddressStruct > "" THEN
   lcError = fCreateOrderCustomer(pcAddressStruct, gcCustomerStructFields, 4, FALSE).
IF lcError <> "" THEN appl_err(lcError).
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* YBP-538 */ 
IF pcContactStruct > "" THEN
   lcError = fCreateOrderCustomer(pcContactStruct, gcCustomerStructFields, 5, FALSE).
IF lcError <> "" THEN appl_err(lcError).
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* YBP-526 */
IF LOOKUP(pcNumberType,"renewal,stc") > 0 THEN DO:
   
   FIND MobSub WHERE
        MobSub.Brand = gcBrand AND
        MobSub.CLI = pcCli NO-LOCK NO-ERROR. 
   IF NOT AVAIL MobSub THEN 
      RETURN appl_err(SUBST("Mobsub with msisdn &1 not found", pcCli)).
   
   FIND Customer WHERE
        Customer.Brand = gcBrand AND
        Customer.OrgId = lcID AND
        Customer.CustidType = lcIdType AND
        Customer.Custnum = MobSub.Custnum NO-LOCK NO-ERROR.

   IF NOT AVAIL Customer THEN 
      RETURN appl_err(SUBST("Customer not found: &1", lcId)).
END.

/* YBP-527 */
IF pcNumberType EQ "renewal" AND
   liTermOfferItemID <= 0 THEN
   RETURN appl_err("Terminal must exist with renewal order"). 

/* YBP-528 */
lcError = fCheckSIM().

IF NOT lcError > "" THEN DO:
   FIND FIRST lbOrder NO-LOCK WHERE
              lbOrder.brand EQ gcBrand AND
              lbOrder.ContractID EQ pcContractId AND
              lbOrder.CLI NE pcCLI NO-ERROR.
   IF AVAIL lbOrder THEN
      lcError = SUBST("Duplicate contract ID &1", pcContractId).
END.

IF lcError <> "" THEN RETURN appl_err(lcError).
IF gi_xmlrpc_error NE 0 THEN RETURN. 

/* YBP-529 */
FIND FIRST lbOrder NO-LOCK WHERE
           lbOrder.brand EQ gcBrand AND
           lbOrder.ContractID EQ pcContractId AND
           lbOrder.CLI = pcCLI AND
           lbOrder.CrStamp = pdePriceSelTime
           USE-INDEX ContractID NO-ERROR.

IF AVAIL lbOrder THEN DO:
   add_int(response_toplevel_id, "", lbOrder.OrderID).
   RETURN.
END.

IF piMultiSimID > 0 THEN DO:

   IF piMultiSimType EQ {&MULTISIMTYPE_PRIMARY} THEN DO:
      FIND FIRST lbOrder NO-LOCK WHERE
                 lbOrder.brand = gcBrand AND
                 lbOrder.MultiSimID = piMultiSimID AND
                 lbOrder.MultiSimType = piMultiSimType NO-ERROR.
      IF AVAIL lbOrder THEN RETURN appl_err("Primary order already exists").
   END.
   ELSE IF piMultiSimType EQ {&MULTISIMTYPE_SECONDARY} THEN DO:

      FIND FIRST lbOrder NO-LOCK WHERE
                 lbOrder.brand = gcBrand AND
                 lbOrder.MultiSimID = piMultiSimID AND
                 lbOrder.MultiSimType = {&MULTISIMTYPE_PRIMARY} NO-ERROR.
      IF NOT AVAIL lbOrder THEN
         RETURN appl_err("Primary order not found").
      ELSE DO:
         IF LOOKUP(lbOrder.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 THEN
         RETURN appl_err("Primary order is closed").
      END.
      
      FIND FIRST lbOrder NO-LOCK WHERE
                 lbOrder.brand = gcBrand AND
                 lbOrder.MultiSimID = piMultiSimID AND
                 lbOrder.MultiSimType = piMultiSimType NO-ERROR.
      IF AVAIL lbOrder AND LOOKUP(lbOrder.Statuscode,{&ORDER_CLOSE_STATUSES}) = 0 THEN
         RETURN appl_err("Secondary order already exists").
   END.
   ELSE RETURN appl_err(SUBST("Incorrect multisim type &1", piMultiSimType)).

END.

IF piMultiSimType NE 0 AND piMultiSimId <= 0 THEN 
   RETURN appl_err("Multi SIM type passed but Multi SIM ID is missing").

IF LOOKUP(pcNumberType,"new,mnp") > 0 AND
   CLIType.LineType > 0 AND
   NOT CAN-FIND(FIRST CLIType WHERE
                      CLIType.Brand = gcBrand AND
                      CLIType.CLIType = pcMobsubBundleType AND
                      CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN DO:

   IF NOT fIsMainLineSubActive(
          lcIdtype,
          lcId) THEN DO:

       IF fIsMainLineOrderPending(
         lcIdType,
         lcId,
         0)
         THEN llPendingMainLineOrder = TRUE.
         ELSE RETURN appl_err("Main line is not active").
   END.
END.

/* YBP-530 */
IF pcFusionStruct > "" THEN DO:
   lcFusionStructFields = validate_request(pcFusionStruct, 
      "fixed_line_number_type!,fixed_line_number,customer_type!,contractid,phone_book,fixed_line_mnp_old_operator,fixed_line_mnp_old_operator_name,fixed_line_mnp_old_operator_code,fixed_line_serial_number,fixed_line_mnp_time_of_change,fixed_line_product!,install_address!,billing_address").
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   ASSIGN
      lcFixedLineNumberType   = get_string(pcFusionStruct,
                                           "fixed_line_number_type")
      lcFixedLineProduct      = get_string(pcFusionStruct,"fixed_line_product")
      lcFixedLineCustomerType = get_string(pcFusionStruct,"customer_type")
      pcFixedInstallAddress = get_struct(pcFusionStruct,"install_address")
      lcFixedContractId = get_string(pcFusionStruct,"contractid") WHEN
         LOOKUP("contractid", lcFusionStructFields) > 0
      llPhoneBook = get_bool(pcFusionStruct,"phone_book") WHEN
         LOOKUP("phone_book", lcFusionStructFields) > 0
      lcFixedLineNumber       = get_string(pcFusionStruct,"fixed_line_number")
         WHEN LOOKUP("fixed_line_number",lcFusionStructFields) > 0
      lcFixedLineMNPOldOper   = get_string(pcFusionStruct,
                                           "fixed_line_mnp_old_operator")
         WHEN LOOKUP("fixed_line_mnp_old_operator",lcFusionStructFields) > 0
      lcFixedLineMNPOldOperName   = get_string(pcFusionStruct,
                                           "fixed_line_mnp_old_operator_name")
         WHEN LOOKUP("fixed_line_mnp_old_operator_name",lcFusionStructFields) > 0
      lcFixedLineMNPOldOperCode   = get_string(pcFusionStruct,
                                           "fixed_line_mnp_old_operator_code")
         WHEN LOOKUP("fixed_line_mnp_old_operator_code",lcFusionStructFields) > 0
      lcFixedLineSerialNbr   = get_string(pcFusionStruct,
                                           "fixed_line_serial_number")
         WHEN LOOKUP("fixed_line_serial_number",lcFusionStructFields) > 0
      lcFixedLineMNPTime = get_string(pcFusionStruct,
                                     "fixed_line_mnp_time_of_change")
         WHEN LOOKUP("fixed_line_mnp_time_of_change",lcFusionStructFields) > 0
      pcFixedBillingAddress   = get_struct(pcFusionStruct,"billing_address")
         WHEN LOOKUP("billing_address",lcFusionStructFields) > 0.

   IF gi_xmlrpc_error NE 0 THEN RETURN.
     
   /* YBP-542 */ 
   lcError = fCreateOrderCustomer(pcFixedInstallAddress, 
                                  gcCustomerStructFields,
                                  {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL},
                                  FALSE).
   IF lcError <> "" THEN RETURN appl_err(lcError).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
  
   /* YBP-543 */ 
   IF pcFixedBillingAddress > "" THEN DO:
      fCreateOrderCustomer(pcFixedBillingAddress,
                           gcCustomerStructFields,
                           {&ORDERCUSTOMER_ROWTYPE_FIXED_BILLING},
                           FALSE).
      IF lcError <> "" THEN RETURN appl_err(lcError).
      IF gi_xmlrpc_error NE 0 THEN RETURN.
   END.

   IF NOT pcChannel BEGINS "fusion" THEN
      RETURN appl_err(SUBST("Incorrect fusion order channel &1",pcChannel)).
   
  
END.

IF pcChannel BEGINS "fusion" AND
   NOT pcFusionStruct > "" THEN
   RETURN appl_err("Fusion order parameters are missing").

/* Quota 25 BEGIN */
IF pcQ25Struct > "" THEN DO:

   lcQ25Struct = validate_request(pcQ25Struct, "q25_extension,q25_discount,per_contract_id!,q25_contract_id").
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   ASSIGN
    /* Quota 25 extension */
    llq25_extension = get_bool(pcQ25Struct, "q25_extension")
      WHEN LOOKUP('q25_extension', lcQ25Struct) > 0
    /* Discount amount over Quota 25 */
    ldeq25_discount = get_double(pcQ25Struct, "q25_discount")
      WHEN LOOKUP('q25_discount', lcQ25Struct) > 0
    /* installment contract id - Quota 25 */
    liper_contract_id = get_pos_int(pcQ25Struct, "per_contract_id")
    /* Quota 25 Contract ID */
    lcq25_contract_id = get_string(pcQ25Struct, "q25_contract_id")
      WHEN LOOKUP('q25_contract_id', lcQ25Struct) > 0.
      
   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.


/* YBP-532 */
/*********************************************************************
 Creation and Update begins (All the validations should be done before)
**********************************************************************/

IF LOOKUP(pcNumberType,"new,mnp") > 0 THEN
   fUpdateSIM().

liOrderId = NEXT-VALUE(OrderId).

/* YBP-531 */
fCreateOrder().

/* YBP-546 */
/* Pass Mandate Information */
If pcMandateId > "" THEN 
   fCreateOrderAction(Order.Orderid,
                      "Mandate",
                      pcMandateId,
                      "").

fSplitTS(Order.CrStamp,OUTPUT ldaOrderDate,OUTPUT liOrderTime).

/* YBP-547 */
/* Apply discount to the subscription */
IF AVAIL DiscountPlan THEN 
   fCreateOrderAction(Order.Orderid,
                      "Discount",
                      STRING(DiscountPlan.DPId),
                      "amount=" + STRING(pdeDiscountPlanAmount) +
                      "|valid_period=" + STRING(piDiscountValidPeriod)).

/* YBP-548 */
IF pcMemo NE "" THEN 
   fCreateMemo("Info", pcMemo, pcSalesMan).

/* YBP-549 */
IF plCheck THEN 
   fCreateMemo("ORDER CHANNEL in Standalone model", 
               "When order made channel was in standalone mode," 
                + " please notice this.",
               pcSalesMan).

/* YBP-550 */
fCreateOrderCustomer(pcCustomerStruct, gcCustomerStructFields, 1, TRUE).
pcAccount = "".

/* YBP-551 */
IF pcAddressStruct > "" THEN 
   fCreateOrderCustomer(pcAddressStruct, gcCustomerStructFields, 4, TRUE).

/* YBP-552 */
IF pcContactStruct > "" THEN 
   fCreateOrderCustomer(pcContactStruct, gcCustomerStructFields, 5, TRUE).
   
/* YBP-553 */
IF pcFixedInstallAddress > "" THEN 
   fCreateOrderCustomer(pcFixedInstallAddress,
                        gcCustomerStructFields,
                        {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL},
                        TRUE). 

/* YBP-554 */
IF pcFixedBillingAddress > "" THEN 
   fCreateOrderCustomer(pcFixedBillingAddress,
                        gcCustomerStructFields,
                        {&ORDERCUSTOMER_ROWTYPE_FIXED_BILLING},
                        TRUE).
                                               
/* YBP-555 */
/* mobsub handling */
IF LOOKUP(pcNumberType,"renewal,stc") > 0 THEN DO:

   ASSIGN Order.MsSeq   = MobSub.MsSeq
          Order.Custnum = Customer.Custnum.
   FIND FIRST OrderCustomer WHERE
      OrderCustomer.Brand = gcBrand AND 
      OrderCustomer.OrderId = Order.OrderId AND
      OrderCustomer.Rowtype = 1 EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN
      OrderCustomer.Custnum = Customer.Custnum
      OrderCustomer.Language = STRING(Customer.Language)
      OrderCustomer.BankCode = Customer.BankAcc 
         WHEN OrderCustomer.BankCode = "".
END.

/* YPR-2105 */
IF Order.OrderChannel BEGINS "retention" THEN
   FOR EACH lbOrder NO-LOCK WHERE
            lbOrder.Brand = gcBrand AND
            lbOrder.CLI = Order.CLI AND
            lbOrder.StatusCode = {&ORDER_STATUS_OFFER_SENT} AND
            ROWID(lbOrder) NE ROWID(Order):

      RUN closeorder.p(lbOrder.OrderId, TRUE).

      IF RETURN-VALUE > "" THEN 
         fCreateMemo("Automatic order closing failed", 
                     SUBST("Failed to close pending order. " + 
                           "Order ID: &1, Error: &2", 
                           lbOrder.orderid, RETURN-VALUE),
                     "Newton RPC").
   END.


/* YBP-556 */
/* With STC order, allow ongoing renewal orders */ 
IF fOngoingOrders(pcCli,pcNumberType) THEN DO:

   Order.statuscode = "4".
   fCreateMemo("Order exists with same MSISDN", 
               SUBST("Orderid: &1", Order.orderid),
               "Newton RPC").

END.

/* YBP-557 */
IF pcNumberType EQ "new" AND NOT llROIClose THEN 
DO:
   FIND CURRENT msisdn EXCLUSIVE-LOCK.

   fMakeMsidnHistory(recid(msisdn)).
   
   msisdn.statuscode = 2.
   msisdn.orderid = Order.OrderId.
END.

IF llROIClose THEN .
/* YBP-558 */
/* MNP Retention Project */
ELSE IF Order.statuscode NE "4" AND
   Order.OrderChannel BEGINS "retention" AND
   fIsMNPOutOngoing(INPUT Order.CLI) THEN DO:

   Order.StatusCode = {&ORDER_STATUS_MNP_RETENTION}.

   IF Order.OrderChannel = "retention_stc" THEN DO:
      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq = Order.MsSeq AND
                 MsRequest.ReqType = 0 AND
                 LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL MsRequest THEN
         MsRequest.ReqIParam2 = Order.OrderId.
      ELSE DO:
         Order.StatusCode = {&ORDER_STATUS_IN_CONTROL}.
         fCreateMemo("STC request was not found", 
                     "", 
                     "Newton RPC").
      END. /* ELSE DO: */
   END. /* IF Order.OrderChannel = "retention_stc" THEN DO: */
END. /* IF Order.statuscode NE "4" AND */

/* order status queue must be i.e. 4 -> 22 -> 20 -> 1 */
ELSE IF Order.statuscode NE "4" THEN DO:
   
   IF pcNumberType EQ "renewal" THEN DO:
      
      /* YBP-559 */
      FIND FIRST OrderCustomer WHERE 
         OrderCustomer.Brand = gcBrand AND
         OrderCustomer.OrderId = Order.OrderId AND
         OrderCustomer.RowType = 1 EXCLUSIVE-LOCK NO-ERROR.

      CASE Order.OrderChannel:
         WHEN "renewal" OR WHEN "renewal_telesales" OR WHEN "retention" OR
         WHEN "renewal_ctc" THEN DO:
            IF fCheckRenewalData() = TRUE THEN
               /* YBP-560 */
               Order.StatusCode = {&ORDER_STATUS_RENEWAL}.
            ELSE DO:

               /* YBP-561 */
               Order.StatusCode = {&ORDER_STATUS_RENEWAL_HOLD}.
               
               /* YOT-1690 */
               /* YBP-562 */
               lcRenoveSMSText = fGetSMSTxt(
                                  "RenoveOnHold",
                                  TODAY,
                                  (IF AVAIL Customer
                                   THEN Customer.Language
                                   ELSE 1),
                                   OUTPUT ldeSMSStamp).
            
               IF lcRenoveSMSText > "" AND
                  plSendOffer EQ FALSE AND
                  LOOKUP(pcROIresult,"risk,busy,concern,ParamsException,inspectionException") = 0 THEN DO:
                  lcRenoveSMSText = REPLACE(lcRenoveSMSText,"#ORDER_NUMBER",STRING(Order.OrderId)).
                  fMakeSchedSMS2(Order.CustNum,
                                Order.CLI,
                                {&SMSTYPE_AFTER_SALES_ORDER},
                                lcRenoveSMSText,
                                ldeSMSStamp,
                                "622",
                                "").
               END.
            END.
         END.
         WHEN "renewal_pos" THEN DO:
           /* Address is changed then customer address should be */
           /* changed without sending order into Renewal Queue   */
           Order.StatusCode = {&ORDER_STATUS_RENEWAL}.
           OrderCustomer.DataChecked = TRUE.
         END. /* WHEN "renewal_pos" THEN DO: */
         WHEN "renewal_pos_stc" OR WHEN "retention_stc" THEN DO:
            /* YBP-563 */
            /* Address is changed then customer address should be */
            /* changed without sending order into Renewal Queue   */
            OrderCustomer.DataChecked = TRUE.

            FIND FIRST MsRequest WHERE
                       MsRequest.MsSeq = Order.MsSeq AND
                       MsRequest.ReqType = 0 AND
            LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0
            EXCLUSIVE-LOCK NO-ERROR.

            IF AVAIL MsRequest THEN DO:
               Order.StatusCode = (IF OrderCustomer.CustIdType EQ "CIF" 
                                   THEN {&ORDER_STATUS_RENEWAL_STC_COMPANY}
                                   ELSE {&ORDER_STATUS_RENEWAL_STC}).
               MsRequest.ReqIParam2 = Order.OrderId.
            END.
            ELSE DO:
               Order.StatusCode = {&ORDER_STATUS_IN_CONTROL}.
               fCreateMemo("STC request was not found", 
                  "", 
                  "Newton RPC").
            END.
         END.
      END.
   
      /* YBP-564 */
      /* SMS for Renewal in POS. YOT-1565 */
      IF LOOKUP(Order.OrderChannel,"renewal_pos,renewal_pos_stc") > 0 THEN DO:

         lcRenoveSMSText = fGetSMSTxt(
                            "RenoveOrderPOS",
                            TODAY,
                            (IF AVAIL Customer
                             THEN Customer.Language
                             ELSE 1),
                            OUTPUT ldeSMSStamp).
               
         IF lcRenoveSMSText > "" THEN DO:
            ldeSMSStamp = DYNAMIC-FUNCTION("fMakeOfficeTS" in ghFunc1).
            fMakeSchedSMS(Order.CustNum,
                          Order.CLI,
                          {&SMSTYPE_AFTER_SALES_ORDER},
                          lcRenoveSMSText,
                          ldeSMSStamp).

         END.
      END.

      FIND CURRENT OrderCustomer NO-LOCK.
   END.   
   ELSE IF lcIdType = "CIF" THEN fHandleCorporateCustomer().
   ELSE IF pcChannel BEGINS "fusion" THEN DO:
      /* YBP-565 */
      Order.statuscode = {&ORDER_STATUS_PENDING_FIXED_LINE}.
   END.
   ELSE DO:
      Order.statuscode = STRING(INT(pcNumberType EQ "mnp") * 2 + 1).
 
      /* YBP-566 */
      /* note: cif newspaper campaigns are handled normally */
      IF NUM-ENTRIES(pcCampaignCode,";") >= 3 AND
         ENTRY(3, pcCampaignCode,";") = "newspaper" AND 
         Order.OrderChannel EQ "cc" THEN
         Order.StatusCode = "23".

      IF Order.MultiSimID > 0 AND
         Order.MultiSimType EQ {&MULTISIMTYPE_SECONDARY} THEN DO:
         FIND FIRST lbOrder NO-LOCK WHERE
                    lbOrder.Brand = gcBrand AND
                    lbOrder.MultiSimID = Order.MultiSimID AND
                    lbOrder.MultiSimType = {&MULTISIMTYPE_PRIMARY} NO-ERROR.
         IF AVAIL lbOrder AND
                  lbOrder.StatusCode NE {&ORDER_STATUS_DELIVERED} THEN
            Order.StatusCode = {&ORDER_STATUS_PENDING_MAIN_LINE}.
      END.

      IF llPendingMainLineOrder THEN
         /* YBP-567 */
         Order.StatusCode = {&ORDER_STATUS_PENDING_MAIN_LINE}.
      ELSE IF Order.PortingDate <> ? THEN
         /* YBP-568 */
         Order.StatusCode = {&ORDER_STATUS_MNP_ON_HOLD}.
   END.
END.

CASE pcNumberType:
   WHEN "new" THEN Order.OrderType = {&ORDER_TYPE_NEW}.
   WHEN "mnp" THEN Order.OrderType = {&ORDER_TYPE_MNP}.
   WHEN "renewal" THEN Order.OrderType = {&ORDER_TYPE_RENEWAL}.
   WHEN "stc" THEN Order.OrderType = {&ORDER_TYPE_STC}.
END.

/* YBP-570 */ 
fCreateOrderTopup().

/* YPR-2478 */
fCreateAccessory().

/* YBP-571 */ 
IF (pcDeviceStruct > "" AND 
    get_paramcount(pcDeviceStruct) > 0) OR
   liTermOfferItemID > 0 THEN fCreateOrderAccessory().

IF piPaymentMethod NE 0 THEN 
DO:
   /* YBP-572 */ 
   fCreateOrderPayment().
   IF piPaymentMethod EQ {&ORDERPAYMENT_M_POD} THEN 
   DO:
      IF liTermOfferItemID > 0 OR pcIMEI NE ""
         THEN Order.FeeModel = "PAYDELTER".
      ELSE IF pcNumberType EQ "New" AND order.paytype THEN
         Order.FeeModel = "PAYDELSIM".
   END.      
END.

/* YBP-573 */ 
/* redefine the order status code taking into account the Real Order Inspection */
CASE pcROIresult:
     WHEN "risk" THEN DO:

         IF LOOKUP(pcROILevel,"7,8") > 0 THEN DO:
            ASSIGN
               Order.StatusCode = pcROILevel
               Order.SendToROI = {&ROI_HISTORY_TO_SEND}
                  WHEN Order.Ordertype NE {&ORDER_TYPE_STC}.
            fMarkOrderStamp(Order.OrderID,"Close",0.0). 
         END.
         ELSE IF NOT plSendOffer THEN
            Order.StatusCode = STRING(40 + INTEGER(pcROIlevel)).
            
         IF pcROIruleId NE '' THEN
            fCreateMemo("ROI Risk Rule_Id", 
                      pcROIruleId,
                      "Newton RPC ROI").
         IF pcROIdescription NE '' THEN
             fCreateMemo("ROI Description", 
                         pcROIdescription,
                         "Newton RPC ROI").

     END.
     WHEN "exception" THEN DO:
         fCreateMemo("ROI Description", 
                   pcROIdescription,
                   "Newton RPC ROI").
     END.
     WHEN "unexpected response" THEN DO:
         fCreateMemo("ROI Unexpected Response", 
                   pcROIdescription,
                   "Newton RPC ROI").
     END.
     WHEN "busy" OR WHEN "concern" OR
     WHEN "ParamsException" OR WHEN "inspectionException" THEN DO:
         Order.StatusCode = "43".
         IF pcROIdescription NE '' THEN
             fCreateMemo("ROI Description",
                         pcROIdescription,
                         "Newton RPC ROI").

     END.
END.

/* YBP-574 */ 
/* add databundle */
IF pcDataBundleType > "" THEN
   fCreateOrderAction(Order.Orderid,"BundleItem",pcDataBundleType,"").
IF pcMobSubBundleType > "" THEN DO:
   lcOnlyVoiceContracts = fCParamC("ONLY_VOICE_CONTRACTS").
   fCreateOrderAction(Order.Orderid,"BundleItem",pcMobSubBundleType,"").
   IF LOOKUP(pcMobSubBundleType,lcOnlyVoiceContracts + ",CONTS15") > 0 THEN
      fCreateOrderAction(Order.Orderid,"BundleItem","GPRS","").
END.

/* YBP-575 */ 
IF plDSSActivate THEN fCreateOrderAction(Order.Orderid,"BundleItem",{&DSS},"").

/* YBP-576 */ 
IF plBonoVoipActivate THEN
   fCreateOrderAction(Order.Orderid,"BundleItem","BONO_VOIP","").

/* YBP-577 */ 
/* SIM Type */
IF lcSimType > "" THEN
   fCreateOrderAction(Order.Orderid,"SIMType",lcSimType,"").

/* YBP-578 */ 
/* Exclude Term Penalty for all kind of renewal orders */
IF plExcTermPenalty AND Order.OrderType = {&ORDER_TYPE_RENEWAL} THEN
   fCreateOrderAction(Order.Orderid,"ExcludeTermPenalty","Yes","").

/* YBP-579 */ 
/* Exclude termination of PayTerm in renewal order */
IF plKeepInstallment AND Order.OrderType = {&ORDER_TYPE_RENEWAL} THEN
   fCreateOrderAction(Order.Orderid,"KeepInstallment","Yes","").

/* YBP-580 */ 
/* Extend TERMx contract with STC orders */
IF plExtendTermContract AND Order.OrderType = {&ORDER_TYPE_STC} THEN
   fCreateOrderAction(Order.Orderid,"ExtendTermContract","Yes","").

/* YBP-581 */ 
/* Creating Promotional offer */
IF plPromotion THEN 
   fCreateOrderAction(Order.Orderid,"Promotion","TARJ7",""). 

IF pcUpsHours NE "" THEN 
   fCreateOrderAction(Order.Orderid,"UPSHours",pcUpsHours,""). 

/* Create Quota 25 extension request */
IF llq25_extension THEN
   fCreateOrderAction(Order.Orderid,
      "Q25Extension",
      lcq25_contract_id,
      STRING(liper_contract_id)).

/* Create Quota 25 discount */
IF ldeq25_discount > 0 THEN
   fCreateOrderAction(Order.Orderid,
      "Q25Discount",
      STRING(ldeq25_discount),
      STRING(liper_contract_id)).

/* YBP-582 */ 
IF pcChannel BEGINS "fusion" THEN
   fCreateOrderFusion().

/* YBP-583 */ 
/* Create ICC change required, if new ICC is specified */
IF Order.OrderChannel BEGINS "Renewal_POS" AND Order.ICC > "" AND
   NOT llRoiClose THEN DO:

   ASSIGN liRequest = 0
          lcError   = ""
          katun     = Order.SalesMan.

   liRequest =  fSubscriptionRequest
                   (INPUT  Order.MSSeq,
                    INPUT  Order.CLI,
                    INPUT  Order.CustNum,
                    INPUT  1,
                    INPUT  "",
                    INPUT  fMakeTS(),
                    INPUT  "CHANGEICC",
                    INPUT  Order.ICC,
                    "",
                    "",
                    "",
                    INPUT  FALSE,
                    INPUT  0.0,
                    INPUT {&REQUEST_SOURCE_ICC_CHANGE_AUTO},
                    OUTPUT lcError).
   IF liRequest = 0 THEN
      fCreateMemo("ICC change request creation failed", 
                  SUBST("Orderid: &1", Order.orderid),
                  "Newton RPC").
   /* YBP-584 */ 
   /* Update SIM status to reserve for ICC change */
   ELSE DO:
      fUpdateSIM().

      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MsRequest",
                       STRING(liRequest),
                       Order.CustNum,
                       "ICC TYPE CHANGE AUTO",
                       Order.OrderChannel).
   END. /* ELSE DO: */

   katun = "NewtonRPC".

END. /* IF Order.OrderChannel BEGINS "Renewal_POS" AND Order.ICC > "" */
 
/* YBP-585 */ 
IF Order.OrderType EQ {&ORDER_TYPE_STC} AND
   NOT llRoiClose THEN DO:

   IF pcMobSubBundleType > "" THEN lcSMSKey = "STCORDER" + pcMobSubBundleType.
   ELSE lcSMSKey = "STCORDER" + pcSubType.
               
   lcSTCSMSText = fGetSMSTxt(
                     lcSMSKey,
                     TODAY,
                     (IF AVAIL Customer
                      THEN Customer.Language
                      ELSE 1),
                      OUTPUT ldeSMSStamp).

   IF lcSTCSMSText > "" THEN
   fMakeSchedSMS2(Order.CustNum,
                  Order.CLI,
                  {&SMSTYPE_INFO},
                  lcSTCSMSText,
                  ldeSMSStamp,
                  "22622",
                  "").
END.

/* YPR-3317 */
IF plCustdataRetr AND NOT plMultiOrder THEN DO:
   IF pcIdentifiedSmsNumber EQ "" THEN
      RETURN appl_err(SUBST("Identified customer SMS number missing.")).
   lcOrderSMSText = fGetSMSTxt(
                     "IdentifiedCustOrder",
                     TODAY,
                     (IF AVAIL Customer
                      THEN Customer.Language
                      ELSE 1),
                      OUTPUT ldeSMSStamp).

   IF lcOrderSMSText > "" THEN DO:
      lcOrderSMSText = REPLACE(lcOrderSMSText, "#CLI", Order.CLI). 
      fMakeSchedSMS2(Order.CustNum,
                     pcIdentifiedSmsNumber,
                     {&SMSTYPE_INFO},
                     lcOrderSMSText,
                     ldeSMSStamp,
                     "622",
                     "").
   END. 
END.

/* should overwrite any roi status */
IF plSendOffer AND NOT llROIClose THEN DO:

   Order.StatusCode = {&ORDER_STATUS_OFFER_SENT}.

   IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} OR
      Order.OrderType EQ {&ORDER_TYPE_MNP} THEN
      lcMobileNumber = Order.CLI.
   ELSE IF fIsMobileNumber(OrderCustomer.MobileNumber) THEN
      lcMobileNumber = OrderCustomer.MobileNumber.
   ELSE IF fIsMobileNumber(OrderCustomer.FixedNumber) THEN
      lcMobileNumber = OrderCustomer.FixedNumber.

   lcOfferSMSText = fGetOrderOfferSMS(Order.OrderID, 
                                      TRUE).

   IF lcMobileNumber NE "" AND 
      lcOfferSMSText NE "" AND lcOfferSMSText NE ? THEN
      fCreateSMS(Order.CustNum,
                 lcMobileNumber,
                 Order.MsSeq, 
                 Order.OrderId,
                 lcOfferSMSText,
                 "622100100",
                 {&SMS_TYPE_OFFER}).

END.

      
/* YTS-2890 */
fMakeCreateEvent((BUFFER Order:HANDLE),"",katun,"").
fMarkOrderStamp(Order.OrderID,"Change",0.0).


/*YDR_1637*/
IF INDEX(Order.OrderChannel, "pos") EQ 0 THEN DO:
   IF (Order.StatusCode EQ {&ORDER_STATUS_MNP_ON_HOLD}        /*22*/ OR
       Order.StatusCode EQ {&ORDER_STATUS_RESIGNATION}        /*51*/ OR
       Order.StatusCode EQ {&ORDER_STATUS_PENDING_MAIN_LINE}  /*76*/ OR
       Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} /*77*/ ) THEN DO:
      RUN prinoconf.p(liOrderId).
   END.
   ELSE IF Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_1}    /*41*/  OR
           Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_2}    /*42*/  OR
           Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_3}    /*43*/   
           THEN DO: 
      RUN sendorderreq.p(liOrderId, OrderCustomer.email, OUTPUT lcError). 
   END.
END.

add_int(response_toplevel_id, "", liOrderId).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
