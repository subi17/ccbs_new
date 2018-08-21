/**
 * Create an order
 *
 * @input   orderdata;struct;order data
            customerdata;struct;order customer data

 * @orderdata  msisdn;string;mandatory;phone number 
               icc;string;mandatory; 
               offer_id;string;mandatory 
               billing_data;string;optional;bank account
               subscription_type;string;mandatory;
               contractid;string;optional;
               subscription_bundle;string;optional;mandatory base bundle for CONTRD or CONTF
 * @customerdata  fname;string;mandatory;
                  lname;string;mandatory;
                  lname2;string;optional;
                  title;string;optional;
                  foundation_date;datetime;optional
                  site_name;string;optional;
                  region;string;mandatory;
                  street;string;mandatory;
                  zip;int;mandatory;
                  city;string;mandatory;
                  country;string;mandatory;
                  street_code:string;optional;
                  city_code;string;optional;
                  nationality;string;mandatory;
                  birthday;datetime;mandatory;
                  email;string;mandatory;
                  sms_number;string;mandatory;
                  phone_number;string;optional;
                  mark_post;boolean;optional;
                  mark_sms;boolean;optional;
                  mark_email;boolean;optional;
                  mark_post_3rd;boolean;optional;
                  mark_sms_3rd;boolean;optional;
                  mark_email_3rd;boolean;optional;
                  language;string;mandatory;
                  person_id;string;mandatory;id of the owner
                  id_type;string;optional;NIF,NIE,CIF or Passport
                  company_id;string;optional;company CIF
 * @contactdata   fname;string;mandatory;
                  lname;string;mandatory;
                  lname2;string;optional;
                  title;string;optional;
                  region;string;mandatory;
                  street;string;mandatory;
                  zip;int;mandatory;
                  city;string;mandatory;
                  country;string;mandatory;
                  street_code:string;optional;
                  city_code;string;optional;
                  nationality;string;mandatory;
                  birthday;datetime;mandatory;
                  email;string;mandatory;
                  sms_number;string;mandatory;
                  phone_number;string;optional;
                  mark_post;boolean;optional;
                  mark_sms;boolean;optional;
                  mark_email;boolean;optional;
                  mark_post_3rd;boolean;optional;
                  mark_sms_3rd;boolean;optional;
                  mark_email_3rd;boolean;optional;
                  language;string;mandatory;
                  person_id;string;mandatory;id of the owner
                  id_type;string;optional;NIF,NIE,CIF or Passport
 * @output  orderid;int;handle for order
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.

{Syst/commpaa.i}
Syst.Var:katun = ghAuthLog::UserName + "_" + ghAuthLog::EndUserId.
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/orderchk.i}
{Func/terminaloffer.i}
{Func/create_eventlog.i}
{Mm/fbundle.i}

DEF VAR top_array       AS CHAR NO-UNDO.
/* Input parameters for order */
DEF VAR pcOrderStruct   AS CHAR NO-UNDO.
DEF VAR lcOrderStruct   AS CHAR NO-UNDO.

/* Input parameters for customer */
DEF VAR pcCustomerStruct AS CHAR NO-UNDO.
DEF VAR pcContactStruct AS CHAR NO-UNDO.

DEF VAR gcOrderStructFields AS CHARACTER NO-UNDO.  
DEF VAR gcCustomerStructFields AS CHARACTER NO-UNDO.
DEF VAR gcCustomerStructStringFields AS CHARACTER NO-UNDO. 

DEF VAR pcSalesman      AS CHAR NO-UNDO INIT "vip".
DEF VAR pcChannel       AS CHAR NO-UNDO INIT "vip".


DEF VAR pcAccount       AS CHAR NO-UNDO.

DEF VAR pcCLI           AS CHAR NO-UNDO.
DEF VAR pcSubType       AS CHAR NO-UNDO.
DEF VAR pcNumberType    AS CHAR NO-UNDO INIT "new".
DEF VAR pcIcc           AS CHAR NO-UNDO INIT "".
DEF VAR pcContractId    AS CHAR NO-UNDO.
DEF VAR pfTopup         AS DEC NO-UNDO INIT 0.
DEF VAR plSendSMS       AS LOG NO-UNDO INIT TRUE.
DEF VAR pcOfferId       AS CHAR NO-UNDO.
DEF VAR pdePriceSelTime AS DEC NO-UNDO.
DEF VAR liTermOfferItemID AS INTEGER NO-UNDO.
DEF VAR llMatchSubType AS LOGICAL NO-UNDO.
DEF VAR pcMobsubBundleType AS CHAR NO-UNDO.
DEF VAR lcOnlyVoiceContracts   AS CHAR NO-UNDO.
DEF VAR lcIPLContracts         AS CHAR NO-UNDO.
DEF VAR lcFLATContracts        AS CHAR NO-UNDO.
DEF VAR lcCONTDContracts       AS CHAR NO-UNDO.
DEF VAR lcCONTSContracts       AS CHAR NO-UNDO.
DEF VAR lcBundleCLITypes       AS CHAR NO-UNDO.

/* Local variables for order */
DEF VAR liOrderId       AS INT NO-UNDO.

/* Local variables for customer */

DEF VAR lccTemp AS CHARACTER NO-UNDO. 
DEF VAR lcError AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcIdType AS CHARACTER NO-UNDO.  
DEFINE VARIABLE lcId AS CHARACTER NO-UNDO. 

FUNCTION fGetOrderFields RETURNS LOGICAL :
   
   IF LOOKUP("billing_data", lcOrderStruct) GT 0 THEN
       pcAccount = get_string(pcOrderStruct, "billing_data").

   pcCLI = get_string(pcOrderStruct, "msisdn").
   pcIcc = get_string(pcOrderStruct, "ICC").
   pcOfferId = get_string(pcOrderStruct, "offer_id").
   pcSubType = get_string(pcOrderStruct, "subscription_type").

   IF LOOKUP("subscription_bundle",lcOrderStruct) > 0 THEN
      pcMobsubBundleType = get_string(pcOrderStruct,"subscription_bundle").

   IF LOOKUP("contractid", lcOrderStruct) GT 0 THEN
      pcContractId = get_string(pcOrderStruct, "contractid").

   pdePriceSelTime = {&nowTS}.
 
   RETURN TRUE.
END.

FUNCTION fCreateOrderCustomer RETURNS CHARACTER 
     (INPUT pcStructId AS CHARACTER,
      INPUT pcStructFields AS CHARACTER,
      INPUT piRowType AS INTEGER,
      INPUT plUpdate  AS LOGICAL):

   DEF VAR lcFError AS CHAR NO-UNDO. 
   DEFINE VARIABLE iData AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcc   AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lii   AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcField AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liFieldIndex AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcMarkOut    AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMarketing  AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE ldBirthDay   AS DATE NO-UNDO. 
   DEFINE VARIABLE ldFoundationDate AS DATE NO-UNDO. 
   DEFINE VARIABLE liLanguage   AS INTEGER NO-UNDO. 
   DEFINE VARIABLE data            AS CHAR EXTENT 19 NO-UNDO.
   DEFINE VARIABLE lcIdOrderCustomer AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcIdTypeOrderCustomer AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcContactId AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcContactIdType AS CHARACTER NO-UNDO.

   lcFError = "".
   
   REPEAT iData = 1 TO EXTENT(data):
      data[iData] = "".
   END. /* iData = */
   
   data[LOOKUP("country", gcCustomerStructStringFields)] = "ES".
   data[LOOKUP("nationality", gcCustomerStructStringFields)] = "ES".
   
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
      ELSE IF lcField EQ "foundation_date" THEN 
      DO:
         ldFoundationDate = get_date(pcStructId, lcField).
      END. /* IF lcField EQ "foundation_date" ... */
      ELSE IF lcField EQ "language" THEN 
      DO:
         liLanguage = LOOKUP(get_string(pcStructId, lcField),
                                 "es_ES,es_CA,es_EU,es_GA,en").
      END. /* IF lcField EQ "language" ... */
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
      IF data[LOOKUP("company_id", gcCustomerStructStringFields)] ne ""
      THEN DO:
          lcContactId = lcIdOrderCustomer.
          lcIdOrderCustomer = data[LOOKUP("company_id", gcCustomerStructStringFields)].
          lcContactIdType = lcIdtypeOrderCustomer.
          lcIdtypeOrderCustomer = "CIF".
      END. /* data[LOOKUP("company_id", ... */
      IF lcIdOrderCustomer EQ "" AND piRowType = 1 THEN
          lcFError = "Expected either person_id or company_id".
      
   END. /* lcFError = "" */
  
   IF plUpdate AND lcFError eq "" THEN
   DO:
      CREATE OrderCustomer.
      ASSIGN
         OrderCustomer.Brand           = Syst.Var:gcBrand 
         OrderCustomer.OrderId         = liOrderId
         OrderCustomer.CustId          = lcIdOrderCustomer 
         OrderCustomer.CustIdType      = lcIdtypeOrderCustomer
         OrderCustomer.AuthCustId      = lcContactId
         OrderCustomer.AuthCustIdType  = lcContactIdType
         OrderCustomer.RowType         = piRowType
         OrderCustomer.BankCode        = pcAccount
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
         OrderCustomer.Street            = 
            data[LOOKUP("street", gcCustomerStructStringFields)]
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
         OrderCustomer.FoundationDate     = ldFoundationDate
         OrderCustomer.Birthday           = ldBirthday
         OrderCustomer.Language           = STRING(liLanguage)
         OrderCustomer.OperSMSMarketing   = (LOOKUP("SMS", lcMarketing, "|") NE 0)
         OrderCustomer.OperEmailMarketing = (LOOKUP("Email", lcMarketing, "|") NE 0)
         OrderCustomer.OperPostMarketing  = (LOOKUP("Post", lcMarketing, "|") NE 0)
         OrderCustomer.OutSMSMarketing    = (LOOKUP("SMS", lcMarkOut, "|") NE 0)
         OrderCustomer.OutEMailMarketing  = (LOOKUP("Email", lcMarkOut, "|") NE 0)
         OrderCustomer.OutPostMarketing   = (LOOKUP("Post", lcMarkOut, "|") NE 0)
         OrderCustomer.OperAllMarketing   = lcMarketing NE "".

         /* set address data */
         OrderCustomer.Address = OrderCustomer.Street. 
         /* Correct SPAIN to ES if incorrect code used */
         IF OrderCustomer.Country EQ "SPAIN" THEN 
            ASSIGN OrderCustomer.Country = "ES".
   END.

   IF piRowType = 1 THEN
   DO:
      lcId      = lcIdOrderCustomer.
      lcIdType  = lcIdTypeOrderCustomer.
   END.

   RETURN lcFError.
END.



FUNCTION fCheckMSISDN RETURNS CHARACTER:
   
   DEF VAR lcError AS CHAR NO-UNDO INIT "".

   IF pcNumberType EQ "new" THEN DO:

      FIND FIRST MSISDN NO-LOCK
         WHERE msisdn.brand EQ Syst.Var:gcBrand 
         AND MSISDN.ValidTo GE {&nowts}
         AND msisdn.cli EQ pcCLI
         AND msisdn.statuscode EQ 1 
         NO-ERROR.

      IF NOT AVAILABLE MSISDN THEN
         lcError = SUBST("Cli &1 not found or not free", pcCLI).
   END.
   RETURN lcError.
END.


FUNCTION fCheckSIM RETURNS CHARACTER:
   DEF VAR lcError AS CHAR NO-UNDO. 
   lcError = "".

   IF pcIcc NE '' THEN 
   DO:
       FIND FIRST SIM EXCLUSIVE-LOCK
          WHERE SIM.brand EQ Syst.Var:gcBrand 
          AND SIM.ICC EQ pcIcc
          AND SIM.simstat EQ 1 
          NO-ERROR.
       IF NOT AVAILABLE SIM THEN
          lcError = SUBST("SIM with ICC &1 not found or not free", pcIcc).
   END.

   RETURN lcError.
END.


FUNCTION fUpdateSIM RETURNS LOGICAL:
   IF pcIcc NE '' THEN 
   DO:
       SIM.simstat = 4.
   END.
   RETURN TRUE.
END.


FUNCTION fCreateOrder RETURNS LOGICAL:
   CREATE Order.
   ASSIGN
      Order.Brand           = Syst.Var:gcBrand 
      Order.OrderId         = liOrderId
      Order.Salesman        = pcSalesman
      Order.Source          = "external"
      Order.OrderChannel    = pcChannel
      Order.CrStamp         = pdePriceSelTime 
      Order.InvCustRole     = 1
      Order.UserRole        = 1
      Order.StatusCode      = "1"
      Order.CLI             = pcCLI
      Order.CLIType         = pcSubType
      Order.mnpstatus       = INT(pcNumberType EQ "mnp")
      Order.paytype         = (IF CLIType.PayType = 2 THEN TRUE ELSE FALSE)  
      Order.ICC             = pcIcc
      Order.ContractId      = pcContractId
      Order.SMSType         = INT(plSendSMS)
      Order.Offer           = pcOfferId 
      Order.MsSeq           = NEXT-VALUE(MobSub).
END.


FUNCTION fHandleCorporateCustomer RETURNS LOGICAL:
   /*
   The Cases where CIF order should go to automated order management

   CASE 1: If the customer has any active subs. then order should go to 21
   CASE 2: If the customer has not any active subs. then order should go to 20
   */

   IF lcIdType = "CIF" THEN DO:
      FIND FIRST Customer WHERE
                 Customer.Brand      = Syst.Var:gcBrand  AND
                 Customer.OrgId      = lcId     AND
                 Customer.CustIdType = lcIdType AND
                 Customer.Roles NE "inactive" NO-LOCK NO-ERROR. 
      IF AVAIL Customer THEN DO:
         FIND FIRST MobSub WHERE
                    MobSub.Brand   = Syst.Var:gcBrand AND
                    MobSub.AgrCust = Customer.CustNum
              NO-LOCK NO-ERROR.
         IF NOT AVAIL MobSub THEN Order.StatusCode = "20".
         ELSE Order.StatusCode = "21".
      END. /* IF AVAIL Customer THEN DO: */
      ELSE Order.StatusCode = "20".
   END. /* IF lcIdType = "CIF" THEN DO: */

END. /* FUNCTION fHandleCorporateCustomer RETURNS LOGICAL: */


FUNCTION fCreateOrderTopup RETURNS LOGICAL:
   DEFINE VARIABLE lCreate AS LOGICAL NO-UNDO. 
   lCreate = FALSE.
   IF pfTopup NE 0 THEN 
   DO:
      CREATE OrderTopup.
      ASSIGN
         OrderTopup.Amount = pfTopup
         OrderTopup.Brand = Syst.Var:gcBrand 
         OrderTopup.OrderId = Order.OrderId
         /*OrderTopup.VatAmount = */
      .
      lCreate = TRUE.
   END.
   RETURN lCreate.
END.

FUNCTION fCreateOrderAccessory RETURNS LOGICAL:
   
   FIND OfferItem WHERE
        OfferItem.OfferItemId = liTermOfferItemID NO-LOCK.

   IF NOT AVAIL OfferItem THEN 
       RETURN FALSE.

   CREATE OrderAccessory.
   ASSIGN
      OrderAccessory.OrderId     = Order.OrderId
      OrderAccessory.brand       = Syst.Var:gcBrand 
      OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE}
      OrderAccessory.ProductCode = OfferItem.ItemKey 
      OrderAccessory.Amount      = OfferItem.Amount .

   RETURN TRUE.
END.


FUNCTION fCriteriaMatch RETURNS LOGIC
   (icCriteria AS CHAR):

   IF OfferCriteria.IncludedValue > "" AND 
      OfferCriteria.IncludedValue NE "*" AND 
      LOOKUP(icCriteria,OfferCriteria.IncludedValue) = 0
   THEN RETURN FALSE.
   
   IF OfferCriteria.ExcludedValue > "" AND 
      (OfferCriteria.ExcludedValue = "*" OR
       LOOKUP(icCriteria,OfferCriteria.ExcludedValue) > 0)
   THEN RETURN FALSE.
   
   RETURN TRUE.
   
END FUNCTION.




/* Input variables for address data */

gcOrderStructFields = "billing_data!," + 
                      "icc!," +
                      "msisdn!," +
                      "offer_id!," + 
                      "subscription_type!," +
                      "contractid," +
                      "subscription_bundle".

gcCustomerStructFields = "birthday!," +
                         "city!," +
                         "city_code," +
                         "street_code," +
                         "country!," +
                         "email!," +
                         "fname!," +
                         "foundation_date," +
                         "id_type!," +
                         "language!," +
                         "lname!," +
                         "lname2," +
                         "mark_email," +
                         "mark_email_3rd," +
                         "mark_post," +
                         "mark_post_3rd," +
                         "mark_sms," +
                         "mark_sms_3rd," +
                         "nationality!," +
                         "person_id!," +
                         "phone_number,"+
                         "region!," +
                         "site_name," +
                         "sms_number!," +
                         "street!," +
                         "title," +
                         "zip!," + 
                         "company_id".

gcCustomerStructStringFields = "city," +
                               "city_code," +
                               "street_code," +
                               "country," +
                               "email," +
                               "fname," +
                               "id_type," +
                               "lname," +
                               "lname2," +
                               "nationality," +
                               "person_id," +
                               "phone_number," +
                               "region," +
                               "site_name," +
                               "sms_number," +
                               "street," +
                               "title," +
                               "zip," +
                               "company_id".

/* common validation */
top_array = validate_request(param_toplevel_id, 
   "struct,struct,struct").
IF top_array EQ ? THEN RETURN.
pcOrderStruct     = get_struct(param_toplevel_id, "0").
pcCustomerStruct  = get_struct(param_toplevel_id, "1").
pcContactStruct   = get_struct(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

/* order validation */
lcOrderStruct = validate_request(pcOrderStruct, gcOrderStructFields).
IF lcOrderStruct EQ ? THEN RETURN.
fGetOrderFields().

{viptool/src/findtenant.i YES ordercanal CliType CliType pcSubType}

IF pcOfferId NE "" THEN DO:
   FIND Offer WHERE 
        Offer.Brand = Syst.Var:gcBrand AND 
        Offer.Offer = pcOfferId NO-LOCK NO-ERROR.
   IF NOT AVAIL Offer THEN
      RETURN appl_err("Offer " + pcOfferId + " is not defined").
   liTermOfferItemID = fGetTerminalOfferItemId(Offer.Offer, {&BITEM_GRP_TERMINAL}, pdePriceSelTime).
   
   /* check CLIType using OfferCriteria */
   FIND FIRST OfferCriteria WHERE
              OfferCriteria.Brand = Syst.Var:gcBrand AND
              OfferCriteria.Offer = Offer.Offer AND
              OfferCriteria.EndStamp   >= pdePriceSelTime AND
              OfferCriteria.BeginStamp <= pdePriceSelTime AND
              OfferCriteria.CriteriaType = "CLIType" NO-LOCK NO-ERROR.
    IF AVAIL OfferCriteria THEN DO: 
       llMatchSubType = fCriteriaMatch(pcSubType).
       IF NOT llMatchSubType THEN 
          RETURN appl_err("Subscription type doesn't match with Offer criteria ").
    END.

END.

/* validate MSISDN */
lcError = fCheckMSISDN().
IF lcError <> "" THEN appl_err(lcError).
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN lcIPLContracts   = fCParamC("IPL_CONTRACTS")
       lcCONTDContracts = fCParamC("CONTD_CONTRACTS")
       lcCONTSContracts = fCParamC("CONTS_CONTRACTS")
       lcFLATContracts  = fCParamC("FLAT_CONTRACTS")
       lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

IF pcMobsubBundleType > "" THEN DO:
   IF LOOKUP(pcMobsubBundleType,lcIPLContracts)  = 0 AND
      LOOKUP(pcMobsubBundleType,lcCONTDContracts) = 0 AND
      LOOKUP(pcMobsubBundleType,lcFLATContracts) = 0 AND
      LOOKUP(pcMobsubBundleType,lcCONTSContracts) = 0 THEN RETURN
      appl_err(SUBST("Incorrect subscription bundle type: &1", pcMobsubBundleType)).

   IF NOT fIsBundleAllowed(INPUT pcSubType,
                           INPUT pcMobsubBundleType,
                           OUTPUT lcError)
   THEN RETURN appl_err(lcError).
END. /* IF pcMobsubBundleType > "" THEN DO: */
ELSE IF LOOKUP(pcSubType,lcBundleCLITypes) > 0 THEN
   RETURN appl_err("Subscription bundle type is missing").

/* Customer validation */
lccTemp = validate_request(pcCustomerStruct, gcCustomerStructFields).
IF gi_xmlrpc_error NE 0 THEN RETURN.
/* Contact person validation */
IF get_paramcount(pcContactStruct) GT 0 THEN
DO:
   lccTemp = validate_request(pcContactStruct, gcCustomerStructFields).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.

lcError = fCreateOrderCustomer(pcCustomerStruct, gcCustomerStructFields, {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}, FALSE).
IF lcError <> "" THEN appl_err(lcError).
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF get_paramcount(pcContactStruct) GT 0 THEN
   lcError = fCreateOrderCustomer(pcContactStruct, gcCustomerStructFields, {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT}, FALSE).
IF lcError <> "" THEN appl_err(lcError).
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcError = fCheckSIM().
IF lcError <> "" THEN appl_err(lcError).
IF gi_xmlrpc_error NE 0 THEN RETURN. 


/* Creation and Update begins */

fUpdateSIM().
liOrderId = NEXT-VALUE(OrderId).

fCreateOrder().

fCreateOrderCustomer(pcCustomerStruct, gcCustomerStructFields, {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}, TRUE).
pcAccount = "".
IF get_paramcount(pcContactStruct) GT 0 THEN 
   fCreateOrderCustomer(pcContactStruct, gcCustomerStructFields, {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT}, TRUE).
                                             
/* mobsub handling */
IF fOngoingOrders(pcCli,"") THEN DO:
   Order.statuscode = "4".
   Func.Common:mWriteMemoWithType("Order",
                                  STRING(Order.OrderId),
                                  0,
                                  "Order exists with same MSISDN",
                                  SUBST("Orderid: &1", Order.orderid),
                                  "",
                                  "External RPC").
END.

IF pcNumberType EQ "new" THEN 
DO:
   FIND CURRENT msisdn EXCLUSIVE-LOCK.
   msisdn.statuscode = 2.
   msisdn.orderid = Order.OrderId.
END.

IF Order.statuscode NE "4" THEN DO:     
      Order.statuscode = STRING(INT(pcNumberType EQ "mnp") * 2 + 1).
      fHandleCorporateCustomer().
END.

IF pcNumberType EQ "new" THEN 
   Order.OrderType = 0.

fCreateOrderTopup().

IF liTermOfferItemID > 0 THEN 
   fCreateOrderAccessory().

IF pcMobSubBundleType > "" THEN DO:
   lcOnlyVoiceContracts = fCParamC("ONLY_VOICE_CONTRACTS").
   fCreateOrderAction(Order.Orderid,"BundleItem",pcMobSubBundleType,"").
   IF LOOKUP(pcMobSubBundleType,lcOnlyVoiceContracts) > 0 THEN
      fCreateOrderAction(Order.Orderid,"BundleItem","GPRS","").
END. /* IF pcMobSubBundleType > "" THEN DO: */

/* YTS-2890 */
fMakeCreateEvent((BUFFER Order:HANDLE),"",Syst.Var:katun,"").

add_int(response_toplevel_id, "", liOrderId).

FINALLY:
   END.
