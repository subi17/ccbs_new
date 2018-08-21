/**
 * Detailed information about a customer (by custnum)
 *
 * @input custnum;int;mandatory;id of customer
          msisdn;string;optional;subscription msisdn (optional, used for renove orders)
 * @output  custnum;int;customer number
	    title;string;
       fname;string;
	    lname;string;
	    lname2;string;
	    coname;string;optional 
       street;string
	    zip;string;
	    city;string;
	    language;string;
	    nationality;string;
	    bankaccount;string
       email;string;
       has_postpaid;boolean;true if owner of at least one postpaid contract
       id_type;string;string
       person_id;string;if company customer then representative id
       company_id;string;only with company customer
       sms_number;string;
       phone_number;string;
       mark_post;boolean;
       mark_sms;boolean;
       mark_email;boolean;
       mark_post_3rd;boolean;
       mark_sms_3rd;boolean;
       mark_email_3rd;boolean;
       mark_dont_share_personal_data;boolean;
       debit_type;int;
       company_name;string;company name if corporate customer
       company_foundationdate;date;Foundation date if corporate customer
       company_contact;struct;contact person data
       delivery_channel;string;
       payment_method;string;
       satisfaction_value;string;
       orders_allowed;boolean; customer doesn't have barred mobsub
       new_subscription_grouping;int;1=Use default invoice group,2=Use new invoice group
       subscription_limit;string;Subscription limit
       subscription_act_limit;string;Subscription activation limit
       invoice_target;string;all_grouped/all_split/customized
       self_employed;boolean; is customer self employed
       profession;string; customer profession
       site_name;string; employer company
       segment;string;customer segment
 * @company_contact title;string;
                    fname;string;
                    lname;string;
                    lname2;string;
                    street;string;
                    zip;string;
                    city;string;
                    region;string;
                    language;string;
                    title;string;
                    nationality;string;
                    email;string;
                    sms_number;string;
                    id_type;string;
                    person_idr;string;
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:katun = "NewtonRPC".
Syst.Var:gcBrand = "1".

{Func/profunc.i}

/* Input parameters */
DEF VAR piCustnum AS INT NO-UNDO.
DEF VAR pcCLI     AS CHAR NO-UNDO.
DEF VAR top_array AS CHAR NO-UNDO.
/* Output parameters */
DEF VAR top_struct AS CHAR NO-UNDO.
DEF VAR contact_struct AS CHAR NO-UNDO.
DEF VAR pending_array AS CHAR NO-UNDO.
/* Local Variables */
DEF VAR llHasPostpaid AS LOGICAL NO-UNDO.
DEF VAR llDefaultLimit AS LOGICAL NO-UNDO. 
DEF VAR liMobsubLimit AS INTEGER NO-UNDO. 
DEF VAR llDefaultActLimit AS LOGICAL NO-UNDO.
DEF VAR liMobsubActLimit AS INTEGER NO-UNDO.
DEF VAR lcDType AS CHAR NO-UNDO.
DEF VAR lcCType AS CHAR NO-UNDO.
DEF VAR llOrdersAllowed AS LOGICAL NO-UNDO INITIAL TRUE.
DEF VAR llSelfEmployed AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR lcInvoiceTarget AS CHAR NO-UNDO. 
DEF VAR liSubCount AS INT NO-UNDO. 
DEF VAR liGroupCount AS INT NO-UNDO. 
DEF VAR liSubLimit AS INTEGER NO-UNDO.
DEF VAR lisubs AS INTEGER NO-UNDO.
DEF VAR liActLimit AS INTEGER NO-UNDO.
DEF VAR liacts AS INTEGER NO-UNDO.

top_array = validate_request(param_toplevel_id, "int,[string]").

IF top_array EQ ? THEN RETURN.
piCustnum = get_pos_int(param_toplevel_id, "0").

IF NUM-ENTRIES(top_array) >= 2 THEN DO:
   pcCLI = get_string(param_toplevel_id, "1").
END.
IF gi_xmlrpc_error NE 0 THEN RETURN.
{newton/src/findtenant.i NO Common Customer CustNum piCustnum}
{Func/fcustdata.i}
{Syst/tmsconst.i}
{Func/barrfunc.i}
{Mc/invoicetarget.i}
{Func/orderchk.i}

IF pcCLI NE "" THEN DO:
   
   FIND FIRST Mobsub WHERE
      Mobsub.Brand = Syst.Var:gcBrand AND
      Mobsub.CLI = pcCLI NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE MobSub THEN DO:
       RETURN appl_err(SUBST("Subscription with msisdn &1 not found", pcCli)).
   END.
   piCustnum = Mobsub.Custnum.
END.

FIND customer NO-LOCK
WHERE customer.brand = Syst.Var:gcBrand AND
   customer.custnum = piCustnum NO-ERROR.

IF NOT AVAILABLE customer THEN DO:
    RETURN appl_err(SUBST("Customer entry for &1 not found", piCustnum)).
END.

top_struct = add_struct(response_toplevel_id, "").

add_int(top_struct, "custnum", Customer.CustNum).
add_string(top_struct, "fname", Customer.Firstname).
add_string(top_struct, "lname", Customer.CustName).
add_string(top_struct, "lname2", Customer.SurName2).
add_string(top_struct, "coname", Customer.coname).
add_string(top_struct, "street", Customer.Address).
add_string(top_struct, "zip", Customer.Zipcode).
add_string(top_struct, "city", Customer.PostOffice).
add_string(top_struct, "region", Customer.Region).
add_string(top_struct, "language", ENTRY(Customer.Language, {&languages})).
add_string(top_struct, "title", Customer.HonTitle).
add_string(top_struct, "bankaccount", Customer.BankAcct).
add_string(top_struct, "nationality", Customer.Nationality).
add_string(top_struct, "email", Customer.Email).
add_string(top_struct, "sms_number", Customer.smsnumber).
add_string(top_struct, "phone_number", Customer.Phone).
add_int(top_struct, "new_subscription_grouping", Customer.InvoiceTargetRule).
/* orgId can also hold the birthday of a user or payer */
IF Customer.CustIdType = "CIF" THEN DO:
   add_string(top_struct, "id_type", Customer.AuthCustIdType).
   add_string(top_struct, "person_id", Customer.AuthCustId).
   add_string(top_struct, "company_id", Customer.orgId).
END.   
ELSE DO:
   add_string(top_struct, "person_id", Customer.orgId).
   add_string(top_struct, "id_type", Customer.CustIdType).
END.

add_int(top_struct, 'debit_type', Customer.DelType).

add_string(top_struct, "birthday", STRING(Customer.BirthDay,"99-99-9999")).

IF Customer.CustIdType = "CIF" THEN DO:
   
   add_string(top_struct, 'company_name', Customer.CompanyName).
   add_date_or_time(top_struct, 'company_foundationdate', Customer.FoundationDate, 0).

   FIND CustContact NO-LOCK WHERE
      CustContact.Brand = Syst.Var:gcBrand AND
      CustContact.CustNum = Customer.Custnum AND
      CustContact.CustType = {&CUSTCONTACT_CONTACT} NO-ERROR.
      
   IF AVAIL CustContact THEN DO:
      contact_struct = add_struct(top_struct, "company_contact").
      add_string(contact_struct, "fname", CustContact.Firstname).
      add_string(contact_struct, "lname", CustContact.CustName).
      add_string(contact_struct, "lname2", CustContact.SurName2).
      add_string(contact_struct, "street", CustContact.Address).
      add_string(contact_struct, "zip", CustContact.Zipcode).
      add_string(contact_struct, "city", CustContact.PostOffice).
      add_string(contact_struct, "region", CustContact.region).
      add_string(contact_struct, "language", ENTRY(INT(CustContact.Language), {&languages})).
      add_string(contact_struct, "title", CustContact.HonTitle).
      add_string(contact_struct, "nationality", CustContact.Nationality).
      add_string(contact_struct, "email", CustContact.Email).
      add_string(contact_struct, "sms_number", CustContact.SMSNumber).
      add_string(contact_struct, 'city_code', CustContact.AddressCodP).
      add_string(contact_struct, 'street_code', CustContact.AddressCodC).
      add_string(contact_struct, 'municipality_code', CustContact.AddressCodM).
      add_string(contact_struct, 'id_type', CustContact.CustIdType).
      add_string(contact_struct, 'person_id', CustContact.OrgId).
   END.


END.

add_boolean(top_struct, 'mark_sms', Customer.DirMarkSMS).
add_boolean(top_struct, 'mark_email', Customer.DirMarkEmail).
add_boolean(top_struct, 'mark_post', Customer.DirMarkPost).
add_boolean(top_struct, 'mark_sms_3rd', Customer.OutMarkSMS).
add_boolean(top_struct, 'mark_email_3rd', Customer.OutMarkEmail).
add_boolean(top_struct, 'mark_post_3rd', Customer.OutMarkPost).
add_boolean(top_struct, 'mark_dont_share_personal_data', Customer.DontSharePersdata).

FIND FIRST CustomerReport WHERE
           CustomerReport.Custnum = Customer.Custnum NO-LOCK NO-ERROR.

IF AVAIL CustomerReport THEN DO:
   add_string(top_struct, 'city_code', CustomerReport.CityCode).
   add_string(top_struct, 'street_code', CustomerReport.StreetCode).
   add_string(top_struct, 'municipality_code', CustomerReport.TownCode).
END.

liMobsubLimit = fGetMobsubLimit(INPUT Customer.Custnum,
             INPUT Customer.Category,
             OUTPUT llDefaultLimit).

IF NOT llDefaultLimit THEN
     add_string(top_struct, 'subscription_limit', STRING(liMobsubLimit)).
ELSE add_string(top_struct, 'subscription_limit', "").

liMobsubActLimit = fGetMobsubActLimit(INPUT Customer.Custnum,
                                      INPUT Customer.Category,
                                      OUTPUT llDefaultActLimit).

IF NOT llDefaultActLimit THEN
     add_string(top_struct,'subscription_act_limit',STRING(liMobsubActLimit)).
ELSE add_string(top_struct,'subscription_act_limit',"").

IF Customer.Custnum EQ Customer.AgrCust THEN DO:
    llHasPostpaid = CAN-FIND(FIRST MobSub
    WHERE MobSub.Brand EQ Customer.Brand
      AND MobSub.AgrCust EQ Customer.CustNum
      AND MobSub.PayType EQ FALSE).

    add_boolean(top_struct, "has_postpaid", llHasPostpaid).
END.


lcDType = Func.Common:mTMSCodeName("Invoice",
                           "DelType",
                           STRING(Customer.DelType)).
add_string(top_struct,"delivery_channel",lcDType).

add_int(top_struct,"payment_method",Customer.ChargeType).

IF fExistBarredSubForCustomer(Customer.CustNum) THEN llOrdersAllowed = FALSE.
add_boolean(top_struct,"orders_allowed",llOrdersAllowed).
/* satisfaction value */
FIND FIRST PIndicator  WHERE
           PIndicator.Brand = Syst.Var:gcBrand AND
           PIndicator.HostTable = "Customer" AND
           PIndicator.KeyValue = STRING(Customer.CustNum) AND
           PIndicator.IndicatorType = {&P_INDICATOR_TYPE_SATISFACTION_VALUE}  
           USE-INDEX HostTable NO-LOCK NO-ERROR. 
IF AVAIL PIndicator THEN 
    add_string(top_struct,"satisfaction_value",PIndicator.IndicatorValue) .

lcInvoiceTarget = fGetCustomerCurrentGrouping(Customer.Custnum, 
                                              output liGroupCount,
                                              output liSubCount).
add_string(top_struct,"invoice_target",lcInvoiceTarget).

/* Check if customer is self employed based on customer category */
IF fIsSelfEmpl(Customer.Category) THEN
   llSelfEmployed = TRUE.
add_boolean(top_struct,"self_employed",llSelfEmployed).
add_string(top_struct, "profession", Customer.Profession).
add_string(top_struct, "site_name", Customer.CompanyName).

FIND FIRST CustCat NO-LOCK WHERE 
           CustCat.brand EQ Syst.Var:gcBrand AND
           CustCat.category EQ Customer.category NO-ERROR.
IF AVAIL custcat THEN           
   add_string(top_struct, "segment", CustCat.Segment).

Func.ValidateACC:mSubscriptionLimitCheck(
   Customer.orgId,
   Customer.custidType,
   llSelfEmployed,
   fispro(Customer.category),
   1,
   OUTPUT liSubLimit,
   OUTPUT lisubs,
   OUTPUT liActLimit,
   OUTPUT liacts).

IF liSubs >= liSubLimit THEN
   add_boolean(top_struct,"subscription_limit_reached",TRUE).
ELSE
   add_boolean(top_struct,"subscription_limit_reached",FALSE).

IF liActs >= liActLimit THEN
   add_boolean(top_struct,"activation_limit_reached",TRUE).
ELSE
   add_boolean(top_struct,"activation_limit_reached",FALSE).

FINALLY:
   END.
