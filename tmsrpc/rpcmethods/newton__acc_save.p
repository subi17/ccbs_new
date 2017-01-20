/**
 * Set address and or marketing data (by custnum).
 * The data will be changed immediately (no request).
 *
 * @input int;mandatory;id of subscription
          string;mandatory;person who requests the change
          datetime;mandatory;when request should be handled
          struct;mandatory;data to be created or changed 
          double;mandatory;charge (0 <= if not given)
          double;mandatory;charge monthly limit used in web
          struct;mandatory;memo
 * @struct  id_type;string;mandatory;
            fname;string;mandatory;
            lname;string;mandatory;
            street;string;mandatory;
            city;string;mandatory;
            region;string;mandatory;
            zip;string;mandatory;
            language;string;mandatory;
            person_id;string;optional;mandatory with normal customer
            company_id;string;optional;mandatory with corporate customer
            nationality;string;mandatory;
            title;string;optional;
            company_name;string;optional;
            coname;string;optional;
            bankaccount;string;optional;
            country;string;optional;
            email;string;optional;
            sms_number;string;optional;
            company_name;string;optional;
            company_foundationdate;datetime;optional;
            sms_number;string;optional;
            phone_number;string;optional;
            mark_post;boolean;optional;
            mark_sms;boolean;optional;
            mark_email;boolean;optional;
            mark_post_3rd;boolean;optional;
            mark_sms_3rd;boolean;optional;
            mark_email_3rd;boolean;optional;
            city_code;string;optional;
            street_code;string;optional;
 * @memo    title;string;mandatory
            content;string;mandatory
            mandate;string;optional

 * @output success;boolean
 */
{xmlrpc/xmlrpc_access.i}
{tmsconst.i}

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.
DEF VAR pcSalesman AS CHAR NO-UNDO.
DEF VAR pdeChgStamp AS DEC NO-UNDO. 
DEF VAR pcstruct AS CHAR NO-UNDO.
DEF VAR pdeCharge AS DECIMAL NO-UNDO. 
DEF VAR pdeChargeLimit AS DECIMAL NO-UNDO. 
DEF VAR ldeLoaded AS DECIMAL NO-UNDO.
DEF VAR pcMemoStruct AS CHAR NO-UNDO. 
DEF VAR pcMemoTitle AS CHAR NO-UNDO. 
DEF VAR pcMemoContent AS CHAR NO-UNDO. 
DEF VAR pcMandateId AS CHAR NO-UNDO. 
DEF VAR pcContractID AS CHAR NO-UNDO.
DEF VAR pcChannel AS CHAR NO-UNDO.

DEFINE VARIABLE lcDataFields AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liRequest AS INTEGER NO-UNDO. 
DEF VAR lcstruct AS CHAR NO-UNDO.
DEF VAR lcError AS CHARACTER NO-UNDO. 
DEF VAR lcCode AS CHARACTER NO-UNDO. 
DEF VAR liSubLimit AS INT NO-UNDO. 
DEF VAR liSubs AS INT NO-UNDO. 
DEF VAR lcDMSInfo AS CHAR NO-UNDO.
DEF VAR liActLimit AS INT NO-UNDO.
DEF VAR liActs AS INT NO-UNDO.
DEF VAR lcReqSource AS CHAR NO-UNDO.

DEF BUFFER bOriginalCustomer FOR Customer.

DEFINE TEMP-TABLE ttCustomer NO-UNDO LIKE Customer
   FIELD cBirthDay AS CHAR
   FIELD CityCode LIKE CustomerReport.CityCode
   FIELD StreetCode LIKE CustomerReport.StreetCode
   FIELD TownCode LIKE CustomerReport.TownCode.

DEF VAR lcAgrCustID AS CHARACTER NO-UNDO.
DEF VAR lcAgrCustIDType AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "int,string,datetime,struct,double,double,struct,string,string,string") EQ ? THEN RETURN.

piMsSeq = get_int(param_toplevel_id, "0").
pcSalesman = get_string(param_toplevel_id, "1").
pdeChgStamp = get_timestamp(param_toplevel_id, "2").
pcstruct = get_struct(param_toplevel_id, "3").
pdeCharge = get_double(param_toplevel_id, "4").
pdeChargeLimit = get_double(param_toplevel_id, "5").
pcMemoStruct = get_struct(param_toplevel_id,"6").
pcMandateId = get_string(param_toplevel_id,"7").
pcChannel = get_string(param_toplevel_id,"8").
pcContractID = get_string(param_toplevel_id,"9").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF validate_struct(pcMemoStruct, "title!,content!") EQ ? THEN RETURN.

pcMemoTitle = get_string(pcMemoStruct,"title").
pcMemoContent = get_string(pcMemoStruct,"content").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcSalesman) EQ "" THEN RETURN appl_err("username is empty").

{findtenant.i NO ordercanal MobSub MsSeq piMsSeq}

FIND FIRST bOriginalCustomer WHERE
           bOriginalCustomer.Custnum = Mobsub.Custnum NO-LOCK NO-ERROR.
IF NOT AVAIL bOriginalCustomer THEN RETURN appl_err("Customer was not found").

lcDataFields = "title,lname!,lname2,fname!,coname,street!,zip!,city!,region!," +
               "language!,nationality!,bankaccount,country," +
               "email,sms_number,person_id,birthday" +
               ",id_type!,company_id,company_name,company_foundationdate," +
               "phone_number,city_code,street_code,municipality_code," +
               "mark_post,mark_sms,mark_email," + 
               "mark_post_3rd,mark_sms_3rd,mark_email_3rd".
lcstruct = validate_request(pcstruct, lcDataFields).

IF gi_xmlrpc_error NE 0 THEN RETURN.

CREATE ttCustomer.

IF LOOKUP("company_id",lcStruct) > 0 THEN DO:
     ttCustomer.OrgId = get_string(pcStruct, "company_id").
     ttCustomer.CustIdType = "CIF".
     lcAgrCustID = get_string(pcStruct, "person_id").
     lcAgrCustIdType = get_string(pcStruct, "id_type").
END.
ELSE DO:
   ttCustomer.OrgId = get_string(pcStruct, "person_id").
   ttCustomer.CustIdType = get_string(pcStruct, "id_type").
END.

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST customer WHERE
           customer.Brand = "1" AND
           customer.OrgId = ttCustomer.OrgId AND
           customer.CustIdType = ttCustomer.CustIdType AND
           customer.roles ne "inactive"
           NO-LOCK NO-ERROR.

IF AVAIL Customer THEN DO:
    
   IF bOriginalCustomer.Custnum EQ Customer.CustNum THEN 
      RETURN appl_err("New agreement customer is the same as current one").

   BUFFER-COPY Customer TO ttCustomer.
   ttCustomer.cBirthDay = STRING(Customer.BirthDay, "99-99-9999").
   FIND FIRST CustomerReport OF Customer NO-LOCK NO-ERROR.
   IF AVAIL CustomerReport THEN
      BUFFER-COPY CustomerReport TO ttCustomer.
END.   
ELSE ttCustomer.Country = "ES".

/* index fields */ 
ttCustomer.custname = get_string(pcStruct, "lname").
ttCustomer.firstname = get_string(pcStruct, "fname").
ttCustomer.zipcode = get_string(pcStruct, "zip").
ASSIGN ttCustomer.SurName2 = get_string(pcStruct, "lname2") WHEN LOOKUP("lname2", lcStruct) > 0.
ASSIGN ttCustomer.CompanyName = get_string(pcStruct,"company_name") WHEN LOOKUP("company_name", lcStruct) > 0.

ASSIGN
   ttCustomer.Address = get_string(pcStruct, "street")
   ttCustomer.region = get_string(pcStruct, "region")
   ttCustomer.postoffice = get_string(pcStruct, "city")
   ttCustomer.language = LOOKUP(get_string(pcStruct, "language"), {&languages})
   ttCustomer.nationality = get_string(pcStruct,"nationality")
   /* optional fields */
   ttCustomer.country = get_string(pcStruct, "country") WHEN LOOKUP("country", lcStruct) > 0
   ttCustomer.smsnumber = get_string(pcStruct, "sms_number") WHEN LOOKUP("sms_number", lcStruct) > 0
   ttCustomer.phone = get_string(pcStruct, "phone_number") WHEN LOOKUP("phone_number", lcStruct) > 0
   ttCustomer.coname = get_string(pcStruct, "coname") WHEN LOOKUP("coname", lcStruct) > 0
   ttCustomer.HonTitle = get_string(pcStruct, "title") WHEN LOOKUP("title", lcStruct) > 0
   ttCustomer.email = get_string(pcStruct, "email") WHEN LOOKUP("email", lcStruct) > 0
   ttCustomer.FoundationDate = get_date(pcStruct,"company_foundationdate") WHEN LOOKUP("company_foundationdate", lcStruct) > 0
   ttCustomer.cbirthday = get_string(pcStruct, "birthday") WHEN LOOKUP("birthday", lcStruct) > 0
   /* marketing fields */
   ttCustomer.DirMarkSMS = get_bool(pcStruct, "mark_sms") WHEN LOOKUP("mark_sms", lcStruct) > 0
   ttCustomer.DirMarkEmail = get_bool(pcStruct, "mark_email") WHEN LOOKUP("mark_email", lcStruct) > 0
   ttCustomer.DirMarkPost = get_bool(pcStruct, "mark_post") WHEN LOOKUP("mark_post", lcStruct) > 0
   ttCustomer.OutMarkSMS = get_bool(pcStruct, "mark_sms_3rd") WHEN LOOKUP("mark_sms_3rd", lcStruct) > 0
   ttCustomer.OutMarkEmail = get_bool(pcStruct, "mark_email_3rd") WHEN LOOKUP("mark_email_3rd", lcStruct) > 0
   ttCustomer.OutMarkPost = get_bool(pcStruct, "mark_post_3rd") WHEN LOOKUP("mark_post_3rd", lcStruct) > 0
   ttCustomer.StreetCode = get_string(pcStruct, "street_code") WHEN LOOKUP("street_code", lcStruct) > 0
   ttCustomer.CityCode = get_string(pcStruct, "city_code") WHEN LOOKUP("city_code", lcStruct) > 0
   ttCustomer.TownCode = get_string(pcStruct, "municipality_code") WHEN LOOKUP("municipality_code", lcStruct) > 0.

IF ttCustomer.cbirthday > "" THEN DO:
   ttCustomer.Birthday = DATE(ttCustomer.cbirthday) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN
      appl_err(SUBST("Incorrect birthday &1", ttCustomer.cbirthday)).
END.

IF Mobsub.PayType = FALSE THEN
   ttCustomer.BankAcct = get_string(pcStruct, "bankaccount").

IF gi_xmlrpc_error NE 0 THEN RETURN.

/* PARSING ENDS */

{commpaa.i}
ASSIGN
   katun = "VISTA_" + pcSalesMan
   gcBrand = "1".
{msagrcustchg.i}
{fcustchangereq.i}
{fcharge_comp_loaded.i}
{orderchk.i}

lcError = fPreCheckSubscriptionForACC(MobSub.MsSeq).
IF lcError > "" THEN RETURN appl_err(lcError).

IF pdeCharge > 0 THEN
   lcError = fCheckChargeLimits (
      Mobsub.CLI,
      Mobsub.PayType,
      pdeCharge,
      pdeChargeLimit).

ASSIGN lcReqSource = (IF pcChannel = "newton" THEN {&REQUEST_SOURCE_NEWTON}
                      ELSE IF pcChannel = "retail_newton" THEN {&REQUEST_SOURCE_RETAIL_NEWTON}
                      ELSE {&REQUEST_SOURCE_MANUAL_TMS}).

IF lcError EQ "" THEN 
   RUN pCheckSubscriptionForACC (
      MobSub.MsSeq,
      0,
      lcReqSource,
      OUTPUT lcError).

IF lcError EQ "" AND AVAIL Customer THEN 
   RUN pCheckTargetCustomerForACC (
      Customer.Custnum,
      OUTPUT lcError).

IF lcError > "" THEN
   RETURN appl_err(lcError).

IF NOT fSubscriptionLimitCheck(INPUT ttCustomer.OrgId,
                               INPUT ttCustomer.CustIdType,
                               INPUT NO,
                               1,
                               OUTPUT lcError,
                               OUTPUT liSubLimit,
                               OUTPUT liSubs,
                               OUTPUT liActLimit,
                               OUTPUT liActs) THEN
   RETURN appl_err("Subscription limit exceeded").

lcCode = fCreateAccDataParam(
          (BUFFER ttCustomer:HANDLE),
          pcSalesMan,
          lcAgrCustIdType,
          lcAgrCustId,
          ttCustomer.StreetCode,
          ttCustomer.CityCode,
          ttCustomer.TownCode,
          pcMandateId,
          OUTPUT lcError).
      
IF lcError > "" THEN
   RETURN appl_err(lcError).


/*empty contract_id if it is not from VFR*/
IF pcChannel NE {&DMS_VFR_REQUEST} THEN
   pcContractId = "".


liRequest = fMSCustChangeRequest(
   MobSub.MsSeq,
   "agrcust",
   ttCustomer.AgrCust,
   MobSub.AgrCust,
   lcCode,
   pdeChgStamp,
   (pdeCharge > 0), /* create fees */
   pdeCharge,
   TRUE,  /* send SMS */
   "",
   lcReqSource,
   0, /* orig. request */
   pcContractId,
   OUTPUT lcError).
  
IF liRequest = 0 THEN
   RETURN appl_err("Request could not be done; " + lcError).

DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                 "customer",
                 STRING(Mobsub.AgrCust),
                 MobSub.AgrCust,
                 pcMemoTitle,
                 pcMemoContent).

add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   EMPTY TEMP-TABLE ttCustomer.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
