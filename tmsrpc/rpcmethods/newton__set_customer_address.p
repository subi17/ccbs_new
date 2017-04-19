/**
 * Set address data (by custnum).
 * The data will be changed through msrequest.
 *
 * @input custnum;int;mandatory;id of customer
          salesman;string;mandatory;person who requests the change
          create_fee;boolean;mandatory;whether the change should be billed
          data;struct;mandatory;data to be changed (rest can be omitted)
 * @struct  person_id;string;optional
            title;string;optional
            fname;string;optional
            lname;string;optional
            coname;string;optional
            street;string;optional
            zip;string;optional
            city;str;optional
            language;string;optional
            nationality;string;optional
            bankaccount;string;optional
            country;string;optional
            email;string;optional
            sms_number;string;optional
            phone_number;string;optional
            mark_post;boolean;optional
            mark_sms;boolean;optional
            mark_email;boolean;optional
            mark_post_3rd;boolean;optional
            mark_sms_3rd;boolean;optional
            mark_email_3rd;boolean;optional
 * @output success;boolean
 */
{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}

{Syst/commpaa.i}
gcBrand = "1".
{Func/timestamp.i}
{Func/fmakemsreq.i}

/* Input parameters */
DEF VAR piCustNum AS INT NO-UNDO.
DEF VAR plCreateFee AS LOGICAL NO-UNDO.
DEF VAR pcSalesman AS CHAR NO-UNDO.
DEF VAR pcstruct AS CHAR NO-UNDO.
DEF VAR pcvalue AS CHAR NO-UNDO.
/* Eventlog parameters */

DEF NEW SHARED VAR scUser AS CHAR NO-UNDO.
scUser = "Newton".
&GLOBAL-DEFINE STAR_EVENT_USER scUser

{Func/lib/eventlog.i}
DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
/* Eventlog definition end */

DEFINE VARIABLE liRegion  AS INTEGER NO-UNDO.
DEFINE VARIABLE liZipCode AS INTEGER NO-UNDO. 
DEFINE VARIABLE liStreetCode AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCityCode AS INTEGER NO-UNDO. 

IF validate_request(param_toplevel_id, "int,string,boolean,struct") EQ ? THEN
   RETURN.

pcstruct = get_struct(param_toplevel_id, "3").
plCreateFee = get_bool(param_toplevel_id, "2").
pcSalesman = get_string(param_toplevel_id, "1").
scUser = "VISTA_" + pcSalesman. /* Read from eventlog functions into eventlog.user */
katun = "VISTA_" + pcSalesman.
piCustNum = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST customer
WHERE customer.custnum = piCustNum
  AND customer.brand = "1" NO-ERROR.
IF NOT AVAILABLE Customer THEN
    RETURN appl_err(SUBST("Customer for &1 not found", piCustNum)).

/* Local variables */
DEF VAR lcstruct AS CHAR NO-UNDO.
DEF VAR lcc AS CHAR NO-UNDO.
DEF VAR lii AS INT NO-UNDO.
DEF VAR liDataPos AS INT NO-UNDO.
DEF VAR llt AS LOGICAL NO-UNDO.
DEF VAR llCustomerChanged AS LOGICAL INITIAL FALSE NO-UNDO.

DEF VAR lcCustomerData AS CHAR EXTENT 9 NO-UNDO.
DEF VAR lcDataFields AS CHAR NO-UNDO.
DEF VAR lcHandleDataFields AS CHAR NO-UNDO.
DEF VAR lcMarketingFields AS CHAR NO-UNDO.
DEF VAR liMsSeq AS INT NO-UNDO.

DEF VAR liReq AS INT NO-UNDO.
DEF VAR ocResult AS CHAR NO-UNDO.

ASSIGN

lcHandleDataFields = "coname,street,zip,city,region," +
               "country,city_code,street_code,municipality_code".

/* must have same interface as newton__set_customer_details */
lcDataFields = "title,lname,lname2,fname,coname,street,zip,city,region," +
               "language,nationality,bankaccount,country," +
               "email,sms_number,phone_number,person_id," +
               "city_code,street_code,municipality_code".
lcMarketingFields = "mark_sms,mark_email,mark_post," +
                    "mark_sms_3rd,mark_email_3rd,mark_post_3rd".

ASSIGN
    lcCustomerData[1] = customer.coname
    lcCustomerData[2] = customer.address
    lcCustomerData[3] = customer.zipcode
    lcCustomerData[4] = customer.postoffice
    lcCustomerData[5] = customer.region
    lcCustomerData[6] = customer.country.

FIND FIRST CustomerReport OF Customer NO-LOCK NO-ERROR.

IF AVAIL CustomerReport THEN ASSIGN
    lcCustomerData[7] = CustomerReport.CityCode
    lcCustomerData[8] = CustomerReport.StreetCode
    lcCustomerData[9] = CustomerReport.TownCode.

lcstruct = validate_request(pcstruct,
        TRIM(lcDataFields + "," + lcMarketingFields)).
IF gi_xmlrpc_error NE 0 THEN RETURN.

DEFINE VARIABLE lcField AS CHARACTER NO-UNDO. 
DO lii = 1 TO NUM-ENTRIES(lcHandleDataFields):
  lcField = ENTRY(lii, lcHandleDataFields).
  IF LOOKUP(lcField, lcStruct) GT 0 THEN DO:
     lcc = get_string(pcstruct, lcField).
     IF lcc NE lcCustomerData[lii] THEN DO:
        lcCustomerData[lii] = lcc.
        llCustomerChanged = TRUE.
     END.
  END.
END.

IF llCustomerChanged THEN DO:
    
   liReq = fAddressRequest(
      piCustnum,
      0,
      lcCustomerData[LOOKUP("street", lcHandleDataFields)],
      lcCustomerData[LOOKUP("zip", lcHandleDataFields)],
      lcCustomerData[LOOKUP("city", lcHandleDataFields)],
      lcCustomerData[LOOKUP("region", lcHandleDataFields)],
      lcCustomerData[LOOKUP("country", lcHandleDataFields)],
      lcCustomerData[LOOKUP("coname", lcHandleDataFields)],
      lcCustomerData[LOOKUP("street_code", lcHandleDataFields)],
      lcCustomerData[LOOKUP("city_code", lcHandleDataFields)],
      lcCustomerData[LOOKUP("municipality_code", lcHandleDataFields)],
      ({&REQUEST_SOURCE_NEWTON}),
      0,
      OUTPUT ocResult).
   
   IF ocResult NE "" THEN 
      RETURN appl_err(ocResult).

END.

add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.


