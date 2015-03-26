/**
 * Check if customer exists and return possible customer data.
 *
 * @input   person_id;string;mandatory;
            id_type;string;mandatory;
 *          
 * @output  false;boolean;if customer not exists
            customer;struct;if customer exists
 * @customer id_type;string;optional;
            person_id;string;optional;
            title;string;optional;
            fname;string;optional;
            lname;string;optional;
            company_name;string;optional;
            coname;string;optional;
            street;string;optional;
            zip;string;optional;
     	      city;string;optional;
            language;string;optional;
            nationality;string;optional;
            bankaccount;string;optional;
	         country;string;optional;
            email;string;optional;
            company_id;string;optional;
            company_name;string;optional;
            company_foundationdate
            sms_number;string;optional;
            phone_number;string;
            mark_post;boolean;
            mark_sms;boolean;
            mark_email;boolean;
            mark_post_3rd;boolean;
            mark_sms_3rd;boolean;
            mark_email_3rd;boolean;
            street_code;string;
            city_code;string;
 */

{xmlrpc/xmlrpc_access.i}
DEF VAR gcBrand   AS CHAR NO-UNDO INIT "1".
{fcustdata.i}
{tmsconst.i}

/* Input parameters */
DEF VAR pcPersonId AS CHAR NO-UNDO.
DEF VAR pcIdType AS CHAR NO-UNDO.
DEF VAR plSelfEmployed AS LOG NO-UNDO.

DEF VAR top_struct AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "string,string") EQ ? THEN RETURN.
pcIdType = get_string(param_toplevel_id, "0").
pcPersonId = get_string(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST Customer WHERE
           Customer.Brand EQ gcBrand
       AND Customer.CustIdType EQ pcIdType
       AND Customer.OrgId EQ pcPersonId 
       AND Customer.Roles NE "inactive" 
NO-LOCK NO-ERROR.

IF NOT AVAIL Customer THEN DO:
   add_boolean(response_toplevel_id, "", FALSE).
   RETURN.
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
add_string(top_struct, "country", Customer.country).
add_string(top_struct, "title", Customer.HonTitle).

add_string(top_struct, "sms_number", Customer.smsnumber).
add_string(top_struct, "phone_number", Customer.Phone).
add_boolean(top_struct, 'mark_sms', Customer.DirMarkSMS).
add_boolean(top_struct, 'mark_email', Customer.DirMarkEmail).
add_boolean(top_struct, 'mark_post', Customer.DirMarkPost).
add_boolean(top_struct, 'mark_sms_3rd', Customer.OutMarkSMS).
add_boolean(top_struct, 'mark_email_3rd', Customer.OutMarkEmail).
add_boolean(top_struct, 'mark_post_3rd', Customer.OutMarkPost).
/* orgId can also hold the birthday of a user or payer */

FIND FIRST CustomerReport WHERE
           CustomerReport.Custnum = Customer.Custnum NO-LOCK NO-ERROR.

IF AVAIL CustomerReport THEN DO:
   add_string(top_struct, 'city_code', CustomerReport.CityCode).
   add_string(top_struct, 'street_code', CustomerReport.StreetCode).
END.

add_string(top_struct, "id_type", Customer.CustIdType).

IF Customer.CustIdType = "CIF" THEN DO:
   add_string(top_struct, "company_id", Customer.orgId).
   add_string(top_struct, 'company_name', Customer.CompanyName).
   add_date_or_time(top_struct, 'company_foundationdate', Customer.FoundationDate, 0).
END.   
ELSE DO:
   add_string(top_struct, "person_id", Customer.orgId).
END.

add_string(top_struct, "birthday", STRING(Customer.BirthDay,"99-99-9999")).


