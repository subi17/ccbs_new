/**
 * Information about subscription and customer (by msisdn)
 *
 * @input msisdn;str;mandatory;subscription msisdn
 * @output result;struct;result struct

 * @result custnum;int;customer number
      msseq;int;subscription number
      msisdn;str;
      subscription_type;str;
      paytype;boolean;
      zipcode;str;
      region;str;
      firstname;str;
      lastname;str;
      lastname2;str;
      custid;str;customer id
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEF VAR gcBrand AS CHARACTER INIT "1".

/* Input parameters */
DEF VAR pcCLI     AS CHAR NO-UNDO.
/* Output parameters */
DEF VAR top_struct AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

pcCLI    = get_string(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{selfservice/src/findtenant.i NO ordercanal MobSub Cli pcCLI "SpecialMobSubError"}

FIND FIRST customer NO-LOCK WHERE
           customer.custnum = MobSub.Custnum NO-ERROR.

IF NOT AVAILABLE customer THEN
   RETURN appl_err(SUBST("Customer with id &1 was not found", MobSub.Custnum)).

top_struct = add_struct(response_toplevel_id, "").

add_int(top_struct, "msseq", Mobsub.MsSeq).
add_string(top_struct, "msisdn", Mobsub.CLI).
add_string(top_struct, "subscription_type", MobSub.CLIType).
add_boolean(top_struct, "paytype", MobSub.PayType).
add_int(top_struct, "custnum", Customer.Custnum).
add_string(top_struct, "zipcode", Customer.ZipCode).
add_string(top_struct, "region", Customer.Region).
add_string(top_struct, "firstname", Customer.FirstName).
add_string(top_struct, "lastname", Customer.CustName).
add_string(top_struct, "lastname2", Customer.Surname2).
add_string(top_struct, "custid", Customer.OrgId).
