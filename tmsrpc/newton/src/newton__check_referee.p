/**
 * Check that MGM (Member Gets Member) referee is not same as orderer
 *
 * @input  msisdn;string;mandatory;referee's msisdn     
           person_id;string;mandatory;orderer's person id
           person_id_type;string;mandatory;orderer's person id type
 * @output allow;boolean;referring is allowed
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR pcTenant     AS CHAR NO-UNDO.
DEF VAR pcMsisdn     AS CHAR NO-UNDO.
DEF VAR pcCustIdType AS CHAR NO-UNDO.
DEF VAR pcOrgId      AS CHAR NO-UNDO.
/* Local variable */
DEF VAR gcBrand   AS CHAR NO-UNDO INIT "1".

IF validate_request(param_toplevel_id, "string,string,string,string") EQ ? THEN RETURN.
pcTenant     = get_string(param_toplevel_id, "0").
pcMsisdn     = get_string(param_toplevel_id, "1").
pcCustIdType = get_string(param_toplevel_id, "2").
pcOrgId      = get_string(param_toplevel_id, "3").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

FIND Mobsub WHERE
   MobSub.Brand = gcBrand AND
   MobSub.CLI   = pcMsisdn NO-LOCK NO-ERROR.

IF NOT AVAIL MobSub THEN 
   RETURN appl_err(SUBST("Subscription &1 was not found", pcMsisdn)).

FIND FIRST Customer WHERE 
   Customer.Brand   = gcBrand AND
   Customer.Custnum = MobSub.Custnum  
NO-LOCK NO-ERROR.

IF AVAIL Customer AND Customer.CustIdType = "CIF" THEN DO:
   RETURN appl_err("Corporate customer not allowed").
END.

IF AVAIL Customer AND 
   Customer.CustIdType eq pcCustIdType AND
   Customer.OrgId      eq pcOrgId THEN DO:
   
   add_boolean(response_toplevel_id, "", FALSE).
   RETURN.
END.

add_boolean(response_toplevel_id, "", TRUE).
