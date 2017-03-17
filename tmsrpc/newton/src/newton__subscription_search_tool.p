/**
 * Initial search for a subscription.

 * Usually the method will return a list of subscriptions of which the given person is owner. Only if a Custnum or Personid is given of someone, who is no owner then the subscriptions array will be empty and an additional member associated_cli will give the firt of that persons CLI. When search with MSISDN and multiple subscriptions are found, then the searched MSISDN will be the first element in the array returned.
 * @input string|int;mandatory;customer number, person ID, MSISDN
         int;mandatory;limit - how many subscriptions to get at one time
         int;mandatory;offset - how many subscriptions to skip over
         string;mandatory;comma seperated list of allowed search types (msisdn,person_id,custnum)
 * @output_struct subscriptions;array;containing mobsub-structures
                  sub_count;int;mandatory;subscription count
 * @subscription  cust_idtype;string;mandatory;Customer Id Type
                  cust_personid;string;mandatory;Customer Person Id
                  custnum;int;mandatory;Customer Number
                  cli;string;mandatory;MSISDN
                  cli_type;string;mandatory;subscription type
                  data_bundle_id;string;optional;subscription bundle id
                  act_stamp;decimal;mandatory;subscription activation stamp
                  userid;string;mandatory;salesman
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR pcTenant     AS CHAR NO-UNDO.
DEF VAR pcInput      AS CHAR NO-UNDO.
/* Output parameters */
DEF VAR top_struct   AS CHAR NO-UNDO.
DEF VAR result_array AS CHAR NO-UNDO.
DEF VAR sub_struct   AS CHAR NO-UNDO.

/* Local variables */
DEF VAR gcBrand          AS CHAR NO-UNDO INIT "1".
DEF VAR lcTmp            AS CHAR NO-UNDO.
DEF VAR lcCallType       AS CHAR NO-UNDO.
DEF VAR liOwner          AS INT  NO-UNDO.
DEF VAR piOffSet         AS INT  NO-UNDO.
DEF VAR piLimit          AS INT  NO-UNDO.
DEF VAR liSubCount       AS INT  NO-UNDO.
DEF VAR llSearchByMobsub AS LOG  NO-UNDO INIT FALSE.
DEF VAR lii              AS INT  NO-UNDO. 
DEF VAR pcSearchTypes    AS CHAR NO-UNDO. 

lcCallType = validate_request(param_toplevel_id, "string,int|string,int,int,string").
IF lcCallType EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").

IF ENTRY(1,lcCallType) EQ "int" THEN
    liOwner = get_pos_int(param_toplevel_id, "1").
ELSE DO:
    pcInput = get_string(param_toplevel_id, "1").
    liOwner = INT(pcInput) NO-ERROR.
END.

piLimit  = get_pos_int(param_toplevel_id, "2").
piOffSet = get_int(param_toplevel_id, "3").
pcSearchTypes = get_string(param_toplevel_id, "4").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

FUNCTION fAddSubStruct RETURNS LOGICAL:
 
   sub_struct = add_struct(result_array, "").

   add_string(sub_struct, "cust_idtype", Customer.CustIdType).
   add_string(sub_struct, "cust_personid", Customer.OrgId).
   add_int(sub_struct, "custnum", Customer.CustNum).
   add_string(sub_struct, "cli", mobsub.cli).
   add_string(sub_struct, "cli_type", mobsub.clitype).
   add_string(sub_struct, "data_bundle_id", Mobsub.TariffBundle).
   add_string(sub_struct, "userid", Mobsub.Salesman).
   add_timestamp(sub_struct, "act_stamp", Mobsub.ActivationTS).

END FUNCTION. 

lcTmp = CAPS(SUBSTRING(pcInput, 9, 1)).
IF LENGTH(pcInput) EQ 9 AND 
   NOT (ASC(lcTmp) >= 65 AND
   ASC(lcTmp) <= 90) AND
   LOOKUP("msisdn", pcSearchTypes) > 0 THEN DO:

   FIND MobSub NO-LOCK WHERE
        MobSub.CLI = pcInput AND
        MobSub.Brand = gcBrand NO-ERROR.
   IF NOT AVAILABLE MobSub THEN
      RETURN appl_err(SUBST("MobSub entry &1 not found", pcInput)).
   ELSE DO:
      FIND FIRST SIM WHERE
                 SIM.Brand EQ gcBrand   AND
                 SIM.ICC   EQ MobSub.ICC AND
                 SIM.Stock EQ "TESTING" NO-LOCK NO-ERROR.
      IF NOT AVAIL SIM THEN
         RETURN appl_err(SUBST("MobSub entry &1 does not belong to testing tool", pcInput)).

      liOwner = mobsub.agrCust.
   END. /* ELSE DO: */

   llSearchByMobsub = TRUE.
END. 
ELSE IF liOwner NE 0 AND LOOKUP("custnum", pcSearchTypes) > 0 THEN DO:
   FIND Customer NO-LOCK WHERE
        Customer.CustNum = liOwner AND
        Customer.brand = gcBrand NO-ERROR.
   IF NOT AVAILABLE Customer THEN
      RETURN appl_err(SUBST("Customer &1 not found 1", liOwner)).
END.
ELSE IF LOOKUP("person_id", pcSearchTypes) > 0 THEN DO:
   
   FOR EACH Customer NO-LOCK WHERE
            Customer.OrgId = pcInput AND
            Customer.brand = gcBrand AND
            Customer.Roles NE "inactive" 
            lii = 1 TO 2:
      IF lii > 1 THEN DO:
         IF LOOKUP("msisdn",pcSearchTypes) = 0 THEN
            RETURN appl_err("Please search with customer number").
         RETURN appl_err("Please search with MSISDN or customer number").
      END.
   END.
    
   FIND FIRST Customer NO-LOCK WHERE
              Customer.OrgId = pcInput AND
              Customer.brand = gcBrand AND
              Customer.Roles NE "inactive" NO-ERROR.
   IF NOT AVAILABLE Customer THEN
      RETURN appl_err(SUBST("Customer &1 not found 2", pcInput)).
   ELSE
      liOwner = Customer.CustNum.
END.
ELSE liOwner = 0.

IF NOT AVAILABLE Customer AND liOwner > 0 THEN DO:
    FIND Customer NO-LOCK WHERE
         Customer.CustNum = liOwner AND
         Customer.brand = gcBrand NO-ERROR.
END.
IF NOT AVAILABLE Customer THEN
   RETURN appl_err(SUBST("Customer &1 not found 3", liOwner)).

top_struct = add_struct(response_toplevel_id, "").
result_array = add_array(top_struct, "subscriptions").

IF llSearchByMobsub AND piOffSet = 0 THEN DO: 
   fAddSubStruct().
   liSubCount = liSubCount + 1.
END.

IF llSearchByMobsub AND piOffSet > 0 THEN liSubCount = liSubCount + 1.

FOR EACH Mobsub NO-LOCK WHERE
         Mobsub.Brand   = gcBrand AND
         Mobsub.AgrCust = liOwner AND
         Mobsub.CLI <> pcInput,
   FIRST SIM NO-LOCK WHERE
         SIM.Brand EQ gcBrand    AND
         SIM.ICC   EQ Mobsub.ICC AND
         SIM.Stock EQ "TESTING",
   FIRST Customer NO-LOCK WHERE
         Customer.Brand = gcBrand AND
         Customer.CustNum = Mobsub.CustNum:
     
   liSubCount = liSubCount + 1.
   IF liSubCount <= piOffSet THEN NEXT.
   IF liSubCount > (piOffSet + piLimit) THEN NEXT. 
       
   fAddSubStruct().
END.

add_int(top_struct, "sub_count", liSubCount).

IF sub_struct = '' THEN
    RETURN appl_err("No MobSub found").
