/**
 * Return all active subscriptions created by testing tool.

 * @input  int;mandatory;limit - how many subscriptions to get at one time
           int;mandatory;offset - how many subscriptions to skip over
 * @output array with structs 
           cust_idtype;string;mandatory;Customer Id Type
           cust_personid;string;mandatory;Customer Person Id
           custnum;int;mandatory;Customer Number
           cli;string;mandatory;MSISDN
           cli_type;string;mandatory;subscription type
           data_bundle_id;string;optional;subscription bundle id
           act_stamp;decimal;mandatory;subscription activation stamp
           userid;string;mandatory;salesman
 */

{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".

DEF VAR piOffSet         AS INT  NO-UNDO.
DEF VAR piLimit          AS INT  NO-UNDO.
DEF VAR liSubCount       AS INT  NO-UNDO.
DEF VAR result_array     AS CHAR NO-UNDO.
DEF VAR sub_struct       AS CHAR NO-UNDO.
DEF VAR top_struct       AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "int,int") = ? THEN RETURN.

piLimit  = get_int(param_toplevel_id, "0").
piOffSet = get_int(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FUNCTION fAddSubStruct RETURNS LOGICAL:
  
   sub_struct = add_struct(result_array, "").

   add_string(sub_struct, "cust_idtype", Customer.CustIdType).
   add_string(sub_struct, "cust_personid", Customer.OrgId).
   add_int(sub_struct, "custnum", Customer.CustNum).
   add_string(sub_struct, "cli", mobsub.cli).
   add_string(sub_struct, "cli_type", mobsub.clitype).
   add_string(sub_struct, "data_bundle_id", MobSub.TariffBundle).
   add_string(sub_struct, "userid", Mobsub.Salesman).
   add_timestamp(sub_struct, "act_stamp", Mobsub.ActivationTS).

END FUNCTION. 

top_struct = add_struct(response_toplevel_id, "").
result_array = add_array(top_struct, "subscriptions").

FOR EACH SIM NO-LOCK WHERE
         SIM.Brand   EQ gcBrand    AND
         SIM.Stock   EQ "TESTING"  AND
         SIM.SimStat EQ 4,
    EACH IMSI WHERE
         IMSI.ICC = SIM.ICC NO-LOCK,
    EACH MobSub NO-LOCK WHERE
         MobSub.Brand = gcBrand AND
         MobSub.IMSI  = IMSI.IMSI,
   FIRST Customer NO-LOCK WHERE
         Customer.Brand = gcBrand AND
         Customer.CustNum = MobSub.CustNum:

   liSubCount = liSubCount + 1.
   IF liSubCount <= piOffSet THEN NEXT.
   IF liSubCount > (piOffSet + piLimit) THEN NEXT.

   fAddSubStruct().
END.

add_int(top_struct, "sub_count", liSubCount).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
