/**
 * Initial search for a subscription (Vista For Retailers)
 * @input string;mandatory;MSISDN
         string;mandatory;DNI type
         string;mandatory;DNI
         string;mandatory;reseller id
 * @output_struct name;string;name of the owner
           custnum;int;customer number of owner
           subscriptions;array;containing subscription structure
 
 * @subscription seq;int;mandatory;subscription ID
                 description;string;mandatory;MSISDN Number
                 status;int;mandatory;subscription status (4=active, 8=barred)
                 subscription_type_id;string;mandatory;subscription type
                 data_bundle_id;string;mandatory;data bundle id
                 notification;boolean;optional;e.g. pending icc change
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{rpcmethods/json_key.i}
{Syst/commpaa.i}
gcBrand = "1".
{Syst/tmsconst.i}
&SCOPED-DEFINE RESELLERS "PH,TC,TP"

/* Input parameters */
DEF VAR pcMSISDN AS CHAR NO-UNDO. 
DEF VAR pcDNIType AS CHAR NO-UNDO. 
DEF VAR pcDNI AS CHAR NO-UNDO. 
DEF VAR pcReseller AS CHAR NO-UNDO. 

/* Output parameters */
DEF VAR top_struct AS CHAR NO-UNDO.
DEF VAR result_array AS CHAR NO-UNDO.
DEF VAR sub_struct AS CHAR NO-UNDO.
/* Local variables */
DEF VAR lcCallType AS CHAR NO-UNDO.

lcCallType = validate_request(param_toplevel_id, "string,string,string,string").
IF lcCallType EQ ? THEN RETURN.

pcMSISDN = get_string(param_toplevel_id, "0").
pcDNIType = get_string(param_toplevel_id, "1").
pcDNI = get_string(param_toplevel_id, "2").
pcReseller = get_string(param_toplevel_id, "3"). 

IF gi_xmlrpc_error NE 0 THEN RETURN.

FUNCTION fAddSubStruct RETURNS LOGICAL:

   sub_struct = add_json_key_struct(result_array, "").
   add_int(sub_struct   , "seq"        , mobsub.msseq).
   add_string(sub_struct, "description", mobsub.cli).
   add_string(sub_struct, "subscription_type_id", mobsub.clitype).
   add_int(sub_struct   , "status"     , mobsub.msstatus).
   add_string(sub_struct, "data_bundle_id", MobSub.TariffBundle).
    
   IF CAN-FIND(
      FIRST MsRequest NO-LOCK WHERE
            MsRequest.MsSeq   = mobsub.msseq AND
            MsRequest.ReqType = {&REQTYPE_ICC_CHANGE} AND
            MsRequest.Reqstatus = {&REQUEST_STATUS_CONFIRMATION_PENDING}) THEN
      add_boolean(sub_struct, "notification", TRUE).

END FUNCTION. 

FIND mobsub NO-LOCK WHERE
     mobsub.brand = gcBrand AND
     mobsub.cli = pcMSISDN NO-ERROR.

IF NOT AVAILABLE mobsub THEN
   RETURN appl_err("Subscription not found").

FIND Customer NO-LOCK WHERE
     Customer.CustNum = MobSub.Custnum NO-ERROR.

IF NOT AVAILABLE Customer THEN
   RETURN appl_err("Customer not found").

IF Customer.OrgId NE pcDNI THEN
   RETURN appl_err("DNI not match").

IF Customer.CustIdType NE pcDNIType THEN
   RETURN appl_err("DNI type not match").

/* there's no salesman record for WEB,MGM,GIFT,YOIGO,VIP */
FIND Salesman WHERE
     Salesman.Brand = gcBrand AND
     Salesman.Salesman = MobSub.Salesman NO-LOCK NO-ERROR.

/* YDR-149
- Phone house (PH) user can find only subscriptions that has been sold
  by Phone house
- Other user can find all other subscriptions, but not those which are sold
  from Phone house 
- YOT-1851 - added TC, TP to behave like PH */
IF LOOKUP(pcReseller,{&RESELLERS}) > 0 THEN DO:
   
   IF NOT AVAIL SalesMan THEN 
      RETURN appl_err("Salesman not found").

   IF SalesMan.Reseller NE pcReseller THEN 
      RETURN appl_err("Reseller not match").
END.
ELSE IF AVAIL SalesMan AND 
        LOOKUP(SalesMan.Reseller,{&RESELLERS}) > 0 THEN 
   RETURN appl_err("Reseller not match").

top_struct = add_struct(response_toplevel_id, "").

add_int(top_struct, "custnum", Customer.CustNum).
add_string(top_struct, "name", SUBST("&1 &2 &3", Customer.FirstName,
                                              Customer.CustName,
                                              Customer.Surname2)).

result_array = add_array(top_struct, "subscriptions").
       
fAddSubStruct().

add_int(top_struct, "sub_count", 1).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
