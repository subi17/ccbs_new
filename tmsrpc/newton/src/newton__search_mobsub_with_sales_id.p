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
{newton/src/json_key.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/cparam2.i}

&SCOPED-DEFINE RESELLERS "PH,TC,TP"
&SCOPED-DEFINE EXCLUSIVERESELLERS "AX,BY,DX,KH,TA,MD"

/* Input parameters */
DEF VAR pcTenant   AS CHAR NO-UNDO.
DEF VAR pcMSISDN   AS CHAR NO-UNDO. 
DEF VAR pcDNIType  AS CHAR NO-UNDO. 
DEF VAR pcDNI      AS CHAR NO-UNDO. 
DEF VAR pcReseller AS CHAR NO-UNDO. 
DEF VAR pcChannel  AS CHAR NO-UNDO. 

/* Output parameters */
DEF VAR top_struct   AS CHAR NO-UNDO.
DEF VAR result_array AS CHAR NO-UNDO.
DEF VAR sub_struct   AS CHAR NO-UNDO.
/* Local variables */
DEF VAR lcCallType         AS CHAR NO-UNDO.
DEF VAR lcIndirectChannels AS CHAR NO-UNDO. 

lcCallType = validate_request(param_toplevel_id, "string,string,string,string,string").
IF lcCallType EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").
pcMSISDN = get_string(param_toplevel_id, "1").
pcDNIType = get_string(param_toplevel_id, "2").
pcDNI = get_string(param_toplevel_id, "3").
pcReseller = get_string(param_toplevel_id, "4"). 

lcIndirectChannels = fCParamC("InDirectChannels").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

FUNCTION fAddSubStruct RETURNS LOGICAL:

   sub_struct = add_json_key_struct(result_array, "").
   add_int(sub_struct   , "seq"        , mobsub.msseq).
   add_string(sub_struct, "description", mobsub.cli).
   add_string(sub_struct, "fixed_number", mobsub.fixednumber).
   add_string(sub_struct, "subscription_type_id", mobsub.clitype).
   add_int(sub_struct   , "status"     , mobsub.msstatus).
   add_string(sub_struct, "data_bundle_id", MobSub.TariffBundle).
    
   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.MsSeq   = MobSub.MsSeq AND
              MsRequest.ReqType = {&REQTYPE_ICC_CHANGE} AND
              LOOKUP(STRING(MsRequest.Reqstatus),"19,20") > 0 NO-ERROR.
   IF AVAIL MsRequest THEN DO:
      add_boolean(sub_struct, "notification", TRUE).
      add_int(sub_struct    , "notification_status", MsRequest.ReqStatus).
   END.

END FUNCTION. 

IF pcMSISDN BEGINS "8" OR
   pcMSISDN BEGINS "9" THEN   /* Fixed line number */
   FIND mobsub NO-LOCK WHERE
        mobsub.brand = Syst.Var:gcBrand AND
        mobsub.fixednumber = pcMSISDN NO-ERROR.
ELSE 
   FIND mobsub NO-LOCK WHERE
        mobsub.brand = Syst.Var:gcBrand AND
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
     Salesman.Brand = Syst.Var:gcBrand AND
     Salesman.Salesman = MobSub.Salesman NO-LOCK NO-ERROR.

/* YDR-149
- Phone house (PH) user can find only subscriptions that has been sold
  by Phone house
- Other user can find all other subscriptions, but not those which are sold
  from Phone house 
- YOT-1851 - added TC, TP to behave like PH 
- YOT-5165 - User Exclusive can find Exclusive subscriptions,PH,TP & TC */

IF LOOKUP(pcReseller,{&EXCLUSIVERESELLERS}) > 0 THEN DO:

   /* YOT-5180, User Exclusive could find ANY subscription created from 
      direct channel without restriction of the reseller that made the activation*/
   FIND FIRST Order NO-LOCK WHERE 
              Order.Brand = Syst.Var:gcBrand      AND 
              Order.MsSeq = MobSub.MsSeq AND 
              Order.CLI   = MobSub.CLI AND
              Order.OrderType NE {&ORDER_TYPE_RENEWAL} AND
              Order.OrderType NE {&ORDER_TYPE_ACC} NO-ERROR. 
   
   IF AVAIL Order AND LOOKUP(Order.OrderChannel,lcIndirectChannels) > 0 AND 
      AVAIL Salesman                                                    THEN
   DO:
      IF LOOKUP(SalesMan.Reseller,{&EXCLUSIVERESELLERS}) EQ 0 AND 
         LOOKUP(SalesMan.Reseller,{&RESELLERS})          EQ 0 THEN 
      RETURN appl_err("Salesman Reseller not match").
   END.

END.
ELSE IF LOOKUP(pcReseller,{&RESELLERS}) > 0 THEN DO:

   IF NOT AVAIL SalesMan THEN 
      RETURN appl_err("Salesman not found").

   IF SalesMan.Reseller NE pcReseller THEN 
      RETURN appl_err("Reseller not match").

END.
ELSE IF AVAIL SalesMan AND
       (LOOKUP(SalesMan.Reseller,{&EXCLUSIVERESELLERS}) > 0  OR
        LOOKUP(SalesMan.Reseller,{&RESELLERS})         > 0) THEN 
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
   END.
