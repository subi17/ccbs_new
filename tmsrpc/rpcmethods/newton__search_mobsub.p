/**
 * YI-020, YBP-499
 * Initial search for a subscription.
 * Usually the method will return a list of subscriptions of which the given person is owner. Only if a Custnum or Personid is given of someone, who is no owner then the subscriptions array will be empty and an additional member associated_cli will give the firt of that persons CLI. When search with MSISDN and multiple subscriptions are found, then the searched MSISDN will be the first element in the array returned.
 * @input string|int;mandatory;customer number, person ID, MSISDN or IMSI
         int;mandatory;limit - how many subscriptions to get at one time
         int;mandatory;offset - how many subscriptions to skip over
         string;mandatory;comma seperated list of allowed search types (msisdn,person_id,imsi,custnum)
         boolean;optional;True(return only subscription_type_id and description)/otherwise full sub struct
 * @output_struct name;string;name of the owner
           custnum;int;customer number of owner
           subscriptions;array;containing mobsub-structures OR
 
 * @subscription seq;int;mandatory;subscription ID
                 description;string;mandatory;MSISDN Number
                 fixed_number;string;optional;Fixed Line Number
                 status;int;mandatory;subscription status
                 subscription_type_id;string;mandatory;subscription type (e.g. CONT2)
                 data_bundle_id;string;mandatory;data bundle id
                 notification;boolean;optional;e.g. pending icc change
 */
{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{rpcmethods/json_key.i}
{Syst/commpaa.i}
gcBrand = "1".
{Syst/tmsconst.i}

/* Input parameters */
DEF VAR pcInput AS CHAR NO-UNDO.
/* Output parameters */
DEF VAR top_struct AS CHAR NO-UNDO.
DEF VAR result_array AS CHAR NO-UNDO.
DEF VAR sub_struct AS CHAR NO-UNDO.
/* Local variables */
DEF VAR lcTmp AS CHAR NO-UNDO.
DEF VAR lcCallType AS CHAR NO-UNDO.
DEF VAR liOwner AS INT NO-UNDO.
DEF VAR piOffSet AS INT NO-UNDO.
DEF VAR piLimit AS INT NO-UNDO.
DEF VAR liSubCount AS INT NO-UNDO.
DEF VAR llPreactivated AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR llSearchByMobsub AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR lii AS INTEGER NO-UNDO. 
DEF VAR pcSearchTypes AS CHARACTER NO-UNDO. 
DEF VAR plFewRecords  AS LOGICAL   NO-UNDO INIT FALSE.

lcCallType = validate_request(param_toplevel_id, "int|string,int,int,string,[boolean]").
IF lcCallType EQ ? THEN RETURN.

IF ENTRY(1,lcCallType) EQ "int" THEN
    liOwner = get_pos_int(param_toplevel_id, "0").
ELSE DO:
    pcInput = get_string(param_toplevel_id, "0").
    liOwner = INT(pcInput) NO-ERROR.
END.

piLimit  = get_pos_int(param_toplevel_id, "1").
piOffSet = get_int(param_toplevel_id, "2").
pcSearchTypes = get_string(param_toplevel_id, "3").

IF NUM-ENTRIES(lcCallType) >= 5 THEN
   plFewRecords = get_bool(param_toplevel_id, "4").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FUNCTION fAddSubStruct RETURNS LOGICAL:

   sub_struct = add_json_key_struct(result_array, "").
   IF NOT plFewRecords THEN DO:
      add_int(sub_struct   , "seq"        , mobsub.msseq).
   END.
   IF Mobsub.fixednumber <> ? THEN 
      add_string(sub_struct, "fixed_number", Mobsub.fixednumber).
   add_int(sub_struct   , "status"     , mobsub.msstatus).
   add_string(sub_struct, "description", mobsub.cli).
   add_string(sub_struct, "subscription_type_id", mobsub.clitype).
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

lcTmp = CAPS(SUBSTRING(pcInput, 9, 1)).
IF LENGTH(pcInput) EQ 9 AND 
   (pcInput BEGINS "6" OR pcInput BEGINS "7") AND
   NOT (ASC(lcTmp) >= 65 AND
   ASC(lcTmp) <= 90) AND
   LOOKUP("msisdn", pcSearchTypes) > 0 THEN DO:

    FIND mobsub NO-LOCK WHERE 
         mobsub.brand = gcBrand AND
         mobsub.cli = pcInput NO-ERROR.
    IF NOT AVAILABLE mobsub THEN
        RETURN appl_err(SUBST("MobSub entry &1 not found", pcInput)).
    ELSE
        liOwner = mobsub.agrCust.
    llSearchByMobsub = TRUE.
END.
/* fixed line number search */
ELSE IF LENGTH(pcInput) EQ 9 AND
  (pcInput BEGINS "8" OR
   pcInput BEGINS "9" ) AND
   NOT (ASC(lcTmp) >= 65 AND
   ASC(lcTmp) <= 90) AND
   LOOKUP("msisdn", pcSearchTypes) > 0 THEN DO:
   
   FIND mobsub NO-LOCK WHERE
        mobsub.brand = gcBrand AND
        mobsub.fixednumber = pcInput NO-ERROR.
    IF NOT AVAILABLE mobsub THEN
        RETURN appl_err(SUBST("MobSub entry &1 not found", pcInput)).
    ELSE
        liOwner = mobsub.agrCust.
    llSearchByMobsub = TRUE.
END. 
ELSE IF LENGTH(pcInput) = 15 AND pcInput BEGINS "21404" AND LOOKUP("imsi", pcSearchTypes) > 0 THEN DO:
   
   FIND FIRST MobSub NO-LOCK WHERE
      MobSub.Brand = gcBrand AND
      MobSub.IMSI  = pcInput NO-ERROR.
   
   IF NOT AVAIL MobSub THEN 
      RETURN appl_err(SUBST("IMSI &1 not found", pcInput)).

   liOwner = mobsub.agrCust.
   pcInput = MobSub.CLI.
   llSearchByMobsub = TRUE.

END.
ELSE IF liOwner NE 0 AND LOOKUP("custnum", pcSearchTypes) > 0 THEN DO:
    FIND Customer NO-LOCK
    WHERE Customer.CustNum = liOwner
      AND Customer.brand = gcBrand NO-ERROR.
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
    
    FIND FIRST Customer NO-LOCK
    WHERE Customer.OrgId = pcInput
      AND Customer.brand = gcBrand
      AND Customer.Roles NE "inactive" NO-ERROR.
    IF NOT AVAILABLE Customer THEN
        RETURN appl_err(SUBST("Customer &1 not found 2", pcInput)).
    ELSE
        liOwner = Customer.CustNum.
END.
ELSE liOwner = 0.

IF NOT AVAILABLE Customer AND liOwner > 0 THEN DO:
    FIND Customer NO-LOCK
    WHERE Customer.CustNum = liOwner
      AND Customer.brand = gcBrand NO-ERROR.
END.
IF NOT AVAILABLE Customer THEN
   RETURN appl_err(SUBST("Customer &1 not found 3", liOwner)).

/* if customer is one of the special types (preactivated subscriptions) return fault */
llPreactivated = Customer.Salesman = "PRE-ACT".
IF NOT llSearchByMobsub AND llPreactivated THEN DO:
   IF LOOKUP("msisdn",pcSearchTypes) = 0 THEN
      RETURN appl_err("Too many subscriptions to show").
   RETURN appl_err("Please search for MSISDN only").
END.

top_struct = add_struct(response_toplevel_id, "").

add_int(top_struct, "custnum", Customer.CustNum).
add_string(top_struct, "name", SUBST("&1 &2 &3", Customer.FirstName,
                                              Customer.CustName,
                                              Customer.Surname2)).

result_array = add_array(top_struct, "subscriptions").

IF llPreactivated THEN DO:
    /* additional message for Newton to display for user */
    add_string(top_struct, "message", "MSIDSN listing is disabled for this customer"). 
    /* 
        search found preactivated customer and search was done with msisdn,
        we return only the found msisdn        
    */
    IF AVAILABLE mobsub THEN DO:
        fAddSubStruct().
        liSubCount = 1.
    END.
END.
ELSE DO:
        
   IF llSearchByMobsub AND piOffSet = 0 THEN DO: 
      fAddSubStruct().
      liSubCount = liSubCount + 1.
   END.

   IF llSearchByMobsub AND piOffSet > 0 THEN liSubCount = liSubCount + 1.

   /* TODO fixednumber search option */
   FOR EACH mobsub NO-LOCK
   WHERE mobsub.brand = gcBrand
     AND mobsub.agrCust = liOwner
     AND Mobsub.CLI <> pcInput
     AND Mobsub.FixedNumber <> pcInput:
     
       liSubCount = liSubCount + 1.
       IF liSubCount <= piOffSet THEN NEXT.
       IF liSubCount > (piOffSet + piLimit) THEN NEXT. 
       
       fAddSubStruct().
   END.

END.

add_int(top_struct, "sub_count", liSubCount).

IF sub_struct = '' THEN
    RETURN appl_err(SUBST("No MobSub for &1 found", Customer.CustNum)).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
