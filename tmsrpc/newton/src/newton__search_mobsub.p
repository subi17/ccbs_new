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
           discount_type;string;Discount information 

 * @subscription seq;int;mandatory;subscription ID
                 description;string;mandatory;MSISDN Number
                 fixed_number;string;optional;Fixed Line Number
                 status;int;mandatory;subscription status
                 subscription_type_id;string;mandatory;subscription type (e.g. CONT2)
                 data_bundle_id;string;mandatory;data bundle id
                 notification;boolean;optional;e.g. pending icc change
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{newton/src/json_key.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/cparam2.i}

/* Input parameters */
DEF VAR pcInput AS CHAR NO-UNDO.
/* Output parameters */
DEF VAR top_struct   AS CHAR NO-UNDO.
DEF VAR result_array AS CHAR NO-UNDO.
DEF VAR sub_struct   AS CHAR NO-UNDO.
/* Local variables */
DEF VAR lcTmp             AS CHAR    NO-UNDO.
DEF VAR lcCallType        AS CHAR    NO-UNDO.
DEF VAR liOwner           AS INT     NO-UNDO.
DEF VAR pcTenant AS CHAR NO-UNDO.
DEF VAR piOffSet          AS INT     NO-UNDO.
DEF VAR piLimit           AS INT     NO-UNDO.
DEF VAR liSubCount        AS INT     NO-UNDO.
DEF VAR llPreactivated    AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR llSearchByMobsub  AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR lii               AS INTEGER NO-UNDO. 
DEF VAR pcSearchTypes     AS CHAR    NO-UNDO. 
DEF VAR plFewRecords      AS LOGICAL NO-UNDO INIT FALSE.
DEF VAR lcSubDiscountType AS CHAR    NO-UNDO.

lcCallType = validate_request(param_toplevel_id, "string,int|string,int,int,string,[boolean]").
IF lcCallType EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").

IF ENTRY(2,lcCallType) EQ "int" THEN
    liOwner = get_pos_int(param_toplevel_id, "1").
ELSE 
DO:
    pcInput = get_string(param_toplevel_id, "1").
    liOwner = INT(pcInput) NO-ERROR.
END.

piLimit  = get_pos_int(param_toplevel_id, "2").
piOffSet = get_int(param_toplevel_id, "3").
pcSearchTypes = get_string(param_toplevel_id, "4").

IF NUM-ENTRIES(lcCallType) >= 6 THEN
   plFewRecords = get_bool(param_toplevel_id, "5").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

FUNCTION fGetDiscountType RETURNS CHARACTER
   (INPUT iiMsSeq   AS INT,
    INPUT icCLIType AS CHAR):

   DEF VAR lcExtraLineCLITypes  AS CHAR NO-UNDO.
   DEF VAR lcExtraLineDiscounts AS CHAR NO-UNDO.
   DEF VAR lcDiscountType       AS CHAR NO-UNDO INITIAL "".

   ASSIGN
      lcExtraLineCLITypes  = fCParam("DiscountType","ExtraLine_CLITypes")
      lcExtraLineDiscounts = fCParam("DiscountType","ExtraLine_Discounts").

   /* Get available discount info for subscriptions     */
   /* Only one additionaline/extraline discount will be
      available for mobile only tariff                  */
   FOR FIRST DPMember NO-LOCK WHERE
             DPMember.HostTable  = "MobSub"        AND
             DPMember.KeyValue   = STRING(iiMsSeq) AND
             DPMember.ValidFrom <= TODAY           AND
             DPMember.ValidTo   >= TODAY           AND
             DPMember.ValidTo   >= DPMember.ValidFrom,
       FIRST DiscountPlan NO-LOCK WHERE
             DiscountPlan.DPId    = DPMember.DPId AND
             DiscountPlan.Subject = "Contract Target"
       BY DPMember.ValidTo DESC:

       IF LOOKUP(icCLIType,{&ADDLINE_CLITYPES}) > 0 THEN DO:

          IF LOOKUP(DiscountPlan.DPRuleId,{&ADDLINE_DISCOUNTS_20}) > 0 THEN
             lcDiscountType = "additional_20".
          ELSE IF (LOOKUP(DiscountPlan.DPRuleId,{&ADDLINE_DISCOUNTS})    > 0  OR
                   LOOKUP(DiscountPlan.DPRuleId,{&ADDLINE_DISCOUNTS_HM}) > 0) THEN
             lcDiscountType = "additional_50".

       END.
       ELSE IF LOOKUP(icCLIType,lcExtraLineCLITypes) > 0 THEN DO:

          IF LOOKUP(DiscountPlan.DPRuleId,lcExtraLineDiscounts) > 0 THEN
             lcDiscountType = "extra_100".
          ELSE
             lcDiscountType = "extra_0".

       END.

   END.

   RETURN lcDiscountType.

END FUNCTION.

FUNCTION fAddSubStruct RETURNS LOGICAL:

   sub_struct = add_json_key_struct(result_array, "").
   add_string(sub_struct, "brand", fConvertTenantToBrand(BUFFER-TENANT-NAME(MobSub))).
   IF NOT plFewRecords THEN DO:
      add_int(sub_struct   , "seq"        , mobsub.msseq).
   END.
   IF Mobsub.fixednumber <> ? THEN 
      add_string(sub_struct, "fixed_number", Mobsub.fixednumber).
   add_int(sub_struct   , "status"     , mobsub.msstatus).
   add_string(sub_struct, "description", mobsub.cli).
   add_string(sub_struct, "subscription_type_id", mobsub.clitype).
   add_string(sub_struct, "data_bundle_id", MobSub.TariffBundle).

   lcSubDiscountType = fGetDiscountType(MobSub.MsSeq,
                                        MobSub.CLIType).
   add_string(sub_struct, "discount_type", lcSubDiscountType).

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
         mobsub.brand = Syst.Var:gcBrand AND
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
        mobsub.brand = Syst.Var:gcBrand AND
        mobsub.fixednumber = pcInput NO-ERROR.
    IF NOT AVAILABLE mobsub THEN
        RETURN appl_err(SUBST("MobSub entry &1 not found", pcInput)).
    ELSE
        liOwner = mobsub.agrCust.
    llSearchByMobsub = TRUE.
END. 
ELSE IF LENGTH(pcInput) = 15 AND pcInput BEGINS "21404" AND LOOKUP("imsi", pcSearchTypes) > 0 THEN DO:
   
   FIND FIRST MobSub NO-LOCK WHERE
      MobSub.Brand = Syst.Var:gcBrand AND
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
      AND Customer.brand = Syst.Var:gcBrand NO-ERROR.
    IF NOT AVAILABLE Customer THEN
        RETURN appl_err(SUBST("Customer &1 not found 1", liOwner)).
END.
ELSE IF LOOKUP("person_id", pcSearchTypes) > 0 THEN DO:
   
   FOR EACH Customer NO-LOCK WHERE
            Customer.OrgId = pcInput AND
            Customer.brand = Syst.Var:gcBrand AND
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
      AND Customer.brand = Syst.Var:gcBrand
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
      AND Customer.brand = Syst.Var:gcBrand NO-ERROR.
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

   /* Add same customers other subscriptions */
   FOR EACH mobsub NO-LOCK
   WHERE mobsub.brand = Syst.Var:gcBrand
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
   END.
