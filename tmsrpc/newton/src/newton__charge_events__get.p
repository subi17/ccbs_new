/**
 * Get charge events.
 *
 * @input ids;array of string;mandatory; charge events ids
 * @output charge_event;array of struct; charge event data
 * @charge_event id;string; FeeModel id
                 name;string; 
                 amount;double;
                 billing_item_id;string;
                 paytype; string; postpaid or prepaid 
                 valid_from;date;
                 valid_to;date;
          
*/
{newton/src/header_get.i}
{Func/multitenantfunc.i}

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   IF NUM-ENTRIES(pcID,"|") > 1 THEN
       ASSIGN
           pcTenant = ENTRY(2, pcID, "|")
           pcID     = ENTRY(1, pcID, "|").
   ELSE
       RETURN appl_err("Invalid tenant information").

   {newton/src/settenant.i pcTenant}
       
   FIND FeeModel NO-LOCK WHERE 
        FeeModel.Brand = Syst.Var:gcBrand AND 
        FeeModel.FeeModel = pcId NO-ERROR.

   IF NOT AVAIL FeeModel THEN RETURN appl_err("Charge event not found: "+ pcId).
   
   FIND FIRST FMItem OF FeeModel WHERE 
              FMItem.ToDate >= TODAY NO-LOCK NO-ERROR.

   IF NOT AVAIL FMItem THEN RETURN appl_err("Charge event " + pcId + " doesn't contain active billing item ").
  
   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "id"   , FeeModel.FeeModel + "|" + fConvertTenantToBrand(pcTenant)). 
   add_string(lcResultStruct, "brand", fConvertTenantToBrand(pcTenant)).
   add_string(lcResultStruct, "name" , FeeModel.FeeName).

   add_double(lcResultStruct, "amount", FMItem.Amount).
   add_string(lcResultStruct, "billing_item_id", FMItem.BillCode).
   FIND TMSParam WHERE 
        TMSParam.Brand = "1" AND
        TMSParam.ParamGroup = "CCAdminTool" AND
        TMSParam.CharVal = FMItem.PriceList NO-LOCK NO-ERROR. 

   IF NOT AVAIL TMSParam THEN RETURN appl_err("Missing system parameter").

   add_string(lcResultStruct,"paytype",LOWER(SUBSTRING(TMSParam.ParamCode,16,-1,"CHARACTER"))).
   add_datetime(lcResultStruct, "valid_from", FMItem.FromDate).

   IF FMItem.ToDate < 1/19/2038 THEN 
   add_datetime(lcResultStruct,"valid_to",FMItem.ToDate).
 

 END.
