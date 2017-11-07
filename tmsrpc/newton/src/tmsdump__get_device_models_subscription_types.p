/**
 * Return relationship device_models <-> subscription_types for xfera
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}

DEF VAR lcArray  AS CHARACTER NO-UNDO.
DEF VAR lcStruct AS CHARACTER NO-UNDO.
DEF VAR pcTenant AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcArray = add_array(response_toplevel_id, "").

{newton/src/settenant.i pcTenant}

FOR EACH BillItem WHERE BillItem.Brand = "1" AND BillItem.BiGroup = "7":

    FIND FIRST FMItem WHERE FMItem.Brand     = "1" 
      AND FMItem.BillCode  = BillItem.BillCode
      AND FMItem.PriceList = "OrderCh"
      AND FMItem.FromDate <= TODAY
      AND FMItem.ToDate   >= TODAY
      AND FMITem.FeeModel BEGINS "PRE" NO-ERROR.

    IF AVAILABLE FMItem THEN DO:
        lcStruct = add_struct(lcArray, "").
        add_string(lcStruct, "device_model_id", BillItem.BillCode).
        add_string(lcStruct, "subscription_type_id", "prepaid").
        add_double(lcStruct, "price", FMItem.Amount).
    END.

    FIND FIRST FMItem
    WHERE FMItem.Brand     = "1"
      AND FMItem.BillCode  = BillItem.BillCode
      AND FMItem.PriceList = "OrderCh"
      AND FMItem.FromDate <= TODAY
      AND FMItem.ToDate   >= TODAY
      AND FMITem.FeeModel BEGINS "POS" NO-ERROR.

    IF AVAILABLE FMItem THEN DO:
        lcStruct = add_struct(lcArray, "").
        add_string(lcStruct, "device_model_id", BillItem.BillCode).
        add_string(lcStruct, "subscription_type_id", "postpaid").
        add_double(lcStruct, "price", FMItem.Amount).
    END.

END.
