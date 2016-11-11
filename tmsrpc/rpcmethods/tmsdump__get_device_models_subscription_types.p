/**
 * Return relationship device_models <-> subscription_types for xfera
 */
{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}

IF NOT get_paramcount(param_toplevel_id) EQ 0 THEN
    RETURN param_err("Unexpected parameters").

DEF VAR lcArray AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.

lcArray = add_array(response_toplevel_id, "").

FOR EACH BillItem
WHERE BillItem.Brand = "1"
  AND BillItem.BiGroup = "7":

    FIND FIRST FMItem
    WHERE FMItem.Brand     = "1"
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
