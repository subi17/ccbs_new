/**
 * Get tmscodes group ids
 *
 * @input conditions;struct;mandatory;empty
 * @output struct;array of offer ids
*/

{xmlrpc/xmlrpc_access.i}

DEF VAR gcBrand AS CHAR NO-UNDO INIT "1".
DEF VAR lcResultStruct AS CHARACTER NO-UNDO. 
IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

lcResultStruct = add_array(response_toplevel_id, "").

add_string(lcResultStruct, "", "criteria_type"). 
add_string(lcResultStruct, "", "duration_unit"). 
add_string(lcResultStruct, "", "item_type"). 
add_string(lcResultStruct, "", "NumberType"). 
add_string(lcResultStruct, "", "OldPayType"). 
add_string(lcResultStruct, "", "OrderChannel"). 
add_string(lcResultStruct, "", "PayType"). 
add_string(lcResultStruct, "", "profession"). 
add_string(lcResultStruct, "", "RenewalSegment").
add_string(lcResultStruct, "", "request_type").
add_string(lcResultStruct, "", "valid_from_rule"). 
add_string(lcResultStruct, "", "TFBank"). 
