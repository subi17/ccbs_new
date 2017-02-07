/**
 * Get billing items ids.
 *
 * @input conditions;struct;mandatory; billing_group list string '31,32'
 * @output struct;array of BillItem ids
*/

{newton/src/flistrpc.i}

DEF VAR pcTenant AS CHAR NO-UNDO.
DEF VAR lcQuery  AS CHARACTER NO-UNDO. 

lcStruct = validate_struct(pcStruct, "brand!,billing_group").

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcTenant = get_string(pcStruct,"brand").

add_string(top_struct, "brand", pcTenant).

{newton/src/settenant.i pcTenant}

lcQuery = 'FOR EACH BillItem NO-LOCK WHERE BillItem.Brand = "1" '.

IF LOOKUP("billing_group",lcStruct) > 0 THEN DO:
    
   lcQuery = lcQuery + " AND LOOKUP( BillItem.BIGroup, " + 
                       QUOTER(get_string(pcStruct,"billing_group"))  + " ) > 0" .

   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.

fListQuery(
   "BillItem",
   lcQuery,
   "BillCode").
