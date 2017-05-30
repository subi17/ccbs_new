/**
 * Get billing items ids.
 *
 * @input conditions;struct;mandatory; billing_group list string '31,32'
 * @output struct;array of BillItem ids
*/

{newton/src/flistrpc.i}

lcStruct = validate_struct(pcStruct, "billing_group").

IF gi_xmlrpc_error NE 0 THEN RETURN.

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

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
