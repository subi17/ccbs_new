/** 
 * Return terminal financing fee configuration values 
 *
 * @input conditions;struct;mandatory;empty
 * @output struct;array of tfconf ids
*/
{newton/src/flistrpc.i}

DEF VAR pcTenant AS CHARACTER NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!").

pcTenant = get_string(pcStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH TFConf NO-LOCK'.

fListQuery(
   "TFConf",
   lcQuery,
   "TFConfId").

