/** 
 * Return terminal financing fee configuration values 
 *
 * @input conditions;struct;mandatory;empty
 * @output struct;array of tfconf ids
*/
{rpcmethods/flistrpc.i}

lcStruct = validate_struct(pcStruct, "").

IF gi_xmlrpc_error NE 0 THEN RETURN.

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH TFConf NO-LOCK'.

fListQuery(
   "TFConf",
   lcQuery,
   "TFConfId").

