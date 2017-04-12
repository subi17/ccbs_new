/** 
 * Return terminal financing fee configuration values 
 *
 * @input conditions;struct;mandatory;empty
 * @output struct;array of tfconf ids
*/
{newton/src/flistrpc.i}

DEF VAR pcTenant       AS CHAR NO-UNDO.
DEF VAR lcQuery        AS CHAR NO-UNDO. 
DEF VAR lcRVPercentage AS CHAR NO-UNDO.
DEF VAR ldeRVPerc      AS DECI NO-UNDO.
DEF VAR llError		   AS LOGI NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!,rv_percentage").

ASSIGN 
	pcTenant     = get_string(pcStruct,"brand")
	lcPercentage = get_string(pcStruct,"rv_percentage").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

ASSIGN ldeRVPerc = DECIMAL(lcRVPercentage) NO-ERROR.

llError = ERROR-STATUS:ERROR.

lcQuery = 'FOR EACH TFConf ' + (IF NOT llError THEN 'WHERE TFConf.RVPercentage = ' + QUOTER(ldeRVPerc) + ' AND TFConf.ValidTo >= ' + QUOTER(TODAY) ELSE '') + ' NO-LOCK'.

fListQuery(
   "TFConf",
   lcQuery,
   "TFConfId").

