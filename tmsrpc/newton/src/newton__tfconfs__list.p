/** 
 * Return terminal financing fee configuration values 
 *
 * @input    conditions;struct;mandatory
   @conditions  brand;string;mandatory
                rv_percentage;string;mandatory
 * @output struct;array of tfconf ids
*/
{newton/src/flistrpc.i}

DEF VAR pcTenant        AS CHAR NO-UNDO.
DEF VAR lcQuery         AS CHAR NO-UNDO. 
DEF VAR lcRVPercentage  AS CHAR NO-UNDO.
DEF VAR ldeRVPerc       AS DECI NO-UNDO.
DEF VAR llError		    AS LOGI NO-UNDO.
DEF VAR lcPrevNumFormat AS CHAR NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!,rv_percentage!").

ASSIGN lcPrevNumFormat = SESSION:NUMERIC-FORMAT.

SESSION:NUMERIC-FORMAT = "AMERICAN":U.

ASSIGN 
	pcTenant     = get_string(pcStruct,"brand")
	lcRVPercentage = get_string(pcStruct,"rv_percentage").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

ASSIGN ldeRVPerc = DECIMAL(lcRVPercentage) NO-ERROR.

llError = ERROR-STATUS:ERROR.

lcQuery = 'FOR EACH TFConf ' + (IF NOT llError THEN 
									'WHERE TFConf.RVPercentage = ' + QUOTER(ldeRVPerc) + ' AND TFConf.ValidTo >= ' + QUOTER(TODAY) 
								ELSE '') + ' NO-LOCK'.
fListQuery(
   "TFConf",
   lcQuery,
   "TFConfId").

SESSION:NUMERIC-FORMAT = lcPrevNumFormat.