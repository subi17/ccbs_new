/**
 * Get subscription type ids.
 *
 * @output struct;array of subscription type ids
*/

{newton/src/flistrpc.i}

DEF VAR pcTenant AS CHARACTER NO-UNDO.

lcStruct = validate_struct(pcStruct, "brand!").

pcTenant = get_string(pcStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

DEF VAR lcQuery AS CHARACTER NO-UNDO.

lcQuery = 'FOR EACH CLIType NO-LOCK WHERE
                    CLIType.Brand   = "1" AND
                    CLIType.WebStatusCode > 0'.

fListQuery("CLIType",lcQuery,"CLIType").
