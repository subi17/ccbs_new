/**
 * Get subscription type ids.
 *
 * @output struct;array of subscription type ids
*/

{flistrpc.i}

DEF VAR lcQuery AS CHARACTER NO-UNDO.

lcQuery = 'FOR EACH CLIType NO-LOCK WHERE
                    CLIType.Brand   = "1" AND
                    CLIType.WebStatusCode > 0'.

fListQuery("CLIType",lcQuery,"CLIType").
