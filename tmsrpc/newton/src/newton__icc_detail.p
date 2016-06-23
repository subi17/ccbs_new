/**
 * Return ICC details
 *
 * @input   string;mandatory;ICC
 * @output  struct;mandatory
            sim_type;string;mandatory
            sim_status;int;mandatory
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR pcIcc AS CHAR NO-UNDO.

DEF VAR top_struct AS CHAR NO-UNDO.
DEF VAR lcSimType  AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcIcc = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST SIM WHERE
           SIM.Brand = "1" AND
           SIM.ICC   = pcIcc NO-LOCK NO-ERROR.
IF NOT AVAILABLE SIM THEN
   RETURN appl_err("SIM does not exists").

lcSimType = (IF SIM.SimArt = "Plug_IN" THEN "Regular" ELSE Sim.SimArt).

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "sim_type", lcSimType).
add_int(top_struct, "sim_status", SIM.SimStat).

