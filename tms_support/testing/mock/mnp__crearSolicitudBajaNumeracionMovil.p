/* RPC to return DSS related information.
 *
 * @input: custnum;int;mandatory;customer number
 * @output array;containing DSS details
 * @struct cli;string;
           cli_type;string;
           data_bundle_limit;double;
           dss_limit;double
*/
{xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR pcstruct            AS CHAR NO-UNDO.
DEF VAR top_array           AS CHAR NO-UNDO.
DEF VAR lcResultStruct      AS CHAR NO-UNDO.
DEF VAR ldePortingTime      AS DEC  NO-UNDO.
DEF VAR lcValue             AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.

REPEAT:
   lcvalue = "005D0" + STRING(RANDOM(1,100000000),"999999999") + 
             STRING(RANDOM(1,100000000),"999999999").
   FIND FIRST MNPProcess WHERE
              MNPProcess.PortRequest = lcvalue NO-LOCK NO-ERROR.
   IF NOT AVAIL MNPProcess THEN LEAVE.
END.

lcResultStruct = add_struct(response_toplevel_id, "").
add_string(lcResultStruct, "codigoReferencia", lcvalue).
add_string(lcResultStruct,"descripcion","La operacion se ha realizado con exito").
add_string(lcResultStruct, "codigoRespuesta", "0000 00000").


