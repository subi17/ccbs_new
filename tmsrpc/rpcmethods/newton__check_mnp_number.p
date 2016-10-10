/** Check if MNP number is used already in different order/subscription.
 * @input cli;string;mandatory;
 * @output success;boolean;true or error
 */
{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{tmsconst.i}

/* Input parameters */
DEF VAR pcCli      AS CHAR NO-UNDO.
DEF VAR lcText     AS CHAR NO-UNDO.
DEF VAR top_struct AS CHAR NO-UNDO.
DEF VAR liMsSeq    AS INT  NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcCli = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN top_struct = add_struct(response_toplevel_id, "").

FIND FIRST MobSub NO-LOCK WHERE
           MobSub.Brand EQ "1"   AND
           MobSub.CLI   EQ pcCLI NO-ERROR.
IF AVAILABLE MobSub THEN
   ASSIGN liMsSeq = MobSub.MsSeq
          lcText  = "Subscription exists for this number".

FIND FIRST Order NO-LOCK WHERE
           Order.brand EQ "1"   AND
           Order.CLI   EQ pcCLI NO-ERROR.
IF AVAILABLE Order THEN DO:
   IF lcText = "" THEN
      lcText = "Order exists for this number".

   add_string(top_struct, "orderstatus",Order.StatusCode).
   add_string(top_struct, "Error",lcText).
   add_boolean(top_struct, "Result", FALSE).

   RETURN.
END.

add_boolean(top_struct, "Result", TRUE).
