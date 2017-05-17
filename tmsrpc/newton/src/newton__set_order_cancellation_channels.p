/**
 * Add/modify order cancellation channel for orders by channel
 *
 * @input      order_channel;mandatory;order made by channel
               cancel_channels;mandatory;order can be cancelled by mentioned channel
 * @output     boolean;true
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEF VAR pcOrderChannel   AS CHAR NO-UNDO.
DEF VAR pcCancelChannel  AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id,"string,string") EQ ? THEN RETURN.

pcOrderChannel  = get_string(param_toplevel_id,"0").
pcCancelChannel = get_string(param_toplevel_id,"1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF pcOrderChannel = "" THEN
   RETURN appl_err("Order Channel is missing").

IF pcCancelChannel = "" THEN
   RETURN appl_err("Order cancellation channel is missing").

FIND FIRST TMSCodes WHERE
           TMSCodes.TableName = "OrderCancel" AND
           TMSCodes.FieldName = "Channel" AND
           TMSCodes.CodeValue = pcOrderChannel
     EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF LOCKED TMSCodes THEN
   RETURN appl_err("Order cancellation configuration is locked").

IF NOT AVAIL TMSCodes THEN DO:
   CREATE TMSCodes.
   ASSIGN TMSCodes.TableName = "OrderCancel"
          TMSCodes.FieldName = "Channel"
          TMSCodes.CodeGroup = "Admin"
          TMSCodes.CodeName  = "Cancellation channel for orders per channel"
          TMSCodes.CodeValue = pcOrderChannel
          TMSCodes.InUse     = 1.
END. /* IF NOT AVAIL TMSCodes THEN DO: */

TMSCodes.ConfigValue = pcCancelChannel.

add_boolean(response_toplevel_id,?,TRUE).
