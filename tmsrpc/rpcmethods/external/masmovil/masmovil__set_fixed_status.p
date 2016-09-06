{commpaa.i}
gcBrand = "1".
{tmsconst.i}
{fexternalapi.i}
{xmlrpc/xmlrpc_access.i}

DEF VAR top_struct AS CHAR NO-UNDO.
DEF VAR lcTopStruct AS CHAR NO-UNDO.

DEF VAR lcNotificationID AS CHAR NO-UNDO.
DEF VAR lcNotificationTime AS DATETIME NO-UNDO.
DEF VAR lcNotificationType AS CHAR NO-UNDO.
DEF VAR liOrderId AS INT NO-UNDO.
DEF VAR lcNotificationStatus AS CHAR NO-UNDO. /*struct*/

top_struct = get_struct(param_toplevel_id, "0").

lcTopStruct = validate_struct(top_struct,"notificationID!,notificationTime!,notificationType!,orderID!,Status!").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcNotificationID = get_string(lcTopStruct,"notificationID").
lcNotificationTime = get_date(lcTopStruct,"notificationtime").
lcNotificationType = get_string(lcTopStruct,"notificationType").
liOrderId = get_int(lcTopStruct,"orderID").
lcNotificationStatus = get_struct(lcTopStruct,"Status").

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
   












