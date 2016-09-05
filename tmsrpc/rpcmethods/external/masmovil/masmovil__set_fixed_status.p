{commpaa.i}
gcBrand = "1".
{tmsconst.i}
{fexternalapi.i}
{xmlrpc/xmlrpc_access.i}

DEF VAR pcNotificationID AS CHAR NO-UNDO.
DEF VAR pcNotificationTime AS DATETIME NO-UNDO.
DEF VAR pcNotificationType AS CHAR NO-UNDO.
DEF VAR pcNotificationStatus AS CHAR NO-UNDO. /*struct*/


IF validate_request(param_toplevel_id, "string,datetime,string,struct") EQ ? THEN RETURN.
pcNotificationID = get_string(param_toplevel_id,"0").
pcNotificationTime = get_date(param_toplevel_id,"1").
pcNotificationType = get_string(param_toplevel_id,"2").
pcNotificationStatus = get_struct(param_toplevel_id,"3").

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
   












