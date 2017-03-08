/* ----------------------------------------------------------------------
  MODULE .......: NEWTON__GET_MINCONS_STATUSES.P
  TASK .........: Find six latest Actionlogs about mincons.
  APPLICATION ..: TMS
  AUTHOR .......: kariaika
  CREATED ......: 18.06.2015
  CHANGED ......:

  Version ......: Yoigo
  ---------------------------------------------------------------------- */
{xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}

ASSIGN
katun = "cron"
gcbrand = "1".
DEF VAR resp_array AS CHARACTER NO-UNDO.
DEF VAR resp_struct AS CHARACTER NO-UNDO.
DEF VAR file_struct AS CHARACTER NO-UNDO.

DEF VAR liCount AS INTEGER NO-UNDO.

IF NOT get_paramcount(param_toplevel_id) EQ 0 THEN
    RETURN param_err("Unexpected parameters").

resp_array = add_array(response_toplevel_id, "").
liCount = 1.

FOR EACH ActionLog WHERE Actionlog.actionID EQ "MINCONS" AND
                         Actionlog.brand = "1" NO-LOCK.
   resp_struct = add_struct(resp_array,"").
   add_string(resp_struct,"filename",ActionLog.keyValue).
   add_timestamp(resp_struct,"start_processing",ActionLog.ActionTS).
   add_timestamp(resp_struct,"end_processing",ActionLog.ActionDec).
   add_int(resp_struct,"result",ActionLog.ActionStatus).
   add_string(resp_struct,"result_text",ActionLog.ActionChar).

   liCount = liCount + 1.
   IF liCount > 6 THEN LEAVE.
END.

IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
