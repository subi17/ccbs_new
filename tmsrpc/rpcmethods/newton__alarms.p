/**
 TMS Alarm get method
 *
 * @input  alarmnumber;string;Optional;Alarm ID
           alarm_status;string;Optional;Alarm status
 * @output alarmstruct;array;a list of alarms struct
 * @alarmstruct  ActionLog;string;
                 AlarmID;int;
                 AlarmText;string;
                 CancelTime;dec;
                 ClearTime;dec;
                 ResetTime;dec;
                 SettingTime;dec;
                 CurrentStatus;int;
                 Parameters;string;
                 ProgramBlock;string;
                 Severity;int;

*/

{xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcalarmnumber  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcalarmstatus  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcCustIdType   AS CHARACTER NO-UNDO.
DEFINE VARIABLE piMaxCount     AS INTEGER   NO-UNDO. 
DEFINE VARIABLE top_array      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError        AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "string,string") EQ ? THEN RETURN.

pcalarmnumber = get_string(param_toplevel_id, "0").
pcalarmstatus = get_string(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

top_array = add_array(response_toplevel_id, "").

FUNCTION fAddAlarmStruct RETURN LOGICAL (INPUT piCountOrder AS INTEGER):
   DEFINE VARIABLE lcAlarmStruct AS CHARACTER NO-UNDO. 
   lcAlarmStruct = add_struct(top_array,"").

   add_string(    lcAlarmStruct, "ActionLog",   Alarm.ActionLog          ).
   add_int(       lcAlarmStruct, "AlarmID",     Alarm.AlarmID          ).
   add_string(    lcAlarmStruct, "AlarmText",   Alarm.AlarmText              ).
   add_timestamp( lcAlarmStruct, "CancelTime",  Alarm.CancelTime).
   add_timestamp( lcAlarmStruct, "ClearTime",   Alarm.ClearTime ).
   add_int(       lcAlarmStruct, "CurrentStatus",  Alarm.CurrentStatus ).
   add_string(    lcAlarmStruct, "Parameters",     Alarm.Parameters ).
   add_string(    lcAlarmStruct, "ProgramBlock", Alarm.ProgramBlock ).
   add_timestamp( lcAlarmStruct, "ResetTime",   Alarm.ResetTime ).
   add_timestamp( lcAlarmStruct, "SettingTime", Alarm.SettingTime ).
   add_int(       lcAlarmStruct, "Severity",    Alarm.Severity ).

END.


FUNCTION fAddAlarmBaseOnStatus RETURN CHARACTER:
   DEFINE VARIABLE iCount   AS INTEGER NO-UNDO. 
   DEFINE VARIABLE pialarmstatus AS INTEGER NO-UNDO. 
   pialarmstatus = INTEGER(pcalarmstatus) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      RETURN "Search string was not integer as alarm status should be".

   iCount = 0.
   AlarmsBasedOnAlarmNumber:
   FOR EACH Alarm WHERE 
      Alarm.CurrentStatus = pialarmstatus 
      NO-LOCK:
      fAddAlarmStruct(iCount).
      iCount = iCount + 1.
      IF iCount = piMaxCount THEN
         LEAVE AlarmsBasedOnAlarmNumber.
   END.
   RETURN "".
END.


FUNCTION fAddAlarmBaseOnNumber RETURN CHARACTER:
   DEFINE VARIABLE iCount   AS INTEGER NO-UNDO. 
   DEFINE VARIABLE pialarmnumber AS INTEGER NO-UNDO. 
   pialarmnumber = INTEGER(pcalarmnumber) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      RETURN "Search string was not integer as alarm number should be".

   iCount = 0.
   AlarmsBasedOnAlarmNumber:
   FOR EACH Alarm WHERE 
      Alarm.AlarmID = pialarmnumber 
      NO-LOCK:
      fAddAlarmStruct(iCount).
      iCount = iCount + 1.
      IF iCount = piMaxCount THEN
         LEAVE AlarmsBasedOnAlarmNumber.
   END.
   RETURN "".
END.

FUNCTION fAddAlarmAll RETURN CHARACTER:
   DEFINE VARIABLE iCount   AS INTEGER NO-UNDO. 

   iCount = 0.
   AlarmsBasedOnAlarmNumber:
   FOR EACH Alarm 
      NO-LOCK:
      fAddAlarmStruct(iCount).
      iCount = iCount + 1.
      IF iCount = piMaxCount THEN
         LEAVE AlarmsBasedOnAlarmNumber.
   END.
   RETURN "".
END.

lcError = "".

IF pcalarmnumber > "" THEN lcError = fAddAlarmBaseOnNumber().
ELSE IF pcalarmstatus > "" THEN lcError = fAddAlarmBaseOnStatus().
ELSE lcError = fAddAlarmAll().

IF lcError NE "" THEN RETURN appl_err(lcError).

