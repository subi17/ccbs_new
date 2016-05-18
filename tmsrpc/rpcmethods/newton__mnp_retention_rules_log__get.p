/**
 * Get mnp retention file rules eventlog 
 *
 * @input ;empty
 * @output array or struct;mnp retention rule changes
 * @struct username;string
           event_stamp;datetime
           modified_fields;string
           new_values;string
           old_values;string
           id;string
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Func/timestamp.i}

DEF VAR resp_struct AS CHARACTER NO-UNDO. 
DEF VAR resp_array AS CHARACTER NO-UNDO. 
DEF VAR i AS INTEGER NO-UNDO.
DEF VAR valuecount AS INT NO-UNDO. 

IF validate_request(param_toplevel_id, "") EQ ? THEN RETURN.
IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_array = add_array(response_toplevel_id, "").

FOR EACH eventlog NO-LOCK where
         eventlog.tablename = "MNPRetentionRule" and
         eventlog.action NE "Delete" use-index tablename
   by eventdate by eventtime:

   valuecount = num-entries(Eventlog.DataValues, CHR(255)) / 3.
   
   DO i = 0 TO valuecount - 1 :
      resp_struct = add_struct(resp_array, "").
      add_string(resp_struct, "username", Eventlog.usercode).
      add_timestamp(resp_struct,"event_stamp", 
         fHMS2TS(EventLog.EventDate, EventLog.EventTime)).
      add_string(resp_struct, "modified_fields", entry(3 * i + 1,Eventlog.DataValues,CHR(255))).
      add_string(resp_struct, "old_values", entry(3 * i + 2,Eventlog.DataValues,CHR(255))).
      add_string(resp_struct, "new_values", entry(3 * i + 3,Eventlog.DataValues,CHR(255))).
      add_string(resp_struct, "id", EventLog.Key).
   END. 

END.
