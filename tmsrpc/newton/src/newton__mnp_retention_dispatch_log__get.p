/**
 * Get mnp retention dispatch rule changes
 *
 * @input ;empty
 * @output array or struct;mnp retention dispatch rule changes
 * @struct username;string
           event_stamp;datetime
           modified_fields;string
           new_values;string
           old_values;string
           name;string;Platform name
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEF VAR resp_struct AS CHARACTER NO-UNDO. 
DEF VAR resp_array AS CHARACTER NO-UNDO. 
DEF VAR i AS INTEGER NO-UNDO.
DEF VAR lcNewValues AS CHAR NO-UNDO. 
DEF VAR lcOldValues AS CHAR NO-UNDO. 
DEF VAR lcFieldNames AS CHAR NO-UNDO. 
DEF VAR valuecount AS INT NO-UNDO. 
DEF VAR pcTenant   AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

resp_array = add_array(response_toplevel_id, "").

FOR EACH eventlog NO-LOCK WHERE eventlog.tablename = "MNPRetplatform" AND eventlog.action NE "Delete" use-index tablename
    BY eventdate by eventtime:

   valuecount = num-entries(Eventlog.DataValues, CHR(255)) / 3.
   
   DO i = 0 TO valuecount - 1 :
      resp_struct = add_struct(resp_array, "").
      add_string(resp_struct, "username", Eventlog.usercode).
      add_timestamp(resp_struct,"event_stamp", 
         Func.Common:mHMS2TS(EventLog.EventDate, EventLog.EventTime)).
      add_string(resp_struct, "modified_fields", entry(3 * i + 1,Eventlog.DataValues,CHR(255))).
      add_string(resp_struct, "old_values", entry(3 * i + 2,Eventlog.DataValues,CHR(255))).
      add_string(resp_struct, "new_values", entry(3 * i + 3,Eventlog.DataValues,CHR(255))).
      FIND FIRST MNPRetplatform NO-LOCK WHERE
                 MNPRetplatform.RetentionPlatform = EventLog.Key 
      NO-ERROR.
      add_string(resp_struct, "name", 
                   (IF AVAIL MNPRetplatform 
                    THEN MNPRetplatform.Name
                    ELSE "")). 
   END. 

END.
