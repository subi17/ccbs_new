/**
 * Get scheduled dumps status information 
 *
 * @input dumpid;int;optional; dump id
          
 * @output array of dump structs
   @dumpa  dumps;struct
   @dumps  id;int;dump id
           name;string;name of the dump
           mode;string;full or modified
           day;string;weekday
           timestamp;timestamp;when dump should be run
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
ASSIGN
   gcBrand = "1".
{Syst/eventval.i}
{Func/create_eventlog.i}
{Func/timestamp.i}
{Func/scheduled_dumps.i}


DEF VAR ldeDumpTime AS DECIMAL NO-UNDO. 
DEF VAR liDumpId AS INT NO-UNDO.
DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR resp_array AS CHAR NO-UNDO.
DEF VAR dump_struct AS CHAR NO-UNDO.
DEF VAR lcWeekDays AS CHAR NO-UNDO.
DEF VAR lcDay AS CHAR NO-UNDO.
DEF VAR litime AS INT NO-UNDO.
DEF VAR ldate AS DATE NO-UNDO.
DEF VAR pcTenant AS CHAR NO-UNDO.

lcWeekDays = "Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday".

IF validate_request(param_toplevel_id, "string,struct") = ? THEN RETURN.

pcTenant = get_struct(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").

lcStruct = validate_struct(pcStruct,"dumpid").

/* READ Dump id */
liDumpId = get_int(pcStruct, "dumpid").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

resp_array = add_array(response_toplevel_id, "").

fSplitTS(fmakeTS(),ldate,litime).
/* Search by ID */
fDumpEventList(TODAY + 62,TODAY, liDumpId).
FOR EACH ttEvent NO-LOCK:
   IF ttEvent.EventDate = ldate AND
      ttEvent.EventTime < litime THEN NEXT.
   RUN pAddResultsArray.
END.

/* Struct builder */
PROCEDURE pAddResultsArray:
   dump_struct = add_struct(resp_array, "").
   add_int(dump_struct, "id", ttEvent.DumpID).
   add_string(dump_struct, "name", ttEvent.DumpName).
   add_string(dump_struct,"mode", ttEvent.DumpMode).
   lcDay = ENTRY(ttEvent.EventDay, lcWeekDays).
   add_string(dump_struct,"day", lcDay).
   ldeDumpTime = fMake2Dt(ttEvent.EventDate, ttEvent.EventTime).
   add_timestamp(dump_struct,"timestamp", ldeDumpTime).
   add_string(dump_struct,"duration", ttEvent.AveDuration).
END PROCEDURE. /* PROCEDURE pAddResultsArray: */

FINALLY:
  EMPTY TEMP-TABLE ttDays.
  EMPTY TEMP-TABLE ttTimes.
  EMPTY TEMP-TABLE ttEvent.
  IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
