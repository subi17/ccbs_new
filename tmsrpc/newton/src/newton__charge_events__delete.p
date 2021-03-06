/**
 * Delete charge event.
 *
 * @input  id;string;mandatory; charge event id
           input_struct;struct;
 * @input_struct username;string;user who made the request
 * @output success;boolean; 
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/eventval.i}

DEFINE VARIABLE pcTenant   AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcId       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcUserName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcStruct   AS CHAR      NO-UNDO. 
DEFINE VARIABLE lcListFixedEvents AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcId     = get_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

validate_struct(pcStruct, "username!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUserName = "VISTA_" + get_string(pcStruct, "username").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN 
    RETURN appl_err("username is empty").

Syst.Var:katun = pcUserName.

IF NUM-ENTRIES(pcID,"|") > 1 THEN
   ASSIGN
       pcTenant = ENTRY(2, pcID, "|")
       pcID     = ENTRY(1, pcID, "|").
ELSE
   RETURN appl_err("Invalid tenant information").

{newton/src/settenant.i pcTenant}

FIND FeeModel WHERE 
     FeeModel.Brand = Syst.Var:gcBrand AND
     FeeModel.FeeModel = pcId NO-LOCK NO-ERROR.
IF NOT AVAIL FeeModel THEN DO:
   RETURN appl_err(SUBST("Charge event &1 not found", pcId)).
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun 
   {Func/lib/eventlog.i}
END.

/* delete FMItem */
FIND FIRST FMItem OF FeeModel NO-LOCK NO-ERROR.
IF AVAIL FMItem THEN DO: 

   IF llDoEvent THEN DO:
      DEF VAR lhFMItem AS HANDLE NO-UNDO.
      lhFMItem = BUFFER FMItem:HANDLE.
      RUN StarEventInitialize(lhFMItem).
      RUN StarEventMakeDeleteEvent (lhFMItem).
   END.

  FIND CURRENT FMItem EXCLUSIVE-LOCK NO-ERROR.
  DELETE FMItem. 

  IF llDoEvent THEN fCleanEventObjects().

END.
/* delete FeeModel */
IF llDoEvent THEN DO:
     DEF VAR lhFeeModel AS HANDLE NO-UNDO.
     lhFeeModel = BUFFER FeeModel:HANDLE.
     RUN StarEventInitialize(lhFeeModel).
     RUN StarEventMakeDeleteEvent (lhFeeModel).
END.

FIND CURRENT FeeModel EXCLUSIVE-LOCK NO-ERROR.
DELETE FeeModel.
IF llDoEvent THEN fCleanEventObjects().
RELEASE FeeModel.

add_struct(response_toplevel_id, "").

FINALLY:
   END.
