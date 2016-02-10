/**
 * Delete charge event.
 *
 * @input  id;string;mandatory; charge event id
           input_struct;struct;
 * @input_struct username;string;user who made the request
 * @output success;boolean; 
 */

{xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcId AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcUserName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcStruct AS CHAR NO-UNDO. 
DEFINE VARIABLE lcListFixedEvents AS CHAR NO-UNDO.

/*lcListFixedEvents = "STC_POSTPAID,STC_PREPAID," + 
                    "ACC_POSTPAID,ACC_PREPAID,"
*/

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcId = get_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

validate_struct(pcStruct, "username!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUserName = "VISTA_" + get_string(pcStruct, "username").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

{Syst/commpaa.i}
gcBrand = "1".
katun = pcUserName.
{Syst/eventval.i}

FIND FeeModel WHERE 
     FeeModel.Brand = gcBrand AND
     FeeModel.FeeModel = pcId NO-LOCK NO-ERROR.
IF NOT AVAIL FeeModel THEN DO:
   RETURN appl_err(SUBST("Charge event &1 not found", pcId)).
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun 
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
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
