/**
 * Delete offer criteria.
 *
 * @input  id;string;mandatory;offer criteria id
           input_struct;struct;
 * @input_struct username;string;user who made the request
 * @output success;boolean; 
 */

{xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcId AS CHARACTER NO-UNDO.
DEFINE VARIABLE liId AS INTEGER NO-UNDO. 
DEFINE VARIABLE pcUserName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcStruct AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcId = get_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

validate_struct(pcStruct, "username!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

liId = INT(pcId) NO-ERROR.

FIND OfferCriteria WHERE OfferCriteria.OfferCriteriaId = liId NO-LOCK NO-ERROR.
IF NOT AVAIL OfferCriteria THEN 
   RETURN appl_err(SUBST("OfferCriteria &1 not found", pcId)).

pcUserName = "VISTA_" + get_string(pcStruct, "username").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{Func/timestamp.i}

IF OfferCriteria.BeginStamp < fMakeTs() THEN 
   RETURN appl_err("Cannot delete active or history data").

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

{Syst/commpaa.i}
gcBrand = "1".
katun = pcUserName.
{Syst/eventval.i}

FIND CURRENT OfferCriteria EXCLUSIVE-LOCK.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER pcUsername 
   {lib/eventlog.i}
   DEF VAR lhOfferCriteria AS HANDLE NO-UNDO.
   lhOfferCriteria = BUFFER OfferCriteria:HANDLE.
   RUN StarEventInitialize(lhOfferCriteria).
   RUN StarEventMakeDeleteEvent (lhOfferCriteria).
END.

DELETE OfferCriteria.

IF llDoEvent THEN fCleanEventObjects().

RELEASE OfferItem.

add_struct(response_toplevel_id, "").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
