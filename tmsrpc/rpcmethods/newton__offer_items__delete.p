/**
 * Delete offer item.
 *
 * @input  id;string;mandatory;offer item id
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

FIND OfferItem WHERE OfferItem.OfferItemId = liId NO-LOCK NO-ERROR.
IF NOT AVAIL OfferItem THEN 
   RETURN appl_err(SUBST("OfferItem &1 not found", pcId)).

pcUserName = "VISTA_" + get_string(pcStruct, "username").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

{Func/timestamp.i}

IF OfferItem.BeginStamp < fMakeTs() THEN 
   RETURN appl_err("Cannot delete active or history data").

{Syst/commpaa.i}
gcBrand = "1".
katun = pcUserName.
{Syst/eventval.i}

FIND CURRENT OfferItem EXCLUSIVE-LOCK.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER pcUsername 
   {Func/lib/eventlog.i}
   DEF VAR lhOfferItem AS HANDLE NO-UNDO.
   lhOfferItem = BUFFER OfferItem:HANDLE.
   RUN StarEventInitialize(lhOfferItem).
   RUN StarEventMakeDeleteEvent (lhOfferItem).
END.

DELETE OfferItem.

IF llDoEvent THEN fCleanEventObjects().

RELEASE OfferItem.

add_struct(response_toplevel_id, "").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
