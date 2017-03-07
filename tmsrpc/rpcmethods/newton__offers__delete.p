/**
 * Delete offer and related offer data.
 *
 * @input  id;string;mandatory;offer item
           input_struct;struct;
 * @input_struct username;string;mandatory;user who made the request
 * @output struct;empty struct 
 */

{xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcId AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcUserName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcStruct AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcId = get_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

validate_struct(pcStruct, "username!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUserName = "VISTA_" + get_string(pcStruct, "username").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

FIND Offer WHERE 
     Offer.Brand = "1" AND
     Offer.Offer = pcId NO-LOCK NO-ERROR.
IF NOT AVAIL Offer THEN 
   RETURN appl_err(SUBST("Offer &1 not found", pcId)).

IF Offer.FromDate <= TODAY THEN 
   RETURN appl_err("Cannot delete active or history data").

{Syst/commpaa.i}
gcBrand = "1".
katun = pcUserName.
{Syst/eventval.i}

DO TRANSACTION:

   FOR EACH OfferItem WHERE
            OfferItem.Brand = gcBrand AND
            OfferItem.Offer = Offer.Offer EXCLUSIVE-LOCK:
      DELETE OfferItem.
   END.
   
   FOR EACH OfferCriteria WHERE
            OfferCriteria.Brand = gcBrand AND
            OfferCriteria.Offer = Offer.Offer EXCLUSIVE-LOCK:
      DELETE OfferCriteria.
   END.

   FIND CURRENT Offer EXCLUSIVE-LOCK.

   IF llDoEvent THEN DO:
      &GLOBAL-DEFINE STAR_EVENT_USER pcUsername 
      {Func/lib/eventlog.i}
      DEF VAR lhOffer AS HANDLE NO-UNDO.
      lhOffer = BUFFER Offer:HANDLE.
      RUN StarEventInitialize(lhOffer).
      RUN StarEventMakeDeleteEvent (lhOffer).
   END.

   DELETE Offer.

END.

IF llDoEvent THEN fCleanEventObjects().

add_struct(response_toplevel_id, "").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
