/**
 * Set offer 
 *
 * @input   id;string;offer id
            offer;struct;mandatory;offer data
 * @offer   description;optional;int;
            display_item_amounts;optional;boolean;
            valid_from;optional;datetime;
            valid_to;optional;datetime;
            amount;optional;double;
            priority;optional;int;
            vat_included;optional;boolean;
            active;optional;boolean;
            username;mandatory;newton username
 * @output  result;struct;contains changed parameter values
 */

{xmlrpc/xmlrpc_access.i}

DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR pcUsername AS CHAR NO-UNDO.
DEF VAR pcId AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO. 
DEF VAR lcRespStruct AS CHAR NO-UNDO. 
DEF VAR ocError AS CHARACTER NO-UNDO. 
DEF VAR llEqual AS LOGICAL NO-UNDO.

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcId     = get_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").
lcstruct = validate_struct(pcStruct, "description,display_item_amounts,valid_from,valid_to,amount,priority,vat_included,active,username!").

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUsername = "VISTA_" + get_string(pcStruct, "username").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

FIND Offer NO-LOCK WHERE
   Offer.Brand = "1" AND
   Offer.Offer = pcId NO-ERROR.
IF NOT AVAIL Offer THEN 
   RETURN appl_err(SUBST("Offer &1 not found", pcId)).

{Syst/commpaa.i}
gcBrand = "1".
katun = pcUserName.
{Syst/eventval.i}
{Syst/tmsconst.i}
{Mc/offer.i}

CREATE ttOffer.
BUFFER-COPY Offer TO ttOffer.

ASSIGN
   ttOffer.fromdate        = get_date(pcStruct,"valid_from") 
                             WHEN LOOKUP("valid_from", lcStruct) > 0 
   
   ttOffer.vatincl         = get_bool(pcStruct, "vat_included")
                             WHEN LOOKUP("vat_included", lcStruct) > 0 
   
   ttOffer.priority        = get_int(pcStruct, "priority")
                             WHEN LOOKUP("priority", lcStruct) > 0 
   
   ttOffer.dispitemamounts = int(get_bool(pcStruct, "display_item_amounts"))
                             WHEN LOOKUP("display_item_amounts", lcStruct) > 0 
   
   ttOffer.description     = get_string(pcStruct, "description")
                             WHEN LOOKUP("description", lcStruct) > 0 
   
   ttOffer.offeramount     = get_double(pcStruct, "amount")
                             WHEN LOOKUP("amount", lcStruct) > 0.
   
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   ttOffer.todate          = get_date(pcStruct,"valid_to")
                             WHEN LOOKUP("valid_to", lcStruct) > 0.

/* allow empty string and convert it to default value */
IF gi_xmlrpc_error = {&INVALID_METHOD_PARAMS} THEN DO:
   gi_xmlrpc_error = 0.
   gc_xmlrpc_error = "".
   IF get_string(pcStruct,"valid_to") EQ "" THEN DO:
      ttOffer.ToDate = 12/31/2049. 
   END.
   ELSE RETURN
      appl_err("Incorrect valid_to value: " + get_string(pcStruct,"valid_to")).
END.

ASSIGN                             
   ttOffer.active          = get_bool(pcStruct,"active")
                             WHEN LOOKUP("active", lcStruct) > 0.

IF gi_xmlrpc_error NE 0 THEN DO:
   RETURN.
END.

IF fValidateOffer(TABLE ttOffer, FALSE, OUTPUT ocError) > 0 THEN DO:
   RETURN appl_err(ocError).
END.

lcRespStruct = add_struct(response_toplevel_id, "").

IF Offer.FromDate <= TODAY THEN DO:
   
   IF ttOffer.ToDate NE Offer.ToDate THEN DO:
      IF Offer.ToDate > TODAY THEN DO:
         IF ttOffer.Todate < TODAY THEN DO:
            ttOffer.Todate = TODAY.
            add_datetime(lcRespStruct,"valid_to",ttOffer.todate).
         END.
      END.
      ELSE DO:
         ttOffer.ToDate = Offer.ToDate.
         add_datetime(lcRespStruct,"valid_to",ttOffer.todate).
      END.
   END.

END.

BUFFER-COMPARE ttOffer TO Offer SAVE llEqual.

IF NOT llEqual THEN DO:

   FIND CURRENT Offer EXCLUSIVE-LOCK.

   IF llDoEvent THEN DO:
      &GLOBAL-DEFINE STAR_EVENT_USER katun 
      {lib/eventlog.i}
      DEF VAR lhOffer AS HANDLE NO-UNDO.
      lhOffer = BUFFER Offer:HANDLE.
      RUN StarEventInitialize(lhOffer).
      RUN StarEventSetOldBuffer(lhOffer).
   END.
   
   BUFFER-COPY ttOffer EXCEPT Brand Offer TO Offer.

   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent(lhOffer).
      fCleanEventObjects().
   END.
END.

RELEASE Offer.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
