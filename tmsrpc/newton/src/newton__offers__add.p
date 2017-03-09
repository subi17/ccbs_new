/**
 * Add offer 
 *
 * @input   offer;struct;mandatory;offer data
 * @offer   id;string;mandatory;offer id
            description;mandatory;int;mandatory;
            display_item_amounts;boolean;optional;
            valid_from;datetime;mandatory;
            valid_to;datetime;optional;
            amount;double;optional;
            priority;int;optional;
            vat_included;boolean;optional;
            active;boolean;mandatory;
            username;string;mandatory;newton username
 * @output  result;struct;
 * @result  id;string;offer id
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Syst/eventval.i}
{Syst/tmsconst.i}
{Mc/offer.i}

DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR pcTenant AS CHAR NO-UNDO.
DEF VAR pcUsername AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO. 
DEF VAR lcRespStruct AS CHAR NO-UNDO. 
DEF VAR ocError AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcTenant = get_struct(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").

lcstruct = validate_struct(pcStruct, "id!,description!,display_item_amounts,valid_from!,valid_to,amount,priority!,vat_included,active!,username!").

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUsername = "VISTA_" + get_string(pcStruct, "username").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

katun = pcUserName.

{newton/src/settenant.i pcTenant}

CREATE ttOffer.

ttOffer.offer           = get_string(pcStruct,"id").
ttOffer.brand           = gcBrand.
ttOffer.todate = ( IF LOOKUP("valid_to", lcStruct) > 0
                   THEN get_date(pcStruct,"valid_to")
                   ELSE 12/31/2049 ).
ASSIGN
   ttOffer.fromdate        = get_date(pcStruct,"valid_from") 
   ttOffer.vatincl         = ( IF LOOKUP("vat_included", lcStruct) > 0 THEN
                             get_bool(pcStruct, "vat_included") ELSE FALSE )
   ttOffer.priority        = get_int(pcStruct, "priority")
   ttOffer.dispitemamounts = ( IF LOOKUP("display_item_amounts", lcStruct) > 0 
                          THEN int(get_bool(pcStruct, "display_item_amounts"))
                          ELSE 0)
   ttOffer.description     = get_string(pcStruct, "description")
   ttOffer.offeramount     = ( IF LOOKUP("amount", lcStruct) > 0
                               THEN get_double(pcStruct, "amount") ELSE 0)
   ttOffer.active          = get_bool(pcStruct, "active").

IF gi_xmlrpc_error NE 0 THEN DO:
   RETURN.
END.

IF fValidateOffer(TABLE ttOffer, TRUE, OUTPUT ocError) > 0 THEN DO:
   RETURN appl_err(ocError).
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER pcUsername 
   {Func/lib/eventlog.i}
   DEF VAR lhOffer AS HANDLE NO-UNDO.
   lhOffer = BUFFER Offer:HANDLE.
   RUN StarEventInitialize(lhOffer).
END.

CREATE Offer.
BUFFER-COPY ttOffer TO Offer.

IF llDoEvent THEN DO:
   RUN StarEventMakeCreateEvent (lhOffer).
   fCleanEventObjects().
END.

lcRespStruct = add_struct(response_toplevel_id, "").
add_string(lcRespStruct, "id", Offer.Offer).

RELEASE Offer.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
