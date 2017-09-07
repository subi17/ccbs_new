/**
 * Add offer criteria
 *
 * @input offer_criteria;struct;mandatory;offer criteria data
 * @offer_criteria offer_id;string;mandatory;
          valid_from;datetime;mandatory;
          valid_to;datetime;optional;
          included_values;string;mandatory;
          excluded_values;string;mandatory;
          criteria_type;string;mandatory;
          username;string;mandatory;
 * @output result;struct;
 * @result id;string;offer criteria id
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Syst/eventval.i}
{Syst/tmsconst.i}
{Mc/offer.i}

DEF VAR pcStruct     AS CHARACTER NO-UNDO. 
DEF VAR pcTenant     AS CHARACTER NO-UNDO.
DEF VAR pcUsername   AS CHARACTER NO-UNDO.
DEF VAR lcStruct     AS CHARACTER NO-UNDO. 
DEF VAR lcRespStruct AS CHARACTER NO-UNDO. 
DEF VAR ocError      AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").

lcstruct = validate_struct(pcStruct, "offer_id!,valid_from!,valid_to,included_values,excluded_values,criteria_type!,username!").

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUsername = "VISTA_" + get_string(pcStruct, "username").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

katun = pcUserName.

{newton/src/settenant.i pcTenant}

CREATE ttOfferCriteria.
ASSIGN 
    ttOfferCriteria.offercriteriaid = NEXT-VALUE(OfferCriteriaSeq)
    ttOfferCriteria.brand           = gcBrand
    ttOfferCriteria.offer           = get_string(pcStruct,"offer_id")
    ttOfferCriteria.criteriatype    = get_string(pcStruct, "criteria_type")    
    ttOfferCriteria.BeginStamp      = get_timestamp(pcStruct,"valid_from")
    ttOfferCriteria.EndStamp        = (IF LOOKUP("valid_to", lcStruct) > 0 THEN get_timestamp(pcStruct,"valid_to") ELSE 20491231.86399)
    ttOfferCriteria.includedvalue   = get_string(pcStruct, "included_values") WHEN LOOKUP("included_values", lcStruct) > 0
    ttOfferCriteria.excludedvalue   = get_string(pcStruct, "excluded_values") WHEN LOOKUP("excluded_values", lcStruct) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF fValidateOfferCriteria(TABLE ttOfferCriteria, TRUE, OUTPUT ocError) > 0 THEN DO:
   RETURN appl_err(ocError).
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER pcUsername 
   {Func/lib/eventlog.i}
   DEF VAR lhOfferCriteria AS HANDLE NO-UNDO.
   lhOfferCriteria = BUFFER OfferCriteria:HANDLE.
   RUN StarEventInitialize(lhOfferCriteria).
END.

CREATE OfferCriteria.
BUFFER-COPY ttOfferCriteria TO OfferCriteria.
VALIDATE OfferCriteria.

IF llDoEvent THEN DO:
   RUN StarEventMakeCreateEvent (lhOfferCriteria).
   fCleanEventObjects().
END.

lcRespStruct = add_struct(response_toplevel_id, "").
add_string(lcRespStruct, "id", STRING(OfferCriteria.OfferCriteriaId)).

RELEASE OfferCriteria.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
