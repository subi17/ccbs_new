/**
 * Set offer criteria
 *
 * @input id;string;offer id
          offer_criteria;struct;mandatory;offer criteria data
 * @offer_criteria offer_id;string;optional;
          valid_from;datetime;optional;
          valid_to;datetime;optional;
          included_values;string;optional;
          excluded_values;string;optional;
          criteria_type;string;optional;
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

DEF VAR pcStruct     AS CHAR      NO-UNDO. 
DEF VAR pcTenant     AS CHAR      NO-UNDO.
DEF VAR pcUsername   AS CHAR      NO-UNDO.
DEF VAR pcId         AS CHAR      NO-UNDO.
DEF VAR piId         AS INT       NO-UNDO.
DEF VAR lcStruct     AS CHAR      NO-UNDO. 
DEF VAR lcRespStruct AS CHAR      NO-UNDO. 
DEF VAR ocError      AS CHARACTER NO-UNDO. 
DEF VAR i            AS INTEGER   NO-UNDO. 
DEF VAR llEqual      AS LOG       NO-UNDO.
DEF VAR ldeNowTS     AS DECIMAL   NO-UNDO. 

IF validate_request(param_toplevel_id, "string,string,struct") EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").
pcId     = get_string(param_toplevel_id, "1").
pcStruct = get_struct(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcstruct = validate_struct(pcStruct, "offer_id,valid_from,valid_to,included_values,excluded_values,criteria_type,username!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUsername = "VISTA_" + get_string(pcStruct, "username").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN 
    RETURN appl_err("username is empty").

katun = pcUserName.

piId = INTEGER(pcId) NO-ERROR.
IF ERROR-STATUS:ERROR THEN 
   RETURN appl_err(SUBST("OfferCriteriaId &1 is not numerical", pcId)).

{newton/src/settenant.i pcTenant}

FIND OfferCriteria NO-LOCK WHERE OfferCriteria.OfferCriteriaID = piId NO-ERROR.
IF NOT AVAIL OfferCriteria THEN 
   RETURN appl_err(SUBST("Offer criteria &1 not found", piId)).

CREATE ttOfferCriteria.
BUFFER-COPY OfferCriteria TO ttOfferCriteria.
           
ASSIGN
   ttOfferCriteria.BeginStamp      = get_timestamp(pcStruct,"valid_from")
     WHEN LOOKUP("valid_from", lcStruct) > 0.

ASSIGN
   ttOfferCriteria.CriteriaType     = get_string(pcStruct,"criteria_type")
     WHEN LOOKUP("criteria_type", lcStruct) > 0.
   
ASSIGN   
   ttOfferCriteria.includedvalue   = get_string(pcStruct, "included_values")
      WHEN LOOKUP("included_values", lcStruct) > 0 
   ttOfferCriteria.excludedvalue   = get_string(pcStruct, "excluded_values")
      WHEN LOOKUP("excluded_values", lcStruct) > 0.
   
IF gi_xmlrpc_error NE 0 THEN DO:
   RETURN.
END.
   
ASSIGN   
   ttOfferCriteria.EndStamp = get_timestamp(pcStruct,"valid_to")
      WHEN LOOKUP("valid_to", lcStruct) > 0.

/* allow empty string and convert it to default value */
IF gi_xmlrpc_error = {&INVALID_METHOD_PARAMS} THEN DO:
   gi_xmlrpc_error = 0.
   gc_xmlrpc_error = "".
   IF get_string(pcStruct,"valid_to") EQ "" THEN DO:
      ttOfferCriteria.EndStamp = 20491231.86399. 
   END.
   ELSE DO:
      RETURN appl_err("Incorrect valid_to value: " + 
             get_string(pcStruct,"valid_to")).
   END.
END.

IF fValidateOfferCriteria(TABLE ttOfferCriteria, FALSE, OUTPUT ocError) > 0 THEN DO:
   RETURN appl_err(ocError).
END.

ldeNowTS = fMakeTS().

lcRespStruct = add_struct(response_toplevel_id, "").


IF OfferCriteria.BeginStamp <= ldeNowTS THEN DO:
   
   IF ttOfferCriteria.EndStamp NE OfferCriteria.EndStamp THEN DO:
      IF OfferCriteria.EndStamp > ldeNowTS THEN DO:
         IF ttOfferCriteria.EndStamp < ldeNowTS THEN DO:
            ttOfferCriteria.EndStamp = ldeNowTS.
            add_timestamp(lcRespStruct,"valid_to",ttOfferCriteria.EndStamp).
         END.
      END.
      ELSE DO:
         ttOfferCriteria.EndStamp = OfferCriteria.EndStamp.
         add_timestamp(lcRespStruct,"valid_to",ttOfferCriteria.EndStamp).
      END.
   END.

END.

BUFFER-COMPARE ttOfferCriteria TO OfferCriteria SAVE llEqual.

IF NOT llEqual THEN DO:

   FIND CURRENT OfferCriteria EXCLUSIVE-LOCK.

   IF llDoEvent THEN DO:
      &GLOBAL-DEFINE STAR_EVENT_USER katun 
      {Func/lib/eventlog.i}
      DEF VAR lhOfferCriteria AS HANDLE NO-UNDO.
      lhOfferCriteria = BUFFER OfferCriteria:HANDLE.
      RUN StarEventInitialize(lhOfferCriteria).
      RUN StarEventSetOldBuffer(lhOfferCriteria).
   END.
   
   BUFFER-COPY ttOfferCriteria EXCEPT Brand Offer OfferCriteriaId TO OfferCriteria.

   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent(lhOfferCriteria).
      fCleanEventObjects().
   END.
END.


RELEASE OfferCriteria.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
