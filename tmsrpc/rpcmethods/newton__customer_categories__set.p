/**
 * List customer categories.
 *
 * @input  ;;
 * @output array of custcat;struct;customer category data
 * @custcat category;string;category ID
            name;string;category name
            limit;int;default subscription limit;optional
            activationlimit;default subscription activation limit;optional
            username;string;user who made the request
 */

{xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Syst/eventval.i}

DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR pcCategory AS CHAR NO-UNDO.
DEF VAR pcUsername AS CHAR NO-UNDO.
DEF VAR piLimit AS INT NO-UNDO.
DEF VAR piActivationLimit AS INT NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO. 
DEF VAR llActLimitUpdate AS LOG NO-UNDO.
DEF VAR llLimitUpdate    AS LOG NO-UNDO.

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcCategory = get_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").
lcstruct = validate_struct(pcStruct,"limit,username!,activationlimit").
pcUsername = "VISTA_" + get_string(pcStruct, "username").
katun = pcUsername.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

IF LOOKUP("limit",lcstruct) GT 0 THEN DO:
   piLimit = get_int(pcStruct,"limit").

   IF piLimit < 0 OR piLimit > 999 THEN 
   RETURN appl_err(SUBST("Limit &1 is not valid", piLimit)).

   llLimitUpdate = TRUE.
END. /* IF LOOKUP("limit", lcstruct) GT 0 THEN DO: */

IF LOOKUP("activationlimit",lcstruct) GT 0 THEN DO:
   piActivationLimit = get_int(pcStruct,"activationlimit").

   IF piActivationLimit < 0 OR piActivationLimit > 999 THEN 
      RETURN appl_err(SUBST("Subscription activation limit &1 is not valid",
                            piActivationLimit)).

   llActLimitUpdate = TRUE.
END. /* IF LOOKUP("activationlimit",lcstruct) GT 0 THEN DO: */

IF NOT llLimitUpdate AND NOT llActLimitUpdate THEN DO:
   add_struct(response_toplevel_id, "").
   RETURN.
END. /* IF NOT llLimitUpdate AND NOT llActLimitUpdate THEN DO: */

FIND CustCat EXCLUSIVE-LOCK WHERE
     CustCat.Brand = gcBrand AND
     CustCat.Category = pcCategory NO-WAIT NO-ERROR.

IF NOT AVAIL CustCat THEN 
   RETURN appl_err(SUBST("Unknown category &1", pcCategory)).

/* Make sure activation limit should not be less than subscription limit */
IF llActLimitUpdate THEN DO:
   IF llLimitUpdate THEN DO:
      IF piActivationLimit < piLimit THEN
         RETURN appl_err(SUBST("Subscription activation limit &1 can not be " +
                         "less than subscription limit", piActivationLimit)).
   END. /* IF llLimitUpdate THEN DO: */
   ELSE IF piActivationLimit < CustCat.MobSubLimit THEN
      RETURN appl_err(SUBST("Subscription activation limit &1 can not be less " +
                            "than subscription limit", piActivationLimit)).
END. /* IF llActLimitUpdate THEN DO: */

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER pcUsername 
   {lib/eventlog.i}
   DEFINE VARIABLE lhCustCat AS HANDLE NO-UNDO.
   lhCustCat = BUFFER CustCat:HANDLE.
   RUN StarEventInitialize(lhCustCat).
   RUN StarEventSetOldBuffer(lhCustCat).
END.

IF llLimitUpdate THEN
   CustCat.MobSubLimit = piLimit.

IF llActLimitUpdate THEN
   CustCat.ActivationLimit = piActivationLimit.
 
IF llDoEvent THEN DO:
   RUN StarEventMakeModifyEvent(lhCustCat).
END.

RELEASE CustCat.

add_struct(response_toplevel_id, "").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   IF llDoEvent THEN fCleanEventObjects().
END.
