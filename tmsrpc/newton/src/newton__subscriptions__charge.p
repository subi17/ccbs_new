
/**
 * create charge events for mobsub.
 *
 * @input  struct;
 * @struct msseq;
           charge_event;
           amount;
           username;
           limit;

 * @output requestid;
*/   


{fcgi_agent/xmlrpc/xmlrpc_access.i}


DEF VAR lcstruct AS CHAR NO-UNDO. 
DEF VAR liReqId AS INTEGER NO-UNDO.
DEF VAR liMsSeq AS INTEGER NO-UNDO.
DEF VAR lcEventId AS CHAR NO-UNDO.
DEF VAR ldAmount AS DECIMAL NO-UNDO.
DEF VAR lcUserId AS CHAR NO-UNDO.
DEF VAR ldChargeLimit AS DECIMAL NO-UNDO.
DEF VAR ldeLoaded AS DECIMAL NO-UNDO.
DEF VAR ldeCurrBal AS DECIMAL NO-UNDO.

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.
lcstruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* get parameters */

liMsSeq = get_int(lcstruct,"msseq").
lcEventId = get_string(lcstruct,"charge_event_id").
ldAmount = get_double(lcstruct,"amount").
lcUserId = "VISTA_" + get_string(lcstruct,"username").
ldChargeLimit = get_double(lcstruct,"charge_limit").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF ldAmount = 0 THEN RETURN appl_err("Charge/Comp value can not be zero").

IF TRIM(lcUserId) EQ "VISTA_" THEN RETURN appl_err("username is empty").

FIND mobsub NO-LOCK
WHERE mobsub.msseq = liMsSeq
  AND mobsub.brand = "1" NO-ERROR.
IF NOT AVAILABLE mobsub THEN
   RETURN appl_err(SUBST("MobSub entry &1 not found", liMsSeq)).

{Syst/commpaa.i}
katun = lcUserId.
gcBrand = "1". 
{Syst/tmsconst.i}


{Func/fcharge_comp_loaded.i}
/* check monthly limit */
ldeLoaded = fMonthLoaded(
               (IF ldAmount > 0 THEN "CHARGE" ELSE "COMP"),
               Mobsub.CLI,
               Mobsub.PayType).

IF ABSOLUTE(ldAmount + ldeLoaded) > ldChargeLimit THEN DO:
   RETURN appl_err("Charge exceeds monthly limit").
END.
/* check balance in prepaid */
IF Mobsub.PayType AND ldAmount > 0 THEN DO:
   ldeCurrBal = 0.
   RUN Gwy/balancequery.p(Mobsub.CLI).
   ldeCurrBal = INT(RETURN-VALUE) / 100 NO-ERROR.
   IF ldeCurrBal < ldAmount THEN DO:
      RETURN appl_err("Charge exceeds balance").
   END.
END.

RUN Mm/create_charge_comp.p( {&REQUEST_SOURCE_NEWTON} ,
                       liMsSeq,
                       (IF MobSub.PayType THEN lcUserId ELSE ""), 
                       ldAmount,
                       lcEventId,
                       0,
                       OUTPUT liReqId) .

IF ERROR-STATUS:ERROR = TRUE THEN DO:
   RETURN appl_err(RETURN-VALUE).
END.
ELSE
add_int(response_toplevel_id, "request_id",liReqId).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
