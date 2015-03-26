/**
 * create charge events for mobsub.
 *
 * @input  struct;
           msseq;int;mandatory
           charge_event;string;mandatory
           amount;double;optional
           charge_limit;double;mandatory

 * @output requestid;int
*/   


{xmlrpc/xmlrpc_access.i}
/* &GLOBAL-DEFINE SKIP_FUNC_I YES */
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.

{commpaa.i}
katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId.
gcBrand = "1".
{tmsconst.i}
{fcustpl.i}

DEF VAR lcstruct AS CHAR NO-UNDO.
DEF VAR pcstruct AS CHAR NO-UNDO. 
DEF VAR liReqId AS INTEGER NO-UNDO.
DEF VAR liMsSeq AS INTEGER NO-UNDO.
DEF VAR lcEventId AS CHAR NO-UNDO.
DEF VAR ldAmount AS DECIMAL NO-UNDO.
DEF VAR ldChargeLimit AS DECIMAL NO-UNDO.
DEF VAR ldeLoaded AS DECIMAL NO-UNDO.
DEF VAR ldeCurrBal AS DECIMAL NO-UNDO.
DEF VAR lcPriceList AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.
pcstruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.
lcstruct = validate_request(pcstruct, "msseq!,charge_event_id!,amount,charge_limit!").

/* get parameters */

liMsSeq = get_int(pcstruct,"msseq").
lcEventId = get_string(pcstruct,"charge_event_id").
ldChargeLimit = get_double(pcstruct,"charge_limit").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND mobsub NO-LOCK
WHERE mobsub.msseq = liMsSeq
  AND mobsub.brand = gcBrand NO-ERROR.
IF NOT AVAILABLE mobsub THEN DO:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   RETURN appl_err(SUBST("MobSub entry &1 not found", liMsSeq)).
END.

IF LOOKUP("amount", lcStruct) GT 0 THEN
  ldAmount = get_double(pcstruct,"amount").
ELSE DO:

  FIND FeeModel WHERE 
       FeeModel.Brand = gcBrand AND
       FeeModel.FeeModel = lcEventId NO-LOCK NO-ERROR.
  IF NOT AVAIL FeeModel THEN DO:
      IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
      RETURN appl_err(SUBST("Charge/Comp billing event  &1 not found", lcEventId)).
  END.
  /* Fetch default charge */
  lcPriceList = fFeeModelPriceList(MobSub.Custnum,
                                   MobSub.BillTarget,
                                   FeeModel.FeeModel,
                                   TODAY).

  FIND FIRST FMItem NO-LOCK  WHERE
             FMItem.Brand     = gcBrand       AND
             FMItem.FeeModel  = FeeModel.FeeModel AND
             FMItem.PriceList = lcPriceList AND
             FMItem.FromDate <= TODAY     AND
             FMItem.ToDate   >= TODAY NO-ERROR.

  IF AVAIL FMItem THEN ldAmount = FMItem.Amount.
  ELSE DO:
     IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
     RETURN appl_err(SUBST("Charge/Comp billing event  &1 doesn't contain active item", lcEventId)).
  END.
END.

IF ldAmount = 0 THEN DO:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   RETURN appl_err("Charge/Comp value can not be zero").
END.

/* by the moment we don't have many details
  of how they will apply one time limit */

{fcharge_comp_loaded.i}
/* check monthly limit */
ldeLoaded = fMonthLoaded(
               (IF ldAmount > 0 THEN "CHARGE" ELSE "COMP"),
               Mobsub.CLI,
               Mobsub.PayType).

IF ABSOLUTE(ldAmount + ldeLoaded) > ldChargeLimit THEN DO:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   RETURN appl_err("Charge exceeds monthly limit").
END.
/* check balance in prepaid */
IF Mobsub.PayType AND ldAmount > 0 THEN DO:
   RUN balancequery(Mobsub.CLI).
   ldeCurrBal = INT(RETURN-VALUE) / 100.
   IF ldeCurrBal < ldAmount THEN DO:
        IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
        RETURN appl_err("Charge exceeds balance").
   END.
END.

RUN create_charge_comp.p( {&REQUEST_SOURCE_EXTERNAL_API} ,
                       liMsSeq,
                       (IF MobSub.PayType THEN katun ELSE ""), 
                       ldAmount,
                       lcEventId,
                       0,
                       OUTPUT liReqId) NO-ERROR.

IF ERROR-STATUS:ERROR = TRUE THEN DO:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   RETURN appl_err(RETURN-VALUE).
END.
ELSE
add_int(response_toplevel_id, "request_id",liReqId).

IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 


