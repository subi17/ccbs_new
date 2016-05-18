/**
 * Get monthly prepaid EDRs
 *
 * @input  int;mandatory;subscription ID
 * @output array of edr_struct;mandatory;EDR data
 * @edr_struct event_time;datetime;mandatory;event time
               success_code;string;mandatory;OK/NOK
               charge_amount;double;mandatory;charged amount in euros
               balance_after;double;mandatory;prepaid account balance after charge in euros
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
ASSIGN
   katun = "NewtonRPC"
   gcBrand = "1".
{Func/date.i}
{Func/callquery.i}

DEF VAR resp_array AS CHAR NO-UNDO. 
DEF VAR edr_struct AS CHAR NO-UNDO. 
DEF VAR piMsSeq AS INT NO-UNDO. 

DEF TEMP-TABLE ttCall NO-UNDO LIKE PrepEDR.

DEF VAR ldeEventTime AS DEC NO-UNDO. 
DEF VAR lcSuccessCode AS CHAR NO-UNDO. 
DEF VAR liErrorCodeOut AS INT NO-UNDO. 
DEF VAR tthCDR AS HANDLE NO-UNDO.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piMsSeq = get_pos_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST mobsub NO-LOCK WHERE
           mobsub.msseq = piMsSeq NO-ERROR.
IF NOT AVAILABLE mobsub THEN
   RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

resp_array = add_array(response_toplevel_id, "").
tthCDR = TEMP-TABLE ttCall:HANDLE.

fMobCDRCollect(INPUT "edr",
               INPUT gcBrand,
               INPUT katun,
               INPUT TODAY - 30,
               INPUT TODAY,
               INPUT 0,
               INPUT "",
               INPUT MobSub.CLI,
               INPUT 0,
               INPUT 0,
               INPUT "",
               INPUT "",
               INPUT "",
               INPUT 0,
               INPUT-OUTPUT liErrorCodeOut,
               INPUT-OUTPUT tthCDR).

FOR EACH ttCall NO-LOCK WHERE
         ttCall.MsSeq = MobSub.Msseq AND
         ttCall.ErrorCode = 0:

   ldeEventTime = fMake2Dt(ttCall.DateSt, ttCall.TimeStart).

   CASE ttCall.SuccessCode:
      WHEN 1 THEN lcSuccessCode = "OK".
      WHEN 3 THEN lcSuccessCode = "NOK".
      OTHERWISE lcSuccessCode = STRING(ttCall.SuccessCode).
   END.

   edr_struct = add_struct(resp_array, "").
   add_timestamp(edr_struct,"event_time",ldeEventTime).
   add_string(edr_struct,"success_code",lcSuccessCode).
   add_double(edr_struct,"charge_amount",ttCall.SubscriberFee).
   add_double(edr_struct,"balance_after",ttCall.BalanceAfter).
END.

FINALLY:
   EMPTY TEMP-TABLE ttCall.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.
END.
