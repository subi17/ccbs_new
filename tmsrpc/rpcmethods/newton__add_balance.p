/**
 * Change balance according to MSISDN.
 *
 * @input  msisdn;string;mandatory;user msisdn
           balance;double;mandatory;balance amount
           balance_type;string;mandatory;balance type (Default=TOP)
 * @output status;int;Change status (0=failure, 1=Success)
 */

{xmlrpc/xmlrpc_access.i}
{tmsparam4.i}
{commpaa.i}
katun = "Newton".
gcBrand = "1".
{provmaint.i}
{timestamp.i}

DEF VAR pcTenant  AS CHAR NO-UNDO.
DEF VAR pcMsisdn  AS CHAR NO-UNDO.
DEF VAR pdBalance AS DEC NO-UNDO.
DEF VAR pcBalType AS CHAR NO-UNDO INIT "TOP".
DEF VAR piStatus  AS INT NO-UNDO INIT 0.
DEF VAR lcParams  AS CHAR NO-UNDO.

/* validate 1st,2nd and 3rd parameter. */
lcParams = validate_request(param_toplevel_id, "string,string,double,string").
IF lcParams EQ ? THEN RETURN.
pcTenant = get_string(param_toplevel_id, "0").
pcMsisdn = get_string(param_toplevel_id, "1").
pdBalance = get_double(param_toplevel_id, "2"). /* Returns decimal */
IF NUM-ENTRIES(lcParams) EQ 4 THEN DO: /* If empty string then use initial value */
   IF get_string(param_toplevel_id, "3") > "" THEN
   pcBalType = get_string(param_toplevel_id, "3").
END.
IF gi_xmlrpc_error NE 0 THEN RETURN.

{settenant.i pcTenant}

FIND FIRST MobSub NO-LOCK WHERE
           MobSub.Brand = gcBrand AND
           MobSub.CLI = pcMsisdn NO-WAIT NO-ERROR. /* Find with MSISDN */
IF NOT AVAILABLE MobSub THEN
   RETURN appl_err(SUBST("MSISDN not found &1", pcMsisdn)).

IF NOT MobSub.PayType THEN /* If post paid */
   RETURN appl_err(SUBST("Incorrect pay type &1", MobSub.PayType)).
   
   /* parameter validation ends*/
   /* Clean all other Balance types to zero */
FOR EACH MsBalance EXCLUSIVE-LOCK WHERE
         MsBalance.MsSeq   = MobSub.MsSeq AND
         MsBalance.CustNum = MobSub.CustNum:
   ASSIGN 
         MsBalance.Amount  = 0
         MsBalance.BalDate = TODAY.
   RELEASE MsBalance.
END.

   /* Search with selected Balance type */
FIND FIRST MsBalance WHERE
           MsBalance.MsSeq   = MobSub.MsSeq   AND
           MsBalance.CustNum = MobSub.CustNum AND
           MsBalance.BalType = pcBalType EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE MsBalance THEN DO:
          CREATE MsBalance.
          ASSIGN MsBalance.MsSeq   = MobSub.MsSeq
                 MsBalance.CustNum = MobSub.CustNum 
                 MsBalance.BalType = pcBalType.
END.

   /* Change the balance */
ASSIGN 
   MsBalance.Amount  = pdBalance
   MsBalance.BalDate = TODAY
   piStatus = 1.
RELEASE MsBalance.

   /*  Response status  */
add_int(response_toplevel_id, "",  piStatus).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
