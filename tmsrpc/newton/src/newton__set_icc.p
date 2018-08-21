/**
 * Request a new ICC
 *
 * @input   msisdn;int;mandatory;the subscription
            salesman;str;mandatory;person who requests the change
            value;str;mandatory;new ICC
            charge;double;mandatory;0 <= if not given
            charge_limit;double;mandatory;charge monthly limit used in web
            reason;mandatory; reason of change
 * @output success;boolean
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Func/msreqfunc.i}

DEF VAR liReq AS INTEGER NO-UNDO.

/* Input parameters */
DEF VAR pcTenant AS CHAR NO-UNDO.
DEF VAR pcMSISDN AS CHAR NO-UNDO.
DEF VAR pcSalesman AS CHAR NO-UNDO.
DEF VAR pcValue AS CHAR NO-UNDO.
/* Local variables */
DEF VAR lcc AS CHAR NO-UNDO.
DEF VAR lcError AS CHARACTER NO-UNDO. 
DEF VAR pdeCharge AS DECIMAL NO-UNDO. 
DEF VAR pdeChargeLimit AS DECIMAL NO-UNDO. 
DEF VAR pcReason AS CHARACTER NO-UNDO. 
DEF VAR pcContractID AS CHAR NO-UNDO.
DEF VAR pcChannel AS CHAR NO-UNDO.
DEF VAR lcOldICC AS CHAR NO-UNDO.

lcc = validate_request(param_toplevel_id, "string,string,string,string,double,double,string,string,string").
IF lcc EQ ? THEN RETURN.

pcTenant    = get_string(param_toplevel_id, "0").
pcMSISDN    = get_string(param_toplevel_id, "1").
pcSalesman  = get_string(param_toplevel_id, "2").
pcValue     = get_string(param_toplevel_id, "3").
pdeCharge   = get_double(param_toplevel_id, "4").
pdeChargeLimit = get_double(param_toplevel_id, "5").
pcReason   = get_string(param_toplevel_id, "6").
pcChannel = get_string(param_toplevel_id, "7").
pcContractID = get_string(param_toplevel_id, "8").
/*pcChannel 6
pcContract 7*/
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

FIND mobsub NO-LOCK
WHERE mobsub.brand = "1" and 
      MobSub.CLI = pcMSISDN NO-ERROR.
IF NOT AVAILABLE mobsub THEN
   RETURN appl_err(SUBST("MobSub entry &1 not found", pcMSISDN)).

/*YPR-4777*/
/*Operation is not allowed if fixed line provisioning is pending*/
IF (MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG}    /*16*/ OR
    MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) /*17*/ THEN
   RETURN appl_err("Mobile line provisioning is not complete").

FIND FIRST SIM NO-LOCK WHERE SIM.ICC = pcValue NO-ERROR. 
IF NOT AVAIL SIM THEN 
   RETURN appl_err(SUBST("SIM &1 not found", pcValue)).

IF LOOKUP(STRING(SIM.SimStat),"1,2,9") = 0 THEN    
   RETURN appl_err(SUBST("This SIM can not be used; status is &1", Sim.SimStat)).

/* Get the corrsponding NEW IMSI */
FIND FIRST IMSI WHERE IMSI.ICC = SIM.ICC NO-ERROR.
IF NOT AVAIL IMSI THEN
   RETURN appl_err("There is NO IMSI record assigned to this SIM !").

FIND FIRST MsRequest NO-LOCK
WHERE MsRequest.MsSeq     EQ mobsub.MsSeq
  AND MsRequest.ReqType   EQ 15
  AND LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0 NO-ERROR.

IF AVAIL MsRequest THEN
   RETURN appl_err("ICC change process already exists").

IF Mnp.MNPOutGoing:mIsMNPOutOngoing(mobsub.cli) THEN RETURN appl_err("Ongoing MNP OUT request").

IF TRIM(pcSalesman) EQ "" THEN RETURN appl_err("username is empty").

{Syst/commpaa.i}
ASSIGN 
   Syst.Var:gcBrand = "1"
   Syst.Var:katun = "VISTA_" + pcSalesman.
{Func/fmakemsreq.i}
{Func/fcharge_comp_loaded.i}

IF pdeCharge > 0 THEN DO:
   lcError = fCheckChargeLimits (
      Mobsub.CLI,
      Mobsub.PayType,
      pdeCharge,
      pdeChargeLimit).
END.

IF lcError > "" THEN DO:
   RETURN appl_err(lcError).
END.

/*YPR-3233*/
lcOldICC = Mobsub.ICC.
/*DM gets the value only by contractID*/
IF R-INDEX(pcChannel, "pos") EQ 0 THEN DO:
   pcContractId = "".
   lcOldICC = "".
END.   


liReq = fSubscriptionRequest(
            mobsub.MsSeq,
            Mobsub.Cli,
            Mobsub.CustNum,
            1,                        /*tarifftype*/
            "",                       /*creator*/
            ?,                        /*ActStamp*/
            "CHANGEICC",              /*ReqParam*/
            pcValue,                  /*ReqParam2*/
            "",                       /*OrderProductId*/ 
            lcOldICC,               /*old SIM*/
            pcReason,                 /*Reason*/
            pcContractID,             /*ContractID*/
            (pdeCharge > 0),          /*CreateFees*/
            pdeCharge,                /*Charge*/
            {&REQUEST_SOURCE_NEWTON}, /*Request source*/
            OUTPUT lcc).              /*result*/
            
IF liReq EQ 0 THEN RETURN appl_err(lcc).

FIND FIRST MSRequest WHERE
           MSRequest.MSrequest = liReq EXCLUSIVE-LOCK NO-ERROR.

IF pcReason NE '' THEN DO:
   CREATE Memo.
   ASSIGN
      Memo.CreStamp  = {&nowTS}
      Memo.Brand     = Syst.Var:gcBrand 
      Memo.HostTable = "MsRequest" 
      Memo.KeyValue  = STRING(liReq) 
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = Syst.Var:katun 
      Memo.MemoTitle = "ICC Change Reason"
      Memo.MemoText  = pcReason
      Memo.CustNum   = MsRequest.CustNum .
  
END.
ELSE fReqStatus(19,"").

RELEASE msRequest.          

FIND CURRENT SIM EXCLUSIVE-LOCK NO-ERROR.        
ASSIGN SIM.SimStat = 13.
RELEASE SIM.

add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   END.
