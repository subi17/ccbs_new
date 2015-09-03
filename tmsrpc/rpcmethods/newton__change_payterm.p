/* ----------------------------------------------------------------------
  Module .......: tmsrpc/rpcmethods/newton__change_payterm.p
  Task .........: Change Payterm Contract
  Application ..: RPCMETHOD
  Author .......: Vikas
  Created ......: 21.03.13
  Version ......: Yoigo

*  @input          paytermstruct;struct;mandatory;contains input data
                  memostruct;struct;mandatory;memo

*  @paytermstruct  username;string;mandatory;person who requests the change
                  msseq;int;mandatory;Subs. Id
                  current_payterm;string;mandatory;current payterm contract
                  new_payterm;string;mandatory;new payterm contract
                  per_contract_id;int;mandatory; periodical contract id

*  @memostruct     title;string;mandatory
                  content;string;mandatory

*  @output         boolean;true
---------------------------------------------------------------------- */
{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".
{timestamp.i}
{tmsconst.i}
{fmakemsreq.i}
{fcreatereq.i}

DEF VAR pcPayTermStruct   AS CHARACTER   NO-UNDO.
DEF VAR pcMemoStruct      AS CHARACTER   NO-UNDO.

DEF VAR liMsSeq           AS INTEGER     NO-UNDO.
DEF VAR lcCurrentPayterm  AS CHARACTER   NO-UNDO.
DEF VAR lcNewPayterm      AS CHARACTER   NO-UNDO.
DEF VAR ldeResidualValue  AS DEC NO-UNDO. 
DEF VAR liCreated         AS INTEGER     NO-UNDO.
DEF VAR lcResult          AS CHARACTER   NO-UNDO.
DEF VAR lcStruct          AS CHARACTER   NO-UNDO.
DEF VAR lcMemoTitle       AS CHARACTER   NO-UNDO.
DEF VAR lcMemoContent     AS CHARACTER   NO-UNDO.
DEF VAR liPerContractId   AS INTEGER     NO-UNDO.  

IF validate_request(param_toplevel_id, "struct,struct") EQ ? THEN RETURN.

pcPayTermStruct = get_struct(param_toplevel_id, "0").
pcMemoStruct    = get_struct(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcPayTermStruct,"username!,msseq!,current_payterm!,new_payterm!,per_contract_id!,residual_value").
IF gi_xmlrpc_error NE 0 THEN RETURN.

validate_request(pcMemoStruct,"title!,content!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* Required Params */
ASSIGN 
   liMsSeq  = get_pos_int(pcPayTermStruct, "msseq")
   katun    = "VISTA_" + get_nonempty_string(pcPayTermStruct, "username")
   lcCurrentPayterm = get_nonempty_string(pcPayTermStruct, "current_payterm")
   lcNewPayterm     = get_nonempty_string(pcPayTermStruct, "new_payterm")
   liPerContractId  = get_int(pcPayTermStruct, "per_contract_id")
   lcMemoTitle = get_string(pcMemoStruct, "title")
   lcMemoContent = get_string(pcMemoStruct, "content")
   ldeResidualValue = get_double(pcPayTermStruct, "residual_value") WHEN
                      LOOKUP("residual_value",lcStruct) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(katun) EQ "VISTA_" THEN
   RETURN appl_err("username is empty").

FIND FIRST MobSub WHERE
           MobSub.MsSeq = liMsSeq NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN
   RETURN appl_err("Subscription not found").

FIND FIRST DayCampaign NO-LOCK WHERE 
           DayCampaign.Brand   = gcBrand AND
           DayCampaign.DCEvent = lcNewPayterm AND
           DayCampaign.DCType  = {&DCTYPE_INSTALLMENT} AND
           DayCampaign.ValidFrom <= TODAY AND
           DayCampaign.ValidTo >= TODAY NO-ERROR.
IF NOT AVAIL DayCampaign THEN
   RETURN appl_err("New installment contract type is not valid").

FIND DCCLI WHERE
     DCCLI.MsSeq         = MobSub.MsSeq     AND
     DCCLI.DCEvent       = lcCurrentPayterm AND
     DCCLI.PerContractID = liPerContractId  AND
     DCCLI.ValidTo      >= TODAY NO-LOCK NO-ERROR.
IF NOT AVAIL DCCLI THEN
   RETURN appl_err("Current installment contract is not valid").

liCreated = fInstallmentChangeRequest(MobSub.MsSeq,
                                      DCCLI.DCEvent, 
                                      DayCampaign.DCEvent,
                                      {&REQUEST_SOURCE_NEWTON},
                                      "",    /* creator */
                                      0,     /* orig. request */
                                      FALSE, /* mandatory */
                                      DCCLI.PerContractId,  /* Periodical Contract ID */
                                      ldeResidualValue,
                                      OUTPUT lcResult).
IF liCreated = 0 THEN
   RETURN appl_err("Installment Change request could not be created").

IF lcMemoTitle > "" AND lcMemoContent > "" THEN DO:
   CREATE Memo.
   ASSIGN
       Memo.CreStamp  = {&nowTS}
       Memo.Brand     = gcBrand
       Memo.HostTable = "MobSub"
       Memo.KeyValue  = STRING(MobSub.MsSeq)
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = katun
       Memo.MemoTitle = lcMemoTitle
       Memo.MemoText  = lcMemoContent
       Memo.CustNum   = (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0).
END. /* IF lcMemoTitle > "" AND lcMemoContent > "" THEN DO: */

add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.


