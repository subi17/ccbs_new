/* ----------------------------------------------------------------------
  Module .......: tmsrpc/rpcmethods/newton__change_payterm.p
  Task .........: Change Payterm Contract
  Application ..: RPCMETHOD
  Author .......: Vikas
  Created ......: 21.03.13
  Version ......: Yoigo

  @input .......: paytermstruct;struct;mandatory;contains input data
                  memostruct;struct;mandatory;memo

  @paytermstruct: username;string;mandatory;person who requests the change
                  msseq;int;mandatory;Subs. Id
                  current_payterm;string;mandatory;current payterm contract
                  new_payterm;string;mandatory;new payterm contract
                  per_contract_id;int;mandatory; periodical contract id

  @memostruct   : title;string;mandatory
                  content;string;mandatory

  @Output ......: success;boolean
---------------------------------------------------------------------- */
{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".
{timestamp.i}
{tmsconst.i}
{fmakemsreq.i}
{fcreatereq.i}

DEFINE VARIABLE pcPayTermStruct   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pcMemoStruct      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE liMsSeq           AS INTEGER     NO-UNDO.
DEFINE VARIABLE lcCurrentPayterm  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcNewPayterm      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE liCreated         AS INTEGER     NO-UNDO.
DEFINE VARIABLE lcResult          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcStruct          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcMemoTitle       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcMemoContent     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE liPerContractId   AS INTEGER     NO-UNDO.  

IF validate_request(param_toplevel_id, "struct,struct") EQ ? THEN RETURN.

pcPayTermStruct = get_struct(param_toplevel_id, "0").
pcMemoStruct    = get_struct(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcPayTermStruct,"username!,msseq!,current_payterm!,new_payterm!,per_contract_id!").
IF lcStruct EQ ? THEN RETURN.

lcStruct = validate_request(pcMemoStruct,"title!,content!").
IF lcStruct EQ ? THEN RETURN.

/* Required Params */
ASSIGN 
   liMsSeq  = get_pos_int(pcPayTermStruct, "msseq")
   katun    = "VISTA_" + get_string(pcPayTermStruct, "username")
   lcCurrentPayterm = get_string(pcPayTermStruct, "current_payterm")
   lcNewPayterm     = get_string(pcPayTermStruct, "new_payterm")
   liPerContractId  = get_int(pcPayTermStruct, "per_contract_id")
   lcMemoTitle = get_string(pcMemoStruct, "title")
   lcMemoContent = get_string(pcMemoStruct, "content").

IF TRIM(katun) EQ "VISTA_" THEN
   RETURN appl_err("username is empty").

IF lcCurrentPayterm = "" OR lcCurrentPayterm = ? THEN
   RETURN appl_err("Current installment contract is empty").

IF lcNewPayterm = "" OR lcNewPayterm = ? THEN
   RETURN appl_err("New installment contract is empty").

FIND FIRST MobSub WHERE
           MobSub.MsSeq = liMsSeq NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN
   RETURN appl_err("MobSub not found").

FIND FIRST DayCampaign WHERE 
           DayCampaign.Brand   = gcBrand AND
           DayCampaign.DCEvent = lcNewPayterm NO-LOCK NO-ERROR.
IF NOT AVAIL DayCampaign OR 
   DayCampaign.ValidFrom > TODAY OR
   DayCampaign.ValidTo   < TODAY THEN
   RETURN appl_err("New installment contract is not valid").

FIND FIRST DCCLI WHERE
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


