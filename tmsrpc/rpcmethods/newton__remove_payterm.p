/* ----------------------------------------------------------------------
* Remove or cancel active installment contract
*
* @input          struct;mandatory;payterm_struct
                  struct;mandatory;memo_struct

* @payterm_struct username;string;mandatory;person who requests the change
                  msseq;int;mandatory;subscription id
                  payterm_contract;string;payterm contract name
                  per_contract_id;int;mandatory;TMS internal payterm contract id
                  action;string;mandatory;"remove" or "cancel"

* @memo_struct    title;string;mandatory
                  content;string;mandatory

* @output         boolean;true
---------------------------------------------------------------------- */
{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".
{timestamp.i}
{tmsconst.i}
{fmakemsreq.i}
{coinv.i}

DEF VAR pcPayTermStruct   AS CHARACTER   NO-UNDO.
DEF VAR pcMemoStruct      AS CHARACTER   NO-UNDO.

DEF VAR liMsSeq           AS INTEGER     NO-UNDO.
DEF VAR lcCurrentPayterm  AS CHARACTER   NO-UNDO.
DEF VAR liCreated         AS INTEGER     NO-UNDO.
DEF VAR lcResult          AS CHARACTER   NO-UNDO.
DEF VAR lcStruct          AS CHARACTER   NO-UNDO.
DEF VAR lcMemoTitle       AS CHARACTER   NO-UNDO.
DEF VAR lcMemoContent     AS CHARACTER   NO-UNDO.
DEF VAR liPerContractId   AS INTEGER     NO-UNDO.  
DEF VAR lcAction          AS CHAR        NO-UNDO. 
DEF VAR liLastUnBilledPeriod AS INT NO-UNDO. 
DEF VAR ldaLastUnBilledDate AS DATE NO-UNDO. 
DEF VAR ldePeriodTo AS DEC NO-UNDO. 
DEF VAR llCreateFees AS LOG NO-UNDO. 

IF validate_request(param_toplevel_id, "struct,struct") EQ ? THEN RETURN.

pcPayTermStruct = get_struct(param_toplevel_id, "0").
pcMemoStruct    = get_struct(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcPayTermStruct,"username!,msseq!,payterm_contract!,per_contract_id!,action!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcMemoStruct,"title!,content!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* Required Params */
ASSIGN 
   liMsSeq  = get_pos_int(pcPayTermStruct, "msseq")
   katun    = "VISTA_" + get_nonempty_string(pcPayTermStruct, "username")
   lcCurrentPayterm = get_nonempty_string(pcPayTermStruct, "payterm_contract")
   liPerContractId  = get_int(pcPayTermStruct, "per_contract_id")
   lcAction = get_nonempty_string(pcPayTermStruct, "action")
   lcMemoTitle = get_string(pcMemoStruct, "title")
   lcMemoContent = get_string(pcMemoStruct, "content").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST MobSub WHERE
           MobSub.MsSeq = liMsSeq NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN
   RETURN appl_err("Subscription not found").

CASE lcAction:
   WHEN "remove" THEN ASSIGN
      lcAction = "term"
      llCreateFees = TRUE.
   WHEN "cancel" THEN ASSIGN
      lcAction = "canc"
      llCreateFees = FALSE.
      
   OTHERWISE RETURN appl_err("Incorrect action").
END.

FIND FIRST DayCampaign NO-LOCK WHERE 
           DayCampaign.Brand   = gcBrand AND
           DayCampaign.DCEvent = lcCurrentPayterm AND
           DayCampaign.DCType  = {&DCTYPE_INSTALLMENT} NO-ERROR.
IF NOT AVAIL DayCampaign THEN
   RETURN appl_err("Installment contract type is not valid").

FIND DCCLI WHERE
     DCCLI.MsSeq         = MobSub.MsSeq     AND
     DCCLI.DCEvent       = DayCampaign.DCEvent AND
     DCCLI.PerContractID = liPerContractId  AND
     DCCLI.ValidTo      >= TODAY NO-LOCK NO-ERROR.
IF NOT AVAIL DCCLI THEN
   RETURN appl_err("Active installment contract not found").
   
IF lcAction EQ "canc" THEN DO:
   
   FIND FixedFee NO-LOCK USE-INDEX CustNum WHERE
        FixedFee.Brand     = gcBrand   AND
        FixedFee.CustNum   = MobSub.CustNum AND
        FixedFee.HostTable = "MobSub"  AND
        FixedFee.KeyValue  = STRING(MobSub.MsSeq) AND
        FixedFee.CalcObj   = DCCLI.DCEvent AND
        FixedFee.SourceTable = "DCCLI" AND
        FixedFee.SourceKey = STRING(DCCLI.PerContractId) NO-ERROR.

   IF NOT AVAIL FixedFee THEN 
      RETURN appl_err("Installment contract fee not found").

   IF ADD-INTERVAL(TODAY, -5, "months") >= DCCLI.ValidFrom THEN
      RETURN appl_err("Installment is older than 5 months").

   FOR EACH FFItem OF FixedFee NO-LOCK USE-INDEX FFNum:
      IF FFItem.Billed = TRUE AND
         CAN-FIND (FIRST Invoice USE-INDEX InvNum WHERE
                         Invoice.Brand   = gcBrand AND
                         Invoice.InvNum  = FFItem.InvNum AND
                         Invoice.InvType = 1 NO-LOCK) THEN NEXT.
      liLastUnBilledPeriod = FFItem.BillPeriod.
      LEAVE.
   END.

   IF liLastUnBilledPeriod = 0 THEN
      liLastUnBilledPeriod = FixedFee.BegPeriod.

   ldaLastUnBilledDate = fPer2Date(liLastUnBilledPeriod,0) - 1.
   ldePeriodTo = fMake2Dt(ldaLastUnBilledDate,86399).
END.
ELSE ldePeriodTo = fMakeTS().

liCreated = fPCActionRequest(MobSub.MsSeq,
                             DCCLI.DCEvent,
                             lcAction,
                             ldePeriodTo,
                             llCreateFees,
                             {&REQUEST_SOURCE_NEWTON},
                             "",
                             0,
                             FALSE,
                             "",
                             0,
                             DCCLI.PerContractID,
                             OUTPUT lcResult).
IF liCreated = 0 THEN
   RETURN appl_err(SUBST("Request creation failed: &1",
                   lcResult)).

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
