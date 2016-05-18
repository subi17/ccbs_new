/**
newton__q25_change.p
* Change Quota 25 extension amount in TMS

* @input    struct;mandatory
            q25_struct;struct;mandatory
            memo_struct;struct;optional

* @q25_struct     username;string;mandatory;person who requests the change
                  msseq;int;mandatory;subscription id
                  per_contract_id;int;mandatory;installment contract id (related to q25)
                  new_amount;double;mandatory

* @memo_struct    title;string;mandatory
                  content;string;mandatory

* @output         boolean;true
*/


{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/fsendsms.i}

/* top_struct */
DEF VAR top_struct        AS CHARACTER NO-UNDO.
DEF VAR top_struct_fields AS CHARACTER NO-UNDO.

/* q25_struct */
DEF VAR lcusername    AS CHARACTER NO-UNDO. /* Quota 25 person who requests the change */
DEF VAR limsseq       AS INTEGER   NO-UNDO. /* Quota 25 subscription id */
DEF VAR liper_contract_id AS INTEGER   NO-UNDO. /* Quota 25 installment contract id */
DEF VAR pcQ25Struct       AS CHARACTER NO-UNDO. /* Quota 25 input struct */
DEF VAR lcQ25Struct       AS CHARACTER NO-UNDO.

/* memo_struct */
DEF VAR lcmemo_title       AS CHARACTER NO-UNDO. /* Memo Title */
DEF VAR lcmemo_content     AS CHARACTER NO-UNDO. /* Memo Content */
DEF VAR pcmemoStruct       AS CHARACTER NO-UNDO. /* Memo input struct */
DEF VAR lcmemoStruct       AS CHARACTER NO-UNDO.

DEF VAR liCreated        AS INTEGER   NO-UNDO.
DEF VAR lcResult         AS CHARACTER NO-UNDO.
DEF VAR pdeQ25NewAmt     AS DEC NO-UNDO.


/* common validation */
IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
top_struct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

top_struct_fields = validate_request(top_struct, 
   "q25_struct!,memo_struct").
IF top_struct_fields EQ ? THEN RETURN.

ASSIGN
   pcQ25Struct  = get_struct(top_struct, "q25_struct")
   pcmemoStruct = get_struct(top_struct, "memo_struct") WHEN
      LOOKUP("memo_struct", top_struct_fields) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcQ25Struct = validate_request(pcQ25Struct,"username!,msseq!,per_contract_id!,new_amount!").
IF lcQ25Struct EQ ? THEN RETURN.

ASSIGN
   lcusername = get_string(pcQ25Struct, "username")
   limsseq = get_int(pcQ25Struct, "msseq")
    /* Quota 25 installment contract id */
   liper_contract_id = get_int(pcQ25Struct, "per_contract_id")      
   pdeQ25NewAmt = get_double(pcQ25Struct, "new_amount").

IF gi_xmlrpc_error NE 0 THEN RETURN.
      
IF pcmemoStruct > "" THEN DO:
   
   lcmemoStruct = validate_request(pcmemoStruct, "title!,content!").
   IF lcmemoStruct EQ ? THEN RETURN.
   
   ASSIGN
      lcmemo_title = get_string(pcmemoStruct, "title")
      lcmemo_content = get_string(pcmemoStruct, "content").

   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.

katun = "VISTA_" + lcusername.
IF TRIM(katun) EQ "VISTA_" THEN
   RETURN appl_err("username is empty").

IF pdeQ25NewAmt <= 0 THEN
   RETURN appl_err("incorrect new_amount value").

FIND FIRST MobSub WHERE
           MobSub.MsSeq = liMsSeq NO-LOCK NO-ERROR.
           
IF NOT AVAILABLE MobSub THEN
   RETURN appl_err("Subscription not found").

FIND FIRST DayCampaign NO-LOCK WHERE
           DayCampaign.Brand   = gcBrand AND
           DayCampaign.DCEvent = "RVTERM12" AND
           DayCampaign.DCType  = {&DCTYPE_INSTALLMENT} AND
           DayCampaign.ValidFrom <= TODAY AND
           DayCampaign.ValidTo >= TODAY NO-ERROR.
IF NOT AVAIL DayCampaign THEN
   RETURN appl_err("New Q25 contract type is not valid").

/* Find original Q25 contract */   
FIND FIRST DCCLI NO-LOCK WHERE
           DCCLI.Brand   EQ gcBrand AND
           DCCLI.DCEvent EQ "RVTERM12" AND
           DCCLI.MsSeq   EQ MobSub.MsSeq AND
           DCCLI.ValidTo GE TODAY AND
           DCCLI.PerContractId = liper_contract_id NO-ERROR.

IF NOT AVAIL DCCLI THEN
   RETURN appl_err("Q25 contract not found").

IF DCCLI.TermDate NE ? THEN
   RETURN appl_err("Q25 contract terminated").

IF ADD-INTERVAL(TODAY, -5, "months") >= DCCLI.ValidFrom THEN
   RETURN appl_err("Q25 is older than 5 months").

liCreated = fInstallmentChangeRequest(MobSub.MsSeq,
                                      DCCLI.DCEvent, /* old contract */
                                      DayCampaign.DCEvent, /* new contract */
                                      {&REQUEST_SOURCE_NEWTON},
                                      "",    /* creator */
                                      0,     /* orig. request */
                                      FALSE, /* mandatory */
                                      DCCLI.PerContractId,  /* Per. Contr.ID */
                                      pdeQ25NewAmt,
                                      OUTPUT lcResult).
   
IF liCreated = 0 THEN
   RETURN appl_err(SUBST("Q25 extension change request failed: &1",
                         lcResult)).


IF lcmemo_title > "" THEN DO:

   CREATE Memo.
   ASSIGN
       Memo.CreStamp  = {&nowTS}
       Memo.Brand     = gcBrand
       Memo.HostTable = "MobSub"
       Memo.KeyValue  = STRING(MobSub.MsSeq)
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = katun
       Memo.MemoTitle = lcmemo_title
       Memo.MemoText  = lcmemo_content
       Memo.CustNum   = MobSub.CustNum.
END. /* IF lcmemo_title > "" AND lcmemo_content > "" THEN DO: */

add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
