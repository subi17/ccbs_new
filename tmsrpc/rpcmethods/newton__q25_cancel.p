/**
* Cancel ongoing Quota 25 extension request in TMS 
  or Terminate Quota 25 extension (create single fee for remaining monthly fees)
* @input       struct;mandatory
* @struct      q25_struct;struct;mandatory
               memo_struct;struct;optional
* @q25_struct  username;string;mandatory;person who requests the change
               msseq;int;mandatory;subscription id
               per_contract_id;int;mandatory;installment contract id (related to q25)
               action;string;mandatory
* @memo_struct title;string;mandatory
               content;string;mandatory
* @output      boolean;true
*/

/*
   04.09.2015 hugo.lujan YPR-2516 [Q25] - TMS - TMSRPC changes related
   to Vista/VFR
    AC1: Create new TMSRPC to perform following actions:
    Cancel - Cancel ongoing Quota 25 extension request in TMS or 
    Terminate Quota 25 extension (create single fee for remaining monthly fees)
    AC2: Create a memo in TMS
*/
{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".
{timestamp.i}
{tmsconst.i}
{msreqfunc.i} /* fReqStatus */
{fmakemsreq.i}

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

DEF VAR pcAction           AS CHARACTER NO-UNDO.

DEF VAR liCreated        AS INTEGER   NO-UNDO.
DEF VAR lcResult         AS CHARACTER NO-UNDO.
DEF VAR liLoop           AS INT NO-UNDO. 
DEF VAR liReqStatus      AS INT NO-UNDO. 
DEF VAR lcAction         AS CHARACTER NO-UNDO.
DEF VAR llCreateFees     AS LOGICAL NO-UNDO.

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

lcQ25Struct = validate_request(pcQ25Struct,"username!,msseq!,per_contract_id!,action!").
IF lcQ25Struct EQ ? THEN RETURN.

ASSIGN
   lcusername = get_string(pcQ25Struct, "username")
      WHEN LOOKUP("username", lcQ25Struct) > 0
   limsseq = get_int(pcQ25Struct, "msseq")
      WHEN LOOKUP("msseq", lcQ25Struct) > 0
    /* Quota 25 installment contract id */
   liper_contract_id = get_int(pcQ25Struct, "per_contract_id")      
      WHEN LOOKUP("per_contract_id", lcQ25Struct) > 0
   pcAction     = get_string(pcQ25Struct, "action")
      WHEN LOOKUP("action", lcQ25Struct) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.
      
IF pcmemoStruct > "" THEN DO:
   
   lcmemoStruct = validate_request(pcmemoStruct, "title!,content!").
   IF lcmemoStruct EQ ? THEN RETURN.
   
   ASSIGN
      lcmemo_title = get_string(pcmemoStruct, "title")
         WHEN LOOKUP("title", lcmemoStruct) > 0
      lcmemo_content = get_string(pcmemoStruct, "content")
         WHEN LOOKUP("content", lcmemoStruct) > 0.

   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.

katun = "VISTA_" + lcusername.

FIND FIRST MobSub WHERE
           MobSub.MsSeq EQ limsseq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN
   RETURN appl_err("Subscription not found").

DO liLoop = 1 TO NUM-ENTRIES({&REQ_ONGOING_STATUSES}):
   liReqStatus = INT(ENTRY(liLoop,{&REQ_ONGOING_STATUSES})).
   /* Try to find an ongoing request */   
   FIND FIRST MSRequest NO-LOCK WHERE 
              MSRequest.MsSeq      EQ Mobsub.MsSeq AND
              MSRequest.ReqType    EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
              MsRequest.ReqStatus  EQ liReqStatus AND
              MSREquest.REqcparam3 EQ "RVTERM12" AND
              MSREquest.ReqIParam3 EQ liper_contract_id
   NO-ERROR.
   IF AVAIL MSREquest THEN LEAVE.
END.
      
IF AVAILABLE MsRequest THEN DO:

   IF MsRequest.ReqStatus NE 0 THEN
      RETURN appl_err("Ongoing Q25 activation request").

   fReqStatus(4,"Manually cancelled").
END. 
ELSE DO: /* Cancel Quota 25 Extension */

   FIND DCCLI NO-LOCK WHERE
        DCCLI.Brand   EQ gcBrand AND
        DCCLI.DCEvent EQ "RVTERM12" AND
        DCCLI.MsSeq   EQ MobSub.MsSeq AND
        DCCLI.ValidTo >= TODAY
   NO-ERROR.

   IF AMBIGUOUS(DCCLI) THEN 
      RETURN appl_err("More than one active Q25 contract").
   
   IF NOT AVAIL DCCLI THEN
      RETURN appl_err("No active Q25 contract or activation request").

   IF DCCLI.TermDate NE ? THEN 
      RETURN appl_err("Q25 contract terminated").

   CASE pcAction:
   WHEN "remove" THEN ASSIGN
      lcAction = "term"
      llCreateFees = TRUE.
   WHEN "cancel" THEN DO: 
      IF ADD-INTERVAL(TODAY, -5, "months") >= DCCLI.ValidFrom THEN
         RETURN appl_err("Installment is older than 5 months").
      ASSIGN
         lcAction = "canc"
         llCreateFees = FALSE.
   END.
   OTHERWISE RETURN appl_err("Incorrect action").
   END. 

   liCreated = fPCActionRequest(MobSub.MsSeq,
      "RVTERM12",
      lcAction,
      fMakeTS(),
      llCreateFees, /* create fees */
      {&REQUEST_SOURCE_NEWTON},
      "",
      0,
      FALSE,
      "",
      0, /* payterm residual fee */
      DCCLI.PerContractId,
      OUTPUT lcResult).

      IF liCreated EQ 0 THEN
         RETURN appl_err("ERROR:Q25 Unable to " + LC(pcAction) + " remove " + 
                         "Quota 25 extension request").

END. /* Cancel Quota 25 Extension */   

/* Create a memo in TMS */
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
