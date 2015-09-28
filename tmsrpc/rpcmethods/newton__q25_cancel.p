/**
   newton__q25_cancel.p
   * Cancel ongoing Quota 25 extension request in TMS
    or Terminate Quota 25 extension (create single fee for remaining monthly fees)
   
   * @input    struct;mandatory
   * @struct   q25_struct;struct;mandatory
               memo_struct;struct;optional
   * @q25_struct     username;string;mandatory;person who requests the change
                     msseq;int;mandatory;subscription id
                     per_contract_id;int;mandatory;installment contract id (related to q25)
   
   * @memo_struct    title;string;mandatory
                    content;string;mandatory
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
DEF VAR lcq25_username    AS CHARACTER NO-UNDO. /* Quota 25 person who requests the change */
DEF VAR liq25_msseq       AS INTEGER   NO-UNDO. /* Quota 25 subscription id */
DEF VAR liper_contract_id AS INTEGER   NO-UNDO. /* Quota 25 installment contract id */
DEF VAR pcQ25Struct       AS CHARACTER NO-UNDO. /* Quota 25 input struct */
DEF VAR pcQ25StructFields AS CHARACTER NO-UNDO. /* Quota 25 input fields */
DEF VAR lcQ25Struct       AS CHARACTER NO-UNDO.

/* memo_struct */
DEF VAR lcmemo_title       AS CHARACTER NO-UNDO. /* Memo Title */
DEF VAR lcmemo_content     AS CHARACTER NO-UNDO. /* Memo Content */
DEF VAR pcmemoStruct       AS CHARACTER NO-UNDO. /* Memo input struct */
DEF VAR pcmemoStructFields AS CHARACTER NO-UNDO. /* Memo input fields */
DEF VAR lcmemoStruct       AS CHARACTER NO-UNDO.

DEF VAR pcPayTermStruct  AS CHARACTER NO-UNDO.
DEF VAR liCreated        AS INTEGER   NO-UNDO.
DEF VAR lcResult         AS CHARACTER NO-UNDO.
DEF VAR lcStruct         AS CHARACTER NO-UNDO.
DEF VAR i                AS INTEGER   NO-UNDO. 

/* Date when the payment plan was created */   
DEF VAR ldBillPeriod      AS DATE NO-UNDO.
/* Temp variable for the date of the extension request */
DEF VAR ldaProrateRequest AS DATE NO-UNDO.
/* Month 24 Date */
DEF VAR ldaMonth24Date    AS DATE NO-UNDO.
/* Number of months elapsed since the payment plan was created */
DEF VAR liMonthsElapsed   AS INTEGER NO-UNDO.

   /* Input variables for Quota 25 */
   pcQ25StructFields = "username!," +
                       "msseq!,"    +
                       "per_contract_id!".
                       
   /* Input variables for Memo */
   pcmemoStructFields = "title!," +
                       "content!".                    
                       
   /* common validation */
   IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
   top_struct = get_struct(param_toplevel_id, "0").
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   IF validate_request(top_struct, "struct") EQ ? THEN RETURN.
         
   top_struct_fields = validate_request(top_struct, 
      "q25_struct!,memo_struct").
   IF top_struct_fields EQ ? THEN RETURN.
   
   ASSIGN
      pcQ25Struct  = get_struct(top_struct, "q25_struct"). /* Quota 25 */
      pcmemoStruct = get_struct(top_struct, "memo_struct"). /* Memo */
   IF gi_xmlrpc_error <> 0 THEN RETURN.
   
   lcQ25Struct = validate_request(pcQ25Struct, pcQ25StructFields).
   IF lcQ25Struct EQ ? THEN RETURN.
   RUN fGetQ25Fields IN THIS-PROCEDURE.
   IF gi_xmlrpc_error <> 0 THEN RETURN.
   
   lcmemoStruct = validate_request(pcmemoStruct, pcmemoStructFields).
   IF lcmemoStruct EQ ? THEN RETURN.
   RUN fGetMemoFields IN THIS-PROCEDURE.
   IF gi_xmlrpc_error <> 0 THEN RETURN.
   
   katun = "VISTA_" + lcq25_username.
   
   FIND FIRST MobSub WHERE
              MobSub.MsSeq EQ liq25_msseq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN
      RETURN appl_err("ERROR:Q25 Subscription not found").
   
   /* Try to find an ongoing request */   
   FIND FIRST MSRequest NO-LOCK WHERE 
              MSRequest.MsSeq      EQ Mobsub.MsSeq AND
              MSRequest.ReqType    EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
              LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_ONGOING_STATUSES}) EQ 0 AND
              MSREquest.REqcparam3 EQ "RVTERM12" AND
              MSREquest.ReqIParam3 EQ liper_contract_id
   NO-ERROR.
         
   IF AVAILABLE MsRequest THEN DO:
      fReqStatus(4,"ERROR:Q25 Cancelled Quota 25 extension request").
   END. 
   ELSE 
   DO: /* Cancel Quota 25 Extension */
      /* Try to find a Quota 25 contract */   
      FIND FIRST DCCLI NO-LOCK WHERE
                 DCCLI.Brand   EQ gcBrand AND
                 DCCLI.DCEvent EQ "RVTERM12" AND
                 DCCLI.MsSeq   EQ MobSub.MsSeq AND
                 DCCLI.ValidTo <= TODAY
      NO-ERROR.
      IF AVAILABLE DCCLI THEN
      liCreated = fPCActionRequest(MobSub.MsSeq,
         "RVTERM12",
         "term",
         fMakeTS(),
         TRUE, /* create fees */
         {&REQUEST_SOURCE_NEWTON},
         "",
         0,
         FALSE,
         "",
         0, /* payterm residual fee */
         0,
         OUTPUT lcResult).
   
         IF liCreated EQ 0 THEN
            RETURN appl_err("ERROR:Q25 Unable to cancel Quota 25 extension request").
   END. /* Cancel Quota 25 Extension */   
   
   /* Create a memo in TMS */
   IF lcmemo_title > "" AND lcmemo_content > "" THEN DO:
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
          Memo.CustNum   = (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0).
   END. /* IF lcmemo_title > "" AND lcmemo_content > "" THEN DO: */
   
   FINALLY:
      PAUSE 0.
END. /* MAIN PROGRAM */
/* --- Functions & Procedures --- */
PROCEDURE fGetQ25Fields:
    /* Quota 25 person who requests the change */
    IF LOOKUP("username", lcQ25Struct) > 0 THEN
    lcq25_username = get_string(pcQ25Struct, "username").
    
    /* Quota 25  subscription id */
    IF LOOKUP("msseq", lcQ25Struct) > 0 THEN
    liq25_msseq = get_int(pcQ25Struct, "msseq").
    
    /* Quota 25 installment contract id */
    IF LOOKUP("per_contract_id", lcQ25Struct) > 0 THEN
    liper_contract_id = get_int(pcQ25Struct, "per_contract_id").      
END PROCEDURE.

PROCEDURE fGetMemoFields:
    /* Memo Title */
    IF LOOKUP("title", lcmemoStruct) > 0 THEN
    lcmemo_title = get_string(pcmemoStruct, "title").
    
    /* Memo Content */
    IF LOOKUP("title", lcmemoStruct) > 0 THEN
    lcmemo_content = get_string(pcmemoStruct, "content").
END PROCEDURE.
