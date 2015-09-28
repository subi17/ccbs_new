/**
newton__q25_add.p
* Create Quota 25 extension request in TMS

* @input    struct;mandatory
            q25_struct;struct;mandatory
            memo_struct;struct;optional

* @q25_struct     username;string;mandatory;person who requests the change
                  msseq;int;mandatory;subscription id
                  per_contract_id;int;mandatory;installment contract id (related to q25)

* @memo_struct    title;string;mandatory
                  content;string;mandatory
   19.08.2015 hugo.lujan YPR-2516 [Q25] - TMS - TMSRPC changes related
   to Vista/VFR
    AC1: Create new TMSRPC to perform following actions:
    Create - Create Quota 25 extension request in TMS
    AC2: Create a memo in TMS
    AC3: Send an SMS to customer if he selects Quota 25 extension
*/
{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".
{timestamp.i}
{tmsconst.i}
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

/* Text to be sent in an SMS 160 character maximum */
DEF VAR lcTextSMS AS CHARACTER NO-UNDO.
DEF VAR lcMSISDN  AS CHARACTER NO-UNDO.

/* Date when the payment plan was created */   
DEF VAR ldBillPeriod      AS DATE NO-UNDO.
/* Temp variable for the date of the extension request */
DEF VAR ldaProrateRequest AS DATE NO-UNDO.
/* Month 22 Date */
DEF VAR ldaMonth22Date    AS DATE NO-UNDO.
/* Month 24 Date */
DEF VAR ldaMonth24Date    AS DATE NO-UNDO.
/* Number of months elapsed since the payment plan was created */
DEF VAR liMonthsElapsed   AS INTEGER NO-UNDO.

/* Contract activation timestamp */
DEF VAR ldContractActivTS AS DECIMAL NO-UNDO.

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
   
   top_struct_fields = validate_request(top_struct, 
      "q25_struct!,memo_struct").
   IF top_struct_fields EQ ? THEN RETURN.
   
   ASSIGN
      pcQ25Struct  = get_struct(top_struct, "q25_struct"). /* Quota 25 */
      pcmemoStruct = get_struct(top_struct, "memo_struct"). /* Memo */
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   lcQ25Struct = validate_request(pcQ25Struct, pcQ25StructFields).
   IF lcQ25Struct EQ ? THEN RETURN.
   RUN fGetQ25Fields IN THIS-PROCEDURE.
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   lcmemoStruct = validate_request(pcmemoStruct, pcmemoStructFields).
   IF lcmemoStruct EQ ? THEN RETURN.
   RUN fGetMemoFields IN THIS-PROCEDURE.
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   katun = "VISTA_" + lcq25_username.
   
   FIND FIRST MobSub NO-LOCK WHERE
              MobSub.MsSeq = liq25_msseq NO-ERROR.
              
   IF NOT AVAILABLE MobSub THEN
      RETURN appl_err("ERROR:Q25 Subscription not found").
   
   /* Find original contract */   
   FIND FIRST DCCLI NO-LOCK WHERE
              DCCLI.Brand   EQ gcBrand AND
              DCCLI.DCEvent EQ "PAYTERM" AND
              DCCLI.MsSeq   EQ liq25_msseq
   NO-ERROR.
   
   ASSIGN   
      ldaProrateRequest = TODAY
      ldBillPeriod      = DCCLI.ValidFrom /* Contract start date */
      liMonthsElapsed   = INTERVAL(ldaProrateRequest,ldBillPeriod, 'months':U)
      ldaMonth24Date    = ADD-INTERVAL(ldBillPeriod, 24, 'months':U).
      ldaMonth22Date    = ADD-INTERVAL(ldBillPeriod, 22, 'months':U).
   
   /* If the Quota 25 prorate request is created between 1st day of month 22
      until 20th day of month 24 then
      it should be handled on 21st day of month 24 at 00:00.
      But if the request is created after 20th day of month 24 then
      those will be handled immediately
      */
   IF ldaProrateRequest >=
         DATE(1,MONTH(ldaMonth22Date),YEAR(ldaMonth22Date)) AND
      ldaProrateRequest < 
         DATE(21,MONTH(ldaMonth24Date),YEAR(ldaMonth24Date)) THEN
      /* handle it on 21st day of month 24 at 00:00 */
      ldContractActivTS = fMake2Dt(
         DATE(21,MONTH(ldaMonth24Date),YEAR(ldaMonth24Date)),0).
   ELSE
   IF liMonthsElapsed < 21 THEN 
      RETURN appl_err("ERROR:Q25 Less than 22 months have passed").
   ELSE
      /* Handle it immediately */
      ldContractActivTS = fMakeTS().
   
   liCreated = fPCActionRequest(MobSub.MsSeq,
      DCCLI.DCEvent,
      "act",
      ldContractActivTS,
      TRUE, /* create fees */
      {&REQUEST_SOURCE_NEWTON},
      "",
      0,
      FALSE,
      "",
      0, /* payterm residual fee */
      liper_contract_id, /* Periodical Contract-ID */
      OUTPUT lcResult).   
      
   IF liCreated = 0 THEN
      RETURN appl_err(SUBST("ERROR:Q25 Request creation failed: &1",
                            lcResult)).
   
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
   
   ASSIGN
      /* Where to get this value from? hugo.lujan */
      lcTextSMS = "Customer chose to have the Quota 25 extension".
      lcMSISDN = MobSub.CLI.
      
   /* Send an SMS to customer if he selects Quota 25 extension */
   RUN create_cust_sms (
      INPUT MobSub.CustNum,
      INPUT  lcTextSMS,
      INPUT  lcMSISDN,
      OUTPUT lcResult
      ).
   /* Create a memo in TMS */
      CREATE Memo.
      ASSIGN
          Memo.CreStamp  = {&nowTS}
          Memo.Brand     = gcBrand
          Memo.HostTable = ""
          Memo.KeyValue  = STRING(MobSub.MsSeq)
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = katun
          Memo.MemoTitle = lcmemo_title
          Memo.MemoText  = lcmemo_content
          Memo.CustNum   = MobSub.CustNum.
   
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