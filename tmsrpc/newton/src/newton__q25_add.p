/**
newton__q25_add.p
* Create Quota 25 extension request in TMS

* @input    struct;mandatory
            q25_struct;struct;mandatory
            memo_struct;struct;optional

* @q25_struct     username;string;mandatory;person who requests the change
                  msseq;int;mandatory;subscription id
                  per_contract_id;int;mandatory;installment contract id (related to q25)
                  q25_contract_id;string;optional;Contract ID

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
{Func/q25functions.i}

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

DEF VAR ldaMonth22Date    AS DATE NO-UNDO.
DEF VAR ldaMonth24Date    AS DATE NO-UNDO.
DEF VAR ldaMonth25Date    AS DATE NO-UNDO. /* YDR-2220 */
DEF VAR llNewExtension    AS LOG  NO-UNDO. /* YDR-2220 */
/* Contract activation timestamp */
DEF VAR ldContractActivTS AS DECIMAL NO-UNDO.
DEF VAR ldeSMSStamp AS DEC NO-UNDO. 
DEF VAR lcSMSTxt AS CHAR NO-UNDO. 
DEF VAR ldeFeeAmount AS DEC NO-UNDO. 

DEF VAR lcQ25ContractId AS CHAR NO-UNDO.
DEF VAR lcOrigKatun AS CHAR NO-UNDO.


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

lcQ25Struct = validate_request(pcQ25Struct,"username!,msseq!,per_contract_id!,q25_contract_id").
IF lcQ25Struct EQ ? THEN RETURN.

ASSIGN
   lcusername = get_string(pcQ25Struct, "username")
      WHEN LOOKUP("username", lcQ25Struct) > 0
   limsseq = get_int(pcQ25Struct, "msseq")
      WHEN LOOKUP("msseq", lcQ25Struct) > 0
    /* Quota 25 installment contract id */
   liper_contract_id = get_int(pcQ25Struct, "per_contract_id")      
      WHEN LOOKUP("per_contract_id", lcQ25Struct) > 0
   /*Contract ID*/   
   lcQ25ContractId = get_string(pcQ25Struct, "q25_contract_id")      
      WHEN LOOKUP("q25_contract_id", lcQ25Struct) > 0.

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

FIND FIRST MobSub NO-LOCK WHERE
           MobSub.MsSeq = limsseq NO-ERROR.
           
IF NOT AVAILABLE MobSub THEN
   RETURN appl_err("Subscription not found").

FIND FIRST Customer NO-LOCK WHERE
           Customer.Custnum = MobSub.Custnum NO-ERROR.
IF NOT AVAILABLE Customer THEN
   RETURN appl_err("Customer not found").

/* Find original installment contract */   
FIND FIRST DCCLI NO-LOCK WHERE
           DCCLI.Brand   = gcBrand AND
           DCCLI.DCEvent BEGINS "PAYTERM" AND
           DCCLI.MsSeq   = MobSub.MsSeq AND 
           DCCLI.PerContractId = liper_contract_id NO-ERROR.

IF NOT AVAIL DCCLI THEN
   RETURN appl_err("Installment contract not found").

IF DCCLI.TermDate NE ? THEN
   RETURN appl_err("Installment contract terminated").
   
FIND SingleFee USE-INDEX Custnum WHERE
     SingleFee.Brand       = gcBrand AND
     SingleFee.Custnum     = MobSub.CustNum AND
     SingleFee.HostTable   = "Mobsub" AND
     SingleFee.KeyValue    = STRING(Mobsub.MsSeq) AND
     SingleFee.SourceTable = "DCCLI" AND
     SingleFee.SourceKey   = STRING(DCCLI.PerContractId) AND
     SingleFee.CalcObj     = "RVTERM" NO-LOCK NO-ERROR.

IF NOT AVAIL SingleFee THEN
   RETURN appl_err("Residual fee not found").

ASSIGN   
   ldaMonth22Date    = ADD-INTERVAL(DCCLI.ValidFrom, 22, 'months':U)
   ldaMonth22Date    = DATE(MONTH(ldaMonth22Date),1,YEAR(ldaMonth22Date))
   ldaMonth24Date    = ADD-INTERVAL(DCCLI.ValidFrom, 24, 'months':U)
   ldaMonth24Date    = DATE(MONTH(ldaMonth24Date),21,YEAR(ldaMonth24Date))
   ldaMonth25Date    = ADD-INTERVAL(DCCLI.ValidFrom, 25, 'months':U) 
   ldaMonth25Date    = DATE(MONTH(ldaMonth25Date),21,YEAR(ldaMonth25Date)).
   /* ldaMonth25Date YDR-2220 */

/* If the Quota 25 prorate request is created between 1st day of month 22
   until 20th day of month 24 then
   it should be handled on 21st day of month 24 at 00:00.
   But if the request is created after 20th day of month 24 then
   those will be handled immediately
   */
IF TODAY < ldaMonth22Date THEN
   RETURN appl_err("Q25 extension not allowed before 22th month").
ELSE IF TODAY >= ldaMonth22Date AND
   TODAY < ldaMonth24Date THEN
   /* handle it on 21st day of month 24 at 00:00 */
   ldContractActivTS = fMake2Dt(ldaMonth24Date,0).
ELSE IF TODAY >= ldaMonth24Date AND 
        TODAY < ldaMonth25Date THEN 
   ASSIGN
      ldaMonth24Date = TODAY
      ldContractActivTS = fSecOffSet(fMakeTS(),5). /* Handle it immediately */
/* YDR-2220 Quota 25 prorate request is created after 20th day of 25th month*/
ELSE 
   ASSIGN
      ldaMonth25Date = TODAY
      ldContractActivTS = fSecOffSet(fMakeTS(),5)
      llNewExtension = YES . /* Handle it immediately */

IF CAN-FIND(FIRST DCCLI NO-LOCK WHERE
                  DCCLI.Brand   EQ gcBrand AND
                  DCCLI.DCEvent EQ "RVTERM12" AND
                  DCCLI.MsSeq   EQ MobSub.MsSeq AND
                  DCCLI.ValidTo >= TODAY) THEN
   RETURN appl_err("Q25 extension already active").

IF SingleFee.OrderId > 0 THEN DO:

   FIND FIRST TermReturn NO-LOCK WHERE
              TermReturn.OrderId = SingleFee.OrderId NO-ERROR.

   IF AVAIL TermReturn AND 
          ((TermReturn.DeviceScreen = TRUE AND TermReturn.DeviceStart  = TRUE) OR 
           (TermReturn.DeviceScreen = ?    AND TermReturn.DeviceStart  = ?)) THEN
      RETURN appl_err("Already returned terminal").
END.

lcOrigKatun = katun.
/*YPR-3256*/
IF lcQ25ContractId EQ "" THEN
   katun = "VISTA_" + lcUsername.
ELSE 
   katun = "POS_" + lcUsername.

/*Request for Q25 extension*/
liCreated = fPCActionRequest(
   MobSub.MsSeq,
   "RVTERM12",
   "act",
   ldContractActivTS,
   TRUE, /* create fees */
   {&REQUEST_SOURCE_NEWTON},
   "",
   0,
   FALSE,
   "",
   0, /* payterm residual fee */
   DCCLI.PerContractId, /* Periodical Contract-ID */
   OUTPUT lcResult).   
   
IF liCreated = 0 THEN DO:
   katun = lcOrigKatun.
   RETURN appl_err(SUBST("Q25 extension request failed: &1",
                         lcResult)).
END.

FIND FIRST MSRequest WHERE
           MSRequest.MSrequest EQ liCreated EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL MsRequest THEN DO:
   MsRequest.ReqCparam4 = lcQ25ContractId. /*For dump*/
   /*For this request type (8) the field is used for Bank, not ContractID.*/
   MsRequest.ReqCparam6 = fBankByBillCode(SingleFee.BillCode). /*ybu-5247*/
END.
RELEASE MsRequest.

CASE SingleFee.BillCode:
   WHEN "RVTERM1EF" THEN
      lcSMSTxt = fGetSMSTxt("Q25ExtensionUNOE",
                            TODAY,
                            Customer.Language,
                            OUTPUT ldeSMSStamp).
   /* YPR-3565  */
/*   WHEN "RVTERMBSF" THEN
      lcSMSTxt = fGetSMSTxt("Q25ExtensionSabadell",
                            TODAY,
                            Customer.Language,
                            OUTPUT ldeSMSStamp). */
   /*YPR-4468*/  
   WHEN "RVTERMBCF" THEN
      lcSMSTxt = fGetSMSTxt("Q25ExtensionCetelem",
                            TODAY,
                            Customer.Language,
                            OUTPUT ldeSMSStamp). 
                           
   OTHERWISE 
      lcSMSTxt = fGetSMSTxt("Q25ExtensionYoigo",
                            TODAY,
                            Customer.Language,
                            OUTPUT ldeSMSStamp).
END CASE.

IF lcSMSTxt > "" THEN DO:

   ldeFeeAmount = SingleFee.Amt.

   FOR EACH DiscountPlan NO-LOCK WHERE
            DiscountPlan.Brand = gcBrand AND
           (DiscountPlan.DPRuleID = "RVTERMDT1DISC" OR
            DiscountPlan.DPRuleID = "RVTERMDT4DISC"),
       EACH DPMember NO-LOCK WHERE
            DPMember.DpID       = DiscountPlan.DpId AND
            DPMember.HostTable  = "MobSub" AND
            DPMember.KeyValue   = STRING(MobSub.MsSeq) AND
            DPMember.ValidFrom  = fPer2Date(SingleFee.BillPeriod,0) AND
            DPMember.ValidTo   >= DPMember.ValidFrom:
      ldeFeeAmount = ldeFeeAmount - DPMember.DiscValue.
   END.

   IF ldeFeeAmount > 0 THEN DO:

      ASSIGN
         /* first payment is done next month to Q25 YDR-2220*/
         lcSMSTxt = (IF NOT llNewExtension THEN 
                       REPLACE(lcSMSTxt,"#MONTHNAME",
                               lower(entry(month(ADD-INTERVAL(ldaMonth24Date, 1,
                               'months':U)),{&MONTHS_ES})))
                    ELSE
                       REPLACE(lcSMSTxt,"#MONTHNAME",
                               lower(entry(month(ADD-INTERVAL(ldaMonth25Date, 1,
                               'months':U)),{&MONTHS_ES})))
                    )
         lcSMSTxt = (IF NOT llNewExtension THEN 
                       REPLACE(lcSMSTxt,"#YEAR", STRING(YEAR(ldaMonth24Date)))
                    ELSE
                       REPLACE(lcSMSTxt,"#YEAR", STRING(YEAR(ldaMonth25Date)))
                    )
         lcSMSTxt = REPLACE(lcSMSTxt,"#AMOUNT",
               STRING(TRUNC(ldeFeeAmount / 12, 2))).

      fMakeSchedSMS2(MobSub.CustNum,
                     MobSub.CLI,
                     {&SMSTYPE_CONTRACT_ACTIVATION},
                     lcSMSTxt,
                     ldeSMSStamp,
                     "Yoigo info",
                     "").
   END.
END.

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

katun = lcOrigKatun.
add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.

