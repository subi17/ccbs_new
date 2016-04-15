/**
* Add new installment contract
*
* @input          struct;mandatory;payterm_struct
                  struct;mandatory;memo_struct

* @payterm_struct username;string;mandatory;person who requests the change
                  msseq;int;mandatory;subscription id
                  payterm_contract;string;mandatory;installment contract id (e.g. PAYTERM24_15)
                  residual_value;double;optional;residual fee
                  order_id;integer;mandatory
                  imei;string;optional
                  terminal_billing_code;string;optional

* @memo_struct    title;string;mandatory
                  content;string;mandatory

* @output         boolean;true
*/

{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".
{timestamp.i}
{tmsconst.i}
{fmakemsreq.i}
{fsubsterminal.i}

DEF VAR pcPayTermStruct   AS CHARACTER   NO-UNDO.
DEF VAR pcMemoStruct      AS CHARACTER   NO-UNDO.

DEF VAR liMsSeq           AS INTEGER     NO-UNDO.
DEF VAR lcNewPayterm      AS CHARACTER   NO-UNDO.
DEF VAR ldeResidualValue  AS DECIMAL     NO-UNDO. 
DEF VAR liCreated         AS INTEGER     NO-UNDO.
DEF VAR lcResult          AS CHARACTER   NO-UNDO.
DEF VAR lcStruct          AS CHARACTER   NO-UNDO.
DEF VAR lcMemoTitle       AS CHARACTER   NO-UNDO.
DEF VAR lcMemoContent     AS CHARACTER   NO-UNDO.

DEF VAR liOrderId         AS INTEGER     NO-UNDO.
DEF VAR lcIMEI            AS CHARACTER   NO-UNDO.
DEF VAR lcBillCode        AS CHARACTER   NO-UNDO.
DEF VAr liTerminalID      AS INTEGER     NO-UNDO.

DEF VAR i AS INT NO-UNDO. 

IF validate_request(param_toplevel_id, "struct,struct") EQ ? THEN RETURN.

pcPayTermStruct = get_struct(param_toplevel_id, "0").
pcMemoStruct    = get_struct(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcPayTermStruct,"username!,msseq!,payterm_contract!,residual_value,order_id!,imei,terminal_billing_code").
IF gi_xmlrpc_error NE 0 THEN RETURN.

validate_request(pcMemoStruct,"title!,content!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN 
   liMsSeq           = get_pos_int(pcPayTermStruct, "msseq")
   katun             = "VISTA_" + get_nonempty_string(pcPayTermStruct, "username")
   lcNewPayterm      = get_nonempty_string(pcPayTermStruct, "payterm_contract")
   ldeResidualValue  = get_double(pcPayTermStruct, "residual_value") WHEN
                       LOOKUP("residual_value",lcStruct) > 0
   liOrderId         = get_pos_int(pcPayTermStruct, "order_id")
   lcIMEI            = get_string(pcPayTermStruct, "imei") WHEN
                       LOOKUP("imei",lcStruct) > 0
   lcBillCode        = get_string(pcPayTermStruct, "terminal_billing_code") WHEN
                       LOOKUP("terminal_billing_code",lcStruct) > 0
   lcMemoTitle       = get_string(pcMemoStruct, "title")
   lcMemoContent     = get_string(pcMemoStruct, "content").

IF gi_xmlrpc_error NE 0 THEN RETURN.

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
   RETURN appl_err("Installment contract type is not valid").

FOR EACH DCCLI NO-LOCK WHERE
         DCCLI.MsSeq = MobSub.MsSeq AND
         DCCLI.DCEvent BEGINS "PAYTERM" AND
         DCCLI.ValidTo >= TODAY:
   i = i + 1.
END.

IF i >= 2 THEN RETURN appl_err("Cannot add more installments").

liCreated = fPCActionRequest(MobSub.MsSeq,
                             DayCampaign.DCEvent,
                             "act",
                             fMakeTS(),
                             TRUE, /* create fees */
                             {&REQUEST_SOURCE_NEWTON},
                             "",
                             0,
                             FALSE,
                             "",
                             ldeResidualValue,
                             0,
                             OUTPUT lcResult).

IF liCreated = 0 THEN
   RETURN appl_err(SUBST("Request creation failed: &1",
                         lcResult)).

FIND FIRST MsRequest EXCLUSIVE-LOCK WHERE
           MsRequest.MsRequest EQ liCreated NO-ERROR.
IF AVAILABLE MsRequest THEN ASSIGN
   MsRequest.ReqIParam1 = liOrderId.

RELEASE MsRequest.

FIND FIRST Order NO-LOCK WHERE
           Order.Brand = gcBrand AND
           Order.OrderId = liOrderId NO-ERROR.
IF NOT AVAILABLE Order THEN
   RETURN appl_err("Order not found").

/* Create SubsTerminal for Sim Only order */
IF lcIMEI > "" THEN DO:

   IF lcBillCode = "" THEN RETURN appl_err("Missing Terminal Billing Code").

   FIND FIRST SubsTerminal NO-LOCK WHERE
              SubsTerminal.Brand = gcBrand AND
              SubsTerminal.OrderId = Order.OrderId AND
              SubsTerminal.TerminalType = {&TERMINAL_TYPE_PHONE} NO-ERROR.

   IF AVAILABLE SubsTerminal THEN RETURN appl_err("Not Sim Only order").
   ELSE DO:

      FIND LAST SubsTerminal USE-INDEX TerminalID NO-LOCK NO-ERROR.
      IF AVAILABLE SubsTerminal THEN liTerminalID = SubsTerminal.TerminalID + 1.
      ELSE liTerminalID = 1.

      CREATE SubsTerminal.

      REPEAT:

         SubsTerminal.TerminalID = liTerminalID NO-ERROR.

         VALIDATE SubsTerminal NO-ERROR.

         IF ERROR-STATUS:ERROR OR SubsTerminal.TerminalID = 0 THEN DO:
            liTerminalID = liTerminalID + 1.
            NEXT.
         END.
         ELSE LEAVE.
      END.

      ASSIGN
         SubsTerminal.Brand         = gcBrand
         SubsTerminal.OrderId       = Order.OrderId
         SubsTerminal.IMEI          = lcIMEI
         SubsTerminal.MsSeq         = MobSub.MsSeq
         SubsTerminal.PurchaseTS    = Order.CrStamp
         SubsTerminal.TerminalType  = {&TERMINAL_TYPE_PHONE}
         SubsTerminal.BillCode      = lcBillCode.
   END.

END. /* IF lcIMEI > "" THEN DO: */

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
