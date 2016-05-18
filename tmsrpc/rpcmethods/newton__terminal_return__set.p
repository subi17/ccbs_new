/* *
 * Update returned terminals
 *
 * @input   struct;mandatory
            q25_struct;struct;mandatory
            memo_struct;struct;optional

 * @q25_struct   imei;string;mandatory
                 orderid;integer;mandatory
                 bill_code;string;mandatory
                 msisdn;string;mandatory
                 device_start;boolean;optional
                 device_screen;boolean;optional
                 salesman;string;mandatory
                 terminal_type;string;mandatory
                 envelope_number;string;optional
                 q25_contract_id;string;optional
                 return_channel;string;mandatory

 * @memo_struct  title;string;mandatory
                 content;string;mandatory

 * @output success;boolean
 
 */

/* YPR-2747 */

{xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
katun = "NewtonRPC".
gcBrand = "1".
{Syst/tmsconst.i}
{Syst/eventval.i}
{Func/timestamp.i}
{Mc/dpmember.i}
{Func/coinv.i}
{Func/msreqfunc.i}
{Func/fcreditreq.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {Func/lib/eventlog.i}
   DEFINE VARIABLE lhTermReturn AS HANDLE NO-UNDO.
   lhTermReturn = BUFFER TermReturn:HANDLE.
   RUN StarEventInitialize(lhTermReturn).
END.

DEF VAR top_struct    AS CHAR NO-UNDO.
DEF VAR top_struct_fields    AS CHAR NO-UNDO.

DEF VAR lhBuff           AS HANDLE NO-UNDO.
DEF VAR lcIMEI           AS CHAR   NO-UNDO.
DEF VAR liOrderId        AS INT    NO-UNDO.
DEF VAR lcBillCode       AS CHAR   NO-UNDO.
DEF VAR lcMSISDN         AS CHAR   NO-UNDO.
DEF VAR llDeviceStart    AS LOG    NO-UNDO INIT ?.
DEF VAR llDeviceScreen   AS LOG    NO-UNDO INIT ?.
DEF VAR lcSalesman       AS CHAR   NO-UNDO.
DEF VAR lcTerminalType   AS CHAR   NO-UNDO.
DEF VAR lcEnvelopeNumber AS CHAR   NO-UNDO.
DEF VAR ldReturnTS       AS DEC    NO-UNDO.
DEF VAR lcResult         AS CHAR   NO-UNDO.
DEF VAR liRequest        AS INT    NO-UNDO.
DEF VAR lcMemoTitle      AS CHAR   NO-UNDO.
DEF VAR lcMemoText       AS CHAR   NO-UNDO.
DEF VAR ldaMonth22       AS DATE   NO-UNDO. 
DEF VAR ldeMonth22       AS DEC    NO-UNDO. 
DEF VAR llRenewalOrder   AS LOG    NO-UNDO. 
DEF VAR lcQ25ContractID  AS CHAR   NO-UNDO.
DEF VAR lcOrigKatun      AS CHAR   NO-UNDO.
DEF VAR lcReturnChannel  AS CHAR   NO-UNDO.
DEF VAR llTermAccepted   AS LOG    NO-UNDO.
DEF VAR llCreateMemo     AS LOG    NO-UNDO.
DEF VAR lcError          AS CHAR NO-UNDO.

DEF VAR pcQ25Struct       AS CHARACTER NO-UNDO. /* Quota 25 input struct */
DEF VAR lcQ25Struct       AS CHARACTER NO-UNDO.

/* memo_struct */
DEF VAR lcmemo_title       AS CHARACTER NO-UNDO. /* Memo Title */
DEF VAR lcmemo_content     AS CHARACTER NO-UNDO. /* Memo Content */
DEF VAR pcmemoStruct       AS CHARACTER NO-UNDO. /* Memo input struct */
DEF VAR lcmemoStruct       AS CHARACTER NO-UNDO.

DEF BUFFER bDCCLI FOR DCCLI.
DEF BUFFER bOrder FOR Order.

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.
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

lcQ25Struct = validate_request(pcQ25Struct, 
   "imei!,orderid!,bill_code!,msisdn!,device_start,device_screen,salesman!,terminal_type!,envelope_number,q25_contract_id,return_channel!").
IF gi_xmlrpc_error NE 0 THEN RETURN.
IF lcQ25Struct EQ ? THEN RETURN.

ASSIGN
   lcIMEI            = get_string(pcQ25Struct,"imei")
   liOrderId         = get_pos_int(pcQ25Struct,"orderid")
   lcBillCode        = get_string(pcQ25Struct,"bill_code")
   lcMSISDN          = get_string(pcQ25Struct,"msisdn")
   llDeviceStart     = get_bool(pcQ25Struct,"device_start") WHEN 
                       LOOKUP("device_start", lcQ25Struct) > 0
   llDeviceScreen    = get_bool(pcQ25Struct,"device_screen") WHEN 
                       LOOKUP("device_screen", lcQ25Struct) > 0
   lcSalesman        = get_string(pcQ25Struct,"salesman")
   lcTerminalType    = get_string(pcQ25Struct,"terminal_type")
   lcEnvelopeNumber  = get_string(pcQ25Struct,"envelope_number") WHEN
                       LOOKUP("envelope_number", lcQ25Struct) > 0
   lcQ25ContractId   = get_string(pcQ25Struct,"q25_contract_id") WHEN
                       LOOKUP("q25_contract_id", lcQ25Struct) > 0  
   lcReturnChannel   = get_string(pcQ25Struct,"return_channel")                 
   ldReturnTS        = fMakeTS().

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF lcReturnChannel = "Newton" AND pcmemoStruct > "" THEN DO:

      lcmemoStruct = validate_request(pcmemoStruct, "title!,content!").
      IF lcmemoStruct EQ ? THEN RETURN.

      ASSIGN
         lcmemo_title = get_string(pcmemoStruct, "title")
            WHEN LOOKUP("title", lcmemoStruct) > 0
         lcmemo_content = get_string(pcmemoStruct, "content")
            WHEN LOOKUP("content", lcmemoStruct) > 0.

      IF gi_xmlrpc_error NE 0 THEN RETURN.

END.

IF LENGTH(lcIMEI,"CHARACTER") NE 15 THEN
   RETURN appl_err("IMEI code doesn't contain 15 characters").

IF (llDeviceStart  = ? AND llDeviceScreen <> ?) OR
   (llDeviceScreen = ? AND llDeviceStart  <> ?) THEN
   RETURN appl_err("Both should be NULL for Basic screening").
   
FIND Order NO-LOCK WHERE
     Order.Brand   = gcBrand AND
     Order.OrderId = liOrderId NO-ERROR.
IF NOT AVAILABLE Order THEN
   RETURN appl_err("Unknown order").

FIND FIRST TermReturn NO-LOCK WHERE
           TermReturn.OrderId = liOrderId USE-INDEX OrderId NO-ERROR.

IF AVAIL TermReturn AND
         TermReturn.IMEI           = lcIMEI AND
         TermReturn.BillCode       = lcBillCode AND
         TermReturn.MSISDN         = lcMSISDN AND
         TermReturn.DeviceStart    = llDeviceStart AND
         TermReturn.DeviceScreen   = llDeviceScreen AND
         TermReturn.Salesman       = lcSalesman AND
         TermReturn.TerminalType   = lcTerminalType AND
         TermReturn.EnvelopeNumber = lcEnvelopeNumber AND
         TermReturn.ContractId     = lcQ25ContractId AND
         TermReturn.ReturnChannel  = lcReturnChannel THEN DO:
   add_boolean(response_toplevel_id, "", true).
   RETURN.
END.

ASSIGN llTermAccepted = FALSE
       llCreateMemo   = FALSE.

IF (llDeviceStart AND llDeviceScreen) OR
   (llDeviceStart = ? AND llDeviceScreen = ?) THEN DO:

   FIND MobSub NO-LOCK WHERE
        MobSub.MsSeq = Order.MsSeq NO-ERROR.
   IF NOT AVAILABLE MobSub THEN
      RETURN appl_err("Unknown subscription").
   
   FIND SingleFee USE-INDEX Custnum WHERE
        SingleFee.Brand       = gcBrand AND
        SingleFee.Custnum     = MobSub.CustNum AND
        SingleFee.HostTable   = "Mobsub" AND
        SingleFee.KeyValue    = STRING(MobSub.MsSeq) AND
        SingleFee.OrderId     = Order.OrderId AND
        SingleFee.CalcObj     = "RVTERM" NO-LOCK NO-ERROR.

   IF NOT AVAILABLE SingleFee THEN
      RETURN appl_err("Residual fee not found").
   
   IF SingleFee.SourceTable NE "DCCLI" THEN
      RETURN appl_err("Residual fee is not linked with installment contract").
      
   FIND bDCCLI NO-LOCK WHERE    
        bDCCLI.PerContractId = INT(SingleFee.SourceKey) AND
        bDCCLI.MsSeq = Mobsub.MsSeq AND
        bDCCLI.DCEvent BEGINS "PAYTERM" NO-ERROR.

   IF NOT AVAIL bDCCLI THEN RETURN
      appl_err("Installment contract not found").

   ldaMonth22 = ADD-INTERVAL(bDCCLI.ValidFrom,22,"months":U).
   ldaMonth22 = DATE(MONTH(ldaMonth22),1,YEAR(ldaMonth22)).
   ldeMonth22 = fMake2Dt(ldaMonth22,0).

   IF ldaMonth22 > TODAY THEN
      RETURN appl_err("Installment contract has been active less than 22 months").

   FOR EACH bOrder NO-LOCK WHERE
            bOrder.MsSeq = MobSub.MsSeq AND
            bOrder.OrderType = 2 AND
            bOrder.CrStamp >= ldeMonth22:

      IF LOOKUP(bOrder.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 THEN NEXT.

      IF bOrder.StatusCode EQ {&ORDER_STATUS_DELIVERED} AND
         CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                        MsRequest.MsSeq = MobSub.MsSeq AND
                        MsRequest.ReqType = {&REQTYPE_REVERT_RENEWAL_ORDER} AND
                        MsRequest.ReqStatus NE {&REQUEST_STATUS_CANCELLED} AND
                        MsRequest.ReqIParam1 EQ bOrder.OrderID) THEN NEXT.

      llRenewalOrder = TRUE.
      LEAVE.
   END.

   /* If customer try to return terminal after Q25 extension request
      (ongoing or Done) without renewal order then don't allow
      terminal return.  */
      
   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.MsSeq      EQ Mobsub.MsSeq AND
              MsRequest.ReqType    EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
              LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0 AND
              MsRequest.ReqIParam3 EQ bDCCLI.PerContractID AND
              MsREquest.REqcparam3 EQ "RVTERM12" NO-ERROR.

   IF NOT llRenewalOrder THEN DO:

      FOR EACH DCCLI NO-LOCK WHERE
               DCCLI.Brand   EQ gcBrand AND
               DCCLI.DCEvent EQ "RVTERM12" AND
               DCCLI.MsSeq   EQ MobSub.MsSeq AND
               DCCLI.Validto >= TODAY,
          EACH FixedFee NO-LOCK WHERE
               FixedFee.Brand = gcBrand AND
               FixedFee.Custnum = MobSub.Custnum AND
               FixedFee.HostTable = "MobSub" AND
               Fixedfee.KeyValue = STRING(MobSub.MsSeq) AND
               Fixedfee.BillCode BEGINS "RVTERM" AND
               FixedFee.SourceTable EQ "DCCLI" AND
               FixedFee.SourceKey EQ STRING(DCCLI.PerContractId):

        IF FixedFee.OrderID <= 0 OR
           FixedFee.OrderId EQ SingleFee.OrderID THEN
           RETURN appl_err("Active Q25 extension without renewal order").
      END.

      IF AVAIL MsRequest THEN
         RETURN appl_err("Pending Q25 extension without renewal order").
   END.
   
   /* If Quota 25 is already billed then create a "credit note" with equivalent amount.
      But if Quota 25 is not billed then create a discount (RVTERMDT3) equivalent to 
      Quota 25 fee with same period date. */
   IF SingleFee.Billed AND
      NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                         Invoice.InvNum = SingleFee.InvNum AND
                         Invoice.InvType = {&INV_TYPE_TEST}) THEN DO:

      FOR FIRST Invoice NO-LOCK WHERE
                Invoice.InvNum = SingleFee.InvNum AND
                Invoice.InvType = 1,
          FIRST SubInvoice NO-LOCK WHERE
                Subinvoice.InvNum = Invoice.InvNum AND
                Subinvoice.MsSeq = MobSub.MsSeq,
           EACH InvRow NO-LOCK WHERE
                InvRow.InvNum = Invoice.InvNum AND
                InvRow.SubInvNum = SubInvoice.SubInvNum AND
                InvRow.BillCode = SingleFee.BillCode AND
                InvRow.CreditInvNum = 0 AND
                InvRow.Amt >= SingleFee.Amt:
         
         IF InvRow.OrderId > 0 AND
            SingleFee.OrderID > 0 AND
            InvRow.OrderId NE SingleFee.OrderId THEN NEXT.
      
         liRequest = fFullCreditNote(Invoice.InvNum,
                                 STRING(SubInvoice.SubInvNum),
                                 "InvRow="    + STRING(InvRow.InvRowNum) + "|" +
                                 "InvRowAmt=" + STRING(MIN(InvRow.Amt,
                                                   SingleFee.Amt)),
                                 "Correct",
                                 "2013",
                                 "",
                                 OUTPUT lcError).
         LEAVE.
      END.

      IF liRequest = 0 THEN
         RETURN appl_err("ERROR:Credit Note Creation Failed; " + lcError).

   END.
   ELSE DO:
      liRequest = fAddDiscountPlanMember(MobSub.MsSeq,
                                        "RVTERMDT3DISC",
                                        SingleFee.Amt,
                                        fPer2Date(SingleFee.BillPeriod,0),
                                        1,
                                        SingleFee.OrderId, /* Q25 OrderId */
                                        OUTPUT lcResult).

      IF liRequest NE 0 THEN
         RETURN appl_err("ERROR:Discount creation failed; " + lcResult).

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

         fCloseDiscount(DiscountPlan.DPRuleId,
                        MobSub.MsSeq,
                        DPMember.ValidFrom - 1,
                        FALSE). /* clean event logs */
      END.
   END.

   IF AVAILABLE MsRequest AND
      MsRequest.ReqStatus EQ {&REQUEST_STATUS_NEW} THEN
      fReqStatus(4,"Cancelled by Terminal Return").

   llTermAccepted = TRUE.

END. /* IF llDeviceStart AND llDeviceScreen THEN DO: */

CREATE TermReturn.
ASSIGN TermReturn.IMEI           = lcIMEI
       TermReturn.OrderId        = liOrderId
       TermReturn.BillCode       = lcBillCode
       TermReturn.MSISDN         = lcMSISDN
       TermReturn.DeviceStart    = llDeviceStart
       TermReturn.DeviceScreen   = llDeviceScreen
       TermReturn.Salesman       = lcSalesman
       TermReturn.TerminalType   = lcTerminalType
       TermReturn.EnvelopeNumber = lcEnvelopeNumber
       TermReturn.ContractId     = lcQ25ContractId
       TermReturn.ReturnChannel  = lcReturnChannel
       TermReturn.ReturnTS       = ldReturnTS.

IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTermReturn).

IF lcReturnChannel = "Newton" THEN DO:
   ASSIGN lcMemoTitle  = lcmemo_title
          lcMemoText   = lcmemo_content
          llCreateMemo = TRUE WHEN llTermAccepted.
END.
ELSE DO:
   llCreateMemo = TRUE.
   IF llTermAccepted THEN ASSIGN 
      lcMemoTitle = "Devolución en tienda aceptada"
      lcMemoText  = lcResult.
   ELSE ASSIGN 
      lcMemoTitle = "Devolución en tienda  denegada"
      lcMemoText  = lcResult.
END.

lcOrigKatun = katun.
katun =  "VISTA_" + lcSalesman.

IF llCreateMemo THEN
   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                    "MobSub",
                    STRING(Order.MsSeq),
                    Order.CustNum,
                    lcMemoTitle,
                    lcMemoText).

katun = lcOrigKatun.

RELEASE TermReturn.

add_boolean(response_toplevel_id, "", true).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   fCleanEventObjects().
END.

