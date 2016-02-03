/**
 * Update returned terminals
 *
 * @input  struct;
           imei;string;mandatory
           orderid;integer;mandatory
           bill_code;string;mandatory
           msisdn;string;mandatory
           device_start;boolean;optional
           device_screen;boolean;optional
           salesman;string;mandatory
           terminal_type;string;mandatory
           envelope_number;string;optional

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

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}
   DEFINE VARIABLE lhTermReturn AS HANDLE NO-UNDO.
   lhTermReturn = BUFFER TermReturn:HANDLE.
   RUN StarEventInitialize(lhTermReturn).
END.

DEF VAR pcStruct    AS CHAR NO-UNDO.
DEF VAR lcStruct    AS CHAR NO-UNDO.

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
DEF VAR lcMemo           AS CHAR   NO-UNDO.
DEF VAR ldaMonth22 AS DATE NO-UNDO. 
DEF VAR ldeMonth22 AS DEC NO-UNDO. 
DEF VAR llRenewalOrder AS LOG NO-UNDO. 

DEF BUFFER bDCCLI FOR DCCLI.
DEF BUFFER bOrder FOR Order.

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcStruct, "imei!,orderid!,bill_code!,msisdn!,device_start,device_screen,salesman!,terminal_type!,envelope_number").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   lcIMEI            = get_string(pcStruct,"imei")
   liOrderId         = get_pos_int(pcStruct,"orderid")
   lcBillCode        = get_string(pcStruct,"bill_code")
   lcMSISDN          = get_string(pcStruct,"msisdn")
   llDeviceStart     = get_bool(pcStruct,"device_start") WHEN LOOKUP("device_start", lcStruct) > 0
   llDeviceScreen    = get_bool(pcStruct,"device_screen") WHEN LOOKUP("device_screen", lcStruct) > 0
   lcSalesman        = get_string(pcStruct,"salesman")
   lcTerminalType    = get_string(pcStruct,"terminal_type")
   lcEnvelopeNumber  = get_string(pcStruct,"envelope_number") WHEN LOOKUP("envelope_number", lcStruct) > 0
   ldReturnTS        = fMakeTS().

IF gi_xmlrpc_error NE 0 THEN RETURN.

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
         TermReturn.EnvelopeNumber = lcEnvelopeNumber THEN DO:
   add_boolean(response_toplevel_id, "", true).
   RETURN.
END.

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
   
   IF SingleFee.Billed AND
      NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                         Invoice.InvNum = SingleFee.InvNum AND
                         Invoice.InvType = {&INV_TYPE_TEST}) THEN
      RETURN appl_err("Residual fee already billed").

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
   
   liRequest = fAddDiscountPlanMember(MobSub.MsSeq,
                                     "RVTERMDT3DISC",
                                     SingleFee.Amt,
                                     fPer2Date(SingleFee.BillPeriod,0),
                                     1,
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

   IF AVAILABLE MsRequest AND
      MsRequest.ReqStatus EQ {&REQUEST_STATUS_NEW} THEN
      fReqStatus(4,"Cancelled by Terminal Return").

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
       TermReturn.ReturnTS       = ldReturnTS.

IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTermReturn).


IF (llDeviceStart AND llDeviceScreen) OR
   (llDeviceStart = ? AND llDeviceScreen = ?) 
THEN lcMemo = "Devolución en tienda aceptada".
ELSE lcMemo = "Devolución en tienda denegada".

DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                 "MobSub",
                 STRING(Order.MsSeq),
                 Order.CustNum,
                 lcMemo,
                 lcResult).

RELEASE TermReturn.

add_boolean(response_toplevel_id, "", true).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   fCleanEventObjects().
END.

