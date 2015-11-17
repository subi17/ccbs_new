/**
 * Update returned terminals
 *
 * @input  struct;
           imei;string;mandatory
           orderid;integer;mandatory
           bill_code;string;mandatory
           msisdn;string;mandatory
           device_start;boolean;mandatory
           device_screen;boolean;mandatory
           salesman;string;mandatory
           terminal_type;string;mandatory
           envelope_number;string;optional

 * @output success;boolean
 */

{xmlrpc/xmlrpc_access.i}

{commpaa.i}
katun = "NewtonRPC".
gcBrand = "1".
{tmsconst.i}
{eventval.i}
{timestamp.i}
{dpmember.i}
{coinv.i}

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
DEF VAR llDeviceStart    AS LOG    NO-UNDO.
DEF VAR llDeviceScreen   AS LOG    NO-UNDO.
DEF VAR lcSalesman       AS CHAR   NO-UNDO.
DEF VAR lcTerminalType   AS CHAR   NO-UNDO.
DEF VAR lcEnvelopeNumber AS CHAR   NO-UNDO.
DEF VAR ldReturnTS       AS DEC    NO-UNDO.
DEF VAR lcResult         AS CHAR   NO-UNDO.
DEF VAR liRequest        AS INT    NO-UNDO.

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.
lcStruct = validate_request(pcStruct, "imei!,orderid!,bill_code!,msisdn!,device_start!,device_screen!,salesman!,terminal_type!,envelope_number").

ASSIGN
   lcIMEI            = get_string(pcStruct,"imei")
   liOrderId         = get_int(pcStruct,"orderid")
   lcBillCode        = get_string(pcStruct,"bill_code")
   lcMSISDN          = get_string(pcStruct,"msisdn")
   llDeviceStart     = get_bool(pcStruct,"device_start")
   llDeviceScreen    = get_bool(pcStruct,"device_screen")
   lcSalesman        = get_string(pcStruct,"salesman")
   lcTerminalType    = get_string(pcStruct,"terminal_type")
   lcEnvelopeNumber  = get_string(pcStruct,"envelope_number")
   ldReturnTS        = fMakeTS().

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF LENGTH(lcIMEI,"CHARACTER") NE 15 THEN
   RETURN appl_err("IMEI code doesn't contain 15 characters").

IF llDeviceStart AND llDeviceScreen THEN DO:
   
   FIND Order NO-LOCK WHERE
        Order.Brand   = gcBrand AND
        Order.OrderId = liOrderId NO-ERROR.
   IF NOT AVAILABLE Order THEN
      RETURN appl_err("Unknown order").

   FIND MobSub NO-LOCK WHERE
        MobSub.MsSeq = Order.MsSeq NO-ERROR.
   IF NOT AVAILABLE MobSub THEN
      RETURN appl_err("Unknown subscription").

   IF Order.OrderType NE {&ORDER_TYPE_RENEWAL} AND
      CAN-FIND(FIRST DCCLI WHERE
                     DCCLI.MsSeq   = MobSub.MsSeq AND
                     DCCLI.DCEvent = "RVTERM12") THEN
      RETURN appl_err("Already have Q25 extension").

   FIND FIRST OrderAction WHERE
              OrderAction.Brand    = gcBrand AND
              OrderAction.OrderId  = Order.OrderId AND
              OrderAction.ItemType = "" NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE OrderAction THEN
      RETURN appl_err("Not available OrderAction").

   FIND SingleFee USE-INDEX Custnum WHERE
        SingleFee.Brand       = gcBrand AND
        SingleFee.Custnum     = MovSub.CustNum AND
        SingleFee.HostTable   = "Mobsub" AND
        SingleFee.KeyValue    = STRING(MobSub.MsSeq) AND
        SingleFee.SourceTable = "DCCLI" AND
        SingleFee.SourceKey   = OrderAction.ItemKey AND
        SingleFee.CalcObj     = "RVTERM" NO-LOCK NO-ERROR.

   IF NOT AVAILABLE SingleFee THEN
      RETURN appl_err("Discount creation failed (residual fee not found)").

   liRequest = fAddDiscountPlanMember(MobSub.MsSeq,
                                     "RVTERMDT3DISC",
                                     DEC(OrderAction.ItemParam),
                                     fPer2Date(SingleFee.BillPeriod,0),
                                     1,
                                     OUTPUT lcResult).

   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                    "MobSub",
                    STRING(MobSub.MsSeq),
                    0,
                    "OrderAction " + OrderAction.ItemType,
                    lcResult).

   IF liRequest NE 0 THEN 
      RETURN appl_err("ERROR:Discount not created; " + lcResult).

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

RELEASE TermReturn.

add_boolean(response_toplevel_id, "", true).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   fCleanEventObjects().
END.

