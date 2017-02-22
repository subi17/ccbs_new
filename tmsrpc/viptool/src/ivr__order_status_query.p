/**
 * Order status query 
 *
 * @input       string;mandatory;MSISDN
 * @output      order_status;string;order status code (1,4,..)
                mnp_status;string;status code (NEW,AENV,..) of the latest MNP process of order, returned if mnp order
                order_type;int;order type (eg: 0=New,1=MNP,2=Renewal,3=MNP Rollback, 4=Fusion STC)
                order_channel;string;order channel
                order_salesman;string;order salesman id
                order_crstamp;timestamp;order creation stamp
                logistic_status;int;logistic status
 * @Exceptions  1;Order not found
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}

/* Input parameters */
DEF VAR pcCLI            AS CHAR  NO-UNDO.
DEF VAR gcBrand          AS CHAR  NO-UNDO INIT "1".
DEF VAR lcResponseStruct AS CHAR  NO-UNDO.
DEF VAR ldeCreated       AS DEC   NO-UNDO.
DEF VAR lrMNPProcess     AS ROWID NO-UNDO.
DEF VAR lcCode           AS CHAR  NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcCLI = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO Ordercanal MobSub CLI pcCLI}

FIND FIRST Order NO-LOCK WHERE 
           Order.CLI = pcCli AND
           Order.Brand = gcBrand USE-INDEX CLI_s NO-ERROR.

IF NOT AVAILABLE Order THEN RETURN appl_err("Order not found").

FOR EACH MNPSub WHERE
         MNPSub.CLI = Order.CLI NO-LOCK:

   FIND MNPProcess WHERE
        MNPProcess.MNPSeq = MNPSub.MNPSeq AND
        MNPProcess.MNPType = {&MNP_TYPE_IN} NO-LOCK NO-ERROR.

   IF NOT AVAIL MNPProcess THEN NEXT.

   IF ldeCreated < MNPProcess.CreatedTS THEN DO:
      ASSIGN
         ldeCreated = MNPProcess.CreatedTS
         lrMNPProcess = ROWID(MNPProcess).
   END.
      
END.

FIND MNPProcess WHERE
     ROWID(MNPProcess) = lrMNPProcess NO-LOCK NO-ERROR.

lcResponseStruct = add_struct(response_toplevel_id,"").
add_string(lcResponseStruct,"order_status", Order.StatusCode).
add_int(lcResponseStruct,"order_type", Order.OrderType).
add_string(lcResponseStruct,"order_channel", Order.OrderChannel).
add_string(lcResponseStruct,"order_salesman", Order.Salesman).
add_timestamp(lcResponseStruct,"order_crstamp", Order.CrStamp).

IF AVAIL MNPProcess THEN DO:

   FIND TMSCodes WHERE
        TMSCodes.TableName = "MNPProcess" AND
        TMSCodes.FieldName = "StatusCode" AND
        TMSCodes.CodeGroup = "MNP" AND
        TMSCodes.CodeValue = STRING(MNPProcess.StatusCode) NO-LOCK NO-ERROR.

   IF AVAIL TMSCodes THEN
      lcCode = (IF TMSCodes.CodeName = "AREC_CLOSED" THEN "AREC" 
                ELSE TMSCodes.CodeName).
   ELSE lccode = STRING(MNPProcess.StatusCode).
   
   add_string(lcResponseStruct, "mnp_status", lcCode).
END.

FIND FIRST OrderDelivery WHERE
           OrderDelivery.Brand = gcBrand AND
           OrderDelivery.Orderid = Order.OrderId NO-LOCK NO-ERROR.
IF AVAIL OrderDelivery THEN
   add_int(lcResponseStruct,"logistic_status", OrderDelivery.LOStatusId).


