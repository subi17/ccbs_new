/* ----------------------------------------------------------------------
  MODULE .......: logistics_request.p
  TASK .........: Handles logistics (Dextra/Netkia) requests. YDR-499
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 15.06.12
  CHANGED ......:
  Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/msreqfunc.i}
{Syst/tmsconst.i}
{fcgi_agent/xmlrpc/xmlrpc_client.i}
{Func/forderstamp.i}

DEFINE INPUT PARAMETER iiMSrequest AS INT NO-UNDO.

DEF VAR lcConURL AS CHARACTER NO-UNDO.
DEF VAR lcMemo AS CHAR NO-UNDO. 
DEF VAR liIndex AS INT NO-UNDO. 
DEF VAR liCount AS INT NO-UNDO. 
DEF VAR ldOPChange AS DEC NO-UNDO.
DEF VAR ldOPStamp  AS DEC NO-UNDO.   /* SendToLogistics date from OrderStamp */
DEF VAR lcOPName   AS CHAR NO-UNDO.  /* Name value Dextra/Netkia to XML */

DEF BUFFER bMsRequest FOR MsRequest.

ldOPChange = fCParamDe("LOswitchover"). /* Get date when Dextra changed to Netkia */
lcConURL = fCParam("URL","urlDextra").
IF lcConURL = ? OR lcConURL = "" THEN RETURN "ERROR".

FIND MSRequest NO-LOCK WHERE 
     MSRequest.MSRequest = iiMSRequest NO-ERROR.

IF NOT AVAILABLE MsRequest OR 
                 MsRequest.ReqType NE {&REQTYPE_LOGISTICS}
   THEN RETURN.

IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

IF MsRequest.ReqCParam1 NE "Cancel" THEN DO:
   fReqError(SUBST("ERROR: Unsupported operation: &1", MsRequest.ReqCParam1)). 
   RETURN.
END.

FIND Order WHERE
     Order.Brand = gcBrand AND
     Order.OrderId = MsRequest.ReqIParam1 NO-LOCK NO-ERROR.

IF NOT AVAIL Order THEN DO:
   fReqError("ERROR: Order not found"). 
   RETURN.
END.

IF Order.StatusCode = {&ORDER_STATUS_DELIVERED} AND
   CAN-FIND(FIRST bMsRequest WHERE
                  bMsRequest.MsSeq = Order.MsSeq AND
                  bMsRequest.ReqType = {&REQTYPE_REVERT_RENEWAL_ORDER} AND
                  bMsRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
                  bMsRequest.ReqIParam1 = Order.OrderId NO-LOCK) THEN
   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                    "Order",
                    STRING(Order.OrderID),
                    0,
                    "Logistic Request",
                    "Renewal order was already cancelled").

initialize(lcConURL, 15).

RUN pSendLogisticsCancel(Order.OrderId).

/* automatic resending for NW error */
IF RETURN-VALUE BEGINS "NW_ERROR" THEN DO:

   lcMemo = MsRequest.Memo.
   
   DO WHILE TRUE:
      liIndex = INDEX(lcMemo,"NW_ERROR").
      IF liIndex > 0 THEN DO:
         lcMemo = substring(lcMemo,liIndex + 1).
         licount = licount + 1.
      END.
      ELSE LEAVE.
   END.

   IF liCount < 5 THEN DO:
      FIND CURRENT MsRequest EXCLUSIVE-LOCK.
      MsRequest.ActStamp = fOffSet(fMakeTS(), 1).
      fReqStatus(0,RETURN-VALUE).
   END.
   ELSE fReqError(RETURN-VALUE). 
END.
ELSE IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   fReqError(RETURN-VALUE). 
END.
ELSE fReqStatus(2,STRING(RETURN-VALUE)).

FINALLY:
   xmlrpc_finalize().
END.

PROCEDURE pSendLogisticsCancel:

   DEFINE INPUT PARAMETER piOrderID AS INTEGER NO-UNDO.

   DEF VAR liResponseCode AS INT NO-UNDO. 

   add_int(param_toplevel_id,"", piOrderID).

   /* Add operator name to XML based on date in OrderStamp */
   ldOPStamp = fGetOrderStamp(piOrderID,"SendToLogistics").
   IF ldOPStamp < ldOPChange THEN lcOPName = "Dextra".
   ELSE lcOPName = "Netkia".

   IF ldOPStamp EQ 0 AND
      Order.CrStamp > ldOPChange THEN lcOPName = "Netkia".

   add_string(param_toplevel_id, "operatorName ", lcOPName).

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: XML creation failed: &1", gc_xmlrpc_error). 
 
   RUN pRPCMethodCall("logistics.cancelRequest", TRUE). 

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("NW_ERROR: &1", gc_xmlrpc_error).

   liResponseCode = get_int(response_toplevel_id, "0").

   IF gi_xmlrpc_error NE 0 THEN
      RETURN SUBST("ERROR: Response parsing failed: &1", gc_xmlrpc_error).

   RETURN ("Response: " + STRING(liResponseCode)).
END.
