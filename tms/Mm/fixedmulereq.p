/* fixedmulereq.p - YDR-2973
   Program handles fixed line termination requests.
   Functionality is removed from stc2tms because synchronous call to
   external interface is slow and it causes slowness to STC handling.
   Requests get handled by this program when request status is 
   REQUEST_STATUS_FIXED_LINE_TERMINATION 81.
   After this request returns back to status 
   REQUEST_STATUS_SUB_REQUEST_DONE 8 
   And handling continues normally in stc2tms.
   Multiple handling is blocked bu using requestparam table.
*/   

DEF INPUT PARAMETER iiMsRequest AS INT.

DEF VAR lcResult AS CHAR NO-UNDO.

FIND FIRST MSRequest WHERE
           MSRequest.MSrequest = iiMSrequest NO-LOCK NO-ERROR.

IF NOT (MsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
        MsRequest.ReqStatus EQ {&REQUEST_STATUS_FIXED_LINE_TERMINATION}) THEN
   RETURN "ERROR".

FIND Mobsub WHERE Mobsub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.

IF NOT AVAILABLE Mobsub THEN DO:
   fReqError("Mobsub not found - Fixed Line Termination").
   RETURN "ERROR".
END.

liOrderId = fFindFixedLineOrder(MSRequest.MSSeq).
IF liOrderId EQ 0 THEN 
   lcResult = "OrderID not found".
ELSE DO:

   /* This call makes synchronous termination request to MuleDB */
   lcResult = fSendFixedLineTermReqToMuleDB(liOrderId).
END.

IF lcResult NE "" THEN DO:
   /* "Fixed number termination failed" */
   Func.Common:mWriteMemo("MobSub",
                           STRING(MSrequest.MsSeq),
                           MobSub.CustNum,
                           "La baja del sevicio fijo ha fallado: ",
                            lcResult).
    fReqError(SUBST("La baja del sevicio fijo ha fallado: &1", lcResult)).
    RETURN "ERROR".
END.    

/* Coing back to original status */
fReqStatus({&REQUEST_STATUS_SUB_REQUEST_DONE},"").

RETURN "OK".
