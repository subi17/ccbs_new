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

FIND Mobsub WHERE Mobsub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.

IF NOT AVAILABLE Mobsub THEN DO:
   fReqError("Mobsub not found - Fixed Line Termination").
   RETURN "ERROR".
END.

IF NOT fCheckMsRequestParam(MsRequest.MsRequest,
                            {&FIXED_TERMINATION_PARAM},
                            OUTPUT lcOrderId) THEN DO:
   fReqError("Request Param not found - Fixed Line Termination").
   RETURN "ERROR".
 
END.
liOrderId = INT(lcOrderId).

/* This call makes synchronous termination request to MuleDB */
lcResult = fSendFixedLineTermReqToMuleDB(liOrderId).

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

RETURN "OK".
