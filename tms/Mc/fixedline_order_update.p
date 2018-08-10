{Syst/commali.i}
{Func/msreqfunc.i}
{Syst/eventval.i}
{Func/fcustdata.i}
{Mc/orderfusion.i}

DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

DEF VAR lcError AS CHAR NO-UNDO.

FIND MsRequest NO-LOCK WHERE
     MsRequest.MsRequest = iiRequest
     NO-ERROR.
     
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE {&REQTYPE_FIXEDLINE_ORDER_UPDATE} THEN RETURN "ERROR".

CASE MsRequest.ReqCParam2:
    
    WHEN {&INFLIGHT_ADDRESS_UPDATE} OR WHEN {&INFLIGHT_PHONE_NUMBER_UPDATE} THEN DO:
       fCreateFusionUpdateOrderMessage(MsRequest.ReqIParam1,
                                       MsRequest.ReqCParam2,
                                       OUTPUT lcError).
    END.
    
    OTHERWISE DO:
       fReqStatus(3,"Invalid AmendmentType").
    END.
END CASE.

IF lcError NE "" THEN
   Func.Common:mWriteMemo("Order",
                          STRING(MsRequest.ReqIParam1),
                          0,
                          "Masmovil message creation failed",
                          lcError).