{Syst/commali.i}
{Func/msreqfunc.i}
{Syst/eventval.i}
{Func/fcustdata.i}
{Mc/orderfusion.i}

DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

DEF VAR lcError AS CHAR NO-UNDO. 

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE {&REQTYPE_INSTALL_ADDRESS_UPDATE} THEN RETURN "ERROR".

fCreateFusionAddressChangeMessage(MsRequest.ReqIParam1,
                                  OUTPUT lcError).

IF lcError NE "" THEN 
   Func.Common:mWriteMemo("Order",
                          STRING(MsRequest.ReqIParam1),
                          0,
                          "Masmovil message creation failed",
                          lcError).

