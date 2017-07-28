{Syst/tmsconst.i}
{Func/log.i}
{Func/date.i}
{Mc/orderfusion.i}
{Func/memo.i}
{Func/cparam2.i}

DEF INPUT PARAM piMessageSeq AS INT NO-UNDO.

DEF VAR liCount                AS INTE NO-UNDO.
DEF VAR lcError                AS CHAR NO-UNDO. 
DEF VAR lcResultCode           AS CHAR NO-UNDO. 
DEF VAR lcResultDesc           AS CHAR NO-UNDO. 
DEF VAR lcFixedNumber          AS CHAR NO-UNDO. 
DEF VAR lcTPDeviceProviderList AS CHAR NO-UNDO.

FUNCTION fCreateOrderForServiceProvider RETURN CHAR
    (INPUT  iiOrderId    AS INTE,
     OUTPUT ocResultCode AS CHAR,
     OUTPUT ocResultDesc AS CHAR):

    RETURN "".

END FUNCTION.   

FIND FusionMessage EXCLUSIVE-LOCK WHERE FusionMessage.MessageSeq = piMessageSeq NO-ERROR.
IF NOT AVAIL FusionMessage THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,"FusionMessage not found").

IF FusionMessage.MessageType NE {&THIRDPARTY_DEVICE_LOGISTICS} THEN
   RETURN SUBST("Incorrect message type: &1", FusionMessage.MessageType).

FIND FIRST Order NO-LOCK WHERE Order.Brand = Syst.Parameters:gcBrand AND Order.OrderId = FusionMessage.OrderID NO-ERROR.
IF NOT AVAIL Order THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,"Order not found").

FIND OrderFusion EXCLUSIVE-LOCK WHERE OrderFusion.Brand = Syst.Parameters:gcBrand AND OrderFusion.OrderID = FusionMessage.OrderID NO-ERROR.
IF NOT AVAIL OrderFusion THEN
   RETURN fFusionMessageError(BUFFER FusionMessage, "OrderFusion not found").

FOR EACH OrderTPService WHERE OrderTPService.OrderId = OrderId NO-LOCK BREAK BY OrderTPService.Type:
    
    IF FIRST-OF (OrderTPService.Type) THEN
    DO:
        CASE OrderTPService.Type:
            WHEN "" THEN 
            DO:
                /*
                lcError = fInitConnection().
                IF lcError NE "" THEN 
                    RETURN lcError.
                */    
                RUN pProcessServiceProviderRequests(ENTRY(liCount,lcTPDeviceProviderList),Order.OrderId).
            END.
            OTHERWISE
            DO:
                RETURN "Service Provider '" + ENTRY(liCount,lcTPDeviceProviderList) + "' implementation missing." .
            END.
        END CASE.
    END.
    
END.

RETURN "OK".

PROCEDURE pProcessServiceProviderRequests:
    DEFINE INPUT PARAMETER icProvider AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER iiOrderId  AS INTE NO-UNDO.

    lcError = fCreateOrderForServiceProvider(iiOrderId,OUTPUT lcResultCode,OUTPUT lcResultDesc).

    IF lcError EQ "OK" THEN 
    DO:
       ASSIGN
          FusionMessage.UpdateTS       = OrderFusion.UpdateTS
          FusionMessage.MessageStatus  = {&FUSIONMESSAGE_STATUS_HANDLED}
          FusionMessage.ResponseCode   = lcResultCode 
          FusionMessage.AdditionalInfo = lcResultDesc.
    END.
    ELSE 
    DO:
       ASSIGN
          FusionMessage.UpdateTS       = fMakeTS()
          FusionMessage.MessageStatus  = {&FUSIONMESSAGE_STATUS_ERROR}
          FusionMessage.ResponseCode   = (IF lcResultCode > "" THEN lcResultCode ELSE "ERROR")
          FusionMessage.AdditionalInfo = (IF lcResultDesc > "" THEN lcResultDesc ELSE lcError).
        
       RETURN SUBST("&1, &2, &3", lcError, lcResultCode, lcResultDesc).
    END.

    RETURN "".

END PROCEDURE.

FINALLY:
   xmlrpc_finalize().
END.


