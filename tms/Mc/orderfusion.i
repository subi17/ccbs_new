&IF "{&ORDERFUSION_I}" NE "YES"
&THEN
&GLOBAL-DEFINE ORDERFUSION_I YES

{Syst/tmsconst.i}

&GLOBAL-DEFINE MASMOVIL_ERROR_ADAPTER_PARSING "1"
&GLOBAL-DEFINE MASMOVIL_ERROR_ADAPTER_NETWORK "2"
&GLOBAL-DEFINE MASMOVIL_ERROR_MASMOVIL "3"
&GLOBAL-DEFINE MASMOVIL_RETRY_ERROR_CODES "APIKIT-00404,APIKIT-00405,APIKIT-00406,APIKIT-00415,WO-10000000,ESB-99999999,NU-00000001,500-NO_RESULT_CODE"


FUNCTION fFusionMessageError RETURNS CHAR
 (BUFFER ibFusionMessage FOR FusionMessage,
  icErrorDesc AS CHAR):

   FIND CURRENT ibFusionMessage EXCLUSIVE-LOCK.
         
   ASSIGN
      ibFusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      ibFusionMessage.AdditionalInfo = icErrorDesc
      ibFusionMessage.UpdateTS = Func.Common:mMakeTS().

   RELEASE ibFusionMessage.

   RETURN icErrorDesc.
END.

FUNCTION fTPServiceError RETURNS CHAR
    (BUFFER TPService FOR TPService,
     icErrorDesc AS CHAR):
   
   DEFINE BUFFER bf_TPService FOR TPService.

   FIND FIRST bf_TPService WHERE bf_TPService.MsSeq   = TPService.MsSeq   AND 
                                 bf_TPService.ServSeq = TPService.ServSeq EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   IF AVAIL bf_TPService THEN    
      ASSIGN 
          bf_TPService.ServStatus = {&STATUS_ERROR}   
          bf_TPService.TermReason = icErrorDesc
          bf_TPService.UpdateTS   = Func.Common:mMakeTS().
   
   RETURN "".

END FUNCTION.
   

FUNCTION fCanRetryMasmovilMessage RETURNS LOGICAL
  (icError AS CHAR,
   icResultCode AS CHAR,
   icResultDesc AS CHAR):
/* automatic resending check */
   IF (icError BEGINS "NW_ERROR" AND 
       (LOOKUP(icResultCode,"1,2,3") = 0 OR
        icResultDesc EQ "Could not parse JSON")) /* YTS-10682 */
      OR
      (icResultCode EQ {&MASMOVIL_ERROR_ADAPTER_NETWORK} OR
      (icResultCode EQ {&MASMOVIL_ERROR_MASMOVIL} AND
       LOOKUP(icResultDesc,{&MASMOVIL_RETRY_ERROR_CODES}) > 0))
      OR
      LOOKUP(icResultCode,{&MASMOVIL_RETRY_ERROR_CODES}) > 0 THEN RETURN TRUE.

   RETURN FALSE.
END.

FUNCTION fCanRetryFusionMessage RETURNS LOGICAL
 (BUFFER ibFusionMessage FOR FusionMessage,
  icError AS CHAR,
  icResultCode AS CHAR,
  icResultDesc AS CHAR):

  DEF BUFFER bFusionMessage FOR FusionMessage.
  DEF VAR liMaxRetry AS INT NO-UNDO. 
  DEF VAR liCount AS INT NO-UNDO. 

  liMaxRetry = Syst.Parameters:geti("OnlineRetryMax","Masmovil").

  IF liMaxRetry EQ 0 THEN RETURN FALSE.

   IF fCanRetryMasmovilMessage(
      icError,
      icResultCode,
      icResultDesc) THEN DO:

      FOR EACH bFusionMessage NO-LOCK WHERE
               bFusionMessage.OrderID = ibFusionMessage.OrderID AND
               bFusionMessage.MessageType = ibFusionMessage.MessageType AND
               ROWID(bFusionMessage) NE ROWID(ibFusionMessage):
         liCount = liCount + 1.
      END.

      IF liCount < liMaxRetry THEN RETURN TRUE.
   END.

   RETURN FALSE.

END.

FUNCTION _fCreateFusionMessage RETURNS LOGICAL
 (iiOrderID AS INT,
  icMessageType AS CHAR):

   DEF BUFFER Order FOR Order.
   DEF BUFFER CLIType FOR CLIType.

   DEF VAR lcPrefix AS CHAR NO-UNDO. 
   DEF VAR lcOrderType AS CHAR NO-UNDO. 

   FIND Order NO-LOCK WHERE
        Order.Brand = Syst.Var:gcBrand AND
        Order.OrderID = iiOrderID.

   FIND CLIType NO-LOCK WHERE
        CLIType.CLIType = Order.CLiType.

   IF icMessageType EQ {&FUSIONMESSAGE_TYPE_CANCEL_ORDER} THEN
      lcPrefix = "Cancelación".
   ELSE lcPrefix = "Alta".

   IF CLIType.FixedLineType EQ 1 THEN
      lcOrderType = "xDSL".
   ELSE IF CLIType.FixedLineType EQ 2 THEN
      lcOrderType = "FTTH".

   /* NEBACO-47 */
   IF Order.Clitype MATCHES "CONTFHNB*" THEN 
      lcOrderType = lcOrderType + " NEBA".

   lcOrderType = lcOrderType + " + VOIP".

   CREATE FusionMessage.
   ASSIGN
      FusionMessage.MessageSeq = NEXT-VALUE(FusionMessageSeq)
      FusionMessage.OrderID = iiOrderID
      FusionMessage.MsSeq = Order.Msseq WHEN AVAIL Order
      FusionMessage.CreatedTS = Func.Common:mMakeTS()
      FusionMessage.MessageType = icMessageType
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_NEW}
      FusionMessage.Source = {&FUSIONMESSAGE_SOURCE_TMS}
      FusionMessage.OrderType = lcPrefix + " " + lcOrderType
      FusionMessage.UpdateTS = FusionMessage.CreatedTS.
END.

FUNCTION fCreateFusionReserveNumberMessage RETURNS LOGICAL
 (iiOrderID AS INT,
  OUTPUT ocError AS CHAR):

   DEF BUFFER OrderFusion FOR OrderFusion.

   FIND OrderFusion NO-LOCK WHERE
        OrderFusion.Brand = Syst.Var:gcBrand AND
        OrderFusion.OrderID = iiOrderId NO-ERROR.
   IF NOT AVAIL OrderFusion THEN DO:
      ocError = "ERROR:Order data not found".
      RETURN FALSE.
   END.

   IF OrderFusion.FusionStatus NE {&FUSION_ORDER_STATUS_NEW} THEN DO:
      ocError = SUBST("ERROR:Incorrect fusion status &1", 
                OrderFusion.FusionStatus).
      RETURN FALSE.
   END.

   IF CAN-FIND(FIRST FusionMessage NO-LOCK WHERE
         FusionMessage.OrderID = OrderFusion.OrderID AND
         FusionMessage.MessageType = {&FUSIONMESSAGE_TYPE_RESERVE_NUMBER} AND
         FusionMessage.MessageStatus EQ {&FUSIONMESSAGE_STATUS_NEW}) THEN DO:
      ocError = "ERROR:Ongoing message".
      RETURN FALSE.
   END.

   _fCreateFusionMessage(OrderFusion.OrderId,
                         {&FUSIONMESSAGE_TYPE_RESERVE_NUMBER}).

   RETURN TRUE.
END.

FUNCTION fCreateFusionCreateOrderMessage RETURNS LOGICAL
 (iiOrderID AS INT,
  OUTPUT ocError AS CHAR):

   DEF BUFFER OrderFusion FOR OrderFusion.

   FIND OrderFusion NO-LOCK WHERE
        OrderFusion.Brand = Syst.Var:gcBrand AND
        OrderFusion.OrderID = iiOrderId NO-ERROR.
   IF NOT AVAIL OrderFusion THEN DO:
      ocError = "ERROR:Order data not found".
      RETURN FALSE.
   END.

   IF OrderFusion.FusionStatus NE {&FUSION_ORDER_STATUS_NEW} THEN DO:
      ocError = SUBST("ERROR:Incorrect fusion status &1",
                      OrderFusion.FusionStatus).
      RETURN FALSE.
   END.

   IF OrderFusion.FixedNumber = "" OR
      OrderFusion.FixedNumber EQ ? THEN DO:
      ocError = "ERROR:Fixed number is missing".
      RETURN FALSE.
   END.

   IF CAN-FIND(FIRST FusionMessage NO-LOCK WHERE
         FusionMessage.OrderID = OrderFusion.OrderID AND
         FusionMessage.MessageType = {&FUSIONMESSAGE_TYPE_CREATE_ORDER} AND
         FusionMessage.MessageStatus EQ {&FUSIONMESSAGE_STATUS_NEW}) THEN DO:
     ocError = "ERROR:Ongoing message".
     RETURN FALSE.
   END.

   _fCreateFusionMessage(OrderFusion.OrderId,
                         {&FUSIONMESSAGE_TYPE_CREATE_ORDER}).

   RETURN TRUE.
END.

FUNCTION fCreateFusionCancelOrderMessage RETURNS LOGICAL
 (iiOrderID AS INT,
  OUTPUT ocError AS CHAR):

   DEF BUFFER OrderFusion FOR OrderFusion.

   FIND OrderFusion NO-LOCK WHERE
        OrderFusion.Brand = Syst.Var:gcBrand AND
        OrderFusion.OrderID = iiOrderId NO-ERROR.
   IF NOT AVAIL OrderFusion THEN DO:
      ocError = "ERROR:Order data not found".
      RETURN FALSE.
   END.
    
   IF OrderFusion.FusionStatus NE {&FUSION_ORDER_STATUS_INITIALIZED} THEN DO:
      ocError = SUBST("ERROR: Incorrect fusion order status: &1",
                       OrderFusion.FusionStatus).
      RETURN FALSE.
   END.

   IF OrderFusion.FixedNumber = "" OR
      OrderFusion.FixedNumber EQ ? THEN DO:
      ocError = "ERROR:Fixed number is missing".
      RETURN FALSE.
   END.

   IF CAN-FIND(FIRST FusionMessage NO-LOCK WHERE
         FusionMessage.OrderID = OrderFusion.OrderID AND
         FusionMessage.MessageType = {&FUSIONMESSAGE_TYPE_CANCEL_ORDER} AND
         FusionMessage.MessageStatus EQ {&FUSIONMESSAGE_STATUS_NEW}) THEN DO:
     ocError = "ERROR:Ongoing message".
     RETURN FALSE.
   END.

   _fCreateFusionMessage(OrderFusion.OrderId,
                         {&FUSIONMESSAGE_TYPE_CANCEL_ORDER}).

   RETURN TRUE.
END.

FUNCTION fIsFixedNumberInUse RETURNS LOGICAL
 (icFixedNumber AS CHAR,
  iiOrderID AS INT):

  DEF BUFFER OrderFusion FOR OrderFusion.
  DEF BUFFER Order FOR Order.

  IF icFixedNumber EQ ? OR icFixedNumber EQ "" THEN RETURN FALSE.

  FOR EACH OrderFusion NO-LOCK WHERE
           OrderFusion.FixedNumber = icFixedNumber AND
           OrderFusion.OrderID NE iiOrderID,
      FIRST Order NO-LOCK WHERE
            Order.Brand = OrderFusion.Brand AND
            Order.OrderID = OrderFusion.OrderID AND
     LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0:

     RETURN TRUE.
  END.
   
  RETURN CAN-FIND(FIRST MobSub NO-LOCK WHERE
                        MobSub.Brand = Syst.Var:gcBrand AND
                        MobSub.FixedNumber = icFixedNumber).
END.

&ENDIF
