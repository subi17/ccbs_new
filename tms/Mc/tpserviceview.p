/* ----------------------------------------------------------------------
  MODULE .......: tpservive.p
  TASK .........: Display Third Party Service data 
  APPLICATION ..: tms
  AUTHOR .......: usha
  CREATED ......: 26.07.2017
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */
DEF INPUT PARAMETER iiOrderId     AS INT  NO-UNDO.

DEF VAR lcDoneTime       AS CHAR NO-UNDO.
DEF VAR lcCreatedTime    AS CHAR NO-UNDO.
DEF VAR lcUpdatedTime    AS CHAR NO-UNDO.
DEF VAR lcActivationTime AS CHAR NO-UNDO.

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/timestamp.i}

FIND FIRST OrderTPService NO-LOCK WHERE OrderTPService.OrderID EQ iiOrderID NO-ERROR.
IF NOT AVAIL OrderTPService THEN 
DO:
   MESSAGE "No third party service available!" VIEW-AS ALERT-BOX.
   RETURN.
END.

FORM
    "OrderID ...........:" OrderTPService.OrderID
    SKIP
    "Product ...........:" OrderTPService.Product
    SKIP
    "Service Type ......:" OrderTPService.Type
    SKIP
    "Service Provider ..:" OrderTPService.Provider
    SKIP
    "Service Status ....:" OrderTPService.Status
    SKIP
    "Serial Number .....:" OrderTPService.SerialNumber
    SKIP
    "Cancellation Reason:" OrderTPService.TermReason
    SKIP
    "Order Date .......:" lcCreatedTime FORMAT "X(24)"   
    SKIP
    "Activation Time ..:" lcActivationTime FORMAT "X(24)"
    SKIP
    "Done Time ........:" lcDoneTime FORMAT "X(24)"
    SKIP
    "Updated Time .....:" lcUpdatedTime FORMAT "X(24)" 
    SKIP(4)
WITH OVERLAY ROW 1 WIDTH 80 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) OrderTPService.Type + " service data"
    NO-LABELS
    FRAME fData.

PAUSE 0 NO-MESSAGE.

ASSIGN
    lcCreatedTime    = fTS2HMS(OrderTPService.CreatedTS)  
    lcActivationTime = fTS2HMS(OrderTPService.ActivationTS)
    lcUpdatedTS      = fTS2HMS(OrderTPService.UpdatedTS)
    lcDoneTime       = fTS2HMS(OrderTPService.DoneTS).

DISP OrderTPService.OrderID
     OrderTPService.Product
     OrderTPService.Type
     OrderTPService.Provider
     OrderTPService.Status
     OrderTPService.SerialNumber
     OrderTPService.TermReason
     lcCreatedTime
     lcActivationTime
     lcUpdatedTS
     lcDoneTime
     WITH FRAME fData.

LOOP:
REPEAT WITH FRAME fData ON ENDKEY UNDO LOOP, NEXT LOOP:

   PAUSE 0.
   ASSIGN
      ufk   = 0  
      ufk[5]= 0
      ufk[6]= 0
      ufk[8]= 8 
      ehto  = 0.
   RUN Syst/ufkey.p.
   
   ELSE IF toimi = 8 THEN LEAVE.

END. 

HIDE MESSAGE NO-PAUSE.
HIDE FRAME lis NO-PAUSE.         
