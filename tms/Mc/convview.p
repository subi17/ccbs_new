/* ----------------------------------------------------------------------
  MODULE .......: convview.p
  TASK .........: Display Convergent offer datga
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 6.10.2016
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{tmsconst.i}
{timestamp.i}
DEF INPUT PARAMETER iiOrderId     AS INT  NO-UNDO.
DEF VAR lcUpdateTS AS CHAR NO-UNDO.
DEF VAR lcInstallationTime AS CHAR NO-UNDO.
DEF VAR lcFixedTime AS CHAR NO-UNDO.



FIND FIRST OrderFusion NO-LOCK where
           OrderFusion.Brand eq Syst.Parameters:gcBrand AND
           OrderFusion.OrderID EQ iiOrderID NO-ERROR.

IF NOT AVAIL OrderFusion THEN DO:
   MESSAGE "Convergent data not found!" VIEW-AS ALERT-BOX.
   RETURN.
END.

FORM
    "OrderID ..........:" OrderFusion.OrderID
    "Fixed OrderId.....:" AT 40 OrderFusion.FixedOrderId FORMAT "X(18)"
    SKIP
    "Fixed Number......:" OrderFusion.FixedNumber
    "Fixed Number Type.:" AT 40 OrderFusion.FixedNumberType FORMAT "X(18)"
    SKIP
    "Current Operator..:" OrderFusion.FixedCurrOper
    "Operator Code.....:" AT 40 OrderFusion.FixedCurrOperCode FORMAT "X(18)"
    SKIP
    "Fixed Status......:" OrderFusion.FixedStatus
    "Fusion Status.....:" AT 40 OrderFusion.FusionStatus FORMAT "X(18)"
    SKIP
    "Product...........:" OrderFusion.Product
    "Serial Number.....:" AT 40 OrderFusion.SerialNumber FORMAT "X(18)"
    SKIP
    "Order Date........:" OrderFusion.OrderDate    
    "MNP Time..........:" AT 40 OrderFusion.FixedMNPTime
    SKIP
    "Customer Type.....:" OrderFusion.CustomerType
    SKIP
    "Fixed Inst Time...:" lcInstallationTime FORMAT "X(24)"
    SKIP
    "Fixed status Time.:" lcFixedTime FORMAT "X(24)" 
    SKIP
    "Updated...........:" lcUpdateTS FORMAT "X(24)" 
    SKIP(7)
 

WITH OVERLAY ROW 1 WIDTH 80 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) "Convergent data"
    NO-LABELS
    FRAME fData.

PAUSE 0 NO-MESSAGE.
/*VIEW FRAME fData. */
/*CLEAR FRAME fData NO-PAUSE.*/
   ASSIGN
   lcUpdateTS = fTS2HMS(OrderFusion.UpdateTS)
   lcInstallationTime = fTS2HMS(OrderFusion.FixedInstallationTS)
   lcFixedTime = fTS2HMS(OrderFusion.FixedStatusTS).


DISP OrderFusion.OrderID
     OrderFusion.FixedOrderId
     OrderFusion.FixedNumber
     OrderFusion.FixedNumberType
     OrderFusion.FixedCurrOper
     OrderFusion.FixedCurrOperCode
     OrderFusion.FixedStatus
     OrderFusion.FusionStatus
     OrderFusion.Product
     OrderFusion.SerialNumber
     OrderFusion.OrderDate
     OrderFusion.FixedMNPTime
     OrderFusion.CustomerType
     lcInstallationTime
     lcFixedTime
     lcUpdateTS
     WITH FRAME fData.

LOOP:
REPEAT WITH FRAME fData ON ENDKEY UNDO LOOP, NEXT LOOP:

   PAUSE 0.
   ASSIGN
      ufk   = 0  
      ufk[5]= 9853
      ufk[6]= 9854
      ufk[8]= 8 
      ehto  = 0.
   RUN ufkey.

   IF toimi EQ 5 THEN DO:
     RUN fusionmessage.p(iiOrderID).
   END.
   IF toimi EQ 6 THEN DO:
     RUN addrview.p(iiOrderId).
   END.
   
   ELSE IF toimi = 8 THEN LEAVE.

END. 

HIDE MESSAGE NO-PAUSE.
HIDE FRAME lis NO-PAUSE.         
