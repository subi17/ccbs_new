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
DEF INPUT PARAMETER iiOrderId     AS INT  NO-UNDO.

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
    "Fixed Nbr Typea..." AT 40 OrderFusion.FixedNumberType FORMAT "X(18)"
    SKIP
    "Curr Oper Code....:" OrderFusion.FixedCurrOperCode
    "Current Operator .:" AT 40 OrderFusion.FixedCurrOper FORMAT "X(18)"
    SKIP
    "Fixed Status......:" OrderFusion.FixedStatus
    "Fixed Sub Status .:" AT 40 OrderFusion.FixedSubStatus FORMAT "X(18)"
    SKIP
    "Ext Ticket........:" OrderFusion.ExternalTicket
    "Fixed ContractID .:" AT 40 OrderFusion.FixedContractId FORMAT "X(18)"
    SKIP
    "Salesman..........:" OrderFusion.Salesman
    "Phone Book........:" AT 40 OrderFusion.PhoneBook FORMAT "X(18)"
    SKIP
    "Product...........:" OrderFusion.Product
    "Serial Number.....:" AT 40 OrderFusion.SerialNumber FORMAT "X(18)"
    SKIP
    "Customer Type.....:" OrderFusion.CustomerType
    "Fusion Status.....:" AT 40 OrderFusion.FusionStatus FORMAT "X(18)"
    SKIP
    "Order Date........:" OrderFusion.OrderDate
    "Updated...........:" AT 40 OrderFusion.UpdateTS /*FORMAT "X(20)"*/
    SKIP
    "Brand.............:" OrderFusion.Brand
    "MNP Time..........:" AT 40 OrderFusion.FixedMNPTime /*FORMAT "X(20)"*/
    SKIP(7)


WITH OVERLAY ROW 1 WIDTH 80 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) "Convergent data"
    NO-LABELS
    FRAME fData.

FORM
   "Country..........:" OrderCustomer.Country SKIP
   "Region...........:" OrderCustomer.Region SKIP
   "Post Office......:" OrderCustomer.PostOffice SKIP
   "Street...........:" OrderCustomer.Street SKIP
   "Street Type......:" OrderCustomer.StreetType SKIP
   "Building Number..:" OrderCustomer.BuildingNum SKIP
   "Bis Duplicate....:" OrderCustomer.BisDuplicate SKIP
   "Block............:" OrderCustomer.Block SKIP
   "Door.............:" OrderCustomer.Door SKIP
   "Letter...........:" OrderCustomer.Letter SKIP
   "Stair............:" OrderCustomer.Stair SKIP
   "Floor............:" OrderCustomer.Floor SKIP
   "Hand.............:" OrderCustomer.Hand SKIP
   "Km...............:" OrderCustomer.Km SKIP
   "Zip..............:" OrderCustomer.ZipCode SKIP(2)
WITH OVERLAY ROW 1 WIDTH 80 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) "Address"
    NO-LABELS
    FRAME fAddr.
PAUSE 0 NO-MESSAGE.
/*VIEW FRAME fData. */
/*CLEAR FRAME fData NO-PAUSE.*/

DISP OrderFusion.OrderID
     OrderFusion.FixedOrderId
     OrderFusion.FixedNumber
     OrderFusion.FixedNumberType
     OrderFusion.FixedCurrOperCode
     OrderFusion.FixedCurrOper
     OrderFusion.FixedStatus
     OrderFusion.FixedSubStatus
     OrderFusion.ExternalTicket
     OrderFusion.FixedContractId
     OrderFusion.Salesman
     OrderFusion.PhoneBook
     OrderFusion.Product
     OrderFusion.SerialNumber
     OrderFusion.CustomerType
     OrderFusion.FusionStatus
     OrderFusion.OrderDate
     OrderFusion.UpdateTS
     OrderFusion.Brand
     OrderFusion.FixedMNPTime WITH FRAME fData.

LOOP:
REPEAT WITH FRAME fMessages ON ENDKEY UNDO LOOP, NEXT LOOP:

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
      FIND FIRST OrderCustomer NO-LOCK where
                 OrderCustomer.Brand EQ Syst.Parameters:gcBrand AND
                 OrderCustomer.OrderId EQ iiOrderid AND
                 OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
                 NO-ERROR.
      IF NOT AVAIL OrderCustomer THEN
         MESSAGE "Installation address data not found" VIEW-AS ALERT-BOX.
      ELSE DO:
       PAUSE 0.
       DISP
          OrderCustomer.Country  
          OrderCustomer.Region 
          OrderCustomer.PostOffice 
          OrderCustomer.Street 
          OrderCustomer.StreetType 
          OrderCustomer.BuildingNum 
          OrderCustomer.BisDuplicate 
          OrderCustomer.Block 
          OrderCustomer.Door 
          OrderCustomer.Letter 
          OrderCustomer.Stair 
          OrderCustomer.Floor 
          OrderCustomer.Hand 
          OrderCustomer.Km 
          OrderCustomer.ZipCode WITH FRAME fAddr.
         
      END.
      
   END.
   
   ELSE IF toimi = 8 THEN LEAVE.

END. 

HIDE MESSAGE NO-PAUSE.
HIDE FRAME lis NO-PAUSE.         
