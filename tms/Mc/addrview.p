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

FIND FIRST OrderCustomer NO-LOCK where
           OrderCustomer.Brand EQ Syst.Parameters:gcBrand AND
           OrderCustomer.OrderId EQ iiOrderid AND
           OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
           NO-ERROR.
IF NOT AVAIL OrderCustomer THEN DO:
   MESSAGE "Installation address data not found" VIEW-AS ALERT-BOX.
   RETURN.
END.

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
VIEW FRAME fAddr. 
/*CLEAR FRAME fData NO-PAUSE.*/


LOOP:
REPEAT WITH FRAME fAddr ON ENDKEY UNDO LOOP, NEXT LOOP:

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

   PAUSE 0.
   ASSIGN
      ufk   = 0  
      ufk[5]= 0
      ufk[6]= 0
      ufk[8]= 8 
      ehto  = 0.
   RUN ufkey.

   IF toimi = 8 THEN LEAVE.

END. 

HIDE MESSAGE NO-PAUSE.
