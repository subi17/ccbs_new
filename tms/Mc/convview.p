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
           OrderFusion.Brand eq gcBrand AND
           OrderFusion.OrderID EQ iiOrderID NO-ERROR.

IF NOT AVAIL OrderFusion THEN DO:
   MESSAGE "Convergent data not found!" VIEW-AS ALERT-BOX.
   RETURN.
END.

FORM
   SKIP(1)
   iiOrderid     COLON 20 LABEL "Select Function"  /*FORMAT "X(8)"*/ SKIP
   SKIP(1)

   WITH ROW 4 OVERLAY SIDE-LABELS CENTERED 
        TITLE " Convergent Data View "  
        FRAME fData.



PAUSE 0 NO-MESSAGE.
VIEW FRAME fData. 
CLEAR FRAME fData NO-PAUSE.

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
     MESSAGE "5 pressed" VIEW-AS ALERT-BOX.
     RUN fusionmessage.p(iiOrderID).
   END.
   IF toimi EQ 6 THEN DO:
      MESSAGE "6 pressed" VIEW-AS ALERT-BOX.
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
          OrderCustomer.PostOffice SKIP
          OrderCustomer.Street SKIP
          OrderCustomer.StreetType SKIP
          OrderCustomer.BuildingNum SKIP
          OrderCustomer.BisDuplicate SKIP
          OrderCustomer.Block SKIP 
          OrderCustomer.Door 
          OrderCustomer.Letter 
          OrderCustomer.Stair 
          OrderCustomer.Floor 
          OrderCustomer.Hand 
          OrderCustomer.Km SKIP
          OrderCustomer.ZipCode.
         
      END.
      
   END.
   
   ELSE IF toimi = 8 THEN LEAVE.

END. 

HIDE MESSAGE NO-PAUSE.
HIDE FRAME lis NO-PAUSE.         
