/* ----------------------------------------------------------------------
  MODULE .......: crkcheck.p
  TASK .........: check address info for order
  APPLICATION ..: tms
  AUTHOR .......: tk
  CREATED ......: 26.04.04
  CHANGED ......: 11.05.04 tk use table VRKQuery
                  18.05.04 tk put order on hold if deathday ne ?
                  30.06.04 tk chkerr only for invalid personid
                  13.07.04 tk fmakets
                  04.08.04/aam write errors to log 
                  05.08.04/aam if response error is 070 -> mark status as 2
                  21.02.06/aam actual query routine to vrkcheck.i,
                               write memo if query failes
                  29.03.06/aam use old query only from last 24h 
                               (previously 180 days)
  Version ......: SHARK
  ---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{timestamp.i}
{forderstamp.i}
{vrkcheck.i}
{orderfunc.i}
def input parameter iiOrderId like order.orderId.

def var date1 as da no-undo.
def var date2 as da no-undo.
def var ldstamp as de no-undo.

DEF VAR liError AS INT NO-UNDO.

find Order Where 
     Order.Brand = gcBrand AND
     Order.OrderId = iiOrderId exclusive-lock no-error no-wait.
if locked(Order) THEN do:
   if not session:batch then message "lukko" view-as alert-box.
   return.
end.

FIND FIRST OrderCustomer OF Order WHERE
           OrderCustomer.RowType = 1 EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE OrderCustomer THEN RETURN "ERROR:Customer not available".
 
   
ldStamp = fHMS2TS(TODAY - 1,STRING(TIME,"hh:mm:ss")).
   
FIND FIRST VRKQuery NO-LOCK WHERE 
           VRKQuery.PersonId = OrderCustomer.CustID AND
           VRKQuery.CrStamp > ldStamp NO-ERROR.

IF NOT AVAIL VRKQuery THEN DO:

   RUN pVRKCheck(OrderCustomer.CustID,
                 Order.OrderID,
                 OUTPUT liError).
                 
   /* error occurred */
   IF liError > 0 THEN DO:
       
       IF liError = 2 THEN DO:

          fSetOrderStatus(Order.OrderId,"2").
          /* Mark timestamp as change */
          fMarkOrderStamp(Order.OrderID,"Change",0.0).
          
          DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                     "Order",
                     STRING(Order.OrderId),
                     0,
                     "VRK Failed",
                     "Invalid person ID").
       END.
       
       RETURN.
   END. 
       
end.

assign
  OrderCustomer.Surname1   = VrkQuery.Lastname
  OrderCustomer.FirstName  = VrkQuery.FirstName
  OrderCustomer.address    = VrkQuery.Address when VrkQuery.Address ne ""
  OrderCustomer.zipcode    = VrkQuery.Zipcode when VrkQuery.Zipcode ne ""
  OrderCustomer.postoffice = VrkQuery.Postoffice when VrkQuery.Postoffice ne "".

IF VRKQuery.DeathDay NE ? THEN DO:

   fSetOrderStatus(Order.OrderId,"8").
   /* Mark timestamp as close */
   fMarkOrderStamp(Order.OrderID,"Close",0.0).

   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
              "Order",
              STRING(Order.OrderId),
              0,
              "VRK",
              "Person has died on " + STRING(VRKQuery.DeathDay,"99.99.9999")).
END.
 
/* tilapäinen osoite */

IF VRKQuery.TempFrom NE ? THEN DO:
   if VRKQuery.TempFrom < today and 
     (VRKQuery.TempTo = ? OR
      VRKQuery.TempTo > today) then do:

         assign 
            OrderCustomer.Address    = VrkQuery.TempAddress
            OrderCustomer.zipcode    = VrkQuery.TempZip
            OrderCustomer.PostOffice = VrkQuery.TempPoffice.
   end.

end.   





