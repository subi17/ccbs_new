&IF "{&orderchk}" NE "YES"
&THEN
&GLOBAL-DEFINE orderchk YES

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/extralinefunc.i}

/* ----------------------------------------------------------------------
  MODULE .......: orderchk.i 
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 20.08.08
  CHANGED ......:
  Version ......: xfera
  27/12/17  ashok  YDR-2665 ExtraLine should not be counted for 
                            Subscription and Activation limits
----------------------------------------------------------------------- */

/*Usage of return values:
TRUE  -> RENEWAL
FALSE -> RENEWAL_HOLD*/
FUNCTION fCheckRenewalData RETURNS LOGICAL
   (INPUT iiOrderID AS INT):

   DEF BUFFER bOrderCustomer FOR OrderCustomer.
   DEF BUFFER Order FOR Order.
   FIND FIRST Order NO-LOCK WHERE
              Order.Brand EQ Syst.Var:gcBrand AND
              Order.OrderID EQ iiOrderID NO-ERROR.
   IF NOT AVAIL Order THEN RETURN FALSE.           

   /* delivery address is different than customer address */
   IF CAN-FIND(FIRST bOrderCustomer NO-LOCK WHERE
      bOrderCustomer.Brand = Syst.Var:gcBrand AND
      bOrderCustomer.OrderId  = OrderCustomer.OrderID AND
      bOrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_DELIVERY}) 
      OR
      OrderCustomer.Firstname NE Customer.Firstname OR
      OrderCustomer.Surname1 NE Customer.Custname OR
      OrderCustomer.Surname2 NE Customer.Surname2 OR
      OrderCustomer.BirthDay NE Customer.BirthDay OR
      OrderCustomer.Address NE Customer.Address OR
      OrderCustomer.PostOffice NE Customer.PostOffice OR
      OrderCustomer.Region NE Customer.Region OR
      OrderCustomer.Country NE Customer.Country OR
      OrderCustomer.ZipCode NE Customer.ZipCode OR      
      OrderCustomer.BankCode NE Customer.BankAcct  
   THEN DO:
    /*YDR-2834 [RES-1119] Skip order status 31 SM/PM*/
    /*No need to RENEWAL_HOLD when delivery to shop*/
      IF (Order.OrderChannel EQ "renewal" OR
          Order.OrderChannel EQ "renewal_telesales" OR
          Order.OrderChannel EQ "retention" OR
          Order.OrderChannel EQ "renewal_ctc") AND
         (Order.Deliverytype EQ {&ORDER_DELTYPE_POS}  OR
          Order.Deliverytype EQ {&ORDER_DELTYPE_POST})THEN RETURN TRUE.
      RETURN FALSE.
   END.

   RETURN TRUE.

END FUNCTION. 

/* Two orders cannot be ongoing at the same time with this logic */
FUNCTION fOngoingOrders RETURNS LOGICAL
(pcCli AS CHAR,
 pcNumberType AS CHAR):

   DEF VAR liExcludeOrderType AS INT NO-UNDO. 

   IF pcNumberType EQ "stc" THEN liExcludeOrderType = {&ORDER_TYPE_RENEWAL}.
   ELSE IF pcNumberType EQ "renewal" OR
           pcNumberType EQ "retention" THEN liExcludeOrderType = {&ORDER_TYPE_STC}.
   ELSE liExcludeOrderType = -1.

   DEF BUFFER lbOtherOrder FOR Order.   
   
   FOR EACH lbOtherOrder NO-LOCK WHERE
            lbOtherOrder.brand EQ Syst.Var:gcBrand AND
            lbOtherOrder.CLI EQ pcCLI AND
            LOOKUP(lbOtherOrder.statuscode,{&ORDER_INACTIVE_STATUSES}) EQ 0 AND
            lbOtherOrder.OrderType NE liExcludeOrderType AND
            lbOtherOrder.OrderType <= {&ORDER_TYPE_STC}:
      /* YPR-2105 */
      IF pcNumberType EQ "retention" AND
         lbOtherOrder.StatusCode = {&ORDER_STATUS_OFFER_SENT} THEN NEXT. /* shouldn't never happen because of YDR-2575 */
      RETURN TRUE.
   END.
   
   RETURN FALSE.

END FUNCTION. 

FUNCTION fOngoingFixedOrders RETURNS CHARACTER
(pcFixedNumber AS CHAR,
 pcNumberType  AS CHAR):

   /* Check if same number in ongoing Fusion order */
   DEF BUFFER lbOtherOrder  FOR Order.
   DEF BUFFER lbOrderFusion FOR OrderFusion.

   FOR EACH lbOrderFusion NO-LOCK WHERE
            lbOrderFusion.FixedNumber EQ pcFixedNumber,
      EACH  lbOtherOrder NO-LOCK WHERE
            lbOtherOrder.brand EQ Syst.Var:gcBrand AND
            lbOtherOrder.OrderId EQ lbOrderFusion.OrderId AND
            LOOKUP(lbOtherOrder.statuscode,{&ORDER_INACTIVE_STATUSES}) EQ 0:
   
     IF (pcNumberType EQ "renewal" OR pcNumberType EQ "retention") AND 
        lbOtherOrder.OrderType EQ {&ORDER_TYPE_STC} THEN NEXT.

      RETURN "Ongoing order for number|" + pcFixedNumber.
   END.

   RETURN "".

END FUNCTION. 

FUNCTION fIsPreactivatedCustomer RETURNS LOGICAL
(iiCustnum AS INTEGER):
   RETURN (LOOKUP(STRING(iiCustnum), "233718,239696,239680,239666") > 0).
END FUNCTION. 

&ENDIF
