/**
 * Updates Order delivery status and IMEI
 * 
 * @input orderdelivery;struct;mandatory;
 *  
 * @orderdelivery Order_Id;string;mandatory;
   timestamp;datetime;mandatory;logistic handling time
   LO_Status_ID;integer;mandatory;
   LO_Status_Description;string;optional;
   LO_Id;integer;mandatory;
   LO_Description;string;optional;
   Courier_ID;integer;mandatory;
   Courier_Description;string;optional;
   Courier_Shipping_ID;string;optional;
   IMEI_COL;array;optional;Array of structs (includes IMEI and MSDN fields)
 
 * @output returncode;integer
 
 * @returncode 0;Order status update successful
   1;Integrity errors on order ID
   2;Integrity errors on timestamp
   3;Integrity errors on logistic operator status ID
   4;Integrity errors on logistic operator status name
   5;Integrity errors on logistic operator ID
   6;Integrity errors on logistic operator name
   7;Integrity errors on courier ID
   8;Integrity errors on courier name
   9;Integrity Integrity errors on courier shipping ID
   10;Integrity errors on IMEI information
   11;Integrity errors on delivery_address information
 
*/

{xmlrpc/xmlrpc_access.i}
DEF VAR top_struct AS CHAR NO-UNDO.
DEF VAR lcTopStruct AS CHAR NO-UNDO.
DEF VAR liResult AS INT INITIAL 0 NO-UNDO.

DEFINE VARIABLE liCourierId AS INTEGER NO-UNDO.         
DEFINE VARIABLE lcCourierShippingId AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liLOId AS INTEGER NO-UNDO.             
DEFINE VARIABLE liLOStatusId AS INTEGER NO-UNDO.        
DEFINE VARIABLE ldeLOTimeStamp AS DATETIME NO-UNDO.      
DEFINE VARIABLE liOrderId AS INTEGER NO-UNDO.         
DEFINE VARIABLE lcOrderId AS CHAR NO-UNDO.         
DEFINE VARIABLE lcLOStatusName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcLOName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCourierName AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcIMEIArray AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIMEIStruct AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIMEI AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIMEIFields AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liImeis AS INTEGER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE delivery_address AS CHAR NO-UNDO. 

DEF VAR lcDeliveryAddress AS CHAR NO-UNDO. 
DEF VAR lcRegion AS CHAR NO-UNDO. 
DEF VAR lcStreet AS CHAR NO-UNDO. 
DEF VAR lcZip AS CHAR NO-UNDO. 
DEF VAR lcCity AS CHAR NO-UNDO. 
DEF VAR lcCountry AS CHAR NO-UNDO. 
DEF VAR lcStreetCode AS CHAR NO-UNDO. 
DEF VAR lcCityCode AS CHAR NO-UNDO. 

FUNCTION fCheckIntegrity RETURNS LOGICAL 
   (iiErrCode AS INTEGER):
   IF gi_xmlrpc_error = {&INVALID_METHOD_PARAMS} THEN DO:
      gi_xmlrpc_error = 0.
      gc_xmlrpc_error = "".
      add_int(response_toplevel_id, "", iiErrCode).
      RETURN FALSE.
   END.
   RETURN TRUE.
END FUNCTION. 

top_struct = get_struct(param_toplevel_id, "0").

lcTopStruct = validate_struct(top_struct, "Order_Id,timestamp,LO_Status_ID,LO_Status_Description,LO_Id,LO_Description,Courier_ID,Courier_Description,Courier_Shipping_ID,IMEI_COL,delivery_address").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcOrderId = get_string(top_struct,"Order_Id") NO-ERROR.
IF NOT fCheckIntegrity(1) THEN RETURN.

liOrderId = INT(lcOrderId) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
   add_int(response_toplevel_id, "", 1).
   RETURN.
END.

ldeLOTimeStamp = get_datetime(top_struct,"timestamp").
IF NOT fCheckIntegrity(2) THEN RETURN.
liLOStatusId = get_int(top_struct,"LO_Status_ID").
IF NOT fCheckIntegrity(3) THEN RETURN.

IF LOOKUP("LO_Status_Description", lcTopStruct) GT 0 THEN DO:
   lcLOStatusName = get_string(top_struct,"LO_Status_Description").
   IF NOT fCheckIntegrity(4) THEN RETURN.
END.

liLOId = get_int(top_struct,"LO_Id").
IF NOT fCheckIntegrity(5) THEN RETURN.

IF LOOKUP("LO_Description", lcTopStruct) GT 0 THEN DO:
   lcLOName = get_string(top_struct,"LO_Description").
   IF NOT fCheckIntegrity(6) THEN RETURN.
END.

liCourierId = get_int(top_struct,"Courier_Id").
IF NOT fCheckIntegrity(7) THEN RETURN.

IF LOOKUP("Courier_Description", lcTopStruct) GT 0 THEN DO:
   lcCourierName  = get_string(top_struct,"Courier_Description").
   IF NOT fCheckIntegrity(8) THEN RETURN.
END.

IF LOOKUP("Courier_Shipping_ID", lcTopStruct) GT 0 THEN DO:
   lcCourierShippingId = get_string(top_struct,"Courier_Shipping_ID").
   IF NOT fCheckIntegrity(9) THEN RETURN.
END.

IF LOOKUP("IMEI_COL", lcTopStruct) GT 0 THEN DO:
   
   lcIMEIArray = get_array(top_struct,"IMEI_COL").
   
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   liImeis = get_paramcount(lcIMEIArray).
   IMEI_LOOP:
   do i = 1 to liIMEIs: 
      
      lcIMEIStruct = get_struct(lcIMEIArray,string(i - 1)).
      IF gi_xmlrpc_error NE 0 THEN RETURN.

      lcIMEIFields = validate_struct(lcIMEIStruct, "MSDN,IMEI").
      IF gi_xmlrpc_error NE 0 THEN RETURN.
      
      IF LOOKUP("IMEI",lcIMEIFields) > 0 THEN DO:
         lcIMEI = get_string(lcIMEIStruct,"IMEI").
         IF NOT fCheckIntegrity(10) THEN RETURN.
         IF lcIMEI NE "" AND lcIMEI NE ? THEN LEAVE IMEI_LOOP.
      END.
   END.
END.

IF LOOKUP("delivery_address", lcTopStruct) > 0 THEN DO:
   
   delivery_address = get_struct(top_struct, "delivery_address").
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   lcDeliveryAddress = validate_struct(delivery_address,
      "region!,street!,zip!,city!,country!,street_code,city_code").
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   IF NOT fCheckIntegrity(11) THEN RETURN.

   ASSIGN
      lcRegion = get_string(delivery_address, "region")
      lcStreet = get_string(delivery_address, "street")
      lcZip    = get_string(delivery_address, "zip")
      lcCity   = get_string(delivery_address, "city")
      lcCountry = get_string(delivery_address, "country")
      lcStreetCode = get_string(delivery_address, "street_code") WHEN
                     LOOKUP("street_code", lcDeliveryAddress) > 0
      lcCityCode = get_string(delivery_address, "city_code") WHEN
                     LOOKUP("city_code", lcDeliveryAddress) > 0.
   IF NOT fCheckIntegrity(11) THEN RETURN.

END.

IF gi_xmlrpc_error NE 0 THEN RETURN.

{commpaa.i}
katun = "Dextra".
gcBrand = "1".
{eventval.i}
{tmsconst.i}
{dextra.i}
{eventval.i}
{create_eventlog.i}

FIND Order NO-LOCK WHERE
     Order.Brand = gcBrand AND
     Order.OrderId = liOrderId NO-ERROR.
IF NOT AVAIL Order THEN DO:
   add_int(response_toplevel_id, "", 20).
   RETURN.
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun 
   {lib/eventlog.i}
END.
   
IF lcIMEI NE "" AND lcIMEI NE ? THEN DO:
   /* YPR-4984, Router delivered to customer */
   IF liLOStatusId EQ 99998 THEN DO:
      FIND FIRST OrderFusion WHERE
                 OrderFusion.Brand EQ gcBrand AND
                 OrderFusion.orderid EQ Order.OrderId NO-ERROR.
      IF AVAIL OrderFusion THEN DO:
         FIND FIRST FusionMessage WHERE
                    FusionMessage.orderID EQ Order.OrderId AND
                    FusionMessage.messagetype EQ
                       {&FUSIONMESSAGE_TYPE_LOGISTICS} AND
                    FusionMessage.messageStatus EQ {&FUSIONMESSAGE_STATUS_SENT}
                    NO-ERROR.
         OrderFusion.serialnumber = lcIMEI.
         IF AVAIL FusionMessage THEN
            FusionMessage.messageStatus = {&FUSIONMESSAGE_STATUS_ONGOING}.
      END.
   END.   
   ELSE DO:
      FIND FIRST OrderAccessory WHERE
         OrderAccessory.Brand = gcBrand AND
         OrderAccessory.OrderId = Order.OrderId AND
         OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE}) 
         NO-LOCK NO-ERROR.

      IF AVAIL OrderAccessory AND OrderAccessory.IMEI NE lcIMEI THEN DO:

         FIND CURRENT OrderAccessory EXCLUSIVE-LOCK.

         IF llDoEvent THEN DO:
            DEFINE VARIABLE lhOrderAccessory AS HANDLE NO-UNDO.
            lhOrderAccessory = BUFFER OrderAccessory:HANDLE.
            RUN StarEventInitialize(lhOrderAccessory).
            RUN StarEventSetOldBuffer(lhOrderAccessory).
         END.

         ASSIGN OrderAccessory.IMEI = lcIMEI.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrderAccessory).
      END.

      FIND FIRST SubsTerminal WHERE
         SubsTerminal.Brand = gcBrand AND
         SubsTerminal.OrderId = Order.OrderId AND
         SubsTerminal.TerminalType = ({&TERMINAL_TYPE_PHONE}) NO-LOCK NO-ERROR.

      IF AVAIL SubsTerminal AND SubsTerminal.IMEI NE lcIMEI THEN DO:
         
         FIND CURRENT SubsTerminal EXCLUSIVE-LOCK.
         
         IF llDoEvent THEN DO:
            DEFINE VARIABLE lhSubsTerminal AS HANDLE NO-UNDO.
            lhSubsTerminal = BUFFER SubsTerminal:HANDLE.
            RUN StarEventInitialize(lhSubsTerminal).
            RUN StarEventSetOldBuffer(lhSubsTerminal).
         END.
         
         ASSIGN SubsTerminal.IMEI = lcIMEI.
      
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSubsTerminal).
      
      END.
   END.
END.
   
IF llDoEvent THEN DO:
   DEFINE VARIABLE lhOrderDelivery AS HANDLE NO-UNDO.
   lhOrderDelivery = BUFFER OrderDelivery:HANDLE.
   RUN StarEventInitialize(lhOrderDelivery).
END.

CREATE OrderDelivery.
ASSIGN
   OrderDelivery.Brand = gcBrand
   OrderDelivery.OrderId = Order.OrderId
   OrderDelivery.LOTimeStamp = ldeLOTimeStamp
   OrderDelivery.CourierId = liCourierId
   OrderDelivery.CourierShippingId = lcCourierShippingId
   OrderDelivery.LOId = liLOId
   OrderDelivery.LOStatusId = liLOStatusId
   OrderDelivery.IncidentInfoId = ?
   OrderDelivery.MeasuresInfoId = ?.


IF llDoEvent THEN RUN StarEventMakeCreateEvent (lhOrderDelivery).
      
IF LOOKUP("delivery_address", lcTopStruct) > 0 THEN DO:

   FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
              OrderCustomer.Brand = gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_LOGISTICS}
   NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN DO:
      CREATE OrderCustomer.
      ASSIGN
         OrderCustomer.Brand     = gcBrand 
         OrderCustomer.OrderId   = Order.OrderId
         OrderCustomer.RowType   = {&ORDERCUSTOMER_ROWTYPE_LOGISTICS}.
   END.
   ELSE IF llDoEvent THEN DO:
      DEFINE VARIABLE lhOrderCustomer AS HANDLE NO-UNDO.
      lhOrderCustomer = BUFFER OrderCustomer:HANDLE.
      RUN StarEventInitialize(lhOrderCustomer).
      RUN StarEventSetOldBuffer(lhOrderCustomer).
   END.

   ASSIGN
      OrderCustomer.Region       = lcRegion
      OrderCustomer.Street       = lcStreet
      OrderCustomer.ZipCode      = lcZip
      OrderCustomer.PostOffice   = lcCity
      OrderCustomer.Country      = lcCountry
      OrderCustomer.AddressCodC  = lcStreetCode
      OrderCustomer.AddressCodP  = lcCityCode.
   
   IF llDoEvent THEN DO:
      IF NEW OrderCustomer THEN
         fMakeCreateEvent((BUFFER OrderCustomer:HANDLE),
                                  "",
                                  katun,
                                  "").
      ELSE RUN StarEventMakeModifyEvent(lhOrderCustomer).
   END.
END.
/* YPR-4984, Router delivered to customer */
IF liLOStatusId EQ 99998 AND lcIMEI > "" THEN DO:
   FIND FIRST OrderFusion WHERE
              OrderFusion.Brand EQ gcBrand AND
              OrderFusion.orderid EQ Order.OrderId NO-ERROR.
   IF AVAIL OrderFusion THEN DO:
      FIND FIRST FusionMessage WHERE
                 FusionMessage.orderID EQ Order.OrderId AND
                 FusionMessage.messagetype EQ 
                    {&FUSIONMESSAGE_TYPE_LOGISTICS} AND
                 FusionMessage.messageStatus EQ {&FUSIONMESSAGE_STATUS_SENT}
                 NO-ERROR.
      OrderFusion.serialnumber = lcIMEI.           
      IF AVAIL FusionMessage THEN
         FusionMessage.messageStatus = {&FUSIONMESSAGE_STATUS_ONGOING}.
   END.
END.

/* Remove router prefix 9999 for SMS sending */
IF STRING(liLOStatusId) BEGINS {&LO_STATUS_ROUTER_PREFIX} THEN
   liLOStatusId = INT(SUBSTRING(STRING(liLOStatusId),
                      LENGTH({&LO_STATUS_ROUTER_PREFIX}) + 1)).

fSendDextraSMS(Order.OrderID, liLOStatusId, liCourierId).

FIND CURRENT OrderDelivery NO-LOCK.

IF LOOKUP(STRING(OrderDelivery.LOStatusId),
   {&DEXTRA_CANCELLED_STATUSES}) > 0 THEN DO:
   IF Order.StatusCode = {&ORDER_STATUS_RESIGNATION} THEN
      RUN closeorder.p(Order.OrderId,TRUE).
   ELSE RUN cancelorder.p(Order.OrderId,TRUE).
END.

add_int(response_toplevel_id, "", liResult).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   IF llDoEvent THEN fCleanEventObjects().
END.
