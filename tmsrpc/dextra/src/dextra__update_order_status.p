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
   IMEI_COL;array;optional;Array of structs (includes IMEI, MSDN & ICC fields)

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
   12;Integrity errors on ICC information
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEF VAR top_struct  AS CHAR          NO-UNDO.
DEF VAR lcTopStruct AS CHAR          NO-UNDO.
DEF VAR liResult    AS INT INITIAL 0 NO-UNDO.

DEFINE VARIABLE liCourierId         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcCourierShippingId AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLOId              AS INTEGER   NO-UNDO.
DEFINE VARIABLE liLOStatusId        AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldeLOTimeStamp      AS DATETIME  NO-UNDO.
DEFINE VARIABLE liOrderId           AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcOrderId           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLOStatusName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLOName            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCourierName       AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcIMEIArray      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIMEIStruct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIMEI           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcICC            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIMEIFields     AS CHARACTER NO-UNDO.
DEFINE VARIABLE liImeis          AS INTEGER   NO-UNDO.
DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
DEFINE VARIABLE delivery_address AS CHARACTER NO-UNDO.
DEFINE VARIABLE lhOrder          AS HANDLE    NO-UNDO.
DEFINE VARIABLE liRequest        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcError          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProductCLI     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProductICC     AS CHARACTER NO-UNDO.
DEFINE VARIABLE llHasProducts    AS LOGICAL NO-UNDO.
DEFINE VARIABLE liSIMOrderProductId AS INTEGER NO-UNDO.

DEF VAR lcDeliveryAddress AS CHAR NO-UNDO.
DEF VAR lcRegion          AS CHAR NO-UNDO.
DEF VAR lcStreet          AS CHAR NO-UNDO.
DEF VAR lcZip             AS CHAR NO-UNDO.
DEF VAR lcCity            AS CHAR NO-UNDO.
DEF VAR lcCountry         AS CHAR NO-UNDO.
DEF VAR lcStreetCode      AS CHAR NO-UNDO.
DEF VAR lcCityCode        AS CHAR NO-UNDO.
DEF VAR liDBCount         AS INT  NO-UNDO.
DEF VAR lcTenant          AS CHAR NO-UNDO.

DEFINE BUFFER bOrder FOR Order.
DEFINE BUFFER bfOrderMobile FOR OrderMobile.

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

      lcIMEIFields = validate_struct(lcIMEIStruct, "MSDN,IMEI,ICC").

      IF gi_xmlrpc_error NE 0 THEN RETURN.

      IF LOOKUP("IMEI",lcIMEIFields) > 0 THEN DO:
         lcIMEI = get_string(lcIMEIStruct,"IMEI").
         IF NOT fCheckIntegrity(10) THEN RETURN.
      END.

      IF LOOKUP("ICC",lcIMEIFields) > 0 THEN DO:
         lcICC = get_string(lcIMEIStruct,"ICC").
         IF NOT fCheckIntegrity(12) THEN RETURN.
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

{Syst/commpaa.i}
Syst.Var:katun = "Dextra".
Syst.Var:gcBrand = "1".
{Syst/eventval.i}
{Syst/tmsconst.i}
{Func/dextra.i}
{Syst/eventval.i}
{Func/create_eventlog.i}
{Func/orderfunc.i}

/* Set access to right tenant */
FOR FIRST Order WHERE Order.Brand = Syst.Var:gcBrand AND Order.OrderId = liOrderId TENANT-WHERE TENANT-ID() > -1 NO-LOCK:
    ASSIGN lcTenant = BUFFER-TENANT-NAME(Order).
END.

IF NOT AVAIL Order OR lcTenant = "" THEN DO:
   add_int(response_toplevel_id, "", 20).
   RETURN.
END.

DO liDBCount = 1 TO NUM-DBS
   ON ERROR UNDO, THROW:
    SET-EFFECTIVE-TENANT(lcTenant, LDBNAME(liDBCount)).
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
END.

IF CAN-FIND(FIRST OrderProduct WHERE OrderProduct.OrderID  = Order.OrderID NO-LOCK) THEN 
DO:
    ASSIGN lcProductCLI   =  Func.OrderProductsData:mGetOrderCLI(Order.OrderID)
           lcProductICC   =  Func.OrderProductsData:mGetOrderICC(Order.OrderID)
           llHasProducts  =  YES.

    FIND FIRST OrderMobile WHERE OrderMobile.OrderId        = Order.OrderId AND 
                                 OrderMobile.OrderProductID > 0             NO-LOCK NO-ERROR.
    IF NOT AVAIL OrderMobile THEN
       RETURN.

    ASSIGN liSIMOrderProductId = fGetChildProductID(OrderMobile.OrderId, 
                                                    OrderMobile.OrderProductID, 
                                                    {&ORDER_PRODUCT_SIM}).
END.
ELSE 
DO:
    ASSIGN 
        lcProductCLI        = Order.CLI
        lcProductICC        = Order.ICC
        liSIMOrderProductId = 0.
END.

IF lcIMEI NE "" AND lcIMEI NE ? THEN DO:
   /* YPR-4984, Router delivered to customer */
   IF liLOStatusId EQ 99998 THEN DO:
      FIND FIRST OrderFusion WHERE
                 OrderFusion.Brand EQ Syst.Var:gcBrand AND
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
   ELSE IF liLOStatusId EQ 88887 THEN
   DO:
      FIND FIRST TPService WHERE TPService.MsSeq      = Order.MsSeq                   AND
                                 TPService.Operation  = {&TYPE_ACTIVATION}            AND
                                 TPService.ServType   = "Television"                  AND
                                 TPService.ServStatus = {&STATUS_LOGISTICS_INITIATED} EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAIL TPService THEN
      DO:
          ASSIGN TPService.SerialNbr = lcIMEI.

          fCreateTPServiceMessage(TPService.MsSeq, TPService.ServSeq, {&SOURCE_LOGISTICS}, {&WAITING_FOR_STB_ACTIVATION}).
      END.
      ELSE
      DO:
          add_int(response_toplevel_id, "", 30).
          RETURN.
      END.
   END.
   ELSE DO:
      FIND FIRST OrderAccessory WHERE
         OrderAccessory.Brand = Syst.Var:gcBrand AND
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
         SubsTerminal.Brand = Syst.Var:gcBrand AND
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


IF lcICC NE "" AND lcICC NE ? THEN DO:

   FIND FIRST SIM EXCLUSIVE-LOCK WHERE
              SIM.Brand  EQ Syst.Var:gcBrand AND
              SIM.Stock  EQ {&ICC_STOCK_LO}  AND
              SIM.ICC    EQ lcICC            AND
              SIM.SimArt EQ "universal"      NO-ERROR.

   IF NOT AVAIL SIM THEN DO:
      add_int(response_toplevel_id, "", 41).
      RETURN.
   END.
   ELSE IF SIM.SimStat NE 1 THEN DO:
      add_int(response_toplevel_id, "", 42).
      RETURN.
   END.

   /* If already subscription is available then create
      ICC change request */
   FIND FIRST MobSub NO-LOCK WHERE
              MobSub.MsSeq EQ Order.MsSeq AND
              MobSub.ICC   NE ""          NO-ERROR.

   IF lcProductICC  EQ "" AND
      Order.OrderType EQ {&ORDER_TYPE_RENEWAL} AND
      LOOKUP(Order.StatusCode, {&ORDER_CLOSE_STATUSES}) = 0 THEN DO:

      FIND bOrder EXCLUSIVE-LOCK WHERE ROWID(bOrder) = ROWID(Order) NO-ERROR.
           
       IF llHasProducts THEN 
       DO:
           FIND FIRST bfOrderMobile WHERE ROWID(bfOrderMobile)  =  ROWID(OrderMobile) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
       END.
           
      IF Order.StatusCode NE {&ORDER_STATUS_DELIVERED} THEN DO:

          ASSIGN 
              bOrder.ICC              = lcICC
              SIM.SimStat             = 13
              SIM.MsSeq               = bOrder.MsSeq
              bfOrderMobile.ICC       = lcICC WHEN AVAILABLE bfOrderMobile.
         
         Func.Common:mWriteMemo("Order",
                                STRING(bOrder.OrderID),
                                bOrder.CustNum,
                                "ICC value updated by LO",
                                "").
      END.
      ELSE DO:

         IF Order.SalesMan > "" THEN
            Syst.Var:katun = Order.SalesMan.

         liRequest = fSubscriptionRequest
                         (INPUT  Order.MSSeq,
                          INPUT  lcProductCLI,
                          INPUT  Order.CustNum,
                          INPUT  1,
                          INPUT  "",
                          INPUT  Func.Common:mMakeTS(),
                          INPUT  "CHANGEICC",
                          INPUT  lcICC,
                          INPUT  "", /*for OrderProductId */
                          INPUT  "", /*for old SIM*/
                          INPUT  "", /*for Reason info*/
                          INPUT  "", /*for ContractID*/
                          INPUT  FALSE,
                          INPUT  0.0,
                          INPUT  {&SOURCE_LOGISTICS},
                          OUTPUT lcError).

         IF Order.SalesMan > "" THEN
            Syst.Var:katun = "Dextra".

         IF liRequest = 0 THEN DO:
            add_int(response_toplevel_id, "", 43).
            Func.Common:mWriteMemo("Order",
                             STRING(Order.OrderID),
                             Order.CustNum,
                             "ICC In Use",
                             lcICC + "is already in use").
            RETURN.
         END.
         ELSE DO:

             ASSIGN 
                 bOrder.ICC              = lcICC
                 SIM.SimStat             = 13
                 SIM.MsSeq               = bOrder.MsSeq
                 bfOrderMobile.ICC       = lcICC  WHEN AVAILABLE bfOrderMobile.
                   
            FIND MsRequest WHERE
                 MsRequest.MsRequest = liRequest EXCLUSIVE-LOCK NO-ERROR.
            MsRequest.ReqSource = {&REQUEST_SOURCE_ICC_CHANGE_AUTO}.
            fReqStatus(19,"").

            Func.Common:mWriteMemo("MsRequest",
                             STRING(liRequest),
                             Order.CustNum,
                             "ICC TYPE CHANGE AUTO",
                             Order.OrderChannel).
         END. /* ELSE DO: */

      END.
   END.
   ELSE IF AVAIL MobSub AND
      MobSub.ICC NE lcICC AND
      Order.OrderType NE {&ORDER_TYPE_RENEWAL} THEN DO:
      liRequest = fSubscriptionRequest(Mobsub.MsSeq,
                                       Mobsub.Cli,
                                       Mobsub.CustNum,
                                       1,                        /*tarifftype*/
                                       "",                       /*creator*/
                                       Func.Common:mMakeTS(),    /*ActStamp*/
                                       "CHANGEICC",              /*ReqParam*/
                                       lcICC,                    /*ReqParam2*/
                                       "",                       /*OrderProductId*/
                                       "",                       /*old SIM*/
                                       "",                       /*Reason*/
                                       "",                       /*ContractID*/
                                       FALSE,                    /*CreateFees*/
                                       0,                        /*Charge*/
                                       {&SOURCE_LOGISTICS},      /*Request source*/
                                       OUTPUT lcError).          /*result*/
      IF liRequest EQ 0 THEN DO:
         add_int(response_toplevel_id, "", 43).
         Func.Common:mWriteMemo("MobSub",
                                STRING(MobSub.MsSeq),
                                MobSub.CustNum,
                                "ICC In Use",
                                lcICC + "is already in use").
         RETURN.
      END.
   END.
   ELSE IF lcProductICC  EQ "" AND
      Order.OrderType  NE {&ORDER_TYPE_STC}     AND
      Order.OrderType  NE {&ORDER_TYPE_RENEWAL} AND
     (Order.StatusCode EQ {&ORDER_STATUS_PENDING_ICC_FROM_LO} OR
      Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE}  OR
      Order.StatusCode EQ {&ORDER_STATUS_PENDING_MAIN_LINE}   OR
      Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL}) AND
      LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) EQ 0 THEN DO:

      FIND bOrder EXCLUSIVE-LOCK WHERE
           ROWID(bOrder) = ROWID(Order) NO-ERROR.
           
       IF llHasProducts THEN
          FIND CURRENT OrderMobile EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

      IF ERROR-STATUS:ERROR OR LOCKED(bOrder) THEN RETURN.

       ASSIGN 
           bOrder.ICC              = lcICC
           SIM.SimStat             = 4
           SIM.MsSeq               = bOrder.MsSeq
           OrderMobile.ICC         = lcICC   WHEN AVAILABLE OrderMobile.

      CREATE SimDeliveryhist.
      ASSIGN SimDeliveryHist.OrderID    = bOrder.OrderID
             SimDeliveryHist.MSSeq      = bOrder.MSSeq
             SimDeliveryHist.StatusCode = 2.
             SimDeliveryHist.TimeStamp  = Func.Common:mMakeTS().

      Func.Common:mWriteMemo("Order",
                             STRING(bOrder.OrderID),
                             bOrder.CustNum,
                             "ICC value updated by LO",
                             "").

      IF llDoEvent THEN DO:
         lhOrder = BUFFER bOrder:HANDLE.
         RUN StarEventInitialize(lhOrder).
         RUN StarEventSetOldBuffer(lhOrder).
      END.

      /* order status with 76 is not needed to be released, at it will
         be released when associated main line fixed line is installed */
      IF bOrder.StatusCode EQ {&ORDER_STATUS_PENDING_ICC_FROM_LO} THEN 
      DO:
         CASE bOrder.OrderType:
            WHEN {&ORDER_TYPE_NEW} THEN 
            DO:
                fSetOrderStatus(bOrder.OrderId,{&ORDER_STATUS_NEW}).

                IF liSIMOrderProductId > 0 THEN /* This is populated inside fDelivSIM */
                   fSetOrderProductStatus(bOrder.OrderId, liSIMOrderProductId, {&ORDER_STATUS_NEW}).
            END.
            WHEN {&ORDER_TYPE_MNP} THEN 
            DO:
                fSetOrderStatus(bOrder.OrderId,{&ORDER_STATUS_MNP}).
                
                IF liSIMOrderProductId > 0 THEN /* This is populated inside fDelivSIM */
                   fSetOrderProductStatus(bOrder.OrderId, liSIMOrderProductId, {&ORDER_STATUS_MNP}).
            END.    
         END CASE.
      END.

      IF llDoEvent THEN DO:
         RUN StarEventMakeModifyEvent(lhOrder).
         fCleanEventObjects().
      END.

   END.
   ELSE DO:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
             ErrorLog.ActionID  = "ORDERICC"
             ErrorLog.TableName = "Order"
             ErrorLog.KeyValue  = STRING(Order.OrderId)
             ErrorLog.ErrorMsg  = SUBST(
               "ICC &1 not assigned due to wrong order status &2",
               lcICC,
               Order.StatusCode)
             ErrorLog.UserCode  = Syst.Var:katun
             ErrorLog.ActionTS  = Func.Common:mMakeTS().
   END.

END.

IF llDoEvent THEN DO:
   DEFINE VARIABLE lhOrderDelivery AS HANDLE NO-UNDO.
   lhOrderDelivery = BUFFER OrderDelivery:HANDLE.
   RUN StarEventInitialize(lhOrderDelivery).
END.

CREATE OrderDelivery.
ASSIGN
   OrderDelivery.Brand = Syst.Var:gcBrand
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
              OrderCustomer.Brand = Syst.Var:gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_LOGISTICS}
   NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN DO:
      CREATE OrderCustomer.
      ASSIGN
         OrderCustomer.Brand     = Syst.Var:gcBrand
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
                                  Syst.Var:katun,
                                  "").
      ELSE RUN StarEventMakeModifyEvent(lhOrderCustomer).
   END.
END.


/* Remove router prefix 9999 for SMS sending */
IF STRING(liLOStatusId) BEGINS {&LO_STATUS_ROUTER_PREFIX} THEN
   liLOStatusId = INT(SUBSTRING(STRING(liLOStatusId), LENGTH({&LO_STATUS_ROUTER_PREFIX}) + 1)).
ELSE IF STRING(liLOStatusId) BEGINS {&LO_STATUS_TV_STB_PREFIX} THEN
   liLOStatusId = INT(SUBSTRING(STRING(liLOStatusId), LENGTH({&LO_STATUS_TV_STB_PREFIX}) + 1)).

fSendDextraSMS(Order.OrderID, liLOStatusId, liCourierId).

FIND CURRENT OrderDelivery NO-LOCK.

IF LOOKUP(STRING(OrderDelivery.LOStatusId),{&DEXTRA_CANCELLED_STATUSES}) > 0 THEN
DO:
   IF Order.StatusCode = {&ORDER_STATUS_RESIGNATION} THEN
      RUN Mc/closeorder.p(Order.OrderId,TRUE).
   ELSE
      RUN Mc/cancelorder.p(Order.OrderId,TRUE).
END.

add_int(response_toplevel_id, "", liResult).

FINALLY:
   IF llDoEvent THEN
      fCleanEventObjects().
END.
