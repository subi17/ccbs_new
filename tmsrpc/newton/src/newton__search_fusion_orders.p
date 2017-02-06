/**
 * Search fusion orders, at least one search parameter is mandatory
 *
 * @input order_id;int;optional;
          fixed_line_order_id;string;optional;
          msisdn;string;optional;
          fixed_line_number;optional;
          fusion_order_status;string;optional;
          customer_id;string;optional;if selected then customer_id_type is mandatory
          customer_id_type;string;optional;if selected then customer_id is mandatory
          salesman;string;optional;
          limit;int;optional;default is 50 
          offset;int;optional;default is 0
          sort_order;string;optional;descending/ascending (default is descending)
 * @output orders;array;a list of order structs
 * @order order_id;int;OrderId
          customer_id;string;
          customer_id_type;string;
          msisdn;string;mobile order msisdn
          fixed_line_order_id;string;
          salesman;string;SFID
          orderchannel;string;
          fusion_order_status;string;
          order_date;datetime;date only
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
{Syst/tmsconst.i}
gcBrand = "1".

DEF VAR pcInputStruct AS CHAR NO-UNDO. 
DEF VAR lcInputFields AS CHAR NO-UNDO. 

DEF VAR liOrderId AS INT NO-UNDO. 
DEF VAR lcFixedLineOrderID AS CHAR NO-UNDO. 
DEF VAR lcMsisdn AS CHAR NO-UNDO. 
DEF VAR lcFixedLineNumber AS CHAR NO-UNDO. 
DEF VAR lcFusionOrderStatus AS CHAR NO-UNDO. 
DEF VAR lcCustomerId AS CHAR NO-UNDO. 
DEF VAR lcCustomerIdType AS CHAR NO-UNDO. 
DEF VAR lcSalesman AS CHAR NO-UNDO. 
DEF VAR lcSortOrder AS CHAR NO-UNDO. 
DEF VAR liLimit AS INT NO-UNDO INIT 50. 
DEF VAR liOffSet AS INT NO-UNDO. 

DEF VAR lcQuery AS CHAR NO-UNDO. 
DEF VAR lcBaseQuery AS CHAR NO-UNDO. 
DEF VAR lcSubQuery AS CHAR NO-UNDO. 
DEF VAR llAnd AS LOG NO-UNDO. 
DEF VAR lcBuffers AS CHAR NO-UNDO. 

FUNCTION fAddQueryCondition RETURNS CHAR
( icCondition AS CHAR):
   IF llAnd THEN
      lcSubQuery = lcSubQuery + " AND " + icCondition.
   ELSE ASSIGN
     llAnd = TRUE
     lcSubQuery = lcSubQuery + " " + icCondition.
END.

FUNCTION fListQuery RETURNS CHAR 
(icTables AS CHAR,
 icQuery AS CHAR):
   
   DEF VAR lhQuery AS HANDLE NO-UNDO. 
   DEF VAR lhTable AS HANDLE NO-UNDO. 
   DEF VAR liCount AS INT NO-UNDO. 
   DEF VAR lcResultTopStruct AS CHAR NO-UNDO. 
   DEF VAR lcResultArray AS CHAR NO-UNDO. 
   DEF VAR lcResultStruct AS CHAR NO-UNDO. 
   DEF VAR lhOrder AS HANDLE NO-UNDO. 
   DEF VAR lhOrderCustomer AS HANDLE NO-UNDO.
   DEF VAR lhOrderFusion AS HANDLE NO-UNDO. 

   IF gi_xmlrpc_error NE 0 THEN RETURN "".

   /* all dynamic objects to this */
   CREATE WIDGET-POOL "FusionQuery".
 
   DEF VAR liEntry AS INT NO-UNDO.
   CREATE QUERY lhQuery IN WIDGET-POOL "FusionQuery".
   DO liEntry = 1 TO NUM-ENTRIES(icTables):

     CREATE BUFFER lhTable FOR TABLE ENTRY(liEntry, icTables) IN WIDGET-POOL "FusionQuery".
     lhQuery:ADD-BUFFER(lhTable).

   END.
   
   IF NOT lhQuery:QUERY-PREPARE(icQuery) THEN DO:
      DELETE WIDGET-POOL "FusionQuery".
      RETURN appl_err("Error in query prepare: " + icQuery).
   END.
   
   lhQuery:FORWARD-ONLY = TRUE. 
   lhQuery:QUERY-OPEN.

   lcresultTopstruct = add_struct(response_toplevel_id, "").
   lcResultArray = add_array(lcresultTopstruct, "results").

   REPEAT:
      
      lhQuery:GET-NEXT(NO-LOCK).
      IF lhQuery:QUERY-OFF-END THEN LEAVE.
         
      lhOrderFusion     = lhQuery:GET-BUFFER-HANDLE("OrderFusion").
      IF NOT lhOrderFusion:BUFFER-FIELD("FusionStatus"):BUFFER-VALUE > "" 
         THEN NEXT.
      
      liCount = liCount + 1.
      IF liCount <= liOffSet THEN NEXT.
      IF liCount > liLimit + liOffSet THEN LEAVE.

      ASSIGN
         lhOrderCustomer   = lhQuery:GET-BUFFER-HANDLE("OrderCustomer")
         lhOrder           = lhQuery:GET-BUFFER-HANDLE("Order").

      lcResultStruct = add_struct(lcResultArray,"").
      add_int(lcResultStruct, "order_id", 
         int(lhOrderFusion:BUFFER-FIELD("OrderId"):BUFFER-VALUE)). 

      add_string(lcResultStruct, "customer_id", 
         lhOrderCustomer:BUFFER-FIELD("CustId"):BUFFER-VALUE). 

      add_string(lcResultStruct, "customer_id_type", 
         lhOrderCustomer:BUFFER-FIELD("CustIdType"):BUFFER-VALUE). 

      add_string(lcResultStruct, "msisdn", 
         lhOrder:BUFFER-FIELD("CLI"):BUFFER-VALUE). 

      add_string(lcResultStruct, "fixed_line_order_id", 
         lhOrderFusion:BUFFER-FIELD("FixedOrderId"):BUFFER-VALUE). 

      add_string(lcResultStruct, "salesman", 
         lhOrder:BUFFER-FIELD("Salesman"):BUFFER-VALUE). 

      add_string(lcResultStruct, "orderchannel", 
         lhOrder:BUFFER-FIELD("orderchannel"):BUFFER-VALUE). 

      add_string(lcResultStruct, "fusion_order_status", 
         lhOrderFusion:BUFFER-FIELD("FusionStatus"):BUFFER-VALUE). 

      add_date_or_time(lcResultStruct, "order_date", 
         lhOrderFusion:BUFFER-FIELD("OrderDate"):BUFFER-VALUE, 0). 
   END.

   /* add_int(lcResultStruct, "total_amount", liCount).  */

   lhQuery:QUERY-CLOSE().
   DELETE WIDGET-POOL "FusionQuery".

   RETURN "".

END FUNCTION. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcInputStruct = get_struct(param_toplevel_id,"0").
IF gi_xmlrpc_error NE 0 THEN RETURN.
lcInputFields = validate_request(pcInputStruct,"order_id,fixed_line_order_id,msisdn,fixed_line_number,fusion_order_status,customer_id,customer_id_type,salesman,limit,sort_order,offset").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   liOrderId = get_int(pcInputStruct,"order_id")
      WHEN LOOKUP("order_id",lcInputFields) > 0 
   lcFixedLineOrderID = get_string(pcInputStruct,"fixed_line_order_id")
      WHEN LOOKUP("fixed_line_order_id",lcInputFields) > 0 
   lcMsisdn = get_string(pcInputStruct,"msisdn")
      WHEN LOOKUP("msisdn",lcInputFields) > 0 
   lcFixedLineNumber = get_string(pcInputStruct,"fixed_line_number")
      WHEN LOOKUP("fixed_line_number",lcInputFields) > 0 
   lcFusionOrderStatus = get_string(pcInputStruct,"fusion_order_status")
      WHEN LOOKUP("fusion_order_status",lcInputFields) > 0 
   lcCustomerId = get_string(pcInputStruct,"customer_id")
      WHEN LOOKUP("customer_id",lcInputFields) > 0 
   lcCustomerIdType = get_string(pcInputStruct,"customer_id_type")
      WHEN LOOKUP("customer_id_type",lcInputFields) > 0 
   lcSalesman = get_string(pcInputStruct,"salesman")
      WHEN LOOKUP("salesman",lcInputFields) > 0 
   liLimit = get_int(pcInputStruct,"limit")
      WHEN LOOKUP("limit",lcInputFields) > 0 
   lcSortOrder = get_string(pcInputStruct,"sort_order")
      WHEN LOOKUP("sort_order",lcInputFields) > 0
   liOffSet = get_int(pcInputStruct,"offset")
      WHEN LOOKUP("offset",lcInputFields) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF lcSortOrder > "" AND
   LOOKUP(lcSortOrder,"ascending,descending") = 0 THEN
   RETURN appl_err(SUBST("Incorrect sort_order value &1", lcSortOrder)).
   
IF lcCustomerIdType > "" AND
   lcCustomerId > "" THEN DO:

   lcQuery = 
      'FOR EACH OrderCustomer NO-LOCK WHERE' + 
              ' OrderCustomer.Brand = ' + QUOTER(gcBrand) +
              ' AND OrderCustomer.CustIdType = ' + QUOTER(lcCustomerIdType) +
              ' AND OrderCustomer.CustId = ' + QUOTER(lcCustomerId) +
              ' AND OrderCustomer.RowType = 1'.
   IF liOrderId > 0 THEN
      lcQuery = lcQuery + " AND OrderCustomer.OrderId = " + QUOTER(liOrderId).
   
   ASSIGN
      lcQuery = lcQuery + ', EACH OrderFusion NO-LOCK WHERE' +
                ' OrderFusion.Brand = OrderCustomer.Brand' +
                ' AND OrderFusion.OrderID = OrderCustomer.OrderId'
      lcBuffers = "OrderCustomer,OrderFusion,Order"
      llAnd = TRUE.
   
END.
ELSE IF lcMsisdn > "" THEN ASSIGN
      lcQuery = 'FOR EACH Order NO-LOCK WHERE' +
             ' Order.Brand = ' + QUOTER(gcBrand) +
             ' AND Order.CLI = ' + QUOTER(lcMsisdn) +
             (IF liOrderId > 0
              THEN ' AND Order.OrderId = ' + QUOTER(liOrderId)
              ELSE "") +
             ' AND Order.orderchannel BEGINS "fusion"' +
             ', EACH OrderFusion NO-LOCK WHERE' +
             ' OrderFusion.Brand = Order.Brand' +
             ' AND OrderFusion.OrderID = Order.OrderId'
      llAnd = TRUE
      lcBuffers = "Order,OrderFusion,OrderCustomer".
ELSE IF liOrderId > 0 THEN ASSIGN
   lcQuery = 'FOR EACH OrderFusion NO-LOCK WHERE' +
             ' OrderFusion.Brand = ' + QUOTER(gcBrand) +
             ' AND OrderFusion.OrderID = ' + QUOTER(liOrderId)
   llAnd = TRUE
   lcBuffers = "OrderFusion,Order,OrderCustomer".

IF lcQuery EQ "" THEN ASSIGN
   lcBaseQuery = "FOR EACH OrderFusion NO-LOCK WHERE"
   llAnd = FALSE
   lcQuery = lcBaseQuery
   lcBuffers = "OrderFusion,Order,OrderCustomer".

IF lcFixedLineOrderID > "" THEN
   fAddQueryCondition('OrderFusion.FixedOrderId = ' +
                       QUOTER(lcFixedLineOrderId)).

IF lcFixedLineNumber > "" THEN
   fAddQueryCondition("OrderFusion.FixedNumber = " +
                      QUOTER(lcFixedLineNumber)).

IF lcSalesman > "" THEN
   fAddQueryCondition("OrderFusion.Salesman = " + QUOTER(lcSalesMan)).

/* legacy status PFIN is converted to FIN and order status 79 */
IF lcFusionOrderStatus > "" THEN DO:
   fAddQueryCondition("OrderFusion.FusionStatus = " +
       (IF lcFusionOrderStatus EQ "PFIN"
        THEN QUOTER({&FUSION_ORDER_STATUS_FINALIZED})
        ELSE QUOTER(lcFusionOrderStatus))).
END.

IF lcSubQuery > "" THEN 
   lcQuery = lcQuery + lcSubQuery.

IF lcQuery EQ lcBaseQuery THEN
   RETURN appl_err("At least one search parameter is mandatory").

IF NOT lcMsisdn > "" THEN
   lcQuery = lcQuery + ', EACH Order NO-LOCK WHERE' +
                      ' Order.Brand = OrderFusion.Brand' + 
                       ' AND Order.OrderId = OrderFusion.OrderId'.

IF lcFusionOrderStatus EQ "PFIN" THEN
   lcQuery = lcQuery + " AND Order.StatusCode = " +
                      QUOTER({&ORDER_STATUS_PENDING_MOBILE_LINE}).

IF lcMsisdn > "" THEN
   lcQuery = lcQuery + ' AND Order.CLI = ' + QUOTER(lcMsisdn).

IF NOT (lcCustomerIdType > "" AND lcCustomerId > "") THEN
   lcQuery = lcQuery + ', EACH OrderCustomer NO-LOCK WHERE' + 
                       ' OrderCustomer.Brand = Order.Brand' + 
                       ' AND OrderCustomer.OrderId = Order.OrderId' +
                       ' AND OrderCustomer.RowType = 1'.

IF lcSortOrder > "" AND
   lcSortOrder EQ "ascending" THEN lcQuery = lcQuery + ' BY OrderDate'.
ELSE lcQuery = lcQuery + ' BY OrderDate DESC'.

fListQuery(lcBuffers,lcQuery).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
