/**
 * YCO-121, YI-051, YBP-503
 * Search orders. Takes array of 4 parameters as an input. The meaning of these
 * are the following:
 *
 * @input  key;string;mandatory;orderid if type is "tms_id", MSISDN CLI if type is "msisdn", CustId if type is "custid";
           type;string;type of the search;has value "tms_id", "msisdn" or "custnum". See param key definition.
           idtype;string;mandatory;customer id type;used when type field has value "custid", empty otherwise
           maxcount;int;mandatory;maximum number of orders returned;
 * @output orders;array;a list of order structs
 * @orderstruct  tms_id;int;OrderId
                 custid;string;Customer Id
                 cli;string;MSISDN CLI value
                 fname;string;first name of the order customer
                 lname;string;last name of the order customer
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcSearchString AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcSearchType   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcCustIdType   AS CHARACTER NO-UNDO.
DEFINE VARIABLE piMaxCount     AS INTEGER   NO-UNDO. 
DEFINE VARIABLE top_array      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError        AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "string,string,string,int") EQ ? THEN RETURN.

pcSearchString = get_string(param_toplevel_id, "0").
pcSearchType   = get_string(param_toplevel_id, "1").
pcCustIdType   = get_string(param_toplevel_id, "2").
piMaxCount     = get_int(   param_toplevel_id, "3").

IF gi_xmlrpc_error NE 0 THEN RETURN.

top_array = add_array(response_toplevel_id, "").

FUNCTION fAddOrderStruct RETURN LOGICAL (INPUT piCountOrder AS INTEGER):
   DEFINE VARIABLE lcOrderStruct AS CHARACTER NO-UNDO. 
   lcOrderStruct = add_struct(top_array,"").

   add_int(   lcOrderStruct, "tms_id" , Order.OrderId          ).
   add_int(   lcOrderStruct, "custnum", Order.CustNum          ).
   add_string(lcOrderStruct, "cli"    , Order.CLI              ).
   add_string(lcOrderStruct, "fname"  , OrderCustomer.FirstName).
   add_string(lcOrderStruct, "lname"  , OrderCustomer.SurName1 ).
   IF AVAIL OrderFusion THEN  
      add_string(lcOrderStruct, "fixed_number", OrderFusion.fixednumber).
END.


FUNCTION fIsRecordsAvailable RETURN LOGICAL:
   
   FIND FIRST OrderCustomer WHERE 
        OrderCustomer.Brand = "1"             AND
        OrderCustomer.OrderId = Order.OrderId AND
        OrderCustomer.RowType = 1 
        NO-LOCK NO-ERROR.

   RETURN AVAILABLE OrderCustomer.

END.

FUNCTION fCheckFixedNumber RETURN LOGICAL:
   
   FIND FIRST OrderFusion WHERE 
              OrderFusion.Brand = "1" AND
              OrderFusion.OrderId EQ Order.OrderId
              NO-LOCK NO-ERROR.

   RETURN AVAILABLE OrderFusion.

END.

FUNCTION fAddOrdersBasedOnCLI RETURN CHARACTER:
   DEFINE VARIABLE iCount AS INTEGER NO-UNDO. 
   iCount = 0.
   OrdersBasedOnCLI:
   FOR EACH Order WHERE 
      Order.Brand = "1" AND 
      Order.CLI = pcSearchString NO-LOCK:
      IF NOT fIsRecordsAvailable() THEN 
         NEXT OrdersBasedOnCLI.
      fCheckFixedNumber().

      fAddOrderStruct(iCount).
      iCount = iCount + 1.
      IF iCount = piMaxCount THEN
         LEAVE OrdersBasedOnCLI.
   END.
   RETURN "".
END.

FUNCTION fAddOrdersBasedOnFixed RETURN CHARACTER:
   DEFINE VARIABLE iCount AS INTEGER NO-UNDO. 
   iCount = 0.
   OrdersBasedOnFixed:
   FOR EACH OrderFusion WHERE 
      OrderFusion.FixedNumber = pcSearchString NO-LOCK:
      FOR EACH Order WHERE
               Order.Brand = "1" AND
               Order.OrderId = OrderFusion.OrderId NO-LOCK:
         IF NOT fIsRecordsAvailable() THEN 
            NEXT OrdersBasedOnFixed.

         fAddOrderStruct(iCount).
         iCount = iCount + 1.
         IF iCount = piMaxCount THEN
            LEAVE OrdersBasedOnFixed.
      END.
   END.
   RETURN "".
END.


FUNCTION fAddOrdersBasedOnCustId RETURN CHARACTER:
   
   DEFINE VARIABLE iiCount AS INTEGER NO-UNDO INIT 0. 
   
   OrdersBasedOnCustID:
   FOR EACH OrderCustomer WHERE 
            OrderCustomer.Brand = "1" AND
            OrderCustomer.CustIdType = pcCustIdType AND
            OrderCustomer.CustId = pcSearchString AND
            OrderCustomer.Rowtype = 1 NO-LOCK:
      FOR EACH Order WHERE
               Order.Brand = "1" AND
               Order.OrderId = OrderCustomer.OrderId NO-LOCK:
      fCheckFixedNumber().
      fAddOrderStruct(iiCount).
      iiCount = iiCount + 1.
      IF iiCount = piMaxCount THEN
         LEAVE OrdersBasedOnCustID.
      END.
   END.
   RETURN "".
END.


FUNCTION fAddOrdersBasedOnOrderId RETURN CHARACTER:
   DEFINE VARIABLE iCount   AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iOrderId AS INTEGER NO-UNDO. 
   iOrderId = INTEGER(pcSearchString) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      RETURN "Search string was not integer as order ID should be".

   iCount = 0.
   OrdersBasedOnOrderId:
   FOR EACH Order WHERE 
      Order.Brand = "1"        AND 
      Order.OrderId = iOrderId 
      NO-LOCK:

      IF NOT fIsRecordsAvailable() THEN 
         NEXT OrdersBasedOnOrderId.

      fCheckFixedNumber().
      fAddOrderStruct(iCount).
      iCount = iCount + 1.
      IF iCount = piMaxCount THEN
         LEAVE OrdersBasedOnOrderId.
   END.
   RETURN "".
END.

lcError = "".

CASE pcSearchType:
   WHEN "tms_id"  THEN lcError = fAddOrdersBasedOnOrderId().
   WHEN "custid" THEN lcError = fAddOrdersBasedOnCustId().
   WHEN "msisdn"  THEN lcError = fAddOrdersBasedOnCLI(). 
   WHEN "fixed_number" THEN lcError = fAddOrdersBasedOnFixed().
   OTHERWISE 
      lcError = "Invalid search_type " + pcSearchType.
END.

IF lcError NE "" THEN RETURN appl_err(lcError).
