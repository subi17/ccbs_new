/**
 * newton__queue_orders__list.p

 * @input   status;int;mandatory;order status code.
            offset;int;optional;number of skipped request from the beginning of the found requests
            limit;int;optional;maximum number of returned requests (maximum 1000)

 * @output  orders;array;array of order structs containing information of a single order            
 * @order   created_at;timestamp;timestamp of order creation
            customer_id;int;customer id
            msisdn;string;msisdn
            name;string;name of the customer of the order
            contract_id;string; order contract id
            order_id;int;order id 
 *
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
gcBrand = "1".  

DEFINE VARIABLE pcTenant AS CHARACTER NO-UNDO. 
DEFINE VARIABLE piStatus AS INTEGER NO-UNDO. 
DEFINE VARIABLE piOffset AS INTEGER NO-UNDO. 
DEFINE VARIABLE piLimit  AS INTEGER NO-UNDO INIT 1000000. 
DEFINE VARIABLE lcParams AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError  AS CHARACTER NO-UNDO. 

DEFINE VARIABLE gcParamStruct  AS CHARACTER NO-UNDO.  
DEFINE VARIABLE lcOutArray AS CHARACTER NO-UNDO.  
DEFINE VARIABLE order_struct   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iCount         AS INTEGER NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
IF gi_xmlrpc_error NE 0 THEN RETURN.

gcParamStruct = get_struct(param_toplevel_id, "").

lcParams = validate_request(gcParamStruct, "brand!,status!,offset,limit").
IF lcParams EQ ? THEN RETURN.
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcTenant = get_string(gcParamStruct, "brand"). 
piStatus = get_int(gcParamStruct, "status").
piOffset = get_int(gcParamStruct, "offset").

IF LOOKUP("limit", lcParams) > 0 THEN DO:
   piLimit = get_int(gcParamStruct, "limit").
   IF piLimit > 1000 THEN RETURN
      appl_err(SUBST("Given limit &1 is bigger than maximum limit 1000", piLimit)).
END.
IF LOOKUP("offset", lcParams) > 0 THEN
   piOffSet = get_int(gcParamStruct, "offset").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

FUNCTION fAddOrder RETURN LOGICAL:
  
   add_timestamp(order_struct, "created_at", Order.CrStamp).
   add_string(order_struct,"msisdn", Order.CLI).

    FIND OrderCustomer WHERE 
         OrderCustomer.Brand = gcBrand AND
         OrderCustomer.OrderId = Order.OrderId AND
         OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
    IF AVAIL OrderCustomer THEN DO:
       DEFINE VARIABLE lcAgrCustName AS CHARACTER NO-UNDO. 
       lcAgrCustName = DYNAMIC-FUNCTION("fDispOrderName" IN ghFunc1,
                                         BUFFER OrderCustomer).
       add_string(order_struct,"name",lcAgrCustName).
       add_string(order_struct,"customer_id",OrderCustomer.CustID).
    END.
   add_string(order_struct,"contract_id",Order.ContractId).
   add_int(order_struct,"order_id",Order.OrderId).

   RETURN TRUE.
END.


iCount = 0.
lcOutArray = add_array(response_toplevel_id,"").
OrderLoop:
FOR EACH Order WHERE 
    Order.Brand eq gcBrand AND 
    Order.StatusCode eq STRING(piStatus) NO-LOCK USE-INDEX StatusCode:
    IF iCount >= piOffset + piLimit THEN 
       LEAVE OrderLoop.
    IF iCount >= piOffset THEN DO:
       order_struct = add_struct(lcOutArray,"").
       fAddOrder().
    END.
    iCount = iCount + 1.
    IF iCount > piOffSet + 1000 
      THEN DO:
      lcError = "Limit overflow".
      LEAVE OrderLoop.
    END.
END.
   
IF lcError NE "" THEN RETURN appl_err(lcError).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
