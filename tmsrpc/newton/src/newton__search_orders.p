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

DEFINE VARIABLE pcTenant       AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcSearchString AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcSearchType   AS CHARACTER NO-UNDO.
/* DEFINE VARIABLE pcCustIdType   AS CHARACTER NO-UNDO.  YDR-2688 */
DEFINE VARIABLE piMaxCount     AS INTEGER   NO-UNDO. 
DEFINE VARIABLE top_array      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError        AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcTmp          AS CHARACTER NO-UNDO. /* YDR-2688 */
DEFINE VARIABLE liOwner        AS INTEGER   NO-UNDO. /* YDR-2688 */
DEFINE VARIABLE lcSearchBy     AS CHARACTER NO-UNDO. /* YDR-2688 */
DEFINE VARIABLE lii            AS INTEGER   NO-UNDO. /* YDR-2688 */ 
DEFINE VARIABLE lcDocTypes     AS CHARACTER NO-UNDO INITIAL "NIF,CIF,Passport,NIE". /* YDR-2688 */
DEFINE VARIABLE lcCustIdType   AS CHARACTER NO-UNDO.  /* YDR-2688 */
DEFINE VARIABLE liCount        AS INTEGER   NO-UNDO. 

/* IF validate_request(param_toplevel_id, "string,string,string,string,int") EQ ? THEN RETURN.  YDR-2688 */
IF validate_request(param_toplevel_id, "string,string,string,int") EQ ? THEN RETURN.

pcTenant       = get_string(param_toplevel_id, "0").
pcSearchString = get_string(param_toplevel_id, "1"). 
pcSearchType   = get_string(param_toplevel_id, "2").     /* YDR-2688 - Expecting/valids are: "msisdn,order_id,person_id,imsi" */
/* pcCustIdType   = get_string(param_toplevel_id, "3").     YDR-2688 */
piMaxCount     = get_int(   param_toplevel_id, "3").     /* YDR-2688 */

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

top_array = add_array(response_toplevel_id, "").

FUNCTION fAddOrderStruct RETURN LOGICAL ():

   DEFINE VARIABLE lcOrderStruct AS CHARACTER NO-UNDO. 
   DEF BUFFER OrderFusion FOR OrderFusion.

   lcOrderStruct = add_struct(top_array,"").

   add_int(   lcOrderStruct, "tms_id" , Order.OrderId          ).
   add_int(   lcOrderStruct, "custnum", Order.CustNum          ).
   add_string(lcOrderStruct, "cli"    , Order.CLI              ).
   add_string(lcOrderStruct, "fname"  , OrderCustomer.FirstName).
   add_string(lcOrderStruct, "lname"  , OrderCustomer.SurName1 ).
   
   FIND FIRST OrderFusion NO-LOCK WHERE 
              OrderFusion.Brand = Syst.Var:gcBrand AND
              OrderFusion.OrderId EQ Order.OrderId NO-ERROR.
   IF AVAIL OrderFusion THEN  
      add_string(lcOrderStruct, "fixed_number", OrderFusion.fixednumber).
      
   liCount = liCount + 1.
   RETURN (liCount < piMaxCount).

END.


FUNCTION fIsRecordsAvailable RETURN LOGICAL:
   
   FIND FIRST OrderCustomer WHERE 
        OrderCustomer.Brand = Syst.Var:gcBrand AND
        OrderCustomer.OrderId = Order.OrderId  AND
        OrderCustomer.RowType = 1 
        NO-LOCK NO-ERROR.

   RETURN AVAILABLE OrderCustomer.

END.

FUNCTION fAddOrdersBasedOnCLI RETURN CHARACTER:
  
   OrdersBasedOnCLI:
   FOR EACH Order WHERE 
            Order.Brand = Syst.Var:gcBrand AND 
            Order.CLI = pcSearchString AND
            Order.OrderType <= {&ORDER_TYPE_STC} NO-LOCK:
      
      IF NOT fIsRecordsAvailable() THEN 
         NEXT OrdersBasedOnCLI.

      IF NOT fAddOrderStruct() THEN
         LEAVE OrdersBasedOnCLI.
   END.
   RETURN "".
END.

FUNCTION fAddOrdersBasedOnFixed RETURN CHARACTER:

   OrdersBasedOnFixed:
   FOR EACH OrderFusion WHERE 
      OrderFusion.FixedNumber = pcSearchString NO-LOCK:
      FOR EACH Order WHERE
               Order.Brand = Syst.Var:gcBrand AND
               Order.OrderId = OrderFusion.OrderId AND
               Order.OrderType <= {&ORDER_TYPE_STC} NO-LOCK:
         
         IF NOT fIsRecordsAvailable() THEN 
            NEXT OrdersBasedOnFixed.

         IF NOT fAddOrderStruct() THEN
            LEAVE OrdersBasedOnFixed.
      END.
   END.
   RETURN "".
END.

FUNCTION fAddOrdersBasedOnCustId RETURN CHARACTER:
   
   OrdersBasedOnCustID:
   FOR EACH OrderCustomer WHERE 
            OrderCustomer.Brand = "1" AND
            OrderCustomer.CustIdType = lcCustIdType AND
            OrderCustomer.CustId = pcSearchString AND
            OrderCustomer.Rowtype = 1 NO-LOCK:
      FOR EACH Order WHERE
               Order.Brand = "1" AND
               Order.OrderId = OrderCustomer.OrderId AND
               Order.OrderType <= {&ORDER_TYPE_STC} NO-LOCK:
                   
         IF NOT fAddOrderStruct() THEN
            LEAVE OrdersBasedOnCustID.
      END.
   END.
   RETURN "".
END.

FUNCTION fAddOrdersBasedOnOrderId RETURN CHARACTER:

   DEFINE VARIABLE iOrderId AS INTEGER NO-UNDO. 
   iOrderId = INTEGER(pcSearchString) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      RETURN "Search string was not integer as order ID should be".

   OrdersBasedOnOrderId:
   FOR EACH Order WHERE 
            Order.Brand   = Syst.Var:gcBrand  AND 
            Order.OrderId = iOrderId AND
            Order.OrderType <= {&ORDER_TYPE_STC}
      NO-LOCK:

      IF NOT fIsRecordsAvailable() THEN 
         NEXT OrdersBasedOnOrderId.

      IF NOT fAddOrderStruct() THEN
         LEAVE OrdersBasedOnOrderId.
   END.
   RETURN "".
END.

lcError = "".

/* YDR-2688 - Calculating Convenient Search Mode */
lcTmp   = CAPS(SUBSTRING(pcSearchString, 9, 1)).
liOwner = INT(pcSearchString) NO-ERROR.

/* MSISDN search */    
IF LENGTH(pcSearchString) EQ 9 AND 
   (pcSearchString BEGINS "6" OR pcSearchString BEGINS "7") AND
   NOT (ASC(lcTmp) >= 65 AND
   ASC(lcTmp) <= 90) AND
   LOOKUP("msisdn", pcSearchType) > 0 THEN
 
        lcSearchBy = "msisdn".

/* Fixed line number search */
ELSE IF LENGTH(pcSearchString) EQ 9 AND
   (pcSearchString BEGINS "8" OR pcSearchString BEGINS "9" ) AND
   NOT (ASC(lcTmp) >= 65 AND
   ASC(lcTmp) <= 90) AND
   LOOKUP("msisdn", pcSearchType) > 0 THEN
   
        lcSearchBy = "fixed_number".

/* IMSI - It will use the msisdn */
ELSE IF LENGTH(pcSearchString) = 15 AND 
   pcSearchString BEGINS "21404" AND 
   LOOKUP("imsi", pcSearchType) > 0 THEN
DO:   
    FIND FIRST MobSub NO-LOCK WHERE
               MobSub.Brand = Syst.Var:gcBrand AND
               MobSub.IMSI  = pcSearchString NO-ERROR.
    IF NOT AVAIL MobSub THEN 
       RETURN appl_err(SUBSTITUTE("IMSI &1 not found", pcSearchString)).

    ASSIGN 
        pcSearchString   = MobSub.CLI
        lcSearchBy       = "msisdn".
END.
ELSE IF liOwner NE 0 AND 
   LOOKUP("order_id", pcSearchType) > 0 THEN
        
        lcSearchBy = "order_id".

ELSE IF LOOKUP("person_id", pcSearchType) > 0 THEN
DO:   
    FOR EACH Customer NO-LOCK WHERE
             Customer.OrgId = pcSearchString AND
             Customer.brand = Syst.Var:gcBrand AND
             Customer.Roles NE "inactive" 
             lii = 1 TO 2:
       IF lii > 1 THEN DO:
          IF LOOKUP("msisdn",pcSearchType) > 0 THEN
             RETURN appl_err("Please search with MSISDN").
       END.
    END.
    
    FIND FIRST Customer NO-LOCK WHERE
               Customer.OrgId = pcSearchString AND 
               Customer.brand = Syst.Var:gcBrand AND 
               Customer.Roles NE "inactive" NO-ERROR.
    IF NOT AVAILABLE Customer THEN
    DO:
       /* Trying an alternate way in case there is "future" customers that have one order but no subscriptions and customer id has not been created yet */
       DocTypes_blk:
       DO lii = 1 TO NUM-ENTRIES(lcDocTypes):
          FOR EACH OrderCustomer WHERE 
                   OrderCustomer.Brand = "1" AND
                   OrderCustomer.CustIdType = ENTRY(lii,lcDocTypes) AND
                   OrderCustomer.CustId = pcSearchString AND
                   OrderCustomer.Rowtype = 1 NO-LOCK:
             IF CAN-FIND(FIRST Order WHERE
                               Order.Brand = "1" AND
                               Order.OrderId = OrderCustomer.OrderId AND   
                               Order.OrderType <= {&ORDER_TYPE_STC} NO-LOCK) THEN 
             DO:
                IF NUM-ENTRIES(lcCustIdType) > 0 THEN 
                    lcCustIdType = lcCustIdType + "," + ENTRY(lii,lcDocTypes).
                ELSE 
                    lcCustIdType = ENTRY(lii,lcDocTypes).
                    
                NEXT DocTypes_blk.
             END.
          END.
       END.
       
       IF NUM-ENTRIES(lcCustIdType) = 0 THEN
          RETURN appl_err(SUBSTITUTE("Customer &1 not found", pcSearchString)). 
       ELSE
       DO:
          IF NUM-ENTRIES(lcCustIdType) > 1 THEN /* Unlikely situation where the search criteria can be found for more than one document type, i.e. NIF: 01234567A and Passport: 01234567A, 
                                                   so we cannot determine internally here the document type. */
             RETURN appl_err("Search criteria found for Doc Types: " + lcCustIdType + ". Please search using another criteria.").
          ELSE 
             ASSIGN
                lcSearchBy = "cust_id".
       END.
    END.
    ELSE
       ASSIGN
          lcCustIdType = customer.custidtype
          lcSearchBy   = "cust_id".
END.
ELSE liOwner = 0.


CASE lcSearchBy:
   WHEN "msisdn"       THEN lcError = fAddOrdersBasedOnCLI(). 
   WHEN "fixed_number" THEN lcError = fAddOrdersBasedOnFixed().
   WHEN "order_id"     THEN lcError = fAddOrdersBasedOnOrderId().
   WHEN "cust_id"      then lcError = fAddOrdersBasedOnCustId().   
   OTHERWISE 
      lcError = "Invalid search_type " + pcSearchType.
END.

IF lcError NE "" THEN RETURN appl_err(lcError).
