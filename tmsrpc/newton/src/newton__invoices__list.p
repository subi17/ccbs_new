/**
 * list invoices 
 *
 * @input conditions;struct;mandatory;search conditions 
 * @conditions  brand;string
                invoice_number_external;string;
                msisdn;string;
                invoice_date_start;date;
                invoice_date_end;date;
                dni;string;
                firstname;string;
                surname1;string;
                surname2;string;
                company;string;
                due_date_start;date;
                due_date_end;date;
                invoice_amount_from;double;
                invoice_amount_to;double;
                claim_status;string;
                order_id;int;order id
                customer_id;int; customer id
                bypass;boolean;skip one year invoice date limit rule
                offset;integer;mandatory;how many records to skip
                limit;integer;mandatatory;how many records to fetch

*  @output struct;array of invoices ids

*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Func/log.i}


DEF VAR gcBrand AS CHARACTER NO-UNDO INITIAL "1" .
DEF VAR pcSearchStruct AS CHARACTER NO-UNDO. 
DEF VAR lcSearchStruct AS CHARACTER NO-UNDO. 
DEF VAR pcTenant       AS CHARACTER NO-UNDO.

DEF VAR lcListInvIndexParam AS CHARACTER NO-UNDO. 
DEF VAR lcListInvOtherParam AS CHARACTER NO-UNDO. 
DEF VAR lcListCustIndexParam AS CHARACTER NO-UNDO. 
DEF VAR lcListSubInvParam    AS CHAR NO-UNDO.

DEF VAR lcQuery AS CHARACTER NO-UNDO.
DEF VAR lcError AS CHARACTER NO-UNDO. 
DEF VAR lcTables AS CHARACTER NO-UNDO. 

FUNCTION fSearchOrder RETURNS CHAR
         (OUTPUT ocError AS CHAR):
   
   DEFINE VARIABLE liKey AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcOrderQuery AS CHARACTER NO-UNDO INITIAL "". 

   liKey = get_int(pcSearchStruct,"order_id").

   FIND Order WHERE
        Order.Brand = gcBrand AND
        Order.OrderId = liKey NO-LOCK NO-ERROR.
   
   IF NOT AVAIL Order THEN ocError = "order not found".
   ELSE IF Order.InvNum NE 0 THEN 
      lcOrderQuery = 'FOR EACH Invoice NO-LOCK WHERE Invoice.InvNum = ' +  
                       STRING(Order.InvNum).
   /* use fake invoice number to generate empty result */
   ELSE lcOrderQuery = 'FOR EACH Invoice NO-LOCK WHERE Invoice.InvNum = -12345'.

   RETURN lcOrderQuery.

END FUNCTION.

FUNCTION fSubInvoiceQuery RETURNS CHAR:
   
   DEF VAR iCount AS INTEGER NO-UNDO.
   DEF VAR lcAddLine AS CHARACTER NO-UNDO. 
   DEF VAR lcEntry AS CHARACTER NO-UNDO. 
   DEF VAR lcSubInvQuery AS CHAR NO-UNDO.

   /* add subinvoice params */
   DO iCount = 1 TO NUM-ENTRIES(lcListSubInvParam):
      lcEntry = ENTRY(iCount,lcListSubInvParam).
      IF LOOKUP(lcEntry,lcSearchStruct) = 0 THEN NEXT.
      lcAddLine = "".
      CASE lcEntry:
            WHEN "msisdn" THEN 
                  lcAddLine = ' AND SubInvoice.CLI = ' + QUOTER(get_string(pcSearchStruct,lcEntry)).
      END CASE.
      lcSubInvQuery = lcSubInvQuery + lcAddLine.
   END.

   RETURN lcSubInvQuery.

END FUNCTION. 

FUNCTION fSearchCustomer RETURNS CHAR
    ( OUTPUT ocTables AS CHAR,
      OUTPUT ocError AS CHAR):

   DEFINE VARIABLE lcCustomerQuery AS CHARACTER NO-UNDO INITIAL "". 
   DEFINE VARIABLE liKey AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldInvDate AS DATE NO-UNDO.
   DEF VAR lcSubInvQuery AS CHAR NO-UNDO.

   liKey = get_int(pcSearchStruct,"customer_id"). 
   ldInvDate = get_date(pcSearchStruct,"invoice_date_start").
   lcCustomerQuery = 'FOR EACH Invoice NO-LOCK USE-INDEX CustNum ' +
                   ' WHERE Invoice.Brand = "1" ' + 
                   ' AND Invoice.CustNum = ' + STRING(liKey) + 
                   ' AND Invoice.invdate >= ' + STRING(ldInvDate) + 
                   ' AND (Invoice.invtype = 1  OR Invoice.invtype = 5 ) '.
   
   lcSubInvQuery = fSubInvoiceQuery().
   
   ocTables = "Invoice".
   IF lcSubInvQuery NE "" THEN DO:
      lcCustomerQuery = lcCustomerQuery + 
                      ', EACH SubInvoice OF Invoice NO-LOCK WHERE SubInvoice.Brand = "1" ' + lcSubInvQuery.
      ocTables = ocTables + ",SubInvoice".
   END.
   lcCustomerQuery = lcCustomerQuery + ' BY invoice.InvDate ' + 
                   ' BY invoice.InvNum  DESCENDING'. 

   RETURN lcCustomerQuery.

END FUNCTION. 

FUNCTION fSearchQuery RETURNS CHAR
         (OUTPUT pcTables AS CHAR,
          OUTPUT pcError AS CHAR):

   DEF VAR iCount AS INTEGER NO-UNDO.
   DEF VAR lcEntry AS CHARACTER NO-UNDO. 
   DEF VAR lcEntryValue AS CHARACTER NO-UNDO. 
   DEF VAR lcAddLine AS CHARACTER NO-UNDO. 
   DEF VAR lcCustQuery AS CHARACTER NO-UNDO. 
   DEF VAR lcInvQuery AS CHARACTER NO-UNDO.
   DEF VAR lcInvOtherQuery AS CHARACTER NO-UNDO.
   DEF VAR lcSubInvQuery AS CHAR NO-UNDO.
   DEF VAR lcSearchQuery AS CHARACTER NO-UNDO INITIAL "". 
   DEF VAR ldInvDateStart AS DATE NO-UNDO.
   DEf VAR ldInvDateEnd AS DATE NO-UNDO.
   DEF VAR ldDueDateStart AS DATE NO-UNDO.
   DEf VAR ldDueDateEnd AS DATE NO-UNDO.
   DEF VAR lcClaimStatus AS CHAR NO-UNDO. 
 
   pcError = "".
   /* customer index params */
   DO iCount = 1 TO NUM-ENTRIES(lcListCustIndexParam):
      lcEntry = ENTRY(iCount,lcListCustIndexParam).
      IF LOOKUP(lcEntry,lcSearchStruct) = 0 THEN NEXT.
      lcEntryValue = get_string(pcSearchStruct,lcEntry).
      lcAddLine = "".
      CASE lcEntry:
            WHEN "dni"           THEN lcAddLine = ' AND Customer.Orgid    = ' + QUOTER(lcEntryValue).
            WHEN "firstname"     THEN lcAddLine = ' AND Customer.FirstName = ' + QUOTER(lcEntryValue).
            WHEN "surname1"      THEN lcAddLine = ' AND Customer.CustName = ' + QUOTER(lcEntryValue).
            WHEN "surname2"      THEN lcAddLine = ' AND Customer.Surname2 = ' + QUOTER(lcEntryValue).
            WHEN "company"       THEN lcAddLine = ' AND Customer.Companyname = ' + QUOTER(lcEntryValue).
            WHEN "bank_account"  THEN lcAddLine = ' AND Customer.BankAcct = ' + QUOTER(lcEntryValue).
       END CASE.
       lcCustQuery = lcCustQuery + lcAddLine.
   END.
   
   /* invoice index params */
   DO iCount = 1 TO NUM-ENTRIES(lcListInvIndexParam):
       lcEntry = ENTRY(iCount,lcListInvIndexParam).
       IF LOOKUP(lcEntry,lcSearchStruct) = 0 THEN NEXT.
       lcAddLine = "".
       CASE lcEntry:
            WHEN "invoice_number_external" THEN 
                  lcAddLine = ' AND  Invoice.ExtInvID = ' + QUOTER(get_string(pcSearchStruct,lcEntry)).
            WHEN "invoice_date_start" THEN DO:
                  ldInvDateStart = get_date(pcSearchStruct,lcEntry). 
                  lcAddLine = ' AND Invoice.InvDate >= ' + STRING(ldInvDateStart).
            END.
            WHEN "invoice_date_end" THEN DO:
                  ldInvDateEnd = get_date(pcSearchStruct,lcEntry).
                  lcAddLine = ' AND Invoice.InvDate <= ' + STRING(ldInvDateEnd).
            END.
       END CASE.
       lcInvQuery = lcInvQuery + lcAddLine.
   END.

   /* check Invoice dates */
   IF ldInvDateStart > ldInvDateEnd THEN DO:
      pcError = "Incorrect Invoice Date start-end selection".
      RETURN "".
   END.

   lcSubInvQuery = fSubInvoiceQuery().

   IF lcInvQuery EQ "" AND lcCustQuery EQ "" AND 
      lcSubInvQuery EQ "" THEN DO:
      pcError = "Search criteria doesn't included at least one of the indexed fields!".
      RETURN "".
   END.

   /* let add now the others params */
   DO iCount = 1 TO NUM-ENTRIES(lcListInvOtherParam):
      lcEntry = ENTRY(iCount,lcListInvOtherParam).
      IF LOOKUP(lcEntry,lcSearchStruct) = 0 THEN NEXT.
      lcAddLine = "".
      CASE lcEntry:
            WHEN "invoice_amount_from" THEN 
                  lcAddLine = ' AND  Invoice.InvAmt >= ' + 
                  QUOTER(STRING(get_double(pcSearchStruct,lcEntry))).
            WHEN "invoice_amount_to" THEN 
                  lcAddLine = ' AND  Invoice.InvAmt <= ' + 
                  QUOTER(STRING(get_double(pcSearchStruct,lcEntry))).
            WHEN "claim_status" THEN DO:
                  lcClaimStatus = get_string(pcSearchStruct,lcEntry).
                  IF lcClaimStatus EQ "0" OR lcClaimStatus EQ "0.0" THEN ASSIGN
                     lcAddLine = ' AND (Invoice.ClaimStatus = "0.0"' +
                                 ' OR Invoice.ClaimStatus = "")'.
                  ELSE ASSIGN
                     lcAddLine = ' AND Invoice.ClaimStatus = ' +
                     QUOTER(lcClaimStatus).
            END.
            WHEN "due_date_start" THEN DO:
                  ldDueDateStart =  get_date(pcSearchStruct,lcEntry).
                  lcAddLine = ' AND Invoice.DueDate >= ' + STRING(ldDueDateStart).
            END.
            WHEN "due_date_end" THEN DO:
                  ldDueDateEnd = get_date(pcSearchStruct,lcEntry).
                  lcAddLine = ' AND Invoice.DueDate <= ' + STRING(ldDueDateEnd).
            END.
      END CASE.
      lcInvOtherQuery = lcInvOtherQuery + lcAddLine.
   END.

   /* check Invoice dates */
   IF ldDueDateStart > ldDueDateEnd THEN DO:
      pcError = "Incorrect Invoice due date start-end selection".
      RETURN "".
   END.

   IF lcCustQuery NE "" THEN DO:
      lcSearchQuery = 'FOR EACH Customer NO-LOCK WHERE Customer.Brand = "1" ' + lcCustQuery +
                      ', EACH Invoice OF Customer NO-LOCK WHERE Invoice.Brand = "1" ' + lcInvQuery + lcInvOtherQuery.
      pcTables = "Customer,Invoice".
   END.
   ELSE IF lcInvQuery NE "" AND lcSubInvQuery EQ "" THEN DO: 
      lcSearchQuery = 'FOR EACH Invoice NO-LOCK WHERE Invoice.Brand = "1" ' + lcInvQuery + lcInvOtherQuery + 
                      ' , EACH Customer NO-LOCK WHERE Customer.Brand = "1" AND Customer.CustNum = Invoice.CustNum' .
      pcTables = "Invoice,Customer".
   END.
   
   IF lcSubInvQuery NE "" THEN DO:

      IF lcSearchQuery NE "" THEN DO:
         lcSearchQuery = lcSearchQuery + 
                         ', EACH SubInvoice OF Invoice NO-LOCK WHERE SubInvoice.Brand = "1" ' + lcSubInvQuery.
         pcTables = pcTables + ",SubInvoice".
      END.
      ELSE DO:
         lcSearchQuery = 'FOR EACH SubInvoice NO-LOCK WHERE SubInvoice.Brand = "1" ' + lcSubInvQuery + ' , EACH Invoice OF SubInvoice NO-LOCK WHERE Invoice.Brand = "1"' + lcInvQuery + lcInvOtherQuery.
         pcTables = "SubInvoice,Invoice".
      END.
   END.

   RETURN lcSearchQuery. 
END FUNCTION.

FUNCTION fListQuery RETURNS CHAR 
(icTables AS CHAR,
 icQuery AS CHAR,
 icIdField AS CHAR):
   
   DEF VAR lhQuery AS HANDLE NO-UNDO. 
   DEF VAR lhTable AS HANDLE NO-UNDO. 
   DEF VAR lhInvoice AS HANDLE NO-UNDO.
   DEF VAR liLimit AS INTEGER NO-UNDO INIT 10000000. 
   DEF VAR liOffSet AS INTEGER NO-UNDO. 
   DEF VAR liCount AS INTEGER NO-UNDO. 
   DEF VAR lcIdStruct AS CHARACTER NO-UNDO.
   DEf VAR lcResultStruct AS CHARACTER NO-UNDO. 
   DEF VAR ldLimitDate AS DATE NO-UNDO. 
   DEF VAR liEntry AS INT NO-UNDO.
   DEF VAR lcInvTypeList AS CHAR NO-UNDO INITIAL "1,5,6,7,8,9".
   DEF VAR lcInvNums AS CHAR NO-UNDO. 
   DEF VAR lcInvNum AS CHAR NO-UNDO. 
   DEF VAR llBypassDateCheck AS LOG NO-UNDO. 

   IF (day(today) = 29 AND month(today) = 2 ) THEN
     ldLimitDate = date(month(today),day(today) - 1,year(today) - 1).
   ELSE
     ldLimitDate = date(month(today),day(today),year(today) - 1).

   IF LOOKUP("limit",lcSearchStruct) > 0 THEN 
      liLimit = get_int(pcSearchStruct,"limit").
   IF LOOKUP("offset",lcSearchStruct) > 0 THEN 
      liOffSet = get_int(pcSearchStruct,"offset").
   IF LOOKUP("credit",lcSearchStruct) > 0 THEN DO:
      IF get_bool(pcSearchStruct,"credit") THEN lcInvTypeList = "5,8,9".
      ELSE lcInvTypeList = "1,6,7".
   END.
   IF LOOKUP("bypass",lcSearchStruct) > 0 THEN
      llBypassDateCheck = get_bool(pcSearchStruct,"bypass").
   IF gi_xmlrpc_error NE 0 THEN RETURN "".
 

   /* all dynamic objects to this */
   CREATE WIDGET-POOL "InvoiceQuery".
   
   CREATE QUERY lhQuery IN WIDGET-POOL "InvoiceQuery".

   DO liEntry = 1 TO NUM-ENTRIES(icTables):
     CREATE BUFFER lhTable FOR TABLE ENTRY(liEntry, icTables) IN WIDGET-POOL "InvoiceQuery".
     lhQuery:ADD-BUFFER(lhTable).
   END.
  
   IF NOT lhQuery:QUERY-PREPARE(icQuery) THEN DO:
      DELETE WIDGET-POOL "InvoiceQuery".
      RETURN appl_err("Error in preparing the query: " + icQuery).
   END.
     
   lhQuery:FORWARD-ONLY = TRUE. 
   lhQuery:QUERY-OPEN.

   lcResultStruct = add_struct(response_toplevel_id, "").
   lcIdstruct = add_array(lcResultStruct, "results").

   REPEAT:
      
      lhQuery:GET-NEXT(NO-LOCK).
      IF lhQuery:QUERY-OFF-END THEN LEAVE.
      
      lhInvoice = lhQuery:GET-BUFFER-HANDLE("Invoice").
      IF LOOKUP(STRING(lhInvoice::InvType),lcInvTypeList) = 0 THEN NEXT.
      
      IF NOT llBypassDateCheck AND lhInvoice::InvDate < ldLimitDate THEN NEXT.
      
      lcInvNum = STRING(lhInvoice:BUFFER-FIELD(icIdField):BUFFER-VALUE).
      
      IF LOOKUP(lcInvNum, lcInvNums) > 0 THEN NEXT.
      lcInvNums = lcInvNum + ",".

      liCount = liCount + 1.
      IF liCount <= liOffSet THEN NEXT.
      IF liCount > liLimit + liOffSet THEN NEXT.
      add_string(lcIdStruct, "", lcInvNum). 
   END.

   add_int(lcResultStruct, "total_amount", liCount). 

   lhQuery:QUERY-CLOSE().
   DELETE WIDGET-POOL "InvoiceQuery".

   RETURN "".

END FUNCTION. 


/* --- MAIN --- */
IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcSearchStruct = get_struct(param_toplevel_id, "0").

lcListInvIndexParam = "brand!,invoice_number_external,invoice_date_start,invoice_date_end".
lcListCustIndexParam = "dni,firstname,surname1,surname2,company,bank_account".
lcListInvOtherParam = "due_date_start,due_date_end,invoice_amount_from,invoice_amount_to,claim_status,bypass".
lcListSubInvParam = "msisdn".

lcSearchStruct = validate_struct(pcSearchStruct, lcListInvIndexParam  + "," +
                                                 lcListCustIndexParam + "," +
                                                 lcListInvOtherParam  + "," +
                                                 lcListSubInvParam    + "," +
                                                 "credit,order_id,customer_id,offset,limit").
IF lcSearchStruct = ? THEN RETURN.

pcTenant = get_string(pcSearchStruct,"brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant} 

/* direct seach */
IF LOOKUP("customer_id",lcSearchStruct) NE 0 THEN DO:
   
   lcSearchStruct = validate_struct(pcSearchStruct,
      "brand!,customer_id!,invoice_date_start!,credit,offset,limit,bypass" + "," + lcListSubInvParam).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   lcQuery = fSearchCustomer(
      OUTPUT lcTables,
      OUTPUT lcError).
END.
ELSE IF LOOKUP("order_id",lcSearchStruct) NE 0 THEN DO:
   
   lcSearchStruct = validate_struct(pcSearchStruct, 
      "brand!,order_id!,credit,offset,limit,bypass").
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   lcQuery = fSearchOrder(OUTPUT lcError).
   lcTables = "Invoice".
END.
ELSE DO:
   
   lcSearchStruct = validate_struct(pcSearchStruct, lcListInvIndexParam  + "," +
                                                    lcListCustIndexParam + "," +
                                                    lcListInvOtherParam  + "," + 
                                                    lcListSubInvParam    + "," + 
                                                    "credit,offset,limit").
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   lcQuery = fSearchQuery(OUTPUT lcTables, 
                          OUTPUT lcError).
END.

IF gi_xmlrpc_error NE 0 THEN RETURN.
IF lcError NE "" THEN RETURN appl_err(lcError).
IF lcQuery EQ "" THEN RETURN appl_err("Unsupported search criteria").

fListQuery(
   lcTables,
   lcQuery,
   "InvNum").

