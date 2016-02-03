/**
 * newton__request__search.p
 * @input  conditions;struct;mandatory; search conditions 
           offset;int;mandatory;number of skipped request from the beginning of the found requests
           limit;int;mandatory;maximum number of returned requests (maximum 1000)
 * @conditions type;int; request type
               msisdn;string; msisdn of the request
               customer_number;int; customer number (custnum)
               status;int; request status
 * @output  requests;array;array of request structs containing information of a single request            
 * @request id;int;request id
            msisdn;string;msisdn of the request
            customer_number;int; custmoer number (custnum)
            name;string;name of the customer of the request
            activated_at;timestamp;timestamp of the request activation
            handled_at;timestamp;timestamp of the request handling
            type;int;request type
            status;int;request status
 */
{xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE gcBrand AS CHARACTER INIT "1" NO-UNDO. 
DEFINE VARIABLE katun AS CHARACTER NO-UNDO. 

&SCOPED-DEFINE BrandVarDefined YES
{Func/func.i}

DEFINE VARIABLE piOffset AS INTEGER NO-UNDO. 
DEFINE VARIABLE piLimit AS INTEGER NO-UNDO. 
DEFINE VARIABLE pcSearchStruct AS CHARACTER NO-UNDO. 

DEFINE VARIABLE request_array  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE request_struct AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcSearchStruct AS CHARACTER NO-UNDO.  
DEFINE VARIABLE lcListOtherParam AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liParam AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcEntry AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcAddLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcQuery AS CHARACTER NO-UNDO. 

DEF VAR lhQuery AS HANDLE NO-UNDO. 
DEF VAR lhTable AS HANDLE NO-UNDO.  
DEF VAR liNum   AS INTEGER NO-UNDO. 
DEF VAR lhMsRequest AS HANDLE NO-UNDO.

FUNCTION fAddCustomerData RETURN LOGICAL:
   FIND Customer WHERE Customer.CustNum = lhMsRequest::CustNum NO-LOCK NO-ERROR.
   IF AVAIL Customer THEN DO:
      add_string(request_struct, "name", fDispCustName(BUFFER Customer)). 
      add_int(request_struct,"customer_number",Customer.CustNum).
   END.
   RETURN TRUE.
END.

FUNCTION fAddMsRequest RETURN LOGICAL:
   request_struct = add_struct(request_array, "").
   add_int(request_struct, "id", lhMsRequest::MsRequest).
   add_string(request_struct, "msisdn", lhMsRequest::CLI).
   fAddCustomerData().
   add_timestamp(request_struct, "activated_at", lhMsRequest::ActStamp).
   add_timestamp(request_struct, "handled_at", lhMsRequest::DoneStamp).
   add_int(request_struct, "type", lhMsRequest::ReqType).
   add_int(request_struct, "status", lhMsRequest::ReqStatus).
   RETURN TRUE.
END.

IF validate_request(param_toplevel_id, "struct,int,int") EQ ? THEN RETURN.
pcSearchStruct = get_struct(param_toplevel_id, "0").
piOffSet = get_int(param_toplevel_id, "1").
piLimit = get_int(param_toplevel_id, "2").
IF piLimit > 1000 THEN
appl_err(SUBST("Given limit &1 is bigger than maximum limit 1000", piLimit)).

lcListOtherParam = "msisdn,customer_number".

lcSearchStruct = validate_struct(pcSearchStruct, "type!,status," +
                                                 lcListOtherParam).
IF gi_xmlrpc_error NE 0 THEN RETURN.

/*mandatory parameter for search*/
lcQuery = "FOR EACH MsRequest NO-LOCK WHERE MsRequest.Brand = '1' AND " +
          "MsRequest.ReqType = " + STRING(get_int(pcSearchStruct,"type")).

/* msisdn and customer id param */
DO liParam = 1 TO NUM-ENTRIES(lcListOtherParam):
    lcEntry = ENTRY(liParam,lcListOtherParam).
    IF LOOKUP(lcEntry,lcSearchStruct) = 0 THEN NEXT.
    lcAddLine = "".
    CASE lcEntry:
         WHEN "msisdn"   THEN lcAddLine = " AND MsRequest.CLI    = " +
                                    QUOTER(get_string(pcSearchStruct,lcEntry)).
         WHEN "customer_number" THEN 
                lcAddLine = " AND MsRequest.CustNum = " + STRING(get_int(pcSearchStruct,lcEntry)).
    END CASE.
    lcQuery = lcQuery + lcAddLine.
END.
/* finally the status */
IF LOOKUP("status",lcSearchStruct) > 0 THEN 
   lcQuery = lcQuery + " AND MsRequest.ReqStatus = " + STRING(get_int(pcSearchStruct,"status")).

request_array = add_array(response_toplevel_id, "").

/* call query */
CREATE QUERY lhQuery. 
CREATE BUFFER lhTable FOR TABLE "MsRequest".
lhQuery:ADD-BUFFER(lhTable).
IF NOT lhQuery:QUERY-PREPARE(lcQuery) THEN DO:
      DELETE OBJECT lhQuery.
      DELETE OBJECT lhTable.
      RETURN appl_err("Error in preparing the query: " + lcQuery).
END.

lhQuery:FORWARD-ONLY = TRUE. 
lhQuery:QUERY-OPEN().
liNum = 0.
REPEAT:
     lhQuery:GET-NEXT(NO-LOCK).
     IF lhQuery:QUERY-OFF-END THEN LEAVE.   
     IF liNum >= piOffset + piLimit THEN LEAVE.
     lhMsRequest = lhQuery:GET-BUFFER-HANDLE("MsRequest").
     IF liNum >= piOffset THEN 
        fAddMsRequest().
     liNum = liNum + 1 .
END.

lhQuery:QUERY-CLOSE().

IF VALID-HANDLE(lhMsRequest) THEN DELETE OBJECT lhMsRequest.
DELETE OBJECT lhQuery.
DELETE OBJECT lhTable.

