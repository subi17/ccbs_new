/**
 * newton__request__list.p
 * @input  type;int;mandatory;request type of the requests seeked.
           status;int;optional;request status of the requests seeked
           offset;int;optional;number of skipped request from the beginning of the found requests
           limit;int;mandatory;maximum number of returned requests (maximum 1000)
 * @output  total;int;number of requests found totally with status and type
            requests;array;array of request structs containing information of a single request            
 * @request id;int;request id
            msisdn;string;msisdn of the request
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
{Func/func.p}

DEFINE VARIABLE piType AS INTEGER NO-UNDO. 
DEFINE VARIABLE piStatus AS INTEGER NO-UNDO. 
DEFINE VARIABLE piOffset AS INTEGER NO-UNDO. 
DEFINE VARIABLE piLimit AS INTEGER NO-UNDO INIT 1000000. 
DEFINE VARIABLE lcParams AS CHARACTER NO-UNDO. 

DEFINE VARIABLE gcParamStruct  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE resp_struct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE request_array  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE request_struct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iCount         AS INTEGER NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
IF gi_xmlrpc_error NE 0 THEN RETURN.

gcParamStruct = get_struct(param_toplevel_id, "").

lcParams = validate_request(gcParamStruct, "type!,status!,offset,limit").
IF lcParams EQ ? THEN RETURN.
IF gi_xmlrpc_error NE 0 THEN RETURN.

piType = get_int(gcParamStruct, "type").
piStatus= get_int(gcParamStruct, "status").

IF LOOKUP("limit", lcParams) > 0 THEN DO:
   piLimit = get_int(gcParamStruct, "limit").
   IF piLimit > 1000 THEN RETURN
      appl_err(SUBST("Given limit &1 is bigger than maximum limit 1000", piLimit)).
END.
IF LOOKUP("offset", lcParams) > 0 THEN
   piOffSet = get_int(gcParamStruct, "offset").
IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_struct = add_struct(response_toplevel_id, "").

FOR
   EACH MsReqCounter NO-LOCK WHERE
      MsReqCounter.ReqType   = piType AND
      MsReqCounter.ReqStatus = piStatus:
   ACCUMULATE MsReqCounter.ReqStatusCount (TOTAL).
END.

add_int(resp_struct, "total", (ACCUM TOTAL MsReqCounter.ReqStatusCount)).

request_array = add_array(resp_struct, "requests").

FUNCTION fAddCustomerData RETURN LOGICAL:
   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
   IF AVAIL Customer THEN
      add_string(request_struct, "name", fDispCustName(BUFFER Customer)). 
   RETURN TRUE.
END.

FUNCTION fAddMsRequest RETURN LOGICAL:
   request_struct = add_struct(request_array, "").
   add_int(request_struct, "id", MsRequest.MsRequest).
   add_string(request_struct, "msisdn", MsRequest.CLI).
   fAddCustomerData().
   add_timestamp(request_struct, "activated_at", MsRequest.ActStamp).
   add_timestamp(request_struct, "handled_at", MsRequest.DoneStamp).
   add_int(request_struct, "type", MsRequest.ReqType).
   add_int(request_struct, "status", MsRequest.ReqStatus).
   RETURN TRUE.
END.


iCount = 0.
RequestLoop:
FOR EACH MsRequest USE-INDEX ReqType WHERE 
    MsRequest.Brand eq gcBrand AND 
    MsRequest.ReqType eq piType AND
    MsRequest.ReqStatus eq piStatus NO-LOCK:
    IF iCount >= piOffset + piLimit THEN 
       LEAVE RequestLoop.
    IF iCount >= piOffset THEN
       fAddMsRequest().
    iCount = iCount + 1.
    IF iCount > piOffSet + 1000 THEN DO:
      RETURN appl_err("Limit overflow").
    END.
END.
   



