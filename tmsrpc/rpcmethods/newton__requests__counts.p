/**
 * Counts requests per type and status. Takes no input. 
 *
 * @output      msrequest statuses;array of structs;msrequest count data
 */
{xmlrpc/xmlrpc_access.i}

/* Output parameters */
DEFINE VARIABLE resp_reqtypes   AS CHARACTER NO-UNDO.
DEFINE VARIABLE resp_reqtype    AS CHARACTER NO-UNDO.

resp_reqtypes = add_struct(response_toplevel_id, "").

FOR EACH MsReqCounter NO-LOCK USE-INDEX ReqType
   BREAK BY MsReqCounter.ReqType BY MsReqCounter.ReqStatus:

   ACCUMULATE MsReqCounter.ReqStatusCount (SUB-TOTAL BY MsReqCounter.ReqStatus).

   IF FIRST-OF(MsReqCounter.ReqType) THEN
      resp_reqtype = add_struct(resp_reqtypes, STRING(MsReqCounter.ReqType)).
  
  /* Calculation is not 100% accurate. Prevent negative values */
  IF LAST-OF(MsReqCounter.ReqStatus)
  THEN
   add_int(
      resp_reqtype,
      STRING(MsReqCounter.ReqStatus),
      (IF (ACCUM SUB-TOTAL BY MsReqCounter.ReqStatus MsReqCounter.ReqStatusCount) > 0 THEN
          (ACCUM SUB-TOTAL BY MsReqCounter.ReqStatus MsReqCounter.ReqStatusCount) ELSE 0)).

END.
