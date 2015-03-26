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

FOR EACH MsReqStatistic NO-LOCK USE-INDEX ReqType
   BREAK BY MsReqStatistic.ReqType BY MsReqStatistic.ReqStatus:
   
   IF FIRST-OF(MsReqStatistic.ReqType) THEN
      resp_reqtype = add_struct(resp_reqtypes, STRING(MsReqStatistic.ReqType)).
  
   /* Calculation is not 100% accurate. Prevent negative values */
   add_int(
      resp_reqtype,
      STRING(MsReqStatistic.ReqStatus),
      (IF MsReqStatistic.ReqStatusCount > 0 THEN 
          MsReqStatistic.ReqStatusCount ELSE 0)).

END.
