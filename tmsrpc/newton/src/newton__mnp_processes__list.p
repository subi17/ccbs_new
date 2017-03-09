/**
 * Get mnp process ids
 *
 * @input conditions;struct;mandatory;supports mnp_request_id,msisdn,msseq,order_id,dni,status_code, (error_code, error_handled), (creation_time_start),(creation_time_end), creation time can be used only together with status_code, mnp_request_id and msisdn parameters, salesman_id
 * @conditions mnp_type;int;mandatory;1=incoming,2=outgoing
               mnp_request_id;string;optional;
               msisdn;string;optional;
               msseq;int;optional;
               order_id;int;optional;
               error_code;char;optional;
               status_reason;char;optional;status reason of AREC/AREC_CLOSED
               error_handled;int;1=handled,2=not handled
               dni;string;optional;
               status_code;string;optional;NEW,ASOL,AREC,ACON,APOR,ACAN,AREC_CLOSED,ERR
               creation_time_start;datetime;optional;
               creation_time_end;datetime;optional;
               porting_time_start;datetime;optional;
               porting_time_end;datetime;optional;
               operator_code;str;optional;3 digit operator code
               limit;int;optional;default is unlimited
               offset;int;optional;default is 0
               sort_by;string;optional;oper_code,porting_time,creation_time
               sort_order;string;optional;
 * @output results;array of int;mnpprocess ids
           total_amount;int;total amount if ids (max is 10000)
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}

DEF VAR gcBrand AS CHAR NO-UNDO INIT "1".
DEF VAR lcResultStruct AS CHARACTER NO-UNDO. 
DEF VAR pcTenant AS CHARACTER NO-UNDO.
DEF VAR pcStruct AS CHARACTER NO-UNDO. 
DEF VAR lcStruct AS CHARACTER NO-UNDO. 
DEF VAR liLimit AS INTEGER NO-UNDO INIT 10000000. 
DEF VAR liOffSet AS INTEGER NO-UNDO. 
DEF VAR liCount AS INTEGER NO-UNDO. 
DEF VAR lcIdStruct AS CHARACTER NO-UNDO.
DEF VAR lcSalesmanId       AS CHAR NO-UNDO.
DEF VAR lcOrderStatusCodes AS CHAR NO-UNDO.
DEF VAR lcOrderStatusCode  AS CHAR NO-UNDO.
DEF VAR liNumEntries       AS INT  NO-UNDO.
DEF VAR liEntryCount       AS INT  NO-UNDO.

DEF VAR lcQuery AS CHARACTER NO-UNDO. 
DEF VAR lcTables AS CHARACTER NO-UNDO INIT "MNPProcess". 
DEF VAR lcStatusCode AS CHARACTER NO-UNDO. 
DEF VAR liStatusCode AS INTEGER NO-UNDO. 
DEF VAR liMNPType AS INTEGER NO-UNDO. 

FUNCTION fListQuery RETURNS CHAR 
(icTables AS CHAR,
 icQuery AS CHAR,
 icIdField AS CHAR):
   
   DEF VAR lhQuery AS HANDLE NO-UNDO. 
   DEF VAR lhTable AS HANDLE NO-UNDO. 

   IF gi_xmlrpc_error NE 0 THEN RETURN "".

   /* all dynamic objects to this */
   CREATE WIDGET-POOL "MNPQuery".
 
   DEF VAR liEntry AS INT NO-UNDO.
   CREATE QUERY lhQuery IN WIDGET-POOL "MNPQuery".
   DO liEntry = 1 TO NUM-ENTRIES(icTables):

     CREATE BUFFER lhTable FOR TABLE ENTRY(liEntry, icTables) IN WIDGET-POOL "MNPQuery".
     lhQuery:ADD-BUFFER(lhTable).

   END.
   
   IF NOT lhQuery:QUERY-PREPARE(icQuery) THEN DO:
      DELETE WIDGET-POOL "MNPQuery".
      RETURN appl_err("Error in preparing the query: " + icQuery).
   END.
   
   lhQuery:FORWARD-ONLY = TRUE. 
   lhQuery:QUERY-OPEN.

   lcresultstruct = add_struct(response_toplevel_id, "").
   lcIdstruct = add_array(lcresultstruct, "results").

   REPEAT:
      
      lhQuery:GET-NEXT(NO-LOCK).
      IF lhQuery:QUERY-OFF-END THEN LEAVE.
      
      liCount = liCount + 1.
      IF liCount >= 10000 THEN LEAVE.
      IF liCount <= liOffSet THEN NEXT.
      IF liCount > liLimit + liOffSet THEN NEXT.
      add_int(lcIdStruct, "", 
         int(lhQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD(icIdField):BUFFER-VALUE)). 
   END.

   add_int(lcResultStruct, "total_amount", liCount). 

   lhQuery:QUERY-CLOSE().
   DELETE WIDGET-POOL "MNPQuery".

   RETURN "".

END FUNCTION. 

FUNCTION fCreationTime RETURNS LOGICAL:
   
   IF LOOKUP("creation_time_start",lcStruct) > 0 THEN
      lcQuery = lcQuery + " AND MNPProcess.CreatedTS > " + 
      QUOTER(get_timestamp(pcStruct,"creation_time_start")).
   
   IF LOOKUP("creation_time_end",lcStruct) > 0 THEN
      lcQuery = lcQuery + " AND MNPProcess.CreatedTS < " + 
      QUOTER(get_timestamp(pcStruct,"creation_time_end")).

END FUNCTION. 

FUNCTION fPortingTime RETURNS LOGICAL:

   IF LOOKUP("porting_time_start",lcStruct) > 0 THEN
      lcQuery = lcQuery + " AND MNPProcess.PortingTime > " + 
      QUOTER(get_timestamp(pcStruct,"porting_time_start")).

   IF LOOKUP("porting_time_end",lcStruct) > 0 THEN
      lcQuery = lcQuery + " AND MNPProcess.PortingTime < " + 
      QUOTER(get_timestamp(pcStruct,"porting_time_end")).

END FUNCTION. 

FUNCTION fSort RETURNS LOGICAL:

   IF LOOKUP("sort_by",lcStruct) > 0 then do:
      DEFINE VARIABLE lcSortOrder AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE lcSortBY AS CHARACTER NO-UNDO. 
      lcSortOrder = get_string(pcStruct, "sort_by").
      CASE lcSortOrder:
         WHEN "operator_code" then lcSortOrder = " BY OperCode".
         WHEN "porting_time" then lcSortOrder = " BY Portingtime".
         WHEN "creation_time" then lcSortOrder = " BY CreatedTs".
         OTHERWISE lcSortOrder = "".
      END.
      IF lcSortOrder NE "" AND LOOKUP("sort_order",lcStruct) > 0 THEN DO:
         lcSortBY = get_string(pcStruct, "sort_order").
         IF lcSortBY = "descending" THEN lcSortOrder = lcSortOrder + " " +
            lcSortBy.
      END.
      lcQuery = lcQuery + lcSortOrder.
   end.
END FUNCTION. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").

lcStruct = validate_struct(pcStruct, "brand,mnp_request_id,msisdn,mnp_type,status_code,creation_time_start,creation_time_end,msseq,order_id,dni,limit,offset,error_code,error_handled,porting_time_start,porting_time_end,operator_code,sort_by,sort_order,status_reason,salesman_id").

pcTenant = get_string(pcStruct, "brand").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

IF LOOKUP("limit",lcStruct) > 0 THEN 
   liLimit = get_int(pcStruct,"limit").
IF LOOKUP("offset",lcStruct) > 0 THEN 
   liOffSet = get_int(pcStruct,"offset").

IF LOOKUP("mnp_type",lcStruct) = 0 THEN RETURN appl_err("Unsupported search criteria").

liMNPType = get_int(pcStruct,"mnp_type").

IF LOOKUP("status_code",lcStruct) > 0 THEN 
   lcStatusCode = get_string(pcStruct,"status_code").

lcQuery = 'FOR EACH MNPProcess NO-LOCK WHERE MNPProcess.Brand = "1"' +
          " AND MNPProcess.MNPType = " + QUOTER(liMNPType).
DEF VAR lcQueryBegin AS CHARACTER NO-UNDO. 
lcQueryBegin = lcQuery.

/* 1 */
IF LOOKUP("msisdn",lcStruct) > 0 OR LOOKUP("msseq",lcStruct) > 0 THEN DO:
  
   lcQuery = "FOR EACH MNPSub NO-LOCK WHERE".
   
   IF LOOKUP("msseq",lcStruct) > 0 THEN lcQuery = lcQuery + " MNPSub.MsSeq = " + QUOTER(get_string(pcStruct,"msseq")).
   IF LOOKUP("msisdn",lcStruct) > 0 AND LOOKUP("msseq",lcStruct) > 0 THEN lcQuery = lcQuery + " AND". 
   IF LOOKUP("msisdn",lcStruct) > 0 THEN lcQuery = lcQuery + " MNPSub.CLI = " + QUOTER(get_string(pcStruct,"msisdn")).

   lcQuery = lcQuery +
        ' , EACH MNPProcess NO-LOCK WHERE
                MNPProcess.MNPSeq = MNPSub.MNPSeq AND
                MNPProcess.Brand = "1" AND
                MNPProcess.MNPType = ' + QUOTER(liMNPType) + 
                " USE-INDEX MNPSeq".

   lcTables = "MNPSub,MNPProcess".
END.

/* 2 */
ELSE IF lcStatusCode EQ "ERR" THEN DO:
   
   lcQuery = "FOR EACH MNPOperation NO-LOCK WHERE MNPOperation.ErrorHandled = " + (IF LOOKUP("error_handled",lcStruct) > 0 THEN QUOTER(get_int(pcStruct,"error_handled")) ELSE " 1").
   
   IF LOOKUP("error_code",lcStruct) > 0 THEN 
      lcQuery = lcQuery + " AND MNPOperation.ErrorCode = " + QUOTER(get_string(pcStruct,"error_code")).

   lcQuery = lcQuery + ", FIRST MNPProcess NO-LOCK WHERE MNPProcess.MNPSeq = MNPOperation.MNPSeq" +
       ' AND MNPProcess.Brand = ' + QUOTER(gcBrand) + 
       ' AND MNPProcess.MNPType = ' + QUOTER(liMNPType) + 
       ' AND (MNPProcess.StatusCode = ' + QUOTER({&MNP_ST_NEW}) +
       ' OR MNPProcess.StatusCode = ' + QUOTER({&MNP_ST_ASOL}) +
       ' OR MNPProcess.StatusCode = ' + QUOTER({&MNP_ST_ACON}) + ')'.
   
   lcTables = "MNPOperation,MNPProcess".
END.

/* 3 */
ELSE IF LOOKUP("dni",lcStruct) > 0 THEN DO:
   
   lcQuery = "FOR EACH MNPDetails NO-LOCK WHERE MNPDetails.CustId = " +
      QUOTER(get_string(pcStruct,"dni")).

   lcQuery = lcQuery +
        ' , EACH MNPProcess NO-LOCK WHERE
                MNPProcess.MNPSeq = MNPDetails.MNPSeq AND
                MNPProcess.Brand = "1" AND
                MNPProcess.MNPType = ' + QUOTER(liMNPType) + " USE-INDEX MNPSeq".
   
   lcTables = "MNPDetails,MNPProcess".

END.

IF LOOKUP("mnp_request_id",lcStruct) > 0 THEN DO:
   lcQuery = lcQuery + " AND MNPProcess.PortRequest = " + 
             QUOTER(get_string(pcStruct,"mnp_request_id")) + 
             " USE-INDEX PortRequest".
END.

IF LOOKUP("order_id",lcStruct) > 0 THEN DO:
   lcQuery = lcQuery + " AND MNPProcess.OrderID = " + 
             QUOTER(get_string(pcStruct,"order_id")) + " USE-INDEX OrderId".
END.

IF LOOKUP("operator_code",lcStruct) > 0 THEN DO:
   lcQuery = lcQuery + ' AND MNPProcess.OperCode = ' + QUOTER(get_string(pcStruct,"operator_code")).
END.

IF LOOKUP(lcStatusCode,"NEW,ASOL,AREC,ACON,APOR,ACAN,AREC_CLOSED") > 0 THEN DO:
  
   liStatusCode = LOOKUP(lcStatusCode,"NEW,,ASOL,,AREC,ACON,APOR,ACAN,AREC_CLOSED") - 1.
   
   lcQuery = lcQuery + " AND MNPProcess.StatusCode = " + QUOTER(liStatusCode).
   IF LOOKUP("status_reason", lcStruct) > 0
      THEN lcQuery = lcQuery + " AND MNPProcess.StatusReason = " + QUOTER(get_string(pcStruct, "status_reason")).
   
END.

/* POS Improvement */
IF LOOKUP("salesman_id",lcStruct) > 0 THEN DO:
   lcSalesmanId = get_string(pcStruct, "salesman_id").
   lcresultstruct = add_struct(response_toplevel_id, "").
   lcIdstruct = add_array(lcresultstruct, "results").

   /* Ongoing Orders */
   ASSIGN lcOrderStatusCodes = "3,12,73"
          liNumEntries = NUM-ENTRIES(lcOrderStatusCodes).

   LOOP:
   DO liEntryCount = 1 TO liNumEntries:
      lcOrderStatusCode = ENTRY(liEntryCount,lcOrderStatusCodes).

      FOR EACH Order NO-LOCK WHERE
               Order.Brand      = gcBrand AND
               Order.SalesMan   = lcSalesmanId AND
               Order.StatusCode = lcOrderStatusCode AND
               Order.MNPStatus  > 0 AND
               LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0,
          EACH MNPProcess WHERE
               MNPProcess.OrderID = Order.OrderID AND
               MNPProcess.MNPType = liMNPType NO-LOCK
          BREAK BY MNPProcess.OrderId
                BY MNPProcess.CreatedTS:

          IF LAST-OF(MNPProcess.OrderId) THEN DO:
             liCount = liCount + 1.
             IF liCount >= 10000 THEN LEAVE LOOP.
             IF liCount <= liOffSet THEN NEXT.
             IF liCount > liLimit + liOffSet THEN NEXT.

             add_int(lcIdStruct, "", MNPProcess.MNPSeq).
          
          END. /* IF LAST-OF(MNPProcess.OrderId) THEN DO: */
      END. /* FOR EACH Order WHERE */
   END. /* DO liEntryCount = 1 TO liNumEntries: */

   add_int(lcResultStruct, "total_amount", liCount).

   RETURN.
END. /* IF LOOKUP("salesman_id",lcStruct) > 0 THEN DO: */

fCreationTime().
fPortingTime().

/* operator_code can be after base query, porting/creation time must be after USE-INDEX */
IF LOOKUP(lcStatusCode,"ERR") > 0 THEN
   lcQuery = lcQuery + " USE-INDEX MNPSeq".

fSort().

IF lcQuery EQ lcQueryBegin THEN RETURN appl_err("Unsupported search criteria").

IF gi_xmlrpc_error NE 0 THEN RETURN.

fListQuery(
   lcTables,
   lcQuery,
   "MNPSeq").
