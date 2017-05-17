/**
 * Get invoice groups ids for one customer.
 *
 * @input conditions;struct;mandatory;empty
 * @output struct;array of invoice groups ids
*/


{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEF VAR gcBrand AS CHAR NO-UNDO INIT "1".
DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR lcStruct AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
lcStruct = validate_struct(pcStruct, "customer_id,limit,offset").


FUNCTION fListQuery RETURNS CHAR 
(icTables AS CHAR,
 icQuery AS CHAR,
 icIdField AS CHAR):
   
   DEF VAR lhQuery AS HANDLE NO-UNDO. 
   DEF VAR lhTable AS HANDLE NO-UNDO. 
   DEF VAR liLimit AS INT NO-UNDO INIT 10000000. 
   DEF VAR liOffSet AS INT NO-UNDO. 
   DEF VAR liCount AS INT NO-UNDO. 
   DEF VAR lcIdStruct AS CHAR NO-UNDO.

   IF LOOKUP("limit",lcStruct) > 0 THEN 
      liLimit = get_int(pcStruct,"limit").
   IF LOOKUP("offset",lcStruct) > 0 THEN 
      liOffSet = get_int(pcStruct,"offset").

   IF gi_xmlrpc_error NE 0 THEN RETURN "".
 
   DEF VAR liEntry AS INT NO-UNDO.
   CREATE QUERY lhQuery.
   DO liEntry = 1 TO NUM-ENTRIES(icTables):

     CREATE BUFFER lhTable FOR TABLE ENTRY(liEntry, icTables).
     lhQuery:ADD-BUFFER(lhTable).

   END.
  
   IF NOT lhQuery:QUERY-PREPARE(icQuery) THEN DO:
      DELETE OBJECT lhQuery.
      DELETE OBJECT lhTable.
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
      IF liCount <= liOffSet THEN NEXT.
      IF liCount > liLimit + liOffSet THEN NEXT.

      add_int(lcIdStruct, "", 
         int(lhQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD(icIdField):BUFFER-VALUE)). 

   END.

   add_int(lcResultStruct, "total_amount", liCount). 

   lhQuery:QUERY-CLOSE().
   DELETE OBJECT lhQuery.
   DELETE OBJECT lhTable.

   RETURN "".

END FUNCTION. 

IF gi_xmlrpc_error NE 0 THEN RETURN.

DEF VAR liCustNum AS INT NO-UNDO.
DEF VAR lcQuery AS CHAR NO-UNDO. 

liCustNum = get_int(pcStruct,"customer_id").

lcQuery = 'FOR EACH InvoiceTargetGroup NO-LOCK WHERE ' + 
                   'InvoiceTargetGroup.CustNum = ' + STRING(liCustNum) + 
                   ' AND InvoiceTargetGroup.Brand = "1"'. 

fListQuery(
   "InvoiceTargetGroup",
   lcQuery,
   "ITGroupID").
