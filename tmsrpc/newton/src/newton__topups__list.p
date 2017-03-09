/**
 * Get topups ids for one subscription.
 *
 * @input conditions;struct;mandatory;
   @conditions msseq;int;mandatory;subscription sequence number
               entity;string;optional;entity to return 
               offset;integer;mandatory;how many records to skip
               limit;integer;mandatatory;how many records to fetch

 * @output struct;array of topup ids
*/



{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEF VAR gcBrand AS CHAR NO-UNDO INIT "1".
DEF VAR lcResultStruct AS CHARACTER NO-UNDO. 
DEF VAR pcStruct AS CHARACTER NO-UNDO. 
DEF VAR lcStruct AS CHARACTER NO-UNDO. 
DEF VAR piMsSeq  AS INTEGER   NO-UNDO.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").

FUNCTION fListQueryInArray RETURNS CHAR 
(icTables AS CHAR,
 icQuery AS CHAR,
 icIdField AS CHAR):
   
   DEF VAR lhQuery AS HANDLE NO-UNDO. 
   DEF VAR lhTable AS HANDLE NO-UNDO. 
   DEF VAR liLimit AS INTEGER NO-UNDO INIT 10000000. 
   DEF VAR liOffSet AS INTEGER NO-UNDO. 
   DEF VAR liCount AS INTEGER NO-UNDO. 
   DEF VAR lcIdStruct AS CHARACTER NO-UNDO.

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

lcStruct = validate_struct(pcStruct, "msseq!,entity,offset,limit").

IF gi_xmlrpc_error NE 0 THEN RETURN.

piMsSeq = get_int(pcStruct,"msseq").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO ordercanal MobSub MsSeq piMsSeq}

DEF VAR lcQuery AS CHARACTER NO-UNDO. 

lcQuery = 'FOR EACH PrepaidRequest NO-LOCK WHERE' + 
          ' PrepaidRequest.Brand = ' + QUOTER(gcBrand) +
          ' AND PrepaidRequest.MsSeq = ' + STRING(get_int(pcStruct,"msseq")) + 
          ' AND  PrepaidRequest.Response NE "First4B" '.

IF LOOKUP("entity",lcStruct) NE 0 THEN 
lcQuery = lcQuery + ' AND PrepaidRequest.Entidad = ' + QUOTER(STRING(get_int(pcStruct,'entity'))). 

fListQueryInArray(
   "PrepaidRequest",
   lcQuery,
   "PPRequest").
