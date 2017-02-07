{xmlrpc/xmlrpc_access.i}
{Func/matrix.i}

DEF VAR gcBrand AS CHAR NO-UNDO INIT "1".
DEF VAR lcResultStruct AS CHARACTER NO-UNDO. 
DEF VAR pcStruct   AS CHARACTER NO-UNDO. 
DEF VAR lcStruct   AS CHARACTER NO-UNDO. 
DEF VAR top_struct AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").

top_struct = add_struct(response_toplevel_id, "").
lcResultStruct = add_array(top_struct, "").

FUNCTION fListQuery RETURNS CHAR 
(icTables AS CHAR,
 icQuery AS CHAR,
 icIdField AS CHAR):
   
   DEF VAR lhQuery AS HANDLE NO-UNDO. 
   DEF VAR lhTable AS HANDLE NO-UNDO. 
   DEF VAR liLimit AS INTEGER NO-UNDO INIT 10000000. 
   DEF VAR liOffSet AS INTEGER NO-UNDO. 
   DEF VAR liCount AS INTEGER NO-UNDO. 
   DEF VAR liEntry AS INT NO-UNDO.

   IF LOOKUP("limit",lcStruct) > 0 THEN 
      liLimit = get_int(pcStruct,"limit").
   IF LOOKUP("offset",lcStruct) > 0 THEN 
      liOffSet = get_int(pcStruct,"offset").

   IF gi_xmlrpc_error NE 0 THEN RETURN "".

   CREATE WIDGET-POOL "ListQuery".
   CREATE QUERY lhQuery IN WIDGET-POOL "ListQuery".

   DO liEntry = 1 TO NUM-ENTRIES(icTables):

      CREATE BUFFER lhTable FOR TABLE ENTRY(liEntry, icTables)
         IN WIDGET-POOL "ListQuery".
      lhQuery:ADD-BUFFER(lhTable).

   END.
  
   IF NOT lhQuery:QUERY-PREPARE(icQuery) THEN DO:
      DELETE WIDGET-POOL "ListQuery".
      RETURN appl_err("Error in preparing the query: " + icQuery).
   END.
   
   lhQuery:QUERY-OPEN.

   REPEAT:
      
      lhQuery:GET-NEXT(NO-LOCK).
      
      liCount = liCount + 1.
      IF liCount <= liOffSet THEN NEXT.
      
      IF lhQuery:QUERY-OFF-END OR liCount > liLimit + liOffSet THEN LEAVE.
      
      IF icidField NE ? THEN 
        add_string(lcResultStruct, "", 
           lhQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD(icIdField):BUFFER-VALUE). 
      ELSE DO:
         add_string(lcResultStruct, "", 
           STRING(lhQuery:GET-BUFFER-HANDLE(1):ROWID)). 
      END.
   END.

   lhQuery:QUERY-CLOSE().
   DELETE WIDGET-POOL "ListQuery".

   RETURN "".

END FUNCTION. 

FUNCTION fListBundleQuery RETURNS CHAR 
(INPUT icBrand        AS CHAR,
 INPUT icMatrixKey    AS CHAR,
 INPUT icMItemName    AS CHAR,
 INPUT icMitemValue   AS CHAR):

 DEF VAR liResponse   AS INT  NO-UNDO.
 DEF VAR liCount      AS INT  NO-UNDO.
 DEF VAR liNumEntries AS INT  NO-UNDO.
 DEF VAR lcResult     AS CHAR NO-UNDO.
 DEF VAR lcBundle     AS CHAR NO-UNDO.

 liResponse = fListMatrix(INPUT icBrand,
                          INPUT icMatrixKey,
                          INPUT icMItemName,
                          INPUT icMitemValue,
                          OUTPUT lcResult).
 IF liResponse <> 1 OR lcResult = "" THEN RETURN "".

 liNumEntries = NUM-ENTRIES(lcResult).
 DO liCount = 1 TO liNumEntries:
    lcBundle = ENTRY(liCount,lcResult).
    IF lcBundle = "" OR lcBundle = ? THEN NEXT.
    add_string(lcResultStruct, "", lcBundle).
 END. /* DO liCount = 1 TO liNumEntries: */

END FUNCTION. /* FUNCTION fListBundleQuery RETURNS CHAR */


