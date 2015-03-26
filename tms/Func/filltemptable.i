FUNCTION fFillCDRTempTable RETURNS INTEGER
  (INPUT pcDB    AS CHARACTER,
   INPUT pcQuery AS CHARACTER,
   INPUT iiQty   AS INT,
   INPUT-OUTPUT  tthCDR  AS HANDLE):

   DEFINE VARIABLE lhTMS   AS HANDLE    NO-UNDO.
   DEFINE VARIABLE tthBuf  AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhQuery AS HANDLE    NO-UNDO.
   DEFINE VARIABLE liFound AS INT       NO-UNDO.
   DEF VAR lcTable AS CHAR NO-UNDO.

   CREATE BUFFER tthBuf FOR TABLE tthCDR:DEFAULT-BUFFER-HANDLE.
   CREATE BUFFER lhTMS  FOR TABLE pcDB.

   CREATE QUERY lhQuery.

   IF NUM-ENTRIES(pcDB,".") > 1 THEN lcTable = ENTRY(2,pcDB,".").
   ELSE lcTable = pcDB.

   lhQuery:SET-BUFFERS(lhTMS).
   lhQuery:QUERY-PREPARE("FOR EACH " + pcDB + " NO-LOCK WHERE " + pcQuery).
   lhQuery:QUERY-OPEN.

   DO WHILE TRUE:

      lhQuery:GET-NEXT.
   
      IF lhQuery:QUERY-OFF-END THEN LEAVE.

      tthBuf:BUFFER-CREATE.
      tthBuf:BUFFER-COPY(lhTMS) NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN tthBuf:BUFFER-DELETE.
      ELSE liFound = liFound + 1.

      tthBuf::CDRTable = lcTable NO-ERROR.
      
      IF iiQty > 0 AND liFound >= iiQty THEN LEAVE.
   END.

   DELETE OBJECT tthBuf.
   DELETE OBJECT lhQuery.
   DELETE OBJECT lhTMS.

   RETURN liFound. 

END FUNCTION.
