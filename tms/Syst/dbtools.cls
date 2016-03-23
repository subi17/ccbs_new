CLASS Syst.DbTools:

   METHOD PUBLIC CHARACTER FindDB
     (INPUT pcFileName AS CHARACTER):

      DEFINE VARIABLE liLoop   AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcDB     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcDBName AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lhTable  AS HANDLE    NO-UNDO.    
      DEFINE VARIABLE lhField  AS HANDLE    NO-UNDO. 
      DEFINE VARIABLE lhQuery  AS HANDLE    NO-UNDO. 
      DEFINE VARIABLE lcQuery  AS CHARACTER NO-UNDO. 

      DO liLoop = 1 to NUM-DBS:

         lcDB = LDBNAME(liLoop).

         CREATE QUERY lhQuery.
   
         CREATE BUFFER lhTable FOR TABLE lcDB + "._File".

         lcQuery = "FOR EACH " + lcDB + "._File WHERE " +
                   "_File._File-Name = ~"" + pcFileName + "~"".
 
         lhQuery:SET-BUFFERS(lhTable).
         lhQuery:QUERY-PREPARE(lcQuery).
         lhQuery:QUERY-OPEN.

         lhQuery:GET-NEXT.
      
         IF lhTable:AVAILABLE THEN lcDBName = lcDB.
      
         IF VALID-HANDLE(lhTable) THEN DELETE OBJECT lhTable.
         IF VALID-HANDLE(lhField) THEN DELETE OBJECT lhField.
         IF VALID-HANDLE(lhQuery) THEN DELETE OBJECT lhQuery.
   
      END.
   
      RETURN lcDBName.

   END METHOD.

END CLASS.
