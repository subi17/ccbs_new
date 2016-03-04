/*------------------------------------------------------------------------
    File        : DumpHandlerWithQuery
    Purpose     : Adds dynamic query features to DumpHandler class
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 15:10:34 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.HPDCommon.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DumpHandlerWithQuery ABSTRACT INHERITS DumpHandler:
     
   DEFINE PROTECTED ABSTRACT PROPERTY lcQuery AS CHARACTER NO-UNDO
      GET.

   DEFINE PROTECTED VARIABLE lhQuery     AS HANDLE    NO-UNDO.
   DEFINE PROTECTED VARIABLE lcTableName AS CHARACTER NO-UNDO.
   
   
   CONSTRUCTOR PROTECTED DumpHandlerWithQuery
      (iiDumpID AS INTEGER ):

      mSetTableName(iiDumpID).
      CREATE QUERY lhQuery.
      
   END CONSTRUCTOR.
   

   CONSTRUCTOR PROTECTED DumpHandlerWithQuery
      ():

   END CONSTRUCTOR.

   
   METHOD PROTECTED VOID mSetTableName
      (iiDumpID AS INTEGER):
         
      lcTableName = HPDCommon:mGetDumpTable(iiDumpID).   
  
   END METHOD.
   

   DESTRUCTOR PUBLIC DumpHandlerWithQuery( ):
      
      IF VALID-HANDLE(lhQuery)
      THEN DO:
         lhQuery:QUERY-CLOSE().
         DELETE OBJECT lhQuery.
      END.
      
   END DESTRUCTOR.
         
END CLASS.