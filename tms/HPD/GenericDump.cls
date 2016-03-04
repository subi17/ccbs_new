/*------------------------------------------------------------------------
    File        : GenericDump
    Purpose     : Dump program for generic purpose. This can be used
                  when time based dump is not needed and only one 
                  table is needed for the dump.
                  
                  If there is no special fields that needs
                  mSpecialHandling method it is possible to use
                  this directly. Otherwise a subclass is needed
                  that overrides and implements mSpecialHandling method.
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 15:10:34 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING HPD.DumpHandlerWithQuery.
USING HPD.DumpBase.
USING HPD.DumpFile.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.GenericDump INHERITS DumpHandlerWithQuery:  

   DEFINE PROTECTED OVERRIDE PROPERTY lcQuery AS CHARACTER NO-UNDO
      GET:
         RETURN "FOR EACH " + lhMainTable:NAME + " NO-LOCK".   
      END GET.
      

   CONSTRUCTOR PUBLIC GenericDump
      (iiDumpID AS INTEGER):
      
      /* Here is implicit SUPER() call... */
      
      mSetTableName(iiDumpID).
      
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).

      mInitTable().       
      
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC GenericDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
      
      SUPER(iiDumpID).

      objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).

      mInitTable().

      mProcessToFile().
      
      /* If there has been problems we won't be here... */ 
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.
     
   
   METHOD PRIVATE VOID mInitTable():
      
      CREATE BUFFER lhMainTable FOR TABLE lcTableName.
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END METHOD.


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
     (INPUT icField AS CHARACTER,
      INPUT ilCheckOnly AS LOGICAL):
         
      CASE icField:
         
         WHEN "KeyValue"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(lhMainTable:ROWID).
         OTHERWISE RETURN "". 
         
      END CASE.
     
   END METHOD.
      
   
   METHOD PRIVATE VOID mProcessToFile():
     
      DEFINE VARIABLE objDumpFile   AS CLASS HPD.DumpFile NO-UNDO.
      DEFINE VARIABLE liAmount      AS INTEGER            NO-UNDO.
            
      objDumpFile = CAST(objDumpBase, HPD.DumpFile).

      liAmount = objDumpFile:FullDumpLimit.

      lhQuery:SET-BUFFERS(lhMainTable).
      
      lhQuery:QUERY-PREPARE(lcQuery).

      lhQuery:QUERY-OPEN().

      REPEAT
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:
   
         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.           
         
         IF liAmount > 0 AND
            liAmount <= liEvents
         THEN LEAVE.         
         
         lhQuery:GET-NEXT().
         IF lhQuery:QUERY-OFF-END THEN LEAVE.

         liEvents = liEvents + 1.
         
         objDumpFile:mWriteALine().

         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY lhMainTable:TABLE FORMAT "X(20)" NO-LABEL liEvents NO-LABEL 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.
         
      END.
      
      IF NOT SESSION:BATCH
      THEN HIDE FRAME fQty NO-PAUSE.      
      
      FINALLY:      
         lhQuery:QUERY-CLOSE().
      END FINALLY.  
      
   END.
      
END CLASS.