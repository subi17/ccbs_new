DEFINE VARIABLE gcSessionParam AS CHARACTER NO-UNDO.

gcSessionParam = SESSION:PARAMETER.

IF NUM-ENTRIES(gcSessionParam) NE 1
THEN DO:
   LOG-MANAGER:WRITE-MESSAGE("Incorrect Session parameter", "ERROR").
   RETURN.
END.

RUN pStart.

PROCEDURE pStart:
  
   DEFINE VARIABLE liDumpID         AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcClassName      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llInactivateDump AS LOGICAL   INITIAL NO NO-UNDO.
   DEFINE VARIABLE HandlerObj       AS CLASS HPD.DumpHandler NO-UNDO.
   DEFINE VARIABLE lcErrorText      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lii              AS INTEGER   NO-UNDO.

   ASSIGN
      liDumpID = INTEGER(ENTRY(1,gcSessionParam))
      NO-ERROR.
      
   IF ERROR-STATUS:ERROR
   THEN DO:
      LOG-MANAGER:WRITE-MESSAGE("Invalid dumpid", "ERROR").
      RETURN.
   END.

   FIND FIRST DumpFile NO-LOCK WHERE
      DumpFile.DumpID = liDumpID
   NO-ERROR.
   
   IF NOT AVAILABLE DumpFile
   THEN DO:
      LOG-MANAGER:WRITE-MESSAGE("DumpFile record doesn't exist", "ERROR").
      RETURN.
   END.
   
   IF NOT DumpFile.FileCategory = "HPD"
   THEN DO:
      LOG-MANAGER:WRITE-MESSAGE("Only HPD type dumpfile is allowed", "ERROR").
      RETURN.
   END.
   
   lcClassName = DumpFile.LinkKey.
   
   IF NOT lcClassName BEGINS "HPD."
   THEN DO:
      LOG-MANAGER:WRITE-MESSAGE("DumpFile LinkKey field doesn't contain valid HPD class", "ERROR").
      RETURN.
   END.
   
   LOG-MANAGER:WRITE-MESSAGE("Start processing dump " + DumpFile.DumpName, "INFO").

   HandlerObj = DYNAMIC-NEW lcClassName(liDumpID, "").

   IF HandlerObj:llInterrupted
   THEN DO:
      LOG-MANAGER:WRITE-MESSAGE("Process was interrupted when " + STRING(HandlerObj:liEvents) + " was dumped", "INFO").
      RETURN.
   END.

   LOG-MANAGER:WRITE-MESSAGE("Completed the dump. " + STRING(HandlerObj:liEvents) + " was dumped.", "INFO").
   

   CATCH apperrorobj AS Progress.Lang.AppError:
   
      IF apperrorobj:ReturnValue <> "" AND apperrorobj:ReturnValue <> ?
      THEN DO:
         LOG-MANAGER:WRITE-MESSAGE(apperrorobj:ReturnValue, "ERROR").
         RETURN. 
      END.

      DO lii = 1 TO apperrorobj:NumMessages:    
           lcErrorText = lcErrorText + " " + apperrorObj:GetMessage(lii).
      END.
   
      LOG-MANAGER:WRITE-MESSAGE(lcErrorText, "ERROR").
      
      RETURN.   
   
   END.

   CATCH errorobj AS Progress.Lang.ProError:
    
      DO lii = 1 TO errorobj:NumMessages:  
         
         IF errorObj:GetMessage(lii) BEGINS "DYNAMIC-NEW cannot instantiate class"
         THEN ASSIGN
                 llInactivateDump = YES         
                 lcErrorText = lcErrorText + " " + "Class " + lcClassName + " doesn't allow offline HPD handling. The dump is automatically set to inactive state."
                 .
         ELSE lcErrorText = lcErrorText + " " + errorObj:GetMessage(lii).
      END.
   
      LOG-MANAGER:WRITE-MESSAGE(lcErrorText, "ERROR").
      
      RETURN. 
        
   END.

   FINALLY:

      IF VALID-OBJECT(HandlerObj)
      THEN DELETE OBJECT HandlerObj.

      IF llInactivateDump
      THEN DO TRANSACTION:

         FIND FIRST DumpFile EXCLUSIVE-LOCK WHERE
            DumpFile.DumpID = liDumpID
         NO-ERROR.

         IF AVAILABLE DumpFile
         THEN DumpFile.Active = NO.

         RELEASE DumpFile.       

      END.
      
   END FINALLY.

END PROCEDURE.
