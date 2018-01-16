{HPD/HPDConst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE gcSessionParam   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcTimeBasedDumps AS CHARACTER NO-UNDO.
DEFINE VARIABLE giDumpID         AS INTEGER   NO-UNDO.

gcSessionParam = SESSION:PARAMETER.

IF NUM-ENTRIES(gcSessionParam) NE 1
THEN DO:
   LOG-MANAGER:WRITE-MESSAGE("Incorrect Session parameter", "ERROR").
   RETURN.
END.

ASSIGN
   giDumpID = INTEGER(ENTRY(1,gcSessionParam))
   NO-ERROR.

IF ERROR-STATUS:ERROR
THEN DO:
   LOG-MANAGER:WRITE-MESSAGE("Invalid dumpid", "ERROR").
   RETURN.
END.

DO ON ERROR UNDO, THROW:

   gcTimeBasedDumps = Syst.Parameters:getc("HPD.TimeBasedDumps", "HPD.Interface").

   /* Handler code for any error condition. */
   CATCH anyErrorObject AS Progress.Lang.Error:
      gcTimeBasedDumps = "HPD_EDRHistory,HPD_MobCDR,HPD_PrepCDR,HPD_PrepEDR," +
                            "HPD_Order,HPD_Invoice,HPD_MsRequest,HPD_Payment," +
                            "HPD_PrepaidRequest,HPD_ServiceLCounter".

      Syst.Parameters:setc("HPD.TimeBasedDumps", "HPD.Interface", gcTimeBasedDumps).
   END CATCH.

END.

RUN pStart.

FUNCTION fProcess RETURNS LOGICAL
   (iiDumpID AS INTEGER):

   DEFINE VARIABLE lcClassName      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llInactivateDump AS LOGICAL   INITIAL NO NO-UNDO.
   DEFINE VARIABLE HandlerObj       AS CLASS HPD.DumpHandler NO-UNDO.
   DEFINE VARIABLE lcErrorText      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lii              AS INTEGER   NO-UNDO.

   FIND FIRST DumpFile NO-LOCK WHERE
      DumpFile.DumpID = iiDumpID
   NO-ERROR.

   IF NOT AVAILABLE DumpFile
   THEN DO:
      LOG-MANAGER:WRITE-MESSAGE("DumpFile record doesn't exist", "ERROR").
      RETURN TRUE.
   END.
   
   IF NOT DumpFile.FileCategory = "HPD"
   THEN DO:
      LOG-MANAGER:WRITE-MESSAGE("Only HPD type dumpfile is allowed", "ERROR").
      RETURN TRUE.
   END.

   lcClassName = DumpFile.LinkKey.
   
   IF NOT lcClassName BEGINS "HPD."
   THEN DO:
      LOG-MANAGER:WRITE-MESSAGE("DumpFile LinkKey field doesn't contain valid HPD class", "ERROR").
      RETURN TRUE.
   END.
   
   LOG-MANAGER:WRITE-MESSAGE("Start processing dump " + DumpFile.DumpName, "INFO").

   IF LOOKUP(DumpFile.DumpName, gcTimeBasedDumps) > 0
   THEN HandlerObj = DYNAMIC-NEW lcClassName(iiDumpID, "").
   ELSE HandlerObj = DYNAMIC-NEW lcClassName(iiDumpID, "{&FileIDForMQ}").

   IF HandlerObj:llInterrupted
   THEN DO:
      LOG-MANAGER:WRITE-MESSAGE("Process was interrupted when " + STRING(HandlerObj:liEvents) + " was dumped", "INFO").
      RETURN TRUE.
   END.

   LOG-MANAGER:WRITE-MESSAGE("Completed the dump. " + STRING(HandlerObj:liEvents) + " rows was dumped.", "INFO").
   
   IF LOOKUP(DumpFile.DumpName, gcTimeBasedDumps) > 0
   THEN RETURN FALSE.
   ELSE RETURN TRUE.

   CATCH apperrorobj AS Progress.Lang.AppError:
   
      IF apperrorobj:ReturnValue <> "" AND apperrorobj:ReturnValue <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE(apperrorobj:ReturnValue, "ERROR").

      DO lii = 1 TO apperrorobj:NumMessages:    
           lcErrorText = lcErrorText + " " + apperrorObj:GetMessage(lii).
      END.
   
      LOG-MANAGER:WRITE-MESSAGE(lcErrorText, "ERROR").
      
      RETURN TRUE.
   
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
      
      RETURN TRUE.
        
   END.

   FINALLY:

      IF VALID-OBJECT(HandlerObj)
      THEN DELETE OBJECT HandlerObj.

      IF llInactivateDump
      THEN DO TRANSACTION:

         FIND FIRST DumpFile EXCLUSIVE-LOCK WHERE
            DumpFile.DumpID = iiDumpID
         NO-ERROR.
         IF AVAILABLE DumpFile
         THEN DumpFile.Active = NO.
      END.

   END FINALLY.

END FUNCTION.

PROCEDURE pStart:

   DEFINE VARIABLE lii       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liCount   AS INTEGER   NO-UNDO.

   FIND FIRST DumpHPD NO-LOCK WHERE
      DumpHPD.DumpID = giDumpID
   NO-ERROR.

   IF NOT AVAILABLE DumpHPD
   THEN DO:
      LOG-MANAGER:WRITE-MESSAGE("The DumpHPD record is not available", "ERROR").
   END.

   DO TRANSACTION WHILE TRUE:

      liCount = liCount + 1.

      IF NOT DumpHPD.Active
      THEN DO:
         LOG-MANAGER:WRITE-MESSAGE("The dump is not active!", "INFO").
         LEAVE.
      END.

      /* If there is no final time then we have dumped everything
         to the current time */
      IF DumpHPD.FinalTime EQ ""
      THEN LEAVE.

      IF fProcess(giDumpID)
      THEN LEAVE.

      /* Loops maximum 1000 times just to be sure that no eternal loop happens */
      IF liCount > 1000
      THEN LEAVE.

      PAUSE 5 NO-MESSAGE.

   END.

END PROCEDURE.
