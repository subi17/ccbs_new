/* funcrunprocess_update.i     22.04.10/aam 
*/

{commali.i}
{timestamp.i}

FUNCTION fProcessErrorLog RETURNS LOGIC
   (iiFRProcessID AS INT,
    icMessage     AS CHAR):
 
   DO TRANS:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = gcBrand
             ErrorLog.ActionID  = "FRPROCESS" + STRING(iiFRProcessID)
             ErrorLog.TableName = "FuncRunProcess"
             ErrorLog.KeyValue  = STRING(iiFRProcessID)
             ErrorLog.ErrorMsg  = icMessage
             ErrorLog.UserCode  = katun.
             ErrorLog.ActionTS  = fMakeTS().
   END.
   
END FUNCTION.

FUNCTION fUpdateFuncRunProgress RETURNS LOGIC
   (iiFRProcessID AS INT,
    iiProcessed   AS INT):
   
   FIND FIRST FuncRunProcess WHERE FuncRunProcess.FRProcessID = iiFRProcessID
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunProcess THEN RETURN FALSE.
   
   IF FuncRunProcess.RunState NE "Running" THEN DO:

      IF FuncRunProcess.RunState = "Paused" THEN DO:
         RUN pWaitForContinuing(iiFRProcessID).
         IF RETURN-VALUE BEGINS "ERROR" THEN RETURN FALSE.
      END.

      ELSE IF FuncRunProcess.RunState = "Cancelled" THEN DO:
         fProcessErrorLog(iiFRProcessID,
                          "Cancelled while running").
         RETURN FALSE.
      END.
   
      ELSE RETURN FALSE.
   END.
   
   DO TRANS:
      FIND CURRENT FuncRunProcess EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAILABLE FuncRunProcess THEN DO:
         ASSIGN
            FuncRunProcess.LastTS    = fMakeTS()
            FuncRunProcess.Processed = iiProcessed.
         RELEASE FuncRunProcess.   
      END.   
   END.

   RETURN TRUE. 
   
END FUNCTION.

 
PROCEDURE pWaitForContinuing:

   DEF INPUT PARAMETER iiFRProcessID AS INT  NO-UNDO.

   DEF VAR liLoops    AS INT  NO-UNDO.
   
   DEF BUFFER bPausedProcess FOR FuncRunProcess.
   
   DO liLoops = 1 TO 60:
      PAUSE 60 NO-MESSAGE. 

      FIND FIRST bPausedProcess WHERE 
         bPausedProcess.FRProcessID = iiFRProcessID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bPausedProcess THEN RETURN "ERROR".
           
      IF bPausedProcess.RunState NE "Paused" THEN DO:

         IF bPausedProcess.RunState = "Cancelled" THEN DO:
            fProcessErrorLog(iiFRProcessID,
                            "Cancelled when paused").
            RETURN "ERROR".
         END.

         RETURN "".
      END.
   END.
      
   /* waiting got to the end of loop  */
   fProcessErrorLog(iiFRProcessID,
                    "Pause overdue").
   RETURN "ERROR".

END PROCEDURE.

