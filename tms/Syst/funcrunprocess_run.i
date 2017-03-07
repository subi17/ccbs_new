/* funcrunprocess_run.i     22.04.10/aam 
*/

{Syst/commali.i}
{Func/timestamp.i}

DEF TEMP-TABLE ttFunctionParam NO-UNDO
   FIELD ParamSeq  AS INT  
   FIELD DateParam AS DATE 
   FIELD DecParam  AS DEC  
   FIELD CharParam AS CHAR 
   FIELD IntParam  AS INT
   FIELD LogParam  AS LOG
   INDEX ParamSeq ParamSeq.

   
FUNCTION fSetFuncRunDateParameter RETURNS DATE
   (iiParamSeq AS INT):

   FIND FIRST ttFunctionParam WHERE ttFunctionParam.ParamSeq = iiParamSeq
      NO-ERROR.
   IF AVAILABLE ttFunctionParam THEN RETURN ttFunctionParam.DateParam.
   ELSE RETURN ?.
END FUNCTION.

FUNCTION fSetFuncRunDecParameter RETURNS DEC
   (INPUT iiParamSeq AS INT):

   FIND FIRST ttFunctionParam WHERE ttFunctionParam.ParamSeq = iiParamSeq
      NO-ERROR.
   IF AVAILABLE ttFunctionParam THEN RETURN ttFunctionParam.DecParam.
   ELSE RETURN ?.
END FUNCTION.

FUNCTION fSetFuncRunCharParameter RETURNS CHAR
   (INPUT iiParamSeq AS INT):

   FIND FIRST ttFunctionParam WHERE ttFunctionParam.ParamSeq = iiParamSeq
      NO-ERROR.
   IF AVAILABLE ttFunctionParam THEN RETURN ttFunctionParam.CharParam.
   ELSE RETURN ?.
END FUNCTION.

FUNCTION fSetFuncRunIntParameter RETURNS INT
   (INPUT iiParamSeq AS INT):

   FIND FIRST ttFunctionParam WHERE ttFunctionParam.ParamSeq = iiParamSeq
      NO-ERROR.
   IF AVAILABLE ttFunctionParam THEN RETURN ttFunctionParam.IntParam.
   ELSE RETURN ?.
END FUNCTION.

FUNCTION fSetFuncRunLogParameter RETURNS LOGIC
   (INPUT iiParamSeq AS INT):

   FIND FIRST ttFunctionParam WHERE ttFunctionParam.ParamSeq = iiParamSeq
      NO-ERROR.
   IF AVAILABLE ttFunctionParam THEN RETURN ttFunctionParam.LogParam.
   ELSE RETURN ?.
END FUNCTION.

   
PROCEDURE pInitializeFuncRunProcess:

   DEF OUTPUT PARAMETER oiFRProcessID    AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER oiFRExecID       AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER ocRunMode        AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER oiUpdateInterval AS INT  NO-UNDO.
   
   oiFRProcessID = INTEGER(SESSION:PARAMETER) NO-ERROR.
   IF ERROR-STATUS:ERROR OR oiFRProcessID = 0 THEN 
      RETURN "ERROR:Invalid session parameter".
   
   FIND FIRST FuncRunProcess WHERE FuncRunProcess.FRProcessID = oiFRProcessID
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunProcess THEN 
      RETURN "ERROR:Process data not available".
   
   IF FuncRunProcess.RunState NE "Initialized" THEN 
      RETURN "ERROR:Process not in initialized state".
      
   FIND FIRST FuncRunExec WHERE 
      FuncRunExec.FRExecID = FuncRunProcess.FRExecID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunExec THEN 
      RETURN "ERROR:Execution data not available".
      
   ASSIGN
      ocRunMode  = FuncRunExec.RunMode
      oiFRExecID = FuncRunExec.FRExecID.

   FIND FIRST FuncRunConfig WHERE
      FuncRunConfig.FRConfigID = FuncRunExec.FRConfigID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunConfig THEN 
      RETURN "ERROR:Configuration not available".
      
   oiUpdateInterval = FuncRunConfig.StatusInterval.
      
   DO TRANS:
      FIND FIRST _MyConnection NO-LOCK.

      FIND CURRENT FuncRunProcess EXCLUSIVE-LOCK.
      ASSIGN
         FuncRunProcess.ProcessID = _MyConnection._MyConn-Pid
         FuncRunProcess.StartTS   = fMakeTS()
         FuncRunProcess.RunState  = "Running".
   END.
    
   RETURN "".
   
END PROCEDURE.

PROCEDURE pFinalizeFuncRunProcess:

   DEF INPUT PARAMETER iiFRProcessID  AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiProcessed AS INT  NO-UNDO.
   
   FIND FIRST FuncRunProcess WHERE FuncRunProcess.FRProcessID = iiFRProcessID
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunProcess THEN 
      RETURN "ERROR:Process data not available".
   
   IF FuncRunProcess.RunState NE "Running" THEN 
      RETURN "ERROR:Process not in running state".

   DO TRANS:
      FIND CURRENT FuncRunProcess EXCLUSIVE-LOCK.
      ASSIGN
         FuncRunProcess.EndTS     = fMakeTS()
         FuncRunProcess.RunState  = "Finished"
         FuncRunProcess.Processed = iiProcessed.
         
      RELEASE FuncRunProcess.
   END.
    
END PROCEDURE.

PROCEDURE pCancelFuncRunProcess:

   DEF INPUT PARAMETER iiFRProcessID AS INT  NO-UNDO.
   DEF INPUT PARAMETER icMessage  AS CHAR NO-UNDO.
   
   FIND FIRST FuncRunProcess WHERE FuncRunProcess.FRProcessID = iiFRProcessID
      NO-LOCK NO-ERROR.

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
    
   IF NOT AVAILABLE FuncRunProcess THEN 
      RETURN "ERROR:Process data not available".
   
   IF FuncRunProcess.RunState NE "Running" THEN 
      RETURN "ERROR:Process not in running state".
      
   DO TRANS:
      FIND CURRENT FuncRunProcess EXCLUSIVE-LOCK.
      ASSIGN
         FuncRunProcess.EndTS    = fMakeTS()
         FuncRunProcess.RunState = "Cancelled".
      RELEASE FuncRunProcess.
   END.
END PROCEDURE.

PROCEDURE pGetFuncRunProcessParameters:

   DEF INPUT PARAMETER iiFRProcessID AS INT  NO-UNDO.

   EMPTY TEMP-TABLE ttFunctionParam.
   
   FOR FIRST FuncRunProcess NO-LOCK WHERE
             FuncRunProcess.FRProcessID = iiFRProcessID,
       FIRST FuncRunExec NO-LOCK WHERE
             FuncRunExec.FRExecID = FuncRunProcess.FRExecID,
        EACH FuncRunQSParam NO-LOCK WHERE
             FuncRunQSParam.FRQScheduleID = FuncRunExec.FRQScheduleID AND
             FuncRunQSParam.FRQRowSeq     = FuncRunExec.FRQRowSeq:
       
      CREATE ttFunctionParam.
      BUFFER-COPY FuncRunQSParam TO ttFunctionParam.
   END.
  
END PROCEDURE.


