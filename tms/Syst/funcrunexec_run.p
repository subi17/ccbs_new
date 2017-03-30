/* ---------------------------------------------------------------------------
  MODULE .......: FuncRunExec_run.p
  FUNCTION .....: perform function execution
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 21.04.10
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/log.i}
{Syst/eventlog.i}
{Func/multitenantfunc.i}

DEF INPUT PARAMETER iiFRExecID AS INT  NO-UNDO.
DEF INPUT PARAMETER icHost     AS CHAR NO-UNDO.

DEF VAR llNewRun AS LOG  NO-UNDO.
DEF VAR lcTenant AS CHARACTER NO-UNDO. 

FUNCTION fRunTime RETURNS INTEGER
   (icTime AS CHAR):

   DEF VAR liTime     AS INT NO-UNDO.
   DEF VAR liCalcTime AS INT NO-UNDO.
   DEF VAR liRunTime  AS INT NO-UNDO.
   
   DO liTime = 1 TO NUM-ENTRIES(icTime,":"):
      liCalcTime = 0.
      liCalcTime = INTEGER(ENTRY(liTime,icTime,":")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN ?.
            
      CASE liTime:
      WHEN 1 THEN liCalcTime = liCalcTime * 3600.
      WHEN 2 THEN liCalcTime = liCalcTime * 60.
      END CASE.
            
      liRunTime = liRunTime + liCalcTime.
   END.
    
   RETURN liRunTime.
   
END FUNCTION.

FUNCTION fErrorLog RETURNS LOGIC
   (iiFRExecID AS INT,
    icError AS CHAR):
   
   DO TRANS:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = gcBrand
             ErrorLog.ActionID  = "FREXEC" + STRING(iiFRExecID)
             ErrorLog.TableName = "FuncRunExec"
             ErrorLog.KeyValue  = STRING(iiFRExecID)
             ErrorLog.ErrorMsg  = icError
             ErrorLog.UserCode  = katun.
             ErrorLog.ActionTS  = fMakeTS().
   END.
   
END FUNCTION.


/********** Main start ***********/

RUN pInitialize(iiFRExecID,
                OUTPUT llNewRun).
IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.

IF icHost = "" THEN DO:
   IF llNewRun THEN
      RUN pLaunchProcesses(iiFRExecID).

   RUN pFinalize(iiFRExecID).
END.

/* other hosts (replica) only run already initialized processes */
ELSE RUN pLaunchInOtherHost(iiFRExecID).

RETURN "".

/****** MAIN end  ******************/


PROCEDURE pInitialize:

   DEF INPUT  PARAMETER iiFRExecID AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER olNewRun   AS LOG  NO-UNDO.

   DEF VAR lcError   AS CHAR   NO-UNDO. 

   DEF BUFFER bFuncExec FOR FuncRunExec.

   olNewRun = FALSE.
   
   lcTenant = fGetCurrentBrand().
   IF lcTenant EQ "" THEN DO:
      lcError = "Unknown or super tenant".
      fErrorLog(iiFRExecID,lcError).
      RETURN "ERROR:" + lcError.
   END.
   
   FIND FIRST FuncRunExec WHERE FuncRunExec.FRExecID = iiFRExecID 
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunExec THEN DO:
      lcError = "Unknown execution ID".
      fErrorLog(iiFRExecID,lcError).
      RETURN "ERROR:" + lcError.
   END.

   FIND FIRST FuncRunConfig WHERE 
      FuncRunConfig.FRConfigID = FuncRunExec.FRConfigID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunConfig THEN DO:
      lcError = "Unknown configuration ID: " + STRING(FuncRunExec.FRConfigID).
      fErrorLog(iiFRExecID,lcError).
      RETURN "ERROR:" + lcError.
   END.
 
   IF FuncRunExec.RunState = "Initialized" THEN DO:
   
      olNewRun = TRUE.

      /* has the one that is waited for already finished */
      IF FuncRunExec.WaitForExecSeq > 0 THEN DO:
         FIND FIRST bFuncExec WHERE 
                 bFuncExec.FRQScheduleID = FuncRunExec.FRQScheduleID AND
                 bFuncExec.FRExecID      = FuncRunExec.WaitForExecSeq
            NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bFuncExec OR bFuncExec.RunState NE "Finished" 
         THEN DO:
            lcError = "Previous run has not finished yet".
            RETURN "INFORMATION:" + lcError.
         END.   
      END.
   
      /* earliest possible run time */
      IF FuncRunExec.MinStartTime > "" THEN DO:
         IF fRunTime(FuncRunExec.MinStartTime) > TIME THEN DO:
            lcError = "Run time has not been passed yet".
            RETURN "INFORMATION:" + lcError.
         END.
      END.
   END.

   RETURN "".
   
END PROCEDURE.
  
PROCEDURE pLaunchProcesses:
   
   DEF INPUT PARAMETER iiFRExecID  AS INT  NO-UNDO.
   
   DEF VAR liProcQty  AS INT  NO-UNDO.
   DEF VAR liProcID   AS INT  NO-UNDO.
   DEF VAR liStarted  AS INT  NO-UNDO.
   DEF VAR lcMessage  AS CHAR NO-UNDO.
   DEF VAR liTotalQty AS INT  NO-UNDO.
   
   DO TRANS:
      FIND FIRST FuncRunExec WHERE FuncRunExec.FRExecID = iiFRExecID
         EXCLUSIVE-LOCK.
      ASSIGN 
         FuncRunExec.StartTS  = fMakeTS()
         FuncRunExec.RunState = "Running".

      CREATE FuncRunExecLog.
      ASSIGN 
         FuncRunExecLog.FRExecID    = iiFRExecID
         FuncRunExecLog.StatusStamp = FuncRunExec.StartTS
         FuncRunExecLog.FRStatus    = FuncRunExec.RunState.
   END.
  
   FIND FIRST FuncRunExec WHERE 
      FuncRunExec.FRExecID = iiFRExecID NO-LOCK.
   FIND FIRST FuncRunConfig WHERE 
      FuncRunConfig.FRConfigID = FuncRunExec.FRConfigID NO-LOCK.

   liProcID = 1.
   FIND LAST FuncRunProcess USE-INDEX FRProcessID NO-LOCK NO-ERROR.
   IF AVAILABLE FuncRunProcess THEN liProcID = FuncRunProcess.FRProcessID + 1.
   
   /* if there are feeds created for this, start as many processes as there
      are feed groups, otherwise use qty from configuration */
   liTotalQty = FuncRunConfig.RunQty.
   IF FuncRunExec.FeedFromExecSeq > 0 THEN DO:
      FIND LAST FuncRunResult USE-INDEX FRExecID WHERE
         FuncRunResult.FRExecID = FuncRunExec.FeedFromExecSeq 
            NO-LOCK NO-ERROR.
      IF AVAILABLE FuncRunResult THEN 
         liTotalQty = FuncRunResult.FRResultSeq.
   END.
      
   /* create processes and launch them in os */
   DO liProcQty = 1 TO liTotalQty TRANS:
   
      REPEAT:
         IF NOT CAN-FIND(FIRST FuncRunProcess WHERE 
            FuncRunProcess.FRProcessID = liProcID) THEN LEAVE.
         liProcID = liProcID + 1.
      END.
      
      CREATE FuncRunProcess.
      ASSIGN
         FuncRunProcess.FRProcessID = liProcID   
         FuncRunProcess.FRConfigID  = FuncRunConfig.FRConfigID
         FuncRunProcess.FRExecID    = FuncRunExec.FRExecID
         FuncRunProcess.ProcSeq     = liProcQty
         FuncRunProcess.RunState    = "Initialized"
         FuncRunProcess.RunCommand  = REPLACE(FuncRunConfig.RunCommand,
                                            "#PARAM",
                                            STRING(liProcID))
         FuncRunProcess.RunCommand  = REPLACE(FuncRunProcess.RunCommand,
                                            "#TENANT",
                                             STRING(lcTenant))
         FuncRunProcess.StartTS     = fMakeTS().
                       
      /* run here or in another host */
      FOR FIRST FuncRunResult NO-LOCK USE-INDEX FRExecID WHERE
                FuncRunResult.FRExecID = FuncRunExec.FeedFromExecSeq AND
                FuncRunResult.FRResultSeq = liProcQty:
         FuncRunProcess.ProcessHost = FuncRunResult.ProcessHost.       
      END.

      FuncRunProcess.RunCommand = REPLACE(FuncRunProcess.RunCommand,
                                          "#HOST",
                                          FuncRunProcess.ProcessHost).
                                 
      IF FuncRunProcess.ProcessHost = "" THEN 
         UNIX SILENT VALUE(FuncRunProcess.RunCommand + " >/dev/null 2>&1").
      
      liStarted = liStarted + 1.
   END.

   IF liStarted = 0 THEN DO TRANS:
      FIND FIRST FuncRunExec WHERE FuncRunExec.FRExecID = iiFRExecID
         EXCLUSIVE-LOCK.
      ASSIGN 
         FuncRunExec.EndTS    = fMakeTS()
         FuncRunExec.RunState = "Finished"
         lcMessage            = "No processes were started".
      
      CREATE FuncRunExecLog.
      ASSIGN 
         FuncRunExecLog.FRExecID    = iiFRExecID
         FuncRunExecLog.StatusStamp = FuncRunExec.EndTS
         FuncRunExecLog.FRStatus    = FuncRunExec.RunStat.
   END.
   
   ELSE DO:
      fELog("FUNCRUNEXEC_" + STRING(iiFRExecID),
            "started" + STRING(liStarted)).
      lcMessage = STRING(liStarted) + " processes started at " +
                  STRING(TIME,"hh:mm:ss").
   END.   

   RUN Syst/funcrunexec_notify.p (iiFRExecID,
                             lcMessage).
                              
END.        
 
PROCEDURE pLaunchInOtherHost:

   DEF INPUT PARAMETER iiFRExecID  AS INT  NO-UNDO.
   
   FIND FIRST FuncRunExec WHERE 
      FuncRunExec.FRExecID = iiFRExecID NO-LOCK.
   FIND FIRST FuncRunConfig WHERE 
      FuncRunConfig.FRConfigID = FuncRunExec.FRConfigID NO-LOCK.

   /* launch processes in os */
   FOR EACH FuncRunProcess NO-LOCK WHERE
            FuncRunProcess.FRConfigID = FuncRunConfig.FRConfigID AND
            FuncRunProcess.FRExecID = FuncRunExec.FRExecID AND
            FuncRunProcess.RunState = "Initialized" AND
            FuncRunProcess.ProcessHost = icHost:
            
      UNIX SILENT VALUE(FuncRunProcess.RunCommand + " >/dev/null 2>&1").
   END.

END PROCEDURE.

PROCEDURE pFinalize:

   DEF INPUT  PARAMETER iiFRExecID   AS INT  NO-UNDO.

   DEF VAR lcActionKey AS CHAR NO-UNDO.
   DEF VAR llDone      AS LOG  NO-UNDO.
   DEF VAR llCancelled AS LOG  NO-UNDO.
   

   FIND FIRST FuncRunExec WHERE FuncRunExec.FRExecID = iiFRExecID NO-LOCK.

   /* check if everything is done and mark execution finished */
   IF FuncRunExec.RunState = "Running" THEN DO TRANS:

      ASSIGN   
         llDone      = TRUE
         llCancelled = FALSE.
      
      FOR EACH FuncRunProcess NO-LOCK WHERE
               FuncRunProcess.FRConfigID = FuncRunExec.FRConfigID AND 
               FuncRunProcess.FRExecID = iiFRExecID:
               
         IF LOOKUP(FuncRunProcess.RunState,"Finished,Cancelled") = 0 THEN
            llDone = FALSE.
         IF FuncRunProcess.RunState = "Cancelled" THEN
            llCancelled = TRUE.
      END.

      IF NOT llDone THEN RETURN.
      
      lcActionKey = STRING(iiFRExecID).

      FIND FIRST ActionLog WHERE
                 ActionLog.Brand     = gcBrand AND
                 ActionLog.TableName = "FuncRunExec" AND
                 ActionLog.KeyValue  = lcActionKey AND
                 ActionLog.ActionID  = "FREXEC" + STRING(iiFRExecID) AND
                 ActionLog.ActionStatus = 0 EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE ActionLog THEN DO:
         IF llCancelled THEN ActionLog.ActionStatus = 5.
         ELSE ActionLog.ActionStatus = 2.
      END.
      
      FIND CURRENT FuncRunExec EXCLUSIVE-LOCK.
      ASSIGN 
         FuncRunExec.EndTS    = fMakeTS()
         FuncRunExec.RunState = IF llCancelled 
                                THEN "Cancelled" 
                                ELSE "Finished".

      CREATE FuncRunExecLog.
      ASSIGN 
         FuncRunExecLog.FRExecID    = iiFRExecID
         FuncRunExecLog.StatusStamp = FuncRunExec.EndTS
         FuncRunExecLog.FRStatus    = FuncRunExec.RunState.

      fELog("FUNCRUNEXEC_" + STRING(iiFRExecID),FuncRunExec.RunState).
      
      RUN Syst/funcrunexec_notify.p (iiFRExecID,
                                FuncRunExec.RunState + " at " +
                                   STRING(TIME,"hh:mm:ss")).
   END.
   
END PROCEDURE.



