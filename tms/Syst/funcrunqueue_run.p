/* ----------------------------------------------------------------------
  MODULE .......: FuncRunQueue_run.p
  TASK .........: Run a function queue
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 20.04.10
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/timestamp.i}
{Syst/eventlog.i}

DEF INPUT  PARAMETER iiFRQueueID     AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRQScheduleID AS INT  NO-UNDO.

DEF VAR ldStarted     AS DEC    NO-UNDO. 
DEF VAR lcError       AS CHAR   NO-UNDO. 
DEF VAR llNewRun      AS LOG    NO-UNDO.

FUNCTION fErrorLog RETURNS LOGIC
   (icError AS CHAR):
   
   DO TRANS:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = gcBrand
             ErrorLog.ActionID  = "FRQUEUERUN" + STRING(iiFRQScheduleID)
             ErrorLog.TableName = "FuncRunQSchedule"
             ErrorLog.KeyValue  = STRING(iiFRQScheduleID)
             ErrorLog.ErrorMsg  = icError
             ErrorLog.UserCode  = katun.
             ErrorLog.ActionTS  = fMakeTS().
   END.
   
END FUNCTION.


/****** MAIN start ******************/
      
RUN pInitialize(iiFRQueueID,
                iiFRQScheduleID,
                OUTPUT llNewRun).
IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.

IF llNewRun THEN 
   RUN pCreateExecutions(iiFRQueueID,
                         iiFRQScheduleID).

ELSE 
   RUN pFinalize(iiFRQueueID,
                 iiFRQScheduleID).

/****** MAIN end  ******************/


PROCEDURE pInitialize:

   DEF INPUT  PARAMETER iiFRQueueID     AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiFRQScheduleID AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER olNewRun        AS LOG  NO-UNDO.

   DEF VAR lcActionKey   AS CHAR   NO-UNDO.

   olNewRun = FALSE.
   
   FIND FIRST FuncRunQueue WHERE 
      FuncRunQueue.FRQueueID = iiFRQueueID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunQueue THEN DO:
      lcError = "Queue configuration not available".
      fErrorLog(lcError).
      RETURN "ERROR:" + lcError.
   END.

   IF NOT FuncRunQueue.Active THEN DO:
      lcError = "Queue configuration is inactive".
      fErrorLog(lcError).
      RETURN "INFORMATION:" + lcError.
   END.

   FIND FIRST FuncRunQSchedule WHERE 
      FuncRunQSchedule.FRQScheduleID = iiFRQScheduleID
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FuncRunQSchedule OR 
      FuncRunQSchedule.FRQueueID NE iiFRQueueID 
   THEN DO:
      lcError = "Scheduling configuration not available".
      fErrorLog(lcError).
      RETURN "ERROR:" + lcError.
   END.

   /* start executing the queue */
   IF FuncRunQSchedule.RunState = "Scheduled" THEN DO:
      olNewRun = TRUE.
   
      ASSIGN
         lcActionKey = STRING(iiFRQScheduleID)
         ldStarted   = fMakeTS().              

      /* already running */
      IF CAN-FIND(FIRST ActionLog WHERE
                 ActionLog.Brand     = gcBrand AND
                 ActionLog.TableName = "FuncRunQueue" AND
                 ActionLog.KeyValue  = lcActionKey AND
                 ActionLog.ActionID  = "FRQUEUE" + STRING(iiFRQueueID) AND
                 ActionLog.ActionStatus = 0)
      THEN DO:
         lcError = "Already running".
         fErrorLog(lcError).
         RETURN "INFORMATION:" + lcError.
      END.


      /* mark as started */
      DO TRANS:

         FIND FIRST FuncRunQSchedule WHERE 
            FuncRunQSchedule.FRQScheduleID = iiFRQScheduleID
            EXCLUSIVE-LOCK.
         FuncRunQSchedule.RunState = "Running".
      
         CREATE ActionLog.
         ASSIGN 
            ActionLog.Brand        = gcBrand   
            ActionLog.TableName    = "FuncRunQueue"  
            ActionLog.KeyValue     = lcActionKey
            ActionLog.ActionID     = "FRQUEUE" + STRING(iiFRQueueID)
            ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
            ActionLog.ActionStatus = 0
            ActionLog.UserCode     = katun
            ActionLog.ActionTS     = ldStarted
            ActionLog.ActionChar   = 
               "Scheduling: " + STRING(iiFRQScheduleID) + CHR(10) +
               "Run mode: " + FuncRunQSchedule.RunMode.
      END.
   END.
   
   RETURN "".
   
END PROCEDURE.

PROCEDURE pCreateExecutions:

   DEF INPUT PARAMETER iiFRQueueID     AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiFRQScheduleID AS INT  NO-UNDO.

   DEF VAR liExecID  AS INT  NO-UNDO.
   DEF VAR liExecSeq AS INT  NO-UNDO.
   
   DEF BUFFER bFuncExec FOR FuncRunExec.

   FIND FIRST FuncRunQSchedule WHERE 
              FuncRunQSchedule.FRQScheduleID = iiFRQScheduleID NO-LOCK.
   
   /* initialize executions according to queue definitions, 
      FuncRunExec_run takes then care of running them */
      
   FOR EACH FuncRunQRow NO-LOCK WHERE
            FuncRunQRow.FRQueueID = iiFRQueueID,
      FIRST FuncRunConfig NO-LOCK WHERE
            FuncRunConfig.FRConfigID = FuncRunQRow.FRConfigID
   BY FuncRunQRow.FRQRowSeq:
            
      liExecID = 1.
      FIND LAST FuncRunExec USE-INDEX FRExecID NO-LOCK NO-ERROR.
      IF AVAILABLE FuncRunExec THEN liExecID = FuncRunExec.FRExecID + 1.
           
      liExecSeq = 1.
      FIND LAST FuncRunExec USE-INDEX FRExecSeq WHERE 
                FuncRunExec.FRConfigID = FuncRunConfig.FRConfigID
                NO-LOCK NO-ERROR.
      IF AVAILABLE FuncRunExec THEN liExecSeq = FuncRunExec.FRExecSeq + 1.
     
      CREATE FuncRunExec.
      ASSIGN 
         FuncRunExec.Brand         = FuncRunConfig.Brand   
         FuncRunExec.FRConfigID    = FuncRunConfig.FRConfigID
         FuncRunExec.FRExecID      = liExecID
         FuncRunExec.FRExecSeq     = liExecSeq
         FuncRunExec.RunState      = "Initialized"
         FuncRunExec.FRQueueID     = iiFRQueueID
         FuncRunExec.FRQScheduleID = iiFRQScheduleID
         FuncRunExec.FRQRowSeq     = FuncRunQRow.FRQRowSeq
         FuncRunExec.MinStartTime  = FuncRunQRow.MinStartTime
         FuncRunExec.RunMode       = FuncRunQSchedule.RunMode.
   
      IF FuncRunQRow.WaitForRowSeq > 0 AND 
         FuncRunQRow.WaitForRowSeq < FuncRunQRow.FRQRowSeq THEN 
      FOR FIRST bFuncExec NO-LOCK WHERE
                bFuncExec.FRQScheduleID = iiFRQScheduleID AND
                bFuncExec.FRQRowSeq     = FuncRunQRow.WaitForRowSeq:
         FuncRunExec.WaitForExecSeq = bFuncExec.FRExecID.        
      END.

      IF FuncRunQRow.FeedFromRowSeq > 0 AND 
         FuncRunQRow.FeedFromRowSeq < FuncRunQRow.FRQRowSeq THEN 
      FOR FIRST bFuncExec NO-LOCK WHERE
                bFuncExec.FRQScheduleID = iiFRQScheduleID AND
                bFuncExec.FRQRowSeq     = FuncRunQRow.FeedFromRowSeq:
         FuncRunExec.FeedFromExecSeq = bFuncExec.FRExecID.        
      END.

      CREATE FuncRunExecLog.
      ASSIGN 
         FuncRunExecLog.FRExecID    = FuncRunExec.FRExecID
         FuncRunExecLog.StatusStamp = ldStarted
         FuncRunExecLog.FRStatus    = FuncRunExec.RunState.
        
   END.

   /* no executions found */
   IF liExecID = 0 THEN RUN pCancelQueue(iiFRQueueID,
                                         iiFRQScheduleID,
                                         "No execution data").

END PROCEDURE.

PROCEDURE pCancelQueue:

   DEF INPUT PARAMETER iiFRQueueID     AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiFRQScheduleID AS INT  NO-UNDO.
   DEF INPUT PARAMETER icMessage         AS CHAR NO-UNDO.

   DO TRANS:
   
      FIND FIRST ActionLog WHERE
                 ActionLog.Brand     = gcBrand AND
                 ActionLog.TableName = "FuncRunQueue" AND
                 ActionLog.KeyValue  = STRING(iiFRQScheduleID) AND
                 ActionLog.ActionID  = "FRQUEUE" + STRING(iiFRQueueID) AND
                 ActionLog.ActionStatus = 0 EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE ActionLog THEN ASSIGN 
         ActionLog.ActionStatus = 5
         ActionLog.ActionChar = ActionLog.ActionChar + CHR(10) + 
                                "Cancelled: " + icMessage.
                                
      FIND FIRST FuncRunQSchedule WHERE 
         FuncRunQSchedule.FRQScheduleID = iiFRQScheduleID
         EXCLUSIVE-LOCK.
      ASSIGN
         FuncRunQSchedule.RunState = "Cancelled"
         FuncRunQSchedule.DoneTS   = fMakeTS().
   END.
   
END PROCEDURE.

PROCEDURE pFinalize:

   DEF INPUT  PARAMETER iiFRQueueID     AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiFRQScheduleID AS INT  NO-UNDO.

   DEF VAR lcActionKey AS CHAR NO-UNDO.
   DEF VAR llDone      AS LOG  NO-UNDO.

   FIND FIRST FuncRunQueue WHERE 
      FuncRunQueue.FRQueueID = iiFRQueueID NO-LOCK.
   FIND FIRST FuncRunQSchedule WHERE 
      FuncRunQSchedule.FRQScheduleID = iiFRQScheduleID NO-LOCK.

   /* check if everything is done */
   IF FuncRunQSchedule.RunState = "Running" THEN DO TRANS:

      llDone = TRUE.
      
      FOR EACH FuncRunExec NO-LOCK WHERE
               FuncRunExec.FRQScheduleID = iiFRQScheduleID:
               
         IF LOOKUP(FuncRunExec.RunState,"Finished,Cancelled") = 0 THEN
            llDone = FALSE.
      END.

      IF NOT llDone THEN RETURN.
      
      lcActionKey = STRING(iiFRQScheduleID).

      FIND FIRST ActionLog WHERE
                 ActionLog.Brand     = gcBrand AND
                 ActionLog.TableName = "FuncRunQueue" AND
                 ActionLog.KeyValue  = lcActionKey AND
                 ActionLog.ActionID  = "FRQUEUE" + STRING(iiFRQueueID) AND
                 ActionLog.ActionStatus = 0 EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE ActionLog THEN ActionLog.ActionStatus = 2.
      
      FIND CURRENT FuncRunQSchedule EXCLUSIVE-LOCK.
      ASSIGN 
         FuncRunQSchedule.DoneTS   = fMakeTS()
         FuncRunQSchedule.RunState = "Finished".

   END.
   
END PROCEDURE.


