/* ---------------------------------------------------------------------------
  MODULE .......: FuncRunQueue_batch.p
  FUNCTION .....: batch run for function queues
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 21.04.10
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN 
   gcBrand = "1" 
   katun   = "Cron".
       
{Func/lib/eventlog.i}
{Func/timestamp.i}

DEF TEMP-TABLE ttStatus NO-UNDO
   FIELD RunState AS CHAR.
   
DEF VAR lcOrigState AS CHAR NO-UNDO.
DEF VAR liHandled   AS INT  NO-UNDO.
DEF VAR ldCurrent   AS DEC  NO-UNDO.

ldCurrent = fMakeTS().

CREATE ttStatus.
ttStatus.RunState = "Scheduled".
CREATE ttStatus.
ttStatus.RunState = "Running".


FOR EACH FuncRunQueue NO-LOCK WHERE
         FuncRunQueue.Brand  = gcBrand AND
         FuncRunQueue.Active = TRUE,
    EACH ttStatus,
    EACH FuncRunQSchedule NO-LOCK WHERE
         FuncRunQSchedule.FRQueueID = FuncRunQueue.FRQueueID AND
         FuncRunQSchedule.RunState  = ttStatus.RunState AND
         FuncRunQSchedule.StartTS  <= ldCurrent
BY FuncRunQSchedule.StartTS:

   lcOrigState = FuncRunQSchedule.RunState.
   
   RUN Syst/funcrunqueue_run.p (FuncRunQSchedule.FRQueueID,
                           FuncRunQSchedule.FRQScheduleID).
   
   /* nothing done or interrupted */
   IF RETURN-VALUE BEGINS "ERROR:" OR RETURN-VALUE BEGINS "INFORMATION:"
   THEN DO:

      IF RETURN-VALUE BEGINS "ERROR:" THEN DO TRANS:
         CREATE ErrorLog.
         ASSIGN 
            ErrorLog.Brand     = gcBrand
            ErrorLog.ActionID  = "FRQUEUERUN" + 
                                 STRING(FuncRunQueue.FRQueueID)
            ErrorLog.TableName = "FuncRunQueue"
            ErrorLog.KeyValue  = STRING(FuncRunQueue.FRQueueID)
            ErrorLog.ErrorMsg  = RETURN-VALUE
            ErrorLog.UserCode  = katun.
            ErrorLog.ActionTS  = fMakeTS().
      END.

   END.   

   IF FuncRunQSchedule.RunState NE lcOrigState THEN 
      liHandled = liHandled + 1.
   
END.

EMPTY TEMP-TABLE ttStatus.

IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1.

fELog("FUNCRUNQUEUE_RUN","handled" + STRING(liHandled)).

QUIT.

