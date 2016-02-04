/* ---------------------------------------------------------------------------
  MODULE .......: FuncRunExec_batch.p
  FUNCTION .....: batch run for performing function execution
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
DEF VAR lcHost      AS CHAR NO-UNDO.

IF SOURCE-PROCEDURE:PRIVATE-DATA BEGINS "Host:" THEN 
   lcHost = ENTRY(2,SOURCE-PROCEDURE:PRIVATE-DATA,":").

CREATE ttStatus.
ttStatus.RunState = "Initialized".
CREATE ttStatus.
ttStatus.RunState = "Running".


FOR EACH ttStatus,
    EACH FuncRunExec NO-LOCK WHERE
         FuncRunExec.Brand    = gcBrand AND
         FuncRunExec.RunState = ttStatus.RunState
BY FuncRunExec.FRExecID:

   lcOrigState = FuncRunExec.RunState.
   
   RUN Syst/funcrunexec_run.p (FuncRunExec.FRExecID,
                          lcHost).
   
   /* nothing done or interrupted */
   IF RETURN-VALUE BEGINS "ERROR:" OR RETURN-VALUE BEGINS "INFORMATION:"
   THEN DO:
        
      IF RETURN-VALUE BEGINS "ERROR:" THEN DO TRANS:
         CREATE ErrorLog.
         ASSIGN 
            ErrorLog.Brand     = gcBrand
            ErrorLog.ActionID  = "FREXECRUN" + STRING(FuncRunExec.FRExecID)
            ErrorLog.TableName = "FuncRunExec"
            ErrorLog.KeyValue  = STRING(FuncRunExec.FRExecID)
            ErrorLog.ErrorMsg  = RETURN-VALUE
            ErrorLog.UserCode  = katun.
            ErrorLog.ActionTS  = fMakeTS().
      END.

   END.   

   IF FuncRunExec.RunState NE lcOrigState THEN 
      liHandled = liHandled + 1.
   
END.

EMPTY TEMP-TABLE ttStatus.

IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1.

fELog("FUNCRUNEXEC_RUN" +
         (IF lcHost > "" THEN "_" + lcHost ELSE ""),
      "handled" + STRING(liHandled)).

QUIT.

