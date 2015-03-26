/* ---------------------------------------------------------------------------
  MODULE .......: FuncRunExec_notify.p
  FUNCTION .....: send notifications of a function execution
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 27.04.10
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{email.i}
{fmakesms.i}

DEF INPUT PARAMETER iiFRExecID    AS INT  NO-UNDO.
DEF INPUT PARAMETER icMessage     AS CHAR NO-UNDO.

DEF VAR lcMailAddress AS CHAR NO-UNDO.
DEF VAR lcSMSNumber   AS CHAR NO-UNDO.
DEF VAR lcSubject     AS CHAR NO-UNDO.

DEF STREAM sMail.

FUNCTION fErrorLog RETURNS LOGIC
   (iiFRExecID AS INT,
    icError AS CHAR):
   
   DO TRANS:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = gcBrand
             ErrorLog.ActionID  = "FRNOTIFY" + STRING(iiFRExecID)
             ErrorLog.TableName = "FuncRunExec"
             ErrorLog.KeyValue  = STRING(iiFRExecID)
             ErrorLog.ErrorMsg  = icError
             ErrorLog.UserCode  = katun.
             ErrorLog.ActionTS  = fMakeTS().
   END.
   
END FUNCTION.


/********** Main start ***********/

RUN pInitialize(iiFRExecID,
                OUTPUT lcMailAddress,
                OUTPUT lcSMSNumber,
                OUTPUT lcSubject).
IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.

IF lcMailAddress > "" THEN 
   RUN pSendMail(iiFRExecID,
                 lcMailAddress,
                 lcSubject,
                 icMessage).

IF lcSMSNumber > "" THEN 
   RUN pSendSMS(iiFRExecID,
                lcSMSNumber,
                lcSubject  + ": " + icMessage).

RETURN RETURN-VALUE.

/****** MAIN end  ******************/


PROCEDURE pInitialize:

   DEF INPUT  PARAMETER iiFRExecID  AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER ocMailAddress AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER ocSMSNumber   AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER ocSubject     AS CHAR NO-UNDO.

   DEF VAR lcError   AS CHAR NO-UNDO. 

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
   
   ASSIGN 
      ocMailAddress = FuncRunConfig.NotifyMail
      ocSMSNumber   = FuncRunConfig.NotifySMS
      ocSubject     = FuncRunConfig.ConfName.
      
   RETURN "".   
      
END PROCEDURE.
 
PROCEDURE pSendMail:

   DEF INPUT  PARAMETER iiFRExecID    AS INT  NO-UNDO.
   DEF INPUT  PARAMETER icMailAddress AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icSubject     AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icMessage     AS CHAR NO-UNDO.

   DEF VAR lcMailFile AS CHAR NO-UNDO.
   DEF VAR lcError    AS CHAR NO-UNDO.
   
   ASSIGN 
      xMailAddr  = icMailAddress
      xMailSubj  = "'" + icSubject + "'"
      xMailFrom  = "FunctionRunQueue@qvantel.com"
      lcMailFile = "/tmp/functionexec_" + STRING(iiFRExecID) + ".txt".

   OUTPUT STREAM sMail TO VALUE(lcMailFile).
   PUT STREAM sMail UNFORMATTED
      icMessage SKIP.
   OUTPUT STREAM sMail CLOSE.
   
   IF NOT SendMail(lcMailFile,"") THEN DO:
      lcError = "ERROR:Notify mail send failed; " + icMessage.
      fErrorLog(iiFRExecID,lcError).
   END.
   
   OS-DELETE VALUE(lcMailFile).
      
   RETURN lcError.

END PROCEDURE.

PROCEDURE pSendSMS:

   DEF INPUT  PARAMETER iiFRExecID  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER icSMSNumber AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icMessage   AS CHAR NO-UNDO.

   DEF VAR liNumber  AS INT  NO-UNDO.
   DEF VAR ldCurrent AS DEC  NO-UNDO.
   
   ldCurrent = fMakeTS().
   
   DO liNumber = 1 TO NUM-ENTRIES(icSMSNumber):
   
      fMakeSchedSMS(1,
                    ENTRY(liNumber,icSMSNumber),
                    9,
                    icMessage,
                    ldCurrent).
   END.
   
   RETURN "".

END PROCEDURE.


