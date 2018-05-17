/* ----------------------------------------------------------------------
  MODULE .......: mnpsmsreject.p
  TASK .........: Dumps MNP IN cancellation details via sms. YDR-2812
  APPLICATION ..: TMS
  AUTHOR .......: Vandana 
  CREATED ......: 14-05-2018
  Version ......: Yoigo
----------------------------------------------------------------------- */

/* ********************** Include Definitions  ************************* */
{Syst/commali.i}
{Syst/tmsconst.i}
{Syst/dumpfile_run.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/smsmessage.i}

/* **************** Input/Output Parameter Definitions ***************** */

DEFINE INPUT  PARAMETER iiDumpID      AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER icFile        AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER icDumpMode    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER idLastDump    AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER icEventSource AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER icEventFields AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oiEvents      AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER olInterrupted AS LOGICAL    NO-UNDO.

/* ********************  Stream/Variable Definitions  ************************** */
DEFINE STREAM sdump.

DEFINE VARIABLE ldaDate       AS DATE      NO-UNDO. 
DEFINE VARIABLE ldeFrom       AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE ldeTo         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lcDelimiter   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSMSConfirm  AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldaSMSDeliStamp AS DATE  NO-UNDO.
DEFINE VARIABLE lcStatusReason  AS CHARACTER NO-UNDO INITIAL "AREC EXIST,AREC ENUME,RECH_BNUME,RECH_ICCID,RECH_IDENT".

/* ***************************  Main Block  *************************** */

FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN lcDelimiter = DumpFile.DumpDelimiter.

ASSIGN
    ldaDate = TODAY - 1
    ldeFrom = Func.Common:mHMS2TS(ldaDate, "00:00:00")
    ldeTo   = Func.Common:mHMS2TS(ldaDate,"23:59:59").

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
    WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
    TITLE " Collecting " FRAME fColl.

OUTPUT STREAM sdump TO value (icFile).

ERROR_LOOP:
FOR EACH MNPProcess NO-LOCK 
    WHERE MNPProcess.Brand       =  Syst.Var:gcBrand 
      AND MNPProcess.MNPType     =  {&MNP_TYPE_IN}    
      AND MNPProcess.UpdateTS    >= ldeFrom  
      AND MNPProcess.UpdateTS    <= ldeTo 
      AND MNPProcess.StatusCode  =  {&MNP_ST_AREC}:
    IF LOOKUP(MNPProcess.StatusReason,lcStatusReason) > 0 THEN 
    DO:    
       FIND FIRST MNPSub NO-LOCK 
            WHERE MNPSub.MNPSeq = MNPProcess.MNPSeq NO-ERROR.
       IF NOT AVAILABLE MNPSub THEN NEXT ERROR_LOOP.
       
       FIND FIRST MNPDetails NO-LOCK 
            WHERE MNPDetails.MNPSeq = MNPProcess.MNPSeq NO-ERROR.
       IF NOT AVAILABLE MNPDetails THEN NEXT ERROR_LOOP.
       
       ASSIGN lcSMSConfirm     =  "NOK".
                    
       FIND FIRST CallAlarm NO-LOCK
            WHERE CallAlarm.Brand      =  Syst.Var:gcBrand
              AND CallAlarm.Cli        =  MNPSub.Cli 
              AND CallAlarm.ActStamp   >= MNPProcess.UpdateTS NO-ERROR.
       IF AVAILABLE CallAlarm THEN 
       DO:
          IF CallAlarm.DeliStat   =  {&SMS_DELISTATUS_RECEIVED} THEN
            ASSIGN lcSMSConfirm     =  "OK".
          ELSE
            ASSIGN lcSMSConfirm     =  "NOK".
       END.
                                                   
       PUT STREAM sdump UNFORMATTED 
           Func.Common:mTS2HMS(MNPProcess.CreatedTS) lcDelimiter
           MNPSub.Cli lcDelimiter
           MNPDetails.DonorCode lcDelimiter
           MNPDetails.ReceptorCode lcDelimiter
           MNPProcess.StatusReason lcDelimiter
           Func.Common:mTS2HMS(MNPProcess.MNPUpdateTS) lcDelimiter
           IF AVAILABLE CallAlarm THEN Func.Common:mTS2HMS(CallAlarm.ActStamp) ELSE "" lcDelimiter
           lcSMSConfirm SKIP.
                
       oiEvents = oiEvents + 1.
       IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN 
       DO:
           PAUSE 0.
           DISP oiEvents WITH FRAME fColl.
       END.        
    END.               
END.

OUTPUT STREAM sdump close.
HIDE FRAME fColl NO-PAUSE.
