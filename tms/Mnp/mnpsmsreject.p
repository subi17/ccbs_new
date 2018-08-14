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

DEFINE VARIABLE lcDelimiter      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStatusReason   AS CHARACTER NO-UNDO INITIAL "AREC EXIST,AREC ENUME,RECH_BNUME,RECH_ICCID,RECH_IDENT".
DEFINE VARIABLE ldtActStamp      AS DECIMAL   NO-UNDO.

/* ***************************  Main Block  *************************** */

ldtActStamp = Func.Common:mDate2TS( TODAY - 30 ).

FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN lcDelimiter = DumpFile.DumpDelimiter.

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
    WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
    TITLE " Collecting " FRAME fColl.

OUTPUT STREAM sdump TO value (icFile).

ERROR_LOOP:
FOR EACH CallAlarm NO-LOCK
   WHERE CallAlarm.Brand      = Syst.Var:gcBrand
     AND CallAlarm.ActStamp   >= ldtActStamp     /* Checking for last 30 day ActStamp to follow index */
     AND CallAlarm.DeliStat   = {&CA_DELISTAT_SENT}
     AND CallAlarm.CreditType = {&SMSTYPE_MNP}
     AND CallAlarm.Delitype   = 1
     AND CallAlarm.DeliStamp  > idLastDump
     USE-INDEX ActStamp
     :
   FOR EACH MnpSub NO-LOCK
      WHERE MnpSub.CLI = CallAlarm.CLI,
       EACH MnpProcess NO-LOCK
      WHERE MNPProcess.Brand       = Syst.Var:gcBrand
        AND MnpProcess.MnpSeq      = MnpSub.MnpSeq
        AND MNPProcess.MNPType     = {&MNP_TYPE_IN}
        AND (MNPProcess.StatusCode = {&MNP_ST_AREC}
         OR  MNPProcess.StatusCode = {&MNP_ST_AREC_CLOSED})
        AND MNPProcess.FormRequest    = CallAlarm.DeliPara
        AND LOOKUP(MnpProcess.StatusReason, lcStatusReason) > 0
         BY MNPProcess.UpdateTS DESCENDING:
            
      FIND FIRST MNPDetails NO-LOCK 
           WHERE MNPDetails.MNPSeq = MNPProcess.MNPSeq NO-ERROR.
       
      PUT STREAM sdump UNFORMATTED
         MNPSub.CLI                                                  lcDelimiter
         IF AVAILABLE MNPDetails THEN MNPDetails.DonorCode ELSE ""   lcDelimiter
         MNPProcess.PortRequest                                      lcDelimiter
         MNPProcess.StatusReason                                     lcDelimiter
         Func.Common:mTS2HMS(CallAlarm.DeliStamp)
         SKIP.
                                
      oiEvents = oiEvents + 1.
      
      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN
      DO:
         PAUSE 0.
         DISP oiEvents WITH FRAME fColl.
      END.
      LEAVE.
   END.                   
END.

OUTPUT STREAM sdump CLOSE.
HIDE FRAME fColl NO-PAUSE.