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

DEFINE VARIABLE lcDelimiter   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStatusReason  AS CHARACTER NO-UNDO INITIAL "AREC EXIST,AREC ENUME,RECH_BNUME,RECH_ICCID,RECH_IDENT".

/* ***************************  Main Block  *************************** */

FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN lcDelimiter = DumpFile.DumpDelimiter.

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
    WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
    TITLE " Collecting " FRAME fColl.

OUTPUT STREAM sdump TO value (icFile).

ERROR_LOOP:
FOR EACH MNPProcess NO-LOCK 
    WHERE MNPProcess.Brand       =  Syst.Var:gcBrand 
      AND MNPProcess.MNPType     =  {&MNP_TYPE_IN} 
      AND MNPProcess.StatusCode  =  {&MNP_ST_AREC}   
      AND MNPProcess.UpdateTS    >  idLastDump:
    IF LOOKUP(MNPProcess.StatusReason,lcStatusReason) = 0 THEN NEXT ERROR_LOOP.
            
       FIND FIRST MNPDetails NO-LOCK 
            WHERE MNPDetails.MNPSeq = MNPProcess.MNPSeq NO-ERROR.
       IF NOT AVAILABLE MNPDetails THEN NEXT ERROR_LOOP.     
       
       FOR EACH MNPSub NO-LOCK 
            WHERE MNPSub.MNPSeq = MNPProcess.MNPSeq:
                                                                        
            PUT STREAM sdump UNFORMATTED 
                Func.Common:mTS2HMS(MNPProcess.PortingTime) lcDelimiter
                MNPSub.Cli lcDelimiter
                MNPDetails.DonorCode lcDelimiter
                MNPProcess.PortRequest lcDelimiter
                MNPProcess.StatusReason lcDelimiter
                Func.Common:mTS2HMS(MNPProcess.UpdateTS) SKIP.
                                
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