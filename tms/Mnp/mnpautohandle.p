/* ----------------------------------------------------------------------
  MODULE .......: mnpautohandle.p 
  TASK .........: Create MNP OUT confirmation/rejection messages. Should be run
                  30 mins after mnp time slot start time (8:30 and 14:30)
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 03.11.09
  Version ......: yoigo
----------------------------------------------------------------------- */
{commpaa.i}
katun = "MNP".
gcBrand = "1".

{mnp.i}
{mnpmessages.i}
{tmsconst.i}
{log.i}
{timestamp.i}
{cparam2.i}

DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llOk AS LOGICAL NO-UNDO. 
DEFINE VARIABLE liSkipped AS INTEGER NO-UNDO. 
DEFINE VARIABLE liFailed AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcLogDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE liHandled AS INTEGER NO-UNDO. 
DEF VAR ldeNow AS DEC NO-UNDO. 

lcLogDir = fCParam("MNP","MNPLogDir").
fSetLogFileName(lcLogDir + "mnpautohandle.log").

/* 6 hours ahead */
ldeNow = fOffSetTS(6). 

HANDLE_LOOP:
FOR EACH MNPProcess EXCLUSIVE-LOCK WHERE
         MNPProcess.Brand = gcBrand AND
         MNPProcess.MNPType = {&MNP_TYPE_OUT} AND
         MNPProcess.StatusCode = {&MNP_ST_ASOL} AND
         MNPProcess.StateFlag NE {&MNP_STATEFLAG_NOT_ANALYSED}:

   FIND FIRST mnpdetails WHERE
              mnpdetails.mnpseq = MNPProcess.mnpseq NO-LOCK NO-ERROR.
   IF AVAIL mnpdetails AND
            mnpdetails.statuslimitts > ldeNow THEN NEXT HANDLE_LOOP.
    
   /* if confirm or rejection message is already created, do not create it again  */
   FOR EACH MNPOperation WHERE
      MNPOperation.MNPSeq = MNPProcess.MNPSeq AND
      LOOKUP(MNPOperation.MessageType,
         "rechazarSolicitudAltaPortabilidadMovil,confirmarSolicitudAltaPortabilidadMovil") > 0 NO-LOCK:
      liSkipped = liSkipped + 1.
      NEXT HANDLE_LOOP.
   END.
   
   ASSIGN
      llOk = FALSE
      lcError = "".
   
   IF MNPProcess.StateFlag = {&MNP_STATEFLAG_CONFIRM_PROPOSAL} OR 
      MNPProcess.StateFlag = {&MNP_STATEFLAG_CONFIRM} THEN DO:
      llOk = fSendConfirmation(MNPProcess.PortRequest).
   END.
   ELSE IF MNPProcess.StateFlag = {&MNP_STATEFLAG_REJECT_PROPOSAL} OR
           MNPProcess.StateFlag = {&MNP_STATEFLAG_REJECT} THEN DO:
      llOk = fSendRejection(
         MNPProcess.PortRequest,
         MNPProcess.StatusReason,
         OUTPUT lcError).
   END.
   
   IF llOk THEN DO:
      fLogBasic("Handled (" + STRING(MNPProcess.StateFlag) + ") " + MNPProcess.PortRequest).
      MNPProcess.StateFlag = 0.
      liHandled = liHandled + 1.
   END. 
   ELSE DO:
      fLogError("Failed (" + STRING(MNPProcess.StateFlag) 
      + ") " + MNPProcess.PortRequest + " " + lcError).
      liFailed = liFailed + 1.
   END.

END.

fLogBasic("Total handled: " + STRING(liHandled) + 
          ", skipped: " + STRING(liSkipped) + 
          ", failed: " + STRING(liFailed)).
