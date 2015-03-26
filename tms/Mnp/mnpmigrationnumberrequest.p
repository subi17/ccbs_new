/* ----------------------------------------------------------------------
  MODULE .......: mnpmigrationnumberrequest.p
  TASK .........: Create new mnp migration number process
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 10/2009
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{commali.i}
{timestamp.i}
{mnpmessages.i}
{mnp.i}

DEFINE INPUT PARAMETER pcPortRequest AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER TABLE FOR ttMigrationNumberRequest.

DEF BUFFER MNPProcessFather FOR MNPProcess.
FIND MNPProcessFather WHERE
   MNPProcessFather.PortRequest = pcPortRequest AND
   MNPProcessFather.MNPType = 5 NO-LOCK NO-ERROR.

IF NOT AVAIL MNPProcessFather THEN DO:
   RETURN "ERROR:MNPProcess not found".
END.

/* child processes are connected using formrequest
 (may not be a good idea, but it's enough for testing) */
CREATE MNPProcess.
ASSIGN 
   MNPProcess.CreatedTS   = fMakeTS()
   MNPProcess.MNPSeq      = next-value(m2mrequest)
   MNPProcess.FormRequest = MNPProcessFather.FormRequest
   MNPProcess.OrderId     = 0
   MNPProcess.StatusCode  = 0
   MNPProcess.Brand       = gcBrand
   MNPProcess.MNPType     = 6
   MNPProcess.UserCode    = katun
   MNPProcess.UpdateTS    = MNPProcess.CreatedTS.

IF fSendMigrationNumberRequest(INPUT TABLE ttMigrationNumberRequest BY-REFERENCE) THEN RETURN "".
ELSE RETURN "ERROR:MNPProcess not found".
