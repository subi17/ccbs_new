/* ----------------------------------------------------------------------
  MODULE .......: mnpfileclient.p 
  TASK .........: Create MNP contingency files for requests created in TMS 
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 05.10.2009
  Version ......: yoigo
----------------------------------------------------------------------- */
{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".

{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Func/date.i}
{Func/log.i}
{Mnp/mnpcontingency.i}

DEFINE VARIABLE lcSpoolDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcUnix AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liHandled AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcOutFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTime AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcLogFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcLogDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRootDir AS CHARACTER NO-UNDO. 

ASSIGN
   lcRootDir = fCParam("MNP","ContingencyClientOutDir") 
   lcSpoolDir = lcRootDir + "/spool/" 
   lcOutDir   = lcRootDir + "/outgoing/" 
   lcLogDir   = fCParam("MNP","MPNLogDir")
   lcTime = REPLACE(STRING(fMakeTS()),".","").

fSetLogFileName(lcLogDir + "contingency_client.log").

CONTINGENCY_LOOP:
FOR EACH ttContingency NO-LOCK:

   liHandled = 0.
   /* new (contingency) requests */
   FOR EACH MNPOperation EXCLUSIVE-LOCK WHERE
            MNPOperation.Sender = {&MNP_SENDER_TMS} AND
            MNPOperation.StatusCode = {&MNP_MSG_WAITING_SEND} AND
            MNPOperation.MessageType = ttContingency.operation
      i = 1 TO 500:

      FIND MNPProcess NO-LOCK WHERE
           MNPProcess.MNPSeq = MNPOperation.MNPSeq NO-ERROR.
      IF NOT AVAIL MNPProcess THEN NEXT.

      /* determine file id/name */
      CASE ttContingency.xmlToken:
         
         WHEN "formrequest" THEN lcFileName = MNPProcess.FormRequest.
         WHEN "portRequest" THEN lcFileName = MNPProcess.PortRequest.
         WHEN "MSISDN" THEN DO:
            FIND MNPSub WHERE 
                 MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK NO-ERROR.
            IF NOT AVAIL MNPSub THEN DO:
               fLogError("msisdn not found: " + MNPProcess.formrequest).
               NEXT CONTINGENCY_LOOP.
            end.
            lcFileName = MNPSub.CLI.
         END.
         WHEN "" THEN lcFileName = lcTime.
         OTHERWISE DO:
            fLogError("Unknown xmlToken: " + ttContingency.xmlToken).
            NEXT CONTINGENCY_LOOP.
         END.
      END.
      
      lcLogFile = lcSpoolDir + lcFileName + ".xml".
      COPY-LOB MNPOperation.XMLRequest TO FILE lcLogFile.

      liHandled = liHandled + 1.
      MNPOperation.StatusCode = {&MNP_MSG_SENT_TO_FILE}. /* sent to file */
   END.

   /* pack same type of operations together */
   IF liHandled > 0 THEN DO:
      lcOutFileName = ttContingency.fileToken + "__" + lcTime + ".tar.gz".
      lcUnix = "cd " + lcSpoolDir + ";tar --remove-files -czPf " + 
         lcSpoolDir + lcOutFileName + " *.xml". 
      UNIX SILENT VALUE(lcUnix). 
      fMove2TransDir(lcSpoolDir + lcOutFileName, "", lcOutDir). 
   END.
END.
