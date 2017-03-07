/* ----------------------------------------------------------------------
MODULE .......: mnpfileinresp.p
TASK .........: handles mnp contingency nodo central responses
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 06.10.09
Version ......: xfera
----------------------------------------------------------------------- */
{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Mnp/mnpcontingency.i}
{Func/log.i}
{xmlrpc/xmlrpc_client.i}

DEFINE VARIABLE lcError AS CHAR NO-UNDO. 

/* files and dirs */
DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcXMLFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcRootDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIncDir  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIncProcDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcUnix AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTmpDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcLogDir AS CHARACTER NO-UNDO.

ASSIGN
   lcRootDir = fCParam("MNP","ContingencyClientInDir") 
   lcIncDir = lcRootDir + "/incoming/". 
   lcIncProcDir = lcRootDir + "/processed/".
   lcTmpDir = lcRootDir + "/tmp/". 
   lcLogDir = fCParam("MNP","MPNLogDir").

DEF STREAM sFile.
DEF STREAM sXMLFile.

DEFINE VARIABLE lcToken AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFileToken AS CHARACTER NO-UNDO. 

fSetLogFileName(lcLogDir + "mnpfileclientresp.log").

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
    
   lcInputFile = lcIncDir + lcFileName.
   
   lcUnix = "cd " + lcIncDir + ";tar -xzPf " + lcInputFile + " -C " + lcTmpDir. 
   UNIX SILENT VALUE(lcUnix). 
    
   IF SEARCH(lcInputFile) EQ ? THEN NEXT.
   
   INPUT STREAM sXMLFile THROUGH VALUE("ls -1tr " + lcTmpDir).

   REPEAT TRANS:
      IMPORT STREAM sXMLFile UNFORMATTED lcXMLFileName.

      lcFileToken = SUBSTRING(lcFileName,1,INDEX(lcFileName,"__") - 1).
      
      FIND ttContingency WHERE 
           ttContingency.fileToken = lcFileToken NO-LOCK NO-ERROR.
      IF NOT AVAIL ttContingency THEN DO:
         fLogError("Unknown file " + lcFileToken). 
         NEXT.
      END.
      
      lcToken = SUBSTRING(lcXMLFileName,1,INDEX(lcXMLFileName,".xml") - 1).
      
      /* find mnpprocess */
      CASE ttContingency.xmlToken:
         WHEN "portRequest" THEN DO:
            FIND FIRST MNPProcess WHERE
                    MNPProcess.PortRequest = lcToken NO-LOCK NO-ERROR.
          
         END.
         WHEN "formRequest" THEN DO:
            FIND MNPProcess WHERE
                 MNPProcess.FormRequest = lcToken NO-LOCK NO-ERROR.
         END.
         WHEN "MSISDN" THEN DO:
            FOR EACH MNPSub WHERE
               MNPSub.CLI = lcToken NO-LOCK:
               FIND FIRST MNPProcess WHERE
                 MNPProcess.MNPSeq = MNPSub.MNPSeq AND
                 MNPProcess.MNPType = {&MNP_TYPE_TERMINATION} AND
                 MNPProcess.StatusCode = {&MNP_ST_NEW} NO-LOCK NO-ERROR.
               IF AVAIL MNPProcess THEN LEAVE.
            END.
         END.
         WHEN "" THEN DO:
            FIND mnpprocess where 
               mnpprocess.mnpseq = {&MNP_PROCESS_QUERY} NO-LOCK NO-ERROR.
         END.
         OTHERWISE DO:
            fLogError("Unsupported xml token " + ttContingency.xmlToken + 
             ":" + lcXMLFileName).
            NEXT.
         END.
      END.
     
      IF NOT AVAIL MNPProcess THEN DO:
         fLogError("MNPProcess not found:" + lcInputFile + " "  + lcXmlFileName).
         NEXT.
      END.

      FIND FIRST MNPOperation WHERE
           MNPOperation.MNPSeq = MNPProcess.MNPSeq AND
           MNPOperation.MessageType = ttContingency.operation AND
           MNPOperation.StatusCode = {&MNP_MSG_SENT_TO_FILE} EXCLUSIVE-LOCK NO-ERROR.
      
      IF NOT AVAIL MNPOperation THEN DO:
         fLogError("MNPMessage not found:" + lcInputFile + " " + lcXmlFileName + " " + ttContingency.operation).
         NEXT.
      END.
      ELSE fLogBasic("OK: " + MNPProcess.PortRequest + " " + MNPOperation.MessageType).

      COPY-LOB FROM FILE lcTmpDir + lcXmlFileName TO MNPOperation.XMLResponse.

      MNPOperation.StatusCode = {&MNP_MSG_WAITING_RESPONSE_HANDLE}. /* waiting for handling */
   END.

   UNIX SILENT VALUE("rm -f " + lcTmpDir + "*.xml").
   UNIX SILENT VALUE("mv " + lcInputFile + " " + lcIncProcDir).

END.
