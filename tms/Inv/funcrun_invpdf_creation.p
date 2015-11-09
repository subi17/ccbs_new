/* ----------------------------------------------------------------------
  Module .......: Inv/funcrun_invpdf_creation.p
  Task .........: sends message to revolver throught ActiveMQ through funcrun process
  Application ..: TMS
  Author .......: Subhash Sanjeevi
  Created ......: 11.10.15
  Version ......: Yoigo
---------------------------------------------------------------------- */ 
{commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun   = "Cron".

{cparam2.i}
{replog_reader.i}
{host.i}
{ftransdir.i}

DEFINE INPUT PARAMETER liFRExecID AS INTEGER NO-UNDO. 

DEFINE VARIABLE lcConfDir       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInputFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutputFile    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFeedBackQueue AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFeedBackID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcType          AS CHARACTER NO-UNDO.
DEFINE VARIABLE llgHandled      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcFuncRunInpDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcess       AS CHARACTER NO-UNDO. 

/* These three variables has to be logical, but due to 
   Activemq message constraints it has been defined as char */
DEFINE VARIABLE llgRecursive    AS CHARACTER NO-UNDO.
DEFINE VARIABLE llgMultiFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE llgFeedBack     AS CHARACTER NO-UNDO.

ASSIGN 
   lcInputFile     = fCParam("FuncrunPDF","InputFile") 
   lcOutputFile    = fCParam("FuncrunPDF","OutputFile")
   lcFuncRunInpDir = fCParamC("FRTestRunDir")
   llgRecursive    = "false" 
   llgMultiFile    = "false" 
   llgFeedBack     = "false" 
   lcFeedBackQueue = ""
   lcFeedBackID    = ""
   lcType          = "invoice"
   lcProcess       = "funcrun".

 /* Re-calculating Billrun id */
FIND FIRST FuncRunExec WHERE
           FuncRunExec.Brand    EQ "1"        AND
           FuncRunExec.FRExecID EQ liFRExecID NO-LOCK NO-ERROR.

IF AVAILABLE FuncRunExec THEN 
   ASSIGN 
      lcFeedBackID    = "frq_" + STRING(FuncRunExec.FRQScheduleID)  
      lcInputFile     = lcInputFile  + lcFeedBackID  
      lcFuncRunInpDir = lcFuncRunInpDir + "/" + lcFeedBackID. 

/* Copy Billrunid folder to /mnt/xmlstore/test/ location
   as this is been accessed by revolver and provide folder write permissions */

UNIX SILENT VALUE("cp -r " + lcFuncRunInpDir + " " + lcOutputFile + " >/dev/null 2>&1"). 

lcOutputFile = lcOutputFile + lcFeedBackID.

FILE-INFO:FILE-NAME = lcOutputFile.

IF FILE-INFO:FULL-PATHNAME NE ? THEN 
DO:
   UNIX SILENT VALUE("chmod go+w " + lcOutputFile). 

   RUN pInitialize(INPUT "revolver").

   IF RETURN-VALUE > "" THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR").
         RETURN RETURN-VALUE.
   END.
      
   /* Call ActiveMQ Publisher class */
   lMsgPublisher = NEW Gwy.MqPublisher(lcHost,liPort,
                                       liTimeOut,"revolver",
                                       lcUserName,lcPassword).
       
   IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
         LOG-MANAGER:WRITE-MESSAGE("ActiveMQ Publisher handle not found","ERROR").
   END.

   lcMessage = "~{" + "~"input_file~""       + "~:" + "~"" + lcInputFile  + "~"" + "," +
                      "~"output_file_name~"" + "~:" + "~"" + lcOutputFile + "~"" + "," +
                      "~"recursive~""        + "~:" +        llgRecursive        + "," +
                      "~"multi_file~""       + "~:" +        llgMultiFile        + "," +
                      "~"feedback~""         + "~:" +        llgFeedback         + "," +
                      "~"feedback_id~""      + "~:" + "~"" + lcFeedbackID + "~"" + "," +
                      "~"type~""             + "~:" + "~"" + lcType       + "~"" + "," +
                      "~"process~""          + "~:" + "~"" + lcProcess    + "~"" + "~}".

   IF lMsgPublisher:send_message(lcMessage) THEN
      llgHandled = TRUE.
   ELSE DO:
      llgHandled = FALSE.
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
         LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
   END.

   RUN pFinalize(INPUT "").
END.

