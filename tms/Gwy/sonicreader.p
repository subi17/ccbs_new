
/* ----------------------------------------------------------------------
  MODULE .......: sonicreader.p
  TASK .........: Reads messages from Sonic MQ queue.
                  Replaceable logic for message/error handling
                  Logger Framework
  APPLICATION ..: TMS
  AUTHOR .......: mikko 
  CREATED ......: 29.10.07
  CHANGED ......:
  Version ......: General TMS/Sonic MQ/Progress 10.1B
   
  Note..........: Still in development stage
----------------------------------------------------------------------- */

{commpaa.i}
{date.i}
{log.i}
fSetLogFileName("/home/mikko/sonicreader.log").
fClearLog().
fSetLogEntryTypes(fGetValidLogEntryTypes()).
fSetGlobalLoggingLevel(4).

gcBrand = "1".

DEFINE STREAM sLog.

/* global variables in this module */
DEFINE VARIABLE glConnected AS LOGICAL NO-UNDO INIT FALSE. 
DEFINE VARIABLE giMessagesReceived AS INTEGER NO-UNDO INIT 0. 
DEFINE VARIABLE gdeLastMessageTime AS DECIMAL NO-UNDO.
DEFINE VARIABLE giErrorCount AS INTEGER NO-UNDO. 
DEFINE VARIABLE gcLastErrorMessage AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gdeLastErrorTime AS DECIMAL NO-UNDO. 
DEFINE VARIABLE gcLastMessage AS CHARACTER NO-UNDO INIT "". 
DEFINE VARIABLE gcErrorLogFile AS CHARACTER NO-UNDO. 

DEFINE VARIABLE liMsg AS INTEGER NO-UNDO. 

/** is the reader active or should it be stopped? */
DEFINE VARIABLE glActive AS LOGICAL NO-UNDO INIT YES. 

/** shall we put info to screen? */
DEFINE VARIABLE glDebug AS LOGICAL NO-UNDO. 
glDebug = TRUE. /* TRUE. */

/* shall we write errors to log file? */
DEFINE VARIABLE glLogErrors AS LOGICAL NO-UNDO. 
glLogErrors = FALSE.
DEFINE VARIABLE lhPTPSession AS HANDLE.
DEFINE VARIABLE lhConsumer AS HANDLE.
DEFINE VARIABLE lhErrorConsumer AS HANDLE.
DEFINE VARIABLE lhMsgOut AS HANDLE.
DEFINE VARIABLE lhMsgIn AS HANDLE.

/* Local variables for the main program */
DEFINE VARIABLE lcSessionParams AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBrokerURL AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcUsername AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPasswd AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcQueueIn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcQueueOut AS CHARACTER NO-UNDO.
DEFINE VARIABLE liTimeout AS INTEGER NO-UNDO INIT 5.
DEFINE VARIABLE liWaitKeyPress AS INTEGER NO-UNDO INIT 5. 
DEFINE VARIABLE liWaitLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE liWaitBeforeReconnect AS INTEGER NO-UNDO INIT 5.

ASSIGN
   lcSessionParams = "-H localhost -S 5163"
   lcBrokerURL     = "localhost:15000"
   lcUsername      = ?
   lcPasswd        = ?
   lcQueueIn       = "xfera.netplus_TMS".





/*---------------------------------------------------------------*/
/* Functions                                                     */
/*---------------------------------------------------------------*/


/** This function is called after consuming a message.
 *   see Open Edge development: Messaging and ESB JMS API Reference 
 *   waitForMessages procedure 
 * 
 * This must return true, otherwise receiving messages will end
 * @returns boolean True or False, shall we continue receiving messages
 * @author mikko
 */
FUNCTION fStopCondition RETURNS LOGICAL:
   RETURN glActive.
END FUNCTION. 



FUNCTION fLogErrorStatus RETURNS LOGICAL
(icAction AS CHAR):
   fLog(icAction,"USER").
   /* was there an error */
   IF NOT ERROR-STATUS:ERROR THEN RETURN FALSE.
   
   IF icAction = ? OR icAction = "" THEN icAction = THIS-PROCEDURE:NAME.
   ELSE icAction = THIS-PROCEDURE:NAME + "(" + icAction + ")".
   giErrorCount = giErrorCount + 1.
   gcLastErrorMessage = ERROR-STATUS:GET-MESSAGE(ERROR-STATUS:NUM-MESSAGES).
   gdeLastErrorTime = fMakeTS().
   IF gcLastErrorMessage = ? OR gcLastErrorMessage = "" THEN 
      gcLastErrorMessage = "Undefined Runtime Error".
      
   /* INFO TO SCREEN may be disabled by setting glDebug flag FALSE */
   IF glDebug AND gdeLastErrorTime > 0 THEN DO:
      PUT SCREEN ROW 12 "Errors....: " + STRING(giErrorCount). 
      PUT SCREEN ROW 13 "Last Error: " + SUBSTRIN(gcLastErrorMessage,1,67). 
      PUT SCREEN ROW 14 "Lass ErrTS: " + fTS2C(gdeLastErrorTime).    
   END.
  
   /* write errors to logfile */
   IF glLogErrors THEN DO:
      DEFINE VARIABLE liLoop AS INTEGER NO-UNDO. 
      fLog(gcLastErrorMessage,"ErrorHandler").
      DO liLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
         fLog(ERROR-STATUS:GET-MESSAGE(liLoop),"ErrorHandler").
      END.
      
   END.

   RETURN TRUE.
END FUNCTION.

FUNCTION fStartSession RETURNS LOGICAL
(ihPTPSession AS HANDLE):
   IF glConnected THEN RETURN TRUE.

   /* set broker url */
   RUN setBrokerURL IN ihPTPSession (lcBrokerURL) NO-ERROR. 
   fLogErrorStatus("setBrokerURL").

   /* username and password */
   RUN setUser      IN ihPTPSession (lcUsername) NO-ERROR.
   fLogErrorStatus("setUser").
   RUN setPassword  IN ihPTPSession (lcPasswd) NO-ERROR.
   fLogErrorStatus("setPassword").

   /* ping interval to Broker */
   RUN setPingInterval in ihPTPSession(40) NO-ERROR.
   fLogErrorStatus("setPingInterval").

   /* supress error messages from session */
   RUN setNoErrorDisplay IN ihPTPSession(TRUE) NO-ERROR.
   fLogErrorStatus("setNoErrorDisplay").

   /* begin session */
   RUN beginSession IN ihPTPSession NO-ERROR.
   IF fLogErrorStatus("beginSession") THEN RETURN FALSE.

   /* create error consumer */
   IF VALID-HANDLE(lhErrorConsumer) AND lhErrorConsumer NE ? THEN DO:
      RUN getSession IN lhErrorConsumer NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:
         MESSAGE "delete old error consumer" VIEW-AS ALERT-BOX.
         RUN deleteConsumer IN lhErrorConsumer.
      END.
   END.
   
   RUN createMessageConsumer IN ihPTPSession 
      (THIS-PROCEDURE, "pErrorHandler", OUTPUT lhErrorConsumer) NO-ERROR.
   fLogErrorStatus("createErrorConsumer").

   /* enable error handler */
   RUN setErrorHandler IN ihPTPSession(lhErrorConsumer) NO-ERROR.
   fLogErrorStatus("setErrorHandler").

   /* create consumer */
   IF VALID-HANDLE(lhConsumer) AND lhConsumer NE ? THEN DO:
      RUN getSession IN lhConsumer.
      IF NOT ERROR-STATUS:ERROR THEN DO:
         MESSAGE "deletee old messageconsumer" VIEW-AS ALERT-BOX.
         RUN deleteConsumer IN lhConsumer.
      END.
   END.
   RUN createMessageConsumer IN ihPTPSession 
      (THIS-PROCEDURE, "pMessageReceived", OUTPUT lhConsumer) NO-ERROR.
   fLogErrorStatus("createMessageConsumer").

   /* set queue from where to read */
   RUN receiveFromQueue IN ihPTPSession 
       (lcQueueIN, ?, lhConsumer) NO-ERROR.
   fLogErrorStatus("receiveFromQueue").

   /* start reading */
   RUN startReceiveMessages IN ihPTPSession NO-ERROR.
   fLogErrorStatus("startReceiveMessages").
   
   glConnected = TRUE.
   RETURN TRUE.

END FUNCTION. 


/*---------------------------------------------------------------*/
/* Main program                                                  */
/*---------------------------------------------------------------*/


/*  Creates a session object.  */
RUN jms/ptpsession.p PERSISTENT SET lhPTPSession(lcSessionParams).

PUT SCREEN ROW 1 "Aina XML Interface Reader (Online since " +
   fTS2HMS(fMakeTS()) + ")".

/* Main loop */
REPEAT WHILE glActive ON ENDKEY UNDO, LEAVE:

   IF NOT glConnected THEN DO:
      DO liWaitLoop = liWaitBeforeReconnect TO 1 BY -1:   
         PUT SCREEN ROW 23 "RECONNECT IN " +
            STRING(liWaitLoop) + " seconds. " +
            " F8 to QUIT - F1 reconnect now". 
         READKEY PAUSE 1.
         IF KEYLABEL(LASTKEY) = "f8" THEN DO:
            glActive = FALSE.
            LEAVE.
         END.
         ELSE IF KEYLABEL(LASTKEY) = "f1" THEN LEAVE.
         ELSE IF LASTKEY > 0 THEN NEXT.
      END.
      IF glActive THEN DO:
         DEFINE VARIABLE ll AS LOGICAL NO-UNDO. 
         ll = fStartSession(lhPTPSession).
         IF NOT ll THEN NEXT.
      END.
      ELSE NEXT.
   END.
   
   /* Wait for messages */
   RUN waitForMessages IN lhPTPSession
      ("fStopCondition", THIS-PROCEDURE, liTimeout) NO-ERROR.

   liMsg = liMsg + 1.
   fLog("Received " + STRING(liMsg) + " messages","USER").
   
   /* Give user some time to quit reader */
   DO liWaitLoop = liWaitKeyPress TO 1 BY -1:   
      PUT SCREEN ROW 23 "HIT F8 NOW TO QUIT. Waiting for " +
         STRING(liWaitLoop) + " seconds...    ". 
      READKEY PAUSE 1.
      IF KEYLABEL(LASTKEY) = "f8" THEN DO:
         glActive = FALSE.
         LEAVE.
      END.
      ELSE IF LASTKEY > 0 THEN LEAVE.
   END.
END.

IF glActive THEN DO:
   MESSAGE "Shouldn't come here yet..." VIEW-AS ALERT-BOX.
END.
ELSE DO:
   MESSAGE "Interface reader shut down" VIEW-AS ALERT-BOX.
END.

/* clean up memory */
RUN deleteConsumer IN lhErrorConsumer NO-ERROR.
RUN deleteConsumer IN lhConsumer NO-ERROR.
RUN deleteSession IN lhPTPSession NO-ERROR.

/* end of main program */


/* ----------------------------------------------------------------*/
/* handler procedures below                                        */
/* ----------------------------------------------------------------*/

/** This procedure is called by normal message consumer */
PROCEDURE pMessageReceived:
   DEFINE INPUT PARAMETER ihMsg AS HANDLE.
   DEFINE INPUT PARAMETER ihConsumer AS HANDLE.
   DEFINE OUTPUT PARAMETER ihReply AS HANDLE.
   
   DEFINE VARIABLE liPartCount AS INTEGER NO-UNDO. 
   DEFINE VARIABLE i AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcContentType AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMessage AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lhPart AS HANDLE NO-UNDO. 
   
   
   fLog(DYNAMIC-FUNCTION('getContentType':U IN ihMsg,1),"msgH").

   liPartCount = DYNAMIC-FUNCTION('getPartCount':U IN ihMsg).
   
   DO i = 1 TO liPartCount:
      lcContentType = DYNAMIC-FUNCTION('getContentType':U IN ihMsg,i).
      IF lcContentType = "text/xml" THEN DO:
         DYNAMIC-FUNCTION('getTextPartByIndex':U IN ihMsg,INPUT i,OUTPUT lcMessage).
         fLog(lcMessage,"RecMSG").
      END.
   END.
   gdeLastMessageTime = fMakeTS().

   RUN deleteMessage IN ihMsg.

   /* INFO TO SCREEN, may be turned of with glDebug flag */
   IF glDebug AND gdeLastMessageTime > 0 THEN DO: 
      PUT SCREEN ROW 8  "Messages..: " + STRING(giMessagesReceived).
      PUT SCREEN ROW 9  "Last msg..: " + SUBSTRING(gcLastMessage,1,67).
      PUT SCREEN ROW 10 "Last MsgTS: " + fTS2C(gdeLastMessageTime).
   END.

END.

/** This procedure is called by error message consumer */
PROCEDURE pErrorHandler:
   DEFINE INPUT PARAMETER ihMsg AS HANDLE.
   DEFINE INPUT PARAMETER ihConsumer AS HANDLE.
   DEFINE OUTPUT PARAMETER ihReply AS HANDLE.

   DEFINE VARIABLE lcErrorCode AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcErrorText AS CHARACTER NO-UNDO.
   lcErrorCode = DYNAMIC-FUNCTION('getCharProperty':U IN ihMsg,"errorCode").
   lcErrorText = DYNAMIC-FUNCTION('getText':U IN ihMsg).
  
   fLog(lcErrorText + ":" + lcErrorCode,"ErrH").

   RUN deleteMessage IN ihMsg.
   IF lcErrorCode = "-5" THEN DO:
      glConnected = FALSE.
      fLog("Delete session!","ErrH").
      RUN deleteSession IN lhPTPSession.

   END.

END PROCEDURE.



