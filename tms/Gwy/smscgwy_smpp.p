/* ----------------------------------------------------------------------
  MODULE .......: smscgwy_smpp.p
  TASK .........: Client for SMSC platform using SMPP 3.4 protocol
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 16.04.13
  Version ......: Yoigo
----------------------------------------------------------------------- */

{commpaa.i} gcBrand = "1".
{cparam2.i}
{timestamp.i}
{Gwy/charset.i}
{lib/smpp/smpp_defs.i}
{lib/smpp/smpp_procs.i}

DEFINE VARIABLE lcUser      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPWD       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSystemType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcHost      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPort      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcURL       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lhSocket    AS HANDLE    NO-UNDO.
DEFINE VARIABLE liEtime     AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcServerResponseProcedure AS CHAR NO-UNDO. 

DEFINE VARIABLE liSMSLoop AS INTEGER NO-UNDO. 

DEF TEMP-TABLE ttPriority NO-UNDO
   FIELD CreditType AS INT
   FIELD Priority   AS INT
   FIELD ActInterval AS CHAR
   INDEX CreditType CreditType.

DEF VAR lcLogFile AS CHAR NO-UNDO INIT
   "/scratch/nagios/tms/callalarm/smpp_response.log".

DEFINE VARIABLE liMsgId AS INTEGER NO-UNDO.

FUNCTION fMessageId RETURNS INTEGER:

   liMsgId = liMsgId + 1.
   RETURN liMsgId.
   
END.
         
PROCEDURE pInitialize:

   DEF INPUT PARAM piNode AS INT NO-UNDO.

   CREATE SOCKET lhSocket.

   ASSIGN
      lcHost = (IF piNode MOD 2 EQ 1
                then "95.169.240.124" 
                else "95.169.240.125")  
      lcPort = "2802"
      lcUser = {&SMPP_SYSTEM_ID} 
      lcPWD  = {&SMPP_PASSWORD}
      lcSystemType = {&SMPP_SYSTEM_TYPE}
      lcLogFile = "/scratch/nagios/tms/callalarm/response_" + STRING(piNode) + 
                  ".log".
   lcURL = "-H " + lcHost + " -S " + lcPort.

END PROCEDURE.

PROCEDURE pFinalize:

   DELETE OBJECT lhSocket.

END PROCEDURE.

PROCEDURE pSMSCGWY:

   DEFINE INPUT PARAMETER pcReq AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcSMS AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcCLI AS CHARACTER NO-UNDO.

   DEFINE VARIABLE lcProcedure AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcResponse  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldeTS       AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE llRun       AS LOGICAL   NO-UNDO INIT TRUE.
   DEFINE VARIABLE llLoginFail AS LOGICAL   NO-UNDO INIT FALSE.
   DEFINE VARIABLE llLogoutFail AS LOGICAL  NO-UNDO INIT FALSE.
   DEFINE VARIABLE lcLogoutResp AS CHAR     NO-UNDO.

   ASSIGN
      liMsgId     = 0
      ldeTS       = fMakeTS()
      lcProcedure = "p" + pcReq.

   /* nothing to do ? */
   IF lcProcedure = "pCallAlarm" THEN
      llRun = CAN-FIND(FIRST CallAlarm NO-LOCK WHERE
                             CallAlarm.Brand     = "1" AND
                             CallAlarm.DeliStat  = 1   AND
                             CallAlarm.DeliType  = 1   AND
                             CallAlarm.ActStamp <= ldeTS).
   
   IF llRun THEN DO:
      
      RUN pLogIn(lcUser,lcPWD,lcSystemType,
                 fMessageId(),INPUT lcUrl, INPUT-OUTPUT lhSocket).

      lcResponse = RETURN-VALUE.

      IF lcResponse NE "OK" THEN 
           ASSIGN
                llRun       = FALSE
                llLoginFail = TRUE.
   
      IF llRun THEN DO:

         RUN VALUE(lcProcedure)
           (INPUT pcSMS, INPUT pcCLI).

         lcResponse = RETURN-VALUE.

         RUN pLogOut(INPUT-OUTPUT lhSocket, fMessageId()).

         lcLogoutResp = RETURN-VALUE.
         IF lcLogoutResp NE "OK" THEN llLogoutFail = TRUE.

      END.

   END.

   IF NOT llRun OR llLogoutFail THEN DO:
      
      OUTPUT TO VALUE(lcLogFile) APPEND.

      IF NOT llRun THEN DO:
         IF llLoginFail THEN
              PUT UNFORMATTED fTS2HMS(fMakeTS()) " Login failure: " 
               lcResponse  CHR(10).
         ELSE DO:
              PUT UNFORMATTED fTS2HMS(fMakeTS()) " Nothing to do"  CHR(10).
              lcResponse = "Empty queue".
         END.
      END.
       
     IF llLogoutFail THEN
        PUT UNFORMATTED fTS2HMS(fMakeTS()) " Logout failure: "
           lcLogoutResp  CHR(10).

      OUTPUT CLOSE.
   
   END.
   
   RETURN lcResponse.

END PROCEDURE.

PROCEDURE pOneMessage:

   DEFINE INPUT PARAMETER pcSMS  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcCLI  AS CHARACTER NO-UNDO.

   RUN pSMSMessage(fMessageId(),pcCLI,pcSMS,lhSocket,"622").

   RETURN RETURN-VALUE.
   
END PROCEDURE.

PROCEDURE pCallAlarm:

   DEFINE INPUT PARAMETER pcSMS  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcCLI  AS CHARACTER NO-UNDO.

   DEFINE VARIABLE lcMsg      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liLoop     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liMax      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE ldeTS      AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldActDate   AS DATE      NO-UNDO.   
   DEFINE VARIABLE liActTime   AS INTEGER   NO-UNDO.   
   DEFINE VARIABLE lcInterval  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liStart     AS INT       NO-UNDO.
   DEFINE VARIABLE liEnd       AS INT       NO-UNDO.
   
   ASSIGN
      liMax = 0
      ldeTS = fMakeTS().

   DEFINE BUFFER bufAlarm FOR CallAlarm.

   EMPTY TEMP-TABLE ttPriority.
   
   /* get current priority definition here, so possible changes can be
      done without resetting the run */
   FOR EACH TMSCodes NO-LOCK WHERE
            TMSCodes.TableName = "CallAlarm" AND
            TMSCodes.FieldName = "CreditType":
      CREATE ttPriority.
      ttPriority.CreditType = INTEGER(TMSCodes.CodeValue) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT.
      
      ttPriority.Priority   = IF TMSCodes.ConfigValue = ""
                              THEN 5
                              ELSE INTEGER(TMSCodes.ConfigValue) NO-ERROR.
      IF ttPriority.Priority = 0 THEN ttPriority.Priority = 5.
      
      IF NUM-ENTRIES(TMSCodes.ConfigValue,"-") > 1 THEN 
         ttPriority.ActInterval = TMSCodes.ConfigValue.
   END.

   SMS_LOOP:
   FOR EACH CallAlarm NO-LOCK WHERE
            CallAlarm.Brand     = "1" AND
            CallAlarm.DeliStat  = 1   AND
            CallAlarm.DeliType  = 1   AND
            CallAlarm.ActStamp <= ldeTS,
      FIRST ttPriority WHERE
            ttPriority.CreditType = CallAlarm.CreditType
   BY ttPriority.Priority
   BY CallAlarm.ActStamp:

      DO TRANSACTION:
      
         FIND FIRST bufAlarm WHERE
              RECID(bufAlarm) = RECID(CallAlarm)
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF LOCKED(bufAlarm) THEN NEXT.

         IF bufAlarm.DeliStat NE 1 THEN NEXT.
         
         IF ttPriority.ActInterval > "" THEN
            lcInterval = ttPriority.ActInterval.
         ELSE lcInterval = CallAlarm.ActInterval.
         
         IF lcInterval NE "" THEN DO:
            liStart  = INT(ENTRY(1, lcInterval, "-")) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN lcInterval = "".
            liEnd    = INT(ENTRY(2, lcInterval, "-")) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN lcInterval = "".
         END.

         /* Check possible send time restriction */ 
         IF lcInterval NE "" THEN DO:
            fSplitTS(input ldeTS,
                     output ldActDate,
                     output liActTime).
            IF liActTime < liStart 
               THEN DO: 
               bufAlarm.ActStamp = fMake2Dt(ldActDate, liStart).
               RELEASE bufAlarm.
               liMax = liMax + 1.
               IF liMax >= 4000 THEN LEAVE SMS_LOOP.
               NEXT.
            END.
            ELSE IF liActTime > liEnd 
               THEN DO:
               bufAlarm.ActStamp = fMake2Dt(ldActDate + 1, liStart).
               RELEASE bufAlarm.
               liMax = liMax + 1.
               IF liMax >= 4000 THEN LEAVE SMS_LOOP.
               NEXT.
            END.
         END.
   
         IF CallAlarm.DeliMsg NE "" THEN DO:
      
            lcMsg = CallAlarm.DeliMsg.
         
            liLoop = INDEX(lcMsg,CHR(10)).
         
            DO WHILE liLoop > 0:
               SUBSTR(lcMsg,liLoop,1) = " ".
               liLoop = INDEX(lcMsg,CHR(10)).
            END.
         
            lcMsg = SUBSTR(lcMsg,1,160).
         
            RUN pSMSMessage(fMessageId(),CallAlarm.CLI,lcMsg,lhSocket,CallAlarm.Orig).

            lcResponse = RETURN-VALUE.

            OUTPUT TO VALUE(lcLogFile) APPEND.
            PUT UNFORMATTED fTS2HMS(fMakeTS()) " " CallAlarm.CLI " " 
               liMsgId ": " lcResponse ":" lietime CHR(10).
            OUTPUT CLOSE.
            
            /* TODO: Proper error handling */
            IF lcResponse EQ "ERROR:Not connected" THEN DO:
               RELEASE bufAlarm.
               LEAVE SMS_LOOP.
            END.
            
            /* message is acknowledged */
            IF lcResponse EQ "OK" THEN bufAlarm.DeliStat = 3.
            ELSE bufAlarm.DeliStat = 2.

         END.
         ELSE bufAlarm.DeliStat = 2.
         
         bufAlarm.DeliStamp = fMakeTS().
         RELEASE bufAlarm.
      
      END.
         
      liMax = liMax + 1.
      IF liMax >= 2000 THEN LEAVE SMS_LOOP.
      
   END.

END PROCEDURE.

PROCEDURE pLogIn:

   DEFINE INPUT        PARAMETER pcUser   AS CHARACTER NO-UNDO.
   DEFINE INPUT        PARAMETER pcPWD    AS CHARACTER NO-UNDO.
   DEFINE INPUT        PARAMETER pcSystemType AS CHARACTER NO-UNDO.
   DEFINE INPUT        PARAMETER piMsgNum AS INTEGER   NO-UNDO.
   DEFINE INPUT        PARAMETER pcURL    AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER phSocket AS HANDLE    NO-UNDO.
   
   DEF VAR lcResponse AS CHARACTER NO-UNDO.
   DEF VAR llTimeOut  AS LOGICAL NO-UNDO.
   
   DEF VAR lmPDU AS MEMPTR NO-UNDO.
   DEF VAR lmResponseText AS MEMPTR NO-UNDO.
   DEF VAR liResult AS INT NO-UNDO. 
   DEF VAR liBinSize AS INT NO-UNDO. 
   
   SET-SIZE(lmPDU) = {&SMPP_PDU_MAX_SIZE}.
   SET-SIZE(lmResponseText) = {&SMPP_PDU_MAX_SIZE}.
   SET-BYTE-ORDER(lmPDU) = BIG-ENDIAN.
   SET-BYTE-ORDER(lmResponseText) = BIG-ENDIAN.

   RUN smpp_bind_transceiver(
      INPUT pcUser,
      INPUT pcPWD,
      INPUT pcSystemType,
      INPUT piMsgNum,
      INPUT GET-SIZE(lmPDU),
      INPUT GET-SIZE(lmResponseText),
      lmResponseText,
      lmPDU,
      OUTPUT liBinSize,
      OUTPUT liResult).

   IF liResult NE 0 THEN DO:
      lcResponse = "ERR:smpp_bind_transceiver creation: " + GET-STRING(lmResponseText,1).
      SET-SIZE(lmResponseText) = 0.
      SET-SIZE(lmPDU) = 0.
      RETURN lcResponse.
   END.
   
   SET-SIZE(lmResponseText) = 0.
   
   phSocket:CONNECT(pcURL) NO-ERROR.
   
   IF NOT ERROR-STATUS:ERROR THEN DO:
      
      phSocket:SET-READ-RESPONSE-PROCEDURE('pServerResponse').
      lcServerResponseProcedure = "smpp_bind_transceiver_resp".
      
      RUN pWriteSocket(lmPDU, liBinSize, phSocket).
      
      WAIT-FOR READ-RESPONSE OF phSocket PAUSE 30.
      
      llTimeOut = (LAST-EVENT:CODE = -1). 

      IF NOT llTimeOut THEN lcResponse = RETURN-VALUE.
      ELSE                  lcResponse = "ERR:Timeout".

   END.
   ELSE lcResponse = "ERR:Could not connect".
   
   SET-SIZE(lmPDU) = 0.

   RETURN lcResponse.

END.

PROCEDURE pLogOut:

   DEFINE INPUT-OUTPUT PARAMETER phSocket AS HANDLE NO-UNDO.
   DEFINE INPUT        PARAMETER piMsgNum AS INTEGER   NO-UNDO.
   
   DEF VAR lcResponse AS CHARACTER NO-UNDO.
   DEF VAR llTimeOut  AS LOGICAL NO-UNDO.
   DEF VAR lmPDU AS MEMPTR NO-UNDO.
   DEF VAR lmResponseText AS MEMPTR NO-UNDO.
   DEF VAR liResult AS INT NO-UNDO. 
   DEF VAR liBinSize AS INT NO-UNDO. 
   
   IF phSocket:CONNECTED() THEN DO:
   
      SET-SIZE(lmPDU) = {&SMPP_PDU_MAX_SIZE}.
      SET-SIZE(lmResponseText) = {&SMPP_PDU_MAX_SIZE}.
      SET-BYTE-ORDER(lmPDU) = BIG-ENDIAN.
      SET-BYTE-ORDER(lmResponseText) = BIG-ENDIAN.
      
      RUN smpp_unbind(
         INPUT piMsgNum,
         INPUT GET-SIZE(lmResponseText),
         lmResponseText,
         INPUT GET-SIZE(lmPDU),
         lmPDU,
         OUTPUT liBinSize,
         OUTPUT liResult).
      
      IF liResult NE 0 THEN DO:
         lcResponse = SUBST("ERR:smpp_unbind creation: &1", GET-STRING(lmResponseText,1)).
         SET-SIZE(lmResponseText) = 0.
         SET-SIZE(lmPDU) = 0.
         phSocket:DISCONNECT().
         RETURN lcResponse.
      END.
      
      SET-SIZE(lmResponseText) = 0.
      
      lcServerResponseProcedure = "smpp_unbind_resp".
      RUN pWriteSocket(lmPDU, liBinSize, phSocket).
      SET-SIZE(lmPDU) = 0.
      
      WAIT-FOR READ-RESPONSE OF phSocket PAUSE 30.
      
      llTimeOut = (LAST-EVENT:CODE = -1). 

      IF NOT llTimeOut THEN lcResponse = RETURN-VALUE.
      ELSE                  lcResponse = "ERR:Timeout".

   END.

   phSocket:DISCONNECT().
   
   RETURN lcResponse.

END.

PROCEDURE pSMSMessage:

   DEFINE INPUT PARAMETER piMsgNum AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER pcCLI    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcSMS    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER phSocket AS HANDLE    NO-UNDO.
   DEFINE INPUT PARAMETER pcOrig   AS CHARACTER NO-UNDO.

   DEFINE VARIABLE lcMessage  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSMS AS CHARACTER NO-UNDO. 

   IF NOT phSocket:CONNECTED() THEN RETURN "ERROR:Not connected".

   IF pcCLI BEGINS "+" AND
      INDEX(pcCLI,"+",2) = 0 then
      pcCLI = "00" + SUBSTRING(pcCLI,2).

   IF pcOrig BEGINS "+" AND
     INDEX(pcOrig,"+",2) = 0 then
     pcOrig = "00" + SUBSTRING(pcOrig,2).
   
   IF pcOrig EQ "" THEN pcOrig = {&SMPP_DEFAULT_SOURCE_ADDRESS}.
   ASSIGN
      pcSMS    = REPLACE(pcSMS,"í","i")
      pcSMS    = REPLACE(pcSMS,"ó","o")
      pcSMS    = REPLACE(pcSMS,"á","a")
      pcSMS    = REPLACE(pcSMS,"ú","u").

   lcSMS = fConvertToGSM0338(pcSMS).
   
   DEF VAR lcResponse AS CHARACTER NO-UNDO.
   DEF VAR llTimeOut  AS LOGICAL NO-UNDO.
   
   DEF VAR lmPDU AS MEMPTR NO-UNDO.
   DEF VAR lmResponseText AS MEMPTR NO-UNDO.
   DEF VAR liResult AS INT NO-UNDO. 
   DEF VAR liBinSize AS INT NO-UNDO. 
   
   SET-SIZE(lmPDU) = {&SMPP_PDU_MAX_SIZE}.
   SET-SIZE(lmResponseText) = {&SMPP_PDU_MAX_SIZE}.
   SET-BYTE-ORDER(lmPDU) = BIG-ENDIAN.
   SET-BYTE-ORDER(lmResponseText) = BIG-ENDIAN.
   
   RUN smpp_submit_sm(
      INPUT piMsgNum,
      INPUT pcCLI,
      INPUT pcOrig,
      INPUT lcSMS,
      INPUT GET-SIZE(lmResponseText),
      lmResponseText,
      INPUT GET-SIZE(lmPDU),
      lmPDU,
      OUTPUT liBinSize,
      OUTPUT liResult).

   IF liResult NE 0 THEN DO:
      lcResponse = SUBST("ERR:smpp_submit_sm_resp creation: &1",
         GET-STRING(lmResponseText,1)).
      SET-SIZE(lmResponseText) = 0.
      SET-SIZE(lmPDU) = 0.
      RETURN lcResponse.
   END.
   
   SET-SIZE(lmResponseText) = 0.

   lcServerResponseProcedure = "smpp_submit_sm_resp".
   RUN pWriteSocket(lmPDU, liBinSize, phSocket).
   SET-SIZE(lmPDU) = 0.
   
   WAIT-FOR READ-RESPONSE OF phSocket PAUSE 30.
   
   llTimeOut = (LAST-EVENT:CODE = -1). 
   lietime = etime.

   IF NOT llTimeOut THEN lcResponse = RETURN-VALUE.
   ELSE                  lcResponse = "ERR:Timeout".

   RETURN lcResponse.

END PROCEDURE.

PROCEDURE pServerResponse:

   DEFINE VARIABLE liDataSize AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lmData     AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE llRC       AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
      
   DEF VAR lmPDU AS MEMPTR NO-UNDO.
   DEF VAR lmResponseText AS MEMPTR NO-UNDO.
   DEF VAR liResult AS INT NO-UNDO. 
   DEF VAR liBinSize AS INT NO-UNDO. 
   DEF VAR liMsgIdResp AS INT NO-UNDO.
   DEF VAR liCommandStatus AS INT NO-UNDO. 

   IF SELF:CONNECTED() = FALSE THEN RETURN "ERR:Connection lost".
   
   liDataSize = SELF:GET-BYTES-AVAILABLE().

   SET-SIZE(lmData)       = 0.
   SET-SIZE(lmData)       = liDataSize.
   SET-BYTE-ORDER(lmData) = BIG-ENDIAN.

   llRC = SELF:READ(lmData,1,liDataSize,1) NO-ERROR.

   IF llRC = FALSE OR ERROR-STATUS:GET-MESSAGE(1) <> '' THEN DO:
      IF NOT SESSION:BATCH THEN
         MESSAGE llRC ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX.
      lcResponse =  'ERR:Unable to read response'.
   END.
   ELSE DO:
      SET-SIZE(lmResponseText) = 0.
      SET-SIZE(lmResponseText) = {&SMPP_PDU_MAX_SIZE}.

      RUN VALUE(lcServerResponseProcedure) (
         INPUT liDataSize,
         INPUT lmData, /* input */
         INPUT {&SMPP_PDU_MAX_SIZE},
         INPUT lmResponseText, /* output */
         OUTPUT liCommandStatus,
         OUTPUT liMsgIdResp,
         OUTPUT liResult).
      
      SET-SIZE(lmData) = 0.
      
      IF liResult NE 0 OR liCommandStatus NE 0 THEN DO:
         lcResponse = SUBST("ERR: &1: result code: &2, command status: &3, error text: &4",
                     lcServerResponseProcedure,
                     liResult,
                     liCommandStatus, 
                     GET-STRING(lmResponseText,1)).
         SET-SIZE(lmResponseText) = 0.
         RETURN lcResponse.
      END.
      
      SET-SIZE(lmResponseText) = 0.

      IF liMsgId NE liMsgIdResp THEN DO:
         lcResponse = SUBST("ERR: request sequence id &1 does not match with response sequence id &2",
                      liMsgId, liMsgIdResp).
      END.
      ELSE lcResponse = "OK".
   END.

   RETURN lcResponse.

END PROCEDURE.

PROCEDURE pWriteSocket:

   DEFINE INPUT PARAMETER pmRequest AS MEMPTR NO-UNDO.
   DEFINE INPUT PARAMETER piRequestSize AS INT NO-UNDO.
   DEFINE INPUT PARAMETER phSocket  AS HANDLE    NO-UNDO.

   DEFINE VARIABLE liDataSize AS INTEGER   NO-UNDO.
   DEFINE VARIABLE llRC       AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
   
   liDataSize = GET-SIZE(pmRequest).

   etime(true).
   llRC = phSocket:WRITE(pmRequest,1,piRequestSize) NO-ERROR.

   IF llRC = FALSE OR ERROR-STATUS:GET-MESSAGE(1) <> '' THEN DO:
      lcResponse =  'ERR:Unable to write message'.
   END.
   ELSE lcResponse = "OK".

   SET-SIZE(pmRequest) = 0.

   RETURN lcResponse.

END PROCEDURE.
