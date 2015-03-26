/* ----------------------------------------------------------------------------
  MODULE .......: SMSCGWY.P
  FUNCTION .....: Gateway for SMSC platform
  APPLICATION ..: TMS
  CREATED ......: 20.10.06 KL
  CHANGED ......: 20.03.07 kl user buffer in pCallAlarm
                  21.03.07 kl DO TRANSACTION in pCallAlarm
                  19.07.07 kl reset liMsgId for each process

  Version ......: XFERA
  --------------------------------------------------------------------------- */

/*

File formats etc:

https://luna.starnet.fi/display/XFERA/SMSC+specifications

*/

{commpaa.i} gcBrand = "1".
{mathfunction.i}
{cparam2.i}
{charset.i}
{timestamp.i}
{smsc_messager.i}

DEFINE VARIABLE lcUser      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPWD       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcHost      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPort      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcURL       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lhSocket    AS HANDLE    NO-UNDO.

DEF TEMP-TABLE ttPriority NO-UNDO
   FIELD CreditType AS INT
   FIELD Priority   AS INT
   FIELD ActInterval AS CHAR
   INDEX CreditType CreditType.

DEF VAR lcLogFile AS CHAR NO-UNDO INIT
   "/scratch/nagios/tms/callalarm/response.log".

FUNCTION fCheckSum RETURNS CHARACTER
  (INPUT pcMessage AS CHARACTER):
  
   /* PARAMETERS:
      pcMessage: message in decimal format
   */
     
   DEFINE VARIABLE liLoop AS INT  NO-UNDO.
   DEFINE VARIABLE liCSum AS INT  NO-UNDO.
   DEFINE VARIABLE lcCSum AS CHAR NO-UNDO.
   DEFINE VARIABLE liBit1 AS INT  NO-UNDO.
   DEFINE VARIABLE liBit2 AS INT  NO-UNDO.
   
   DO liLoop = 1 TO LENGTH(pcMessage):

      ASSIGN
         liCSum = liCSum + KEYCODE(SUBSTR(pcMessage,liLoop,1))
         lcCSum = fInt2Bin(liCSum).
      
      liCSum = fBin2Int(lcCSum).

   END.

   IF      LENGTH(lcCSum) > 8 THEN
      lcCSum = SUBSTR(lcCSum, LENGTH(lcCSum) - 7).
   ELSE IF LENGTH(lcCSum) < 8 THEN
      lcCSum = FILL("0",8 - LENGTH(lcCSum)) + lcCSum.

   ASSIGN
      liBit1 = fBin2Int(SUBSTR(lcCSum,1,4))
      liBit2 = fBin2Int(SUBSTR(lcCSum,5))
      lcCSum = fInt2Hex(liBit1) + fInt2Hex(liBit2).

   RETURN lcCSum.
   
END FUNCTION.

DEFINE VARIABLE liMsgId AS INTEGER NO-UNDO.

FUNCTION fMessageId RETURNS INTEGER:

   liMsgId = liMsgId + 1.
   IF liMsgId > 99 THEN liMsgId = 1.
   
   RETURN liMsgId.
   
END.
         
PROCEDURE pInitialize:

   DEF INPUT PARAM piNode AS INT NO-UNDO.

   CREATE SOCKET lhSocket.

   ASSIGN
      lcHost = (IF piNode MOD 2 EQ 1
                  THEN "95.169.240.124" 
                  ELSE "95.169.240.125") 
      lcPort = "2781"
      lcUser = "800"
      /* convert pwd into octal */
      lcPWD  = fIRAChar("test1234").
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
      
      RUN pLogIn(lcUser,lcPWD,fMessageId(),INPUT lcUrl, INPUT-OUTPUT lhSocket).

      lcResponse = RETURN-VALUE.

      IF NUM-ENTRIES(lcResponse,"/") < 5 OR
         ENTRY(5,lcResponse,"/") NE "A" THEN 
           ASSIGN
                llRun       = FALSE
                llLoginFail = TRUE.
   
      IF llRun THEN DO:

         liMsgId = 0.

         RUN VALUE(lcProcedure)
           (INPUT pcSMS, INPUT pcCLI, INPUT lcUser).

         lcResponse = RETURN-VALUE.

         RUN pLogOut(INPUT-OUTPUT lhSocket).

      END.

   END.

   IF NOT llRun THEN DO:
      
      OUTPUT TO VALUE(lcLogFile) APPEND.
      IF llLoginFail THEN
           PUT UNFORMATTED fTS2HMS(fMakeTS()) " Login failure: " + lcResponse  CHR(10).
      ELSE DO:
           PUT UNFORMATTED fTS2HMS(fMakeTS()) " Nothing to do"  CHR(10).
           lcResponse = "Empty queue".
      END.
      OUTPUT CLOSE.
   
   END.
   
   RETURN lcResponse.

END PROCEDURE.

PROCEDURE pOneMessage:

   DEFINE INPUT PARAMETER pcSMS  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcCLI  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcUser AS CHARACTER NO-UNDO.

   RUN pSMSMessage(pcUser,fMessageId(),pcCLI,pcSMS,lhSocket,"622").

   RETURN RETURN-VALUE.
   
END PROCEDURE.

PROCEDURE pCallAlarm:

   DEFINE INPUT PARAMETER pcSMS  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcCLI  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcUser AS CHARACTER NO-UNDO.

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
               IF liMax >= 2000 THEN LEAVE SMS_LOOP.
               NEXT.
            END.
            ELSE IF liActTime > liEnd 
               THEN DO:
               bufAlarm.ActStamp = fMake2Dt(ldActDate + 1, liStart).
               RELEASE bufAlarm.
               liMax = liMax + 1.
               IF liMax >= 2000 THEN LEAVE SMS_LOOP.
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
         
            RUN pSMSMessage(pcUser,fMessageId(),CallAlarm.CLI,lcMsg,lhSocket,CallAlarm.Orig).

            lcResponse = RETURN-VALUE.

            /* message is acknowledged */
            IF NUM-ENTRIES(lcResponse,"/") >= 5 AND
               ENTRY(5,lcResponse,"/") = "A" THEN bufAlarm.DeliStat = 3.
            ELSE bufAlarm.DeliStat = 2.

            OUTPUT TO VALUE(lcLogFile) APPEND.
            PUT UNFORMATTED fTS2HMS(fMakeTS()) " " CallAlarm.CLI ": " lcResponse CHR(10).
            OUTPUT CLOSE.

         END.
         ELSE bufAlarm.DeliStat = 2.
         
         bufAlarm.DeliStamp = fMakeTS().
         RELEASE bufAlarm.
      
      END.
         
      liMax = liMax + 1.
      IF liMax >= 1000 THEN LEAVE SMS_LOOP.
      
   END.

END PROCEDURE.

PROCEDURE pLogIn:

   DEFINE INPUT        PARAMETER pcUser   AS CHARACTER NO-UNDO.
   DEFINE INPUT        PARAMETER pcPWD    AS CHARACTER NO-UNDO.
   DEFINE INPUT        PARAMETER piMsgNum AS INTEGER   NO-UNDO.
   DEFINE INPUT        PARAMETER pcURL    AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER phSocket AS HANDLE    NO-UNDO.
   
   DEFINE VARIABLE lcLogIn    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcUser     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcPWD      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcHeader   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liLength   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llTimeOut  AS LOGICAL   NO-UNDO.
   
   /* Header record:
      1) Transaction reference number
      2) Number of IRA characters
      3) O = Operation
      4) 60 = Session Management
   */

   lcHeader = STRING(piMsgNum,"99")    + "/" +
              STRING(liLength,"99999") + "/" +
              "O"  + "/" + 
              "60" + "/".
   
   /* Data record: See documentation pages 52-53 */
   lcLogin = pcUser + "/" + /* User */
             ""     + "/" + /* OTON */
             ""     + "/" + /* ONPI */
             "1"    + "/" + /* STYP */
             pcPWD  + "/" + /* PWD  */
             ""     + "/" + /* NPWD */
             "0100" + "/" + /* VERS */
             ""     + "/" + /* LAdC */
             ""     + "/" + /* LTON */
             ""     + "/" + /* LNPI */
             ""     + "/" + /* OPID */
             ""     + "/" + /* RES1 */
             "".
             
   liLength = LENGTH(lcHeader) + LENGTH(lcLogin) + 2.

   SUBSTRING(lcHeader,4,5) = STRING(liLength,"99999").

   lcLogin = lcHeader + lcLogin.

   lcLogin = lcLogin + fCheckSum(lcLogin).

   phSocket:CONNECT(pcURL) NO-ERROR.

   IF NOT ERROR-STATUS:ERROR THEN DO:
      
      phSocket:SET-READ-RESPONSE-PROCEDURE('pServerResponse').
      
      RUN pWriteSocket(lcLogin,phSocket).
      
      WAIT-FOR READ-RESPONSE OF phSocket PAUSE 10.
      
      llTimeOut = (LAST-EVENT:CODE = -1). 

      IF NOT llTimeOut THEN lcResponse = RETURN-VALUE.
      ELSE                  lcResponse = "ERR:Timeout".

   END.
   ELSE lcResponse = "ERR:Could not connect".

   RETURN lcResponse.

END.

PROCEDURE pLogOut:

   DEFINE INPUT-OUTPUT PARAMETER phSocket AS HANDLE NO-UNDO.

   phSocket:DISCONNECT().
   
   RETURN RETURN-VALUE.

END.

PROCEDURE pSMSMessage:

   DEFINE INPUT PARAMETER pcUser   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER piMsgNum AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER pcCLI    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pcSMS    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER phSocket AS HANDLE    NO-UNDO.
   DEFINE INPUT PARAMETER pcOrig   AS CHARACTER NO-UNDO.

   DEFINE VARIABLE lcHeader   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcMessage  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcTS       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSMS      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liLength   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llTimeOut  AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE lcOrig     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOTOA     AS CHARACTER NO-UNDO. 

   /* Header record:
      1) Transaction reference number
      2) Number of IRA characters
      3) O  = Operation
      4) 51 = Submit Short Message
   */

   IF NOT phSocket:CONNECTED() THEN RETURN "ERROR".
   
   ASSIGN
      pcSMS    = REPLACE(pcSMS,"ó","o")
      pcSMS    = REPLACE(pcSMS,"á","a")
      lcSMS    = fIRAChar(pcSMS).
      lcTS     = STRING(DAY(TODAY),"99")   +
                 STRING(MONTH(TODAY),"99") +
                 STRING(YEAR(TODAY) - 2000,"99")  +
                 REPLACE(STRING(TIME,"HH:MM:SS"),":","").
      lcHeader = STRING(piMsgNum,"99")    + "/" +
                 STRING(liLength,"99999") + "/" +
                 "O"  + "/" + 
                 "51" + "/".

   /* Default originating number */
   IF pcOrig EQ "" THEN pcOrig = "800622800".
   IF fIsAlphaNum(pcOrig) THEN ASSIGN 
      pcOrig = encodeOAdC(pcOrig)
      lcOTOA = "5039".

   /* Data record: See documentation pages 28-29 */
   lcMessage = pcCLI  + "/" + /* AdC   */
               pcOrig + "/" + /* OAdC  */
               ""     + "/" + /* AC    */
               ""     + "/" + /* NRq   */
               ""     + "/" + /* NAdC  */
               "0"    + "/" + /* NT    */
               ""     + "/" + /* NPID  */
               ""     + "/" + /* LRq   */
               ""     + "/" + /* LRAd  */
               ""     + "/" + /* LPID  */
               ""     + "/" + /* DD    */
               ""     + "/" + /* DDT   */
               ""     + "/" + /* VP    */
               ""     + "/" + /* RPID  */
               ""     + "/" + /* SCTS  */
               ""     + "/" + /* Dst   */
               ""     + "/" + /* Rsn   */
               ""     + "/" + /* DSCTS */
               "3"    + "/" + /* MT    */
               ""     + "/" + /* NB    */
               lcSMS  + "/" + /* AMsg  */
               ""     + "/" + /* MMS   */
               ""     + "/" + /* PR    */
               ""     + "/" + /* DCs   */
               ""     + "/" + /* MCLs  */
               ""     + "/" + /* RPI   */
               ""     + "/" + /* CPg   */
               ""     + "/" + /* RPLy  */
               lcOTOA + "/" + /* OTOA  */
               ""     + "/" + /* HPLMN */
               ""     + "/" + /* XSer  */
               ""     + "/" + /* RES4  */
               ""     + "/".  /* RES5  */

   liLength = LENGTH(lcHeader) + LENGTH(lcMessage) + 2.

   SUBSTRING(lcHeader,4,5) = STRING(liLength,"99999").

   lcMessage = lcHeader + lcMessage.

   lcMessage = lcMessage + fCheckSum(lcMessage).

   RUN pWriteSocket(lcMessage, phSocket).
   
   WAIT-FOR READ-RESPONSE OF phSocket PAUSE 10.
   
   llTimeOut = (LAST-EVENT:CODE = -1). 

   IF NOT llTimeOut THEN lcResponse = RETURN-VALUE.
   ELSE                  lcResponse = "ERR:Timeout".

   RETURN lcResponse.

END PROCEDURE.

PROCEDURE pServerResponse:

   DEFINE VARIABLE liDataSize AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lmData     AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE llRC       AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.

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
   ELSE lcResponse = GET-STRING(lmData,1).

   SET-SIZE(lmData) = 0.

   RETURN lcResponse.

END PROCEDURE.

PROCEDURE pWriteSocket:

   DEFINE INPUT PARAMETER pcRequest AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER phSocket  AS HANDLE    NO-UNDO.

   DEFINE VARIABLE lmData     AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE liDataSize AS INTEGER   NO-UNDO.
   DEFINE VARIABLE llRC       AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSTX      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcETX      AS CHARACTER NO-UNDO.
   
   ASSIGN
      lcSTX      = CHR(2)
      lcETX      = CHR(3)
      pcRequest  = lcSTX + pcRequest + lcETX
      liDataSize = LENGTH(pcRequest).

   SET-SIZE(lmData)       = 0.
   SET-SIZE(lmData)       = liDataSize.
   SET-BYTE-ORDER(lmData) = BIG-ENDIAN.

   PUT-STRING(lmData,1,liDataSize) = pcRequest.

   llRC = phSocket:WRITE(lmData,1,liDataSize) NO-ERROR.

   IF llRC = FALSE OR ERROR-STATUS:GET-MESSAGE(1) <> '' THEN DO:
      lcResponse =  'ERR:Unable to write message'.
   END.
   ELSE lcResponse = "OK".

   SET-SIZE(lmData) = 0.

   RETURN lcResponse.

END PROCEDURE.
