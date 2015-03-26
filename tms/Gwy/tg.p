/* -----------------------------------------------
  MODULE .......: TCPGWY.P
  FUNCTION .....: Write text to TCP port, wait for response
  APPLICATION ..: TMS
  CREATED ......: 20.10.2006 KL
  MODIFIED .....: 16.11.2006 kl piTimeOut as parameter
                  20.11.2006 kl piLoops, pcCheck

  VERSION ......: XFERA
------------------------------------------------------ */

/*
PARAMETERS:
 -pcRequest: message to send
 -pcURL: server/host to connect
 -piTimeOut: seconds to wait for answer
 -piLoop: nr. of answers to wait-for
 -pcCheck: allow response when this is in it
*/

DEFINE INPUT PARAMETER pcRequest  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcURL      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER piTimeOut  AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER piLoops    AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER pcCheck    AS CHARACTER NO-UNDO.

DEFINE VARIABLE lhSocket    AS HANDLE    NO-UNDO.
DEFINE VARIABLE llRC        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lmData      AS MEMPTR    NO-UNDO.
DEFINE VARIABLE liDataSize  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcResponse  AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop      AS INTEGER   NO-UNDO.
DEFINE VARIABLE llTimeOut   AS LOGICAL   NO-UNDO.

liDataSize = LENGTH(pcRequest).

PAUSE 0 BEFORE-HIDE.

CREATE SOCKET lhSocket.

ETIME(YES).

lhSocket:CONNECT(pcURL) NO-ERROR.

IF lhSocket:CONNECTED() = FALSE THEN DO:
   
   lhSocket:DISCONNECT() NO-ERROR.

   DELETE OBJECT lhSocket.

   lcResponse = 'ERR:Unable to Connect'.

   RETURN lcResponse.

END.
ELSE IF ETIME > 3500 THEN DO:

   lhSocket:DISCONNECT() NO-ERROR.

   DELETE OBJECT lhSocket.

   lcResponse = 'ERR:Took too long to Connect ' + STRING(ETIME).
   
   RETURN lcResponse.

END.

OUTPUT TO /tmp/tg.log APPEND.
PUT UNFORMATTED "LogIn took:" pcURL " " STRING(ETIME) CHR(10).
OUTPUT CLOSE.

piTimeOut = piTimeOut - INT(ETIME / 2000).

OUTPUT TO /tmp/tg.log APPEND.
PUT UNFORMATTED "TO: " STRING(piTimeout) CHR(10).
OUTPUT CLOSE.

lhSocket:SET-READ-RESPONSE-PROCEDURE('pServerResponse') NO-ERROR.

RUN pWriteSocket.

ETIME(YES).

IF lcResponse = "" THEN DO:

   WAITLOOP:
   DO liLoop = 1 TO piLoops ON ERROR UNDO WAITLOOP, LEAVE WAITLOOP
                            ON STOP  UNDO WAITLOOP, LEAVE WAITLOOP
                            ON QUIT  UNDO WAITLOOP, LEAVE WAITLOOP:
   
      IF NOT lhSocket:CONNECTED() THEN DO:
         lcResponse = "ERR:Lost connection".
         LEAVE WAITLOOP.
      END.
      
      WAIT-FOR READ-RESPONSE OF lhSocket PAUSE piTimeOut.
      
      llTimeOut = (LAST-EVENT:CODE = -1). 
      lcResponse = RETURN-VALUE.

      IF pcCheck NE "" AND INDEX(lcResponse,pcCheck) > 0 THEN
         LEAVE WAITLOOP.

      IF liLoop = piLoops AND llTimeOut THEN
         lcResponse = "ERR:No response".  

OUTPUT TO /tmp/tg.log APPEND.
PUT UNFORMATTED "Resp: " STRING(liLoop) " " lcResponse " " STRING(ETIME) " " LAST-EVENT:CODE " " piTimeOut CHR (10).
OUTPUT CLOSE.

   END.

END.

lhSocket:DISCONNECT() NO-ERROR.

DELETE OBJECT lhSocket.

RETURN lcResponse.

PROCEDURE pServerResponse:

   DEFINE VARIABLE liDataSize AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
   
OUTPUT TO /tmp/tg.log APPEND.
PUT UNFORMATTED "SC: " STRING(SELF:CONNECTED()) CHR(10).
OUTPUT CLOSE.

   IF SELF:CONNECTED() = FALSE THEN DO:
      lcResponse = "ERR:Connection lost".
      RETURN.
   END.
   
   liDataSize = SELF:GET-BYTES-AVAILABLE() NO-ERROR.

   SET-SIZE(lmData)       = 0.
   SET-SIZE(lmData)       = liDataSize.
   SET-BYTE-ORDER(lmData) = BIG-ENDIAN.

   llRC = SELF:READ(lmData,1,liDataSize,1) NO-ERROR.

   IF llRC = FALSE OR ERROR-STATUS:GET-MESSAGE(1) <> '' THEN DO:
      IF NOT SESSION:BATCH THEN
         MESSAGE llRC ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX.
      lcResponse = 'ERR:Unable to read response'.
      RETURN.
   END.

   lcResponse = GET-STRING(lmData,1) NO-ERROR.

OUTPUT TO /tmp/tg.log APPEND.
PUT UNFORMATTED "SCR: " lcResponse (10).
OUTPUT CLOSE.

   SET-SIZE(lmData) = 0.

   RETURN lcResponse.

END PROCEDURE.

PROCEDURE pWriteSocket:

   SET-SIZE(lmData)       = 0.
   SET-SIZE(lmData)       = liDataSize.
   SET-BYTE-ORDER(lmData) = BIG-ENDIAN.

   PUT-STRING(lmData,1,liDataSize) = pcRequest.

   llRC = lhSocket:WRITE(lmData,1,liDataSize) NO-ERROR.

   IF llRC = FALSE OR ERROR-STATUS:GET-MESSAGE(1) <> '' THEN DO:
      lcResponse = 'ERR:Unable to write message'.
      RETURN.
   END.

   SET-SIZE(lmData) = 0.

END PROCEDURE.

