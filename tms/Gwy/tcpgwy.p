/* -----------------------------------------------
  MODULE .......: TCPGWY.P
  FUNCTION .....: Write text to TCP port, wait for response
  APPLICATION ..: TMS
  CREATED ......: 20.10.2006 KL
  MODIFIED .....: 16.11.2006 kl piTimeOut as parameter
                  20.11.2006 kl piLoops, pcCheck
                  14.03.2007 kl ldeTSNow
                  15.03.2007 kl llDebug
                  22.03.2007 kl llDebug fixed

  VERSION ......: XFERA
------------------------------------------------------ */

{commali.i}
{timestamp.i}
{cparam2.i}
{log.i}

gcBrand = "1".

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
DEFINE VARIABLE ldeTSNow    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE llDebug     AS LOGICAL   NO-UNDO.

ASSIGN
   llDebug    = (fCParam("DEBUG","tcpgwy.p") NE "0")
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


ldeTSNow = fMakeTS().

IF llDebug THEN DO:

   OUTPUT TO VALUE("/tmp/atmtcp_" + STRING(TRUNC(ldeTSNow,0)) + ".log") APPEND.
   PUT UNFORMATTED "LogIn took:" pcURL " " STRING(ETIME) CHR(10).
   OUTPUT CLOSE.

END.

piTimeOut = piTimeOut - INT(ETIME / 2000).

lhSocket:SET-READ-RESPONSE-PROCEDURE('pServerResponse') NO-ERROR.

RUN pWriteSocket.

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
      
      lcResponse = lcResponse + RETURN-VALUE.

      llTimeOut = (LAST-EVENT:CODE = -1). 

      IF pcCheck NE "" AND INDEX(lcResponse,pcCheck) > 0 THEN
         LEAVE WAITLOOP.

      IF liLoop = piLoops AND llTimeOut THEN
         lcResponse = "ERR:No response".  

   END.

END.

lhSocket:DISCONNECT() NO-ERROR.

DELETE OBJECT lhSocket.

RETURN lcResponse.

PROCEDURE pServerResponse:

   DEFINE VARIABLE liDataSize AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
   
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
      IF NOT SESSION:BATCH THEN DO:
         IF katun EQ "Service" OR
            katun EQ "PerCont" OR
            katun EQ "STC" OR
            katun EQ "CreFixed" OR
            katun EQ "CreSub" OR
            katun EQ "request" THEN
            fLogError(SUBST("TCPGWY: Unable to read response: &1, &2",
                      llRC, ERROR-STATUS:GET-MESSAGE(1))).
         ELSE MESSAGE llRC ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX.
      END.
      lcResponse = 'ERR:Unable to read response'.
      RETURN.
   END.

   lcResponse = GET-STRING(lmData,1) NO-ERROR.

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

