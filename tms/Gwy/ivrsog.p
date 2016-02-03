/* ----------------------------------------------------------------------------
  MODULE .......: sogresponse.p
  FUNCTION .....: 
  APPLICATION ..: TMS
  CREATED ......: 
  MODIFIED .....: 

  Version ......: 
  --------------------------------------------------------------------------- */

{Func/timestamp.i}

DEFINE INPUT PARAMETER piSoLog    AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER pcCommLine AS CHARACTER NO-UNDO.

DEFINE VARIABLE lhServer   AS HANDLE    NO-UNDO.
DEFINE VARIABLE lcSogHost  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSogPort  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogIn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lhSocket   AS HANDLE    NO-UNDO.
DEFINE VARIABLE lcAck      AS CHARACTER NO-UNDO.
DEFINE VARIABLE liSoLog    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcError    AS CHARACTER NO-UNDO.
DEFINE VARIABLE llTimeOut  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcLogDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile  AS CHARACTER NO-UNDO.

RUN sogpost(pcCommLine,"LOGIN ivr ivr",OUTPUT lcError).

IF lcError BEGINS "ERROR:" THEN RETURN lcError.

ASSIGN
   lcSogHost = "sedna"
   lcSogPort = "14401"
   lcLogin   = "LOGIN ivr ivr"
   lcLogDir  = "/scratch/nagios/tms/sog/".

RUN pConnection.

IF RETURN-VALUE = "OK" THEN REPEAT ON STOP UNDO, LEAVE ON QUIT UNDO, LEAVE:

   liSoLog = 0.

   IF NOT lhSocket:CONNECTED() THEN RUN pConnection.

   WAIT-FOR READ-RESPONSE OF lhSocket PAUSE 1.

   llTimeOut = (LAST-EVENT:CODE = -1). 

   lcLogFile = "ivrresponse_"             +
               STRING(YEAR(TODAY),"9999") + 
               STRING(MONTH(TODAY),"99")  +
               STRING(DAY(TODAY),"99")    +
               ".log".

   OUTPUT TO VALUE(lcLogDir + lcLogFile) APPEND.
   PUT UNFORMATTED "OK: " fTS2HMS(fMakeTS()) " " llTimeOut " " lcResponse CHR(10).
   OUTPUT CLOSE.

   IF NOT llTimeOut THEN DO:
      
      lcResponse = RETURN-VALUE.

      ASSIGN
         liSoLog = INTEGER(ENTRY(1,lcResponse," "))
         lcAck   = "ACK " + STRING(liSoLog)
      NO-ERROR.

      RUN pPostRequest(lcAck + CHR(10),lhSocket).

      IF liSoLog = piSoLog THEN liLoop = 999.
   
   END.
   ELSE lcResponse = "ERROR: No response in time".

   liLoop  = liLoop + 1.

   IF liLoop >= 60 THEN LEAVE.
   
END.
ELSE lcResponse = RETURN-VALUE.

lhSocket:DISCONNECT() NO-ERROR.

IF VALID-HANDLE(lhSocket) THEN DELETE OBJECT(lhSocket).

RETURN lcResponse.

PROCEDURE pConnection:

   DEFINE VARIABLE lcReturn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llRC     AS LOGICAL   NO-UNDO.

   lhSocket:DISCONNECT() NO-ERROR.
   
   DELETE OBJECT lhSocket NO-ERROR.

   CREATE SOCKET lhSocket.

   llRC = lhSocket:CONNECT('-H ' + lcSogHost + ' -S ' + lcSogPort ) NO-ERROR.

   IF NOT llRC OR NOT lhSocket:CONNECTED() THEN lcReturn = "ERROR: Connection".
   ELSE DO:

      lhSocket:SET-READ-RESPONSE-PROCEDURE('pGetServerResponce').

      /* 1: login */
      RUN pPostRequest(lcLogIn + CHR(10),lhSocket).

      ETIME(YES).

      WAIT-FOR READ-RESPONSE OF lhSocket PAUSE 10.

      IF ETIME <= 10000 THEN lcReturn = RETURN-VALUE.
      ELSE                   lcReturn = "Error".

      IF lcReturn = "OK LOGIN" + CHR(10) THEN
         lcReturn = "OK".
      ELSE
         lcReturn = "ERROR: LogIn".
         
   END.

   RETURN lcReturn.

END.

/* make a request and post the xml document */
PROCEDURE pPostRequest:

   DEFINE INPUT PARAMETER pcRequest AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER lhSocket  AS HANDLE    NO-UNDO.
   
   DEFINE VARIABLE liRequest   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liMsg       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lmRequest   AS MEMPTR    NO-UNDO.

   liRequest = LENGTH(pcRequest).

   SET-SIZE(lmRequest) = 0.
   SET-SIZE(lmRequest) = liRequest + 1.
   SET-BYTE-ORDER(lmRequest) = BIG-ENDIAN.

   PUT-STRING(lmRequest,1) = pcRequest.

   lhSocket:WRITE(lmRequest,1,liRequest) NO-ERROR.

   SET-SIZE(lmRequest) = 0.

   IF ERROR-STATUS:ERROR THEN RETURN "ERROR: Write failure.".

END PROCEDURE.

PROCEDURE pGetServerResponce:

   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lmResponse AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE llRC       AS LOGICAL   NO-UNDO.

   IF SELF:CONNECTED() = FALSE THEN
      lcResponse = "ERROR: Connection failure.".
   ELSE DO:

      SET-SIZE(lmResponse) = 0.
      SET-SIZE(lmResponse) = lhSocket:GET-BYTES-AVAILABLE() + 1.
      SET-BYTE-ORDER(lmResponse) = BIG-ENDIAN.

      llRC = lhSocket:READ(lmResponse,1,lhSocket:GET-BYTES-AVAILABLE() + 1,1).

      lcResponse = GET-STRING(lmResponse,1).

   END.

   RETURN lcResponse.

END PROCEDURE.
