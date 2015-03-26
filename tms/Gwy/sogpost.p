/* ----------------------------------------------------------------------------
  MODULE .......: MVNO_POST.P
  FUNCTION .....: Send MVNO POST request
  APPLICATION ..: TMS
  CREATED ......: 31.05.2006 kl
  MODIFIED .....: 

  Version ......: HPO
  --------------------------------------------------------------------------- */

DEFINE INPUT  PARAMETER pcRequest AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pcLogIn   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pcURL     AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iohSocket AS HANDLE NO-UNDO.

DEFINE OUTPUT PARAMETER ocError   AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
pcRequest = RIGHT-TRIM(RIGHT-TRIM(pcRequest),",").

ocError = "Failed!".

IF NOT VALID-HANDLE(iohSocket) THEN CREATE SOCKET iohSocket.

IF NOT iohSocket:CONNECTED() THEN DO:
   
   iohSocket:CONNECT(pcURL) NO-ERROR.

   IF NOT iohSocket:CONNECTED() THEN DO:
      ocError = "ERROR: Connection failure.".
      RETURN ocError.
   END.   

   iohSocket:SET-READ-RESPONSE-PROCEDURE('pGetServerResponce').

   /* 1: login */
   RUN pPostRequest(pcLogIn + CHR(10), iohSocket).

   WAIT-FOR READ-RESPONSE OF iohSocket.

   lcResponse = RETURN-VALUE.

   /* 2: if login succesful post request */
   IF NOT lcResponse = "OK LOGIN" + CHR(10) THEN RETURN lcResponse. 

END.

iohSocket:SET-READ-RESPONSE-PROCEDURE('pGetServerResponce').

/* 2: send request */
RUN pPostRequest(pcRequest + CHR(10), iohSocket).

WAIT-FOR READ-RESPONSE OF iohSocket.

ASSIGN lcResponse = RETURN-VALUE.

RETURN lcResponse.

/* Responsible for reading server response */
PROCEDURE pGetServerResponce:

   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lmResponse AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE llRC       AS LOGICAL   NO-UNDO.

   IF iohSocket:CONNECTED() = FALSE THEN DO:
      ocError = "ERROR: Connection failure.".
      RETURN ocError.
   END.
   
   SET-SIZE(lmResponse) = 0.

   SET-SIZE(lmResponse) = iohSocket:GET-BYTES-AVAILABLE() + 1.
   SET-BYTE-ORDER(lmResponse) = BIG-ENDIAN.
   llRC = iohSocket:READ(lmResponse,1,iohSocket:GET-BYTES-AVAILABLE() + 1,1).

   lcResponse = lcResponse + GET-STRING(lmResponse,1).
   ocError    = lcResponse.
   SET-SIZE(lmResponse) = 0.
   
   RETURN lcResponse.

END PROCEDURE.

/* make a request and post the xml document */
PROCEDURE pPostRequest:

   DEFINE INPUT PARAMETER pcRequest AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ihSocket  AS HANDLE    NO-UNDO.
   
   DEFINE VARIABLE liRequest   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liMsg       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lmRequest   AS MEMPTR    NO-UNDO.

   ASSIGN
      liRequest   = LENGTH(pcRequest)
      liMsg       = liRequest.

   SET-SIZE(lmRequest) = 0.
   SET-SIZE(lmRequest) = liMsg.
   SET-BYTE-ORDER(lmRequest) = BIG-ENDIAN.

   PUT-STRING(lmRequest,1,liMsg) = pcRequest.

   ihSocket:WRITE(lmRequest,1,liMsg) NO-ERROR.

   SET-SIZE(lmRequest) = 0.

   IF ERROR-STATUS:ERROR THEN RETURN "ERROR: Write failure.".

END PROCEDURE.
