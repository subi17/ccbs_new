/* XMLRPC client library. */
/* Duplicate code? This file might be both part of an tmsrpc installation
 * or copied into the tms source tree. */
/* TODO: SSL support probably does not work. Apparently the certificate
 * must be downloaded separately and installed on the client. */

&IF "{&XMLRPC_CLIENT_I}" NE "YES" 
&THEN
&GLOBAL-DEFINE XMLRPC_CLIENT_I YES

{xmlrpc/xmlrpc.i}
{xmlrpc/xmlrpc_access.i &TOGETHER=1}

&SCOPED-DEFINE CLIENT_VERSION 0.1

DEFINE VARIABLE gcXmlrpcServerData AS CHAR EXTENT 6 NO-UNDO.
                /* 1: host, 2: port, 3: path, 4: ssl, 5: timeout, 6: user-agent */
DEFINE VARIABLE glKeepAlive AS LOGICAL NO-UNDO. 

FUNCTION initialize RETURN LOGICAL ( pcURL AS CHAR, timeout AS INT ):
    glKeepAlive = FALSE.
    gcXmlrpcServerData[6] = "TMSRPC/" + "{&CLIENT_VERSION}".
    gcXmlrpcServerData[5] = STRING(timeout).
    gcXmlrpcServerData[2] = "80".
    IF pcURL BEGINS "http://" THEN
        pcURL = SUBSTRING(pcURL, 8).
    ELSE IF pcURL BEGINS "https://" THEN DO:
        gcXmlrpcServerData[2] = "443".
        gcXmlrpcServerData[4] = "ssl".
        pcURL = SUBSTRING(pcURL, 9).
    END. ELSE IF pcURL MATCHES "*://*" THEN
        RETURN FALSE.

    IF NUM-ENTRIES(pcURL, "/") GE 2 THEN DO:
        gcXmlrpcServerData[3] = SUBSTRING(pcURL, INDEX(pcURL, "/") + 1).
        pcURL = ENTRY(1, pcURL, "/").
    END.
    gcXmlrpcServerData[3] = "/" + gcXmlrpcServerData[3].

    IF NUM-ENTRIES(pcURL, ":") EQ 2 THEN DO:
        gcXmlrpcServerData[2] = ENTRY(2, pcURL, ":").
        pcURL = ENTRY(1, pcURL, ":").
    END.
    gcXmlrpcServerData[1] = pcURL.

    IF VALID-HANDLE(ghServer) THEN DO:
       ghServer:DISCONNECT() NO-ERROR. 
       DELETE OBJECT ghServer.
    END.
    CREATE SOCKET ghServer.
    ghServer:SET-READ-RESPONSE-PROCEDURE('pServerResponse') NO-ERROR.
    RETURN xmlrpc_initialize(FALSE).
END FUNCTION.

FUNCTION set_user-agent RETURN LOGICAL ( pcuser-agent AS CHAR) :
     gcXmlrpcServerData[6] = pcuser-agent.
     RETURN TRUE.
END FUNCTION.

FUNCTION _connect_to_server RETURN LOGICAL ():
    IF gcXmlrpcServerData[4] EQ "ssl" THEN
        ghServer:CONNECT(SUBST("-H &1 -S &2 -ssl -nohostverify",
                                             gcXmlrpcServerData[1],
                                             gcXmlrpcServerData[2])) NO-ERROR.
    ELSE
        ghServer:CONNECT(SUBST("-H &1 -S &2", gcXmlrpcServerData[1],
                                              gcXmlrpcServerData[2])) NO-ERROR.
    ghServer:SET-SOCKET-OPTION("SO-RCVTIMEO", gcXmlrpcServerData[5]) NO-ERROR.
    RETURN ghServer:CONNECTED().
END FUNCTION.

FUNCTION _transp_err RETURN LOGICAL ( pcText AS CHAR ):
    gi_xmlrpc_error = {&TRANSPORT_ERROR}.
    gc_xmlrpc_error = pcText.
    RETURN FALSE.
END FUNCTION.

FUNCTION _send_to_server RETURN LOGICAL
      ( pcRequest AS LONGCHAR):
    DEF VAR lcHttpHead AS CHAR NO-UNDO.
    DEF VAR liContentLength AS INT NO-UNDO.
    DEF VAR lmBuf AS MEMPTR NO-UNDO.
    liContentLength = LENGTH(pcRequest).
    lcHttpHead = SUBST("POST &1 HTTP/1.0~r~n" +
                       "User-Agent: &2~r~n" +
                       "Host: &3~r~n" +
                       "Connection: &4~r~n" +
                       "Content-Type: text/xml~r~n" +
                       "Content-length: &5~r~n~r~n",
                                   gcXmlrpcServerData[3],
                                   gcXmlrpcServerData[6],
                                   gcXmlrpcServerData[1],
                                   STRING(glKeepAlive, "keep-alive/close"),
                                   liContentLength).
    liContentLength = liContentLength + LENGTH(lcHttpHead).
    SET-SIZE(lmBuf) = liContentLength + 1.
    PUT-STRING(lmBuf, 1) = lcHttpHead.
    PUT-STRING(lmBuf, LENGTH(lcHttpHead) + 1) = pcRequest.
    ghServer:WRITE(lmBuf, 1, liContentLength) NO-ERROR.
    SET-SIZE(lmBuf) = 0.
    IF ghServer:BYTES-WRITTEN NE liContentLength THEN
        RETURN _transp_err("Error writing the full request").
    RETURN TRUE.
END FUNCTION.

FUNCTION _receive_from_server RETURN LOGICAL
      ( OUTPUT pcResponse AS LONGCHAR):
    DEF VAR lmBuf AS MEMPTR NO-UNDO.
    DEF VAR liNextNewline AS INT NO-UNDO.
    DEF VAR lcRawData AS CHAR NO-UNDO.
    DEF VAR lcc AS CHAR NO-UNDO.
    DEF VAR lii AS INT NO-UNDO.
    DEF VAR liContentLength AS INT NO-UNDO.
    DEF VAR liTimeout AS INT NO-UNDO.

    liTimeout = INT(gcXmlrpcServerData[5]).
    /* This loop fetches only small parts of the response and parses the
     * HTTP header. The loop will end on error (syntax, timout) or when
     * a newline was received. In the latter case, lcRawData contains the
     * start (or possibly all) of the data body. */
    SET-SIZE(lmBuf) = 4092. /* For parsing HTTP Header only */
    read-loop: REPEAT:
        IF NOT ghServer:CONNECTED() THEN DO:
            _transp_err("Connection lost while reading HTTP header").
            LEAVE read-loop.
        END.
        IF ghServer:GET-BYTES-AVAILABLE() EQ 0 THEN DO:
            _transp_err("Timeout while reading the HTTP header").
            LEAVE read-loop.
        END.
        ghServer:READ(lmBuf, 1, MIN(4092, ghServer:GET-BYTES-AVAILABLE())).
        IF ghServer:BYTES-READ EQ 0 THEN DO:
            _transp_err("Read error").
            LEAVE read-loop.
        END.
        lcRawData = lcRawData + GET-STRING(lmBuf, 1).
        liNextNewline = 0.
        DO lii = 1 TO NUM-ENTRIES(lcRawData, "~n") - 1:
            lcc = ENTRY(lii, lcRawData, "~n").
            liNextNewline = liNextNewline + LENGTH(lcc) + 1.
            lcc = TRIM(lcc).
            IF lcc EQ "" THEN DO:
                lcRawData = SUBSTRING(lcRawData, liNextNewline + 1).
                LEAVE read-loop.
            END.
            IF lcc BEGINS "HTTP" THEN DO:
                IF NOT ENTRY(2, lcc, " ") BEGINS "20" THEN DO:
                    gi_xmlrpc_error = INT(ENTRY(2, lcc, " ")) NO-ERROR.
                    gc_xmlrpc_error = SUBSTRING(lcc, INDEX(lcc, " ", 11) + 1).
                    LEAVE read-loop.
                END.
            END.
            IF lcc EQ "Connection: close" THEN
                glKeepAlive = FALSE.
            IF lcc BEGINS "Content-Length:" THEN DO:
                liContentLength = INT(TRIM(ENTRY(2, lcc, " "))) NO-ERROR.
                IF liContentLength EQ 0 OR liContentLength EQ ? THEN DO:
                    _transp_err("Invalid Content-Length " + ENTRY(2, lcc, " ")).
                    LEAVE read-loop.
                END.
            END.
        END.
        lcRawData = SUBSTRING(lcRawData, liNextNewline + 1).
    END.
    SET-SIZE(lmBuf) = 0.
    IF gi_xmlrpc_error NE 0 THEN RETURN FALSE.

    /* The Header is read, but more data might come. Note that the Server
     * does not necessarily send the Content-Length header. Unfortunately
     * this happens especially for large responses. We'll write it into
     * a temporary file in this case. */
    lii = LENGTH(lcRawData).
    IF liContentLength GT 0 THEN DO:            /* Content length given */
        IF lii LT liContentLength THEN DO:
            IF NOT ghServer:CONNECTED() THEN DO:
                _transp_err("Connection lost before content-length read").
                RETURN FALSE.
            END.
            SET-SIZE(lmBuf) = liContentLength.
            PUT-STRING(lmBuf, 1) = lcRawData.
            ghServer:READ(lmBuf, lii + 1, liContentLength - lii).
            IF GET-SIZE(lmBuf) LT liContentLength THEN
                _transp_err("Timeout before content-length read").
            ELSE
                COPY-LOB FROM lmBuf TO pcResponse.
            SET-SIZE(lmBuf) = 0.
        END. ELSE
            pcResponse = lcRawData.
    END. ELSE IF ghServer:GET-BYTES-AVAILABLE() GT 0 THEN DO:
                                              /* Not given and data pending */
        DEF VAR lcTempFile AS CHAR NO-UNDO.
        lcTempFile = SESSION:TEMP-DIRECTORY + "/xmlrpc_response." +
                                                STRING(random(1000, 9999)).
        pcResponse = lcRawData.
        COPY-LOB FROM pcResponse TO FILE lcTempFile.
        chunk-loop: REPEAT:         /* Read 50k chunks into file */
            IF NOT ghServer:CONNECTED() THEN DO:
                _transp_err("Connection lost while reading body").
                LEAVE chunk-loop.
            END.
            SET-SIZE(lmBuf) = 50000.
            ghServer:READ(lmBuf, 1, min(50000, ghServer:GET-BYTES-AVAILABLE())).
            COPY-LOB FROM lmBuf FOR ghServer:BYTES-READ
                     TO FILE lcTempFile APPEND.
            lcc = GET-STRING(lmBuf, ghServer:BYTES-READ - 16).
            SET-SIZE(lmBuf) = 0.
            IF lcc EQ "</methodResponse>" THEN
                LEAVE chunk-loop.
            REPEAT:
                IF ghServer:GET-BYTES-AVAILABLE() GT 0 THEN LEAVE.
                IF liTimeOut EQ 0 THEN LEAVE chunk-loop.
                PAUSE(1).
                liTimeOut = liTimeOut - 1.
            END.
        END.
        COPY-LOB FROM FILE lcTempFile TO pcResponse.
        UNIX rm VALUE(lcTempFile).
    END.
    RETURN (gi_xmlrpc_error EQ 0).
END FUNCTION.

FUNCTION run_rpc_method RETURN LOGICAL
      ( pcMethodName AS CHAR,
        plKeepAlive AS LOGICAL ):
    DEF VAR lcRequest AS LONGCHAR NO-UNDO.
    DEF VAR lcResponse AS LONGCHAR NO-UNDO.
    DEF VAR liTimeout AS INTEGER NO-UNDO. 
    IF plKeepAlive EQ ? THEN glKeepAlive = FALSE.
    lcRequest = serialize_rpc_call(pcMethodName).
    IF NOT ghServer:CONNECTED() AND NOT _connect_to_server() THEN
        RETURN _transp_err("Could not connect to server").
    IF NOT _send_to_server(lcRequest) THEN RETURN FALSE.
    /* wait until get response */
    liTimeout = INT(gcXmlrpcServerData[5]).
    REPEAT:
            IF ghServer:GET-BYTES-AVAILABLE() GT 0 THEN LEAVE.
            IF liTimeOut EQ 0 THEN LEAVE.
            PAUSE(1).
            liTimeOut = liTimeOut - 1.
    END.
    IF NOT _receive_from_server(OUTPUT lcResponse) THEN
        RETURN FALSE.
    IF NOT glKeepAlive THEN ghServer:DISCONNECT().

    IF NOT parse(lcResponse) THEN
        RETURN _transp_err("Error parsing the response").

    RETURN TRUE.
END FUNCTION. 

/* needed because saxparser won't work in orcus */
FUNCTION run_rpc_method_without_parse RETURN LOGICAL
      ( pcMethodName AS CHAR,
        plKeepAlive AS LOGICAL,
        OUTPUT pcResponse AS LONGCHAR):
    DEF VAR lcRequest AS LONGCHAR NO-UNDO.
    DEF VAR lcResponse AS LONGCHAR NO-UNDO.
    DEF VAR liTimeOut AS INTEGER NO-UNDO. 
    IF plKeepAlive EQ ? THEN glKeepAlive = FALSE.
    lcRequest = serialize_rpc_call(pcMethodName).
    IF NOT ghServer:CONNECTED() AND NOT _connect_to_server() THEN
        RETURN _transp_err("Could not connect to server").
    IF NOT _send_to_server(lcRequest) THEN RETURN FALSE.
    /* wait until get response */
    liTimeout = INT(gcXmlrpcServerData[5]).
    REPEAT:
            IF ghServer:GET-BYTES-AVAILABLE() GT 0 THEN LEAVE.
            IF liTimeOut EQ 0 THEN LEAVE.
            PAUSE(1).
            liTimeOut = liTimeOut - 1.
    END.
    IF NOT _receive_from_server(OUTPUT lcResponse) THEN
        RETURN FALSE.
    IF NOT glKeepAlive THEN ghServer:DISCONNECT().

    pcResponse = lcResponse.

    RETURN TRUE. 
END FUNCTION.

FUNCTION run_rpc_method_without_serializing RETURN LOGICAL
    (  plKeepAlive AS LOGICAL,
       pcRequest AS LONGCHAR,
       plParse AS LOGICAL,
       OUTPUT ocResponse AS LONGCHAR):
    
    DEF VAR liTimeOut AS INTEGER NO-UNDO. 
    IF plKeepAlive EQ ? THEN glKeepAlive = FALSE.
    IF NOT ghServer:CONNECTED() AND NOT _connect_to_server() THEN
        RETURN _transp_err("Could not connect to server").
    IF NOT _send_to_server(pcRequest) THEN RETURN FALSE.
    /* wait until get response */
    liTimeout = INT(gcXmlrpcServerData[5]).
    REPEAT:
            IF ghServer:GET-BYTES-AVAILABLE() GT 0 THEN LEAVE.
            IF liTimeOut EQ 0 THEN LEAVE.
            PAUSE(1).
            liTimeOut = liTimeOut - 1.
    END. 
    IF NOT _receive_from_server(OUTPUT ocResponse) THEN
        RETURN FALSE.
    IF NOT plKeepAlive THEN ghServer:DISCONNECT().
    
    IF plParse AND NOT parse(ocResponse) THEN
        RETURN _transp_err("Error parsing the response").
    RETURN TRUE. 
END FUNCTION.

PROCEDURE pRPCMethodCall:

   DEFINE INPUT PARAMETER pcMethodName AS CHAR.
   DEFINE INPUT PARAMETER plKeepAlive AS LOGICAL.

   DEFINE VARIABLE liTimeOut AS INTEGER NO-UNDO.  
   DEFINE VARIABLE lcRequest AS LONGCHAR NO-UNDO.
   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO. 

   IF plKeepAlive EQ ? THEN glKeepAlive = FALSE.
   lcRequest = serialize_rpc_call(pcMethodName).

   IF NOT ghServer:CONNECTED() AND NOT _connect_to_server() THEN DO:
      _transp_err("Could not connect to server").
      RETURN "Error could not connect to server".
   END.

   IF NOT _send_to_server(lcRequest) THEN RETURN "Error sending to server".

   liTimeOut = INT(gcXmlrpcServerData[5]).
   IF NOT ghServer:CONNECTED() THEN DO:
      _transp_err("Could not connect to server").
      RETURN "Error could not connect to server".
   END.

   WAIT-FOR READ-RESPONSE OF ghServer PAUSE liTimeOut.
 
   IF (LAST-EVENT:CODE = -1) THEN DO:
      _transp_err("Response timeout").
      RETURN "Response timeout".
   END.

   lcResponse = RETURN-VALUE.

   RETURN lcResponse.    
END PROCEDURE.

PROCEDURE pServerResponse: 
   DEFINE VARIABLE lcResponse AS LONGCHAR NO-UNDO. 

   IF NOT _receive_from_server(OUTPUT lcResponse) 
      THEN RETURN "Error receiving from server ".
  
   IF NOT glKeepAlive THEN ghServer:DISCONNECT().
 
   IF NOT parse(lcResponse) THEN DO:
     _transp_err("Error parsing the response").
     RETURN "Error parsing the response".
   END.
   
   RETURN STRING(lcResponse). 
END PROCEDURE.
&ENDIF
