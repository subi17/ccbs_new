/* XMLRPC framework library
 * This library provides the basic operations for handling xmlrpc messages,
 * i.e. parsing, serializing, error responses and data storage.
 * It will be included by server and client applications.
 * Test applications don't use the parse/serialize part and define
 *   the error/storage part independently again.
 *
 * In more detail, this library exports:
 *   temp_tables tt_param and tt_lastpos, which hold the data
 *   global error macros, e.g. &NOT_WELLFORMED_ERROR
 *   global error variables (gi_xmlrpc_error, gc_xmlrpc_error)
 *   global in/out direction variables get_from_tt, add_to_tt
 *   FUNCTION std_error_description (INT) RETURN CHAR
 *   FUNCTION parse (LONGCHAR)
 *   FUNCTION serialize ( parent, 
 *   FUNCTION xmlrpc_initialize(LOGICAL)
                                    (must be called once before anything else)
 *   FUNCTION xmlrpc_cleanup        (resets everything to start values)
 */

&GLOBAL-DEFINE NOT_WELLFORMED_ERROR     -32700
&GLOBAL-DEFINE UNSUPPORTED_ENCODING     -32701
&GLOBAL-DEFINE INVALID_ENCODING_CHAR    -32702
&GLOBAL-DEFINE INVALID_XMLRPC           -32600
&GLOBAL-DEFINE METHOD_NOT_FOUND         -32601
&GLOBAL-DEFINE INVALID_METHOD_PARAMS    -32602
&GLOBAL-DEFINE INTERNAL_ERROR           -32603
&GLOBAL-DEFINE APPLICATION_ERROR        -32500
&GLOBAL-DEFINE SYSTEM_ERROR             -32400
&GLOBAL-DEFINE TRANSPORT_ERROR          -32300

FUNCTION std_error_description RETURNS CHAR ( picode AS INT ):
    DEF VAR lctext AS CHAR NO-UNDO.
    CASE picode:
      WHEN {&NOT_WELLFORMED_ERROR} THEN
        lctext = "Parser error: Not well formed".
      WHEN {&UNSUPPORTED_ENCODING} THEN
        lctext = "Parser error: Unsupported encoding".
      WHEN {&INVALID_ENCODING_CHAR} THEN
        lctext = "Parser error: Invalid character for encoding".
      WHEN {&INVALID_XMLRPC} THEN
        lctext = "Server error: Invalid xmlrpc request".
      WHEN {&METHOD_NOT_FOUND} THEN
        lctext = "Server error: Method not found".
      WHEN {&INVALID_METHOD_PARAMS} THEN
        lctext = "Server error: Wrong number of parameters".
      WHEN {&INTERNAL_ERROR} THEN
        lctext = "Server error: Internal error".
      WHEN {&APPLICATION_ERROR} THEN
        lctext = "Application error".
      WHEN {&SYSTEM_ERROR} THEN
        lctext = "System error".
      WHEN {&TRANSPORT_ERROR} THEN
        lctext = "Transport error".
    END CASE.
    RETURN lctext.
END FUNCTION.

&IF DEFINED(SERIALIZE_ONLY) = 0 &THEN
    &SCOPED-DEFINE RPC_VAR_SCOPE NEW GLOBAL SHARED
&ENDIF

&GLOBAL-DEFINE parse_in 0
&GLOBAL-DEFINE serialize_out 1
/* These two variables define, on which section the get_* and add_* methods
 * act. Usually we get from the parsed and add into the serializable
 * section. Tests work the other way around, as they first put into the
 * the request and the assertions get from the response, which would
 * normally be serialized. */
DEF {&RPC_VAR_SCOPE} VAR get_from_tt AS INT INITIAL {&parse_in} NO-UNDO.
DEF {&RPC_VAR_SCOPE} VAR add_to_tt AS INT INITIAL {&serialize_out} NO-UNDO.

DEFINE {&RPC_VAR_SCOPE} TEMP-TABLE tt_param
    FIELD inout AS INT
    FIELD position AS INT
    FIELD parent AS CHAR INITIAL ?
    FIELD name AS CHAR
    FIELD type AS CHAR
    FIELD cvalue AS CHAR
    INDEX gi IS PRIMARY UNIQUE inout parent position.

DEFINE {&RPC_VAR_SCOPE} TEMP-TABLE tt_lastpos
    FIELD inout AS INT
    FIELD parent AS CHAR INITIAL ?
    FIELD lastposition AS INT
    INDEX gi IS PRIMARY UNIQUE inout parent.

/* These could change for multicalls. Otherwise they are trivially empty.  */
DEF {&RPC_VAR_SCOPE} VAR param_toplevel_id AS CHAR INITIAL "" NO-UNDO.
DEF {&RPC_VAR_SCOPE} VAR response_toplevel_id AS CHAR INITIAL "" NO-UNDO.

DEF {&RPC_VAR_SCOPE} VAR gi_xmlrpc_error AS INT INITIAL 0 NO-UNDO.
DEF {&RPC_VAR_SCOPE} VAR gc_xmlrpc_error AS CHAR INITIAL "" NO-UNDO.

/* Needed by the server only, but must be defined global shared for the
 * parser callback. */
DEFINE {&RPC_VAR_SCOPE} VARIABLE gcMethodName AS CHAR NO-UNDO.

DEFINE VARIABLE ghServer AS HANDLE.

/* These handles will hold the SAX objects and are reused, thus global. */
DEFINE VARIABLE ghParser AS HANDLE.
DEFINE VARIABLE ghSerializer AS HANDLE.
DEFINE VARIABLE ghCallback AS HANDLE.

/* Initialize all handles, load the sax-callback and populate the
 * xml-parser productions table.
 * The parameter defines, whether the parser should expect requests
 * (i.e. we are the server) or responses (i.e. we are the client). */
FUNCTION xmlrpc_initialize RETURN LOGICAL
      ( plServer AS LOGICAL ):
    &IF DEFINED(SERIALIZE_ONLY) = 0 &THEN
    CREATE SAX-READER ghParser. 
    RUN saxhandler PERSISTENT SET ghCallback.
    ghParser:HANDLER = ghCallback .
    RUN init_production_temptable IN ghCallback (plServer).
    &ENDIF
    CREATE SAX-WRITER ghSerializer.
    RETURN TRUE.
END FUNCTION.

FUNCTION xmlrpc_cleanup RETURN LOGICAL ():
    FOR EACH tt_param EXCLUSIVE-LOCK: DELETE tt_param. END.
    FOR EACH tt_lastpos EXCLUSIVE-LOCK: DELETE tt_lastpos. END.
    gi_xmlrpc_error = 0.
    gc_xmlrpc_error = "".
    param_toplevel_id = "".
    response_toplevel_id = "".
    &IF DEFINED(SERIALIZE_ONLY) = 0 &THEN
    IF VALID-HANDLE(ghCallback) THEN RUN cleanup IN ghCallback.
    &ENDIF
END FUNCTION.

FUNCTION xmlrpc_finalize RETURNS LOGICAL ():
   xmlrpc_cleanup().
   &IF DEFINED(SERIALIZE_ONLY) = 0 &THEN
   IF VALID-HANDLE(ghCallback) THEN DELETE PROCEDURE ghCallback NO-ERROR. 
   IF VALID-HANDLE(ghParser) THEN DELETE OBJECT ghParser NO-ERROR. 
   IF VALID-HANDLE(ghServer) THEN DO:
      ghServer:DISCONNECT() NO-ERROR.
      DELETE OBJECT ghServer.
   END.
   &ENDIF
   IF VALID-HANDLE(ghSerializer) THEN DELETE OBJECT ghSerializer NO-ERROR. 
   RETURN TRUE.
END FUNCTION. 

&IF DEFINED(SERIALIZE_ONLY) = 0 &THEN
/* parsing */

/* This function parses the given data and puts it into the temp_table.
 * Returns TRUE on successful parse, FALSE on error during parse.
 * The return value is also TRUE, when the data contains a xmlrpc fault.
 * For the server, this has the side effect of setting gcMethodName.
 */
FUNCTION parse RETURN LOGICAL ( pcData AS LONGCHAR ):
    RUN cleanup IN ghCallback.
    ghParser:SET-INPUT-SOURCE("longchar", pcData).
    ghParser:SAX-PARSE( ) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        gi_xmlrpc_error = {&INTERNAL_ERROR}.
        gc_xmlrpc_error = ERROR-STATUS:GET-MESSAGE(1).
        RETURN FALSE.
    END.
    RETURN TRUE.
END FUNCTION.
&ENDIF

/* serialization */

FUNCTION _json_datetime RETURN CHAR
      ( cval AS CHAR ):
    RETURN '\{"json_class":"DateTime",' +
             '"y":' + SUBSTRING(cval,1,4) + ',' +
             '"m":' + STRING(INT(SUBSTRING(cval,5,2))) + ',' +
             '"d":' + STRING(INT(SUBSTRING(cval,7,2))) + ',' +
             '"H":' + STRING(INT(SUBSTRING(cval,10,2))) + ',' +
             '"M":' + STRING(INT(SUBSTRING(cval,13,2))) + ',' +
             '"S":' + STRING(INT(SUBSTRING(cval,16,2))) + ',' +
             '"of":"0/1"' +
           '}'.
END FUNCTION.

FUNCTION _serialize_json RETURN CHAR
      ( pparent AS CHAR ):
    DEF BUFFER lbResp FOR tt_param.
    DEF VAR lcResult AS CHAR NO-UNDO.

    FOR EACH lbResp
    WHERE lbResp.inout EQ {&serialize_out}
      AND lbResp.parent = pparent
    BREAK BY lbResp.parent BY lbResp.position:
        IF lbResp.name NE "" AND lbResp.name NE ? THEN DO:
            lcResult = lcResult + '"' + lbResp.name + '":'.
        END.
        IF lbResp.type EQ "array" THEN DO:
            lcResult = lcResult + "[" + _serialize_json(
                    lbResp.parent + ',' + STRING(lbResp.position)) + "]".
        END. ELSE IF lbResp.type EQ "struct" THEN DO:
            lcResult = lcResult + "\{" + _serialize_json(
                    lbResp.parent + ',' + STRING(lbResp.position)) + "}".
        END. ELSE IF lbResp.type EQ "string" THEN DO:
            lcResult = lcResult + '"' + REPLACE(REPLACE(lbResp.cvalue,
                                        '"', '\\"'), "\n", "~/n") + '"'.
        END. ELSE IF lbResp.type EQ "boolean" THEN DO:
            lcResult = lcResult + STRING(lbResp.cvalue EQ "1", "true/false").
        END. ELSE IF lbResp.type EQ "dateTime.iso8601" THEN DO:
            lcResult = lcResult + _json_datetime(lbResp.cvalue).
        END. ELSE DO:
            lcResult = lcResult + lbResp.cvalue.
        END.
        IF NOT LAST-OF(lbResp.parent) THEN
            lcResult = lcResult + ", ".
    END.

    RETURN lcResult.
END FUNCTION.

FUNCTION _serialize RETURN LOGICAL
      ( pparent AS CHAR,
        pResult AS HANDLE ):
    DEF BUFFER lbResp FOR tt_param.
    DEF VAR lcJsonChunk AS CHAR NO-UNDO.
    DEF VAR lmpFileContents AS MEMPTR NO-UNDO.

    FOR EACH lbResp
    WHERE lbResp.inout EQ {&serialize_out}
      AND lbResp.parent = pparent
    BREAK BY lbResp.position:
        IF pparent EQ "" THEN
            pResult:START-ELEMENT("param").
        ELSE IF lbResp.name NE "" AND lbResp.name NE ? THEN DO:
            pResult:START-ELEMENT("member").
            pResult:WRITE-DATA-ELEMENT("name", lbResp.name).
        END.
        pResult:START-ELEMENT("value").
        IF lbResp.type BEGINS "json_" THEN DO:
            lcJsonChunk = _serialize_json(
                            lbResp.parent + ',' + STRING(lbResp.position)).
            IF lbResp.type EQ "json_array" THEN
                lcJsonChunk = SUBST("[&1]", lcJsonChunk).
            ELSE
                lcJsonChunk = SUBST("\{&1}", lcJsonChunk).
            pResult:WRITE-DATA-ELEMENT("string", lcJsonChunk).
        END. ELSE DO:
            pResult:START-ELEMENT(lbResp.type).
            IF lbResp.type EQ "array" THEN DO:
               pResult:START-ELEMENT("data").
               _serialize(lbResp.parent + ',' + STRING(lbResp.position),
                                    pResult).
               pResult:END-ELEMENT("data").
            END. ELSE IF lbResp.type EQ "struct" THEN DO:
               _serialize(lbResp.parent + ',' + STRING(lbResp.position),
                                    pResult).
            END. ELSE IF lbResp.type EQ "base64" THEN DO:
               COPY-LOB FROM FILE lbResp.cvalue TO lmpFileContents.
               pResult:WRITE-CHARACTERS(BASE64-ENCODE(lmpFileContents)).
               SET-SIZE(lmpFileContents) = 0.
            END. ELSE
                pResult:WRITE-CHARACTERS(lbResp.cvalue).
            pResult:END-ELEMENT(lbResp.type).
        END.
        pResult:END-ELEMENT("value").
        IF pparent EQ "" THEN
            pResult:END-ELEMENT("param").
        ELSE IF lbResp.name NE "" AND lbResp.name NE ? THEN
            pResult:END-ELEMENT("member").
    END.

    RETURN TRUE.
END FUNCTION.

FUNCTION serialize_response RETURN LONGCHAR ():
    DEF VAR lcResult AS LONGCHAR NO-UNDO.
    ghSerializer:RESET().
    ghSerializer:SET-OUTPUT-DESTINATION("longchar", lcResult).
    ghSerializer:START-DOCUMENT().
    ghSerializer:START-ELEMENT("methodResponse").
    ghSerializer:START-ELEMENT("params").
    _serialize("", ghSerializer).
    ghSerializer:END-ELEMENT("params").
    ghSerializer:END-ELEMENT("methodResponse").
    ghSerializer:END-DOCUMENT().
    RETURN lcResult.
END FUNCTION.

FUNCTION serialize_rpc_call RETURN LONGCHAR ( pcMethodName AS CHAR ):
    DEF VAR lcResult AS LONGCHAR NO-UNDO.
    ghSerializer:RESET().
    ghSerializer:SET-OUTPUT-DESTINATION("longchar", lcResult).
    ghSerializer:START-DOCUMENT().
    ghSerializer:START-ELEMENT("methodCall").
    ghSerializer:WRITE-DATA-ELEMENT("methodName", pcMethodName).
    ghSerializer:START-ELEMENT("params").
    _serialize("", ghSerializer).
    ghSerializer:END-ELEMENT("params").
    ghSerializer:END-ELEMENT("methodCall").
    ghSerializer:END-DOCUMENT().
    RETURN lcResult.
END FUNCTION.

/* This function is also used by multicall, where this fragment is just
 * the response to one (of possibly many) calls. */
FUNCTION xmlrpc_error_frame RETURNS LOGICAL
      ( picode AS INT,
        pctext AS CHAR,
        ghSerializer AS HANDLE ):
    
    pcText = REPLACE(pcText, "&", "&amp;").
    pcText = REPLACE(pcText, "<", "&lt;").
    pcText = REPLACE(pcText, ">", "&gt;"). 
    
    ghSerializer:WRITE-FRAGMENT(SUBST(
           "<fault>~n" +
           "    <value><struct>~n" +
           "        <member>~n" +
           "            <name>faultCode</name>~n" +
           "            <value><int>&1</int></value>~n" +
           "            </member>~n" +
           "        <member>~n" +
           "            <name>faultString</name>~n" +
           "            <value><string>&2</string></value>~n" +
           "            </member>~n" +
           "        </struct></value>~n" +
           "</fault>",
    picode, pctext)).
    RETURN TRUE.
END FUNCTION.

FUNCTION serialize_error RETURN LONGCHAR ():
    DEF VAR lcResult AS LONGCHAR NO-UNDO.
    IF gc_xmlrpc_error EQ ? OR gc_xmlrpc_error EQ "" THEN
        gc_xmlrpc_error = std_error_description(gi_xmlrpc_error).
    ghSerializer:RESET().
    ghSerializer:SET-OUTPUT-DESTINATION("longchar", lcResult).
    ghSerializer:START-DOCUMENT().
    ghSerializer:START-ELEMENT("methodResponse").
    xmlrpc_error_frame(gi_xmlrpc_error, gc_xmlrpc_error, ghSerializer).
    ghSerializer:END-ELEMENT("methodResponse").
    ghSerializer:END-DOCUMENT().
    RETURN lcResult.
END FUNCTION.
