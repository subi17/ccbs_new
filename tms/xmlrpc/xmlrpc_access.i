&IF "{&XMLRPC_ACCESS_I}" NE "YES" 
&THEN
&GLOBAL-DEFINE XMLRPC_ACCESS_I YES

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

&IF DEFINED(NOTSHARED) = 0 &THEN
    &SCOPED-DEFINE SHARED SHARED
&ENDIF

&GLOBAL-DEFINE nowTS (YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY) + TIME / 100000)
&GLOBAL-DEFINE nowDT DATETIME(TODAY, MTIME)

&IF DEFINED(TOGETHER) = 0 &THEN

/* Alternative: Give whole table as parameters. 
 * I tried only the handle-way and that's much to complicated,
 * but apparently you can give temp-tables as parameters, too. */
DEFINE {&SHARED} TEMP-TABLE tt_param
    FIELD inout AS INT
    FIELD position AS INT
    FIELD parent AS CHAR INITIAL ?
    FIELD name AS CHAR
    FIELD type AS CHAR
    FIELD cvalue AS CHAR
    INDEX gi IS PRIMARY UNIQUE inout parent position.

DEF {&SHARED} VAR get_from_tt AS INT NO-UNDO.
DEF {&SHARED} VAR add_to_tt AS INT NO-UNDO.
DEF {&SHARED} VAR param_toplevel_id AS CHAR NO-UNDO.
DEF {&SHARED} VAR response_toplevel_id AS CHAR NO-UNDO.
DEF {&SHARED} VAR gi_xmlrpc_error AS INT NO-UNDO.
DEF {&SHARED} VAR gc_xmlrpc_error AS CHAR NO-UNDO.

DEFINE {&SHARED} TEMP-TABLE tt_lastpos
    FIELD inout AS INT
    FIELD parent AS CHAR INITIAL ?
    FIELD lastposition AS INT
    INDEX gi IS PRIMARY UNIQUE inout parent.

&ENDIF

FUNCTION int_err RETURN CHAR
      ( pctext AS CHAR ):
    gi_xmlrpc_error = {&INTERNAL_ERROR}.
    gc_xmlrpc_error = "Internal Error - please report".
    RETURN ERROR.
END FUNCTION.

FUNCTION appl_err RETURN CHAR
      ( pctext AS CHAR ):
    gi_xmlrpc_error = {&APPLICATION_ERROR}.
    gc_xmlrpc_error = pctext.
    RETURN ERROR.
END FUNCTION.

FUNCTION param_err RETURN CHAR
      ( pctext AS CHAR ):
    gi_xmlrpc_error = {&INVALID_METHOD_PARAMS}.
    gc_xmlrpc_error = pctext.
    RETURN ERROR.
END FUNCTION.


&IF DEFINED(NOTIMEINCLUDES) = 0 &THEN
{xmlrpc/timestamp.i}
&ENDIF

FUNCTION _get_next_position RETURN INT
      ( pparent AS CHAR ):
    DEF VAR result AS INT INITIAL 0 NO-UNDO.

    find tt_lastpos exclusive-lock
    where tt_lastpos.inout EQ add_to_tt
      AND tt_lastpos.parent = pparent NO-ERROR.
    IF AVAILABLE tt_lastpos THEN DO:
        result = tt_lastpos.lastposition.
        tt_lastpos.lastposition = result + 1.
        release tt_lastpos.
    END. ELSE DO:
        result = 0.
        CREATE tt_lastpos.
        tt_lastpos.inout = add_to_tt.
        tt_lastpos.parent = pparent.
        tt_lastpos.lastposition = 1.
    END.

    RETURN result.
END FUNCTION.

FUNCTION _add_value RETURN CHAR
      ( pparent AS CHAR,
	pname AS CHAR,
	ptype AS CHAR,
	pvalue AS CHAR ):
    DEF VAR liposition AS INT NO-UNDO.
    liposition = _get_next_position(pparent).
    IF ptype = "double" THEN
	pvalue = REPLACE(pvalue, ",", ".").
    IF ptype = "datetime" THEN
        ptype = "dateTime.iso8601".
    CREATE tt_param.
    ASSIGN
        inout = add_to_tt
	position = liposition
	parent = pparent
	name = pname
	type = ptype
	cvalue = pvalue.
    RETURN pparent + "," + STRING(liposition).
END FUNCTION.

FUNCTION add_int RETURN LOGICAL
      ( pparent AS CHAR,
        pname AS CHAR,
        pvalue AS INT):
    IF pvalue EQ ? THEN RETURN FALSE.
    _add_value(pparent, pname, "int", STRING(pvalue)).
    RETURN TRUE.
END.

FUNCTION add_double RETURN LOGICAL
      ( pparent AS CHAR,
        pname AS CHAR,
        pvalue AS DECIMAL):
    DEF VAR svalue AS CHAR NO-UNDO.
    IF pvalue EQ ? THEN RETURN FALSE.
    svalue = REPLACE(STRING(pvalue), SESSION:NUMERIC-DECIMAL-POINT, ".").
    IF SUBSTRING(svalue, 1, 1) EQ "." THEN
        svalue = "0" + svalue.
    _add_value(pparent, pname, "double", svalue).
    RETURN TRUE.
END.

FUNCTION add_string RETURN LOGICAL
      ( pparent AS CHAR,
        pname AS CHAR,
        pvalue AS CHAR):
    IF pvalue EQ ? THEN RETURN FALSE.
    pvalue = CODEPAGE-CONVERT(pvalue, "UTF-8", SESSION:CHARSET) NO-ERROR.
    _add_value(pparent, pname, "string", pvalue).
    RETURN TRUE.
END.

FUNCTION add_file RETURN LOGICAL
      ( pparent AS CHAR,
        pname AS CHAR,
        pfilename AS CHAR):
    IF SEARCH(pfilename) EQ ? THEN RETURN FALSE.
    _add_value(pparent, pname, "base64", SEARCH(pfilename)).
    RETURN TRUE.
END.

FUNCTION from_utf8 RETURN CHAR
      ( cval AS CHAR ):
    RETURN CODEPAGE-CONVERT(cval, SESSION:CHARSET, "UTF-8").
END.

FUNCTION add_boolean RETURN LOGICAL
      ( pparent AS CHAR,
        pname AS CHAR,
        pvalue AS LOGICAL):
    IF pvalue EQ ? THEN RETURN FALSE.
    _add_value(pparent, pname, "boolean", STRING(pvalue, "1/0")).
    RETURN TRUE.
END.

FUNCTION add_array RETURN CHAR
      ( pparent AS CHAR,
        pname AS CHAR ):
    RETURN _add_value(pparent, pname, "array", "").
END.

FUNCTION add_struct RETURN CHAR
      ( pparent AS CHAR,
        pname AS CHAR ):
    RETURN _add_value(pparent, pname, "struct", "").
END.

FUNCTION add_json_array RETURN CHAR
      ( pparent AS CHAR,
        pname AS CHAR ):
    RETURN _add_value(pparent, pname, "json_array", "").
END.

FUNCTION add_json_struct RETURN CHAR
      ( pparent AS CHAR,
        pname AS CHAR ):
    RETURN _add_value(pparent, pname, "json_struct", "").
END.

&IF DEFINED(NOTIMEINCLUDES) = 0 &THEN
FUNCTION add_datetime RETURN LOGICAL
      ( pparent AS CHAR,
        pname AS CHAR,
        pvalue AS DATETIME):
    DEF VAR lcDtString AS CHAR NO-UNDO.
    DEF VAR liTzMins AS INT NO-UNDO.
    IF pvalue EQ ? THEN RETURN FALSE.
    IF "{&DATETIME_SENDS}" EQ "local" THEN
        lcDtString = ENTRY(1, ISO-DATE(pvalue), ".").
    ELSE IF "{&DATETIME_SENDS}" EQ "UTC" THEN DO:
        liTzMins = TIMEZONE(SUBSTRING(ISO-DATE(DATETIME-TZ(pvalue)), 24)).
        lcDtString = ENTRY(1, ISO-DATE(pvalue - (liTzMins * 60000)), ".").
    END. ELSE
        lcDtString = ISO-DATE(DATETIME-TZ(pvalue)).
    /* E.g. Apache XMLRPC Client does not support dashes in the date */
    ENTRY(1, lcDtString, "T") = REPLACE(ENTRY(1, lcDtString, "T"), "-", "").
    _add_value(pparent, pname, "dateTime.iso8601", lcDtString).
    RETURN TRUE.
END.

FUNCTION add_date_or_time RETURN LOGICAL
      ( pparent AS CHAR,
        pname AS CHAR,
        pdate AS DATE,
        ptime AS INT ):
    IF pdate = ? THEN pdate = DATE("01.01.1970").
    IF ptime = ? THEN ptime = 0.
    RETURN add_datetime(pparent, pname, DATETIME(pdate, ptime * 1000)).
END.

FUNCTION add_timestamp RETURN LOGICAL
      ( pparent AS CHAR,
        pname AS CHAR,
        pvalue AS DECIMAL):
    DEF VAR ldate AS DATE NO-UNDO.
    DEF VAR ltime AS INT NO-UNDO.
    IF pvalue EQ ? OR pvalue EQ 0 THEN RETURN FALSE.
    IF NOT split_ts(pvalue, OUTPUT ldate, OUTPUT ltime) THEN
        RETURN FALSE.
    RETURN add_datetime(pparent, pname, DATETIME(ldate, ltime * 1000)).
END.
&ENDIF


FUNCTION find_param RETURN CHAR
      ( pparent AS CHAR,
        ppath AS CHAR ):
    DEF VAR result AS CHAR NO-UNDO.
    DEF VAR lcParentType AS CHAR NO-UNDO.
    DEF VAR lcc AS CHAR NO-UNDO.
    DEF VAR lcd AS CHAR NO-UNDO.

    IF pparent = "" THEN
        lcParentType = "array".
    ELSE DO:
        lcc = SUBSTRING(pparent, 1, R-INDEX(pparent, ",") - 1).
        lcd = ENTRY(NUM-ENTRIES(pparent), pparent).
        FIND tt_param
        WHERE tt_param.inout EQ get_from_tt
          AND tt_param.parent = lcc
          AND tt_param.position = INT(lcd) NO-ERROR.
        IF NOT AVAILABLE tt_param THEN DO:
            gc_xmlrpc_error = "Server error: Invalid parent".
            gi_xmlrpc_error = {&INTERNAL_ERROR}.
            RETURN ERROR.
        END.
        lcParentType = tt_param.type.
    END.

    IF lcParentType EQ "array" OR lcParentType EQ "json_array" THEN DO:
        FIND tt_param
        WHERE tt_param.inout EQ get_from_tt
          AND tt_param.parent = pparent
          AND tt_param.position = INT(ppath) NO-ERROR.
    END. ELSE DO:
        FIND tt_param
        WHERE tt_param.inout EQ get_from_tt
          AND tt_param.parent = pparent
          AND tt_param.name = ppath NO-ERROR.
    END.

    IF NOT AVAILABLE tt_param THEN
        RETURN param_err(SUBST("Expected parameter `&1` not found in &2 &3",
                                ppath, lcParentType, pparent)).

    RETURN SUBST("&1,&2", pparent, tt_param.position).
END FUNCTION.


FUNCTION get_int RETURN INT
      ( pparent AS CHAR,
        ppath AS CHAR ):
    DEF VAR result AS INT NO-UNDO.

    IF find_param(pparent, ppath) EQ ? THEN RETURN ERROR.
    IF tt_param.type NE "int" THEN DO:
        param_err(SUBST("Parameter `&1` at &2 must be an integer",
                                ppath, pparent)).
        RETURN ERROR.
    END.
    result = INT(tt_param.cvalue) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        param_err(SUBST("Parameter `&1` at &2 is not an integer",
                                ppath, pparent)).
        RETURN ERROR.
    END.
    RETURN result.
END FUNCTION.


FUNCTION get_pos_int RETURN INT
      ( pparent AS CHAR,
        ppath AS CHAR ):
    DEF VAR result AS INT NO-UNDO.

    result = get_int(pparent, ppath).
    IF result EQ ? THEN RETURN ERROR.
    IF result LE 0 THEN DO:
        param_err(SUBST("Invalid value &1 for &2,&3",
                                        result, pparent, ppath)).
        RETURN ERROR.
    END.
    RETURN result.
END FUNCTION.


FUNCTION get_string RETURN CHARACTER
      ( pparent AS CHAR,
        ppath AS CHAR ):
    DEF VAR lcc AS CHAR NO-UNDO.

    IF find_param(pparent, ppath) EQ ? THEN RETURN ERROR.
    IF tt_param.type NE "string" THEN
        RETURN param_err(SUBST("Parameter `&1` at &2 must be a string",
                                ppath, pparent)).
    lcc = CODEPAGE-CONVERT(tt_param.cvalue, SESSION:CHARSET, "UTF-8") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN param_err(SUBST("Encoding error in `&1` at &2: not UTF-8",
                                ppath, pparent)).
    RETURN lcc.
END FUNCTION.


FUNCTION get_nonempty_string RETURN CHARACTER
      ( pparent AS CHAR,
        ppath AS CHAR ):
    DEF VAR lcc AS CHAR NO-UNDO.
    lcc = get_string(pparent, ppath).
    IF lcc EQ ? THEN RETURN ERROR.
    IF TRIM(lcc) EQ "" THEN
        RETURN param_err(SUBST("String cannot be blank in `&1` at &2",
                                ppath, pparent)).
    RETURN lcc.
END FUNCTION.


FUNCTION store_file RETURN CHARACTER
      ( pparent AS CHAR,
        ppath AS CHAR,
        pFilename AS CHAR ):
    DEF VAR llcContents AS LONGCHAR NO-UNDO.
    DEF VAR lmpContents AS MEMPTR NO-UNDO.

    IF find_param(pparent, ppath) EQ ? THEN RETURN ERROR.
    IF tt_param.type NE "base64" THEN
        RETURN param_err(SUBST("Parameter `&1` at &2 must be a base64",
                                ppath, pparent)).
    COPY-LOB FROM FILE tt_param.cvalue TO llcContents. 
    lmpContents = BASE64-DECODE(llcContents).
/*    lmpContents = BASE64-DECODE(tt_param.cvalue). */
    COPY-LOB FROM lmpContents TO FILE pFilename.
    RETURN "".
END FUNCTION.


FUNCTION get_double RETURN DECIMAL
      ( pparent AS CHAR,
        ppath AS CHAR ):
    DEF VAR result AS DECIMAL NO-UNDO.

    IF find_param(pparent, ppath) EQ ? THEN RETURN ERROR.
    IF tt_param.type NE "double" THEN DO:
        param_err(SUBST("Parameter `&1` at &2 must be a double",
                                ppath, pparent)).
        RETURN ERROR.
    END.
    result = DECIMAL(REPLACE(tt_param.cvalue, ".", SESSION:NUMERIC-DECIMAL-POINT)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        param_err(SUBST("Parameter `&1` at &2 is not an double",
                                ppath, pparent)).
        RETURN ERROR.
    END.
    RETURN result.
END FUNCTION.


FUNCTION get_bool RETURN LOGICAL
      ( pparent AS CHAR,
        ppath AS CHAR ):
    DEF VAR result AS LOGICAL NO-UNDO.

    IF find_param(pparent, ppath) EQ ? THEN RETURN ERROR.
    IF tt_param.type NE "boolean" THEN DO:
        param_err(SUBST("Parameter `&1` at &2 must be a boolean",
                                ppath, pparent)).
        RETURN ERROR.
    END.
    result = (INT(tt_param.cvalue) EQ 1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        param_err(SUBST("Parameter `&1` at &2 is not a boolean",
                                ppath, pparent)).
        RETURN ERROR.
    END.
    RETURN result.
END FUNCTION.


&IF DEFINED(NOTIMEINCLUDES) = 0 &THEN
FUNCTION get_timestamp RETURN DECIMAL
      ( pparent AS CHAR,
        ppath AS CHAR ):
    DEF VAR result AS DECIMAL NO-UNDO FORMAT "99999999.99999".

    IF find_param(pparent, ppath) EQ ? THEN RETURN ERROR.
    IF NOT tt_param.type BEGINS "dateTime" THEN DO:
        param_err(SUBST("Parameter `&1` at &2 must be a dateTime",
                                ppath, pparent)).
        RETURN ERROR.
    END.
    result = _iso8601_to_timestamp(tt_param.cvalue).
    IF result EQ ? THEN DO:
        param_err(SUBST("Parameter `&1` at &2 is not a dateTime",
                                ppath, pparent)).
        RETURN ERROR.
    END.
    RETURN result.
END FUNCTION.

FUNCTION get_date RETURN DATE
      ( pparent AS CHAR,
        ppath AS CHAR ):
    DEF VAR result AS DATE NO-UNDO.

    IF find_param(pparent, ppath) EQ ? THEN RETURN ERROR.
    IF NOT tt_param.type BEGINS "dateTime" THEN DO:
        param_err(SUBST("Parameter `&1` at &2 must be a dateTime",
                                ppath, pparent)).
        RETURN ERROR.
    END.
    result = _iso8601_to_date(tt_param.cvalue).
    IF result EQ ? THEN DO:
        param_err(SUBST("Parameter `&1` at &2 is not a dateTime",
                                ppath, pparent)).
        RETURN ERROR.
    END.
    RETURN result.
END FUNCTION.

FUNCTION get_datetime RETURN DATETIME
      ( pparent AS CHAR,
        ppath AS CHAR ):
    DEF VAR result AS DATETIME NO-UNDO.

    IF find_param(pparent, ppath) EQ ? THEN RETURN ERROR.
    IF NOT tt_param.type BEGINS "dateTime" THEN DO:
        param_err(SUBST("Parameter `&1` at &2 must be a dateTime",
                                ppath, pparent)).
        RETURN ERROR.
    END.
    result = _iso8601_to_datetime(tt_param.cvalue).
    IF result EQ ? THEN DO:
        param_err(SUBST("Parameter `&1` at &2 is not a dateTime",
                                ppath, pparent)).
        RETURN ERROR.
    END.
    RETURN result.
END FUNCTION.

&ENDIF

FUNCTION get_array RETURN CHAR
      ( pparent AS CHAR,
        ppath AS CHAR ):
    DEF VAR lcc AS CHAR NO-UNDO.

    lcc = find_param(pparent, ppath).
    IF lcc EQ ? THEN RETURN ERROR.
    IF tt_param.type NE "array" AND tt_param.type NE "json_array" THEN
        RETURN param_err(SUBST("Parameter `&1` at &2 must be an array or an json_struct",
                                ppath, pparent)).
    RETURN lcc.
END FUNCTION.

FUNCTION get_struct RETURN CHAR
      ( pparent AS CHAR,
        ppath AS CHAR ):
    DEF VAR lcc AS CHAR NO-UNDO.

    lcc = find_param(pparent, ppath).
    IF lcc EQ ? THEN RETURN ERROR.
    IF tt_param.type NE "struct" AND tt_param.type NE "json_struct" THEN
        RETURN param_err(SUBST("Parameter `&1` at &2 must be an struct or an json_struct",
                                ppath, pparent)).
    RETURN lcc.
END FUNCTION.

FUNCTION get_paramcount RETURN INT
      ( pparent AS CHAR ):
    FOR EACH tt_param
    WHERE tt_param.inout EQ get_from_tt
      AND tt_param.parent = pparent:
        ACCUMULATE tt_param.position (COUNT).
    END.
    RETURN (ACCUM COUNT tt_param.position).
END FUNCTION.


FUNCTION _reverse RETURN CHAR
      ( pcList AS CHAR ):
    DEF VAR result AS CHAR NO-UNDO.
    DEF VAR lii AS INT NO-UNDO.

    DO lii = NUM-ENTRIES(pcList) TO 1 BY -1:
        result = result + "," + ENTRY(lii, pcList).
    END.
    RETURN result.
END FUNCTION.


FUNCTION _check_args RETURN CHAR
      ( pparent AS CHAR,
        piposition AS INT,
        INPUT-OUTPUT result AS CHAR,
        poption AS CHAR ):

    DEF VAR lcErrPos AS CHAR.

    FIND tt_param
    WHERE tt_param.inout EQ get_from_tt
      AND tt_param.parent = pparent
      AND tt_param.position = piposition - 1 NO-ERROR.
    lcErrPos = SUBST("&1,&2", pparent, piposition - 1).
    IF NOT AVAILABLE tt_param THEN
        RETURN param_err(SUBST("Missing &1 at &2", poption, lcErrPos)).
    IF LOOKUP(tt_param.type, poption, "|") EQ 0 THEN
        RETURN param_err(SUBST("Parameter at &1 is &2 instead of &3",
                                lcErrPos, tt_param.type, poption)).
    result = TRIM(result + "," + tt_param.type, ",").
    RETURN "".
END FUNCTION.

FUNCTION validate_array RETURN CHAR
      ( pparent AS CHAR,
        poptions AS CHAR ):
    DEF VAR liFront AS INT NO-UNDO.
    DEF VAR liBack AS INT NO-UNDO.
    DEF VAR liOptional AS INT NO-UNDO.
    DEF VAR liParamCount AS INT NO-UNDO.
    DEF VAR liOptionCount AS INT NO-UNDO.
    DEF VAR liParamsLeft AS INT NO-UNDO.
    DEF VAR result AS CHAR NO-UNDO.
    DEF VAR bresult AS CHAR NO-UNDO.
    DEF VAR lcc AS CHAR NO-UNDO.

    liParamCount = get_paramcount(pparent).
    liOptionCount = NUM-ENTRIES(poptions).
    IF liParamCount GT liOptionCount THEN
        RETURN param_err("Too many parameters, expected max. " +
                                        STRING(liOptionCount)).

    liParamsLeft = liParamCount.
    liFront = 1.
    REPEAT:
        IF liFront EQ liOptionCount + 1 THEN RETURN result.
        lcc = ENTRY(liFront, poptions).
        IF SUBSTRING(lcc, 1, 1) EQ "[" THEN LEAVE.
        IF _check_args(pparent, liFront, result, lcc) EQ ? THEN
            RETURN ?.
        liParamsLeft = liParamsLeft - 1.
        liFront = liFront + 1.
    END.

    liBack = liOptionCount.
    REPEAT:
        IF liBack EQ liFront THEN LEAVE.
        lcc = ENTRY(liBack, poptions).
        IF SUBSTRING(lcc, 1, 1) EQ "[" THEN LEAVE.
        IF _check_args(pparent,
                liParamCount - liOptionCount + liBack, bresult, lcc) EQ ? THEN
            RETURN ?.
        liParamsLeft = liParamsLeft - 1.
        liBack = liBack - 1.
    END.

    liOptional = liFront.
    DO WHILE liParamsLeft > 0:
        IF liFront EQ liBack + 1 THEN
            RETURN param_err(SUBST("Mismatched parameter at &1,&2",
                                        pparent, liOptional)).
        lcc = ENTRY(liFront, poptions).
        IF NOT lcc BEGINS "[" THEN
            RETURN param_err(SUBST("Invalid parameter definition at &1",
                                                                liFront)).
        lcc = SUBSTRING(lcc, 2, LENGTH(lcc) - 2).
        IF _check_args(pparent, liOptional, result, lcc) EQ ? THEN DO:
            gi_xmlrpc_error = 0.
            gc_xmlrpc_error = "".
        END. ELSE DO:
            liParamsLeft = liParamsLeft - 1.
            liOptional = liOptional + 1.
        END.
        liFront = liFront + 1.
    END.

    RETURN TRIM(result + _reverse(bresult), ",").

END FUNCTION.


FUNCTION validate_struct RETURN CHAR
      ( pparent AS CHAR,
        poptions AS CHAR ):
    DEF VAR lcc AS CHAR NO-UNDO.
    DEF VAR lcfound AS CHAR NO-UNDO.
    DEF VAR lcnotfound AS CHAR NO-UNDO.
    DEF VAR lii AS INT NO-UNDO.
    lcfound = REPLACE(REPLACE(poptions, "*", ""), "!", "").
    lcnotfound = lcfound.

    FOR EACH tt_param
    WHERE tt_param.inout EQ get_from_tt
      AND tt_param.parent = pparent:
        IF tt_param.name EQ "" THEN
            RETURN param_err(SUBST("Element at &1 in &2 does not have a " +
                               "member name", tt_param.position, pparent)).
        lii = LOOKUP(tt_param.name, lcfound).
        IF lii EQ 0 THEN
            RETURN param_err(SUBST("Unexpected member `&1` in &2",
                                   tt_param.name, pparent)).
        IF ENTRY(lii, lcnotfound) EQ "" THEN
            RETURN param_err(SUBST("Duplicate occurence of member `&1` in &2",
                                   tt_param.name, pparent)).
        IF INDEX(ENTRY(lii, poptions), "*") EQ 0 THEN
            ENTRY(lii, lcnotfound) = "".
    END.

    DO lii = 1 TO NUM-ENTRIES(lcnotfound):
        lcc = ENTRY(lii, lcnotfound).
        IF lcc EQ "" THEN NEXT.
        IF INDEX(ENTRY(lii, poptions), "!") GT 0 THEN
            RETURN param_err(SUBST("Required member `&1` not found in &2",
                                    lcc, pparent)).
        ENTRY(lii, lcfound) = "".
    END.

    DO WHILE INDEX(lcfound, ",,") GT 0:
        lcfound = REPLACE(lcfound, ",,", ",").
    END.

    RETURN TRIM(lcfound, ",").
END FUNCTION.


FUNCTION validate_request RETURN CHAR
      ( pparent AS CHAR,
        poptions AS CHAR ):
    DEF VAR lcparent AS CHAR NO-UNDO.
    DEF VAR lcSelf AS CHAR NO-UNDO.
    DEF VAR lipos AS INT NO-UNDO.

    IF pparent NE "" THEN DO:
        lipos = R-INDEX(pparent, ",").
        lcparent = SUBSTRING(pparent, 1, lipos - 1).
        lcSelf = SUBSTRING(pparent, lipos + 1).

        FIND tt_param
        WHERE tt_param.inout EQ get_from_tt
          AND tt_param.parent = lcparent
          AND tt_param.position = INT(lcSelf) NO-ERROR.

        IF tt_param.type EQ "struct" THEN
            RETURN validate_struct(pparent, poptions).
    END.

    RETURN validate_array(pparent, poptions).
END.

&IF DEFINED(NO_CRC_CHECK) EQ 0 &THEN
IF CAN-FIND(tt_param
            WHERE tt_param.inout EQ get_from_tt
              AND tt_param.parent = param_toplevel_id
              AND tt_param.type EQ "string"
              AND tt_param.cvalue EQ "__CRC_check__") THEN DO:
    add_boolean(response_toplevel_id, "", true).
    RETURN.
END.

&ENDIF
&ENDIF
