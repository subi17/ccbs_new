DEFINE TEMP-TABLE xmlrpcprod
    FIELD curstatus AS INT
    FIELD statusname AS CHAR
    FIELD prodtype AS LOG FORMAT "open/close"
    FIELD production AS CHAR
    FIELD need_data AS INT
    FIELD return_to AS INT
    FIELD nextstatus AS INT
    FIELD action AS CHAR
.
 
DEFINE SHARED VARIABLE gcMethodName AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE gi_xmlrpc_error AS INT NO-UNDO.
DEFINE SHARED VARIABLE gc_xmlrpc_error AS CHAR NO-UNDO.
DEFINE SHARED TEMP-TABLE tt_param
    FIELD inout AS INT
    FIELD position AS INT
    FIELD parent AS CHAR INITIAL ?
    FIELD name AS CHAR
    FIELD type AS CHAR
    FIELD cvalue AS CHAR
    INDEX gi IS PRIMARY UNIQUE inout parent position.
&GLOBAL-DEFINE request_in 0
&GLOBAL-DEFINE response_out 1

DEFINE VARIABLE gcCharData AS CHAR NO-UNDO.
DEFINE VARIABLE giStatus   AS INT INITIAL 0 NO-UNDO.
DEFINE VARIABLE gcLastTag  AS CHAR NO-UNDO.     /* for error message */
DEFINE VARIABLE gcRedirToFile AS CHAR NO-UNDO.
DEFINE VARIABLE gcBase64Files AS CHAR NO-UNDO.

DEF VAR return_stack AS CHAR INITIAL "" NO-UNDO.

/* TODO
 * This lib fails when the value comes earlier than the name in members
 * (or multiple values/names).
 * Solution suggestion: Build a real stack in a temp-table to save the
 * current member, too. */
DEF VAR parent_stack AS CHAR INITIAL ";0" NO-UNDO.
DEF VAR gcMemberName AS CHAR NO-UNDO.

DEFINE STREAM prodin.
PROCEDURE init_production_temptable:
    DEFINE INPUT PARAMETER plServer AS LOGICAL.
    FOR EACH xmlrpcprod EXCLUSIVE-LOCK: DELETE xmlrpcprod. END.
    IF plServer THEN
        INPUT STREAM prodin FROM VALUE(SEARCH("xmlrpc/xmlrpcprod_s.d")).
    ELSE
        INPUT STREAM prodin FROM VALUE(SEARCH("xmlrpc/xmlrpcprod_c.d")).
    REPEAT:
        CREATE xmlrpcprod.
        IMPORT STREAM prodin xmlrpcprod.
    END.
    INPUT STREAM prodin CLOSE.
END.

PROCEDURE cleanup:
    gcMethodName = "".
    gcCharData = "".
    giStatus = 0.
    gcLastTag = "".
    return_stack = "".
    parent_stack = ";0".
    gcMemberName = "".
    gcRedirToFile = ?.
    DEF VAR lii AS INT NO-UNDO.
    DO lii = 1 TO NUM-ENTRIES(gcBase64Files):
        IF SEARCH(ENTRY(lii, gcBase64Files)) NE ? THEN
            OS-DELETE VALUE(ENTRY(lii, gcBase64Files)).
    END.
    gcBase64Files = "".
END PROCEDURE.

FUNCTION production_action RETURN LOGICAL
      ( pcAction AS CHAR,
        pcData AS CHAR ):
    DEF VAR lcParent AS CHAR NO-UNDO.
    DEF VAR liPosition AS INT NO-UNDO.

    IF pcAction EQ "set methodname" THEN DO:
        gcMethodName = pcData.
        RETURN TRUE.
    END.
    IF pcAction EQ "make fault" THEN DO:
        gi_xmlrpc_error = -1.
        gc_xmlrpc_error = "- unspecified -".
        FOR EACH tt_param
        WHERE tt_param.inout EQ {&request_in}
          AND tt_param.parent EQ ",0":
            IF tt_param.name EQ "faultCode" THEN DO:
                gi_xmlrpc_error = INT(tt_param.cvalue) NO-ERROR.
                IF gi_xmlrpc_error EQ 0 OR gi_xmlrpc_error EQ ? THEN
                    gi_xmlrpc_error = -1.
            END. ELSE IF tt_param.name EQ "faultString" THEN
                gc_xmlrpc_error = tt_param.cvalue.
            ELSE DO:
                message "Unexpected fault syntax".
                RETURN FALSE.
            END.
        END.
        FOR EACH tt_param EXCLUSIVE-LOCK
        WHERE tt_param.inout EQ {&request_in}:
           DELETE tt_param.
        END.
        RETURN TRUE.
    END.

    lcParent = ENTRY(NUM-ENTRIES(parent_stack), parent_stack).
    liPosition = INT(ENTRY(2, lcParent, ";")).
    lcParent = ENTRY(1, lcParent, ";").

    IF pcAction EQ "save base64" THEN gcRedirToFile = ?.

    IF pcAction EQ "redir to file" THEN DO:
        gcRedirToFile = SESSION:TEMP-DIR + "/" + GUID(GENERATE-UUID).
        gcBase64Files = TRIM(gcBase64Files + "," + gcRedirToFile, ",").
    END. ELSE IF pcAction BEGINS "close" THEN DO:
        ENTRY(NUM-ENTRIES(parent_stack), parent_stack) = "".
        parent_stack = TRIM(parent_stack, ",").
    END. ELSE IF pcAction BEGINS "save" OR pcAction BEGINS "open" THEN DO:
        CREATE tt_param.
        ASSIGN
            tt_param.inout = {&request_in}
            tt_param.parent = REPLACE(lcParent, ".", ",")
            tt_param.position = liPosition
            tt_param.name = gcMemberName
            tt_param.type = SUBSTRING(pcAction, 6)
            tt_param.cvalue = pcData
            gcMemberName = "".
        ENTRY(NUM-ENTRIES(parent_stack), parent_stack) =
                                lcParent + ";" + STRING(liPosition + 1).
    END. ELSE IF pcAction EQ "set membername" THEN DO:
        gcMemberName = pcData.
    END. ELSE IF pcAction EQ "has name?" THEN DO:
        IF gcMembername NE "" THEN DO:
            message "name of member already defined".
            RETURN FALSE.
        END.
    END.

    IF pcAction BEGINS "open" THEN
        parent_stack = parent_stack + "," +
                        lcParent + "." + STRING(liPosition) + ";0".

    RETURN TRUE.
END FUNCTION.

FUNCTION pop_return_status RETURN INT ( ):
    DEF VAR result AS INT NO-UNDO.

    result = INT(ENTRY(NUM-ENTRIES(return_stack), return_stack)).
    ENTRY(NUM-ENTRIES(return_stack), return_stack) = "".
    return_stack = TRIM(return_stack, ",").
    RETURN result.
END.

FUNCTION ElementCommon RETURN LOGICAL
      ( pcTagname AS CHAR,
        plStart AS LOG ):
    DEF VAR lcTagRepr AS CHAR NO-UNDO.

    lcTagRepr = "<" + (IF plStart THEN "" ELSE "/") + pcTagName + ">".

    FIND FIRST xmlrpcprod
    WHERE xmlrpcprod.curstatus = giStatus
      AND xmlrpcprod.prodtype = plStart
      AND xmlrpcprod.production = pcTagname NO-ERROR.

    IF NOT AVAILABLE xmlrpcprod THEN DO:
        MESSAGE SUBST("Unexpected tag &1 after &2", lcTagRepr, gcLastTag).
        MESSAGE "Allowed productions:".
        FOR EACH xmlrpcprod WHERE xmlrpcprod.curstatus = giStatus:
            MESSAGE "  " +
                    (IF xmlrpcprod.need_data EQ 1 THEN "data" ELSE "") +
                    "<" + (IF xmlrpcprod.prodtype THEN "" ELSE "/") +
                    xmlrpcprod.production + ">".
        END.
        RETURN FALSE.
    END.

    IF plStart THEN DO:
        IF gcCharData NE "" THEN DO:  /* cannot happen in progress sax parser */
            MESSAGE "Unexpected data before tag " + lcTagRepr.
            RETURN FALSE.
        END.
    END. ELSE DO:
        IF xmlrpcprod.need_data EQ 1 AND gcCharData = "" THEN DO:
            MESSAGE "Empty data in tag " + pcTagname.
            RETURN FALSE.
        END.
        IF xmlrpcprod.need_data EQ 0 AND gcCharData NE "" THEN DO:
            MESSAGE "Unexpected data in tag " + pcTagname.
            RETURN FALSE.
        END.
    END.
    IF xmlrpcprod.return_to > -1 THEN DO:
        return_stack = TRIM(return_stack + "," +
                            STRING(xmlrpcprod.return_to), ",").
    END.

    IF xmlrpcprod.action NE ? THEN
        IF NOT production_action(xmlrpcprod.action, gcCharData) THEN
            RETURN FALSE.

    gcCharData = "".
    IF xmlrpcprod.nextstatus > -1 THEN
        giStatus = xmlrpcprod.nextstatus.
    ELSE
        giStatus = pop_return_status().
    gcLastTag = lcTagRepr.
    RETURN TRUE.
END FUNCTION.


PROCEDURE StartElement:
    DEFINE INPUT PARAMETER namespaceURI AS CHARACTER. 
    DEFINE INPUT PARAMETER localName    AS CHARACTER. 
    DEFINE INPUT PARAMETER qName        AS CHARACTER. 
    DEFINE INPUT PARAMETER attributes   AS HANDLE. 
    IF NOT ElementCommon(localName, TRUE) THEN
        RETURN ERROR.
END PROCEDURE.

 
PROCEDURE Characters:  
    DEFINE INPUT PARAMETER charData  AS MEMPTR. 
    DEFINE INPUT PARAMETER numChars  AS INTEGER. 
    IF gcRedirToFile NE ? THEN DO:
        /* The SAX-Reader seems to split into lines anyway and those are
           in base64 by convention 76 chars long. If I COPY-LOB the mptr
           into the file, it appends the /0 character...?! */
        numChars = GET-SIZE(charData) - 1.
        
        IF numChars < 30000 THEN DO:
           gcCharData = TRIM(gcCharData + GET-STRING(charData, 1, numChars)).
           OUTPUT TO VALUE(gcRedirToFile) APPEND.
           PUT UNFORMATTED gcCharData.
           OUTPUT CLOSE.
        END.
        ELSE COPY-LOB charData FOR numChars TO FILE gcRedirToFile APPEND. 
        
        gcCharData = gcRedirToFile.
    END. ELSE DO:
        /* We need number of BYTES, not of CHARS for GET-STRING */
        numChars = GET-SIZE(charData) - 1.
        gcCharData = TRIM(gcCharData + GET-STRING(charData, 1, numChars)).
    END.
END PROCEDURE. 
 
PROCEDURE EndElement:  
    DEFINE INPUT PARAMETER namespaceURI AS CHARACTER. 
    DEFINE INPUT PARAMETER localName    AS CHARACTER. 
    DEFINE INPUT PARAMETER qName        AS CHARACTER. 
    IF NOT ElementCommon(localName, FALSE) THEN
        RETURN ERROR.
END PROCEDURE.
