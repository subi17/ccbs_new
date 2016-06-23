/** Get a list of 9 still free numbers from 622, 633 or 722 MSISDN range.
 * Optional search: If a non-empty parameter is provided, the first matching
   numbers will be returned. No random choice here.
 * @input search_key;string;optional;
 * @output numbers;array of strings
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{Syst/tmsconst.i}

/* Input parameters */
DEF VAR pcSearch AS CHAR NO-UNDO.
DEF VAR pcFor AS CHAR NO-UNDO.
/* Output parameters */
DEF VAR top_array AS CHAR NO-UNDO.
/* Local variables */
DEF VAR ldTS AS DECIMAL NO-UNDO.
DEF VAR lii AS INT NO-UNDO.

DEF VAR lcRange            AS CHAR NO-UNDO.
DEF VAR lcRanges AS CHAR NO-UNDO. 
DEF VAR liRangeCount AS INT NO-UNDO. 

&GLOBAL-DEFINE count 9

top_array = validate_request(param_toplevel_id, "[string]").

IF NUM-ENTRIES(top_array) EQ 1 THEN
    pcSearch = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 then RETURN.

pcFor = {&MSISDN_STOCK_ONLINE}.
ldTS = {&nowTS}.

top_array = add_array(response_toplevel_id, "").

FUNCTION fAvailCLI RETURN LOG(INPUT icRange AS CHAR,
                              INPUT-OUTPUT iocRanges AS CHAR):

    lii = 0.

    FOR EACH msisdn NO-LOCK
    WHERE msisdn.brand EQ "1"
      AND msisdn.statuscode EQ 1
      AND msisdn.cli begins icRange
      AND MSISDN.POS EQ pcFor
      AND MSISDN.ValidTo GE ldTS
      AND msisdn.LockedTo LT ldTS
    BY msisdn.cli DESCENDING:
       lii = lii + 1.
       IF lii >= ({&count} * 3) THEN DO:
          iocRanges = iocRanges +
                      (IF iocRanges > "" THEN "," ELSE "") + icRange.
          RETURN TRUE.
       END.
    END.

    RETURN FALSE.
   
END FUNCTION. /* FUNCTION fAvailCLI */


FUNCTION fBuildCLI RETURN LOG(INPUT icRange  AS CHAR):

    DEF VAR lcBlock     AS CHAR NO-UNDO.
    DEF VAR lcStart     AS CHAR NO-UNDO.
    DEF VAR lcEnd       AS CHAR NO-UNDO.

    FOR EACH msisdn NO-LOCK
    WHERE msisdn.brand EQ "1"
      AND msisdn.statuscode EQ 1
      AND msisdn.cli begins icRange
      AND MSISDN.POS EQ pcFor
      AND MSISDN.ValidTo GE ldTS
      AND msisdn.LockedTo LT ldTS
    BY msisdn.cli:
        lcStart = msisdn.cli.
        LEAVE.
    END.

    FOR EACH msisdn NO-LOCK
    WHERE msisdn.brand EQ "1"
      AND msisdn.statuscode EQ 1
      AND msisdn.cli begins icRange
      AND MSISDN.POS EQ pcFor
      AND MSISDN.ValidTo GE ldTS
      AND msisdn.LockedTo LT ldTS
    BY msisdn.cli DESCENDING
    lii = 1 to {&count} * 2:
        lcEnd = msisdn.cli.
    END.

    lii = TRUNCATE((INTEGER(lcEnd) - INTEGER(lcStart)) / {&count}, 0).
    lcBlock = STRING(INTEGER(lcStart) + (RANDOM(0, lii) * {&count})).

    FOR EACH msisdn NO-LOCK
    WHERE msisdn.brand EQ "1"
      AND msisdn.statuscode EQ 1
      AND msisdn.cli GE lcBlock
      AND MSISDN.POS EQ pcFor
      AND msisdn.LockedTo LT ldTS
      AND MSISDN.ValidTo GE ldTS
      BY msisdn.cli
      lii = 1 TO {&count}:
        add_string(top_array, "", msisdn.cli).
    END.
   
END FUNCTION. /* FUNCTION fBuildCLI */


IF pcSearch NE "" THEN DO:

    pcSearch = "*" + pcSearch + "*".
    FOR EACH msisdn NO-LOCK
    WHERE MSISDN.Brand EQ "1"
      AND msisdn.statuscode EQ 1
      AND msisdn.LockedTo LT ldTS
      AND MSISDN.ValidTo GE ldTS
      AND MSISDN.POS EQ pcFor
      AND msisdn.cli MATCHES pcSearch
      BY msisdn.cli
      lii = 1 TO {&count}:
        add_string(top_array, "", msisdn.cli).
    END.

END.
ELSE DO:
    fAvailCLI(INPUT "622", INPUT-OUTPUT lcRanges).
    fAvailCLI(INPUT "633", INPUT-OUTPUT lcRanges).
    fAvailCLI(INPUT "722", INPUT-OUTPUT lcRanges).

    liRangeCount = NUM-ENTRIES(lcRanges).
    IF liRangeCount >= 2 THEN lcRange = ENTRY(RANDOM(1,liRangeCount),lcRanges).
    ELSE lcRange = lcRanges.
    
    IF lcRange > "" THEN fBuildCLI(INPUT lcRange).
END.
