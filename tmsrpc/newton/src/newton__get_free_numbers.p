/** Get a list of 9 still free numbers from 622, 633 or 722 MSISDN range.
 * Optional search: If a non-empty parameter is provided, the first matching
   numbers will be returned. No random choice here.
 * @input search_key;string;optional;
 * @output numbers;array of strings
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{Syst/tmsconst.i}

/* Input parameters */
DEF VAR pcTenant AS CHAR NO-UNDO.
DEF VAR pcSearch AS CHAR NO-UNDO.
DEF VAR pcFor AS CHAR NO-UNDO.
/* Output parameters */
DEF VAR top_array AS CHAR NO-UNDO.
/* Local variables */
DEF VAR ldTS AS DECIMAL NO-UNDO.
DEF VAR lii AS INT NO-UNDO.

DEFINE VARIABLE gcRangeList AS CHARACTER INITIAL "622,633,722" NO-UNDO.
DEFINE VARIABLE gcRange AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE count 9

top_array = validate_request(param_toplevel_id, "string,[string]").

pcTenant = get_string(param_toplevel_id, "0").

IF NUM-ENTRIES(top_array) > 1 THEN
    pcSearch = get_string(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 then RETURN.

{newton/src/settenant.i pcTenant}

ASSIGN
   pcFor = {&MSISDN_STOCK_ONLINE}
   ldTS = {&nowTS}.

DEFINE TEMP-TABLE ttMSISDN NO-UNDO
   FIELD CLI AS CHARACTER
   INDEX CLI IS PRIMARY UNIQUE CLI.

FUNCTION fRange RETURNS CHARACTER ():

   DEFINE VARIABLE liEntries  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liEntry    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lii        AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcNewRange AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcRange    AS CHARACTER NO-UNDO.

   liEntries = NUM-ENTRIES(gcRangeList).

   IF liEntries EQ 0
   THEN RETURN "".
   ELSE IF liEntries EQ 1
   THEN liEntry = 1.
   ELSE liEntry = RANDOM(1,liEntries).

   DO lii = 1 TO liEntries:
      IF lii EQ liEntry
      THEN lcRange = ENTRY(lii,gcRangeList).
      ELSE lcNewRange = lcNewRange + "," + ENTRY(lii,gcRangeList).
   END.

   IF lcNewRange > ""
   THEN gcRangeList = SUBSTRING(lcNewRange, 2).
   ELSE gcRangeList = "".

   RETURN lcRange.

END FUNCTION.

top_array = add_array(response_toplevel_id, "").

FUNCTION fBuildCLI RETURNS LOGICAL ():

   EMPTY TEMP-TABLE ttMSISDN.

   DEFINE VARIABLE liAmount AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcCLI    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liMAX    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liMIN    AS INTEGER   NO-UNDO.

   FOR FIRST MSISDN NO-LOCK USE-INDEX POSDCLI WHERE
            MSISDN.POS EQ pcFor       AND
            MSISDN.StatusCode EQ 1    AND
            MSISDN.CLI BEGINS gcRange AND
            MSISDN.ValidTo GE ldTS    AND
            MSISDN.ValidFrom LE ldTS  AND
            MSISDN.LockedTo LT ldTS:
      liMAX = INTEGER(MSISDN.CLI).
   END.
   
   IF liMAX EQ 0
   THEN RETURN FALSE.

   FOR FIRST MSISDN NO-LOCK USE-INDEX POS WHERE
            MSISDN.POS EQ pcFor       AND
            MSISDN.StatusCode EQ 1    AND
            MSISDN.CLI BEGINS gcRange AND
            MSISDN.ValidFrom LE ldTS  AND
            MSISDN.ValidTo GE ldTS    AND
            MSISDN.LockedTo LT ldTS:
      liMIN = INTEGER(MSISDN.CLI).
   END.

   IF liMIN EQ 0 OR liMin EQ liMax
   THEN RETURN FALSE.

   lcCLI = STRING(RANDOM(liMin,liMax)).

   FOR EACH MSISDN NO-LOCK USE-INDEX POS WHERE
            MSISDN.POS EQ pcFor       AND
            MSISDN.StatusCode EQ 1    AND
            MSISDN.CLI GE lcCLI       AND
            MSISDN.ValidFrom LE ldTS  AND
            MSISDN.ValidTo GE ldTS    AND
            MSISDN.LockedTo LT ldTS:

      CREATE ttMSISDN.
      ASSIGN
         liAmount     = liAmount + 1
         ttMSISDN.CLI = MSISDN.CLI.

      IF liAmount EQ {&count}
      THEN RETURN TRUE.
   END.

   FOR EACH MSISDN NO-LOCK USE-INDEX POSDCLI WHERE
            MSISDN.POS EQ pcFor       AND
            MSISDN.StatusCode EQ 1    AND
            MSISDN.CLI LT lcCLI       AND
            MSISDN.ValidTo GE ldTS    AND
            MSISDN.ValidFrom LE ldTS  AND
            MSISDN.LockedTo LT ldTS:

      CREATE ttMSISDN.
      ASSIGN
         liAmount     = liAmount + 1
         ttMSISDN.CLI = MSISDN.CLI.

      IF liAmount EQ {&count}
      THEN RETURN TRUE.
   END.

   RETURN FALSE.

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
ELSE DO WHILE TRUE:
    gcRange = fRange().
    IF gcRange EQ ""
    THEN LEAVE.

    IF fBuildCLI()
    THEN DO:
      FOR EACH ttMSISDN:
         add_string(top_array, "", ttMSISDN.CLI).
      END.
      LEAVE.
    END.
END.
