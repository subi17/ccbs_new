/**
 * Return countries and codes, as saved in tms.
 */
{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}

IF NOT get_paramcount(param_toplevel_id) EQ 0 THEN
    RETURN param_err("Unexpected parameters").

DEF VAR lcArray AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.

lcArray = add_array(response_toplevel_id, "").

FOR EACH country
WHERE country.coname NE "":
    lcStruct = add_struct(lcArray, "").
    add_string(lcStruct, "id", Country.Country).
    add_string(lcStruct, "name", TRIM(TRIM(Country.CoName, " "), '"')).
END.
