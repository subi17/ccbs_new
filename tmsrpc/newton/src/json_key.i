/* {xmlrpc_access.i} */

FUNCTION add_json_key_struct RETURN CHAR
      ( pparent AS CHAR,
        pname AS CHAR ):
    DEF VAR lcJSONHash AS CHAR NO-UNDO.
    lcJSONHash = add_struct(pparent, pname).
    RETURN add_json_struct(lcJSONHash, "json").
END.

FUNCTION add_json_key_array RETURN CHAR
      ( pparent AS CHAR,
        pname AS CHAR ):
    DEF VAR lcJSONHash AS CHAR NO-UNDO.
    lcJSONHash = add_struct(pparent, pname).
    RETURN add_json_array(lcJSONHash, "json").
END.


