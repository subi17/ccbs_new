/**
 * Adding a single memo.
 *
 * @input   reference;string;mandatory;key of table referenced by type
            type;string;mandatory;type of the memo (mobsub, service, customer or invoice)
            custnum;int;mandatory;customer number if any
            creator;string;mandatory;id of logged in user
            title;string;mandatory;title of memo
            content;string;mandatory;contents of memo
            priority;int;mandatory;0 = not important, 1 = important
 * @output  success;boolean
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1} 

{Func/timestamp.i}

/* Input parameters */
DEF VAR pcReference AS CHAR NO-UNDO.
DEF VAR pcType AS CHAR NO-UNDO.
DEF VAR piCustnum AS INT NO-UNDO.
DEF VAR pcCreator AS CHAR NO-UNDO.
DEF VAR pcTitle AS CHAR NO-UNDO.
DEF VAR pcContent AS CHAR NO-UNDO.
DEF VAR piPriority AS INTEGER NO-UNDO.
/* Local variables */
DEF VAR lcType AS CHAR NO-UNDO.
/* Output parameters */
DEF VAR result AS LOGICAL.


IF validate_request(param_toplevel_id,
                    "string,string,int,string,string,string,int") EQ ? THEN
    RETURN.
piPriority  = get_int(param_toplevel_id, "6").
pcContent   = get_string(param_toplevel_id, "5").
pcTitle     = get_string(param_toplevel_id, "4").
pcCreator   = "VISTA_" + get_string(param_toplevel_id, "3").
piCustnum   = get_int(param_toplevel_id, "2").
pcType      = get_string(param_toplevel_id, "1").
pcReference = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcCreator) EQ "VISTA_" THEN RETURN appl_err("username is empty").

IF pcType = "service" THEN DO:
    pcType = "MobSub".
    lcType = "service".
END. 

result = FALSE.
DO TRANSACTION ON ERROR UNDO:
    CREATE Memo.
    ASSIGN
        memo.brand = "1"
        memo.MemoSeq   = NEXT-VALUE(memoseq)
        memo.CreUser   = pcCreator
        memo.HostTable = pcType
        memo.MemoType  = lcType
        memo.CustNum   = piCustnum
        memo.KeyValue  = pcReference
        memo.MemoTitle = pcTitle
        memo.MemoText  = pcContent
        memo.CreStamp  = {&nowTS}
        memo.Priority  = piPriority
    .
    result = TRUE.
END.

add_boolean(response_toplevel_id, "", result).
