/**
 * Change a memo priority.
 *
 * @input   memoseq;int;memo sequence number,
            important;int;0 = not important, 1 = important
 * @output  success;boolean
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1} 

/* Input parameters */
DEF VAR piReference AS INT NO-UNDO.
DEF VAR piPriority  AS INT NO-UNDO.
/* Local variables */
DEF VAR lcType AS INT NO-UNDO.
/* Output parameters */
DEF VAR result AS LOGICAL.

IF validate_request(param_toplevel_id,
                    "int,int") EQ ? THEN RETURN.
piReference = get_pos_int(param_toplevel_id, "0").
piPriority  = get_int(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO common Memo MemoSeq piReference}

FIND CURRENT Memo EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF LOCKED Memo THEN 
	RETURN appl_err("Record is locked!").

ASSIGN memo.Priority  = piPriority.

RELEASE Memo.

add_boolean(response_toplevel_id, "", true).
