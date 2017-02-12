/**
 * Update the priority of a Memo.
 *
 * @input  memo_id;int;the indenfier of the memo for which priority is changed.
           important;int;the new priority for the Memo
 * @output success;boolean;TRUE if the Memo was found with the identifier
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE piMemoSeq  AS INTEGER NO-UNDO. 
DEFINE VARIABLE piPriority AS INTEGER NO-UNDO. 
DEFINE VARIABLE lFound     AS LOGICAL NO-UNDO. 

IF validate_request(param_toplevel_id, "int,int") EQ ? THEN RETURN.

piMemoSeq  = get_int(param_toplevel_id, "0").
piPriority = get_int(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO common Memo MemoSeq piMemoSeq}

IF Memo.Priority NE piPriority THEN DO: 

   FIND CURRENT Memo EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF LOCKED Memo THEN RETURN appl_err("Record is locked!").
   ASSIGN Memo.Priority = piPriority.
   RELEASE Memo.
END.

add_boolean(response_toplevel_id, "", TRUE).

