/**
 * RPC for getting memo history for a subscription and/or customer
 *
 * @input key;int;mandatory;Mobile subscription identifier/customer number
          type;string;mandatory;The type of memo (mobsub/customer/invoice/service)
          last_in_cache;int;mandatory;The latest memo sequence stored in cache
          memos_max;int;mandatory;The amount of memos allowed to fetch
 * @output memos;array;containing one structure
           overflow;int; 0 = no overflow and 1 = overflow
 * @struct memo_id;int;
           mtype;string;
           custnum;int;
           creator;str;
           title;string;
           content;string;
           creation_time;datetime;
           reference;string;(optional)
           important;int;
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{newton/src/get_memos.i}

/* Input parameters */
DEF VAR piKey AS INT NO-UNDO.
DEF VAR pcType AS CHAR NO-UNDO.
DEF VAR piLatest AS INT NO-UNDO.
DEF VAR piMemosMax AS INT NO-UNDO.
/* Output parameters */
DEF VAR toptop_array AS CHAR NO-UNDO.
DEF VAR top_array AS CHAR NO-UNDO.

DEFINE VARIABLE liMemoCount  AS INTEGER NO-UNDO INIT 0.

FUNCTION addMemoDataToResponse RETURN LOGICAL
      ( top_array AS CHAR,
        pcType AS CHAR ):

    DEF VAR memo_struct AS CHAR NO-UNDO.

    memo_struct = add_struct(top_array, "").

    IF pcType = 'mobsub' THEN
        add_string(memo_struct, "reference", STRING(Memo.Custnum)).
    ELSE IF pcType = 'invoice' THEN 
        add_string(memo_struct, "reference", Memo.KeyValue).

    add_int(memo_struct, "memo_id", Memo.MemoSeq).
    add_string(memo_struct, "mtype", pcType).
    add_int(memo_struct, "custnum", Memo.Custnum).
    add_string(memo_struct, "creator", Memo.CreUser).
    add_string(memo_struct, "title", Memo.MemoTitle).
    add_string(memo_struct, "content", Memo.MemoText).
    add_timestamp(memo_struct, "creation_time", Memo.CreStamp).
    add_int(memo_struct, "important", Memo.Priority).

END FUNCTION. 

IF validate_request(param_toplevel_id, "int,string,int,int") EQ ? THEN RETURN.
piKey = get_int(param_toplevel_id, "0").
pcType = get_string(param_toplevel_id, "1").
piLatest = get_int(param_toplevel_id, "2").
piMemosMax = get_int(param_toplevel_id, "3").
IF gi_xmlrpc_error NE 0 THEN RETURN.

toptop_array = add_array(response_toplevel_id, "").
top_array = add_array(toptop_array, "").

fMemoCount(pcType, piKey, FALSE).

FOR EACH ttMemo WHERE ttMemo.memoseq > piLatest NO-LOCK:
   
   liMemoCount = liMemoCount + 1.
   If liMemoCount > piMemosMax THEN DO:
      add_int(toptop_array, "", 1).
      RETURN.
   END.
   
   FIND FIRST Memo WHERE Memo.MemoSeq = ttMemo.memoseq NO-LOCK NO-ERROR.  
   addMemoDataToResponse(top_array, pcType).

END. 

add_int(toptop_array, "", 0).

FINALLY:
   EMPTY TEMP-TABLE ttMemo.
END.
