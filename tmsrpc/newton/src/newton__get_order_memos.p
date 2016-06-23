/**
 * Get Memos for an order
 *
 * @input  tms_id;int;mandatory;Order Id
           type;string;mandatory;name of the table for which memos are seeked; here typically Order
           limit;int;mandatory;mamimum number of memos returned
           last_id;int;mandatory;the latest seeked memo with same above parameter values or 0 for the first search.
 * @output memos;array;a list of memo structs
 * @memo   memo_id;int;MemoSeq of the Memo
           mtype;int;MemoType of the Memo
           custnum;int;Customer number of the Memo
           creator;str;Creator of the Memo
           title;str;title of the Memo
           content;str;content of the Memo
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE piOrderId AS INTEGER NO-UNDO. 
DEFINE VARIABLE pcType    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE piMaxCount   AS INTEGER NO-UNDO.
DEFINE VARIABLE piLastId  AS INTEGER NO-UNDO. 
DEFINE VARIABLE top_struct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE top_array AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liCounter AS INTEGER NO-UNDO. 
DEFINE VARIABLE liLeftCounter AS INTEGER NO-UNDO. 

IF validate_request(param_toplevel_id, "int,string,int,int") EQ ? THEN RETURN.

piOrderId  = get_pos_int(param_toplevel_id, "0").
pcType     = get_string(param_toplevel_id, "1").
piMaxCount = get_int(param_toplevel_id, "2").
piLastid   = get_int(param_toplevel_id, "3").
IF gi_xmlrpc_error NE 0 THEN RETURN.

top_struct = add_struct(response_toplevel_id, "").
top_array = add_array(top_struct, "memos").

FUNCTION fAddMemoStruct RETURN LOGICAL:
 
   DEFINE VARIABLE lcMemoStruct AS CHARACTER NO-UNDO. 
   lcMemoStruct = add_struct(top_array, "").
   add_int(lcMemoStruct, "memo_id", Memo.MemoSeq).
   add_timestamp(lcMemoStruct, "creation_time", Memo.CreStamp).
   add_int(lcMemoStruct, "priority", Memo.Priority).
   add_int(lcMemoStruct, "custnum", Memo.CustNum).
   add_string(lcMemoStruct, "creator", Memo.CreUser).
   add_string(lcMemoStruct, "title"  , Memo.MemoTitle).
   add_string(lcMemoStruct, "content", Memo.MemoText).
   RETURN TRUE.
END.

liCounter = 0.
liLeftCounter = 0.

MemoLoop:
FOR EACH Memo WHERE 
   Memo.Brand = "1" AND 
   Memo.HostTable = pcType AND
   Memo.KeyValue = STRING(piOrderId) NO-LOCK:
    
   liCounter = liCounter + 1.
   IF liCounter > piLastId + piMaxCount THEN DO:
      liLeftCounter = 1.
      LEAVE MemoLoop.
   END.
   IF liCounter < piLastId + 1 THEN NEXT MemoLoop.
   fAddMemoStruct().
END.

add_int(top_struct, "more_memos", liLeftCounter).
