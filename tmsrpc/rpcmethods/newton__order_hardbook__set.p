/**
 * Update order' terminal hard book state
 *
 * @input  struct;
           order_id;int;mandatory
           hard_book;int;mandatory
           hard_book_state;string;mandatory

 * @output success;boolean
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
katun = "NewtonRPC".
gcBrand = "1".
{Syst/tmsconst.i}
&GLOBAL-DEFINE STAR_EVENT_USER katun 
{Func/lib/eventlog.i}

DEF VAR pcStruct    AS CHAR NO-UNDO.
DEF VAR lcStruct    AS CHAR NO-UNDO.

DEF VAR lhBuff           AS HANDLE NO-UNDO.
DEF VAR liOrderId        AS INT    NO-UNDO.
DEF VAR liHardBook       AS INT    NO-UNDO.
DEF VAR lcHardBookState  AS CHAR   NO-UNDO.

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.
lcStruct = validate_request(pcStruct, "order_id!,hard_book!,hard_book_state!").

ASSIGN
   liOrderId = get_int(pcStruct,"order_id")
   liHardBook = get_int(pcStruct,"hard_book")
   lcHardBookState = get_string(pcStruct,"hard_book_state").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST OrderAccessory EXCLUSIVE-LOCK WHERE
           OrderAccessory.Brand = gcBrand AND
           OrderAccessory.OrderId = liOrderId AND
           OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE} NO-ERROR.

IF NOT AVAIL OrderAccessory THEN
   RETURN appl_err("Order doesn't contain any terminal").

lhBuff = BUFFER OrderAccessory:HANDLE.
RUN StarEventInitialize(lhBuff).
RUN StarEventSetOldBuffer(lhBuff).
ASSIGN OrderAccessory.HardBook      = liHardBook
       OrderAccessory.HardBookState = lcHardBookState.
RUN StarEventMakeModifyEvent(lhBuff).
RELEASE OrderAccessory.

add_boolean(response_toplevel_id, "", true).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   fCleanEventObjects().
END.