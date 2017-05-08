/**
 * Get terminal information.
 *
 * @output terminal_info;array of struct;terminal information
 * @terminal_info terminal_code;string;terminal billing code
                  dextra_price;double;monthly cost
                  valid_from;datetime;valid from
                  valid_to;datetime;valid to
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE resp_array     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcResultStruct AS CHARACTER NO-UNDO.

DEF BUFFER bTerminalConf       FOR TerminalConf.

resp_array = add_array(response_toplevel_id, "").

FOR EACH TerminalConf WHERE
         TerminalConf.ValidTo >= TODAY NO-LOCK:

   FIND FIRST bTerminalConf WHERE
              bTerminalConf.TerminalCode = TerminalConf.TerminalCode AND
              bTerminalConf.ValidFrom <= TODAY AND
              bTerminalConf.ValidTo >= TODAY AND
              ROWID(bTerminalConf) <> ROWID(TerminalConf) NO-LOCK NO-ERROR.
   IF AVAIL bTerminalConf THEN NEXT.

   lcResultStruct = add_struct(resp_array, "").
   add_string(lcResultStruct, "terminal_code", TerminalConf.TerminalCode).
   add_double(lcResultStruct,"dextra_price", TerminalConf.DextraPrice).
   add_datetime(lcResultStruct,"valid_from", TerminalConf.ValidFrom).
   add_datetime(lcResultStruct,"valid_to", TerminalConf.ValidTo).

END.
