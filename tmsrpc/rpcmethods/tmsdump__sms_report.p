/**
 * Fetch data for sms report sender (it takes about two minutes to collect data)
 *
 * @input empty;
 * @output data_struct;array 
 * @data_struct key;string;
                value;string;
*/
{xmlrpc/xmlrpc_access.i}

DEF VAR resp_array AS CHARACTER NO-UNDO. 
DEF VAR resp_struct AS CHARACTER NO-UNDO. 

DEFINE TEMP-TABLE ttReport NO-UNDO
FIELD name AS CHAR
FIELD type AS CHAR
FIELD field_value AS CHAR
INDEX name IS PRIMARY UNIQUE name.

IF NOT get_paramcount(param_toplevel_id) EQ 0 THEN
    RETURN param_err("Unexpected parameters").

RUN smsreport_data.p(output table ttReport BY-REFERENCE).

resp_array = add_array(response_toplevel_id, "").

FOR EACH ttReport NO-LOCK:
   resp_struct = add_struct(resp_array,"").   
   add_string(resp_struct,"key",ttReport.name).
   add_string(resp_struct,"value",ttReport.field_value).
END.

FINALLY:
   EMPTY TEMP-TABLE ttReport.
END.
