
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
ASSIGN
katun = "cron"
gcbrand = "1".
DEF VAR resp_array AS CHARACTER NO-UNDO.
DEF VAR resp_struct AS CHARACTER NO-UNDO.

DEF VAR cDir AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttFiles NO-UNDO
   FIELD filename AS CHARACTER FORMAT "X(78)"
   INDEX filename IS PRIMARY UNIQUE filename.

IF NOT get_paramcount(param_toplevel_id) EQ 0 THEN
    RETURN param_err("Unexpected parameters").



DEF STREAM outfile.
OUTPUT STREAM outfile to VALUE( "/apps/yoigo/tms/templates/temp/testing2.txt").

cDir = SEARCH("templates/do_not_remove_templatefolder.txt").

PUT STREAM outfile UNFORMATTED
        cDir SKIP.
OUTPUT STREAM outfile CLOSE.


IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.

resp_array = add_array(response_toplevel_id, "").
      resp_struct = add_struct(resp_array,"").
         add_string(resp_struct,"filename",cDir).

FINALLY:
   EMPTY TEMP-TABLE ttFiles.
END.

