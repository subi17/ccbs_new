/* ----------------------------------------------------------------------
  MODULE .......: NEWTON_GET_CDRFILES.P
  TASK .........: Browse Files for web tool
  APPLICATION ..: TMS
  AUTHOR .......: kariaika & ilkkasav
  CREATED ......: 11.02.2015
  CHANGED ......:

  Version ......: Yoigo
  ---------------------------------------------------------------------- */
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


/*Search correct directory*/
cDir = SEARCH("testing/donotremove_testdir.txt").
cDir = REPLACE(cDir, "donotremove_testdir.txt", "").

INPUT THROUGH VALUE("ls " + cDir + "cdrfiles/*.asc" + " | xargs -n 1 basename").
REPEAT:
   CREATE ttFiles.
   IMPORT UNFORMATTED ttFiles.FileName.
END.

/* RUN filelist_data.p(output table ttFiles BY-REFERENCE). */

resp_array = add_array(response_toplevel_id, "").

FOR EACH ttFiles NO-LOCK:

   resp_struct = add_struct(resp_array,"").
   add_string(resp_struct,"filename",ttFiles.filename).
END.


IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.

FINALLY:
   EMPTY TEMP-TABLE ttFiles.
END.

