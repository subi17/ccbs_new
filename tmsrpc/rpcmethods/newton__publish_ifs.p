/* ----------------------------------------------------------------------
  MODULE .......: newton__publish_ifs.p
  TASK .........: Creates msrequest request to publish invoices to
                  ifs
  APPLICATION ..:
  AUTHOR .......: subhash sanjeevi
  CREATED ......: 19.08.2015
  CHANGED ......:
  Version ......:
  ---------------------------------------------------------------------- */

{xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcUsername  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcError     AS CHARACTER NO-UNDO.
DEFINE VARIABLE liRequestID AS INTEGER   NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcUserName = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "" THEN RETURN appl_err("username is empty").

{Syst/commpaa.i}
gcBrand = "1".
katun = "VISTA_" + pcUserName.
{Syst/tmsconst.i}
{Func/publish_ifs.i}

liRequestID = fPublishIFSRequest
                        (fMakeTS(),  /* when request should be handled */
                         DATE(MONTH(TODAY),1,YEAR(TODAY)),
                         katun, /* creator */
                         {&REQUEST_SOURCE_NEWTON},
                         OUTPUT lcError).

IF liRequestID = 0 THEN DO:
   RETURN appl_err(lcError).
END.

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
