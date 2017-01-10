/**
 * Creates request for paper invoice push notification 
 *
 * @input string;mandatory;username
 * @output boolean;true
 */

{xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcUserName    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liRequestID   AS INTEGER   NO-UNDO. 

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcUserName = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUserName) EQ "" THEN RETURN appl_err("username is empty").

{commpaa.i}
gcBrand = "1".
katun = "VISTA_" + pcUserName.
{tmsconst.i}
{fcreatereq.i}

fCreateRequest(({&REQTYPE_PUSH_INVOICE}),
                  fMakeTS(),
                  "",
                  false,     /* fees */
                  false).    /* send sms */

ASSIGN
   bCreaReq.ReqSource   = {&REQUEST_SOURCE_NEWTON}
   bCreaReq.ReqDtParam1 = DATE(MONTH(TODAY),1,YEAR(TODAY))
   liRequestID          = bCreaReq.MsRequest.

RELEASE bCreaReq.

IF liRequestID = 0 THEN DO:
   RETURN appl_err("request was not created").
END.

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.

