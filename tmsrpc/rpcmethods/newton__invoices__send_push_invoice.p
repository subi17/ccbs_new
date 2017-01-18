/**
 * Creates request for paper invoice push notification 
 *
 * @input string;mandatory;username
 * @output boolean;true
 */

{xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcUserName    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liRequestID   AS INTEGER   NO-UNDO. 
DEFINE VARIABLE ldtPeriod     AS DATE      NO-UNDO.
DEFINE VARIABLE liMonth       AS INTEGER   NO-UNDO. 
DEFINE VARIABLE liYear        AS INTEGER   NO-UNDO. 
DEFINE VARIABLE ldeCurrentMonth AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldeNextMonth  AS DECIMAL   NO-UNDO. 

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcUserName = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUserName) EQ "" THEN RETURN appl_err("username is empty").

{commpaa.i}
gcBrand = "1".
katun = "VISTA_" + pcUserName.
{tmsconst.i}
{fcreatereq.i}

ldtPeriod = DATE(MONTH(TODAY),1,YEAR(TODAY)).

ASSIGN
   liMonth = MONTH(ldtPeriod) + 1
   liYear  = YEAR(ldtPeriod).

IF liMonth = 13 THEN ASSIGN 
   liMonth = 1
   liYear = liYear + 1.

ASSIGN
   ldeCurrentMonth = YEAR(ldtPeriod) * 10000 + MONTH(ldtPeriod) * 100 + 1
   ldeNextMonth    = liYear * 10000 + liMonth * 100 + 1.

FIND FIRST MsRequest WHERE
           MsRequest.Brand    = gcBrand AND
           MsRequest.ReqType  = ({&REQTYPE_PUSH_INVOICE}) AND
           MsRequest.ActStamp > ldeCurrentMonth AND
           MsRequest.ActStamp < ldeNextMonth AND
           LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_ONGOING_STATUSES}) > 0 
           NO-LOCK NO-ERROR.

IF AVAIL MsRequest THEN 
   RETURN appl_err("Push notification request for current month is ongoing").

fCreateRequest(({&REQTYPE_PUSH_INVOICE}),
                 fMakeTS(),
                 "",
                 false,     /* fees */
                 false).    /* send sms */

ASSIGN
   bCreaReq.ReqSource   = {&REQUEST_SOURCE_NEWTON}
   bCreaReq.ReqDtParam1 = ldtPeriod
   liRequestID          = bCreaReq.MsRequest.

RELEASE bCreaReq.

IF liRequestID = 0 THEN
   RETURN appl_err("request was not created").

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.

