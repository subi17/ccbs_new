/**
 * Activate eInvoice Email
 *
 * @input      int;mandatory;request id
               string;mandatory;hash key
 * @output     boolean;true
 */
{xmlrpc/xmlrpc_access.i}

{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "tmsrpc".
{cparam2.i}
{tmsconst.i}
{msreqfunc.i}
{eventval.i}

DEF VAR pcHashKey      AS CHAR   NO-UNDO.
DEF VAR piRequestId    AS INT    NO-UNDO.

DEF VAR lcEncodedLink  AS CHAR   NO-UNDO.
DEF VAR lcSaltKey      AS CHAR   NO-UNDO.
DEF VAR liConfDays     AS INT    NO-UNDO.

DEF VAR lhCustomer           AS HANDLE NO-UNDO.
DEF VAR lhInvoiceTargetGroup AS HANDLE NO-UNDO.

IF validate_request(param_toplevel_id,"int,string") EQ ? THEN RETURN.
piRequestId = get_int(param_toplevel_id,"0").
pcHashKey   = get_string(param_toplevel_id,"1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}
   lhCustomer = BUFFER Customer:HANDLE.
   lhInvoiceTargetGroup = BUFFER InvoiceTargetGroup:HANDLE.
END. /* IF llDoEvent THEN DO: */

IF pcHashKey = "" OR pcHashKey = ? THEN
   RETURN appl_err("Hash Key is blank or unknown").

lcSaltKey = fCParam("EI","SaltKey").
IF lcSaltKey = "" OR lcSaltKey = ? THEN
   RETURN appl_err("Salt key is missing").

liConfDays = fCParamI("WaitingCanceleInvoiceDays").
IF liConfDays = 0 OR liConfDays = ? THEN liConfDays = 90.

FIND FIRST MsRequest WHERE
           MsRequest.MsRequest = piRequestId NO-LOCK NO-ERROR.
IF NOT AVAIL MsRequest THEN
   RETURN appl_err("Invalid Request Id").

IF MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} THEN
   RETURN appl_err("Customer already activated the link").
ELSE IF MsRequest.ReqStatus = {&REQUEST_STATUS_CANCELLED} THEN DO:
   IF MsRequest.Memo BEGINS "Activation link for eInvoice is not valid" THEN
      RETURN appl_err("Link is older than " + STRING(liConfDays) +
                      " days therefore it does not work anymore").
   ELSE
      RETURN appl_err("Link does not work because either delivery type " +
                      "or email address has been changed").
END. /* ELSE IF MsRequest.ReqStatus = {&REQUEST_STATUS_CANCELLED} */
ELSE IF MsRequest.ReqStatus <> {&REQUEST_STATUS_CONFIRMATION_PENDING} THEN
   RETURN appl_err("Invalid Request Status").

FIND FIRST Customer WHERE
           Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN RETURN appl_err("Customer not found").

FIND FIRST InvoiceTargetGroup WHERE
           InvoiceTargetGroup.CustNum = Customer.CustNum AND
           InvoiceTargetGroup.ToDate >= TODAY AND
           (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
            InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
     NO-LOCK NO-ERROR.

IF Customer.DelType <> {&INV_DEL_TYPE_EMAIL_PENDING} AND
   Customer.DelType <> {&INV_DEL_TYPE_EMAIL} AND
   NOT AVAIL InvoiceTargetGroup THEN
   RETURN appl_err("Invalid Invoice Delivery Type").

ASSIGN lcEncodedLink = Customer.Email + STRING(Customer.Custnum)
       lcEncodedLink = HEX-ENCODE(SHA1-DIGEST(lcEncodedLink,lcSaltKey)).

IF lcEncodedLink <> pcHashKey THEN
   RETURN appl_err("Hash key does not match").

IF Customer.DelType = {&INV_DEL_TYPE_EMAIL_PENDING} THEN DO:
   FIND CURRENT Customer EXCLUSIVE-LOCK NO-ERROR.
   RUN StarEventInitialize(lhCustomer).
   RUN StarEventSetOldBuffer(lhCustomer).

   Customer.DelType = {&INV_DEL_TYPE_EMAIL}.

   FIND CURRENT Customer NO-LOCK.
   RUN StarEventMakeModifyEvent(lhCustomer).
   fCleanEventObjects().
END. /* IF Customer.DelType = {&INV_DEL_TYPE_EMAIL_PENDING} THEN DO: */

IF AVAIL InvoiceTargetGroup AND
   InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} THEN DO:
   FIND CURRENT InvoiceTargetGroup EXCLUSIVE-LOCK NO-ERROR.
   RUN StarEventInitialize(lhInvoiceTargetGroup).
   RUN StarEventSetOldBuffer(lhInvoiceTargetGroup).

   InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL}.

   FIND CURRENT InvoiceTargetGroup NO-LOCK.
   RUN StarEventMakeModifyEvent(lhInvoiceTargetGroup).
   fCleanEventObjects().
END.

fReqStatus(2,"eInvoice has been activated").

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
