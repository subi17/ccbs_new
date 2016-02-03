/**
 * Set invoice delivery type
 *
 * @input      transaction_id;string;mandatory;transaction id
               msisdn;string;mandatory;MSISDN
               delivery_type;integer;mandatory;Delivery Type
 * @output     struct;mandatory;response struct
 * @response   transaction_id;string;transaction id
               result;boolean;True
 * @Examples   Delivery Type;Delivery Type Name
               1;Paper Invoice
               2;Email Invoice
               4;SMS Invoice
               10;No Delivery
 * @Exceptions 1;Subscription not found
               2;Customer not found
               3;Customer record is locked
               4;Unknown invoice delivery type
               5;Invoice Delivery Type to Email can not be changed
               6;Email has already been sent to customer to activate the Email Invoice service
               7;Invoice Delivery Type to Email can not be changed since customer does not have email address
               8;Application Id does not match
 */

{xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.

/* Input parameters */
DEF VAR pcMSISDN       AS CHAR NO-UNDO.
DEF VAR piDelType      AS INT  NO-UNDO.
DEF VAR pcTransId      AS CHAR NO-UNDO.
DEF VAR top_struct     AS CHAR NO-UNDO.

DEF VAR liRequest      AS INT  NO-UNDO.
DEF VAR lcResult       AS CHAR NO-UNDO.
DEF VAR lhCustomer     AS HANDLE NO-UNDO.
DEF VAR lcApplicationId AS CHAR NO-UNDO. 

{Syst/commpaa.i}
ASSIGN
   katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId
   gcBrand = "1".
{Syst/tmsconst.i}
{Func/timestamp.i}
{Syst/eventval.i}
{Func/fmakemsreq.i}
{Func/femailinvoice.i}
{Func/fexternalapi.i}

IF validate_request(param_toplevel_id, "string,string,int") EQ ? THEN RETURN.

ASSIGN pcTransId = get_string(param_toplevel_id, "0")
       pcMSISDN = get_string(param_toplevel_id,"1")
       piDelType = get_int(param_toplevel_id,"2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcApplicationId = substring(pcTransId,1,3).

IF NOT fchkTMSCodeValues(gbAuthLog.UserName, lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").

katun = lcApplicationId + "_" + gbAuthLog.EndUserId.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun   
   {lib/eventlog.i}
   lhCustomer = BUFFER Customer:HANDLE.
END.

FIND FIRST MobSub WHERE 
           MobSub.Brand = gcBrand AND
           MobSub.cli   = pcMSISDN NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN RETURN appl_err("Subscription not found").

CASE piDelType:
   WHEN {&INV_DEL_TYPE_PAPER}       THEN .
   WHEN {&INV_DEL_TYPE_EMAIL}       THEN .
   WHEN {&INV_DEL_TYPE_SMS}         THEN .
   WHEN {&INV_DEL_TYPE_NO_DELIVERY} THEN .
   OTHERWISE RETURN appl_err("Unknown invoice delivery status").
END. /* CASE piDelType: */

FIND FIRST Customer WHERE 
           Customer.CustNum = MobSub.CustNum
     EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF LOCKED Customer THEN RETURN appl_err("Customer record is locked").
IF NOT AVAIL Customer THEN RETURN appl_err("Customer not found").

IF piDelType = {&INV_DEL_TYPE_EMAIL} AND
   Customer.Email = "" THEN
   RETURN appl_err("Invoice delivery type to Email can not be changed " +
                   "since customer does not have email address").

IF Customer.DelType NE piDelType THEN DO:
   RUN StarEventInitialize(lhCustomer).
   RUN StarEventSetOldBuffer(lhCustomer).

   /* If DelType is Email then set to Email Pending first and send 
      an email to customer to activate the email service */
   IF piDelType = {&INV_DEL_TYPE_EMAIL} THEN DO:
      liRequest = fEmailInvoiceRequest(INPUT fMakeTS(),
                                       INPUT TODAY,
                                       INPUT katun,
                                       INPUT MobSub.MsSeq,
                                       INPUT MobSub.CLI,
                                       INPUT Mobsub.Custnum,
                                       INPUT {&REQUEST_SOURCE_EXTERNAL_API},
                                       INPUT Customer.Email,
                                       INPUT 0, /* msseq */
                                       OUTPUT lcResult).
      IF liRequest = 0 THEN DO:
         IF lcResult = "Customer already has an active request" THEN .
         ELSE RETURN appl_err("Invoice delivery type to Email can not be changed").
      END. /* IF liRequest = 0 THEN DO: */

      /* If Email already validated then mark DelType EMAIL */
      IF liRequest = 1 THEN
         Customer.DelType = {&INV_DEL_TYPE_EMAIL}.
      ELSE
         Customer.DelType = {&INV_DEL_TYPE_EMAIL_PENDING}.

   END. /* IF piDelType = {&INV_DEL_TYPE_EMAIL} THEN DO: */
   ELSE DO:
      Customer.DelType = piDelType.

      /* Cancel Ongoing Email Activation Request (if any) */
      FIND FIRST InvoiceTargetGroup WHERE
                 InvoiceTargetGroup.CustNum = Customer.CustNum AND
                 InvoiceTargetGroup.ToDate >= TODAY AND
                (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
                 InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
           NO-LOCK NO-ERROR.
      IF NOT AVAIL InvoiceTargetGroup AND
         fPendingEmailActRequest(INPUT Mobsub.Custnum) THEN
         fCancelPendingEmailActRequest(INPUT Mobsub.Custnum,
                                       INPUT "Invoice Delivery Type is " +
                                       "changed to " + STRING(Customer.DelType)).
   END. /* ELSE DO: */

   FIND CURRENT Customer NO-LOCK.
   RUN StarEventMakeModifyEvent(lhCustomer).
   fCleanEventObjects().
END.

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_boolean(top_struct, "result", True).

FINALLY:
   /* Store the transaction id */
   gbAuthLog.TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
