/**
 * Set Customer call itemization deliverables 

 * @input   data;struct; input struct of data

 * @data    customer_id;int;mandatory;customer number
 *          itemizations;array;mandatory;array of deliverables items
            delivery_channel;string;optional;delivery channel
            invoice_target:string;optional;invoice target grouping (allsplit/all_grouped")
            username;string;mandatory;username 
            reason;string;optional;reason of change
 * @itemization    msseq;int;subscripton id   
                   callspec;boolean; call itemization in paper/email invoice
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Func/fmakemsreq.i}
{Mm/subser.i}
{Syst/tmsconst.i}
{Func/femailinvoice.i}
{Mc/invoicetarget.i}

DEF VAR liCount AS INT NO-UNDO.
DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR pcUsername AS CHAR NO-UNDO.
DEF VAR pcReason AS CHAR NO-UNDO.
DEF VAR pcItemsArray AS CHAR NO-UNDO.
DEF VAR pcItemStruct AS CHAR NO-UNDO.
DEF VAR piMsSeq AS INT NO-UNDO.
DEF VAR piStatus AS INT NO-UNDO.
DEF VAR lcParam AS CHAR NO-UNDO.
DEF VAR liValidate AS INT NO-UNDO.
DEF VAR lcError AS CHAR NO-UNDO.
DEF VAR ldActStamp AS DEC NO-UNDO.
DEF VAR ldtReqActDate AS DATE NO-UNDO.
DEF VAR liRequest AS INT NO-UNDO.
DEF VAR piCustNum AS INT NO-UNDO.
DEF VAR liDelType AS INT NO-UNDO.
DEF VAR pcDeliveryChannel AS CHAR NO-UNDO.
DEF VAR pcInvoiceGrouping AS CHAR NO-UNDO.
DEF VAR llUpdate AS LOG NO-UNDO.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
lcStruct = validate_request(pcStruct,"itemizations,delivery_channel,invoice_target,customer_id!,username!,reason").
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUserName = "VISTA_" + get_string(pcStruct,"username").
katun = pcUserName. 

IF LOOKUP("reason",lcStruct) > 0 THEN 
   pcReason = get_string(pcStruct,"reason").

IF LOOKUP("delivery_channel",lcStruct) > 0 THEN 
   pcDeliveryChannel = get_string(pcStruct,"delivery_channel").

IF LOOKUP("invoice_target",lcStruct) > 0 THEN 
   pcInvoiceGrouping = get_string(pcStruct,"invoice_target").

IF LOOKUP("itemizations",lcStruct) > 0 THEN
   pcItemsArray = get_array(pcStruct,"itemizations").

piCustNum = get_int(pcStruct,"customer_id").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND Customer WHERE
     Customer.CustNum = piCustNum NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN 
   RETURN appl_err("Not found customer with custnum " + STRING(piCustNum)).

CASE pcDeliveryChannel:
   WHEN "PAPER"   THEN liDelType = {&INV_DEL_TYPE_PAPER}.
   WHEN "EMAIL"   THEN liDelType = {&INV_DEL_TYPE_EMAIL}.
   WHEN "SMS"     THEN liDelType = {&INV_DEL_TYPE_SMS}.
   WHEN "No Delivery" THEN liDelType = {&INV_DEL_TYPE_NO_DELIVERY}.
   WHEN "" OR WHEN "Email - Waiting for Activation" THEN .
   OTHERWISE RETURN appl_err("Invalid Invoice Delivery Type").
END CASE. /* CASE pcDeliveryChannel: */

IF pcInvoiceGrouping NE "" AND
   pcInvoiceGrouping NE {&INVOICE_TARGET_ALL_SPLIT} AND
   pcInvoiceGrouping NE {&INVOICE_TARGET_ALL_GROUPED} THEN
   RETURN appl_err(SUBST("Unsupported invoice_target value &1", pcInvoiceGrouping)).

{Syst/eventval.i}
&GLOBAL-DEFINE STAR_EVENT_USER katun   
{Func/lib/eventlog.i}
DEF VAR lhCustomer AS HANDLE NO-UNDO. 
lhCustomer = BUFFER Customer:HANDLE.

IF liDelType > 0 AND Customer.DelType <> liDelType THEN DO:
   RUN StarEventInitialize(lhCustomer).
   RUN StarEventSetOldBuffer(lhCustomer).
   FIND CURRENT Customer EXCLUSIVE-LOCK.

   IF liDelType = {&INV_DEL_TYPE_EMAIL} THEN DO:
      IF Customer.Email = "" THEN
         RETURN appl_err("Invoice delivery type to Email can not be " +
                         "changed since customer does not have email address").

      /* If DelType is Email then set to Email Pending first and send
         an email to customer to activate the email service */
      liRequest = fEmailInvoiceRequest(INPUT fMakeTS(),
                                       INPUT TODAY,
                                       INPUT katun,
                                       INPUT 0,
                                       INPUT "",
                                       INPUT Customer.Custnum,
                                       INPUT {&REQUEST_SOURCE_NEWTON},
                                       INPUT Customer.Email,
                                       INPUT 0, /* msseq */
                                       OUTPUT lcError).
      IF liRequest = 0 THEN DO:
         IF lcError = "Customer already has an active request" THEN .
         ELSE RETURN appl_err("Invoice delivery type to Email can not be changed").
      END. /* IF liRequest = 0 THEN DO: */

      /* If Email already validated then mark DelType EMAIL */
      IF liRequest = 1 THEN
         Customer.DelType = {&INV_DEL_TYPE_EMAIL}.
      ELSE
         Customer.DelType = {&INV_DEL_TYPE_EMAIL_PENDING}.
   END. /* IF piDelType = {&INV_DEL_TYPE_EMAIL} THEN DO: */
   ELSE DO:
      Customer.DelType = liDelType.

      /* Cancel Ongoing Email Activation Request (if any) */
      FIND FIRST InvoiceTargetGroup WHERE
                 InvoiceTargetGroup.CustNum = Customer.CustNum AND
                 InvoiceTargetGroup.ToDate >= TODAY AND
                (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
                 InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
           NO-LOCK NO-ERROR.
      IF NOT AVAIL InvoiceTargetGroup AND
         fPendingEmailActRequest(INPUT Customer.Custnum) THEN
         fCancelPendingEmailActRequest(INPUT Customer.Custnum,
                                       INPUT "Invoice Delivery Type is " +
                                    "changed to " + STRING(Customer.DelType)).
      IF liDelType EQ {&INV_DEL_TYPE_NO_DELIVERY} THEN DO:
         FOR EACH MobSub WHERE
                  MobSub.brand EQ gcbrand AND
                  Mobsub.custnum EQ Customer.Custnum NO-LOCK:
            fMakeSchedSMS3(Customer.Custnum,MobSub.CLI,9,
                           "InvDelivTypeChanged",Customer.Language,0,
                           "622","").
         END.
      END.
   END. /* ELSE DO: */

   FIND CURRENT Customer NO-LOCK.
   RUN StarEventMakeModifyEvent(lhCustomer).
   fCleanEventObjects().
   llUpdate = TRUE.
END. /* IF liDelType > 0 AND Customer.DelType <> liDelType */

IF pcInvoiceGrouping > "" THEN DO:
   IF pcInvoiceGrouping EQ {&INVOICE_TARGET_ALL_GROUPED} THEN 
      fGroupAllInvoiceTargets(Customer.Custnum,
                              OUTPUT lcError).
   ELSE IF pcInvoiceGrouping EQ {&INVOICE_TARGET_ALL_SPLIT} THEN
      fSplitAllInvoiceTargets(Customer.Custnum,
                              OUTPUT lcError).
   IF lcError > "" THEN RETURN appl_err("Invoice grouping error: " + lcError).
   llUpdate = TRUE.
END. /* FOR EACH InvoiceTargetGroup NO-LOCK WHERE */


DO liCount = 0 TO get_paramcount(pcItemsArray) - 1:
 
   pcItemStruct = get_struct(pcItemsArray,STRING(liCount)).
   piMsSeq = get_int(pcItemStruct,"msseq").
   piStatus = INT(get_bool(pcItemStruct,"callspec")).
   IF gi_xmlrpc_error NE 0 THEN LEAVE.
   
   FIND FIRST MobSub WHERE
              MobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN NEXT.

   FIND FIRST SubSer WHERE
              SubSer.MsSeq = piMsSeq AND
              SubSer.ServCom = "CALLSPEC" AND
              SubSer.SSDate <= TODAY USE-INDEX ServCom NO-LOCK NO-ERROR.
   IF NOT AVAIL SubSer THEN NEXT.

   /* Check ongoing service requests */
   IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq      = piMsSeq AND
                     MsRequest.ReqType    = ({&REQTYPE_SERVICE_CHANGE}) AND
                     MsRequest.ReqCParam1 = SubSer.ServCom AND
                     MsRequest.ReqStat    < {&REQUEST_STATUS_DONE}) THEN NEXT.
     
   IF piStatus = 1 THEN lcParam = SubSer.SSParam.

   liValidate = fSubSerValidate(
         INPUT piMsSeq,
         INPUT SubSer.ServCom,
         INPUT piStatus,
         OUTPUT lcError).

   IF liValidate NE 0 THEN NEXT.

   /* Do nothing if new value is same as existing value */ 
   IF piStatus EQ SubSer.SSStat THEN NEXT.

   /* check the validity of change date */
    ldActStamp = fServiceActStamp(SubSer.MsSeq,
                                  SubSer.ServCom,
                                  piStatus).
   IF ldActStamp > 0 THEN DO:
      fSplitTS(ldActStamp,
               OUTPUT ldtReqActDate,
               OUTPUT liRequest).

      IF ldtReqActDate > SubSer.SSDate OR
         (DAY(ldtReqActDate) = 1 AND liRequest < TIME - 120 AND
          DAY(SubSer.SSDate) NE 1)
      THEN .
      ELSE ldActStamp = fMakeTS().
   END.
   ELSE ldActStamp = fMakeTS().

   IF ldtReqActDate = TODAY
   THEN ldActStamp = fMakeTS().
   ELSE ldActStamp = fMake2DT(ldtReqActDate,1).

   liRequest = fServiceRequest(SubSer.MsSeq,
                           Subser.ServCom,
                           piStatus,
                           lcParam,
                           ldActStamp,
                           "",
                           FALSE,      /* fees */
                           FALSE,      /* sms */
                           "",
                           {&REQUEST_SOURCE_NEWTON},
                           0, /* father request */
                           false, /* mandatory for father request */
                           OUTPUT lcError).
   IF liRequest = 0 THEN NEXT.

   llUpdate = TRUE.
END.

IF pcReason NE '' AND llUpdate THEN DO:
   CREATE Memo.
   ASSIGN
       Memo.CreStamp  = {&nowTS}
       Memo.Brand     = gcBrand 
       Memo.HostTable = "Customer" 
       Memo.KeyValue  = STRING(piCustNum) 
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = katun 
       Memo.MemoTitle = "Update Deliverables"
       Memo.MemoText  = pcReason
       Memo.CustNum   = piCustNum. 
END.

add_boolean(response_toplevel_id, "", TRUE).


FINALLY:
IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.

