/**
 * Get mnp processes.
 *
 * @input  ids;array of int;mandatory;mnp process ids
 * @output mnpprocess;array of struct;mnp process data
 * @mnpprocess id;int;mnprocess unique id
      mnp_type;int;1=incoming,2=outgoing
      tms_request_id;string;internal mnp process id
      mnp_request_id;string;external mnp process id
      status_code;string;NEW,AENV,ASOL,AREC,ACON,APOR,ACAN,AREC_CLOSED
      status_reason;string;reason code of mnp process in status AREC,AREC_CLOSED,ACAN or with proposed AREC
      status_proposed;string;proposition for the next proces status (AREC_PROPOSED, ACON_PROPOSED, AREC, ACON)
      creation_time;datetime;mnp process creation time in tms
      change_time;datetime;mnp process status change time in tms
      mnp_change_time;datetime;mnp process status change time in nodo central
      operator_code;string;operator code (3 digits)
      operator_name;string;operator name (old operator (IN), new operator (OUT))
      old_payment_type;string;PrePaid/PostPaid
      order_id;int;order id (MNP IN)
      contract_id;string;externa order id (web) (MNP IN)
      msseq;int;subscription id (MNP OUT)
      order_channel;string;orderchannel (MNP IN)
      salesman_id;string;salesman (MNP IN)
      old_payment_method;string;old payment method ('PostPaid' or 'PrePaid) 
      porting_time;datetime;porting time
      msisdns;array of struct;msisdn/icc data
      mnp_messages;array of struct;mnp messages (without xml data)
      reject_history;string;handled rejected process state (closed / relaunched)
      pdfs;array of struct;mnp cancellation proposals
      ongoing_messages;bool;some message is not sent or handled yet
      cancellation_limit;datetime;cancellation deadline
      retention_order;struct;Retention Order Info (MNP OUT)
 * @pdf pdf_id;string;unique id of pdf file
      creation_time;datetime;time of cancellation proposal creation
      status_code;string;NEW,CONFIRMED,REJECTED
 * @msisdns msisdn;string;msisdn number
      icc;string;icc number
      msseq;int;subscription id
      retention_order_id;int;order id
      retention_status_code;string;status code
      retention_creation_time;datetime;order creation time
 * @mnpmessage type;string;mnp message type
      from;string;origin of message "TMS" or "MNP"
      creation_time;datetime;message creation time
      status;int;message status (1 = waiting request sending, 5 = waiting response handling, 10 = response handled)
      id;string;unique mnp message id
      error_code;string;error_code;only returned id error_code is not empty
      error_handled;int;is error handled or not;1=not handled, 2= handled, only returned id error_code is not empty
      error_description;string;error description
 */

{xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
{Syst/tmsconst.i}
{Mnp/mnp.i}

ASSIGN
   katun = "Newton"
   gcBrand = "1".

DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR pcId AS CHAR NO-UNDO. 
DEF VAR pcIdArray AS CHAR NO-UNDO. 
DEF VAR liCounter AS INTEGER NO-UNDO. 
DEF VAR resp_array AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "array") = ? THEN RETURN.
pcIDArray = get_array(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_array = add_array(response_toplevel_id, "").
      
DEFINE VARIABLE lcStructMnpMessage AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcRejectStatus AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCancelProposals AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCancelProposal AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCancelStatus AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProposedStatus AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcArrayMnpMessages AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llOngoingMessages AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lcOperName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llMultiple AS LOGICAL NO-UNDO. 
DEFINE VARIABLE ldeCancelDueStamp AS DECIMAL NO-UNDO.
DEFINE VARIABLE lcRetentionStruct AS CHARACTER NO-UNDO.

DEF BUFFER bMNPProcess FOR MNPProcess.

DEF VAR liId AS INT NO-UNDO. 
DEF VAR lcMsisdns AS CHARACTER NO-UNDO. 
DEF VAR lcMsisdn AS CHARACTER NO-UNDO. 
DEF VAR lcMNPStatus AS CHARACTER NO-UNDO. 

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   liId = get_int(pcIDArray, STRING(liCounter)).

   FIND MNPProcess NO-LOCK WHERE 
        MNPProcess.MNPSeq = liId NO-ERROR.

   IF NOT AVAIL MNPProcess THEN
      RETURN appl_err("MNPProcess not found: "+ pcId).
   
   lcResultStruct = add_struct(resp_array, "").
      
   add_string(lcResultStruct, "operator_code", MNPProcess.OperCode).
   
   FIND MNPOperator WHERE 
        MNPOperator.Brand = gcBrand AND
        MNPOperator.OperCode = MNPProcess.OperCode
   NO-LOCK NO-ERROR.
   
   IF AVAIL MNPOperator THEN DO:
      lcOperName = MNPOperator.OperName.
   END.
   ELSE IF AMBIGUOUS(MNPOperator) THEN DO:
      FIND FIRST MNPOperator WHERE 
                 MNPOperator.Brand = gcBrand AND
                 MNPOperator.OperCode = MNPProcess.OperCode
      NO-LOCK NO-ERROR.
      IF AVAIL MNPOperator THEN lcOperName = MNPOperator.OperBrand.
      ELSE lcOperName = "".
   END.
   ELSE lcOperName = "".
   
   add_string(lcResultStruct, "operator_name", lcOperName). 
   
   /* cancellation limit is at 14:00 of the last day before porting time */
   ldeCancelDueStamp = fCalculateDueDate(MNPProcess.PortingTime, 1).
   add_timestamp(lcResultStruct, "cancellation_limit", ldeCancelDueStamp).
   
   IF MNPProcess.MNPType = {&MNP_TYPE_IN} THEN DO:

      IF MNPProcess.MNPSeq NE {&MNP_PROCESS_DUMMY_IN} THEN DO:
         FIND Order WHERE
              Order.Brand = gcBrand AND
              Order.Orderid = MNPProcess.Orderid NO-LOCK NO-ERROR.
         IF NOT AVAIL Order THEN
            RETURN appl_err("Order not found: " + STRING(MNPProcess.OrderID)).
         
         add_string(lcResultStruct, "old_payment_type", 
                    STRING(Order.OldPayType,"PrePaid/PostPaid")).
         add_int(lcResultStruct, "order_id", Order.OrderId).
         add_string(lcResultStruct, "contract_id", Order.ContractID).
         add_string(lcResultStruct, "salesman_id", Order.Salesman).
         add_string(lcResultStruct, "order_channel", Order.OrderChannel).
      END.
   
      IF MNPProcess.StatusCode = {&MNP_ST_AREC_CLOSED} THEN DO:
         
         IF CAN-FIND(FIRST bMNPProcess NO-LOCK WHERE
                           bMNPProcess.OrderID = MNPProcess.OrderID AND
                           bMNPProcess.MNPSeq > MNPProcess.MNPSeq)
            THEN lcRejectStatus = "relaunched".
            ELSE lcRejectStatus = "closed".
         
         add_string(lcResultStruct, "reject_history", lcRejectStatus).

      END.
   END.
   
   ELSE IF MNPProcess.MNPType = {&MNP_TYPE_OUT} THEN DO:
   
      /* multiple msisdn ? */
      
      llMultiple = FALSE.
      
      FIND MNPSub WHERE
           MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK NO-ERROR.
      
      IF NOT AVAIL MNPSub THEN DO:
         llMultiple = TRUE.
         FIND FIRST MNPSub WHERE
                    MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK NO-ERROR.
      END.
      
      IF NOT AVAIL MNPSub THEN
         RETURN appl_err("MNPSub not found").
      
      IF llMultiple THEN add_string(lcResultStruct , "old_payment_type", "").
      ELSE DO:
         
         /* mobsub does not exist if state is APOR  */
         FIND MobSub WHERE
              MobSub.MsSeq = MNPSub.MsSeq NO-LOCK NO-ERROR.
         
         IF AVAIL Mobsub THEN DO:
            add_string(lcResultStruct , "old_payment_type", STRING(MobSub.PayType,"PrePaid/PostPaid")).
         END.
         ELSE DO:
            FIND TermMobSub WHERE
                 TermMobSub.MsSeq = MNPSub.MsSeq NO-LOCK NO-ERROR.
            IF AVAIL TermMobsub THEN 
               add_string(lcResultStruct , "old_payment_type", STRING(TermMobsub.PayType,"PrePaid/PostPaid")).
            ELSE add_string(lcResultStruct , "old_payment_type", "").
         END.
      END.
      
      add_int(lcResultStruct, "msseq", MNPSub.MsSeq).

      IF MNPProcess.StatusCode = {&MNP_ST_ASOL} THEN DO:
         CASE MNPProcess.StateFlag:
            WHEN {&MNP_STATEFLAG_CONFIRM_PROPOSAL}
               THEN lcProposedStatus = "ACON_PROPOSED".
            WHEN {&MNP_STATEFLAG_REJECT_PROPOSAL}
               THEN lcProposedStatus = "AREC_PROPOSED".
            WHEN {&MNP_STATEFLAG_REJECT}
               THEN lcProposedStatus = "AREC".
            WHEN {&MNP_STATEFLAG_CONFIRM}
               THEN lcProposedStatus = "ACON".
            OTHERWISE lcProposedStatus = "".
         END.
         add_string(lcResultStruct, "proposed_status", lcProposedStatus).
      END.

      FOR FIRST MNPDetails NO-LOCK WHERE
                MNPDetails.MNPSeq = MNPProcess.MNPSeq:
         add_string(lcResultStruct, "id_type", MNPDetails.CustIdType).
         add_string(lcResultStruct, "person_id", MNPDetails.CustId).
      END.
   END.

   /* show reason for rejection, cancellation or rejection proposal */
   IF LOOKUP(STRING(MNPProcess.StatusCode),"4,7,8") > 0 OR
      (MNPProcess.StatusCode <= 2 AND 
       (MNPProcess.StateFlag = {&MNP_STATEFLAG_REJECT_PROPOSAL} OR 
        MNPProcess.StateFlag = {&MNP_STATEFLAG_REJECT})) THEN
      add_string(lcResultStruct, "status_reason", MNPProcess.StatusReason).

   lcCancelProposals = add_array(lcResultStruct,"pdfs").

   FOR EACH MNPCancelProposal WHERE
            MNPCancelProposal.MNPSeq = MNPProcess.MNPSeq NO-LOCK:

      lcCancelProposal = add_struct(lcCancelProposals,"").
      add_string(lcCancelProposal,"pdf_id", MNPCancelProposal.AttachmentFile).
      add_timestamp(lcCancelProposal,
         "creation_time",MNPCancelProposal.CreatedTS).
      
      CASE MNPCancelProposal.StatusCode:
         WHEN {&MNP_CANCEL_PROPOSAL_NEW} THEN lcCancelStatus = "NEW".
         WHEN {&MNP_CANCEL_PROPOSAL_CONFIRMED} THEN lcCancelStatus = "CONFIRMED".
         WHEN {&MNP_CANCEL_PROPOSAL_REJECTED} THEN lcCancelStatus = "REJECTED".
         WHEN {&MNP_CANCEL_PROPOSAL_CANCELLED} THEN lcCancelStatus = "CANCELLED".
         WHEN {&MNP_CANCEL_PROPOSAL_RECIPIENT} THEN lcCancelStatus = "RECIPIENT".
         OTHERWISE lcCancelStatus = STRING(MNPCancelProposal.StatusCode).
      END.

      add_string(lcCancelProposal, "status_code", lcCancelStatus).
   END.

   /* at the moment porting time is same for all msisdn numbers */
   add_timestamp(lcResultStruct, "porting_time", MNPProcess.PortingTime).

   lcMsisdns = add_array(lcResultStruct,"msisdns").
   FOR EACH MNPSub NO-LOCK WHERE
            MNPSub.MNPSeq = MNPProcess.MNPSeq:
      lcMsisdn = add_struct(lcMsisdns,"").
      add_string(lcMsisdn, "msisdn", MNPSub.CLI).
      add_string(lcMsisdn, "icc", MNPSub.ICC).
      add_int(lcMsisdn, "msseq", MNPSub.MsSeq).

      /* Return retention order */
      IF MNPProcess.MNPType = {&MNP_TYPE_OUT} THEN
         FOR LAST Order WHERE
                  Order.MsSeq = MNPSub.MsSeq AND
                  Order.OrderType = {&ORDER_TYPE_RENEWAL} AND
                  Order.OrderChannel BEGINS "Retention" AND
                  Order.CrStamp > MNPProcess.CreatedTS /* YDR-695 */
             NO-LOCK BY Order.CrStamp:
            add_int(lcMsisdn, "retention_order_id", Order.OrderId).
            add_string(lcMsisdn, "retention_status_code", Order.StatusCode).
            add_timestamp(lcMsisdn, "retention_creation_time", Order.CrStamp).
         END. /* FOR LAST Order WHERE */
   END. /* FOR EACH MNPSub NO-LOCK WHERE */

   add_int(lcResultStruct, "id", MNPProcess.MNPSeq). 
   add_int(lcResultStruct, "mnp_type", MNPProcess.MNPType). 
   add_string(lcResultStruct, "mnp_request_id", MNPProcess.PortRequest). 
   
   add_string(lcResultStruct, "tms_request_id", MNPProcess.FormRequest). 
   add_timestamp(lcResultStruct, "change_time", MNPProcess.UpdateTS).
   add_timestamp(lcResultStruct, "mnp_change_time", MNPProcess.MNPUpdateTS).
   add_timestamp(lcResultStruct, "creation_time", MNPProcess.CreatedTS).
   
   FIND TMSCodes WHERE 
        TMSCodes.TableName = "MNPProcess" AND
        TMSCodes.FieldName = "StatusCode" AND
        TMSCodes.CodeGroup = "MNP" AND
        TMSCodes.CodeValue = STRING(MNPProcess.StatusCode)
   NO-LOCK NO-ERROR.
   IF AVAIL TMSCodes THEN lcMNPStatus = TMSCodes.CodeName.
   ELSE lcMNPStatus = STRING(MNPProcess.StatusCode).
   add_string(lcResultStruct, "status_code", lcMNPStatus).
   
   lcArrayMnpMessages = add_array(lcResultStruct, "mnp_messages").
  
   llOngoingMessages = FALSE.
   FOR EACH MNPOperation NO-LOCK WHERE
            MNPOperation.MNPSeq = MnpProcess.MnpSeq:

      lcStructMnpMessage = add_struct(lcArrayMnpMessages, ""). 
      
      add_string(lcStructMnpMessage, "type", MNPOperation.MessageType).
      add_string(lcStructMnpMessage, "from", (IF MNPOperation.Sender = 1 THEN "TMS" ELSE "MNP")).

      add_timestamp(lcStructMnpMessage, "creation_time", MNPOperation.CreatedTs).
      add_int(lcStructMnpMessage, "status", MNPOperation.StatusCode).
      IF MNPOperation.ErrorCode NE "" THEN DO:
         add_string(lcStructMnpMessage, "error_code", MNPOperation.ErrorCode).
         add_int(lcStructMnpMessage, "error_handled", MNPOperation.ErrorHandled).
         add_string(lcStructMnpMessage, "error_description", MNPOperation.ErrorDesc).
      END.
      add_string(lcStructMnpMessage, "id", STRING(MNPOperation.MNPOperationID)).
      
      IF MNPOperation.StatusCode < 10 THEN llOngoingMessages = TRUE.

   END.

   add_boolean(lcResultStruct, "ongoing_messages", llOngoingMessages).
 
END.
 
FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
