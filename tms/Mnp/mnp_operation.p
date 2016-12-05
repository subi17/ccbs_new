/* ----------------------------------------------------------------------
MODULE .......: mnp_operation.p 
TASK .........: Handle manual mnp operations from web and cui
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 27.10.09
Version ......: xfera
----------------------------------------------------------------------- */

DEFINE INPUT PARAMETER piMNPSeq AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pcOperation AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcParam AS CHAR NO-UNDO.

DEFINE VARIABLE llResponse AS LOGICAL NO-UNDO.
DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
DEFINE VARIABLE liPeriods AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldaDueDate AS DATE NO-UNDO. 
DEFINE VARIABLE llOrderClosed AS LOGICAL NO-UNDO. 
DEF VAR liLanguage AS INT NO-UNDO. 

DEF BUFFER bMNPOngoing FOR MNPProcess.

{commali.i}
{mnpmessages.i}
{mnp.i}
{tmsconst.i}
{forderstamp.i}
{orderfunc.i}
{log.i}
{eventval.i}
{ordercancel.i}

FIND MNPProcess WHERE
     MNPProcess.MNPSeq = piMNPSeq NO-LOCK NO-ERROR.

IF NOT AVAIL MNPProcess THEN RETURN "ERROR:Unknown process".

FOR EACH MNPOperation WHERE
   MNPOperation.MNPSeq = MNPProcess.MNPSeq AND
   MNPOperation.StatusCode < 10 NO-LOCK:
   RETURN "ERROR:Ongoing messages".
END.

CASE pcOperation:
   
   /* Cancellation Rules:
      1) Only MNP IN process can be cancelled
      2) MNP Process status must be Sent, Requested or Confirmed
      3) MNP Cancellation cannot be done too late 
        (after last day at 14:00 before porting time)
   */
   WHEN "cancel" THEN DO:
      
      IF MNPProcess.MNPType NE {&MNP_TYPE_IN} THEN
         RETURN "ERROR:Only MNP IN process can be cancelled".

      IF LOOKUP(STRING(MNPProcess.StatusCode),"0,1,2,5") = 0 THEN
         RETURN "ERROR:Cannot cancel MNPProcess with current status".
            
      FIND Order WHERE
         Order.Brand = gcBrand AND
         Order.OrderID = MNPProcess.OrderId NO-LOCK.

      IF MNPProcess.StatusCode = {&MNP_ST_NEW} THEN DO:

         IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0 THEN 
            RETURN "ERROR:Cancellation failed. Order is already delivered or closed".

         IF CAN-FIND(FIRST bMNPOngoing WHERE
                           bMNPOngoing.OrderId = MNPProcess.OrderId AND
             LOOKUP(STRING(bMNPOngoing.StatusCode),"0,1,2,5") > 0 AND
                     ROWID(bMNPOngoing) NE ROWID(MNPProcess) NO-LOCK) THEN DO:
            RETURN "ERROR:Cancellation failed. Another process is ongoing".
         END.
            
         FIND CURRENT MNPProcess EXCLUSIVE-LOCK.

         llOrderClosed = fSetOrderStatus(Order.OrderId,"7").
         /* Mark the timestamp as close */
         if llOrderClosed then fMarkOrderStamp(Order.OrderID,"Close",0.0).
         IF NOT llOrderClosed THEN RETURN "ERROR:Order closing failed".
         
         /* YDR-16 */
         IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 AND
            Order.ICC > "" THEN DO:
          
            FIND SIM WHERE
                 SIM.ICC = Order.ICC AND
                 SIM.SimStat = 4 NO-LOCK NO-ERROR.
            
            IF AVAIL SIM AND SIM.Stock = "RETAILER" THEN
               fReleaseSIM(Order.OrderID).
            
            /* YDR-70 */
            fReleaseIMEI(Order.OrderId).
         END.
      
         ASSIGN         
            MNPProcess.UpdateTS = fMakeTS()
            MNPProcess.StatusCode = {&MNP_ST_ACAN}
            MNPProcess.StatusReason = pcParam
            Order.MNPStatus = MNPProcess.StatusCode + 1.

         RELEASE MNPProcess.
         FIND CURRENT Order NO-LOCK.

         /*YPR-5316:release COFF retention or COFF stc if MNP out is cancelled*/
         IF Order.StatusCode EQ {&ORDER_STATUS_MNP_RETENTION} AND
            (fIsConvergenceTariff(Order.CliType) OR
             Order.OrderChannel BEGINS "retention") THEN
            RUN orderinctrl.p(Order.OrderId, 0, TRUE).   
         ELSE
            RUN cancelorder.p(Order.OrderId, TRUE).

         llResponse = TRUE.

      END.
      ELSE DO: 
         
         IF MNPProcess.PortingTime > 0 THEN DO:
               
            liPeriods = 1.

            IF liPeriods > fMNPPeriods(
               input fMakeTS(),
               input MNPProcess.PortingTime,
               INPUT 0,
               OUTPUT ldaDueDate) THEN
               RETURN "ERROR:Too late to cancel".
         END.

         llResponse = fSendCancellation(MNPProcess.PortRequest,
            pcParam,
            FALSE,
            OUTPUT lcResponse).

         IF llResponse THEN DO:
            FIND CURRENT MNPProcess EXCLUSIVE-LOCK.
            MNPProcess.StatusReason = pcParam.
            FIND CURRENT MNPProcess NO-LOCK.
         END.
      END.

   END.

   WHEN "reject" OR WHEN "confirm" THEN DO:

      IF MNPProcess.MNPType NE {&MNP_TYPE_OUT} THEN
         RETURN "ERROR:Only MNP OUT process can be rejected or confirmed".
      
      IF MNPProcess.StatusCode NE {&MNP_ST_ASOL} THEN
         RETURN "ERROR:Cannot reject MNP process with current status".
      
      IF MNPProcess.StateFlag = {&MNP_STATEFLAG_NOT_ANALYSED} THEN
         RETURN "ERROR:MNP process is not yet analysed".

      IF MNPProcess.StateFlag = {&MNP_STATEFLAG_REJECT} THEN
         RETURN "ERROR:MNP process is allready set as rejected".
      
      FIND CURRENT MNPProcess EXCLUSIVE-LOCK.
      ASSIGN   
         MNPProcess.StatusReason = pcParam
         MNPProcess.StateFlag = (
            IF pcOperation EQ "reject" THEN {&MNP_STATEFLAG_REJECT} 
            ELSE {&MNP_STATEFLAG_CONFIRM} ).
      FIND CURRENT MNPProcess NO-LOCK.
      llResponse = True.
   END.

   WHEN "close" THEN DO:

      IF MNPProcess.MNPType NE {&MNP_TYPE_IN} THEN
         RETURN "ERROR:Only MNP IN process can be closed".

      IF MNPProcess.statuscode NE {&MNP_ST_AREC} THEN
         RETURN "ERROR:Only rejected process can be closed".

      FIND Order WHERE
           Order.Brand = gcBrand AND
           Order.OrderID = MNPProcess.OrderId NO-LOCK.

      IF Order.StatusCode NE "73" THEN RETURN "ERROR:Order is in wrong status".

      RUN closeorder.p(Order.Orderid, TRUE).
      IF RETURN-VALUE NE "" THEN RETURN "ERROR:" + RETURN-VALUE.

      IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 AND
         LOOKUP(MNPProcess.StatusReason,
               "AREC ENUME,RECH_BNUME,AREC EXIST,RECH_IDENT,RECH_ICCID") > 0
         THEN DO:
      
         FIND FIRST OrderCustomer OF Order NO-LOCK WHERE
                    OrderCustomer.RowType = 1 NO-ERROR.
         IF AVAIL OrderCustomer THEN
            liLanguage = INT(OrderCustomer.Language) NO-ERROR.

         fMNPCallAlarm("MNPRejectedPosClose",
                      0.0,
                      MNPProcess.FormRequest,
                      Order.CLI,
                      Order.CustNum,
                      liLanguage,
                      "800622600",
                      Order.OrderId).
         
      END.

      llResponse = True.
   END.
   
   WHEN "confirm_cancel_proposal" THEN DO:
      
      IF MNPProcess.MNPType NE {&MNP_TYPE_IN} THEN
         RETURN "ERROR:MNPProcess is wrong type".

      IF NOT fIsNCTime() THEN RETURN "ERROR:Not NC online time".
            
      liPeriods = 1. /* YDR-115 */

      IF liPeriods > fMNPPeriods(
         input fMakeTS(),
         input MNPProcess.PortingTime,
         INPUT 0,
         OUTPUT ldaDueDate) THEN
         RETURN "ERROR:Too late to cancel".
      
      llResponse = fSendCancellation(MNPProcess.PortRequest,
         "CANC_ABONA",
         TRUE,
         OUTPUT lcResponse).
      
      /* set cancel proposal status to confirm */
      IF llResponse = TRUE THEN DO:
         
         FIND CURRENT MNPProcess EXCLUSIVE-LOCK.
         MNPProcess.StatusReason = "CANC_ABONA".
         FIND CURRENT MNPProcess NO-LOCK.
         
         FOR EACH MNPCancelProposal WHERE
            MNPCancelProposal.MnpSeq = MNPProcess.MNPSeq AND
            MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_NEW} EXCLUSIVE-LOCK:
            MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_CONFIRMED}.
         END.
      END.
      
   END.

   WHEN "reject_cancel_proposal" THEN DO:
      /* set cancel proposal status to confirm */
      FOR EACH MNPCancelProposal WHERE
         MNPCancelProposal.MnpSeq = MNPProcess.MNPSeq AND
         MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_NEW} EXCLUSIVE-LOCK:
         MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_REJECTED}.
      END.
      llResponse = True.
   END.

   WHEN "cancelarSolicitudBajaNumeracionMovil" THEN DO:
      IF NOT fSendNumberTerminationCancel(MNPProcess.PortRequest) THEN
         lcResponse = "MNP Baja cancellation request failed".
      ELSE llResponse = True.
   END.
   
   OTHERWISE RETURN "ERROR:Unknown operation: " + pcOperation.

END.

IF llDoEvent THEN fCleanEventObjects().

IF lcResponse NE "" THEN RETURN "ERROR:" + lcResponse.
RETURN STRING(llResponse,"OK/ERROR:Something went wrong").
