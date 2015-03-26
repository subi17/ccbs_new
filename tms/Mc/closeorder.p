/* closeorder.p    

   changed:         22.11.06/aam fMarkOrderStamp, ask verification
                    30.11.06/aam validations
                    01.02.07 kl  allow close when invoice is credited

*/
   
{commali.i}
{eventval.i}
{timestamp.i}
{forderstamp.i}
{msreqfunc.i}
{orderfunc.i}
{tmsconst.i}
{fcreditreq.i}
{ordercancel.i}
{msisdn.i}

DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.
DEF INPUT PARAMETER ilSilent AS LOG NO-UNDO.

DEF VAR llOk AS LOG NO-UNDO.
DEF VAR lcError AS CHARACTER NO-UNDO.
DEF VAR lcQuestion AS CHARACTER NO-UNDO. 
DEF VAR lcCreditReason AS CHARACTER NO-UNDO. 

FIND Order WHERE 
     Order.Brand   = gcBrand AND 
     Order.OrderID = iiOrder EXCLUSIVE-LOCK NO-ERROR.

IF not avail order THEN DO:
   lcError = "Unknown order ID " + STRING(iiorder).
   IF NOT ilSilent THEN MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
   RETURN lcError.
END.

IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES} + ",12") > 0 THEN DO:
   lcError = SUBST("Cannot close order with status &1",Order.StatusCode).
   IF NOT ilSilent THEN   
      MESSAGE lcError
      VIEW-AS ALERT-BOX INFORMATION.
   RETURN lcError.
END. 

llOk = FALSE.
   
FOR EACH MNPProcess WHERE
         MNPProcess.OrderId = Order.OrderId AND
         MNPProcess.MNPType = {&MNP_TYPE_IN} NO-LOCK:
   
   IF MNPProcess.StatusCode NE {&MNP_ST_AREC} AND
      MNPProcess.StatusCode NE {&MNP_ST_AREC_CLOSED} AND
      MNPProcess.StatusCode NE {&MNP_ST_ACAN} AND
      MNPProcess.StatusCode NE {&MNP_ST_APOR} THEN DO:
      
      lcError = "Cannot close order. Ongoing MNP process".
      IF NOT ilSilent THEN   
         MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
      RETURN lcError.
   END.
END.
   
IF NOT ilSilent THEN DO:   
   
   IF Order.Invnum > 0 THEN
      lcQuestion = "Are you sure you want to close the Order and Create credit note to the Cash invoice?".
   ELSE lcQuestion = "Do you want to mark this order closed?".

   MESSAGE lcQuestion
      VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   TITLE " ORDER " + STRING(Order.OrderID) + " "
   SET llOk.

   IF NOT llOk THEN RETURN "".
END.

IF Order.InvNum > 0 THEN DO:
   
   IF Order.MNPStatus = 5 THEN lcCreditReason = "1012".
   ELSE IF Order.MNPStatus = 0 THEN lcCreditReason = "1010".
   ELSE DO:
      IF CAN-FIND(FIRST MNPProcess NO-LOCK WHERE
           MNPProcess.OrderId = Order.OrderId AND
           MNPProcess.StatusCode = {&MNP_ST_ACAN} AND
           MNPProcess.StatusReason = "CANC_ABONA") THEN
           lcCreditReason = "1010".
      ELSE lcCreditReason = "1011".
   END.
   
   lcError = fCashInvoiceCreditNote(
      Order.Invnum,
      lcCreditReason).

   IF lcError NE "" THEN DO:
      IF NOT ilSilent THEN MESSAGE lcError VIEW-AS ALERT-BOX.
      /* used in Vista and translated (do not change) */
      RETURN "Order cannot be closed because of unsuccessful Credit note request creation.". 
   END.

END.

/* ongoing 'renewal pos stc' must be cancelled */
IF LOOKUP(Order.OrderChannel,"renewal_pos_stc,retention_stc") > 0 THEN DO:

   FIND FIRST MsRequest WHERE 
              MsRequest.MsSeq = Order.Msseq AND
              MsRequest.ReqType = 0 AND
              LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9") = 0 NO-LOCK NO-ERROR.

   IF AVAIL MsRequest THEN DO:
      
      IF NOT ilSilent THEN DO:
         MESSAGE 
            "This will cancel ongoing subcription type change request\n"
            "Do you still want to continue?"
         VIEW-AS ALERT-BOX QUESTION
         BUTTONS YES-NO
         TITLE " ORDER " + STRING(Order.OrderID) + " "
         SET llOk.
         
         IF NOT llOk THEN RETURN "".
      END.

      IF fChkReqStatusChange(4) EQ FALSE AND
         NOT (MsRequest.ReqStatus EQ 3 AND
              MsRequest.Memo MATCHES "*Pending renewal order*") THEN DO:
         lcError = "Cannot cancel STC request. Order cancellation is not possible".
         IF NOT ilSilent THEN 
            MESSAGE lcError 
            VIEW-AS ALERT-BOX
            TITLE " ORDER " + STRING(Order.OrderID) + " ".
         RETURN lcError.
      END.
      ELSE fReqStatus(4,SUBST("Cancelled " + Order.OrderChannel +
                      " order &1", Order.OrderId)).
   END.
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {lib/eventlog.i}
      
   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).
   RUN StarEventSetOldBuffer(lhOrder).
END.               

IF Order.OrderType EQ {&ORDER_TYPE_NEW} AND
  (Order.StatusCode EQ {&ORDER_STATUS_OFFER_SENT} OR
   Order.StatusCode EQ {&ORDER_STATUS_RESIGNATION}) THEN DO:
   
   FIND FIRST MSISDN WHERE
              MSISDN.Brand = gcBrand AND
              MSISDN.CLI   = Order.CLI
   EXCLUSIVE-LOCK NO-ERROR.
   
   IF AVAIL MSISDN AND MSISDN.StatusCode EQ 2 THEN DO:
      fMakeMsidnHistory(INPUT RECID(MSISDN)).
      ASSIGN
         MSISDN.StatusCode = 4
         MSISDN.CustNum    = 0.
      RELEASE MSISDN.
   END.
   
   If Order.StatusCode EQ {&ORDER_STATUS_RESIGNATION} AND
      Order.ICC > "" THEN DO:

      FIND SIM EXCLUSIVE-LOCK WHERE
           SIM.ICC = Order.ICC AND
           SIM.SimStat = 20 AND 
           SIM.MsSeq = Order.Msseq NO-ERROR.
      IF AVAIL SIM THEN 
         SIM.SIMStat = {&SIM_SIMSTAT_AVAILABLE}.
      RELEASE SIM.
   END.
END.

fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_CLOSED}).

IF llDoEvent THEN
   RUN StarEventMakeModifyEvent(lhOrder).

FOR EACH MNPProcess WHERE
   MNPProcess.OrderID = Order.OrderId AND
   MNPProcess.MNPType = {&MNP_TYPE_IN} AND
   MNPProcess.StatusCode = {&MNP_ST_AREC} EXCLUSIVE-LOCK:
   ASSIGN
      MNPProcess.UpdateTS = fMakeTS()
      MNPProcess.StatusCode = {&MNP_ST_AREC_CLOSED}.
END.

/* YDR-16 and YDR-415 */
IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 AND
   Order.OrderType <= 1 THEN DO:

   FIND SIM WHERE
        SIM.ICC = Order.ICC AND
        SIM.SimStat = 4 NO-LOCK NO-ERROR.
   IF AVAIL SIM AND SIM.Stock = "RETAILER" THEN DO:
      IF LOOKUP(STRING(Order.MNPStatus),"1,5,8") > 0 OR
         Order.OrderType = 0 THEN
         fReleaseSIM(Order.OrderId).
   END. /* IF AVAIL SIM THEN DO: */
END.

/* Move the SIM Status to LOST if rollback MNP order is closed */
IF Order.OrderType = 3 THEN DO:
   FIND SIM WHERE
        SIM.ICC = Order.ICC AND
        SIM.SimStat = 4 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL SIM THEN SIM.SimStat = 7.
END. /* IF Order.OrderType = 3 THEN DO: */

/* Release SIM if renewal order is made with ICC change */
IF Order.OrderType = 2 AND
   NOT Order.OrderChannel BEGINS "RENEWAL_POS" THEN DO:
   FIND SIM WHERE
        SIM.ICC = Order.ICC AND
        SIM.SimStat = 13 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL SIM THEN
      SIM.SIMStat = {&SIM_SIMSTAT_AVAILABLE}.
END. /* IF Order.OrderType = 2 AND */

fMarkOrderStamp(Order.OrderID,
                "Close",
                0.0).

IF llDoEvent THEN fCleanEventObjects().

RUN cancelorder.p(Order.OrderId,TRUE).

RETURN "".

