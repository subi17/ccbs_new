/* ----------------------------------------------------------------------
  MODULE .......: fusion_bob.p
  TASK .........: Back door tools: Automate fusion order data/status changes
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 01.10.13
  Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".

{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/date.i}
{Syst/eventval.i}
{Func/msreqfunc.i}
{Func/orderfunc.i}
{Mc/orderfusion.i}
{Func/fixedlinefunc.i}
{Func/fsubstermreq.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun 
   {Func/lib/eventlog.i}
   DEFINE VARIABLE lhOrderFusion AS HANDLE NO-UNDO.
   lhOrderFusion = BUFFER OrderFusion:HANDLE.
   RUN StarEventInitialize(lhOrderFusion).
END.

DEF VAR lcLine AS CHARACTER NO-UNDO.
DEF VAR lcSep AS CHARACTER NO-UNDO INIT ";".
DEF VAR liNumOK AS INTEGER NO-UNDO. 
DEF VAR liNumErr AS INTEGER NO-UNDO. 

/* files and dirs */
DEF VAR lcLogFile AS CHAR NO-UNDO. 
DEF VAR lcFileName AS CHARACTER NO-UNDO. 
DEF VAR lcIncDir  AS CHARACTER NO-UNDO. 
DEF VAR lcInputFile AS CHARACTER NO-UNDO. 
DEF VAR lcProcDir AS CHARACTER NO-UNDO. 
DEF VAR lcProcessedFile AS CHARACTER NO-UNDO. 
DEF VAR lcSpoolDir AS CHARACTER NO-UNDO. 
DEF VAR lcReportFileOut AS CHARACTER NO-UNDO. 
DEF VAR lcOutDir AS CHARACTER NO-UNDO. 
DEF VAR lcRootDir AS CHAR NO-UNDO. 
DEF VAR liEntries AS INT NO-UNDO. 

/* field variables */
DEF VAR liOrderId AS INT NO-UNDO. 
DEF VAR lcFixedOrderID AS CHAR NO-UNDO. 
DEF VAR lcOldStatus AS CHAR NO-UNDO. 
DEF VAR lcNewStatus AS CHAR NO-UNDO. 
DEF VAR lcFixedNumber AS CHAR NO-UNDO. 
DEF VAR lcFixedStatus AS CHAR NO-UNDO. 
DEF VAR lcFixedSubStatus AS CHAR NO-UNDO. 
DEF VAR lcExternalTicket AS CHAR NO-UNDO. 
DEF VAR lcReleaseMobile AS CHAR NO-UNDO. 

ASSIGN
   lcRootDir = fCParam("OrderFusionBob","RootDir").

IF NOT lcRootDir > "" THEN RETURN.

ASSIGN
   lcIncDir  = lcRootDir + "incoming/incoming/" 
   lcProcDir = lcRootDir + "incoming/processed/"
   lcSpoolDir = lcRootDir + "outgoing/spool/"
   lcOutDir   = lcRootDir + "outgoing/outgoing/".

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine lcSep 
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).
END FUNCTION.

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
 
   lcInputFile = lcIncDir + lcFileName.
   
   IF SEARCH(lcInputFile) NE ? THEN 
      INPUT STREAM sin FROM VALUE(lcInputFile).
   ELSE NEXT.

   ASSIGN
      liNumOk = 0
      liNumErr = 0.
   
   fBatchLog("START", lcInputFile).
   lcLogFile = lcSpoolDir + lcFileName + ".LOG".
   OUTPUT STREAM sLog TO VALUE(lcLogFile) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.
  
   LINE_LOOP:
   REPEAT:
      
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF TRIM(lcLine) EQ "" THEN NEXT.

      ASSIGN 
         liEntries   = NUM-ENTRIES(lcLine,lcSep)
         liOrderId    = INTEGER(ENTRY(1,lcLine,lcSep))
         lcOldStatus  = ENTRY(2,lcLine,lcSep)        
         lcNewStatus  = ENTRY(3,lcLine,lcSep)        
         lcFixedOrderID = ENTRY(4,lcLine,lcSep)        
         lcFixedNumber = ENTRY(5,lcLine,lcSep)
         lcFixedStatus = ENTRY(6,lcLine,lcSep)
         lcFixedSubStatus = ENTRY(7,lcLine,lcSep)
         lcExternalTicket = ENTRY(8,lcLine,lcSep)
         lcReleaseMobile = ENTRY(9,lcLine,lcSep)
         NO-ERROR.
 
      IF ERROR-STATUS:ERROR OR liEntries NE 9 THEN DO:
         fError("Incorrect input data format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.
     
      RUN pUpdateFusionOrder(liOrderId,
                             lcOldStatus,
                             lcNewStatus,
                             lcFixedOrderID,
                             lcFixedNumber,
                             lcFixedStatus,
                             lcFixedSubStatus,
                             lcExternalTicket,
                             lcReleaseMobile).

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         fError(ENTRY(2,RETURN-VALUE,":")).
         liNumErr = liNumErr + 1 .
      END.
      ELSE liNumOK = liNumOK + 1 .
   END.
  
   PUT STREAM sLog UNFORMATTED 
       "input: " STRING(liNumOK + liNumErr) ", "
       "updated: " STRING(liNumOK) ", "
       "errors: " STRING(liNumErr) SKIP.
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir).  
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).

END.

INPUT STREAM sFile CLOSE.

IF llDoEvent THEN fCleanEventObjects().

PROCEDURE pUpdateFusionOrder:

   DEF INPUT PARAM piOrderId AS INT NO-UNDO. 
   DEF INPUT PARAM pcOldStatus AS CHAR NO-UNDO.
   DEF INPUT PARAM pcNewStatus AS CHAR NO-UNDO.
   DEF INPUT PARAM pcFixedOrderID AS CHAR NO-UNDO.
   DEF INPUT PARAM pcFixedNumber AS CHAR NO-UNDO.
   DEF INPUT PARAM pcFixedStatus AS CHAR NO-UNDO.
   DEF INPUT PARAM pcFixedSubStatus AS CHAR NO-UNDO.
   DEF INPUT PARAM pcExternalTicket AS CHAR NO-UNDO.
   DEF INPUT PARAM pcReleaseMobile AS CHAR NO-UNDO.

   DEF VAR liTermReason  AS INTEGER   NO-UNDO.
   DEF VAR ldeTS         AS DECIMAL   NO-UNDO.
   DEF VAR liReq         AS INTEGER   NO-UNDO.
   DEF VAR llYoigoCLi    AS LOGICAL   NO-UNDO.
   DEF VAR llMasmovilCLi AS LOGICAL   NO-UNDO.
   DEF VAR liMsisdnStat  AS INTEGER   NO-UNDO.
   DEF VAR liSimStat     AS INTEGER   NO-UNDO.
   DEF VAR liQuarTime    AS INTEGER   NO-UNDO.
   DEF VAR llPenaltyFee  AS LOGICAL   NO-UNDO.
   DEF VAR lcResult      AS CHARACTER NO-UNDO.
   DEF VAR liRequest     AS INT       NO-UNDO. 
   DEF VAR lcError       AS CHAR      NO-UNDO.

   IF pcReleaseMobile > "" AND
      pcReleaseMobile NE "Y" THEN
      RETURN "ERROR:Incorrect release mobile order parameter".

   /* find order */   
   FIND Order NO-LOCK WHERE 
        Order.Brand = gcBrand AND
        Order.OrderId = piOrderId NO-ERROR.
   IF NOT AVAILABLE Order THEN 
      RETURN "ERROR:Invalid Order ID".

   FIND OrderFusion NO-LOCK WHERE
        OrderFusion.Brand = gcBrand AND 
        OrderFusion.OrderID = Order.OrderID NO-ERROR.
   IF NOT AVAILABLE OrderFusion THEN 
      RETURN "ERROR:Order type is not Fusion".

   IF OrderFusion.FusionStatus NE pcOldStatus THEN
      RETURN SUBST("ERROR:CurrentFusionPackageStatus does not match with real value &1", OrderFusion.FusionStatus).

   IF pcNewStatus > "" AND 
      LOOKUP(pcNewStatus,"NEW,ONG,PCAN,CAN,PFIN,FIN") = 0 THEN
      RETURN "ERROR:Unsupported NewFusionPackageOrderStatus".

   IF pcFixedStatus > "" AND
      LOOKUP(pcFixedStatus,"NEW,PROCESSED,INCIDENT,INSTALLED,CANCELLED") = 0
      THEN RETURN "ERROR:Unsupported Fixed_line_order_status".
   
   IF pcNewStatus EQ OrderFusion.FusionStatus THEN .
   ELSE IF pcNewStatus EQ "CAN" THEN DO:
   
      IF pcReleaseMobile EQ "Y" THEN
         RETURN "ERROR:Mobile order release not allowed with CAN".

      IF fIsConvergenceTariff(order.clitype) AND
         OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_ONGOING} THEN
         RETURN "ERROR:CAN not allowed because fusion status ONG".

      IF (Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} AND
          NOT fIsConvergenceTariff(order.clitype)) OR
         Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_1} OR
         Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_2} OR
         Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_3} OR
         Order.StatusCode EQ {&ORDER_STATUS_IN_CONTROL} OR
         Order.StatusCode EQ {&ORDER_STATUS_MNP_REJECTED} OR
         Order.StatusCode EQ {&ORDER_STATUS_MORE_DOC_NEEDED}
         OR
         (fIsConvergenceTariff(order.clitype) AND
          Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} AND
         (OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_ERROR} OR
          OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_CANCELLED} OR
          OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_NEW})) 
         OR
         (fIsConvergenceTariff(order.clitype) AND
          Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL})
         THEN DO:

         RUN Mc/closeorder.p(Order.OrderId,TRUE).

         IF RETURN-VALUE NE "" THEN
            RETURN "ERROR:Order closing failed: " + STRING(RETURN-VALUE).
      END.
      ELSE IF fIsConvergenceTariff(order.clitype) AND
              Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} THEN DO:

         IF fCreateFusionCancelOrderMessage(OrderFusion.OrderID,
                                            OUTPUT lcError) EQ FALSE THEN
            RETURN SUBST("ERROR:Cancel message creation failed: &1", lcError).
      END.
      ELSE IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0 THEN DO:
         
         IF Order.OrderType EQ {&ORDER_TYPE_STC} THEN DO:
            FIND FIRST MsRequest NO-LOCK WHERE
                       MsRequest.MsSeq = Order.MsSeq AND
                       MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                       MsRequest.Actstamp > fMakeTs() AND
                       MsRequest.ReqIParam2 = Order.OrderID NO-ERROR.
            IF NOT AVAIL MsRequest OR MsRequest.ReqStatus EQ 4
               THEN fSetOrderStatus(Order.OrderID,"7").
            ELSE IF MsRequest.ReqStatus EQ 2 THEN RETURN "ERROR:Cannot cancel STC request".
            ELSE DO:
               fSetOrderStatus(Order.OrderID,"7").
               fReqStatus(4,"Cancelled by Fusion order status change").
            END.
         END.
         ELSE RETURN "ERROR:Order closing not allowed".

      END.
      ELSE IF LOOKUP(Order.StatusCode,{&ORDER_STATUS_DELIVERED}) > 0 THEN DO:

         RUN Mm/fusion_stc_fallback.p(Order.OrderId, OUTPUT liRequest).

         IF liRequest = 0 AND 
            RETURN-VALUE NE "Subscription type is not Fusion" AND
            RETURN-VALUE NE "Subscription not found" THEN
            RETURN "ERROR:Fallback STC request creation failed, " + 
               STRING(RETURN-VALUE).
      END.
   END.
   ELSE IF pcNewStatus EQ "FIN" THEN DO:
      
      IF pcReleaseMobile EQ "Y" THEN
         RETURN "ERROR:Mobile order release not allowed with FIN".
      
      IF LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) = 0 AND
         Order.StatusCode NE {&ORDER_STATUS_IN_CONTROL} AND 
         Order.StatusCode NE {&ORDER_STATUS_PENDING_FIXED_LINE} AND
         Order.StatusCode NE {&ORDER_STATUS_ROI_LEVEL_1} AND
         Order.StatusCode NE {&ORDER_STATUS_ROI_LEVEL_2} AND
         Order.StatusCode NE {&ORDER_STATUS_ROI_LEVEL_3} AND
         Order.StatusCode NE {&ORDER_STATUS_MORE_DOC_NEEDED} THEN .
      ELSE IF (Order.OrderType EQ {&ORDER_TYPE_STC} OR Order.ICC > "" OR
               (Order.OrderChannel <> "Fusion_POS" AND Order.ICC = "")) AND
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} THEN DO:

         RUN Mc/orderinctrl.p(Order.OrderId, 0, TRUE).
         IF RETURN-VALUE > "" THEN
            RETURN "ERROR:Mobile order release failed: " + STRING(RETURN-VALUE).
      END.
      ELSE RETURN "ERROR:Fusion order status change not allowed".
   END.

   IF pcReleaseMobile EQ "Y" THEN DO:
      IF (NOT (Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} AND
             (Order.OrderType = {&ORDER_TYPE_STC} OR Order.ICC > "" OR
              (Order.OrderChannel <> "Fusion_POS" AND Order.ICC = "")))) AND
         (NOT (fIsConvergenceTariff(order.clitype) AND 
              Order.StatusCode EQ {&ORDER_STATUS_PENDING_MOBILE_LINE}))
         THEN RETURN "ERROR:Mobile order release not allowed".
      ELSE IF fIsConvergenceTariff(order.clitype) AND
              Order.StatusCode EQ {&ORDER_STATUS_PENDING_MOBILE_LINE} THEN DO:
         fSetOrderStatus(Order.OrderID,"7").
         ASSIGN
            liTermReason  = {&SUBSCRIPTION_TERM_REASON_DIRECT_ORDER_CANCELATION}
            llYoigoCLI    = fIsYoigoCLI(order.CLI)
            llMasmovilCLi = fIsMasmovilCLI(order.CLI)
            llPenaltyFee  = fIsPenalty(liTermReason,Order.MsSeq).
         fInitialiseValues(
            INPUT  liTermReason,
            INPUT  llYoigoCLi,
            INPUT  llMasmovilCLi,
            OUTPUT liMsisdnStat,
            OUTPUT liSimStat,
            OUTPUT liQuarTime).
         liReq = fTerminationRequest(
                        Order.MsSeq,
                        fSecOffSet(fMakeTS(),5), /* when request handled */
                        liMsisdnStat, /* msisdn status code: available */
                        liSimStat, /* sim status code : available */
                        liQuarTime, /* quarantie time */
                        INT(llPenaltyFee), /*penalty fee calcultion */
                        "", /* opcode */
                        STRING(liTermReason),
                        {&REQUEST_SOURCE_ORDER_CANCELLATION},
                        "",
                        0,
                        {&TERMINATION_TYPE_PARTIAL},
                        OUTPUT lcResult). 
      END.
      ELSE DO:
         RUN Mc/orderinctrl.p(Order.OrderId, 0, TRUE).
         IF RETURN-VALUE > "" THEN RETURN "ERROR:Mobile order release failed".
      END.
   END.

   FIND CURRENT OrderFusion EXCLUSIVE-LOCK.

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrderFusion).

   ASSIGN
      OrderFusion.FusionStatus   = UPPER(pcNewStatus) WHEN pcNewStatus > ""
      OrderFusion.FixedOrderID   = pcFixedOrderID WHEN pcFixedOrderID > ""
      OrderFusion.FixedNumber    = pcFixedNumber WHEN pcFixedNumber > ""
      OrderFusion.FixedStatus    = UPPER(pcFixedStatus) WHEN pcFixedStatus > ""
      OrderFusion.FixedSubStatus = pcFixedSubStatus WHEN pcFixedSubStatus > ""
      OrderFusion.ExternalTicket = pcExternalTicket WHEN pcExternalTicket > ""
      OrderFusion.UpdateTS       = fMakeTS().

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrderFusion).

   RELEASE OrderFusion.

   RETURN "".
END.
