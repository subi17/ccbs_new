/* fcreditreq.i         29.08.07/aam 

   create a credit note request 
*/
&IF "{&FCREDITREQ_I}" NE "YES" 
&THEN
&GLOBAL-DEFINE FCREDITREQ_I YES
   
{fcreatereq.i}
{cparam2.i}
{faccper.i}
{fcreditvalid.i}
{fparse.i}
{Func/date.i}

FUNCTION fFullCreditNoteRequest RETURNS INTEGER
   (INPUT  iiCustNum       AS INT,  
    INPUT  iiInvNum        AS INT,
    INPUT  pcSubInvoices   AS CHAR,
    INPUT  pcInvRowDetails AS CHAR,
    INPUT  icReasonCode    AS CHAR,   /* reason code for crediting */
    INPUT  icReasonNote    AS CHAR,   /* reason note for crediting */
    INPUT  iiReturn        AS INT,    /* money return; none,to balance,refund */
    INPUT  ilRelease       AS LOG,    /* release events */
    INPUT  idActStamp      AS DEC,    /* when request should be handled */
    INPUT  ilSendSMS       AS LOG,
    INPUT  icCreator       AS CHAR,
    INPUT  icMode          AS CHAR,
    OUTPUT ocResult        AS CHAR).

   DEF VAR liReqCreated AS INT NO-UNDO.

   ocResult = fChkRequest(iiCustNum,
                          22,
                          STRING(iiInvNum),
                          pcSubInvoices).

   IF ocResult > "" THEN RETURN 0.                       

   /* set activation time */
   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = fMakeTS().

   fCreateRequest(22,
                  idActStamp,
                  icCreator,
                  FALSE,      /* fees */
                  ilSendSMS).

   ASSIGN 
      bCreaReq.CustNum    = iiCustNum
      bCreaReq.ReqIParam1 = iiInvNum
      bCreaReq.ReqIParam2 = INTEGER(ilRelease)
      bCreaReq.ReqIParam4 = iiReturn
      bCreaReq.ReqCParam1 = icReasonCode
      bCreaReq.ReqCParam2 = icReasonNote
      bCreaReq.ReqCParam3 = icMode
      bCreaReq.ReqCParam4 = pcSubInvoices
      bCreaReq.ReqCParam5 = pcInvRowDetails
      liReqCreated        = bCreaReq.MsRequest.

   RELEASE bCreaReq.
   
   RETURN liReqCreated.
     
END FUNCTION.

FUNCTION fFullCreditNote RETURNS INT
(piInvNum AS INTEGER,
 pcSubInvoices AS CHARACTER,
 pcInvRowDetails AS CHARACTER,
 pcReasonGrp AS CHARACTER,
 pcReason AS CHARACTER,
 pcReasonNote AS CHARACTER,
 OUTPUT ocError AS CHARACTER):
   
   DEF VAR piReturnPayment AS INT NO-UNDO INITIAL 0.
   DEF VAR plReleaseEvents AS LOG NO-UNDO INITIAL FALSE.

   DEF VAR ldInvDate       AS DATE NO-UNDO INITIAL TODAY.
   DEF VAR ldActTime       AS DEC  NO-UNDO.
   DEF VAR liActTime       AS INT  NO-UNDO.
   DEF VAR ldActStamp      AS DEC  NO-UNDO.
   DEF VAR lii             AS INT  NO-UNDO. 
   DEF VAR liCounter       AS INT  NO-UNDO. 
   DEF VAR liNumEntries    AS INT  NO-UNDO.
   DEF VAR liSubInvoiceNum AS INT  NO-UNDO.
   DEF VAR liInvRowNum     AS INT  NO-UNDO.
   DEF VAR liInvRowAmt     AS DEC  NO-UNDO.
   DEF VAR lcInvRowDetail  AS CHAR NO-UNDO.
   DEF VAR lcInvRowNumList AS CHAR NO-UNDO.
   DEF VAR lcCreditMode    AS CHAR NO-UNDO.

   DEF BUFFER Invoice FOR Invoice.
   DEF BUFFER SubInvoice FOR SubInvoice.
   DEF BUFFER TMSCodes FOR TMSCodes.
   
   /* check invoice */
   FIND Invoice WHERE 
        Invoice.InvNum = piInvNum  NO-LOCK NO-ERROR.
   IF NOT AVAIL Invoice THEN DO:
      ocError = "Invalid invoice number".
      RETURN 0.
   END.

   liNumEntries = NUM-ENTRIES(pcSubInvoices).
   DO liCounter = 1 TO liNumEntries:

      liSubInvoiceNum = INT(ENTRY(liCounter, pcSubInvoices)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         ocError = "Incorrect SubInvNum data format".
         RETURN 0.
      END.
      FIND SubInvoice WHERE
           SubInvoice.InvNum = piInvNum AND
           SubInvoice.SubInvNum = liSubInvoiceNum NO-LOCK NO-ERROR.
      IF NOT AVAIL SubInvoice THEN DO:
         ocError = "Invalid subinvoice number".
         RETURN 0.
      END.
   END.

   liNumEntries = NUM-ENTRIES(pcInvRowDetails).
   DO liCounter = 1 TO liNumEntries:
      lcInvRowDetail = ENTRY(liCounter, pcInvRowDetails).

      liInvRowNum = INT(fParseKVP("InvRow",lcInvRowDetail,"|")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         ocError = "Incorrect InvRow data format".
         RETURN 0.
      END.

      liInvRowAmt = DEC(fParseKVP("InvRowAmt",lcInvRowDetail,"|")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         ocError = "Incorrect InvRowAmt data format".
         RETURN 0.
      END.

      FIND FIRST InvRow WHERE
                 InvRow.InvNum    = piInvNum AND
                 InvRow.InvRowNum = liInvRowNum NO-LOCK NO-ERROR.
      IF NOT AVAIL InvRow THEN DO:
         ocError = "Invalid invrow number: " + STRING(liInvRowNum) +
         ", InvN:" + STRING(piInvNum). 
         RETURN 0.
      END.

      lcInvRowNumList = lcInvRowNumList + "," + STRING(liInvRowNum).
   END.

   lcInvRowNumList = TRIM(lcInvRowNumList,",").
   
   /* check reason */
   IF pcReason = "" THEN DO:
      ocError = "Unknown reason code".
      RETURN 0.
   END.

   IF pcReasonGrp > "" THEN DO:

      FIND TMSCodes WHERE
           TMSCodes.TableName = "CreditNote" AND
           TMSCodes.FieldName = "ReasonGrp"  AND
           TMSCodes.CodeValue = pcReasonGrp NO-LOCK NO-ERROR.
      IF AVAILABLE TMSCodes AND 
         LOOKUP(STRING(Invoice.InvType),TMSCodes.ConfigValue) = 0 THEN DO:
         ocError = "Category is not allowed for this invoice type".
         RETURN 0.
      END.
   
   END.
      
   FIND TMSCodes WHERE
        TMSCodes.TableName = "CreditNote" AND
        TMSCodes.FieldName = "Reason"     AND
        TMSCodes.CodeValue = pcReason NO-LOCK NO-ERROR.
   IF AVAILABLE TMSCodes THEN DO:
      IF pcReasonGrp > "" AND 
         TMSCodes.CodeGroup NE pcReasonGrp THEN DO:
         ocError = "Reason belongs to another category".
         RETURN 0.
      END.
   END.
   ELSE DO:
      ocError = "Unknown reason code".
      RETURN 0.
   END.

   /* check invoice date period - not in use? */
   IF fPeriodLocked(ldInvDate,FALSE) THEN DO:
      ocError = "Period for " + STRING(ldInvDate,"99.99.9999") + " is locked." +
                 " No new events can be posted or old events changed" +
                 " on this period before releasing it.".
      RETURN 0.
   END.
   
   ocError = fCheckInvoice(BUFFER Invoice,
                           pcSubInvoices,
                           lcInvRowNumList,
                           OUTPUT lcCreditMode).
   IF ocError NE "" THEN RETURN 0.
    
   /* action time */
   ASSIGN 
      ldActTime  = fCParamDE("CreditNoteActTime").   
   IF ldActTime = ?  THEN ldActTime  = 19.
   liActTime = TRUNCATE(ldActTime,0) * 3600 +
               (ldActTime - TRUNCATE(ldActTime,0)) * 6000.
   /* move to next day if activation time has passed */
   IF liActTime < TIME AND ldInvDate = TODAY THEN ldInvDate = ldInvDate + 1.
   /* requests will be run in the evening of given date */
   ldActStamp = fMake2DT(ldInvDate,liActTime).

   /* make a request */
   lii = fFullCreditNoteRequest(Invoice.Custnum,
                                Invoice.InvNum,
                                pcSubInvoices,
                                pcInvRowDetails,
                                pcReason,
                                pcReasonNote,
                                piReturnPayment,
                                plReleaseEvents,
                                ldActStamp,
                                FALSE,
                                "",
                                lcCreditMode,
                                OUTPUT ocError).
   IF lii > 0 THEN RETURN lii.
   ELSE DO:
      ocError = "Creation of request failed: " + ocError.
      RETURN 0.
   END.

END FUNCTION. 

FUNCTION fOrderPickingStarted RETURNS LOGICAL
   (iiOrderID AS INTEGER):

   DEFINE BUFFER OrderDelivery FOR OrderDelivery.

   FOR
      FIRST OrderDelivery NO-LOCK WHERE
         OrderDelivery.Brand   = gcBrand   AND
         OrderDelivery.OrderID = iiOrderID AND
         OrderDelivery.LOStatusId = {&DEXTRA_PICKING_STARTED_STATUS}:
      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fCloseGiftFATime RETURNS LOGICAL
   (iiOrderID AS INTEGER):

   DEF BUFFER FATime  FOR FATime.
   DEF BUFFER Invoice FOR Invoice.

   DEFINE VARIABLE lcOrderID AS CHARACTER NO-UNDO.
   lcOrderID = STRING(iiOrderID).

   FOR
      EACH FATime NO-LOCK WHERE
         FATime.Brand     = gcBrand                         AND
         FATime.HostTable = "Order"                         AND
         FATime.KeyValue  = lcOrderID                       AND
         FATime.FTGrp     = {&FATGROUP_WELCOME_GIFT},
      FIRST Invoice NO-LOCK WHERE
         Invoice.InvNum   = FATime.InvNum                   AND
         Invoice.InvType NE 99 USE-INDEX InvNum_s:
      RETURN FALSE.
   END.

   FOR
      EACH FATime EXCLUSIVE-LOCK WHERE
         FATime.Brand     = gcBrand                         AND
         FATime.HostTable = "Order"                         AND
         FATime.KeyValue  = lcOrderID                       AND
         FATime.FTGrp     = {&FATGROUP_WELCOME_GIFT}:

      FATime.LastPeriod = fPrevPeriod(FATime.Period).

   END.

   RETURN TRUE.

END.

FUNCTION fCashInvoiceCreditNote RETURNS CHARACTER
(  BUFFER pbOrder FOR Order,
   icReason AS CHAR):

   DEFINE VARIABLE lcError AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcShippingCostBillCode AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSubInvNums           AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcInvRowDetails        AS CHARACTER NO-UNDO.
  
   FIND Invoice WHERE Invoice.InvNum = pbOrder.InvNum NO-LOCK NO-ERROR.

   IF AVAILABLE Invoice AND
                Invoice.CrInvNum = 0 AND
               (Invoice.InvType = 6 OR
                Invoice.InvType = 7)
   THEN DO:
   
      lcError = fCheckCreditNoteRequest(Invoice.CustNum,
                                        Invoice.InvNum).
      /* no action, if request is ongoing */
      IF lcError NE "" THEN RETURN "".

      IF pbOrder.FeeModel = {&ORDER_FEEMODEL_SHIPPING_COST}
      THEN DO:

         IF pbOrder.PayType = FALSE /* PostPaid */
         THEN fCloseGiftFATime(pbOrder.OrderID).

         IF fOrderPickingStarted(pbOrder.OrderId)
         THEN DO:
            FOR
               FIRST FMItem NO-LOCK WHERE
                  FMItem.Brand    = gcBrand AND
                  FMItem.FeeModel = {&ORDER_FEEMODEL_SHIPPING_COST}:
               lcShippingCostBillCode = FMItem.BillCode.
            END.

            IF lcShippingCostBillCode > ""
            THEN DO:
               FOR
                  EACH InvRow NO-LOCK WHERE
                     InvRow.InvNum   =  Invoice.InvNum         AND
                     InvRow.BillCode NE lcShippingCostBillCode:
                  IF LOOKUP(STRING(InvRow.SubInvNum), lcSubInvNums) = 0
                  THEN lcSubInvNums = lcSubInvNums + "," + STRING(InvRow.SubInvNum).

                  lcInvRowDetails = lcInvRowDetails + "," +
                                    "InvRow=" + STRING(InvRow.InvRowNum) + "|" +
                                    "InvRowAmt=" + REPLACE(STRING(InvRow.Amt),",",".").
               END.

               /* lcInvRowDetail is empty when the order is linked both
                  LO and Yoigo internal invoices. I.e. then Yoigo internal
                  invoice contains only shipping cost fee. If picking is started
                  we must not do a credit note in this case. */
               IF lcInvRowDetails > ""
               THEN fFullCreditNote(Invoice.InvNum,
                                    LEFT-TRIM(lcSubInvNums,","),
                                    LEFT-TRIM(lcInvRowDetails,","),
                                    "Order",  /*reason group*/
                                    icReason, /*reason*/
                                    "",       /*reason note*/
                                    OUTPUT lcError).
            END.
         END.
      END.

      /* There are no shipping cost available or the picking is not started
         or there are no active billing item for a shipping cost */
      IF lcShippingCostBillCode = ""
      THEN fFullCreditNote(Invoice.InvNum,
                           "",
                           "",
                           "Order",
                           icReason,
                           "",
                           OUTPUT lcError).
   END.

   RETURN lcError.

END FUNCTION. 

&ENDIF
