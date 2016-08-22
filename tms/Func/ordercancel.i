/* ----------------------------------------------------------------------
  MODULE .......: ordercancel.i 
  TASK .........: Order cancellation functions
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 23.01.11
  Version ......: Yoigo
----------------------------------------------------------------------- */
&IF "{&ordercancel}" NE "YES"
&THEN

&GLOBAL-DEFINE ordercancel YES
{commali.i}
{eventval.i} 
{tmsconst.i}
{timestamp.i}
{fcreditvalid.i}
{fcreditreq.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun 
   {lib/eventlog.i}
END.

DEFINE TEMP-TABLE ttInvoice NO-UNDO
       FIELD InvNum         AS INT
       FIELD SubInvNum      AS INT
       FIELD InvRowDetail   AS CHAR.

DEFINE TEMP-TABLE ttInvRow NO-UNDO
       FIELD InvRowNum      AS INT
       FIELD Amt            AS DEC.

FUNCTION fReleaseIMEI RETURNS LOGICAL
   (iiOrderId AS INT):

   FIND FIRST OrderAccessory WHERE
              OrderAccessory.Brand = gcBrand AND
              OrderAccessory.OrderId = iiOrderId AND
              OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE})
   EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL OrderAccessory AND OrderAccessory.IMEI NE "" THEN DO:
      
      IF llDoEvent THEN DO:
         DEFINE VARIABLE lhOrderAccessory AS HANDLE NO-UNDO.
         lhOrderAccessory = BUFFER OrderAccessory:HANDLE.
         RUN StarEventInitialize(lhOrderAccessory).
         RUN StarEventSetOldBuffer(lhOrderAccessory).
      END.
      
      OrderAccessory.IMEIStatus = ({&IMEI_STATUS_TO_BE_RELEASED}).

      IF llDoEvent THEN
         RUN StarEventMakeModifyEvent(lhOrderAccessory).

      RELEASE OrderAccessory.
   END.

END FUNCTION.


FUNCTION fReleaseSIM RETURNS LOGICAL
   (iiOrderId AS INT):

   DEF BUFFER ActionLog FOR ActionLog.
   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER Order FOR Order.
   DEF BUFFER SIM FOR SIM.
   
   FIND Order WHERE
        Order.Brand = gcBrand AND
        Order.OrderId = iiOrderId NO-LOCK NO-ERROR.
   IF NOT AVAIL Order THEN RETURN FALSE.

   FIND SIM WHERE
      SIM.ICC = Order.ICC EXCLUSIVE-LOCK NO-ERROR.

   IF NOT AVAIL SIM THEN RETURN False.

   CREATE ActionLog.
   ASSIGN
      ActionLog.ActionTS     = fMakeTS()
      ActionLog.Brand        = gcBrand  
      ActionLog.TableName    = "Order"  
      ActionLog.KeyValue     = STRING(Order.Orderid)
      ActionLog.UserCode     = katun
      ActionLog.ActionID     = "SIMRELEASE"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
      ActionLog.ActionStatus = 0
      ActionLog.ActionChar   = SIM.ICC.

   FIND FIRST MsRequest WHERE
              MsRequest.MsSeq     = Order.MsSeq AND
              MsRequest.ReqType   = 13          AND
      LOOKUP(STRING(MsRequest.ReqStatus),"4,9") = 0
   NO-LOCK NO-ERROR.

   IF AVAIL MSRequest THEN DO:
      
      ASSIGN
         ActionLog.ActionStatus = 1
         ActionLog.ActionChar   = SIM.ICC + "|" + "Ongoing subscription activation".
         
   END.
   ELSE DO:
      
      ASSIGN
         SIM.SIMStat = {&SIM_SIMSTAT_AVAILABLE}
         ActionLog.ActionStatus = 2
         ActionLog.ActionChar = SIM.ICC.
   END.
      
   RELEASE SIM.

   RETURN True.

END FUNCTION.

PROCEDURE pCreditInstallment:

   DEF INPUT PARAM iiFFNum AS INT NO-UNDO.
   DEF INPUT PARAM ilCreditCommission AS LOG NO-UNDO.
   DEF INPUT PARAM iiOrigRequest AS INT NO-UNDO.

   DEF VAR lcError AS CHAR NO-UNDO. 
   DEF VAR liReq AS INT NO-UNDO. 
   DEF VAR liMsSeq AS INT NO-UNDO. 
   DEF VAR ldeCreditAmt AS DEC NO-UNDO. 
   DEF VAR lcInvRows AS CHAR NO-UNDO. 

   DEF BUFFER Invoice FOR Invoice.
   DEF BUFFER SubInvoice FOR SubInvoice.
   DEF BUFFER InvRow FOR InvRow.
   DEF BUFFER FixedFee FOR FixedFee.
   DEF BUFFER SingleFee FOR SingleFee.
   DEF BUFFER bCreditRequest FOR MsRequest.

   FIND FixedFee NO-LOCK WHERE
        FixedFee.FFNum = iiFFNum NO-ERROR.
   IF NOT AVAIL FixedFee THEN RETURN.

   IF FixedFee.HostTable NE "MobSub" THEN RETURN.

   liMsSeq = INT(FixedFee.KeyValue) NO-ERROR.
   IF ERROR-STATUS:ERROR OR liMsSeq EQ 0 THEN RETURN.

   FOR EACH FFItem NO-LOCK WHERE
            FFItem.FFNum = FixedFee.FFNum AND
            FFItem.Billed = TRUE AND
            FFItem.InvNum > 0,
      FIRST Invoice NO-LOCK WHERE
            Invoice.InvNum = FFItem.Invnum AND
            Invoice.InvType = 1,
      FIRST SubInvoice NO-LOCK WHERE
            SubInvoice.InvNum = Invoice.InvNum AND
            SubInvoice.MsSeq = liMsSeq:

      /* there's no unique link between billed FFItem and InvRow */
      FOR EACH InvRow NO-LOCK WHERE
          InvRow.InvNum = Invoice.InvNum AND
          InvRow.SubInvNum = SubInvoice.SubInvNum AND
          InvRow.BillCode = FFItem.BillCode AND
          InvRow.CreditInvNum = 0 AND
          InvRow.Amt >= FFItem.Amt AND
          LOOKUP(STRING(InvRow.InvRowNum),lcInvRows) = 0:

         IF InvRow.OrderId > 0 AND
            FixedFee.OrderID > 0 AND
            InvRow.OrderId NE FixedFee.OrderId THEN NEXT.

         ASSIGN
            lcInvRows = lcInvRows + "," + STRING(InvRow.InvRowNum)
            ldeCreditAmt = MIN(InvRow.Amt,FFItem.Amt).

         FIND FIRST ttInvoice WHERE
                    ttInvoice.InvNum = Invoice.InvNum AND
                    ttInvoice.SubInvNum = SubInvoice.SubInvNum NO-ERROR.

         IF NOT AVAIL ttInvoice THEN DO:
            CREATE ttInvoice.
            ASSIGN ttInvoice.InvNum       = Invoice.InvNum
                   ttInvoice.SubInvNum    = SubInvoice.SubInvNum.
         END. /* IF NOT CAN-FIND (FIRST ttInvoice WHERE */
               
         ttInvoice.InvRowDetail = ttInvoice.InvRowDetail + "," + 
                                  "InvRow=" + STRING(InvRow.InvRowNum) + "|" +
                                  "InvRowAmt=" + STRING(ldeCreditAmt).
         LEAVE.
      END.
   END.
      
   FIND SingleFee USE-INDEX Custnum WHERE
        SingleFee.Brand       = gcBrand AND
        SingleFee.Custnum     = FixedFee.CustNum AND
        SingleFee.HostTable   = FixedFee.HostTable AND
        SingleFee.KeyValue    = FixedFee.KeyValue AND
        SingleFee.SourceTable = "FixedFee" AND
        SingleFee.SourceKey   = STRING(FixedFee.FFNum) AND
        SingleFee.BillCode BEGINS "PAYTERMCG" AND
        SingleFee.Billed      = TRUE AND
        SingleFee.InvNum      > 0 NO-LOCK NO-ERROR.

   IF AVAIL SingleFee AND ilCreditCommission THEN DO:
  
      FOR FIRST Invoice NO-LOCK WHERE
                Invoice.InvNum = SingleFee.InvNum AND
                Invoice.InvType = 1,
          FIRST SubInvoice NO-LOCK WHERE
                Subinvoice.InvNum = Invoice.InvNum AND
                Subinvoice.MsSeq = liMsSeq:

         
         FOR EACH InvRow NO-LOCK WHERE
                  InvRow.InvNum = Invoice.InvNum AND
                  InvRow.SubInvNum = SubInvoice.SubInvNum AND
                  InvRow.BillCode = SingleFee.BillCode AND
                  InvRow.CreditInvNum = 0 AND
                  InvRow.Amt >= SingleFee.Amt:
         
            IF InvRow.OrderId > 0 AND
               SingleFee.OrderID > 0 AND
               InvRow.OrderId NE SingleFee.OrderId THEN NEXT.
         
            ldeCreditAmt = MIN(InvRow.Amt,SingleFee.Amt).

            FIND FIRST ttInvoice WHERE
                       ttInvoice.InvNum = Invoice.InvNum AND
                       ttInvoice.SubInvNum = SubInvoice.SubInvNum NO-ERROR.

            IF NOT AVAIL ttInvoice THEN DO:
               CREATE ttInvoice.
               ASSIGN ttInvoice.InvNum       = Invoice.InvNum
                      ttInvoice.SubInvNum    = SubInvoice.SubInvNum.
            END. /* IF NOT CAN-FIND (FIRST ttInvoice WHERE */
                  
            ttInvoice.InvRowDetail = ttInvoice.InvRowDetail + "," + 
                                     "InvRow=" + STRING(InvRow.InvRowNum) + "|" +
                                     "InvRowAmt=" + STRING(ldeCreditAmt).
            LEAVE.
         END.
      END.
   END.
          
   /* Create credit note */
   FOR EACH ttInvoice NO-LOCK:
      liReq = fFullCreditNote(ttInvoice.InvNum,
                              STRING(ttInvoice.SubInvNum),
                              TRIM(ttInvoice.InvRowDetail,","),
                              "Correct",
                              "2013",
                              "",
                              OUTPUT lcError).
      IF liReq = 0 THEN
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "MobSub",
                          STRING(liMsSeq),
                          FixedFee.Custnum,
                          "CREDIT NOTE CREATION FAILED",
                          "ERROR:" + lcError). 
      ELSE DO TRANS:
         FIND FIRST bCreditRequest WHERE
                    bCreditRequest.MsRequest = liReq
              EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL bCreditRequest THEN
            bCreditRequest.OrigRequest = iiOrigRequest.
      END.
   END. /* FOR EACH ttInvoice NO-LOCK: */
   EMPTY TEMP-TABLE ttInvoice.

END PROCEDURE. 

PROCEDURE pCreatePaytermCreditNote:

   DEF INPUT PARAM iiOrderId AS INT NO-UNDO.

   DEF VAR lcInvRowDetail AS CHAR NO-UNDO. 
   DEF VAR lcError AS CHAR NO-UNDO. 
   DEF VAR liReq AS INT NO-UNDO. 

   DEF BUFFER Order FOR Order.
   DEF BUFFER Invoice FOR Invoice.
   DEF BUFFER SubInvoice FOR SubInvoice.
   DEF BUFFER InvRow FOR InvRow.
   DEF BUFFER BillItem FOR BillItem.

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand = gcBrand AND
              Order.OrderID = iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN RETURN.

   /* collect billed installments + commission fees */
   FOR EACH Invoice NO-LOCK WHERE
            Invoice.Brand = gcBrand AND
            Invoice.Custnum = Order.Custnum AND
            Invoice.InvType = {&INV_TYPE_NORMAL},
      FIRST SubInvoice OF Invoice NO-LOCK WHERE
            SubInvoice.MsSeq = Order.MsSeq:

      lcInvRowDetail = "".
      FOR EACH InvRow NO-LOCK WHERE
               InvRow.InvNum = SubInvoice.InvNum AND
               InvRow.SubInvNum = SubInvoice.SubInvNum AND
               InvRow.CreditInvNum = 0 AND
               InvRow.BillCode BEGINS "PAYTERM",
         FIRST BillItem NO-LOCK WHERE
               BillItem.Brand = gcBrand AND
               BillItem.BillCode = InvRow.BillCode AND
               BillItem.BiGroup = "33":

         lcInvRowDetail = lcInvRowDetail + "," +
                          "InvRow=" + STRING(InvRow.InvRowNum) + "|" +
                          "InvRowAmt=" + STRING(InvRow.Amt).
      END.
         
      IF lcInvRowDetail > "" THEN DO:
         
         lcInvRowDetail = TRIM(lcInvRowDetail,",").

         CREATE ttInvoice.
         ASSIGN ttInvoice.InvNum       = Invoice.InvNum
                ttInvoice.SubInvNum    = SubInvoice.SubInvNum
                ttInvoice.InvRowDetail = lcInvRowDetail.
      END.
   END.
          
   /* Create credit note */
   FOR EACH ttInvoice NO-LOCK:
      liReq = fFullCreditNote(ttInvoice.InvNum,
                              STRING(ttInvoice.SubInvNum),
                              ttInvoice.InvRowDetail,
                              "Correct",
                              "2013",
                              "",
                              OUTPUT lcError).
      IF liReq = 0 THEN
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "MobSub",
                          STRING(Order.MsSeq),
                          Order.Custnum,
                          "CREDIT NOTE CREATION FAILED",
                          "ERROR:" + lcError). 
   END. /* FOR EACH ttInvoice NO-LOCK: */
   EMPTY TEMP-TABLE ttInvoice.

END PROCEDURE. 

PROCEDURE pCreateRenewalCreditNote:

   DEF INPUT PARAM iiOrderId AS INT NO-UNDO.
   DEF INPUT PARAM icSingleFeeIds AS CHAR NO-UNDO.

   DEF VAR lcError AS CHAR NO-UNDO. 
   DEF VAR liReq AS INT NO-UNDO. 
   DEF VAR liCustnum AS INT NO-UNDO. 

   DEF BUFFER Order FOR Order.
   DEF BUFFER FixedFee FOR FixedFEe.
   DEF BUFFER FFItem FOR FFItem.
   DEF BUFFER Invoice FOR Invoice.
   DEF BUFFER SubInvoice FOR SubInvoice.
   DEF BUFFER InvRow FOR InvRow.
   DEF BUFFER SingleFee FOR SingleFee.
   DEF VAR i AS INT NO-UNDO. 
   DEF VAR liFMItemId AS INT NO-UNDO. 
   DEF VAR ldeCreditAmt LIKE InvRow.Amt. 

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand = gcBrand AND
              Order.OrderID = iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN RETURN.

   FIND FIRST MobSub NO-LOCK WHERE
              Mobsub.MsSeq = Order.MsSeq NO-ERROR.

   IF NOT AVAIL Mobsub THEN DO:
      FIND FIRST TermMobSub NO-LOCK WHERE
                 TermMobSub.MsSeq = Order.MsSeq NO-ERROR.
      IF NOT AVAIL TermMobSub THEN RETURN.
      liCustnum = TermMobSub.Custnum.
   END.
   ELSE liCustnum = MobSub.Custnum.

   IF Order.Custnum NE liCustnum THEN DO:

      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = gcBrand
             ErrorLog.ActionID  = "ORDERCANCEL"
             ErrorLog.TableName = "Order"
             ErrorLog.KeyValue  = STRING(Order.OrderId) 
             ErrorLog.ErrorMsg  = "Credit note not created due to ACC"
             ErrorLog.UserCode  = katun
             ErrorLog.ActionTS  = fMakeTS().
   END.

   FIND FIRST FixedFee NO-LOCK WHERE
              FixedFee.Brand = gcBrand AND
              FixedFee.Custnum = Order.Custnum AND
              FixedFee.HostTable = "MobSub" AND
              FixedFee.KeyValue = STRING(Order.MsSeq) AND
              FixedFee.OrderId = Order.OrderId AND
              FixedFee.BillCode = "PAYTERM" NO-ERROR.

   IF AVAIL FixedFee THEN
   /* Check billed Fixed Fee Items */
   FOR EACH FFItem OF FixedFee WHERE
            FFItem.Billed = TRUE NO-LOCK,
      FIRST Invoice WHERE
            Invoice.InvNum  = FFItem.InvNum AND
            Invoice.InvType = {&INV_TYPE_NORMAL} NO-LOCK,
      FIRST SubInvoice OF Invoice WHERE
            SubInvoice.MsSeq = Order.MsSeq NO-LOCK:

      FOR FIRST InvRow NO-LOCK WHERE
                InvRow.InvNum = SubInvoice.InvNum AND
                InvRow.SubInvNum = SubInvoice.SubInvNum AND
                InvRow.BillCode = FFItem.BillCode AND
                InvRow.CreditInvNum = 0:

         ldeCreditAmt = MIN(InvRow.Amt,FFItem.Amt). 
         
         FIND FIRST ttInvoice WHERE
                    ttInvoice.InvNum = Invoice.InvNum AND
                    ttInvoice.SubInvNum = SubInvoice.SubInvNum NO-ERROR.

         IF NOT AVAIL ttInvoice THEN DO:
            CREATE ttInvoice.
            ASSIGN ttInvoice.InvNum       = Invoice.InvNum
                   ttInvoice.SubInvNum    = SubInvoice.SubInvNum.
         END. /* IF NOT CAN-FIND (FIRST ttInvoice WHERE */
               
         ttInvoice.InvRowDetail = ttInvoice.InvRowDetail + "," + 
                                  "InvRow=" + STRING(InvRow.InvRowNum) + "|" +
                                  "InvRowAmt=" + STRING(ldeCreditAmt).

      END. /* FOR EACH InvRow OF SubInvoice WHERE */
            
   END. /* FOR EACH FFItem OF FixedFee WHERE */

   /* Credit note for old paytermend,termperiod fees */
   IF icSingleFeeIds > "" THEN DO:

      DO i = 1 TO NUM-ENTRIES(icSingleFeeIds,","):

         liFMItemId = INT(ENTRY(i,icSingleFeeIds)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN NEXT.

         FIND FIRST SingleFee NO-LOCK WHERE
                    SingleFee.Brand = gcBrand AND
                    SingleFee.FMItemId = liFMItemId NO-ERROR.
        
         IF NOT AVAIL SingleFee OR NOT SingleFee.Billed THEN NEXT.

         FOR FIRST Invoice NO-LOCK WHERE
                   Invoice.InvNum = SingleFee.InvNum AND
                   Invoice.InvType = {&INV_TYPE_NORMAL},
             FIRST SubInvoice OF Invoice NO-LOCK WHERE
                   SubInvoice.MsSeq = Order.MsSeq,
             FIRST InvRow NO-LOCK WHERE
                   InvRow.InvNum = SubInvoice.InvNum AND
                   InvRow.SubInvNum = SubInvoice.SubInvNum AND
                   InvRow.BillCode = SingleFee.BillCode AND
                   InvRow.CreditInvNum = 0:
            
            ldeCreditAmt = MIN(InvRow.Amt, SingleFee.Amt).
         
            FIND FIRST ttInvoice WHERE
                       ttInvoice.InvNum = Invoice.InvNum AND
                       ttInvoice.SubInvNum = SubInvoice.SubInvNum NO-ERROR.

            IF NOT AVAIL ttInvoice THEN DO:
               CREATE ttInvoice.
               ASSIGN ttInvoice.InvNum     = Invoice.InvNum
                      ttInvoice.SubInvNum  = SubInvoice.SubInvNum.
            END.

            ttInvoice.InvRowDetail = ttInvoice.InvRowDetail + "," +
                             "InvRow=" + STRING(InvRow.InvRowNum) + "|" +
                             "InvRowAmt=" + STRING(ldeCreditAmt).
         END.
      END.
      
   END.

   /* Create credit note */
   FOR EACH ttInvoice:

      ttInvoice.InvRowDetail = TRIM(ttInvoice.InvRowDetail,",").
      
      liReq = fFullCreditNote(ttInvoice.InvNum,
                              STRING(ttInvoice.SubInvNum),
                              ttInvoice.InvRowDetail,
                              "Correct",
                              "2013",
                              "",
                              OUTPUT lcError).
      IF liReq = 0 THEN
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "MobSub",
                          STRING(Order.MsSeq),
                          Order.Custnum,
                          "CREDIT NOTE CREATION FAILED",
                          "ERROR:" + lcError). 
   END. /* FOR EACH ttInvoice NO-LOCK: */

   EMPTY TEMP-TABLE ttInvoice.

END PROCEDURE. 


&ENDIF
