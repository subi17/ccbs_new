USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpTimeBased.

{HPD/HPDConst.i}
{Syst/tmsconst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.OrderDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbOrder          FOR Order.
   DEFINE PRIVATE BUFFER lbOrderAccessory FOR OrderAccessory.
   DEFINE PRIVATE BUFFER lbOrderPayment   FOR OrderPayment.
   DEFINE PRIVATE BUFFER lbOrderCustomer  FOR OrderCustomer.
   DEFINE PRIVATE BUFFER lbMNPProcess     FOR MNPProcess.
   
   DEFINE PRIVATE VARIABLE lcDeliveryTime AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE lcChangeTime   AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE lcCloseTime    AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE lcOnlySimAmt   AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE lcTopupScheme  AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE lcTerm         AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE lcPayTerm      AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE ldeTermLeasAmt AS DECIMAL   NO-UNDO.
   DEFINE PRIVATE VARIABLE lcAccessories  AS CHARACTER NO-UNDO. /* APIBSS-282 */       
  
   CONSTRUCTOR PUBLIC OrderDump
      (iiDumpID AS INTEGER):
               
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).      
      objDumpBase:mUseTable(BUFFER lbOrder:HANDLE).
      objDumpBase:mUseTable(BUFFER lbOrderAccessory:HANDLE).
      objDumpBase:mUseTable(BUFFER lbOrderPayment:HANDLE).
      objDumpBase:mUseTable(BUFFER lbOrderCustomer:HANDLE).
      objDumpBase:mUseTable(BUFFER lbMNPProcess:HANDLE).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC OrderDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
               
      objDumpBase = NEW DumpTimeBased(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbOrder:HANDLE).
      objDumpBase:mUseTable(BUFFER lbOrderAccessory:HANDLE).
      objDumpBase:mUseTable(BUFFER lbOrderPayment:HANDLE).
      objDumpBase:mUseTable(BUFFER lbOrderCustomer:HANDLE).
      objDumpBase:mUseTable(BUFFER lbMNPProcess:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessTimeBased(INPUT CAST(objDumpBase, DumpTimeBased)).
      
      /* If there has been problems we won't be here... */ 
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.


	/*------------------------------------------------------------------------------
	 Purpose: Collect OrderAccesories
	 Notes:   APIBSS-282
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID mDataFromAccessories(  ):
      
      ASSIGN lcAccessories = "".

      FOR EACH OrderAccessory NO-LOCK 
         WHERE OrderAccessory.Brand   = "1" AND
               OrderAccessory.OrderId = lbOrder.OrderId:

         ASSIGN lcAccessories = lcAccessories + "," + 
                                OrderAccessory.Productcode.
      END.
		
      ASSIGN lcAccessories = LEFT-TRIM(lcAccessories,",").
		
		RETURN.

	END METHOD.

   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):

      CASE icField:
         WHEN "DeliveryTime"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN lcDeliveryTime.
         WHEN "LastUpdateStamp"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN lcChangeTime.
         WHEN "ClosedTimeStamp"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN lcCloseTime.
         WHEN "PayTermContr"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN lcPayTerm.
         WHEN "TermContr"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN lcTerm.
         WHEN "TermLeasAmt"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN TRIM(STRING(ldeTermLeasAmt,"->>>>>>>>>>>>>>>>>>>>>>9.9<<<<<<<<<")).
         WHEN "Accessories"  /* APIBSS-282 */
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN lcAccessories.
         WHEN "OnlySimAmt"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN lcOnlySimAmt.
         WHEN "TopUpScheme"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN lcTopupScheme.
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.


   METHOD PRIVATE VOID mDataFromOfferItems():

      ASSIGN
         lcOnlySimAmt   = ""
         lcTopupScheme  = ""
         lcTerm         = ""
         lcPayTerm      = ""
         ldeTermLeasAmt = 0
         .

      IF lbOrder.Offer = "" OR lbOrder.Offer = ?
      THEN RETURN.

      FOR
         EACH OfferItem NO-LOCK USE-INDEX EndStamp WHERE
            OfferItem.Brand       = "1"              AND
            OfferItem.Offer       = lbOrder.Offer    AND
            OfferItem.EndStamp   >= lbOrder.CrStamp  AND
            OfferItem.BeginStamp <= lbOrder.CrStamp:

         CASE OfferItem.ItemType:

            WHEN "BillItem"
            THEN DO:
               IF lcOnlySimAmt = ""
               THEN
               FOR FIRST BillItem FIELDS (Brand BillCode BIGroup) NO-LOCK WHERE
                  BillItem.Brand    = "1"               AND
                  BillItem.BillCode = OfferItem.ItemKey AND
                  BillItem.BIGroup  = "9":

                  lcOnlySimAmt = STRING(OfferItem.Amount).
               END.
            END.
            WHEN "TopUp"
            THEN DO:
               IF lcTopupScheme = "" AND
                  CAN-FIND(FIRST TopupScheme NO-LOCK WHERE
                              TopupScheme.Brand       = "1"          AND
                              TopupScheme.TopupScheme = OfferItem.ItemKey)
               THEN lcTopupScheme = OfferItem.ItemKey.

            END.
            WHEN "PerContract"
            THEN DO:
               IF lcTerm = "" AND OfferItem.ItemKey BEGINS "TERM"
               THEN lcTerm = OfferItem.ItemKey.

               ELSE IF lcPayTerm = "" AND OfferItem.ItemKey BEGINS "PAYTERM"
               THEN ASSIGN
                       lcPayTerm      = OfferItem.ItemKey
                       ldeTermLeasAmt = OfferItem.Amount
                       .
            END.

         END CASE.

      END.

   END METHOD.


   METHOD PRIVATE VOID mOrderTimes():

      ASSIGN
         lcDeliveryTime = ""
         lcChangeTime   = ""
         lcCloseTime    = ""
         .

      FOR EACH OrderTimeStamp FIELDS (Brand OrderID RowType TimeStamp) NO-LOCK WHERE
         OrderTimeStamp.Brand   = "1"             AND
         OrderTimeStamp.OrderId = lbOrder.OrderId:

         CASE OrderTimeStamp.RowType:
            WHEN {&ORDERTIMESTAMP_DELIVERY}
            THEN lcDeliveryTime = STRING(OrderTimeStamp.Timestamp).
            WHEN {&ORDERTIMESTAMP_CHANGE}
            THEN lcChangeTime   = STRING(OrderTimeStamp.Timestamp).
            WHEN {&ORDERTIMESTAMP_CLOSE}
            THEN lcCloseTime    = STRING(OrderTimeStamp.Timestamp).
         END CASE.

      END.

   END METHOD.


   METHOD PRIVATE VOID mCollectInformation():

      mOrderTimes().

      mDataFromOfferItems().
      
      mDataFromAccessories(). /* APIBSS-282 */

      FIND FIRST lbOrderAccessory NO-LOCK WHERE
         lbOrderAccessory.Brand        = "1"                    AND
         lbOrderAccessory.OrderId      = lbOrder.OrderId        AND
         lbOrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE}
      NO-ERROR.

      FIND FIRST lbOrderPayment NO-LOCK WHERE
         lbOrderPayment.Brand          = "1"                    AND
         lbOrderPayment.OrderId        = lbOrder.OrderId 
      NO-ERROR.         

      FIND FIRST lbOrderCustomer NO-LOCK WHERE
         lbOrderCustomer.Brand          = "1"                    AND
         lbOrderCustomer.OrderId        = lbOrder.OrderId        AND
         lbOrderCustomer.RowType        = 1
      NO-ERROR.

      RELEASE lbMNPProcess.
      FOR
         EACH lbMNPProcess NO-LOCK WHERE
            lbMNPProcess.OrderId = lbOrder.OrderId AND
            lbMNPProcess.MNPType = {&MNP_TYPE_IN}
         BY lbMNPProcess.CreatedTS DESC:

         LEAVE.
      END.

   END METHOD.


   METHOD OVERRIDE PUBLIC CHARACTER mProcessRepLog
      (INPUT icRowID AS CHARACTER):

      FIND lbOrder NO-LOCK WHERE
         ROWID(lbOrder) = TO-ROWID(icRowID)
      NO-ERROR.

      IF AVAILABLE lbOrder
      THEN DO:
         FIND CURRENT lbOrder NO-LOCK.

         mCollectInformation().

         RETURN objDumpBase:mGetData().
      END.

      RETURN "". 

   END METHOD.


   METHOD PRIVATE VOID mProcessTimeBased
      (INPUT objDumpTB AS CLASS DumpTimeBased):
   
      FOR EACH lbOrder NO-LOCK USE-INDEX Stamp WHERE
         lbOrder.Brand   = "1"                  AND
         lbOrder.CrStamp >= objDumpTB:ldeFromTS AND
         lbOrder.CrStamp < objDumpTB:ldeToTS
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.            

         liEvents = liEvents + 1.

         mCollectInformation().

         objDumpTB:mWriteALine().

         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "Order Counter"
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.

      END.

      IF NOT SESSION:BATCH THEN 
         HIDE FRAME fQty NO-PAUSE.    

   END METHOD.

END CLASS.