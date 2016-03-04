/* NOTE!
      
      In order to get ride of one extra query we will store
      TermLeasAmt to temporary variable in method mGetTermContract.
      
      This is only working when #TermLeasAmt is specified after
      #PayTermContr (in DFField table)!!
*/

USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.

{HPD/HPDConst.i}
{Syst/tmsconst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.OrderDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbOrder          FOR Order.
   DEFINE PRIVATE BUFFER lbOrderAccessory FOR OrderAccessory.
   DEFINE PRIVATE BUFFER lbOrderPayment   FOR OrderPayment.
   DEFINE PRIVATE BUFFER lbMNPProcess     FOR MNPProcess.
   
   DEFINE PRIVATE VARIABLE ldeTermLeasAmt AS DECIMAL NO-UNDO.
  
   CONSTRUCTOR PUBLIC OrderDump
      (iiDumpID AS INTEGER):
               
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).      
      objDumpBase:mUseTable(BUFFER lbOrder:HANDLE).
      objDumpBase:mUseTable(BUFFER lbOrderAccessory:HANDLE).
      objDumpBase:mUseTable(BUFFER lbOrderPayment:HANDLE).
      objDumpBase:mUseTable(BUFFER lbMNPProcess:HANDLE).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC OrderDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
               
      objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbOrder:HANDLE).
      objDumpBase:mUseTable(BUFFER lbOrderAccessory:HANDLE).
      objDumpBase:mUseTable(BUFFER lbOrderPayment:HANDLE).
      objDumpBase:mUseTable(BUFFER lbMNPProcess:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessFullDump(INPUT CAST(objDumpBase, DumpFile)).
      
      /* If there has been problems we won't be here... */ 
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):

      CASE icField:
         WHEN "DeliveryTime"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mGetOrderTime({&ORDERTIMESTAMP_DELIVERY}).
         WHEN "LastUpdateStamp"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mGetOrderTime({&ORDERTIMESTAMP_CHANGE}).
         WHEN "ClosedTimeStamp"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mGetOrderTime({&ORDERTIMESTAMP_CLOSE}).
         WHEN "DeliveryTime"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mGetOrderTime({&ORDERTIMESTAMP_DELIVERY}).
         WHEN "PayTermContr"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mGetTermContract("PAYTERM").
         WHEN "TermContr"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mGetTermContract("TERM").
         WHEN "TermLeasAmt"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(ldeTermLeasAmt,"->>>>>>>>>>>>>>>>>>>>>>9.9<<<<<<<<<").
         WHEN "OnlySimAmt"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mGetOnlySimAmt().
         WHEN "TopUpScheme"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN mGetTopupScheme().
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.
   

   METHOD PRIVATE CHARACTER mGetOnlySimAmt():

      FOR EACH OfferItem NO-LOCK WHERE
               OfferItem.Brand       = "1"              AND
               OfferItem.Offer       = lbOrder.Offer    AND
               OfferItem.ItemType    = "BillItem"       AND
               OfferItem.EndStamp   >= lbOrder.CrStamp  AND
               OfferItem.BeginStamp <= lbOrder.CrStamp,
         FIRST BillItem NO-LOCK WHERE
               BillItem.Brand    = "1"                  AND
               BillItem.BillCode = OfferItem.ItemKey    AND
               BillItem.BIGroup  = "9":

         RETURN STRING(OfferItem.Amount).
      END.

      RETURN "".
      
   END METHOD.
      

   METHOD PRIVATE CHARACTER mGetTopupScheme():

      FOR
         EACH OfferItem NO-LOCK WHERE
            OfferItem.Brand         = "1"              AND
            OfferItem.Offer         = lbOrder.Offer    AND
            OfferItem.ItemType      = "TopUp"          AND
            OfferItem.EndStamp     >= lbOrder.CrStamp  AND
            OfferItem.BeginStamp   <= lbOrder.CrStamp,
         FIRST TopupScheme NO-LOCK WHERE
            TopupScheme.Brand       = "1"              AND
            TopupScheme.TopupScheme = OfferItem.ItemKey:
         
         RETURN TopupScheme.TopupScheme.
         
      END.

      RETURN "".
      
   END METHOD.


   METHOD PRIVATE CHARACTER mGetTermContract
      ( icTermContractType AS CHARACTER ):

      IF icTermContractType = "PAYTERM"
      THEN ldeTermLeasAmt = 0.
      
      FOR
         FIRST OfferItem NO-LOCK WHERE
            OfferItem.Brand       = "1"                     AND
            OfferItem.Offer       = lbOrder.Offer           AND
            OfferItem.ItemType    = "PerContract"           AND
            OfferItem.ItemKey     BEGINS icTermContractType AND
            OfferItem.EndStamp   >= lbOrder.CrStamp  AND
            OfferItem.BeginStamp <= lbOrder.CrStamp:
         
         IF icTermContractType = "PAYTERM"
         THEN ldeTermLeasAmt = OfferItem.Amount. 
         
         RETURN OfferItem.ItemKey.
         
      END.
      
      RETURN "".


   END METHOD.


   METHOD PRIVATE CHARACTER mGetOrderTime
      ( iiType AS INTEGER ):

      FOR FIRST OrderTimeStamp FIELDS (Brand OrderID RowType TimeStamp) NO-LOCK WHERE
         OrderTimeStamp.Brand   = "1"             AND
         OrderTimeStamp.OrderId = lbOrder.OrderId AND
         OrderTimeStamp.RowType = iiType:
            
         RETURN STRING(OrderTimeStamp.Timestamp).
      END.
      
      RETURN "".
      
   END METHOD.


   METHOD PRIVATE VOID mFindAdditionalTables():
      
      FIND FIRST lbOrderAccessory NO-LOCK WHERE
         lbOrderAccessory.Brand        = "1"                    AND
         lbOrderAccessory.OrderId      = lbOrder.OrderId        AND
         lbOrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE}
      NO-ERROR.
      
      FIND FIRST lbOrderPayment NO-LOCK WHERE
         lbOrderPayment.Brand          = "1"                    AND
         lbOrderPayment.OrderId        = lbOrder.OrderId 
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

         mFindAdditionalTables().
               
         RETURN objDumpBase:mGetData().
         
      END.
            
      RETURN "". 
         
   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpFile AS CLASS DumpFile):
   
      FOR EACH lbOrder NO-LOCK
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:
   
         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.            
            
         liEvents = liEvents + 1.
      
         mFindAdditionalTables().

         objDumpFile:mWriteALine().
         
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