/* ----------------------------------------------------------------------
  MODULE .......: ifs_dextra_orders.p
  TASK .........: New IFS file about orders from direct channel with
                  installment contract for NIF and NIE
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 24.06.14
  Version ......: Yoigo
----------------------------------------------------------------------- */
{commali.i}
{dumpfile_run.i}
{timestamp.i}
{tmsconst.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcDelimiter     AS CHAR NO-UNDO INIT ";".
DEF VAR ldOperationDate AS DATE NO-UNDO.
DEF VAR liTime          AS INT  NO-UNDO.

DEF STREAM sout.
OUTPUT STREAM sout TO VALUE(icFile).

FIND FIRST DumpFile NO-LOCK WHERE
           DumpFile.DumpID = iiDumpID NO-ERROR.
IF AVAILABLE DumpFile THEN
   lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).

RUN pDump.

PROCEDURE pDump:

   DEF VAR liOrderID AS INT NO-UNDO. 
   DEF VAR lcTerminalCode AS CHAR NO-UNDO.
   DEF VAR liDays          AS INT  NO-UNDO. 

   DO liDays = 1 TO 90:
      FOR EACH EventLog NO-LOCK where
               EventLog.eventdate = TODAY - liDays and
               EventLog.tablename = "order" and
               EventLog.action = "CREATE" use-index eventdate:
         
         liOrderID = int(entry(2,key,chr(255))) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN NEXT.

         ASSIGN lcTerminalCode  = ""
                ldOperationDate = EventLog.EventDate.

         FOR FIRST Order NO-LOCK WHERE
                   Order.Brand = gcBrand AND
                   Order.OrderId = liOrderID AND
             INDEX(Order.OrderChannel,"pos") = 0 AND
                   Order.PayType = FALSE,
            FIRST OrderPayment NO-LOCK WHERE
                  OrderPayment.Brand = gcBrand AND
                  OrderPayment.OrderId = Order.OrderId AND
                  OrderPayment.Method = 2: /* credit card */

            /* Check Terminal billcode */
            FOR EACH OfferItem NO-LOCK WHERE
                     OfferItem.Brand = gcBrand AND
                     OfferItem.Offer = Order.Offer AND
                     OfferItem.BeginStamp <= Order.CrStamp AND
                     OfferItem.EndStamp >= Order.CrStamp AND
                     OfferItem.ItemType = "BillItem",
               FIRST BillItem NO-LOCK WHERE
                     BillItem.Brand    = gcBrand AND
                     BillItem.BillCode = OfferItem.ItemKey,
               FIRST BitemGroup NO-LOCK WHERE
                     BitemGroup.Brand   = gcBrand AND
                     BitemGroup.BIGroup = BillItem.BIGroup AND
                     BItemGroup.BIGroup EQ "7":

               /* Exclude discount billing item on terminal */
               IF BillItem.BillCode BEGINS "CPDISC" THEN NEXT.

               lcTerminalCode = BillItem.BillCode.
               LEAVE.
            END.
            /* Exclude order if without terminal */
            IF lcTerminalCode = "" THEN LEAVE.

            RUN pWriteToFile.
         END.
      END.
   END.
END PROCEDURE.


PROCEDURE pWriteToFile:

   DEF VAR liFFItemQty    AS INT NO-UNDO.
   DEF VAR ldeFFItemAmt   AS DEC NO-UNDO.
   DEF VAR ldeResidualFee AS DEC NO-UNDO.

   FOR EACH OfferItem NO-LOCK WHERE
            OfferItem.Brand = gcBrand AND
            OfferItem.Offer = Order.Offer AND
            OfferItem.ItemType = "PerContract" AND
            OfferItem.BeginStamp <= Order.CrStamp AND
            OfferItem.EndStamp >= Order.CrStamp,
      FIRST DayCampaign NO-LOCK WHERE
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = OfferItem.ItemKey AND
            DayCampaign.DCType = {&DCTYPE_INSTALLMENT},
      FIRST FMItem NO-LOCK WHERE
            FMItem.Brand     = gcBrand AND
            FMItem.FeeModel  = DayCampaign.FeeModel AND
            FMItem.ToDate   >= ldOperationDate AND
            FMItem.FromDate <= ldOperationDate:

      ASSIGN liFFItemQty    = FMItem.FFItemQty
             ldeFFItemAmt   = FMItem.Amount
             ldeResidualFee = OfferItem.Amount.
      LEAVE.
   END.

   PUT STREAM sout UNFORMATTED
       OrderPayment.CCReference lcDelimiter
       OrderPayment.BinNumber lcDelimiter
       OrderPayment.AuthNumber lcDelimiter
       STRING(ldOperationDate,"99-99-9999") lcDelimiter
       STRING(Order.CustNum) lcDelimiter
       STRING(Order.MsSeq) lcDelimiter
       STRING(Order.OrderId) lcDelimiter
       STRING(liFFItemQty) lcDelimiter
       STRING(ldeFFItemAmt) lcDelimiter
       STRING(ldeResidualFee) lcDelimiter
       SKIP.

   oiEvents = oiEvents + 1.
   IF NOT SESSION:BATCH THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "Orders"
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.

END PROCEDURE.

FINALLY:
   OUTPUT STREAM sout CLOSE.
   IF NOT SESSION:BATCH THEN HIDE FRAME fQty NO-PAUSE.
END.
