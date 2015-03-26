/* ----------------------------------------------------------------------
  MODULE .......: order_percontract_dump
  TASK .........: Create a dump file for orders with periodical contracts 
  ---------------------------------------------------------------------- */

{commali.i}
{dumpfile_run.i}
{create_eventlog.i}
{timestamp.i}
{tmsconst.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric    AS CHAR     NO-UNDO.
DEF VAR lcDelimiter  AS CHAR     NO-UNDO.
DEF VAR liCnt        AS INT      NO-UNDO.
DEF VAR lcDumpFields AS CHAR     NO-UNDO.
DEF VAR lcValue      AS CHAR     NO-UNDO.
DEF VAR lcField      AS CHAR     NO-UNDO.
DEF VAR ldaLastDumpDate AS DATE NO-UNDO. 
DEF VAR lcLastDumpTime AS CHARACTER NO-UNDO. 
DEF VAR liOrderId   AS INT NO-UNDO. 

DEFINE VARIABLE ldtCrDate AS DATE NO-UNDO.
DEFINE VARIABLE liCrTime  AS INT NO-UNDO.
DEFINE VARIABLE lcContract AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcItemTypes AS CHARACTER NO-UNDO EXTENT 2. 
DEFINE VARIABLE liLastDumpTime AS INTEGER NO-UNDO. 
ASSIGN
lcItemTypes[1] = "PerContract"
lcItemTypes[2] = "BundleItem".

FORM
   oiEvents LABEL "Order Per Contract" 
WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
   TITLE " Collecting " FRAME fQty.

DEFINE TEMP-TABLE ttContract
FIELD dcevent AS CHAR format "x(15)"
FIELD contract AS CHAR format "x(30)"
INDEX contract contract.
             

FOR EACH DayCampaign NO-LOCK WHERE
         DayCampaign.Brand = gcBrand AND
  LOOKUP(DayCampaign.DCType,"1,4") > 0,
  EACH ServiceLimitGroup NO-LOCK WHERE 
       ServiceLimitGroup.Brand     = gcBrand AND
       ServiceLimitGroup.GroupCode = DayCampaign.DCEvent,
  EACH ServiceLimit NO-LOCK WHERE 
       ServiceLimit.GroupCode = DayCampaign.DCEvent:

   CREATE ttContract.
   ASSIGN
      ttContract.dcevent  = DayCampaign.dcevent
      ttContract.contract = servicelimit.groupcode + "/" + ServiceLimit.SLCode.
END.
FOR EACH DayCampaign NO-LOCK WHERE
         DayCampaign.Brand = gcBrand AND
  LOOKUP(DayCampaign.DCType,"1,4") = 0:

   CREATE ttContract.
   ASSIGN
      ttContract.dcevent  = DayCampaign.dcevent
      ttContract.contract = DayCampaign.DcEvent.
END.

DEFINE TEMP-TABLE ttSubContract
FIELD dcevent AS CHAR format "x(15)".

DEF STREAM sFile.

lcNumeric = SESSION:NUMERIC-FORMAT.

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).
   
   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END.
ELSE DO:
   ASSIGN 
      lcDelimiter = CHR(9)
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END.

FOR EACH DFField OF DumpFile NO-LOCK WHERE
         DFField.ToDate   >= TODAY AND
         DFField.FromDate <= TODAY
BY DFField.OrderNbr:
   lcDumpFields = lcDumpFields + (IF lcDumpFields > "" THEN "," ELSE "") +
                  DFField.DFField.
END.

OUTPUT STREAM sFile TO VALUE(icFile).

IF icDumpMode EQ "modified" THEN DO:

   fSplitTs(idLastDump, OUTPUT ldaLastDumpDate, OUTPUT liLastDumpTime).
   lcLastDumpTime = STRING(liLastDumpTime,"hh:mm:ss").

   FOR EACH EventLog NO-LOCK where
            EventLog.eventdate >= ldaLastDumpDate and
            EventLog.tablename = "order" and
            EventLog.action = "CREATE" use-index eventdate:

      IF EventLog.EventDate EQ ldaLastDumpDate AND
         EventLog.eventtime < lcLastDumpTime THEN NEXT.

      liOrderID = int(entry(2,key,chr(255))) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT.

      FIND FIRST Order NO-LOCK WHERE
                 Order.Brand = gcBrand AND
                 Order.OrderId = liOrderID NO-ERROR.
      IF NOT AVAIL Order THEN NEXT.
      
      EMPTY TEMP-TABLE ttSubContract.
      RUN pReadOffer.
      RUN pReadOrderAction.
      RUN pWriteContract.

      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
             PAUSE 0. 
             DISP oiEvents WITH FRAME fQty.
      END.

   END.
END.
ELSE
/* order loop */
Order_loop:
FOR EACH Order NO-LOCK WHERE
         Order.Brand = gcBrand:

      EMPTY TEMP-TABLE ttSubContract.
      RUN pReadOffer.
      RUN pReadOrderAction.
      RUN pWriteContract.

      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
             PAUSE 0. 
             DISP oiEvents WITH FRAME fQty.
      END.
    
END. /* end OrderLoop --*/

EMPTY TEMP-TABLE ttContract.
EMPTY TEMP-TABLE ttSubContract.
IF NOT SESSION:BATCH THEN HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.

PROCEDURE pReadOffer :

   DEFINE VARIABLE lcItemType AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE i AS INTEGER NO-UNDO. 
   DEF BUFFER bOfferItem FOR OfferItem.

   FIND FIRST Offer WHERE 
              Offer.Brand = gcBrand AND
              Offer.Offer = Order.Offer NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Offer THEN RETURN.

   DO i = 1 TO 2:

      FOR EACH OfferItem  NO-LOCK WHERE
               OfferItem.Brand       = gcBrand       AND
               OfferItem.Offer       = Offer.Offer   AND
               OfferItem.ItemType    = lcItemTypes[i] AND
               OfferItem.EndStamp   >= Order.CrStamp AND
               OfferItem.BeginStamp <= Order.CrStamp:
         
	      /* With renewal or stc orders, bundle selection should always be in
            OrderAction table. YBU-1037 */
         IF OfferItem.ItemType EQ "BundleItem" THEN DO:
         
            IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN NEXT.
            IF Order.OrderType EQ {&ORDER_TYPE_STC} THEN NEXT.

            IF CAN-FIND(FIRST bOfferItem NO-LOCK WHERE
                              bOfferItem.Brand = Order.Brand AND
                              bOfferItem.Offer = Order.Offer AND
                              bOfferItem.ItemType = OfferItem.ItemType AND
                              bOfferItem.EndStamp >= Order.CrStamp AND
                              bOfferItem.BeginStamp <= Order.CrStamp AND
                              ROWID(bOfferItem) NE ROWID(OfferItem)) THEN LEAVE. 
         END.
         CREATE ttSubContract.
         ttSubContract.dcEvent = OfferItem.ItemKey.
      END.
   END.

END PROCEDURE.

PROCEDURE pReadOrderAction:

   FOR EACH OrderAction NO-LOCK WHERE
            OrderAction.Brand = gcBrand AND
            OrderAction.OrderId = Order.OrderId AND
            OrderAction.ItemType = "BundleItem":

      FIND FIRST ttSubContract NO-LOCK where
                 ttSubContract.dcevent = OrderAction.ItemKey
      NO-ERROR.
      IF NOT AVAIL ttSubContract THEN DO:
         CREATE ttSubContract.
         ttSubContract.dcEvent = OrderAction.ItemKey.
      END.
   END.

END PROCEDURE.


PROCEDURE pWriteContract :

      FOR EACH ttSubContract,
          EACH ttContract WHERE
               ttContract.dcevent = ttSubContract.dcevent:
      DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

         lcField = ENTRY(liCnt,lcDumpFields).
      
         IF lcField BEGINS "#" THEN DO:
            CASE lcField:
            WHEN "#Order_Id" THEN lcValue = STRING(Order.OrderId).
            WHEN "#PerContract" THEN lcValue = ttContract.Contract.
            OTHERWISE lcValue = "".
            END CASE.
         END.
         ELSE lcValue = "". 
  
         PUT STREAM sFile UNFORMATTED lcValue.

         IF liCnt < NUM-ENTRIES(lcDumpFields) THEN 
         PUT STREAM sFile UNFORMATTED lcDelimiter.
      
      END.
      PUT STREAM sFile UNFORMATTED  SKIP.
      oiEvents = oiEvents + 1.
      END.

END PROCEDURE.


