/* ----------------------------------------------------------------------
  MODULE .......: ifs_rove_ko.p
  TASK .........: New IFS file about orders cancelled because ROVE. YDR-695
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 07.11.12
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

DEF VAR lcDelimiter AS CHAR NO-UNDO INIT ";". 

DEF STREAM sout.
OUTPUT STREAM sout TO VALUE(icFile).

FIND FIRST DumpFile NO-LOCK WHERE
           DumpFile.DumpID = iiDumpID NO-ERROR.
IF AVAILABLE DumpFile THEN
   lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).

RUN pDump.
RUN pDumpRetention.
RUN pDumpSubTerm.

PROCEDURE pDump:

   DEF VAR ldeFrom AS DEC NO-UNDO. 
   DEF VAR ldeTo   AS DEC NO-UNDO. 

   ASSIGN
      ldeFrom = fMake2Dt(TODAY - 1,0).
      ldeTo   = fMake2Dt(TODAY - 1,86399).

   LOOP:
   FOR EACH OrderTimeStamp WHERE
            OrderTimeStamp.Brand      = gcBrand AND
            OrderTimeStamp.RowType    = {&ORDERTIMESTAMP_CLOSE} AND
            OrderTimeStamp.TimeStamp >= ldeFrom AND
            OrderTimeStamp.TimeStamp <= ldeTo NO-LOCK,
      FIRST Order NO-LOCK USE-INDEX OrderId WHERE
            Order.Brand = gcBrand AND
            Order.OrderId = OrderTimeStamp.OrderId AND
            LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 AND
            Order.Invnum = 0 AND
            INDEX(Order.OrderChannel,"pos") = 0 AND
            INDEX(Order.OrderChannel,"retention") = 0,
      FIRST OrderPayment NO-LOCK WHERE
            OrderPayment.Brand = gcBrand AND
            OrderPayment.OrderId = Order.OrderId AND
            OrderPayment.Method = 2: /* credit card */

      RUN pWriteToFile.

   END.

END PROCEDURE. 

PROCEDURE pDumpRetention:
   
   DEF VAR liOrderId AS INT NO-UNDO. 
   DEF VAR ldeNow AS DEC NO-UNDO. 
   ldeNow = fMakeTS().

   LOOP:
   FOR EACH ActionLog EXCLUSIVE-LOCK USE-INDEX ActionID WHERE
            ActionLog.Brand = gcBrand AND
            ActionLog.ActionID = "OrderCancelRetention" AND
            ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE}:

      liOrderId = INT(ActionLog.Key) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT LOOP.

      FOR FIRST Order NO-LOCK USE-INDEX OrderId WHERE
                Order.Brand = gcBrand AND
                Order.OrderId = liOrderId AND
                LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 AND
                Order.Invnum = 0 AND
                LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) = 0,
         FIRST OrderPayment NO-LOCK WHERE
               OrderPayment.Brand = gcBrand AND
               OrderPayment.OrderId = Order.OrderId AND
               OrderPayment.Method = 2: /* credit card */

         IF NOT Order.OrderChannel BEGINS "retention" THEN NEXT.

         /* MNP OUT must be  */
         FOR EACH MNPSub NO-LOCK WHERE
                  MNPSub.MsSeq = Order.MsSeq,
            FIRST MNPProcess WHERE
                  MNPProcess.MNPSeq = MNPSub.MNPSeq AND
                  MNPProcess.MNPType = 2 AND
                  MNPProcess.CreatedTS < Order.CrStamp NO-LOCK:
            IF fOffSet(MNPProcess.PortingTime,15 * 24) > ldeNow THEN NEXT LOOP.
         END.
         
         RUN pWriteToFile.
         
         ASSIGN
            ActionLog.ActionChar = icFile
            ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
      END.
   END.

END PROCEDURE. 

PROCEDURE pDumpSubTerm:

   DEF VAR ldeFrom   AS DEC NO-UNDO. 
   DEF VAR ldeTo     AS DEC NO-UNDO. 
   DEF VAR liOrderId AS INT NO-UNDO. 

   ASSIGN
      ldeFrom  = fHMS2TS(TODAY - 1,"00:00:00").
      ldeTo  = fHMS2TS(TODAY - 1,"23:59:59").

   LOOP:
   FOR EACH ActionLog NO-LOCK USE-INDEX ActionID WHERE
            ActionLog.Brand = gcBrand AND
            ActionLog.ActionID = "OrderCancel" AND
            ActionLog.ActionTS >= ldeFrom AND
            ActionLog.ActionTS <= ldeto AND
            ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}:

      liOrderId = INT(ActionLog.Key) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT LOOP.

      FOR FIRST Order NO-LOCK USE-INDEX OrderId WHERE
                Order.Brand = gcBrand AND
                Order.OrderId = liOrderId AND
                LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0 AND
                Order.Invnum = 0 AND
                INDEX(Order.OrderChannel,"pos") = 0 AND
                INDEX(Order.OrderChannel,"retention") = 0,
         FIRST OrderPayment NO-LOCK WHERE
               OrderPayment.Brand = gcBrand AND
               OrderPayment.OrderId = Order.OrderId AND
               OrderPayment.Method = 2: /* credit card */

         RUN pWriteToFile.

      END.
   END.

END PROCEDURE.

PROCEDURE pWriteToFile:

   DEF VAR ldOperationDate AS DATE NO-UNDO.
   DEF VAR liTime AS INTEGER NO-UNDO.

   FIND FIRST EventLog WHERE
              EventLog.TableName = "Order" AND
              EventLog.Key = "1" + CHR(255) + STRING(Order.OrderId) AND
              EventLog.Action = "Create" NO-LOCK NO-ERROR.
   IF AVAILABLE EventLog THEN
      ldOperationDate = EventLog.EventDate.
   ELSE
      fSplitTS(Order.CrStamp,
               OUTPUT ldOperationDate,
               OUTPUT liTime).

   PUT STREAM sout UNFORMATTED
      OrderPayment.CCReference lcDelimiter
      OrderPayment.BinNumber lcDelimiter
      OrderPayment.AuthNumber lcDelimiter
      STRING(ldOperationDate,"99-99-9999") SKIP.

   oiEvents = oiEvents + 1.
   IF NOT SESSION:BATCH THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "Invoices" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.

END PROCEDURE.

FINALLY:
   OUTPUT STREAM sout CLOSE.
   IF NOT SESSION:BATCH THEN HIDE FRAME fQty NO-PAUSE.
END.
