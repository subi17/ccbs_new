/* ----------------------------------------------------------------------
  MODULE .......: read_orderstatuscode.p
  TASK .........: Back door tools: Automate order status update script. YDR-136
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 06/2010
  CHANGED ......: 04/2012 - YDR-420
  CHANGED ......: 02/2013 - YDR-889
  CHANGED ......: 05/2013 - YOT-2468
  CHANGED ......: 06/2016 - YOT-4490 order status 73 can be closed
  Version ......: yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".

{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/date.i}
{Func/email.i}
{Mc/orderfusion.i}

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
DEF VAR liEntries AS INT NO-UNDO. 
DEF VAR lcMemoTitle AS CHAR NO-UNDO. 
DEF VAR lcMemoText AS CHAR NO-UNDO. 
DEF VAR lcMailConfDir AS CHARACTER NO-UNDO. 

/* field variables */
DEF VAR liOrderId AS INT NO-UNDO. 
DEF VAR lcMsisdn AS CHAR NO-UNDO. 
DEF VAR lcOldStatus AS CHAR NO-UNDO. 
DEF VAR lcNewStatus AS CHAR NO-UNDO. 
DEF VAR liSecure AS INT NO-UNDO. 

ASSIGN
   lcIncDir  = fCParam("OrderStatUpdate","IncDir") 
   lcProcDir = fCParam("OrderStatUpdate","IncProcDir")
   lcSpoolDir = fCParam("OrderStatUpdate","OutSpoolDir")
   lcOutDir   = fCParam("OrderStatUpdate","OutDir")
   lcMailConfDir = fCParamC("RepConfDir").

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine  "|"
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
   lcLogFile = lcSpoolDir + lcFileName + ".log".
   OUTPUT STREAM sLog TO VALUE(lcLogFile) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.
  
   LINE_LOOP:
   REPEAT:
      
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine EQ "" THEN NEXT.

      /*[Order ID];[MSISDN];[Current Order ID status];[New Order Status]*/
      /*[Order ID];[MSISDN];[Current Order ID status];[New Order Status];[Memo's Title];[Message]*/ 
      
      ASSIGN
         liEntries   = NUM-ENTRIES(lcLine,lcSep)
         lcMemoTitle = ""
         lcMemoText  = "".

      ASSIGN 
         liOrderId    = INTEGER(ENTRY(1,lcLine,lcSep))
         lcMSISDN     = ENTRY(2,lcLine,lcSep)        
         lcOldStatus  = ENTRY(3,lcLine,lcSep)        
         lcNewStatus  = ENTRY(4,lcLine,lcSep)        
         liSecure     = INTEGER(ENTRY(5,lcLine,lcSep))
         lcMemoTitle  = ENTRY(6,lcLine,lcSep) WHEN liEntries >= 6
         lcMemoText   = ENTRY(7,lcLine,lcSep) WHEN liEntries >= 7
         NO-ERROR.
 
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Incorrect input data format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.
     
      RUN pUpdateOrderStatus(liOrderId,
                             lcMSISDN,
                             lcOldStatus,
                             lcNewStatus, 
                             liSecure,
                             lcMemoTitle,
                             lcMemoText).

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         fError(ENTRY(2,RETURN-VALUE,":")).
         liNumErr = liNumErr + 1 .
      END.
      ELSE DO:
         liNumOK = liNumOK + 1 .
      END.
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
   
   IF lcMailConfDir > "" AND lcReportFileOut > "" THEN DO:
      GetRecipients(lcMailConfDir + "read_orderstatusfile.email").
      SendMail(lcReportFileOut,"").
   END.
END.

INPUT STREAM sFile CLOSE.

PROCEDURE pUpdateOrderStatus:

   DEF INPUT PARAMETER iiOrderId AS INTEGER NO-UNDO.
   DEF INPUT PARAMETER icMSISDN AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icOldStatus AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icNewStatus AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiSecure AS INT NO-UNDO.
   DEF INPUT PARAMETER icMemoTitle AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icMemoText AS CHAR NO-UNDO.

   DEF VAR lcError AS CHAR NO-UNDO.

   /* find order */   
   FIND Order WHERE 
        Order.Brand = gcBrand AND
        Order.OrderId = iiOrderId NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Order THEN 
      RETURN "ERROR:Invalid Order ID".

   IF Order.CLI NE icMSISDN THEN
      RETURN "ERROR:MSISDN does not match with order".

   IF LOOKUP(icOldStatus,"20,21,22,41,42,43,44,50,51,73,76,77,78,79,80,99") = 0 THEN
      RETURN "ERROR:Unsupported current order status value".

   IF Order.StatusCode NE icOldStatus THEN
      RETURN "ERROR:Current status does not match actual one on the system".

   IF Order.StatusCode EQ icNewStatus THEN
      RETURN SUBST("ERROR:Error order ID &1 already in status &2", 
                   Order.OrderId, icNewStatus).

   IF iiSecure < 0 OR iiSecure > 2 THEN
      RETURN "ERROR:Unsupported secure option value".

   IF iiSecure > 0 THEN DO:
      IF INDEX(Order.OrderChannel,"pos") > 0 OR
               Order.OrderType > 2 THEN 
        RETURN "ERROR:Secure option is allowed only with direct channel orders".

      IF LOOKUP(Order.StatusCode,"41,42,43,44") = 0 THEN
         RETURN "ERROR:Secure option and old order status are not compatible".

      IF icNewStatus NE "6" THEN
         RETURN "ERROR:Secure option and new order status are not compatible".

      IF iiSecure EQ 2 AND Order.DeliveryType NE {&ORDER_DELTYPE_POS} THEN
         RETURN "ERROR:Secure to POS in allowed only for POS delivery type".
   END.
   
   IF icOldStatus EQ "76" OR icOldStatus EQ "22" OR 
      icOldStatus EQ "73" OR icOldStatus EQ "78" THEN DO:
      CASE icNewStatus:
         WHEN "7" THEN RUN Mc/closeorder.p(Order.OrderId, TRUE).
         OTHERWISE RETURN "ERROR:Unsupported new order status value".
      END.
   END.
   ELSE IF LOOKUP(icOldStatus,"20,21,51") > 0 THEN DO:
      CASE icNewStatus:
         WHEN "6" THEN RUN Mc/orderhold.p(Order.OrderId, "RELEASE_BATCH").
         WHEN "7" THEN RUN Mc/closeorder.p(Order.OrderId, TRUE).
         OTHERWISE RETURN "ERROR:Unsupported new order status value".
      END.
   END.
   ELSE IF icOldStatus EQ "50" THEN DO:
      CASE icNewStatus:
         WHEN "6" THEN DO: 
            RUN Mc/orderinctrl.p(Order.OrderId, iiSecure, TRUE).
            /* RUN Mc/sendorderreq.p(Order.OrderId). not needed here? confirmation sent */
         END.
         WHEN "7" THEN RUN Mc/closeorder.p(Order.OrderId, TRUE).
         OTHERWISE RETURN "ERROR:Unsupported new order status value".
      END.
   END.
   ELSE IF icOldStatus EQ {&ORDER_STATUS_PENDING_FIXED_LINE} /* 77 */ THEN DO:
      CASE icNewStatus:
         WHEN "7" THEN DO:
            FIND OrderFusion NO-LOCK WHERE
                 OrderFusion.Brand = gcBrand AND
                 OrderFusion.OrderID = Order.OrderID NO-ERROR.
            IF NOT AVAILABLE OrderFusion THEN
               RETURN "ERROR:Order type is not Fusion".
            IF OrderFusion.fusionstatus EQ {&FUSION_ORDER_STATUS_ONGOING} THEN
               RETURN "ERROR:Not allowed because fusion status ONG".
            IF fCreateFusionCancelOrderMessage(OrderFusion.OrderID,
                                               OUTPUT lcError) EQ FALSE THEN
               RETURN SUBST("ERROR:Cancel message creation failed: &1", lcError).
        END.
        OTHERWISE RETURN "ERROR:Unsupported new order status value".
     END.
   END.
   ELSE IF icOldStatus EQ {&ORDER_STATUS_PENDING_MOBILE_LINE} /* 79 */ THEN DO:
     CASE icNewStatus:
         WHEN "7" THEN RUN Mc/closeorder.p(Order.OrderId, TRUE).
         WHEN "8" THEN RUN Mc/orderbyfraud.p(Order.OrderId, TRUE,
                                          {&ORDER_STATUS_CLOSED_BY_FRAUD}).
         WHEN "9" THEN RUN Mc/orderbyfraud.p(Order.OrderId, TRUE,
                                          {&ORDER_STATUS_AUTO_CLOSED}).
         OTHERWISE RETURN "ERROR:Unsupported new order status value".
      END.
   END.
   ELSE IF icOldStatus EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} /* 80 */
   THEN DO:
      CASE icNewStatus:
         WHEN "6" THEN RUN Mc/orderinctrl.p(Order.OrderId, iiSecure, TRUE).
         WHEN "7" THEN RUN Mc/closeorder.p(Order.OrderId, TRUE).
         WHEN "8" THEN RUN Mc/orderbyfraud.p(Order.OrderId, TRUE,
                                          {&ORDER_STATUS_CLOSED_BY_FRAUD}).
         WHEN "9" THEN RUN Mc/orderbyfraud.p(Order.OrderId, TRUE,
                                          {&ORDER_STATUS_AUTO_CLOSED}).
         OTHERWISE RETURN "ERROR:Unsupported new order status value".
      END.
   END.
   ELSE DO:
      CASE icNewStatus:
         WHEN "6" THEN RUN Mc/orderinctrl.p(Order.OrderId, iiSecure, TRUE).
         WHEN "7" THEN RUN Mc/closeorder.p(Order.OrderId, TRUE).
         WHEN "8" THEN RUN Mc/orderbyfraud.p(Order.OrderId, TRUE,
                                          {&ORDER_STATUS_CLOSED_BY_FRAUD}).
         WHEN "9" THEN RUN Mc/orderbyfraud.p(Order.OrderId, TRUE,
                                          {&ORDER_STATUS_AUTO_CLOSED}).
         WHEN "44" THEN RUN Mc/orderneeddoc.p(Order.OrderId, TRUE).
         OTHERWISE RETURN "ERROR:Unsupported new order status value".
      END.
   END.
   IF RETURN-VALUE > "" THEN RETURN "ERROR:" + RETURN-VALUE.

   IF icMemoTitle > "" THEN DO:

      CREATE Memo.
      ASSIGN
         Memo.CreStamp  = fMakeTS() 
         Memo.Brand     = gcBrand 
         Memo.HostTable = "Order" 
         Memo.KeyValue  = STRING(Order.OrderId) 
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CreUser   = katun 
         Memo.MemoTitle = icMemoTitle
         Memo.MemoText  = icMemoText.
   END.

   RETURN "".
END.
