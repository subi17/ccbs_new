/* ----------------------------------------------------------------------
  MODULE .......: offer_sms_resp_batch.p
  TASK .........: CDL2 - Offer SMS response SMS handling. YPR-2055
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 27.03.15
  Version ......: yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".

{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/lib/eventlog.i}
{Func/cparam2.i}
{Func/date.i}
{Func/fgettxt.i}
{Func/smsmessage.i}

DEF VAR lcLine AS CHARACTER NO-UNDO.
DEF VAR lcSep AS CHARACTER NO-UNDO INIT ",".
DEF VAR liNumOK AS INTEGER NO-UNDO.
DEF VAR liSkipped AS INTEGER NO-UNDO.
DEF VAR liNumErr AS INTEGER NO-UNDO.

/* files and dirs */
DEF VAR lcRootDir AS CHAR NO-UNDO.
DEF VAR lcLogFile AS CHAR NO-UNDO.
DEF VAR lcFileName AS CHARACTER NO-UNDO.
DEF VAR lcIncDir  AS CHARACTER NO-UNDO.
DEF VAR lcInputFile AS CHARACTER NO-UNDO.
DEF VAR lcProcDir AS CHARACTER NO-UNDO.
DEF VAR lcProcessedFile AS CHARACTER NO-UNDO.
DEF VAR lcSpoolDir AS CHARACTER NO-UNDO.
DEF VAR lcOutDir AS CHARACTER NO-UNDO.

/* field variables */
DEF VAR lcContactNumber AS CHAR NO-UNDO.
DEF VAR lcSentTS AS CHAR NO-UNDO.
DEF VAR ldeSentTS AS DEC NO-UNDO. 
DEF VAR lcCustomerResponse AS CHAR NO-UNDO.

ASSIGN
   lcRootDir  = fCParam("Order","OfferSMSRootDir")
   lcIncDir  = lcRootDir + "incoming/incoming/"
   lcProcDir = lcRootDir + "incoming/processed"
   lcSpoolDir = lcRootDir + "outgoing/spool/"
   lcOutDir   = lcRootDir + "outgoing/outgoing/".

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine  "|"
      icMessage SKIP.

END FUNCTION.

/* parses the format YYYYMMDDhhmmss */
FUNCTION fParseTimeStamp RETURNS LOGIC
   (icTimeStamp AS CHAR,
    OUTPUT odeStamp AS DEC):

   DEF VAR ldaDate AS DATE NO-UNDO. 
   DEF VAR lcTime AS CHAR NO-UNDO. 
   DEF VAR ldeStamp AS DEC NO-UNDO. 

   IF LENGTH(icTimeStamp) NE 14 THEN RETURN FALSE.

   ldaDate = DATE(int(substring(icTimeStamp,5,2)),
                  int(substring(icTimeStamp,7,2)),
                  int(substring(icTimeStamp,1,4))) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN FALSE.

   lcTime = SUBST("&1:&2:&3", 
                  substring(icTimeStamp,9,2),
                  substring(icTimeStamp,11,2),
                  substring(icTimeStamp,13,2)) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN FALSE.

   IF fCheckTime(REPLACE(lcTime,":","")) EQ FALSE THEN RETURN FALSE.

   odeStamp = fHMS2TS(ldaDate,lcTime).
   IF odeStamp EQ ? OR odeStamp EQ 0 THEN RETURN FALSE.

   RETURN TRUE.

END FUNCTION.

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.

   lcInputFile = lcIncDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:
      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.
      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   ASSIGN
      liNumOk = 0
      liSkipped = 0
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
      IF lcLine EQ "" THEN NEXT.

      IF NUM-ENTRIES(lcLine,lcSep) NE 7 THEN DO:
         fLogLine("ERROR:Incorrect input data format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      ASSIGN
         lcSentTS = TRIM(ENTRY(2,lcLine,lcSep))
         lcContactNumber = TRIM(ENTRY(5,lcLine,lcSep))
         lcCustomerResponse = TRIM(TRIM(TRIM(ENTRY(7,lcLine,lcSep)),'"'))
      NO-ERROR.

      IF ERROR-STATUS:ERROR THEN DO:
         fLogLine("ERROR:Incorrect input data format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      IF lcContactNumber BEGINS "34" THEN
         lcContactNumber = SUBSTRING(lcContactNumber, 3).

      IF fParseTimeStamp(lcSentTs, OUTPUT ldeSentTs) EQ FALSE THEN DO:
         fLogLine("ERROR:Incorrect input data format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      RUN pHandleOfferSMSResponse(lcCustomerResponse,
                                  lcContactNumber,
                                  ldeSentTs).

      IF RETURN-VALUE BEGINS "SKIPPED" THEN
         liSkipped = liSkipped + 1.
      ELSE IF RETURN-VALUE BEGINS "ERROR" THEN
         liNumErr = liNumErr + 1 .
      ELSE liNumOK = liNumOK + 1 .

      fLogLine(RETURN-VALUE).
   END.

   PUT STREAM sLog UNFORMATTED
       "input: " STRING(liNumOK + liNumErr + liSkipped) ", "
       "updated: " STRING(liNumOK) ", "
       "skipped: " STRING(liSKipped) ", "
       "errors: " STRING(liNumErr) SKIP.

   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir).
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).

END.

INPUT STREAM sFile CLOSE.

FUNCTION fSendOfferSMSResp RETURNS LOGICAL (
   iiCustnum AS INT,
   icContactNumber AS CHAR,
   iiMsSeq AS INT,
   iiOrderID AS INT,
   icSMSToken AS CHAR,
   icContractId AS CHAR).

   DEF VAR lcSMSText AS CHAR NO-UNDO.
   DEF VAR ldReqStamp AS DEC NO-UNDO.
   DEF VAR lcSMSMessage AS CHAR NO-UNDO.

   lcSMSMessage = fGetSMSTxt(icSMSToken, TODAY, 1, OUTPUT ldReqStamp).

   IF lcSMSMessage EQ "" OR lcSMSMessage EQ ? THEN RETURN FALSE.

   lcSMSMessage = REPLACE(lcSMSMessage,"#CONTRACTID", icContractId).
   fCreateSMS(iiCustNum,
              icContactNumber,
              iiMsSeq,
              iiOrderId,
              lcSMSMessage,
              "622100100",
              {&SMS_TYPE_OFFER}).

   RETURN TRUE.
END FUNCTION.


PROCEDURE pHandleOfferSMSResponse:

   DEF INPUT PARAMETER icResponse AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icContactNumber AS CHAR NO-UNDO.
   DEF INPUT PARAMETER ideSentTs AS DEC NO-UNDO.

   DEF VAR lcCommand AS CHAR NO-UNDO.
   DEF VAR ldeCheckBuffer AS DEC NO-UNDO. 
   DEF VAR lcError AS CHAR NO-UNDO. 
   DEF VAR lcContractId AS CHAR NO-UNDO. 

   DEF BUFFER bOrder FOR Order.

   RELEASE Order.
   RELEASE bOrder.
   
   ASSIGN
      lcCommand      = ENTRY(1,icResponse, " ")
      lcContractID   = RIGHT-TRIM(ENTRY(2,icResponse, " "),".") WHEN 
         NUM-ENTRIES(icResponse," ") >= 2
      ldeCheckBuffer = fOffSet(fMakeTS(), -90 * 24).

   IF lcContractID > "" THEN
      /* find order based on contact number */
      FOR EACH SMSMessage NO-LOCK WHERE
               SMSMessage.MSISDN = icContactNumber AND
               SMSMessage.SMSType = {&SMS_TYPE_OFFER} AND
               SMSMessage.DeliType = {&SMS_DELITYPE_OUT}  BY CreStamp DESC:

         IF SMSMessage.CreStamp < ldeCheckBuffer THEN NEXT.

         IF SMSMessage.OrderId EQ 0 THEN NEXT.

         FIND FIRST bOrder NO-LOCK WHERE
                    bOrder.Brand  = gcBrand AND
                    bOrder.OrderId = SMSMessage.OrderID AND
                    bOrder.ContractId = lcContractID NO-ERROR.

         IF AVAIL bOrder THEN LEAVE.
      END.
   
   IF NOT AVAIL bOrder THEN
   /* find order based on contact number */
   FOR EACH SMSMessage NO-LOCK WHERE
            SMSMessage.MSISDN = icContactNumber AND
            SMSMessage.SMSType = {&SMS_TYPE_OFFER} AND
            SMSMessage.DeliType = {&SMS_DELITYPE_OUT}  BY CreStamp DESC:

      IF SMSMessage.CreStamp < ldeCheckBuffer THEN NEXT.

      IF SMSMessage.OrderId EQ 0 THEN NEXT.

      FIND FIRST bOrder NO-LOCK WHERE
                 bOrder.Brand  = gcBrand AND
                 bOrder.OrderId = SMSMessage.OrderID NO-ERROR.
      LEAVE.
   END.
   
   IF LOOKUP(lcCommand,"SI,NO") = 0 OR 
      NUM-ENTRIES(icResponse, " ") > 2 THEN DO:

      IF AVAIL bOrder THEN
      fSendOfferSMSResp(bOrder.Custnum,
                        icContactNumber,
                        bOrder.MsSeq,
                        bOrder.OrderId,
                        "OfferRespIncorrect",
                        bOrder.ContractId).

      lcError = "SKIPPED:Incorrect response (syntax)".
   END.
   
   /* find order based on contract id */
   IF lcError EQ "" AND lcContractId > "" THEN DO:

      FIND Order NO-LOCK WHERE
           Order.Brand = gcBrand AND
           Order.ContractId = lcContractID NO-ERROR.
      IF NOT AVAILABLE Order THEN DO:
            
         IF AVAIL bOrder THEN
         fSendOfferSMSResp(bOrder.Custnum,
                           icContactNumber,
                           bOrder.MsSeq,
                           bOrder.OrderId,
                           "OfferRespIncorrect",
                           bOrder.ContractID).

        lcError =  "SKIPPED:Incorrect response (order not found)".
      END.
   END.

   IF AVAIL bOrder THEN
   fCreateResponseSMS(bOrder.Custnum,
                      bOrder.MsSeq,
                      bOrder.OrderID,
                      icResponse,
                      icContactNumber,
                      {&SMS_TYPE_OFFER},
                      ideSentTs).

   IF lcError > "" THEN RETURN lcError.
   
   IF LOOKUP(lcCommand,"SI,NO") = 0
      OR
      (lcCommand EQ "SI" AND NOT AVAIL Order)
      OR
      (lcCommand EQ "NO" AND (icResponse NE "NO" AND NOT AVAIL Order)) THEN DO:

      IF AVAIL bOrder THEN
      fSendOfferSMSResp(bOrder.Custnum,
                        icContactNumber,
                        bOrder.MsSeq,
                        bOrder.OrderID,
                        "OfferRespIncorrect",
                        bOrder.ContractId).

      RETURN "SKIPPED:Incorrect response (syntax)".
   END.

   IF NOT AVAIL Order THEN DO:
      IF icResponse EQ "NO" AND AVAIL bOrder THEN 
         FIND Order NO-LOCK WHERE
              ROWID(Order) = ROWID(bOrder).
      ELSE RETURN "SKIPPED:Order(s) not found".
   END.

   IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} OR
      Order.OrderType EQ {&ORDER_TYPE_MNP} THEN DO:
      IF Order.CLI NE icContactNumber THEN 
         lcError = "SKIPPED:Incorrect response (order contact number does not match)".
   END.
   ELSE DO:

      FIND OrderCustomer NO-LOCK WHERE
           OrderCustomer.Brand = gcBrand AND
           OrderCustomer.OrderId = Order.OrderID AND
           OrderCustomer.RowType = 1.

      IF NOT AVAIL OrderCustomer OR
         (OrderCustomer.MobileNumber NE icContactNumber AND
          OrderCustomer.FixedNumber NE icContactNumber) THEN
         lcError = "SKIPPED:Incorrect response (order contact number does not match)".
   END.

   IF lcError > "" THEN DO:
      IF AVAIL bOrder THEN
      fSendOfferSMSResp(bOrder.Custnum,
                        icContactNumber,
                        bOrder.MsSeq,
                        bOrder.OrderID,
                        "OfferRespIncorrect",
                        bOrder.ContractId).

      RETURN lcError.
   END.
         
   IF Order.StatusCode NE {&ORDER_STATUS_OFFER_SENT} THEN DO:

      IF LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 AND
         lcCommand EQ "SI" THEN DO:

         fSendOfferSMSResp(Order.Custnum,
                           icContactNumber,
                           Order.MsSeq,
                           Order.OrderId,
                           (IF Order.OrderType EQ 2
                            THEN "OfferRespMissingRenewal"
                            ELSE "OfferRespMissingNew"),
                           Order.ContractId).

      END.

      RETURN SUBST("SKIPPED:Order status is &1", Order.StatusCode).
   END.

   CASE lcCommand:

      WHEN "SI" THEN DO:

         RUN Mc/orderinctrl.p(Order.OrderId, 0, TRUE).

         IF RETURN-VALUE BEGINS "ERROR" THEN RETURN
            "ERROR:Order releasing failed".
      END.
      WHEN "NO" THEN DO:

         RUN Mc/closeorder.p(Order.OrderId, TRUE).

         IF RETURN-VALUE BEGINS "ERROR" THEN
            RETURN "ERROR:Order closing failed".

         fSendOfferSMSResp(Order.Custnum,
                           icContactNumber,
                           Order.MsSeq,
                           Order.OrderId,
                           (IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL}
                            THEN "OfferNoRespRenewal"
                            ELSE "OfferNoRespNew"),
                           Order.ContractId).
      END.
   END.

   RETURN "OK".
END.
