/* dms.i         03.09.15/ivekov 

*/

{commali.i}
{tmsparam4.i}
{timestamp.i}
{cparam2.i}
{tmsconst.i}
{amq.i}

ASSIGN
   katun   = "Cron"
   gcBrand = "1".

DEF TEMP-TABLE ttDMS    NO-UNDO LIKE DMS.
DEF TEMP-TABLE ttDMSDoc NO-UNDO LIKE DMSDoc.

DEF TEMP-TABLE ttDocs NO-UNDO
   FIELD DocTypeID      AS CHAR
   FIELD DocTypeDesc    AS CHAR
   FIELD DocStatusCode  AS CHAR
   FIELD DocStatusDesc  AS CHAR
   FIELD DMSStatusTS    AS DEC FORMAT "99999999.99999"
   FIELD Comment        AS CHAR.

FUNCTION fGetOrderStatusDMS RETURNS CHAR
   (icContractID AS CHAR):
   FIND FIRST DMS NO-LOCK WHERE
              DMS.ContractID EQ icContractID
              NO-ERROR.
   IF AVAIL DMS THEN RETURN DMS.OrderStatus.
   RETURN "".      
END.

FUNCTION fUpdateDMS RETURNS CHAR
   (icDmsExternalID  AS CHAR,
    icCaseTypeID     AS CHAR,
    icContractID     AS CHAR,
    icHostTable      AS CHAR,
    iiHostId         AS INT,
    icStatusCode     AS CHAR,
    icStatusDesc     AS CHAR,
    icOrderStatus    AS CHAR,
    idStatusTS       AS DEC,
    icDocList        AS CHAR,
    icDocListSep     AS CHAR):

   DEF VAR i         AS INT NO-UNDO.
   DEF VAR llCompare AS LOG NO-UNDO.

   FIND DMS EXCLUSIVE-LOCK WHERE
        DMS.ContractID = icContractID 
        NO-ERROR.

   IF AMBIGUOUS(DMS) THEN RETURN "AMBIGUOUS DMS".
   ELSE IF NOT AVAIL DMS THEN DO:
      CREATE DMS.
      ASSIGN DMS.DMSID    = NEXT-VALUE(DMS)
             DMS.StatusTS = fMakeTS().
   END.

   CREATE ttDMS.
   BUFFER-COPY DMS TO ttDMS.

   ASSIGN DMS.DmsExternalID = icDmsExternalID WHEN icDmsExternalID NE ""
          DMS.CaseTypeID    = icCaseTypeID
          DMS.ContractID    = icContractID
          DMS.HostTable     = icHostTable
          DMS.HostId        = iiHostId
          DMS.StatusCode    = icStatusCode
          DMS.StatusDesc    = icStatusDesc
          DMS.DMSStatusTS   = idStatusTS.

   /* Store current order status */
   IF NEW DMS AND icOrderstatus = "" THEN DO:
      FIND FIRST Order NO-LOCK WHERE
                 Order.Brand = gcBrand AND
                 Order.OrderID = iiHostId NO-ERROR.
      IF AVAILABLE Order THEN DMS.OrderStatus = Order.StatusCode.
   END.
   ELSE IF icOrderStatus NE "" THEN DMS.OrderStatus = icOrderstatus.

   IF NOT NEW DMS THEN
      BUFFER-COMPARE DMS TO ttDMS SAVE RESULT IN llCompare.
   IF NOT llCompare THEN DMS.StatusTS = fMakeTS().


   IF icDocList <> "" THEN
      DO i = 1 TO NUM-ENTRIES(icDocList,icDocListSep) BY 4:

      FIND FIRST DMSDoc EXCLUSIVE-LOCK WHERE
                 DMSDoc.DMSID     = DMS.DMSID AND
                 DMSDoc.DocTypeID = ENTRY(i,icDocList,icDocListSep)
                 NO-ERROR.

      IF NOT AVAIL DMSDoc THEN DO:
         CREATE DMSDoc.
         ASSIGN DMSDoc.DMSID       = DMS.DMSID
                DMSDoc.DocStatusTS = fMakeTS().
      END.

      CREATE ttDMSDoc.
      BUFFER-COPY DMSDoc TO ttDMSDoc.

      ASSIGN DMSDoc.DocTypeID     = ENTRY(i,icDocList,icDocListSep)
             DMSDoc.DocTypeDesc   = ENTRY(i + 1,icDocList,icDocListSep)
             DMSDoc.DocStatusCode = ENTRY(i + 2,icDocList,icDocListSep)
             DMSDoc.DocRevComment = ENTRY(i + 3,icDocList,icDocListSep)
             DMSDoc.DMSStatusTS   = idStatusTS.

      IF NOT NEW DMSDoc THEN
         BUFFER-COMPARE DMSDoc TO ttDMSDoc SAVE RESULT IN llCompare.
      IF NOT llCompare THEN DMSDoc.DocStatusTS = fMakeTS().

   END.

   RELEASE DMS.
   RELEASE DMSDoc.

   RETURN "OK".

END.

FUNCTION fCollectDocs RETURNS LOGICAL
   (INPUT iiDMSID AS INT).
   FOR EACH DMSDoc NO-LOCK WHERE
            DMSDoc.DMSID = iiDMSID:
      CREATE ttDocs.
      ASSIGN ttDocs.DocTypeID     = DMSDoc.DocTypeID
             ttDocs.DocTypeDesc   = DMSDoc.DocTypeDesc
             ttDocs.DocStatusCode = DMSDoc.DocStatusCode
             ttDocs.DMSStatusTS   = DMSDoc.DMSStatusTS
             ttDocs.Comment       = DMSDoc.DocRevComment.

      CASE DMSDoc.DocStatusCode:
         WHEN "A" THEN ttDocs.DocStatusDesc = "Pending".
         WHEN "B" THEN ttDocs.DocStatusDesc = "Sent".
         WHEN "C" THEN ttDocs.DocStatusDesc = "Error".
         WHEN "D" THEN ttDocs.DocStatusDesc = "OK".
      END CASE.
   END.
   
END FUNCTION. 

FUNCTION fDMSOnOff RETURNS LOGICAL:
   DEF VAR liOnOff AS INT NO-UNDO.
   liOnOff = INT(fCParamI4("1","DMS","DmsOnOff")).
   IF liOnOff EQ 1 THEN RETURN TRUE.
   ELSE RETURN FALSE.
END.


FUNCTION fSendToMQ RETURNS CHAR
   (icMsg AS CHAR):
   DEF VAR lcRet AS CHAR NO-UNDO.
   RUN pInitialize(INPUT "dms").

   IF RETURN-VALUE > "" THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR").

         RETURN RETURN-VALUE.
   END.
   /* Call ActiveMQ Publisher class */
   lMsgPublisher = NEW Gwy.MqPublisher(lcHost,liPort,
                                       liTimeOut,{&DMS_MQ_ID}, 
                                       lcUserName,lcPassword).

   IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN DO:
            LOG-MANAGER:WRITE-MESSAGE("ActiveMQ Publisher handle not found",
                                    "ERROR").
            lcRet = "ActiveMQ Publisher handle not found".
      END.
   END.
   ELSE IF NOT lMsgPublisher:send_message(icMsg) THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN DO:
         LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
         lcRet = "ActiveMQ message sending failed".
      END.
   END.
   RUN pFinalize(INPUT "").
   RETURN lcRet.

END.


/*Function sends SMS and EMAIL generating information to WEB if it is needed*/
FUNCTION fSendChangeInformation RETURNS CHAR
   (icDMSStatus AS CHAR,
    icOrderID AS INT,
    icDeposit AS CHAR,
    OUTPUT ocSentMessage AS CHAR):

   DEF BUFFER Order FOR Order.
   DEF BUFFER OrderCustomer FOR OrderCustomer.

   DEF VAR lcNotifCaseID AS CHAR NO-UNDO.
   DEF VAR lcParam AS CHAR NO-UNDO.
   DEF VAR lcMessage AS CHAR NO-UNDO.
   DEF VAR lcMSISDN AS CHAR NO-UNDO.
   DEF VAR lcContractID AS CHAR NO-UNDO.
   DEF VAR lcDNIType AS CHAR NO-UNDO.
   DEF VAR lcDNI AS CHAR NO-UNDO.
   DEF VAR lcFname AS CHAR NO-UNDO.
   DEF VAR lcLname AS CHAR NO-UNDO.
   DEF VAR lcEmail AS CHAR NO-UNDO.
   DEF VAR lcBankAcc AS CHAR NO-UNDO.
   DEF VAR lcSeq AS CHAR NO-UNDO.

   /*search data for message*/
   FIND FIRST Order NO-LOCK WHERE
              Order.Brand EQ gcBrand AND
              Order.OrderId EQ icOrderID NO-ERROR.
   IF NOT AVAIL Order THEN RETURN "DMS Notif: No Order available".

   FIND FIRST OrderCustomer NO-LOCK WHERE
              Order.Brand EQ gcBrand AND
              Order.OrderId EQ icOrderID AND
              OrderCustomer.RowType EQ 1 NO-ERROR.
   IF NOT AVAIL Order THEN RETURN "DMS Notif: No OrderCustomer available".

   /*DMS triggered cases:*/
   /*Read Parameter that defines case ID*/
   IF icDMSStatus NE "" THEN DO:
      lcParam = "DMSMsgID_" + icDMSStatus. /*DMSMsgIF_E -> returns 03*/
      lcNotifCaseID = fCParam("DMS",lcParam).

      IF lcNotifCaseID EQ "" THEN RETURN "". /*No actions for the case*/
   END.
   ELSE DO:
   /*Get the caseId by using TMS information. This is used in casefile 
     sending.*/
       lcParam = "DMSMsgID_" + Order.StatusCode. /*DMSMsgIF_20 -> returns 1*/
       lcNotifCaseID = fCParam("DMS",lcParam).
   END.
   IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN 
      lcMSISDN = fNotNull(OrderCustomer.ContactNum).
   ELSE lcMSISDN = fNotNull(OrderCustomer.ContactNum).

   lcContractID = fNotNull(Order.ContractId).
   lcDNIType = fNotNull(OrderCustomer.CustIdType).
   lcDNI = fNotNull(OrderCustomer.CustId).
   lcFname = fNotNull(OrderCustomer.FirstName).
   lcLname = fNotNull(OrderCustomer.SurName1) +
             fNotNull(Ordercustomer.SurName2).
   lcEmail = fNotNull(OrderCustomer.Email).
   lcBankAcc = fNotNull(OrderCustomer.BankCode).
   lcSeq = STRING(NEXT-VALUE(SMSSEQ)). /*read and increase SMSSEQ. The sequence must be 
                                         reserved as ID for WEB&HPD*/


   /*Fill data for message.*/
   lcMessage = "~{" + "~"metadata~""  + "~:" + "~{" +
                         "~"case~""  + "~:" + "~"" + lcNotifCaseID  + "~"," +
                         "~"smsseq~""  + "~:" + "~"" + lcSeq  + "~"" +
                     "~}" + "," +
                      "~"data~"" + "~:" + "~{" +
                         "~"msisdn~""   + "~:" + "~"" + lcMSISDN + "~"" + "," +
                         "~"contractid~"" +  "~:" + "~"" +
                                     lcContractID + "~"" + "," +
                         "~"dni_type~"" +  "~:" + "~"" + lcDNIType + "~"" + "," +
                         "~"dni~""      +  "~:" + "~"" + lcFname + "~"" + "," +
                         "~"fname~""    +  "~:" + "~"" + lcLname + "~"" + "," +
                         "~"lname~""    +  "~:" + "~"" + lcEmail + "~"" + "," +
                         "~"deposit_amount~""   +  "~:" + "~"" +
                                      icDeposit + "~"" + "," +
                         "~"bank_account_number~"" +  "~:" + "~"" +
                                      lcBankAcc + "~"" +
                      "~}" +
                 "~}".
   ocSentMessage = lcMessage.              
   RETURN fSendToMQ(lcMessage).
END.


