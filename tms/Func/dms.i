/* dms.i         03.09.15/ivekov 

*/

{commali.i}
{tmsparam4.i}
{timestamp.i}
{cparam2.i}
{tmsconst.i}
{amq.i}
{jsonlib.i}

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


FUNCTION fNeededDocs RETURNS CHAR
   (BUFFER Order FOR Order):
   DEF VAR lcPAram AS CHAR NO-UNDO.
   DEF VAR liCount AS INT NO-UNDO.
   DEF VAR lcDocListEntries AS CHAR NO-UNDO.

   /*CASE More DOC needed*/
   IF Order.StatusCode EQ  {&ORDER_STATUS_MORE_DOC_NEEDED}  THEN DO: /*44*/
     /*portability pos-pos*/
      IF Order.OrderType EQ {&ORDER_TYPE_MNP} AND
         Order.PayType EQ FALSE AND
         Order.OldPayType EQ FALSE  THEN
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T1".
      /*new add pos / portability pre-pos.*/
      ELSE IF (Order.OrderType EQ {&ORDER_TYPE_NEW} AND
               Order.PayType EQ FALSE )
         OR
              (Order.OrderType EQ {&ORDER_TYPE_MNP} AND
              Order.PayType EQ FALSE AND
              Order.OldPayType EQ TRUE ) THEN
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T2".
      /*stc to pos / migration+renewal*/
      ELSE IF Order.OrderType EQ {&ORDER_TYPE_STC} AND
              Order.PayType EQ FALSE
         OR
              Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T3".
      /*add new pre / portability to pre*/
      ELSE IF Order.OrderType EQ {&ORDER_TYPE_NEW} AND
              Order.PayType EQ TRUE
         OR
              Order.OrderType EQ {&ORDER_TYPE_MNP} AND
              Order.PayType EQ TRUE THEN
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T4".
   END.
   /*Company orders:*/
   ELSE IF Order.StatusCode EQ {&ORDER_STATUS_COMPANY_NEW} THEN DO:
      IF Order.OrderType EQ {&ORDER_TYPE_MNP} AND
         Order.PayType EQ FALSE AND
         Order.OldPayType EQ FALSE  THEN
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T1".
      /*new add pos / portability pre-pos.*/
      ELSE IF (Order.OrderType EQ {&ORDER_TYPE_NEW} AND
               Order.PayType EQ FALSE )
         OR
              (Order.OrderType EQ {&ORDER_TYPE_MNP} AND
               Order.PayType EQ FALSE AND
               Order.OldPayType EQ TRUE ) THEN
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T2".

      /*add new pre / portability to pre*/
      ELSE IF Order.OrderType EQ {&ORDER_TYPE_NEW} AND
              Order.PayType EQ TRUE
         OR
              Order.OrderType EQ {&ORDER_TYPE_MNP} AND
              Order.PayType EQ TRUE THEN
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T3".

   END.
   /*CASE 21,33*/
   ELSE IF Order.Statuscode EQ {&ORDER_STATUS_RENEWAL_STC_COMPANY} OR
           Order.StatusCode EQ {&ORDER_STATUS_COMPANY_MNP} THEN DO:
      IF Order.OrderType EQ {&ORDER_TYPE_MNP} AND
         Order.PayType EQ FALSE AND
         Order.OldPayType EQ FALSE  THEN
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T1".
      /*new add pos / portability pre-pos.*/
      ELSE IF (Order.OrderType EQ {&ORDER_TYPE_NEW} AND
               Order.PayType EQ FALSE )
         OR
              (Order.OrderType EQ {&ORDER_TYPE_MNP} AND
               Order.PayType EQ FALSE AND
               Order.OldPayType EQ TRUE ) THEN
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T2".
      /*stc to pos / migration+renewal*/
      ELSE IF Order.OrderType EQ {&ORDER_TYPE_STC} AND
              Order.PayType EQ FALSE
         OR
              Order.OrderType EQ {&ORDER_TYPE_RENEWAL} /* AND
              Order.OrderChannel EQ TODO */ THEN
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T3".
      /*add new pre / portability to pre*/
      ELSE IF Order.OrderType EQ {&ORDER_TYPE_NEW} AND
              Order.PayType EQ TRUE
         OR
              Order.OrderType EQ {&ORDER_TYPE_MNP} AND
              Order.PayType EQ TRUE THEN
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T4".

   END.
   RETURN fCParam("DMS",lcParam).

END.

FUNCTION fGenerateMessage RETURNS CHAR
   (icNotifCaseId AS CHAR,
    icDeposit AS CHAR,
   BUFFER Order FOR Order,
   BUFFER Ordercustomer FOR Ordercustomer):
  
   DEF VAR lcMSISDN AS CHAR NO-UNDO.
   DEF VAR lcContractID AS CHAR NO-UNDO.
   DEF VAR lcDNIType AS CHAR NO-UNDO.
   DEF VAR lcDNI AS CHAR NO-UNDO.
   DEF VAR lcFname AS CHAR NO-UNDO.
   DEF VAR lcLname AS CHAR NO-UNDO.
   DEF VAR lcEmail AS CHAR NO-UNDO.
   DEF VAR lcBankAcc AS CHAR NO-UNDO.
   DEF VAR lcSeq AS CHAR NO-UNDO.
   DEF VAR lcMessage AS CHAR NO-UNDO.
   DEF VAR lcArray AS CHAR NO-UNDO.
   DEF VAR lcDocList AS CHAR NO-UNDO.
   DEF VAR liCount AS INT NO-UNDO.


   IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN
      lcMSISDN = fNotNull(OrderCustomer.ContactNum).
   ELSE lcMSISDN = fNotNull(Order.CLI).

   lcContractID = fNotNull(Order.ContractId).
   lcDNIType = fNotNull(OrderCustomer.CustIdType).
   lcDNI = fNotNull(OrderCustomer.CustId).
   lcFname = fNotNull(OrderCustomer.FirstName).
   lcLname = fNotNull(OrderCustomer.SurName1) +
             fNotNull(Ordercustomer.SurName2).
   lcEmail = fNotNull(OrderCustomer.Email).
   lcBankAcc = fNotNull(OrderCustomer.BankCode).

   lcSeq = STRING(NEXT-VALUE(SMSSEQ)). /*read and increase SMSSEQ. The sequence must be reserved as ID for WEB&HPD*/
   lcDocList = fNeededDocs(BUFFER Order).  
   DO liCount = 1 TO NUM-ENTRIES(lcDocList):
      fAddToJsonArray(lcArray, STRING(ENTRY(liCount,lcDocList))).
   END.

   /*Fill data for message.*/
   lcMessage = "~{" + "~"metadata~""  + "~:" + "~{" +
                         "~"case~""  + "~:" + "~"" + icNotifCaseID  + "~"," +
                         lcArray + "," +
                         "~"smsseq~""  + "~:" + "~"" + lcSeq  + "~"" +
                     "~}" + "," +
                      "~"data~"" + "~:" + "~{" +
                         "~"msisdn~""   + "~:" + "~"" + lcMSISDN + "~"" + "," +
                         "~"contractid~"" +  "~:" + "~"" +
                                     lcContractID + "~"" + "," +
                         "~"dni_type~"" +  "~:" + "~"" + 
                                     lcDNIType + "~"" + "," +
                         "~"dni~""      +  "~:" + "~"" + lcDNI + "~"" + "," +
                         "~"fname~""    +  "~:" + "~"" + lcFname + "~"" + "," +
                         "~"lname~""    +  "~:" + "~"" + lcLname + "~"" + "," +
                         "~"email~""    +  "~:" + "~"" + lcEmail + "~"" + "," + 
                         "~"deposit_amount~""   +  "~:" + "~"" +
                                      icDeposit + "~"" + "," +
                         "~"bank_account_number~"" +  "~:" + "~"" +
                                      lcBankAcc + "~"" +
                      "~}" +
                 "~}".
   RETURN lcMessage.
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
   DEF VAR lcMQ AS CHAR NO-UNDO.
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

   /*Messages are sent only for direct channel orders.*/
   IF R-INDEX(Order.OrderChannel, "pos") NE 0 THEN RETURN "".

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
       IF lcNotifCaseID EQ "" THEN RETURN "". /*No actions for the case*/

   END.
   lcMessage = fGenerateMessage(lcNotifCaseID,
                                icDeposit,
                                BUFFER Order,
                                BUFFER Ordercustomer).

   ocSentMessage = lcMessage.              
   lcMQ =  fCParam("DMS","DMS_MQ"). 
   RETURN fSendToMQ(lcMessage, "dms", lcMQ).
END.


