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

FUNCTION fCparamNotNull RETURNS CHAR
   (icGR AS CHAR,
    icP AS CHAR):
   DEF VAR lcP AS CHAR NO-UNDO.
   lcP = fCParam(icGr,icP).
   IF lcP NE ? THEN RETURN lcP.
   ELSE RETURN "".

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
   RETURN fCParamNotNull("DMS",lcParam).

END.



/*DMS specific, quick implementation*/
FUNCTION fDoc2Msg RETURNS CHAR
   (icDocNbr AS CHAR,
    icDocComment AS CHAR):
   DEF VAR lcRet AS CHAR NO-UNDO.

   IF icDocComment = "" THEN icDocComment = "null".
   ELSE icDocComment =  "~"" + icDocComment + "~"".

   lcRet =  "~{" + "~"number~"" + "~:" + "~"" + icDocNbr +  "~"" + "," +
                  "~"revision_comment~"" + "~:" + icDocComment  +
            "~}".
   RETURN lcRet.
END.

/*Function generates JSON message for providing information for
  SMS/EMAIL sending. */
FUNCTION fGenerateMessage RETURNS CHAR
   (icNotifCaseId AS CHAR,
    icDeposit AS CHAR,
    icDocList AS CHAR,
    icDocListSep AS CHAR,
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
   DEF VAR lcDocList AS CHAR NO-UNDO. /*Plain list if required doc numbers*/
   DEF VAR i AS INT NO-UNDO.
   DEF VAR lcDocNotifEntry AS CHAR NO-UNDO.
   DEF VAR lcVersion AS CHAR NO-UNDO.
   DEF VAR lcRecEmail AS CHAR NO-UNDO.
   DEF VAR lcRecMSISDN AS CHAR NO-UNDO.


   IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN
      lcMSISDN = fNotNull(Order.CLI).
   ELSE 
      lcMSISDN = fNotNull(OrderCustomer.MobileNumber).

   lcContractID = fNotNull(Order.ContractId).
   lcDNIType = fNotNull(OrderCustomer.CustIdType).
   lcDNI = fNotNull(OrderCustomer.CustId).
   lcFname = fNotNull(OrderCustomer.FirstName).
   lcLname = fNotNull(OrderCustomer.SurName1) +
             fNotNull(Ordercustomer.SurName2).
   lcEmail = fNotNull(OrderCustomer.Email).
   lcBankAcc = fNotNull(OrderCustomer.BankCode).

   lcRecEmail = lcEmail.
   lcRecMSISDN = fNotNull(Order.CLI).

   lcSeq = STRING(NEXT-VALUE(SMSSEQ)). /*read and increase SMSSEQ. The sequence must be reserved as ID for WEB&HPD*/
   lcDocList = fNeededDocs(BUFFER Order).  
   lcArray = fInitJsonArray("documents").
   
   /*Add document comment if DMS has given it.*/
   IF icDocList EQ "" THEN DO: /*from TMS batch, initial information, use local*/
      DO i = 1 TO NUM-ENTRIES(lcDocList):
         lcDocNotifEntry = fDoc2Msg(ENTRY(i,lcDocList), 
                                    "").
         fObjectToJsonArray(lcArray, lcDocNotifEntry).
      END.
   END.
   ELSE DO: /*from DMS, add doc comments*/
      DO i = 1 TO NUM-ENTRIES(icDocList,icDocListSep) BY 4:
         lcDocNotifEntry = fDoc2Msg(ENTRY(i,icDocList,icDocListSep),
                                    ENTRY(i + 3,icDocList,icDocListSep)).
         fObjectToJsonArray(lcArray, lcDocNotifEntry).
      END.
   END.


   
   /*Fill data for message.*/
   lcMessage = "~{" + "~"metadata~""  + "~:" + "~{" +
                         "~"version~""  + "~:" + "~"" + lcVersion + "~"," +
                         "~"case~""  + "~:" + "~"" + icNotifCaseID  + "~"," +
                         lcArray + "," +
                         "~"smsseq~""  + "~:" + "~"" + lcSeq  + "~"," +
                         "~"recipient_email~""  + "~:" + "~"" + 
                            lcRecEmail  + "~"," +
                         "~"recipient_msisdn~""  + "~:" + "~"" + 
                            lcRecMSISDN  + "~"" +
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

FUNCTION fDmsConfig RETURNS CHAR ():
   DEF VAR lcHostName AS CHAR NO-UNDO.   
   DEF VAR lcConfFile AS CHAR NO-UNDO.

   /* get hostname */
   INPUT THROUGH uname -n.
   IMPORT lcHostName.
   INPUT CLOSE.
   /*TODO: make constants for environments!!!*/
   CASE lcHostName:
      WHEN "Alpheratz" THEN DO:
         lcConfFile = "Mailconf/dms_messaging_conf.alpheratz".
      END.
      WHEN "Pallas" THEN DO:
         lcConfFile = "Mailconf/dms_messaging_conf.prod".
      END.
      OTHERWISE DO:
         RETURN "Unknown configuration".
      END.
   END CASE.
   RETURN lcConfFile.
END.

/*Function sends SMS and EMAIL generating information to WEB if it is needed*/
FUNCTION fSendChangeInformation RETURNS CHAR
   (icDMSStatus AS CHAR, /*DMS Status*/ 
    icOrderID AS INT,    /*Order*/
    icDeposit AS CHAR,   /*Deposit, if available*/
    icDocList AS CHAR,   /*Doc List if available*/
    icDocListSep AS CHAR, /*Separator if doc list is available*/
    icModule AS CHAR,    /*identifier for MQ log file */
    OUTPUT ocSentMessage AS CHAR): /*for debugging, also includes additional data related to message.*/

   DEF BUFFER Order FOR Order.
   DEF BUFFER OrderCustomer FOR OrderCustomer.
   
   DEF VAR lcConfig AS CHAR NO-UNDO.
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
              OrderCustomer.Brand EQ gcBrand AND
              OrderCustomer.OrderId EQ icOrderID AND
              OrderCustomer.RowType EQ 1 NO-ERROR.
   IF NOT AVAIL Order THEN RETURN "DMS Notif: No OrderCustomer available".

   /*Messages are sent only for direct channel orders.*/
   IF R-INDEX(Order.OrderChannel, "pos") NE 0 THEN RETURN "".

   /*DMS triggered cases:*/
   /*Read Parameter that defines case ID*/
   IF icDMSStatus NE "" THEN DO:
      lcParam = "DMSMsgID_" + icDMSStatus. /*DMSMsgIF_E -> returns 03*/
      lcNotifCaseID = fCParamNotNull("DMS",lcParam).

      IF lcNotifCaseID EQ "" THEN RETURN "No Message for " + lcParam.
   END.
   ELSE DO:
   /*Get the caseId by using TMS information. This is used in casefile 
     sending.*/
       lcParam = "DMSMsgID_" + Order.StatusCode. /*DMSMsgIF_20 -> returns 1*/
       lcNotifCaseID = fCParamNotNull("DMS",lcParam).
       IF lcNotifCaseID EQ "" THEN RETURN "No Message for " + lcParam.
   END.
   lcMessage = fGenerateMessage(lcNotifCaseID,
                                icDeposit,
                                icDocList,
                                icDocListSep,
                                BUFFER Order,
                                BUFFER Ordercustomer).

   ocSentMessage = "Case param: " + lcParam + "Msg: " + lcMessage.              
   lcMQ =  fCParamNotNull("DMS","DMS_MQ"). 
   lcConfig = fDMSConfig().
   IF lcConfig EQ "" THEN RETURN "MQ config not available".
   RETURN fSendToMQ(lcMessage, lcMQ, lcConfig, icModule).
END.


