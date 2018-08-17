/* dms.i         03.09.15/ivekov 

*/

{Syst/commali.i}
{Func/tmsparam4.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/amq.i}
{Func/jsonlib.i}

ASSIGN
   Syst.Var:gcBrand = "1".

DEF TEMP-TABLE ttDocs NO-UNDO
   FIELD DocTypeID      AS CHAR
   FIELD DocTypeDesc    AS CHAR
   FIELD DocStatusCode  AS CHAR
   FIELD DocStatusDesc  AS CHAR
   FIELD DMSStatusTS    AS DEC FORMAT "99999999.99999"
   FIELD Comment        AS CHAR.

FUNCTION fChkDMSExists RETURNS LOGICAL
   (icHostTable   AS CHARACTER,
    iiHostID      AS INTEGER):

   DEFINE BUFFER bDMS FOR DMS.

   FIND FIRST bDMS NO-LOCK WHERE
              bDMS.HostTable   = icHostTable  AND
              bDMS.HostID      = iiHostID
              NO-ERROR.

   RETURN AVAILABLE(bDMS).

END.

FUNCTION fGetOrderStatusDMS RETURNS CHAR
   (icContractID AS CHAR):
   FIND FIRST DMS NO-LOCK WHERE
              DMS.ContractID EQ icContractID
              NO-ERROR.
   IF AVAIL DMS THEN RETURN DMS.OrderStatus.
   RETURN "".      
END.

FUNCTION fSetSendToROI RETURNS LOGICAL
   (iiOrderId AS INTEGER):

   DEFINE BUFFER lbOrder   FOR Order.

   FIND FIRST lbOrder EXCLUSIVE-LOCK WHERE
              lbOrder.Brand     = Syst.Var:gcBrand AND
              lbOrder.OrderID   = iiOrderId NO-ERROR.
   IF AVAILABLE lbOrder THEN
      lbOrder.SendToROI = {&ROI_HISTORY_TO_SEND}.

END.

FUNCTION fGetOrderDMSStatus RETURNS CHAR
   (iiOrderId AS INTEGER):

   FIND FIRST DMS NO-LOCK WHERE
              DMS.HostTable = {&DMS_HOST_TABLE_ORDER} AND
              DMS.HostID    = iiOrderId
              NO-ERROR.
   IF AVAIL DMS THEN RETURN DMS.StatusCode.

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
   DEFINE VARIABLE lcPrevStatusCode    AS    CHARACTER   NO-UNDO.

   FIND DMS EXCLUSIVE-LOCK WHERE
        DMS.ContractID = icContractID 
        NO-ERROR.

   IF AMBIGUOUS(DMS) THEN RETURN "AMBIGUOUS DMS".
   ELSE IF NOT AVAIL DMS THEN DO:
      CREATE DMS.
      ASSIGN DMS.DMSID    = NEXT-VALUE(DMS)
             DMS.StatusTS = Func.Common:mMakeTS().
   END.
   ELSE
      lcPrevStatusCode = DMS.StatusCode.

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
                 Order.Brand = Syst.Var:gcBrand AND
                 Order.OrderID = iiHostId NO-ERROR.
      IF AVAILABLE Order THEN DMS.OrderStatus = Order.StatusCode.
   END.
   ELSE IF icOrderStatus NE "" THEN DMS.OrderStatus = icOrderstatus.

   IF LOOKUP(DMS.OrderStatus, {&ORDER_SENDTOROI_STATUSES}) > 0 AND 
      (NEW DMS OR DMS.StatusCode <> lcPrevStatusCode) 
   THEN
      fSetSendToROI(DMS.HostId).

   /*YPR-3077:A0 response must erase SENT doocuments*/
   IF DMS.StatusCode EQ "A0" THEN DO:
      FOR EACH DMSDoc EXCLUSIVE-LOCK WHERE
               DMSDoc.DMSID EQ DMS.DMSID AND
               DMSDOC.DocStatusCode EQ {&DMS_INIT_STATUS_SENT}:
         DELETE DMSDoc.         
      END.
   END.

   IF icDocList <> "" THEN
      DO i = 1 TO NUM-ENTRIES(icDocList,icDocListSep) BY 4:

      FIND FIRST DMSDoc EXCLUSIVE-LOCK WHERE
                 DMSDoc.DMSID     = DMS.DMSID AND
                 DMSDoc.DocTypeID = ENTRY(i,icDocList,icDocListSep)
                 NO-ERROR.

      IF NOT AVAIL DMSDoc THEN DO:
         CREATE DMSDoc.
         ASSIGN DMSDoc.DMSID       = DMS.DMSID
                DMSDoc.DocStatusTS = Func.Common:mMakeTS().
      END.

      ASSIGN DMSDoc.DocTypeID     = ENTRY(i,icDocList,icDocListSep)
             DMSDoc.DocTypeDesc   = ENTRY(i + 1,icDocList,icDocListSep)
             DMSDoc.DocStatusCode = ENTRY(i + 2,icDocList,icDocListSep)
             DMSDoc.DocRevComment = ENTRY(i + 3,icDocList,icDocListSep)
             DMSDoc.DMSStatusTS   = idStatusTS.
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

FUNCTION fIsHolder RETURNS LOGICAL
   (iiOrderId AS INTEGER,
    iiRowType AS INTEGER):

   RETURN CAN-FIND(FIRST OrderCustomer NO-LOCK WHERE
                         OrderCustomer.Brand = Syst.Var:gcBrand     AND
                         OrderCustomer.OrderId = iiOrderId AND
                         OrderCustomer.RowType = iiRowType).

END FUNCTION.

/* Function is used in customer category change documentation */
/* If we see that there are need for changes a specific CPARAM will be added */
FUNCTION fNeededDocsCategoryChange RETURNS CHAR
():
   RETURN "15".
END.

FUNCTION fNeededDocs RETURNS CHAR
   (BUFFER Order FOR Order):
   DEF VAR lcParam AS CHAR NO-UNDO.

   /*CASE More DOC needed*/
   IF Order.StatusCode EQ  {&ORDER_STATUS_MORE_DOC_NEEDED}  THEN DO: /*44*/
     /*portability pos-pos*/
      IF Order.OrderType EQ {&ORDER_TYPE_MNP} AND
         Order.PayType EQ FALSE AND
         Order.OldPayType EQ FALSE
      THEN lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T1" +
                     IF fIsHolder(Order.OrderId, {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER})
                     THEN ".1" ELSE "".
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
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T1" +
                   IF fIsHolder(Order.OrderId, {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER})
                   THEN ".1" ELSE "".
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
         lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T1" +
                   IF fIsHolder(Order.OrderId, {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER})
                   THEN ".1" ELSE "".
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
   RETURN fCParamNotNull("DMS",lcParam) + /*NOTE: in this return the doc list is comma separated*/
          IF fIsHolder(Order.OrderId, {&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER})
          THEN ",12,14" ELSE "".

END.


/*DMS specific, quick implementation*/
FUNCTION fDoc2Msg RETURNS CHAR
   (icDocNbr AS CHAR,
    icDocComment AS CHAR,
    icDocDesc AS CHAR):
   DEF VAR lcRet AS CHAR NO-UNDO.

   IF icDocComment = "" THEN icDocComment = "null".
   ELSE icDocComment =  "~"" + icDocComment + "~"".
   lcRet =  "~{" + "~"number~"" + "~:" + "~"" + icDocNbr +  "~"" + ",".
   IF icDocNbr EQ "10" THEN
      lcRet = lcRet + "~"type_description~"" + "~:" + "~"" + icDocDesc +  "~"" + ",".
   lcRet = lcRet + "~"revision_comment~"" + "~:" + icDocComment  +
            "~}".

   RETURN lcRet.
END.


FUNCTION fGetBankName RETURNS CHAR
   (icCode AS CHAR):
   FIND FIRST Bank WHERE
              Bank.Brand      = Syst.Var:gcBrand AND
              Bank.BankID     = SUBSTRING(icCode,5,4) NO-LOCK NO-ERROR.
   IF AVAIL Bank THEN RETURN Bank.Name.
   RETURN "".
END.   

/*returns document type - comment - description sets: "1;PENDING;comment2;SENT;2;PENDING..."*/
FUNCTION fDocListByOrder RETURNS CHAR
   (iiOrderId AS INT,
    icNotifCaseId AS CHAR):
   DEF BUFFER bDMS FOR DMS.
   DEF BUFFER bDMSDOC FOR DMSDOC.
   DEF VAR lcDocList AS CHAR NO-UNDO.
   DEF VAR lcDocReminderStatuses AS CHAR NO-UNDO. /*docs that need reminder */
   DEF VAR i AS INT NO-UNDO.

   FIND FIRST bDMS NO-LOCK WHERE
              bDMS.HostTable EQ {&DMS_HOST_TABLE_ORDER} AND
              bDMS.HostId EQ iiOrderID NO-ERROR.

   IF NOT AVAIL bDMS THEN RETURN "".
   FOR EACH bDMSDOC NO-LOCK WHERE
            bDMSDOC.DMSID EQ bDMS.DMSID BY bDMSDOC.DocTypeId:
      IF icNotifCaseID EQ {&DMS_INITIAL_NOTIF_CASE} /*1*/ THEN DO:
         IF bDMSDOC.DocStatusCode EQ {&DMS_INIT_STATUS_SENT} THEN DO:
            IF lcDocList NE "" THEN lcDocList = lcDocList + {&DMS_DOCLIST_SEP}.
            lcDocList = lcDocList + bDmsDoc.DocTypeId + {&DMS_DOCLIST_SEP}  +
                                    /*empty*/           {&DMS_DOCLIST_SEP} +
                                    bDmsDoc.DocTypeDesc.
         END.
      END.
      ELSE DO:
         lcDocReminderStatuses =  fCParam("DMS","DMS_doc_reminder_statuses"). /*A,C*/
  /*       lcDocReminderStatuses = "A,C".*/
         IF LOOKUP(bDMSDOC.DocStatusCode, lcDocReminderStatuses) > 0 THEN DO:
            IF lcDocList NE "" THEN lcDocList = lcDocList + {&DMS_DOCLIST_SEP}.
            lcDocList = lcDocList + bDmsDoc.DocTypeId + {&DMS_DOCLIST_SEP} +
                                    bDmsDoc.DocRevComment + {&DMS_DOCLIST_SEP} +
                                    bDmsDoc.DocTypeDesc.
         END. 
      END.

   END.
   RETURN lcDocList.
END.


/*Function generates JSON message for providing information for
  SMS/EMAIL sending. */
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
   DEF VAR lcDocList AS CHAR NO-UNDO. /*Plain list if required doc numbers*/
   DEF VAR i AS INT NO-UNDO.
   DEF VAR lcDocNotifEntry AS CHAR NO-UNDO.
   DEF VAR lcVersion AS CHAR NO-UNDO.
   DEF VAR lcRecEmail AS CHAR NO-UNDO.
   DEF VAR lcRecMSISDN AS CHAR NO-UNDO.
   DEF VAR lcBankName AS CHAR NO-UNDO.



   IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN
      lcRecMSISDN = fNotNull(Order.CLI).
   ELSE 
      lcRecMSISDN = fNotNull(OrderCustomer.MobileNumber).

   lcVersion = "3".
   lcContractID = fNotNull(Order.ContractId).
   lcDNIType = fNotNull(OrderCustomer.CustIdType).
   lcDNI = fNotNull(OrderCustomer.CustId).
   lcFname = fNotNull(OrderCustomer.FirstName).
   lcLname = fNotNull(OrderCustomer.SurName1) + " " + 
             fNotNull(Ordercustomer.SurName2).
   lcEmail = fNotNull(OrderCustomer.Email).
   lcBankAcc = "ES13 0049 1500 08 2310410432". 
   lcBankName = "Santander".

   lcRecEmail = lcEmail.

   lcMSISDN = fNotNull(Order.CLI).

   lcSeq = STRING(NEXT-VALUE(SMSSEQ)). /*read and increase SMSSEQ. The sequence must be reserved as ID for WEB&HPD*/
   lcArray = fInitJsonArray("documents").

/*Doc list is created from matrix in type1(initial info of the case) notificatoins. 
In other notifications only A&C (pending&error) cases are sent to identify missing docs*/
   lcDocList = fDocListByOrder(Order.Orderid, icNotifCaseId).
   DO i = 1 TO NUM-ENTRIES(lcDocList, {&DMS_DOCLIST_SEP}) BY 3:
      lcDocNotifEntry = fDoc2Msg(ENTRY(i,lcDocList,{&DMS_DOCLIST_SEP}),
                                 ENTRY(i + 1,lcDocList,{&DMS_DOCLIST_SEP}),
                                 ENTRY(i + 2,lcDocList,{&DMS_DOCLIST_SEP})).
      fObjectToJsonArray(lcArray, lcDocNotifEntry).
   END.
   
   /*Fill data for message.*/
   lcMessage = "~{" + "~"metadata~""  + "~:" + "~{" +
                         "~"version~""  + "~:" + "~"" + lcVersion + "~"," +
                         "~"case~""  + "~:" + "~"" + icNotifCaseID  + "~"," +
                         "~"smsseq~""  + "~:" + "~"" + lcSeq  + "~"," +
                         "~"recipient_email~""  + "~:" + "~"" + 
                            lcRecEmail  + "~"," +
                         "~"recipient_msisdn~""  + "~:" + "~"" + 
                            lcRecMSISDN  + "~"" +
                     "~}" + "," +
                      "~"data~"" + "~:" + "~{" +
                         lcArray + "," +
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
                         "~"bank_name~"" +  "~:" + "~"" +
                                      lcBankName + "~"" + "," +
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
      WHEN "Yanai" THEN DO:
         lcConfFile = "Mailconf/dms_messaging_conf.yanai".
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
              Order.Brand EQ Syst.Var:gcBrand AND
              Order.OrderId EQ icOrderID NO-ERROR.
   IF NOT AVAIL Order THEN RETURN "DMS Notif: No Order available".

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand EQ Syst.Var:gcBrand AND
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
                                BUFFER Order,
                                BUFFER Ordercustomer).

   ocSentMessage = "Case param: " + lcParam + "Msg: " + lcMessage.              
   lcMQ =  fCParamNotNull("DMS","DMS_MQ"). 
   lcConfig = fDMSConfig().
   IF lcConfig EQ "" THEN RETURN "MQ config not available".
   RETURN fSendToMQ(lcMessage, lcMQ, lcConfig, icModule, FALSE).
END.


