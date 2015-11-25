/* dms.i         03.09.15/ivekov 

*/

{commali.i}
{tmsparam4.i}
{timestamp.i}
{replog_reader.i}
{cparam2.i}

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
   ELSE DMS.OrderStatus = icOrderstatus.

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
   RUN pInitialize(INPUT "dms").

   IF RETURN-VALUE > "" THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE(RETURN-VALUE, "ERROR").

         RETURN RETURN-VALUE.
   END.

   /* Call ActiveMQ Publisher class */
   lMsgPublisher = NEW Gwy.MqPublisher(lcHost,liPort,
                                       liTimeOut,"angela_in",
                                       lcUserName,lcPassword).

   IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
         LOG-MANAGER:WRITE-MESSAGE("ActiveMQ Publisher handle not found",
                                    "ERROR").

   END.

   IF NOT lMsgPublisher:send_message(icMsg) THEN
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
         LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
   

   RUN pFinalize(INPUT "").


END.


/*Function sends SMS and EMAIL generating information to WEB if it is needed*/
FUNCTION fSendChangeInformation RETURNS CHAR
   (icStatus AS CHAR,
    icOrderID AS INT):

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
   DEF VAR lcDeposit AS CHAR NO-UNDO.
   DEF VAR lcBankAcc AS CHAR NO-UNDO.

   /*Read Parameter that defines case ID*/
   lcParam = "DMSMsgID_" + icStatus. /*DMSMsgIF_E -> returns 03 as specified.*/
   lcNotifCaseID = fCParam("DMS",lcParam).

   IF lcNotifCaseID EQ "" THEN RETURN "". /*No actions for the case*/

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



   lcMSISDN = fNotNull(OrderCustomer.ContactNum).
   lcContractID = fNotNull(Order.ContractId).
   lcDNIType = fNotNull(OrderCustomer.CustIdType).
   lcDNI = fNotNull(OrderCustomer.CustId).
   lcFname = fNotNull(OrderCustomer.FirstName).
   lcLname = fNotNull(OrderCustomer.SurName1) +
             fNotNull(Ordercustomer.SurName2).
   lcEmail = fNotNull(OrderCustomer.Email).
   lcDeposit = fNotNull("MISSING").
   lcBankAcc = fNotNull(OrderCustomer.BankCode).


   /*Fill data for message.*/
   lcMessage = "~{" + "~"metadata~""  + "~:" + "~"" + "~{" +
                         "~"case~""  + "~:" + "~"" + lcNotifCaseID  + "~"" +
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
                                      lcDeposit + "~"" + "," +
                         "~"bank_account_number~"" +  "~:" + "~"" +
                                      lcBankAcc + "~"" +
                      "~}" +
                 "~}".
   RETURN fSendToMQ(lcMessage).
END.


