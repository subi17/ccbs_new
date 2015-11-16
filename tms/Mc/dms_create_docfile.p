/*--------------------------------------------------------------------
  MODULE .......: dms_create_docfile.p
  TASK .........: Create DMS file for requested case types.
                  Data is fetched by ordertimestamp and msrequest 
                  timestamps. 
                  Data is stored to given file.
  APPLICATION ..: tms
  AUTHOR .......: ilkkasav
  CREATED ......: 01.09.15
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{tmsconst.i}
{timestamp.i}
{offer.i}
{dms.i}

DEF INPUT PARAMETER icCases AS CHAR. /*List of reported cases*/
DEF INPUT PARAMETER idPeriodStart AS DEC. /*reporting period strat*/
DEF INPUT PARAMETER idPeriodEnd AS DEC. /*reporting period end*/
DEF INPUT PARAMETER icOutFile AS CHAR. /*Case file with path*/
DEF INPUT PARAMETER icLogFile AS CHAR. /*Log file with path*/

DEF STREAM sLogFile.
DEF STREAM sOutFile.

DEF VAR lcStatus        AS CHAR NO-UNDO.
DEF VAR ldCurrentTime   AS DEC  NO-UNDO. 
DEF VAR lcDelim         AS CHAR NO-UNDO.
DEF VAR liCaseCount     AS INT  NO-UNDO.
DEF VAR lcInitStatus    AS CHAR NO-UNDO.
DEF VAR lcDMSStatusDesc AS CHAR NO-UNDO.
DEF VAR lcDocStatus     AS CHAR NO-UNDO.
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO. 
lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

DEF TEMP-TABLE ttOrderList NO-UNDO
   FIELD OrderID AS INT
   FIELD CaseID AS CHAR
   FIELD Direct AS LOGICAL
INDEX OrderID OrderID
INDEX CaseID CaseID
INDEX Direct Direct.

ASSIGN
   lcInitStatus    = "SENT"
   lcDMSStatusDesc = "SENT"
   lcDocStatus     = "A"
   lcDelim         = "|".

/*Functions:*/

FUNCTION fLogLine RETURNS LOGICAL
   (icLine AS CHAR,
   icMessage AS CHAR):
   PUT STREAM sLogFile UNFORMATTED
      icLine "#"
      icMessage "#"
      "TMS" SKIP.
END FUNCTION.

/*Decide that what kind of data must be collected (Order or Msrequest data)*/
/*Function also defines category for handling.*/
FUNCTION fMakeTempTable RETURNS CHAR
   (icCaseList AS CHAR,
    idStartTS AS DECIMAL,
    idEndTS AS DECIMAL
   ):
   DEF VAR lcCase AS CHAR NO-UNDO.
   DEF VAR llgDirect AS LOG NO-UNDO. /*Direct chanel order that is sent to BS*/
   DEF VAR llgAddEntry AS LOG NO-UNDO.
   DEF VAR llgOrderSeek AS LOG NO-UNDO.
   DEF VAR llgDirectNeeded AS LOG NO-UNDO.
   DEF VAR ldeInstallment AS DECIMAL NO-UNDO.
   DEF VAR ldeMonthlyFee  AS DECIMAL NO-UNDO.
   DEF VAR liMonths AS INT NO-UNDO.
   DEF VAR ldeFinalFee AS DECIMAL NO-UNDO.
   DEF VAR liCount AS INT NO-UNDO.
   DEF VAR liAddId AS Int NO-UNDO.
   DEF VAR liRT AS Int NO-UNDO.
   DEF VAR liStampTypeCount AS INT NO-UNDO.
   DEF VAR lcStampTypes AS CHAR NO-UNDO.

   llgOrderSeek = FALSE.
   llgDirectNeeded = FALSE.
   DO liCount = 1 TO NUM-ENTRIES(icCaseList):
      CASE ENTRY(liCount,icCaseList):
         WHEN {&DMS_CASE_TYPE_ID_ORDER_ACT}      THEN llgOrderSeek = TRUE.
         WHEN {&DMS_CASE_TYPE_ID_ORDER_RESTUDY}  THEN llgOrderSeek = TRUE.
         WHEN {&DMS_CASE_TYPE_ID_COMPANY}        THEN llgOrderSeek = TRUE.
         WHEN {&DMS_CASE_TYPE_ID_DIRECT_CH}      THEN DO:
            llgOrderSeek = TRUE.
            llgDirectNeeded = TRUE.
         END.
         WHEN {&DMS_CASE_TYPE_ID_CANCEL}         THEN llgOrderSeek = TRUE.
     END.
   END.
   IF llgOrderSeek EQ TRUE THEN DO:
      lcStampTypes = STRING({&ORDERTIMESTAMP_DELIVERY}) + "," +
                     STRING({&ORDERTIMESTAMP_SEND}) + "," +
                     STRING({&ORDERTIMESTAMP_CHANGE}).
      DO liStamptypeCount = 1 TO NUM-ENTRIES (lcStampTypes):
         liRT = INT(ENTRY(liStampTypeCount,lcStampTypes)).

         FOR EACH OrderTimestamp NO-LOCK WHERE
                  OrderTimestamp.Brand EQ gcBrand AND
                  OrderTimestamp.RowType EQ liRt AND
                  Ordertimestamp.TimeStamp < idEndTS AND
                  Ordertimestamp.TimeStamp >= idStartTS:
            FIND FIRST ttOrderList WHERE
                       ttOrderList.OrderID EQ OrderTimestamp.OrderId NO-ERROR.
            IF AVAIL ttOrderList THEN NEXT.
            FIND FIRST Order WHERE
                       Order.Brand EQ gcBrand AND
                       Order.OrderID EQ OrderTimestamp.OrderId NO-ERROR.
            IF NOT AVAIL Order THEN NEXT.
            /*Default values for new loop*/
            llgDirect = FALSE.
            llgAddEntry = FALSE.
            /*Case 5: Direct channels*/
            /*This can NOT be parallell with other cases.*/
            /*Reason to store llgDirect information is that the case is easy
              to be changed parallel with other cases*/
            IF llgDirectNeeded EQ TRUE AND
               Order.Logistics NE "" AND
              NOT (Order.StatusCode EQ {&ORDER_STATUS_CLOSED} OR
                   Order.StatusCode EQ {&ORDER_STATUS_CLOSED_BY_FRAUD} OR
                   Order.StatusCode EQ {&ORDER_STATUS_AUTO_CLOSED}  )
               AND R-INDEX(Order.OrderChannel, "pos") EQ 0  THEN DO: /*NO POS*/
               ldeInstallment = fGetOfferDeferredPayment(Order.Offer,
                                              Order.CrStamp,
                                              OUTPUT ldeMonthlyFee,
                                              OUTPUT liMonths,
                                              OUTPUT ldeFinalFee).

                 /* This is financed case */
                  IF liMonths NE 0 THEN DO:
                     llgDirect = TRUE.
                     CREATE ttOrderList.
                     ASSIGN ttOrderList.OrderID = OrderTimestamp.OrderId
                            ttOrderList.CaseID = lcCase
                            ttOrderList.Direct = llgDirect.
                     lcCase = {&DMS_CASE_TYPE_ID_DIRECT_CH}.
                     NEXT.  /*no need to check other cases because they                                           can not be parallel according to current specs.*/
                  END.
            END.
            /*Case 1: Activations*/
            IF (Order.StatusCode EQ {&ORDER_STATUS_DELIVERED} /*6*/
               OR
               Order.StatusCode EQ {&ORDER_STATUS_RENEWAL_STC} /*32*/) AND
               R-INDEX(Order.OrderChannel, "pos"  ) > 0 /* POS needed*/
               /*Only POS  orders*/
               THEN DO:
                  lcCase = {&DMS_CASE_TYPE_ID_ORDER_ACT}.
                  llgAddEntry = TRUE.
               END.
            /*Case 2: More doc needed*/
            ELSE IF Order.StatusCode EQ
                    {&ORDER_STATUS_MORE_DOC_NEEDED} /*44*/
               THEN DO:
                  lcCase = {&DMS_CASE_TYPE_ID_ORDER_RESTUDY}.
                  llgAddEntry = TRUE.
               END.
            /*Case 3: Companies*/
            ELSE IF Order.StatusCode EQ
                    {&ORDER_STATUS_COMPANY_NEW} OR  /*20*/
                    Order.StatusCode EQ
                    {&ORDER_STATUS_COMPANY_MNP} OR  /*21*/
                    Order.StatusCode EQ
                     {&ORDER_STATUS_RENEWAL_STC_COMPANY}  /*33*/
               THEN DO:
                  lcCase = {&DMS_CASE_TYPE_ID_COMPANY}.
                  llgAddEntry = TRUE.
               END.
            /*Case 4 - not related to order table*/
            /*Csse 6: Cancellations*/
            ELSE IF Order.StatusCode EQ {&ORDER_STATUS_CLOSED} OR
                    Order.StatusCode EQ {&ORDER_STATUS_CLOSED_BY_FRAUD} OR
                    Order.StatusCode EQ {&ORDER_STATUS_AUTO_CLOSED} THEN DO:
               /*Send Cancel notif if TMS has sent other notif to DMS
                (previous sending)*/
               FIND FIRST DMS NO-LOCK WHERE
                          DMS.HostTable EQ {&DMS_HOST_TABLE_ORDER} AND
                          DMS.HostId EQ Order.OrderID NO-ERROR.
               IF AVAIL DMS THEN DO:
                  lcCase = {&DMS_CASE_TYPE_ID_CANCEL}.
                  llgAddEntry = TRUE.
               END.
            END.
            /*Other cases, no need to create entry*/
            ELSE NEXT.

            IF llgAddEntry EQ TRUE THEN DO TRANS:
               CREATE ttOrderList.
               ASSIGN ttOrderList.OrderID = OrderTimestamp.OrderId
                      ttOrderList.CaseID = lcCase.
                      ttOrderList.Direct = llgDirect.
            END.
         END. /*ordertimestamp*/
      END.
   END. /**/
      /*If order has already gone to DELIVERED 6, the order status
     will not return to 7,8,9. This cases need to be seeked from
     requests.  order mobsub
     If activation information is not sent yet to DMS (found in this exec round)
     cancellation is not allowed to be sent -> need to erase existing entry.*/
  FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand EQ gcBrand AND
            MsRequest.ReqStatus EQ 2 AND
            MsRequest.UpdateStamp > idStartTS AND
            MsRequest.UpdateStamp < idEndTS AND
            (
            MsRequest.ReqType EQ
              {&REQTYPE_SUBSCRIPTION_TERMINATION} /*18*/
            OR MsRequest.ReqType EQ
              {&REQTYPE_REVERT_RENEWAL_ORDER} /*49*/
            ) AND
           MsRequest.UpdateStamp <= MsRequest.DoneStamp :
      
      IF MsRequest.ReqType EQ {&REQTYPE_REVERT_RENEWAL_ORDER} THEN DO:
         FIND FIRST ttOrderlist WHERE
                    ttOrderlist.OrderId EQ MsRequest.ReqIparam1 NO-ERROR.
         IF AVAIL ttOrderList THEN DO:
            DELETE ttOrderList.
            NEXT.
         END.
         ELSE DO:
            liAddId = MsRequest.ReqIparam1.
            lcCase = {&DMS_CASE_TYPE_ID_CANCEL}.
         END.
      END.
      ELSE IF  MsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
               MsRequest.ReqCparam3 EQ "11" THEN DO:

         FIND FIRST Order NO-LOCK WHERE
                    Order.Msseq EQ MsRequest.MsSeq AND
                    Order.OrderType < 2 AND /*allowed only for new and MNP */
                    Order.StatusCode EQ "6" NO-ERROR.
         IF AVAIL Order THEN DO:
            FIND FIRST ttOrderlist WHERE
                      ttOrderlist.OrderId EQ Order.OrderId NO-ERROR.
            IF AVAIL ttOrderList THEN DO:
               DELETE ttOrderList.
               NEXT.
            END.
            ELSE DO:
               liAddId = Order.OrderId.
               lcCase = {&DMS_CASE_TYPE_ID_CANCEL}.
            END.
         END.
         ELSE NEXT.
      END.
      ELSE NEXT.

      /*Sending is allowed only if there is previous DMS entry for
        the change. */
      FIND FIRST DMS NO-LOCK WHERE
                 DMS.HostTable EQ {&DMS_HOST_TABLE_ORDER} AND
                 DMS.HostID EQ liAddId NO-ERROR.

      IF AVAIL DMS THEN DO TRANS:
         CREATE ttOrderList.
         ASSIGN ttOrderList.OrderID = liAddId
                ttOrderList.CaseID = lcCase.
      END.
   END. /*Msrequest search*/

END.





FUNCTION fPrintDate RETURNS CHAR
   (idDate AS DECIMAL):
   DEF VAR lcTmp AS CHAR NO-UNDO.

   lcTmp = STRING(idDate).
   RETURN SUBSTR(lcTmp,7,2) + "-" + 
          SUBSTR(lcTmp,5,2) + "-" + 
          SUBSTR(lcTmp,1,4). 
END.   

FUNCTION fPrintDateD RETURNS CHAR
   (idaDate AS DATE):
   IF idaDate EQ ? THEN RETURN "-".
   RETURN STRING(idaDate, "99-99-9999").
END.

FUNCTION fConvertPayType RETURNS CHAR
   (ilgValue AS LOGICAL):
   IF ilgValue EQ TRUE THEN RETURN "TARJ".
   ELSE RETURN "CONT".
END.   

FUNCTION fGetSegment RETURNS CHAR
   (iiOrderID AS INT):
   FIND FIRST OrderCustomer NO-LOCK  WHERE
              OrderCustomer.Brand EQ gcBrand AND
              OrderCustomer.OrderID EQ iiOrderID AND
              Ordercustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
              NO-ERROR.
   IF AVAIL OrderCustomer THEN DO:
      IF OrderCustomer.CustIdType EQ "CIF" THEN RETURN "Company".
      ELSE IF OrderCustomer.SelfEmployed EQ TRUE THEN RETURN "Self-employed".
      ELSE RETURN "Consumer".
   END.
   ELSE RETURN "-".
END.   

/*The value can be Simonly, Handset, Financed Handset.*/
FUNCTION fGetTerminalFinanceType RETURNS CHAR
   (iiOrderID AS INT):
   DEF BUFFER bOrder FOR Order.
   DEF VAR ldeInstallment AS DECIMAL NO-UNDO.
   DEF VAR ldeMonthlyFee  AS DECIMAL NO-UNDO.
   DEF VAR liMonths AS INT NO-UNDO.
   DEF VAR ldeFinalFee AS DECIMAL NO-UNDO.

   FIND FIRST bOrder NO-LOCK WHERE
              bOrder.Brand EQ gcBrand AND
              bOrder.OrderId EQ iiOrderId NO-ERROR.
   IF NOT AVAIL bOrder THEN RETURN "".

   FIND FIRST OrderAccessory NO-LOCK  WHERE
              OrderAccessory.Brand EQ gcBrand AND
              OrderAccessory.OrderID EQ iiOrderID AND
              Orderaccessory.TerminalType EQ {&TERMINAL_TYPE_PHONE} NO-ERROR.
   IF AVAIL OrderAccessory THEN DO:

      ldeInstallment = fGetOfferDeferredPayment(bOrder.Offer,
                                                bOrder.CrStamp,
                                                OUTPUT ldeMonthlyFee,
                                                OUTPUT liMonths,
                                                OUTPUT ldeFinalFee).
      IF liMonths EQ 0 THEN RETURN "Handset".
      ELSE RETURN "Financed Handset".
   END.
   RETURN "Simonly".
END.


FUNCTION fGetHandset RETURNS CHAR
   (iiOrderId AS INT,
   icImei AS CHAR):
   DEF BUFFER bOAcc FOR OrderAccessory.
   FIND FIRST bOAcc NO-LOCK WHERE
              bOAcc.Brand EQ gcBrand AND
              bOAcc.OrderId EQ iiOrderId AND
              bOAcc.Imei EQ icImei NO-ERROR.
   IF AVAIL bOAcc THEN 
      RETURN STRING(bOAcc.Manufacturer) + " " +
             STRING(bOAcc.Model)        + " " +
             STRING(bOAcc.ModelColor).
   RETURN "".             
END.   

FUNCTION fGetCancellationInfo RETURNS CHAR
   (iiMsSeq AS INT,
    icStatus AS CHAR,
    idStartTS AS DECIMAL,
    idEndTS AS DECIMAL,
   OUTPUT odeTime AS DECIMAL):
   odeTime = idEndTS.

   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.Brand EQ gcBrand AND
              MsRequest.ReqStatus EQ 2 AND
              MsRequest.UpdateStamp > idStartTS AND
              MsRequest.UpdateStamp < idEndTS AND
              (
              MsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TERMINATION} /*18*/
              OR MsRequest.ReqType EQ {&REQTYPE_REVERT_RENEWAL_ORDER} /*49*/
              )
              AND Msrequest.MsSeq EQ iiMsSeq AND
              MsRequest.UpdateStamp <= MsRequest.DoneStamp NO-ERROR.
   
   IF AVAIL MsRequest THEN DO:
      IF MsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
         MsRequest.ReqCparam3 EQ "11" THEN DO:
         odeTime = MsRequest.CreStamp.
         RETURN "POS Order Cancellation".
      END.
      ELSE IF MsRequest.ReqType EQ {&REQTYPE_REVERT_RENEWAL_ORDER} THEN DO:
         odeTime = MsRequest.CreStamp.
         RETURN "Order Cancellation".
      END.
   END.
   ELSE IF icStatus EQ {&ORDER_STATUS_MORE_DOC_NEEDED} OR
      icStatus EQ {&ORDER_STATUS_COMPANY_NEW} OR
      icStatus EQ {&ORDER_STATUS_COMPANY_MNP } OR
      icStatus EQ {&ORDER_STATUS_RENEWAL_STC_COMPANY} THEN DO:
      RETURN "User Cancellation".
   END.
   RETURN "".
END.

FUNCTION fGetTerminalData RETURNS CHAR
   (iiOrderId AS INT):

   FIND FIRST OrderAccessory NO-LOCK  WHERE
              OrderAccessory.Brand EQ gcBrand AND
              OrderAccessory.OrderID EQ iiOrderID AND
              Orderaccessory.TerminalType EQ {&TERMINAL_TYPE_PHONE} NO-ERROR.
   IF AVAIL OrderAccessory THEN DO:
      FIND FIRST Billitem NO-LOCK WHERE
                 BillItem.Brand   = gcBrand AND
                 BillItem.BillCode = OrderAccessory.ProductCode 
                 NO-ERROR.
      IF AVAILABLE BillItem THEN RETURN BillItem.BIName.
      /* should not be possible, return just in case */
      ELSE RETURN STRING(OrderAccessory.Manufacturer) + " " +
                  STRING(OrderAccessory.Model)        + " " +
                  STRING(OrderAccessory.ModelColor).
   END.
   
   RETURN "".

END.


/* Previous tariff: In case of Portability this value can be TARJ 
   for prepaid and CONT for postpaid. In case of STC+RENEWAL and 
   Fusion STC Previous tariff should contain original tariff before STC happens. 
   In all other cases this field would be blank */
/* 0 for New adds, 1 for portability, 2 for Renewal and 4 for Fusion STC */
/**/
FUNCTION fGetPrevTariff RETURNS CHAR
   (BUFFER Order FOR Order):

   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER Mobsub FOR Mobsub.
   DEF BUFFER MsOwner FOR MsOwner.

   IF Order.OrderType EQ {&ORDER_TYPE_MNP} /*1 Portability*/ THEN DO:
      /*pre=true pos=false*/
      IF Order.OldPayType EQ TRUE THEN RETURN "TARJ".
      ELSE RETURN "CONT".
   END.
   ELSE IF Order.OrderType EQ {&ORDER_TYPE_STC} /*4 Fusion STC*/ OR
           (Order.OrderType EQ {&ORDER_TYPE_RENEWAL} AND
            INDEX(Order.OrderChannel,"stc") > 0) /*2 Renewal */ THEN DO:

      FIND FIRST msrequest NO-LOCK where
                 msrequest.msseq  = order.msseq and
                 msrequest.reqtype = 0 and
                 msrequest.reqiparam2 = order.orderid no-error.

      IF AVAIL msrequest AND
         LOOKUP(msrequest.reqcparam1, lcBundleCLITypes) = 0 THEN
         RETURN msrequest.reqcparam1.
      ELSE IF NOT AVAIL msrequest or msrequest.reqstatus NE 2 then do:

          FIND FIRST Mobsub NO-LOCK where
                     Mobsub.MsSeq = order.msseq no-error.
          if not avail Mobsub then return "".

          if Mobsub.tariffbundle > "" THEN
            return Mobsub.tariffbundle.
          ELSE return Mobsub.clitype.

      end.
      else do:

         /* TODO: not fool proof check */
         /* 100% sure solution: add */
         FIND FIRST MsOwner NO-LOCK WHERE
                    Msowner.Brand = gcBrand AND
                    MsOwner.CLI   = Order.CLI AND
                    MsOwner.TsEnd < msrequest.donestamp AND 
                    MsOwner.CLIType = msrequest.reqcparam1 AND
                    MsOwner.MsSeq = msrequest.msseq
              USE-INDEX CLI NO-ERROR.

         IF AVAILABLE MSOwner THEN RETURN 
            (IF MSOwner.tariffbundle > "" THEN 
                MSOwner.tariffbundle ELSE MsOwner.CLIType).


      end.
   END. 
   RETURN "".   
   
END.   


FUNCTION fCountIMEIModifications RETURN CHAR
   (iiMsSeq AS INT):
   DEF BUFFER bMsrequest FOR MsRequest.
   DEF VAR liCount AS INT NO-UNDO.
   FOR EACH bMsRequest NO-LOCK WHERE
            bMsRequest.MsSeq EQ iiMsSeq AND
            bMsRequest.ReqType EQ {&REQTYPE_IMEI_CHANGE} AND
            bMsRequest.ReqStatus EQ 2 AND
            bMsRequest.ReqCparam6 NE "" AND
            bMsRequest.UpdateStamp <= bMsRequest.DoneStamp :
      liCount = liCount + 1.
   END.
   RETURN STRING(liCount).
END.

/*Order activation*/
/*Function generates order documentation*/
FUNCTION fCreateDocumentCase1 RETURNS CHAR
   (iiOrderId AS INT):
   DEF VAR lcCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcContractID    AS CHAR NO-UNDO.
   DEF VAR lcStatusCode    AS CHAR NO-UNDO.
   DEF VAR lcDocList       AS CHAR NO-UNDO.
   DEF VAR lcCreateDMS     AS CHAR NO-UNDO.
   DEF VAR lcDocListEntries AS CHAR NO-UNDO.
   DEF VAR lcCasefileRow   AS CHAR NO-UNDO.

   lcCaseTypeId = "1".

   FIND FIRST Order NO-LOCK WHERE 
              Order.Brand EQ gcBrand AND
              Order.OrderID EQ iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN
      RETURN "1:Order not available" + STRING(iiOrderId).

   lcCaseFileRow =  
   lcCaseTypeID                    + lcDelim + 
   /*Order_OD*/
   STRING(Order.OrderID)           + lcDelim + 
   /*Contract_ID*/
   STRING(Order.ContractID)        + lcDelim +
   /*SFID*/
   STRING(Order.Salesman)          + lcDelim +
   /*MSISDN*/
   STRING(Order.CLI)               + lcDelim +
   /*Order date*/
   fPrintDate(Order.CrStamp)       + lcDelim +
   /*Status*/
   STRING(Order.StatusCode)        + lcDelim +
   /*Channel*/
   STRING(Order.OrderChannel)      + lcDelim +
   /*Tariff type*/
   STRING(Order.CLIType)           + lcDelim +
   /*Previous tariff*/
   fGetPrevTariff(BUFFER Order) + lcDelim +
   /**/
   STRING(Order.OrderType)          + lcDelim +
   /**/
   fGetSegment(Order.OrderID)       + lcDelim +
   /*Terminal Type: The value can be Simonly, Handset, Financed Handset.*/
   fGetTerminalFinanceType(iiOrderId).

   /*Document type,DocStatusCode,RevisionComment*/
   lcDocListEntries = "".

   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
   OUTPUT STREAM sOutFile CLOSE.
   fLogLine(lcCaseFileRow,"").
   lcCreateDMS = fUpdateDMS("", /*DmsExternalID*/
                            lcCaseTypeID,
                            Order.ContractID,
                            {&DMS_HOST_TABLE_ORDER},
                            Order.OrderId,
                            lcInitStatus,/*StatusCode*/
                            lcDMSStatusDesc,
                            Order.StatusCode,
                            0,
                            lcDocListEntries /*DocList*/,
                            ",").
END.   

/*Order restudy ORDER_STATUS_MORE_DOC_NEEDED "44" */
FUNCTION fCreateDocumentCase2 RETURNS CHAR
   (iiOrderId AS INT):
   DEF VAR lcCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcDeliveryAddress AS CHAR NO-UNDO.
   DEF VAR lcDeliveryZip AS CHAR NO-UNDO.
   DEF VAR lcDeliveryPost AS CHAR NO-UNDO.
   DEF VAR ldeInstallment AS DECIMAL NO-UNDO.
   DEF VAR ldeMonthlyFee  AS DECIMAL NO-UNDO.
   DEF VAR liMonths AS INT NO-UNDO.
   DEF VAR ldeFinalFee AS DECIMAL NO-UNDO.
   DEF VAR lcPAram AS CHAR NO-UNDO.
   DEF VAR liCount AS INT NO-UNDO.
   DEF VAR lcDocListEntries AS CHAR NO-UNDO.
   DEF VAR lcCreateDMS     AS CHAR NO-UNDO.
   DEF VAR lcRequiredDocs AS CHAR NO-UNDO.
   DEF VAR lcCasefileRow   AS CHAR NO-UNDO.
   DEF VAR lcModel   AS CHAR NO-UNDO.
   DEF VAR ldePermanencyAmount AS DECIMAL.
   DEF VAR liPermancyLength AS INT.

   DEF BUFFER DeliveryCustomer FOR OrderCustomer.

   ASSIGN
      lcCaseTypeId      = "2"
      lcModel           = "-".

   FIND FIRST Order NO-LOCK WHERE 
              Order.Brand = gcBrand  AND
              Order.OrderID EQ iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN RETURN "2:Order not available" + STRING(iiOrderId).

   FIND FIRST OrderCustomer NO-LOCK  WHERE
              OrderCustomer.Brand EQ gcBrand AND  
              OrderCustomer.OrderID EQ iiOrderID AND
              OrderCustomer.RowType EQ 1 NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN 
      RETURN "2:Ordercustomer not available" + STRING(iiOrderId).

   /*Get delivery address if it is available*/
   FIND FIRST DeliveryCustomer NO-LOCK  WHERE
              DeliveryCustomer.Brand EQ gcBrand AND
              DeliveryCustomer.OrderID EQ iiOrderID AND
              DeliveryCustomer.RowType EQ 4 NO-ERROR.
   IF AVAIL DeliveryCustomer THEN DO:
      ASSIGN
         lcDeliveryAddress = DeliveryCustomer.Address
         lcDeliveryZIP = DeliveryCustomer.ZipCode
         lcDeliveryPost = DeliveryCustomer.PostOffice.
   END.
   ELSE DO:
      ASSIGN
         lcDeliveryAddress = OrderCustomer.Address
         lcDeliveryZip = OrderCustomer.ZipCode
         lcDeliveryPost = OrderCustomer.PostOffice.
   END.
   
   lcModel = fGetTerminalData(iiOrderId).

   ldeInstallment = fGetOfferDeferredPayment(Order.Offer,
                                              Order.CrStamp,
                                              OUTPUT ldeMonthlyFee,
                                              OUTPUT liMonths,
                                              OUTPUT ldeFinalFee).
   RUN offer_penaltyfee.p(Order.OrderID,
                          OUTPUT liPermancyLength,
                          OUTPUT ldePermanencyAmount).
   lcCaseFileRow =
   lcCaseTypeId                   + lcDelim +
   /*Order_ID: 14547132*/
   STRING(Order.OrderID)          + lcDelim +
   /*Contract_ID: EB0B71*/
   STRING(Order.ContractID)       + lcDelim +
   /*SFID: WEB*/
   STRING(Order.Salesman)         + lcDelim +
   /*MSISDN: 637165990*/
   STRING(Order.CLI)              + lcDelim +
   /*Order_date: 20-04-2015*/
   fPrintDate(Order.CrStamp)      + lcDelim +
   /*Order Channel: self*/
   STRING(Order.OrderChannel)     + lcDelim +
   /*Status_Code :  44*/
   STRING(Order.StatusCode)       + lcDelim +
   /*Full Name: Jose Carlos Prieto Perez*/
   OrderCustomer.Firstname + " " +
   OrderCustomer.Surname1 + " " +
   OrderCustomer.Surname2         + lcDelim +

   /*Date of Birth: 23-10-1983*/
   fPrintDateD(OrderCustomer.Birthday) + lcDelim +
   /*Nationality: España*/
   STRING(OrderCustomer.Nationality) + lcDelim +
   /*Doc ID Type: NIF (ES Personal ID) NIE=foreign id (CIF for company)*/
   /*fGetIdType(Ordercustomer.Nationality) + lcDelim +*/
   STRING(OrderCustomer.CustIDType) + lcDelim +
   /*Doc ID: 53233826G*/
   STRING(OrderCustomer.CustID) + lcDelim +
   /*Tariff Type: CONT9*/
   STRING(Order.CLIType)          + lcDelim +
   /*Previous Tariff: CONT*/
   fGetPrevTariff(BUFFER Order) + lcDelim +
   /*Donor Operator: Vodafone*/
   STRING(Order.CurrOper)          + lcDelim +
   /*Bank Account:  ES4800811342630006235932*/
   STRING(OrderCustomer.BankCode)  + lcDelim +
   /*Address: CALLE DOCTOR CLARAMUNT 3 2-D*/
   STRING(OrderCustomer.Address)   + lcDelim +
   /*Zipcode: 03011*/
   STRING(OrderCustomer.ZipCode)   + lcDelim +
   /*Postal Office:  ALICANTE*/
   STRING(OrderCustomer.PostOffice)  + lcDelim +
   /*Delivery Address: CALLE DOCTOR CLARAMUNT 3 2-D*/
   lcDeliveryAddress                 + lcDelim +
   /*Delivery Zipcode: 03011*/
   lcDeliveryZIP                     + lcDelim +
   /*Delivery Postal Office:  ALICANTE*/
   lcDeliveryPost                    + lcDelim +
   /*Email:morfeonuncaduerme@gmail.com*/
   STRING(OrderCustomer.Email)  + lcDelim +
   /*Mobile number ( contact number)*/
   STRING(OrderCustomer.MobileNumber) + lcDelim +
   /*Total Installment: 528*/
   STRING(ldeInstallment)            + lcDelim +
  /*Residual value: 70*/
   STRING(ldeFinalFee)               + lcDelim +
  /*Permanency: 50*/
   STRING(ldePermanencyAmount)       + lcDelim + 
   /*Model Type: Sony Xperia Z3 Blanco*/
   STRING(lcModel)                   + lcDelim +   
   /*ROI Risk: R11A;C--;S.;RSVODAFONE::02/11/2009::383::1;Z03011*/
   STRING(Order.RiskCode).
   
   /*Solve tmsparam value for getting correct matrix row*/
   /*portability pos-pos*/
   IF Order.OrderType EQ {&ORDER_TYPE_MNP} AND 
      Order.PayType EQ FALSE AND
      Order.OldPayType EQ FALSE  THEN lcParam = "DMS_S44_T1".
   /*new add pos / portability pre-pos.*/
   ELSE IF (Order.OrderType EQ {&ORDER_TYPE_NEW} AND 
            Order.PayType EQ FALSE )
      OR
           (Order.OrderType EQ {&ORDER_TYPE_MNP} AND
           Order.PayType EQ FALSE AND
           Order.OldPayType EQ TRUE ) THEN lcParam = "DMS_S44_T2".
   /*stc to pos / migration+renewal*/
   ELSE IF Order.OrderType EQ {&ORDER_TYPE_STC} AND
           Order.PayType EQ FALSE
      OR   
           Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN lcPAram = "DMS_S44_T3".
   /*add new pre / portability to pre*/
   ELSE IF Order.OrderType EQ {&ORDER_TYPE_NEW} AND
           Order.PayType EQ TRUE
      OR
           Order.OrderType EQ {&ORDER_TYPE_MNP} AND
           Order.PayType EQ TRUE THEN lcPAram = "DMS_S44_T4".

   lcRequiredDocs = fCParam("DMS",lcParam).
   DO liCount = 1 TO NUM-ENTRIES(lcRequiredDocs):
      /*Document type,DocStatusCode,RevisionComment*/
      lcDocListEntries = ENTRY(liCount,lcRequiredDocs) + "," + 
                         "," + 
                         lcDMSStatusDesc + "," + 
                         "Doc created".
      IF liCount NE NUM-ENTRIES(lcRequiredDocs) 
         THEN lcDocListEntries = lcDocListEntries + ",".
   END.

   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
   OUTPUT STREAM sOutFile CLOSE.

   fLogLine(lcCaseFileRow,"").
   lcCreateDMS = fUpdateDMS("", /*DmsExternalID*/
                            lcCaseTypeID,
                            Order.ContractID,
                            {&DMS_HOST_TABLE_ORDER},
                            Order.OrderId,
                            lcInitStatus,/*StatusCode*/
                            lcDMSStatusDesc,
                            Order.StatusCode,
                            0,
                            lcDocListEntries /*DocList*/,
                            ",").



   RETURN "".

END.   
/*{3} Companies*/
FUNCTION fCreateDocumentCase3 RETURNS CHAR
   (iiOrderId AS INT):
   DEF VAR lcCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcName AS CHAR No-UNDO.
   DEF VAR lcCreateDMS     AS CHAR NO-UNDO.
   DEF VAR ldeInstallment AS DECIMAL NO-UNDO.
   DEF VAR ldeMonthlyFee  AS DECIMAL NO-UNDO.
   DEF VAR liMonths AS INT NO-UNDO.
   DEF VAR ldeFinalFee AS DECIMAL NO-UNDO.
   DEF VAR lcPAram AS CHAR NO-UNDO.
   DEF VAR lcRequiredDocs AS CHAR NO-UNDO.
   DEF VAR lcDocListEntries AS CHAR NO-UNDO.
   DEF VAR lcCasefileRow   AS CHAR NO-UNDO.
   DEF VAR liCount AS INT NO-UNDO.
   DEF VAR lcModel   AS CHAR NO-UNDO.
   DEF VAR ldePermanencyAmount AS DECIMAL.
   DEF VAR liPermancyLength AS INT.

   ASSIGN
      lcCaseTypeId      = "3".

   FIND FIRST Order NO-LOCK WHERE 
              Order.Brand = gcBrand  AND
              Order.OrderID EQ iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN RETURN "3:Order not available" + STRING(iiOrderId).

   FIND FIRST OrderCustomer NO-LOCK  WHERE
              OrderCustomer.Brand EQ gcBrand  AND  
              OrderCustomer.OrderID EQ iiOrderID AND
              OrderCustomer.RowType = 1 NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN 
      RETURN "3:Ordercustomer not available" + STRING(iiOrderId).

   lcModel = fGetTerminalData(Order.OrderId).

   ASSIGN lcName = OrderCustomer.Firstname + " " +
                   OrderCustomer.Surname1 + " " +
                   OrderCustomer.Surname2. 

   ldeInstallment = fGetOfferDeferredPayment(Order.Offer,
                                              Order.CrStamp,
                                              OUTPUT ldeMonthlyFee, 
                                              OUTPUT liMonths, /*24*/
                                              OUTPUT ldeFinalFee). /*residual*/
   RUN offer_penaltyfee.p(Order.OrderID,
                          OUTPUT liPermancyLength,
                          OUTPUT ldePermanencyAmount).
   lcCaseFileRow =
   lcCaseTypeId                   + lcDelim +
   /*Order_ID : 14566933*/
   STRING(Order.OrderID)          + lcDelim +
   /*Contract_ID : EB5CB2*/
   STRING(Order.ContractID)       + lcDelim +
   /*SFID: WEB*/
   STRING(Order.Salesman)         + lcDelim +
   /*MSISDN : 609321999*/
   STRING(Order.CLI)              + lcDelim +
   /*Order_date : 23-04-2015*/
   fPrintDate(Order.CrStamp)      + lcDelim +
   /*Order Channel: self*/
   STRING(Order.OrderChannel)     + lcDelim +
   /*Status_Code: 21*/
   STRING(Order.StatusCode)       + lcDelim +
   /*Company Name: IGOR SALAMANCA, S.L*/
   STRING(OrderCustomer.Company)  + lcDelim +
   /*Company ID : B37339967*/
   STRING(OrderCustomer.CustId) + lcDelim +
   /*Foundation date: 15-06-1998*/
   fPrintDateD(OrderCustomer.FoundationDate) + lcDelim +
   /*Full Name: ELENA DE FRANCISCO MARTINEZ*/
   lcName                         + lcDelim +
   /*Nationality: ES*/
   STRING(OrderCustomer.Nationality) + lcDelim +
   /*Doc ID Type: NIF*/
   STRING(Order.OrdererIDType)  + lcDelim +
   /*Doc ID: 44903161P*/
   STRING(Order.Ordererid)        +  lcDelim +
   /*Tariff Type: CONT15*/
   STRING(Order.CLIType)          + lcDelim +
   /*Previous Tariff: CONT*/
   fGetPrevTariff(BUFFER Order) + lcDelim +
   /*Donor operator: MoviStar*/
   STRING(Order.CurrOper)          + lcDelim +
   /*Bank Account: ES8321040075353030002643*/
   STRING(OrderCustomer.BankCode)  + lcDelim +
   /*Address: CALLE JALON 31 P6 2C*/
   STRING(OrderCustomer.Address)   + lcDelim +
   /*Zipcode:  29004*/
   STRING(OrderCustomer.ZipCode)   + lcDelim +   
   /*Postal Office:  Malaga*/
   STRING(OrderCustomer.PostOffice) + lcDelim +
   /*Email: ogmterra@gmail.com*/
   STRING(OrderCustomer.Email)      + lcDelim +
   /*Mobile number ( contact number ): 659016137*/
   STRING(OrderCustomer.MobileNumber) + lcDelim +  
   /*Total Installment: 720*/
   STRING(ldeInstallment)           + lcDelim +
   /*Residual value: 50*/
   STRING(ldeFinalFee)              + lcDelim + 
   /*Permanency: 400*/
   STRING(ldePermanencyAmount)      + lcDelim +
   /*Model Type: Samsung Galaxy S6 Edge Negro*/
   STRING(lcModel)   + lcDelim +
   /*Roi Risk: <Blank>*/
   STRING(Order.RiskCode).
   /*CASE 20*/
   /*portability pos-pos*/
   IF Order.StatusCode EQ {&ORDER_STATUS_COMPANY_NEW} AND
      Order.OrderType EQ {&ORDER_TYPE_MNP} AND
      Order.PayType EQ FALSE AND
      Order.OldPayType EQ FALSE  THEN lcParam = "DMS_S20_T1".
   /*new add pos / portability pre-pos.*/
   ELSE IF (Order.StatusCode EQ {&ORDER_STATUS_COMPANY_NEW} AND
            Order.OrderType EQ {&ORDER_TYPE_NEW} AND
            Order.PayType EQ FALSE )
      OR
           (Order.StatusCode EQ {&ORDER_STATUS_COMPANY_NEW} AND
            Order.OrderType EQ {&ORDER_TYPE_MNP} AND
            Order.PayType EQ FALSE AND
            Order.OldPayType EQ TRUE ) THEN lcParam = "DMS_S20_T2".
   /*add new pre / portability to pre*/
   ELSE IF Order.StatusCode EQ {&ORDER_STATUS_COMPANY_NEW} AND
           Order.OrderType EQ {&ORDER_TYPE_NEW} AND
           Order.PayType EQ TRUE
      OR
           Order.StatusCode EQ {&ORDER_STATUS_COMPANY_NEW} AND  
           Order.OrderType EQ {&ORDER_TYPE_MNP} AND
           Order.PayType EQ TRUE THEN lcPAram = "DMS_S20_T3".

   /*CASE 21,33*/
   /*portability pos-pos*/
   IF Order.StatusCode EQ {&ORDER_STATUS_COMPANY_MNP} AND
      Order.OrderType EQ {&ORDER_TYPE_MNP} AND
      Order.PayType EQ FALSE AND
      Order.OldPayType EQ FALSE  THEN 
      lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T1".
   /*new add pos / portability pre-pos.*/
   ELSE IF (Order.StatusCode EQ {&ORDER_STATUS_COMPANY_MNP} AND
            Order.OrderType EQ {&ORDER_TYPE_NEW} AND
            Order.PayType EQ FALSE )
      OR
           (Order.StatusCode EQ {&ORDER_STATUS_COMPANY_MNP} AND
            Order.OrderType EQ {&ORDER_TYPE_MNP} AND
            Order.PayType EQ FALSE AND
            Order.OldPayType EQ TRUE ) THEN 
      lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T2".
   /*stc to pos / migration+renewal*/
   ELSE IF Order.StatusCode EQ {&ORDER_STATUS_COMPANY_MNP} AND
           Order.OrderType EQ {&ORDER_TYPE_STC} AND
           Order.PayType EQ FALSE
      OR
           Order.OrderType EQ {&ORDER_TYPE_RENEWAL} /* AND
           Order.OrderChannel EQ TODO */ THEN 
      lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T3".
   /*add new pre / portability to pre*/
   ELSE IF Order.StatusCode EQ {&ORDER_STATUS_COMPANY_MNP} AND
           Order.OrderType EQ {&ORDER_TYPE_NEW} AND
           Order.PayType EQ TRUE
      OR
           Order.StatusCode EQ {&ORDER_STATUS_COMPANY_MNP} AND  
           Order.OrderType EQ {&ORDER_TYPE_MNP} AND
           Order.PayType EQ TRUE THEN 
      lcParam = "DMS_S" + STRING(Order.StatusCode) + "_T4".

   lcRequiredDocs = fCParam("DMS",lcParam).
   DO liCount = 1 TO NUM-ENTRIES(lcRequiredDocs):
      /*Document type,DocStatusCode,RevisionComment*/
      lcDocListEntries = ENTRY(liCount,lcRequiredDocs) + "," + 
                         "," +
                         lcDMSStatusDesc + "," + 
                         "Doc created".
      IF liCount NE NUM-ENTRIES(lcRequiredDocs) 
         THEN lcDocListEntries = lcDocListEntries + ",".
   END.

   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
   OUTPUT STREAM sOutFile CLOSE.
   fLogLine(lcCaseFileRow,"").
   lcCreateDMS = fUpdateDMS("", /*DmsExternalID*/
                            lcCaseTypeID,
                            Order.ContractID,
                            {&DMS_HOST_TABLE_ORDER},
                            Order.OrderId,
                            lcInitStatus,/*StatusCode*/
                            lcDMSStatusDesc,
                            Order.StatusCode,
                            0,
                            lcDocListEntries /*DocList*/,
                            ",").
END.

/*VFR*/
FUNCTION fCreateDocumentCase4 RETURNS CHAR
   (idStartTS AS DECIMAL,
    idEndTS AS DECIMAL):
   DEF VAR lcACCCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcSTCCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcIMEICaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcTariff AS CHAR NO-UNDO.
   DEF VAR lcDocListEntries AS CHAR NO-UNDO.
   DEF VAR lcCaseTypeId AS CHAR NO-UNDO.
   DEF VAR lcCreateDMS     AS CHAR NO-UNDO.
   DEF VAR ldeInstallment AS DECIMAL NO-UNDO.
   DEF VAR ldeMonthlyFee  AS DECIMAL NO-UNDO.
   DEF VAR liMonths AS INT NO-UNDO.
   DEF VAR ldeFinalFee AS DECIMAL NO-UNDO.
   DEF VAR lcCasefileRow   AS CHAR NO-UNDO.
   DEF VAR lcModel   AS CHAR NO-UNDO.
   DEF VAR ldePermanencyAmount AS DECIMAL.
   DEF VAR liPermancyLength AS INT.
   ASSIGN
      lcACCCaseTypeID   = '4c'
      lcSTCCaseTypeID   = '4b'
      lcIMEICaseTypeID  = '4a'.

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand EQ gcBrand AND
            MsRequest.ReqStatus EQ 2 AND
            MsRequest.UpdateStamp > idStartTS AND
            MsRequest.UpdateStamp < idEndTS AND
            (
             MsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}  /*81*/
             OR MsRequest.ReqType EQ {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE} /*10*/
             OR MsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}  /*0*/
             OR MsRequest.ReqType EQ {&REQTYPE_IMEI_CHANGE} /*80*/
            ) AND
            MsRequest.ReqCparam6 NE "" AND 
            MsRequest.UpdateStamp <= MsRequest.DoneStamp :

      CASE MsRequest.ReqType:
         WHEN {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE}  THEN DO:
            lcCaseTypeId = lcACCCaseTypeId.
            /*fenerate tariff:*/
            lcTariff = "".
            FIND FIRST MobSub NO-LOCK WHERE
                       MobSub.MsSeq EQ MsRequest.MsSeq NO-ERROR.
            IF AVAIL MobSub THEN lcTariff = MobSub.CLIType.

            lcCaseFileRow =
            lcCaseTypeId                                    + lcDelim +
            /*Contract_ID */
            STRING(MsRequest.ReqCparam6)                    + lcDelim +
            /*.SFID */
            REPLACE(Msrequest.UserCode, "VISTA_", "")       + lcDelim +
            /*.MSISDN*/
            STRING(MsRequest.CLI)                           + lcDelim +
            /*.ACC_Request_date*/
            fPrintDate(MsRequest.ReqDparam1)                + lcDelim +
            /*.Current Tariff*/
            lcTariff.
         END.

         WHEN {&REQTYPE_BUNDLE_CHANGE} THEN DO:
            lcCaseTypeId = lcSTCCaseTypeId.

            lcCaseFileRow =
            lcCaseTypeID                                    + lcDelim +
            /*Contract_ID*/
            STRING(MsRequest.ReqCparam6)                    + lcDelim +
            /*SFID*/
            REPLACE(Msrequest.UserCode, "VISTA_", "")       + lcDelim +
            /*MSISDN*/
            STRING(MsRequest.CLI)                           + lcDelim +
            /*STC_Request_date*/
            fPrintDate(MsRequest.ActStamp)                + lcDelim +
            /*Previous_Tariff*/            
            STRING(MsRequest.ReqCparam1)                    + lcDelim +
            /*New_Tariff*/
            STRING(MsRequest.ReqCparam2).
         END.
 
         WHEN {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} THEN DO:
            lcCaseTypeId = lcSTCCaseTypeId.
            lcTariff = "".

            IF LOOKUP(msrequest.reqcparam1, lcBundleCLITypes) = 0 THEN
               lcTariff = msrequest.ReqCParam1.
            ELSE DO:
               
               FIND FIRST MsOwner NO-LOCK WHERE
                          Msowner.Brand = gcBrand AND
                          MsOwner.CLI   = msrequest.CLI AND
                          MsOwner.CLIType = msrequest.reqcparam1 AND
                          MsOwner.TsEnd < msrequest.donestamp AND 
                          MsOwner.MsSeq = msrequest.msseq
                    USE-INDEX CLI NO-ERROR.
               IF AVAIL MsOwner THEN 
                  lcTariff = (IF MsOwner.tariffbundle > ""
                              THEN MsOwner.tariffbundle 
                              ELSE MsOwner.CLIType).
               ELSE lcTariff = MsRequest.ReqCparam1.
            END.

            lcCaseFileRow =
            lcCaseTypeID                                    + lcDelim +
            /*Contract_ID*/
            STRING(MsRequest.ReqCparam6)                    + lcDelim +
            /*SFID*/
            REPLACE(Msrequest.UserCode, "VISTA_", "")       + lcDelim +
            /*MSISDN*/
            STRING(MsRequest.CLI)                           + lcDelim +
            /*STC_Request_date*/
            fPrintDate(MsRequest.ReqDparam1)                + lcDelim +
            /*Previous_Tariff*/            
            lcTariff                                        + lcDelim +
            /*New_Tariff*/
            STRING(MsRequest.ReqCparam2).
         END.
         WHEN {&REQTYPE_IMEI_CHANGE} THEN DO:
            lcCaseTypeId = lcIMEICaseTypeID.
            FIND FIRST Order NO-LOCK WHERE 
                       Order.Brand EQ gcBrand AND
                       Order.OrderID EQ MsRequest.ReqIparam1 NO-ERROR.
            IF NOT AVAIL Order THEN DO:
               fLogLine(lcCaseFileRow,"Order not found " + lcCaseTypeId).
               NEXT.
            END. 
            lcModel = fGetTerminalData(MsRequest.ReqIparam1).
            ldeInstallment = fGetOfferDeferredPayment(Order.Offer,
                                                       Order.CrStamp,
                                                       OUTPUT ldeMonthlyFee,
                                                       OUTPUT liMonths,
                                                       OUTPUT ldeFinalFee).
               RUN offer_penaltyfee.p(Order.OrderID,
                                      OUTPUT liPermancyLength,
                                      OUTPUT ldePermanencyAmount).
            /**/ 
            lcCaseFileRow =  
            lcCaseTypeId                                    + lcDelim + 
            /*Order_ID*/ 
            STRING(MsRequest.ReqIparam1)                    + lcDelim + 
            /*Contract_ID*/ 
            STRING(MsRequest.ReqCparam6)                    + lcDelim + 
            /*SFID*/ 
            REPLACE(Msrequest.UserCode, "VISTA_", "")       + lcDelim + 
            /*MSISDN*/ 
            STRING(MsRequest.CLI)                           + lcDelim + 
            /*Order_date*/ 
            fPrintDate(MsRequest.CreStamp)                  + lcDelim + 
            /*Modification date*/ 
            fPrintDate(MsRequest.UpdateStamp)               + lcDelim + 
            /*Modification number*/ 
            fCountIMEIModifications(MsRequest.MsSeq)        + lcDelim + 
            /*New IMEI*/ 
            STRING(MsRequest.ReqCparam2)                    + lcDelim + 
            /*New Handset*/ 
            STRING(lcModel)                                 + lcDelim + 
            /*New Installment*/ 
            STRING(ldeInstallment)                          + lcDelim + 
            /*New Residual value*/ 
            STRING(ldeFinalFee)                             + lcDelim + 
            /*New Permanency*/ 
            STRING(ldePermanencyAmount)                     + lcDelim + 
            /*Previous IMEI*/ 
            STRING(MsRequest.ReqCparam1)                    + lcDelim + 
            /*Previous Handset*/ 
            fGetHandset(MsRequest.ReqIparam1,
                                MsRequest.ReqCparam1)       + lcDelim + 
            /*Previous Installment*/ 
            STRING(ldeInstallment)                          + lcDelim + 
            /*Previous Residual value*/ 
            STRING(ldeFinalFee)                             + lcDelim + 
            /*Previous Permanency*/  
            STRING(ldePermanencyAmount). 

         END. 
      END. 
      /*Document type,DocStatusCode,RevisionComment*/
      lcDocListEntries = "". 

      OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
      PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
      OUTPUT STREAM sOutFile CLOSE.

      fLogLine(lcCaseFileRow,"").
      lcCreateDMS = fUpdateDMS("", /*DmsExternalID*/
                               lcCaseTypeID,
                               MsRequest.ReqCparam6,
                               {&DMS_HOST_TABLE_MSREQ},
                               MsRequest.MsRequest,
                               lcInitStatus,/*StatusCode*/
                               lcDMSStatusDesc,
                               "",
                               0,
                               lcDocListEntries /*DocList*/,
                               ",").      
   END.
END.

FUNCTION fCreateDocumentCase5 RETURNS CHAR
   (iiOrderId AS INT):
   DEF VAR lcCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcStatusDesc    AS CHAR NO-UNDO.
   DEF VAR lcCreateDMS     AS CHAR NO-UNDO.
   DEF VAR lcDocListEntries AS CHAR NO-UNDO.
   DEF VAR lcCasefileRow   AS CHAR NO-UNDO.

   lcCaseTypeId    = "5".

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand EQ gcBrand AND
              Order.OrderID EQ iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN 
      RETURN "5:Order not available" + STRING(iiOrderId).

   lcCaseFileRow =
   lcCaseTypeID                    + lcDelim +
   /*Order_ID*/
   STRING(Order.OrderID)          + lcDelim +
   /*Contract_ID*/
   STRING(Order.ContractID)       + lcDelim +
   /*SFID*/
   STRING(Order.Salesman)         + lcDelim +
   /*MSISDN*/
   STRING(Order.CLI)              + lcDelim +
   /*Order_date*/
   fPrintDate(Order.CrStamp)      + lcDelim +
   /*Order Channel*/
   STRING(Order.OrderChannel)     + lcDelim +
   /*Status_Code*/
   STRING(Order.StatusCode)       + lcDelim +
   /*Order type*/
   STRING(Order.OrderType)        + lcDelim +
   /*Segment*/
   fGetSegment(Order.OrderID)     + lcDelim +
   /*Terminal type*/
   fGetTerminalFinanceType(iiOrderId).
   /*Document type,DocStatusCode,RevisionComment*/
   lcDocListEntries = "".

   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
   OUTPUT STREAM sOutFile CLOSE.

   fLogLine(lcCaseFileRow,"").
   lcCreateDMS = fUpdateDMS("", /*DmsExternalID*/
                            lcCaseTypeID,
                            Order.ContractID,
                            {&DMS_HOST_TABLE_ORDER},
                            Order.OrderId,
                            lcInitStatus,/*StatusCode*/
                            lcDMSStatusDesc,
                            Order.StatusCode,
                            0,
                            lcDocListEntries /*DocList*/,
                            ",").
END.

FUNCTION fCreateDocumentCase6 RETURNS CHAR
   (iiOrderId AS INT):
   DEF VAR lcCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcDocListEntries AS CHAR NO-UNDO.
   DEF VAR lcCreateDMS     AS CHAR NO-UNDO.
   DEF VAR ldeCancellationTime AS DECIMAL NO-UNDO.
   DEF VAR lcCancellationType AS CHAR NO-UNDO.
   DEF VAR lcPrevStatus AS CHAR NO-UNDO.
   DEF VAR lcCasefileRow   AS CHAR NO-UNDO.

   ASSIGN
      lcCaseTypeId    = "6"
      lcCancellationType = "".

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand EQ gcBrand AND
              Order.OrderID EQ iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN 
      RETURN "6:Order not available" + STRING(iiOrderId).

   lcPrevStatus = fGetOrderStatusDMS(Order.ContractID).
   lcCAncellationType = fGetCancellationInfo(Order.MsSeq,
                                             lcPrevStatus,
                                             idPeriodStart, 
                                             idPeriodEnd,
                                             OUTPUT ldeCAncellationTime).
   lcCaseFileRow =
   lcCaseTypeID                    + lcDelim +
   /*Order_ID*/
   STRING(Order.OrderID)          + lcDelim +
   /*Contract_ID*/
   STRING(Order.ContractID)       + lcDelim +
   /*SFID*/
   STRING(Order.Salesman)         + lcDelim +
   /*MSISDN*/
   STRING(Order.CLI)              + lcDelim +
   /*Order_date*/
   fPrintDate(Order.CrStamp)      + lcDelim +
   /*Status_Code*/
   lcPrevStatus                   + lcDelim + 
   /*Cancellation date*/
   fPrintDate(ldeCancellationTime) + lcDelim + 
   /*Cancellation type*/
   lcCancellationType.
   /*Document type,DocStatusCode,RevisionComment*/
   lcDocListEntries = "".

   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
   OUTPUT STREAM sOutFile CLOSE.

   fLogLine(lcCaseFileRow,"").
   lcCreateDMS = fUpdateDMS("", /*DmsExternalID*/
                            lcCaseTypeID,
                            Order.ContractID,
                            {&DMS_HOST_TABLE_ORDER},
                            Order.OrderId,
                            lcInitStatus,/*StatusCode*/
                            lcDMSStatusDesc,
                            Order.StatusCode,
                            0,
                            lcDocListEntries /*DocList*/,
                            ",").
END.

FUNCTION fCreateDocumentRows RETURNS CHAR
 (icCaseID as CHAR):
   DEF VAR lcStatus AS CHAR NO-UNDO.
   fLogLine("","Create Documents " + icCaseID).
   CASE icCaseID:
      WHEN {&DMS_CASE_TYPE_ID_ORDER_ACT} THEN DO:
         /*From Order*/
         FOR EACH ttOrderList WHERE 
                  ttOrderList.CaseID EQ {&DMS_CASE_TYPE_ID_ORDER_ACT}: 
            lcStatus = fCreateDocumentCase1(ttOrderList.OrderID).
            IF lcStatus NE "" THEN fLogLine("",lcStatus).
         END.
      END.
      WHEN {&DMS_CASE_TYPE_ID_ORDER_RESTUDY} THEN DO:
         /*From Order*/
         FOR EACH ttOrderList WHERE
                  ttOrderList.CaseID EQ {&DMS_CASE_TYPE_ID_ORDER_RESTUDY}:
            lcStatus = fCreateDocumentCase2(ttOrderList.OrderID).
            IF lcStatus NE "" THEN fLogLine("",lcStatus).
         END.
      END.
      WHEN {&DMS_CASE_TYPE_ID_COMPANY} THEN DO:
         /*From Order*/
         FOR EACH ttOrderList WHERE
                   ttOrderList.CaseID EQ {&DMS_CASE_TYPE_ID_COMPANY}:
           lcStatus = fCreateDocumentCase3(ttOrderList.OrderID).
           IF lcStatus NE "" THEN fLogLine("",lcStatus).
         END.
      END.
      WHEN {&DMS_CASE_TYPE_ID_ORDER_VFR} THEN DO:
         /*From MsRequest*/
         lcStatus = fCreateDocumentCase4(idPeriodStart, idPeriodEnd).
      END.
      WHEN {&DMS_CASE_TYPE_ID_DIRECT_CH} THEN DO:
         /*From Order*/
         FOR EACH ttOrderList WHERE
                  ttOrderList.Direct EQ TRUE:
            lcStatus = fCreateDocumentCase5(ttOrderList.OrderID).
            IF lcStatus NE "" THEN fLogLine("",lcStatus).
         END.
      END.
      WHEN {&DMS_CASE_TYPE_ID_CANCEL} THEN DO:
         /*From Order*/
         FOR EACH ttOrderList WHERE
                  ttOrderList.CaseID EQ {&DMS_CASE_TYPE_ID_CANCEL}:
            lcStatus = fCreateDocumentCase6(ttOrderList.OrderID).
            IF lcStatus NE "" THEN fLogLine("",lcStatus).
         END.
      END.
   END. /*Case*/
   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED "" SKIP.
   OUTPUT STREAM sOutFile CLOSE.

END.

/*Main functionality*/
OUTPUT STREAM sLogFile TO VALUE(icLogFile) APPEND.

ldCurrentTime = fMakeTS().

fLogLine("","DMS Casefile creation starts " + fTS2HMS(ldCurrentTime)).
fLogLine("", "Collection period: " + 
         STRING(idPeriodStart) + " " + fTS2HMS(idPeriodStart) + " - " + 
         STRING(idPeriodEnd) + " " + fTS2HMS(idPeriodEnd) ).

/* Create temb table to ensure that multiple order changes 
   do not produce extra documents. Only 1 doc/order is provided. */
fMakeTempTable(icCases, idPeriodStart, idPeriodEnd).
/*Create data files for requested types.*/
DO liCaseCount = 1 TO NUM-ENTRIES(icCases):
   lcStatus = fCreateDocumentRows(ENTRY(liCaseCount,icCases)).
END.

ldCurrentTime = fMakeTS().
fLogLine("","DMS Casefile creation ends " + fTS2HMS(ldCurrentTime)).
OUTPUT STREAM sLogFile CLOSE.
