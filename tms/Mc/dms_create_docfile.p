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

{Syst/commali.i}
{Syst/tmsconst.i}
{Mc/offer.i}
{Func/dms.i}
{Func/q25functions.i}
{Func/profunc.i}

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
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO. 
DEF VAR lcDMSDOCStatus AS CHAR NO-UNDO.
lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

DEF TEMP-TABLE ttOrderList NO-UNDO
   FIELD OrderID   AS INT
   FIELD CaseID    AS CHAR
   FIELD Direct    AS LOGICAL
   FIELD MsRequest AS INT
   FIELD TeleSales AS LOGICAL
   INDEX OrderID OrderID
   INDEX CaseID CaseID
   INDEX Direct Direct
   INDEX MsRequest MsRequest
   INDEX TeleSales TeleSales
   .

ASSIGN
   lcInitStatus    = {&DMS_INIT_STATUS_SENT}
   lcDMSStatusDesc = {&DMS_INIT_STATUS_COMMENT}
   lcDMSDOCStatus  = {&DMS_INIT_STATUS_SENT}
   lcDelim         = {&DMS_FILE_SEP}.

/*Functions:*/

FUNCTION fLogLine RETURNS LOGICAL
   (icLine AS CHAR,
   icMessage AS CHAR):
   PUT STREAM sLogFile UNFORMATTED
      icLine "#"
      icMessage "#"
      "TMS" SKIP.
END FUNCTION.

FUNCTION fLogMsg RETURNS LOGICAL
   (icMessage AS CHAR):
   PUT STREAM sLogFile UNFORMATTED
      icMessage "#"
      "DMS" SKIP.
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
   DEF VAR llTeleSales  AS LOG NO-UNDO.
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
   DEF VAR lcCancelTypeList AS CHAR NO-UNDO.
   DEF VAR liMsRequest AS INT NO-UNDO.

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
         WHEN {&DMS_CASE_TYPE_ID_TELESALES}      THEN
            ASSIGN
               llgOrderSeek = TRUE
               llTeleSales  = TRUE
               .
     END.
   END.
   IF llgOrderSeek EQ TRUE THEN DO:
      lcStampTypes = STRING({&ORDERTIMESTAMP_DELIVERY}) + "," +
                     STRING({&ORDERTIMESTAMP_SEND}) + "," +
                     STRING({&ORDERTIMESTAMP_CHANGE}).
      DO liStamptypeCount = 1 TO NUM-ENTRIES (lcStampTypes):
         liRT = INT(ENTRY(liStampTypeCount,lcStampTypes)).

         FOR EACH OrderTimestamp NO-LOCK WHERE
                  OrderTimestamp.Brand EQ Syst.Var:gcBrand AND
                  OrderTimestamp.RowType EQ liRt AND
                  Ordertimestamp.TimeStamp < idEndTS AND
                  Ordertimestamp.TimeStamp >= idStartTS:
            FIND FIRST ttOrderList WHERE
                       ttOrderList.OrderID EQ OrderTimestamp.OrderId NO-ERROR.
            IF AVAIL ttOrderList THEN NEXT.
            FIND FIRST Order NO-LOCK WHERE
                       Order.Brand EQ Syst.Var:gcBrand AND
                       Order.OrderID EQ OrderTimestamp.OrderId NO-ERROR.
            IF NOT AVAIL Order THEN NEXT.
            /*Default values for new loop*/
            ASSIGN
               llgDirect = FALSE
               llgAddEntry = FALSE
               lcCase = "".
            /* Case 14 TeleSales */
            IF llTeleSales AND LOOKUP(Order.OrderChannel, {&DMS_CASE_14_FILTER}) > 0 THEN DO:
               IF fChkDMSExists({&DMS_HOST_TABLE_ORDER},OrderTimestamp.OrderId) = FALSE THEN DO:
                  CREATE ttOrderList.
                  ASSIGN
                     ttOrderList.OrderID     = OrderTimestamp.OrderId
                     ttOrderList.CaseID      = {&DMS_CASE_TYPE_ID_TELESALES}
                     ttOrderList.TeleSales   = TRUE.
               END.      
            END.
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
                            ttOrderList.CaseID = {&DMS_CASE_TYPE_ID_DIRECT_CH}.
                            ttOrderList.Direct = llgDirect.
                     NEXT.  /*no need to check other cases because they can not be parallel according to current specs.*/
                  END.
            END.
            /*Case 1: Activations*/
            IF Order.StatusCode EQ {&ORDER_STATUS_DELIVERED} /*6*/ AND
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
                (previous sending). NOTE: This is not allowed if DMS has notified the
                cancellation(statuses E,J,F,N,G).
                Also cancellations are filtered by OrderStatus so that only specific
                orders are allowed to produce cancellations.
                */
               lcCancelTypeList =  SUBST("&1,&2,&3,&4,&5",
                  {&ORDER_STATUS_RENEWAL_STC_COMPANY},
                  {&ORDER_STATUS_MORE_DOC_NEEDED},
                  {&ORDER_STATUS_COMPANY_NEW},
                  {&ORDER_STATUS_COMPANY_MNP},
                  {&ORDER_STATUS_DELIVERED}).


               FIND FIRST DMS NO-LOCK WHERE
                          DMS.ContractID EQ Order.ContractID AND 
                           LOOKUP(DMS.StatusCode, "E,J,F,N,G") = 0 AND
                           LOOKUP(DMS.OrderStatus, lcCancelTypeList) > 0
                           NO-ERROR.
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
            MsRequest.Brand EQ Syst.Var:gcBrand AND
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
         ELSE ASSIGN
            liAddId     = MsRequest.ReqIparam1
            lcCase      = {&DMS_CASE_TYPE_ID_CANCEL}
            liMsRequest = MsRequest.MsRequest.
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
            ELSE ASSIGN
               liAddId     = Order.OrderId
               lcCase      = {&DMS_CASE_TYPE_ID_CANCEL}
               liMsRequest = MsRequest.MsRequest.
         END.
         ELSE NEXT.
      END.
      ELSE NEXT.

      /*Sending is allowed only if there is previous DMS entry for
        the change. */
      FIND FIRST DMS NO-LOCK WHERE
                 DMS.HostTable EQ {&DMS_HOST_TABLE_ORDER} AND
                 DMS.HostID EQ liAddId AND
                 (  DMS.OrderStatus EQ {&ORDER_STATUS_DELIVERED} OR
                    DMS.OrderStatus EQ
                       {&ORDER_STATUS_MORE_DOC_NEEDED} OR
                    DMS.OrderStatus EQ
                       {&ORDER_STATUS_RENEWAL_STC_COMPANY} OR
                    DMS.OrderStatus EQ {&ORDER_STATUS_COMPANY_NEW} OR
                    DMS.OrderStatus EQ {&ORDER_STATUS_COMPANY_MNP}
                  )  NO-ERROR.

      IF AVAIL DMS THEN DO TRANS:
         CREATE ttOrderList.
         ASSIGN ttOrderList.OrderID    = liAddId
                ttOrderList.CaseID     = lcCase
                ttOrderList.MsRequest  = liMsRequest.
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

/*The value can be Simonly, Handset, Financed Handset.*/
FUNCTION fGetTerminalFinanceType RETURNS CHAR
   (iiOrderID AS INT):
   DEF BUFFER bOrder FOR Order.
   DEF BUFFER bOA FOR OrderAccessory.
   DEF VAR ldeInstallment AS DECIMAL NO-UNDO.
   DEF VAR ldeMonthlyFee  AS DECIMAL NO-UNDO.
   DEF VAR liMonths AS INT NO-UNDO.
   DEF VAR ldeFinalFee AS DECIMAL NO-UNDO.

   FIND FIRST bOrder NO-LOCK WHERE
              bOrder.Brand EQ Syst.Var:gcBrand AND
              bOrder.OrderId EQ iiOrderId NO-ERROR.
   IF NOT AVAIL bOrder THEN RETURN "".

   FIND FIRST bOA NO-LOCK  WHERE
              bOA.Brand EQ Syst.Var:gcBrand AND
              bOA.OrderID EQ iiOrderID AND
              bOA.TerminalType EQ {&TERMINAL_TYPE_PHONE} NO-ERROR.
   IF AVAIL bOA THEN DO:

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

FUNCTION fGetCancellationInfo RETURNS CHAR
   (iiMsSeq AS INT,
    icStatus AS CHAR,
    idStartTS AS DECIMAL,
    idEndTS AS DECIMAL,
   OUTPUT odeTime AS DECIMAL):
   DEF BUFFER bMsRequest FOR MsRequest.
   DEF VAR liRt AS INT NO-UNDO.
   DEF VAR liReqTypeCount AS INT NO-UNDO.
   DEF VAR lcReqTypes AS CHAR NO-UNDO.
   odeTime = idEndTS.

   lcReqTypes = STRING({&REQTYPE_SUBSCRIPTION_TERMINATION}) + "," +
                STRING({&REQTYPE_REVERT_RENEWAL_ORDER} ).
   DO liReqTypeCount = 1 TO NUM-ENTRIES (lcReqTypes):
      liRT = INT(ENTRY(liReqTypeCount,lcReqTypes)).
      FIND FIRST bMsRequest NO-LOCK WHERE
                 bMsRequest.Brand EQ Syst.Var:gcBrand AND
                 bMsRequest.ReqStatus EQ 2 AND
                 bMsRequest.UpdateStamp > idStartTS AND
                 bMsRequest.UpdateStamp < idEndTS AND
                 bMsRequest.ReqType EQ liRt AND
                 bMsrequest.MsSeq EQ iiMsSeq AND
                 bMsRequest.UpdateStamp <= bMsRequest.DoneStamp NO-ERROR.

      IF AVAIL bMsRequest THEN DO:
         IF bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
            bMsRequest.ReqCparam3 EQ "11" THEN DO:
            odeTime = bMsRequest.CreStamp.
            RETURN "POS Order Cancellation".
         END.
         ELSE IF bMsRequest.ReqType EQ {&REQTYPE_REVERT_RENEWAL_ORDER} THEN DO:
            odeTime = bMsRequest.CreStamp.
            RETURN "Order Cancellation".
         END.
      END.
   END. /*DO for msrequest search*/
   IF icStatus EQ {&ORDER_STATUS_MORE_DOC_NEEDED} OR
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
              OrderAccessory.Brand EQ Syst.Var:gcBrand AND
              OrderAccessory.OrderID EQ iiOrderID AND
              Orderaccessory.TerminalType EQ {&TERMINAL_TYPE_PHONE} NO-ERROR.
   IF AVAIL OrderAccessory THEN DO:
      FIND FIRST Billitem NO-LOCK WHERE
                 BillItem.Brand   = Syst.Var:gcBrand AND
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
                    Msowner.Brand = Syst.Var:gcBrand AND
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
   (iiMsSeq AS INT,
    idTime  AS DECIMAL):
   DEF BUFFER bMsrequest FOR MsRequest.
   DEF VAR liCount AS INT NO-UNDO.
   FOR EACH bMsRequest NO-LOCK WHERE
            bMsRequest.MsSeq EQ iiMsSeq AND
            bMsRequest.ReqType EQ {&REQTYPE_IMEI_CHANGE} AND
            bMsRequest.ReqStatus EQ 2 AND
            bMsRequest.ReqCparam6 NE "" AND
            bMsRequest.UpdateStamp <= bMsRequest.DoneStamp AND
            bMsRequest.UpdateStamp <= idTime:
      liCount = liCount + 1.
   END.
   RETURN STRING(liCount).
END.

FUNCTION fGetQ25Extension RETURNS CHAR
   (iiOrderId AS INT,
    OUTPUT ocItem AS CHAR):
   DEF BUFFER bOA FOR OrderAction.
   FIND FIRST bOA WHERE
              bOA.Brand EQ Syst.Var:gcBrand AND
              bOA.OrderID EQ iiOrderID AND
              bOA.ItemType EQ "Q25Extension" NO-ERROR.
   IF AVAIL bOA THEN DO:
      ocItem = bOa.ItemParam. /*itemparam -> singlefee.sourcekey.*/
      RETURN bOA.ItemKey. 
   END.
   ELSE RETURN "".
END.


/*key from orderaction*/
FUNCTION fGetQ25BankByOrder RETURNS CHAR
   (BUFFER Order FOR Order,
    icSourceKey AS CHAR):
FIND SingleFee USE-INDEX Custnum WHERE
   SingleFee.Brand = Syst.Var:gcBrand AND
   SingleFee.Custnum = Order.CustNum AND
   SingleFee.HostTable = "Mobsub" AND
   SingleFee.KeyValue = STRING(Order.MsSeq) AND
   SingleFee.SourceTable = "DCCLI" AND
   SingleFee.SourceKey = icSourceKey AND
   SingleFee.CalcObj = "RVTERM" NO-LOCK NO-ERROR.
   IF AVAIL Singlefee THEN DO:
      RETURN fBankByBillCode(SingleFee.BillCode).
   END.
   ELSE
   RETURN "".
END.


FUNCTION fFindQ25Cancellation RETURNS CHAR
   (BUFFER Order FOR Order,
    INPUT iiMsRequest AS INT):
   
   DEF BUFFER bMsRequest FOR MsRequest.

   IF Order.OrderType NE {&ORDER_TYPE_RENEWAL} THEN RETURN "".

   FIND FIRST OrderAction NO-LOCK WHERE
              OrderAction.Brand    EQ Syst.Var:gcBrand AND
              OrderAction.OrderId  EQ Order.OrderId AND
              OrderAction.ItemType EQ "Q25Discount" NO-ERROR.

   IF NOT AVAILABLE OrderAction THEN RETURN "".

   /* before deadline */
   FIND FIRST bMsRequest NO-LOCK WHERE
              bMsRequest.MsSeq      = Order.MsSeq AND
              bMsRequest.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION} AND
              bMsRequest.ReqStatus  = {&REQUEST_STATUS_CANCELLED} AND
              bMsRequest.ReqCParam3 = "RVTERM12" AND
              bMsRequest.reqiparam3 = INT(OrderAction.ItemParam) AND
              INDEX(bMsRequest.Memo, STRING(Order.OrderID)) > 0 NO-ERROR.
   IF AVAILABLE bMsRequest THEN RETURN bMsRequest.ReqCParam4.

   /* after deadline */
   IF iiMsRequest > 0 THEN DO:
      FIND FIRST bMsRequest NO-LOCK WHERE
                 bMsRequest.OrigRequest = iiMsRequest AND
                 bMsRequest.MsSeq       = Order.MsSeq AND
                 bMsRequest.ReqStatus  NE {&REQUEST_STATUS_CANCELLED} AND
                 bMsRequest.ReqType     = {&REQTYPE_CONTRACT_TERMINATION} AND
                 bMsRequest.ReqSource   = 
                                    {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER} AND
                 bMsRequest.ReqCParam2  = "term" AND
                 bMsRequest.ReqCparam3  = "RVTERM12" NO-ERROR.
      IF AVAILABLE bMsRequest THEN RETURN bMsRequest.ReqCParam4.
   END.
   RETURN "".
END.

/*function removes channel name from salesmanID.
For example */
FUNCTION fCutChannel RETURNS CHAR
   (icSalesman AS CHAR):
   DEF VAR i AS INT NO-UNDO.
   i = INDEX(icSalesman, "_").
   IF i > 0 THEN
      RETURN SUBSTRING(icSalesman, i + 1).

  RETURN icSalesman.

END.

FUNCTION fGetPermanencyAndHandset RETURNS CHAR
   (icList AS CHAR,
    OUTPUT ocNewHS AS CHAR,
    OUTPUT ocPrevHS AS CHAR,
    OUTPUT ocNewP AS CHAR,
    OUTPUT ocPrevP AS CHAR):
   DEF VAR i AS INT NO-UNDO.
   DEF VAR lcEntryContent AS CHAR NO-UNDO.

   DO i = 1 TO NUM-ENTRIES(icList, {&DMS_REQ_FIELD_SEP}):
       lcEntryContent = ENTRY(i,icList,{&DMS_REQ_FIELD_SEP}).
       /*Content format: FieldName CHAR(255) FieldValue*/
      CASE ENTRY(1,lcEntryContent,{&DMS_REQ_VAL_SEP}):
         WHEN "NewHS" THEN
            ocNewHS = ENTRY(2,lcEntryContent,{&DMS_REQ_VAL_SEP}).
         WHEN "PrevHS" THEN
            ocPrevHS = ENTRY(2,lcEntryContent,{&DMS_REQ_VAL_SEP}).
         WHEN "NewPerm" THEN
            ocNewP = ENTRY(2,lcEntryContent,{&DMS_REQ_VAL_SEP}).
         WHEN "PrevPerm" THEN
            ocPrevP = ENTRY(2,lcEntryContent,{&DMS_REQ_VAL_SEP}).
      END.
   END.
END.  

FUNCTION fGetTVService RETURNS CHARACTER
  (iiMsSeq   AS INTEGER, 
   iiOrderId AS INTEGER):

  DEFINE BUFFER bf_TPService   FOR TPService.
  DEFINE BUFFER bf_OrderAction FOR OrderAction.
  DEFINE BUFFER bf_DayCampaign FOR DayCampaign.

  DEFINE VARIABLE lcProduct AS CHAR NO-UNDO.

  FIND FIRST bf_TPService WHERE bf_TPService.MsSeq     = iiMsSeq            AND 
                                bf_TPService.Operation = {&TYPE_ACTIVATION} AND 
                                bf_TPService.ServType  = "Television"       AND 
                         LOOKUP(bf_TPService.ServStatus, {&STATUS_CANCELED} + "," + {&STATUS_ERROR}) = 0 NO-LOCK NO-ERROR.
  IF AVAIL bf_TPService THEN 
      ASSIGN lcProduct = bf_TPService.Product.
  ELSE IF NOT (CAN-FIND(FIRST MobSub     WHERE MobSub.MsSeq     = iiMsSeq NO-LOCK)  OR 
               CAN-FIND(FIRST TermMobSub WHERE TermMobSub.MsSeq = iiMsSeq NO-LOCK)) THEN
  DO:
      FOR EACH bf_OrderAction WHERE bf_OrderAction.Brand    = Syst.Var:gcBrand      AND 
                                    bf_OrderAction.OrderId  = iiOrderId    AND 
                                    bf_OrderAction.ItemType = "BundleItem" NO-LOCK,
          FIRST bf_DayCampaign WHERE bf_DayCampaign.Brand   = Syst.Var:gcBrand AND 
                                     bf_DayCampaign.DCEvent = bf_OrderAction.ItemKey NO-LOCK:

          IF LOOKUP(STRING(bf_DayCampaign.BundleTarget), STRING({&TELEVISION_BUNDLE})) = 0 THEN 
              NEXT.

          ASSIGN lcProduct = bf_OrderAction.ItemKey.
          
          LEAVE.
      END.
  END.    

  RETURN lcProduct.

END FUNCTION.

FUNCTION fFixNumberAndDonorInformation RETURNS CHARACTER
   ( iiOrderID AS INTEGER,
     icDelim   AS CHARACTER,
     ilIncludeHolderInformation AS LOGICAL ):

   DEFINE BUFFER OrderCustomer FOR OrderCustomer.

   DEFINE VARIABLE lcReturnValue  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcMobileHolder AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcFixedHolder  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lii           AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcRowTypes    AS CHARACTER
      INITIAL "{&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER},{&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER}"
      NO-UNDO.
   DEFINE VARIABLE lcCustID      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llSameID      AS LOGICAL INITIAL FALSE NO-UNDO.
   DEFINE VARIABLE lcTemp        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liRowType     AS INTEGER   NO-UNDO.

   ASSIGN
      lcMobileHolder = FILL(icDelim,4)
      lcFixedHolder  = FILL(icDelim,4)
      .

   FIND FIRST OrderFusion NO-LOCK WHERE
              OrderFusion.Brand EQ Syst.Var:gcBrand AND
              OrderFusion.OrderID EQ iiOrderId NO-ERROR.

   IF AVAILABLE OrderFusion
   THEN lcReturnValue = OrderFusion.FixedNumberType + icDelim +
                        OrderFusion.FixedNumber + icDelim +
                        OrderFusion.FixedCurrOper + icDelim.
   ELSE lcReturnValue = FILL(icDelim,3).

   DO lii = 1 TO NUM-ENTRIES(lcRowTypes):

      liRowType = INTEGER(ENTRY(lii,lcRowTypes)).

      FIND FIRST OrderCustomer NO-LOCK WHERE
         OrderCustomer.Brand = Syst.Var:gcBrand AND
         OrderCustomer.OrderID = iiOrderID AND
         OrderCustomer.RowType = liRowType
      NO-ERROR.

      lcReturnValue = lcReturnValue + STRING(INTEGER(AVAILABLE OrderCustomer)) + icDelim.

      IF AVAILABLE OrderCustomer
      THEN DO:
         IF lcCustID EQ OrderCustomer.CustID
         THEN llSameID = TRUE.
         lcCustID = OrderCustomer.CustID.

         IF ilIncludeHolderInformation
         THEN DO:
            lcTemp = ( IF OrderCustomer.CustIdType = "CIF"
                       THEN OrderCustomer.AuthCustID
                       ELSE OrderCustomer.CustId ) + icDelim +
                     ( IF OrderCustomer.CustIdType = "CIF"
                       THEN OrderCustomer.AuthCustIDType
                       ELSE OrderCustomer.CustIdType ) + icDelim +
                     OrderCustomer.FirstName + " " +
                     OrderCustomer.SurName1 + " " +
                     OrderCustomer.SurName2 + icDelim +
                     IF OrderCustomer.CustIdType = "CIF"
                     THEN OrderCustomer.CustId + icDelim + OrderCustomer.Company
                     ELSE icDelim.

            CASE liRowType:
               WHEN {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER}
               THEN lcMobileHolder = lcTemp.
               WHEN {&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER}
               THEN lcFixedHolder = lcTemp.
            END CASE.
         END.
      END.
   END.

   lcReturnValue = lcReturnValue + STRING(INTEGER(NOT llSameID)).

   IF ilIncludeHolderInformation
   THEN lcReturnValue = lcReturnValue + icDelim + lcMobileHolder + icDelim +
                        lcFixedHolder.

   RETURN lcReturnValue.

END FUNCTION.

FUNCTION fGetFixNumber RETURNS CHARACTER
   ( iiOrderID AS INTEGER):

   DEFINE BUFFER bOrderFusion FOR OrderFusion.
   
   FIND FIRST bOrderFusion NO-LOCK WHERE
              bOrderFusion.Brand EQ Syst.Var:gcBrand AND
              bOrderFusion.OrderID EQ iiOrderId NO-ERROR.

   IF AVAILABLE bOrderFusion THEN
      RETURN bOrderFusion.FixedNumber.

   RETURN "".

END FUNCTION.


FUNCTION fGetTarrifType RETURNS CHARACTER
   (icCLIType AS CHARACTER):

   /* Ideally should never return -1 or -2 */
   
   DEFINE BUFFER bCLIType FOR CLIType.
   DEFINE VARIABLE liTarrifType  AS INTEGER   NO-UNDO INITIAL -2.
   
   FIND FIRST bCLIType NO-LOCK WHERE
              bCLIType.Brand      = Syst.Var:gcBrand           AND
              bCLIType.CLIType    = icCLIType
        NO-ERROR.
   
   IF AVAILABLE bCLIType THEN
   
      CASE bCLIType.TariffType:
         WHEN {&CLITYPE_TARIFFTYPE_CONVERGENT}  THEN liTarrifType =  0.
         WHEN {&CLITYPE_TARIFFTYPE_MOBILEONLY}  THEN liTarrifType =  1.
         WHEN {&CLITYPE_TARIFFTYPE_FIXEDONLY}   THEN liTarrifType =  2.
         OTHERWISE                                   liTarrifType = -1.
      END CASE.

   RETURN STRING(liTarrifType).
   
END FUNCTION.

/*Order activation*/
/*Function generates order documentation*/
FUNCTION fCreateDocumentCase1 RETURNS CHAR
   (iiOrderId AS INT):
   DEF VAR lcCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcContractID    AS CHAR NO-UNDO.
   DEF VAR lcStatusCode    AS CHAR NO-UNDO.
   DEF VAR lcDocList       AS CHAR NO-UNDO.
   DEF VAR lcCasefileRow   AS CHAR NO-UNDO.
   DEF VAR lcBank AS CHAR NO-UNDO.
   DEF VAR lcQ25Extension AS CHAR NO-UNDO.
   DEF VAR lcItem AS CHAR NO-UNDO.

   lcCaseTypeId = "1".

   FIND FIRST Order NO-LOCK WHERE 
              Order.Brand EQ Syst.Var:gcBrand AND
              Order.OrderID EQ iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN
      RETURN "1:Order not available" + STRING(iiOrderId).

   FIND FIRST OrderCustomer NO-LOCK  WHERE
              OrderCustomer.Brand EQ Syst.Var:gcBrand AND
              OrderCustomer.OrderID EQ iiOrderID AND
              OrderCustomer.RowType EQ 1 NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN
      RETURN "1:Ordercustomer not available" + STRING(iiOrderId).

   lcq25Extension = fGetQ25Extension(iiOrderId, lcItem).
   IF lcQ25Extension NE "" AND Order.Orderchannel BEGINS "renewal_pos" THEN DO:
      lcBank = fGetQ25BankByOrder(BUFFER Order, lcItem).
   END.
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
   /*Doc ID Type: NIF (ES Personal ID) NIE=foreign id (CIF for company)*/
   STRING(OrderCustomer.CustIDType) + lcDelim +
   /*Doc ID: 53233826G*/
   STRING(OrderCustomer.CustID)    + lcDelim +
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
   STRING(Order.OrderType)         + lcDelim +
   /**/
   fGetSegment(Order.Custnum, order.orderid)      + lcDelim +
   /*Terminal Type: The value can be Simonly, Handset, Financed Handset.*/
   fGetTerminalFinanceType(iiOrderId) + lcDelim +
   /*q25Extension YPR-3269*/
   lcQ25Extension                  + lcDelim +
   /* Q25 Extension bank */ 
   lcBank                          + lcDelim +
   fFixNumberAndDonorInformation(Order.OrderID, lcDelim, FALSE) + lcDelim +
   fGetTVService(Order.MsSeq, Order.OrderId)
   .

   /*Document type,DocStatusCode,RevisionComment*/

   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
   OUTPUT STREAM sOutFile CLOSE.

   fLogLine(lcCaseFileRow, "").     
   RETURN "".                         
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
   DEF VAR liPermancyLength AS INT NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR lcMsg AS CHAR NO-UNDO.
   DEFINE VARIABLE lcKialaCode AS CHARACTER NO-UNDO.

   DEF BUFFER DeliveryCustomer FOR OrderCustomer.

   ASSIGN
      lcCaseTypeId      = "2"
      lcModel           = "-".

   FIND FIRST Order NO-LOCK WHERE 
              Order.Brand = Syst.Var:gcBrand  AND
              Order.OrderID EQ iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN RETURN "2:Order not available" + STRING(iiOrderId).

   FIND FIRST OrderCustomer NO-LOCK  WHERE
              OrderCustomer.Brand EQ Syst.Var:gcBrand AND  
              OrderCustomer.OrderID EQ iiOrderID AND
              OrderCustomer.RowType EQ 1 NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN 
      RETURN "2:Ordercustomer not available" + STRING(iiOrderId).

   /*Get delivery address if it is available*/
   FIND FIRST DeliveryCustomer NO-LOCK  WHERE
              DeliveryCustomer.Brand EQ Syst.Var:gcBrand AND
              DeliveryCustomer.OrderID EQ iiOrderID AND
              DeliveryCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_DELIVERY} NO-ERROR.
   IF AVAIL DeliveryCustomer THEN DO:
      ASSIGN
         lcDeliveryAddress = DeliveryCustomer.Address
         lcDeliveryZIP = DeliveryCustomer.ZipCode
         lcDeliveryPost = DeliveryCustomer.PostOffice
         lcKialaCode    = DeliveryCustomer.KialaCode WHEN Order.DeliveryType = {&ORDER_DELTYPE_POS}
         .
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
   RUN Mc/offer_penaltyfee.p(Order.OrderID,
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
   STRING(Order.RiskCode)            + lcDelim +
   ( IF Order.DeliverySecure EQ 1
     THEN STRING({&ORDER_DELTYPE_POST_SECURE})
     ELSE IF Order.DeliverySecure EQ 2
     THEN STRING({&ORDER_DELTYPE_POS_SECURE})
     ELSE STRING(Order.DeliveryType) ) + lcDelim +
   lcKialaCode + lcDelim +
   fFixNumberAndDonorInformation(Order.OrderID, lcDelim, TRUE)  + lcDelim +
   fGetTVService(Order.MsSeq, Order.OrderId)
   .
   
   /*Solve tmsparam value for getting correct matrix row*/
   lcRequiredDocs = fNeededDocs(BUFFER Order).
   DO liCount = 1 TO NUM-ENTRIES(lcRequiredDocs):
      /*Document type, Type desc,DocStatusCode,RevisionComment*/
      lcDocListEntries = lcDocListEntries +
                         ENTRY(liCount,lcRequiredDocs) + {&DMS_DOCLIST_SEP} +
                         {&DMS_DOCLIST_SEP} + /*filled only by DMS resp*/
                         lcDMSDOCStatus + {&DMS_DOCLIST_SEP}.
      IF liCount NE NUM-ENTRIES(lcRequiredDocs )
         THEN lcDocListEntries = lcDocListEntries + {&DMS_DOCLIST_SEP}.
   END.

   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
   OUTPUT STREAM sOutFile CLOSE.

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
                            {&DMS_DOCLIST_SEP}).
   fLogLine(lcCaseFileRow,lcCreateDMS).
   lcErr = fSendChangeInformation("", 
                                  Order.OrderId, 
                                  "", 
                                  {&DMS_DOCLIST_SEP},
                                  "create_cf",
                                  lcMsg).
   fLogMsg("Msg,2 : " + lcMsg + " #Status: " + lcErr).

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
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR lcMsg AS CHAR NO-UNDO.
   DEFINE VARIABLE lcKialaCode AS CHARACTER NO-UNDO.

   DEF BUFFER DeliveryCustomer FOR OrderCustomer.

   ASSIGN
      lcCaseTypeId      = "3".

   FIND FIRST Order NO-LOCK WHERE 
              Order.Brand = Syst.Var:gcBrand  AND
              Order.OrderID EQ iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN RETURN "3:Order not available" + STRING(iiOrderId).

   FIND FIRST OrderCustomer NO-LOCK  WHERE
              OrderCustomer.Brand EQ Syst.Var:gcBrand  AND  
              OrderCustomer.OrderID EQ iiOrderID AND
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN 
      RETURN "3:Ordercustomer not available" + STRING(iiOrderId).

   IF Order.DeliveryType = {&ORDER_DELTYPE_POS}
   THEN DO:
      /*Get delivery address if it is available*/
      FIND FIRST DeliveryCustomer NO-LOCK  WHERE
                 DeliveryCustomer.Brand EQ Syst.Var:gcBrand AND
                 DeliveryCustomer.OrderID EQ iiOrderID AND
                 DeliveryCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_DELIVERY} NO-ERROR.
      IF AVAIL DeliveryCustomer
      THEN lcKialaCode = DeliveryCustomer.KialaCode.
   END.

   lcModel = fGetTerminalData(Order.OrderId).

   ASSIGN lcName = OrderCustomer.Firstname + " " +
                   OrderCustomer.Surname1 + " " +
                   OrderCustomer.Surname2. 

   ldeInstallment = fGetOfferDeferredPayment(Order.Offer,
                                              Order.CrStamp,
                                              OUTPUT ldeMonthlyFee, 
                                              OUTPUT liMonths, /*24*/
                                              OUTPUT ldeFinalFee). /*residual*/
   RUN Mc/offer_penaltyfee.p(Order.OrderID,
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
   STRING(OrderCustomer.AuthCustIdType)  + lcDelim +
   /*Doc ID: 44903161P*/
   STRING(OrderCustomer.AuthCustId)      +  lcDelim +
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
   STRING(Order.RiskCode) + lcDelim +
   ( IF Order.DeliverySecure EQ 1
     THEN STRING({&ORDER_DELTYPE_POST_SECURE})
     ELSE IF Order.DeliverySecure EQ 2
     THEN STRING({&ORDER_DELTYPE_POS_SECURE})
     ELSE STRING(Order.DeliveryType) ) + lcDelim +
   lcKialaCode + lcDelim +
   fFixNumberAndDonorInformation(Order.OrderID, lcDelim, TRUE).
   
   /*solve needed documents:*/
   lcRequiredDocs =  fNeededDocs(BUFFER Order).
   DO liCount = 1 TO NUM-ENTRIES(lcRequiredDocs):
      /*Document type, Type desc,DocStatusCode,RevisionComment*/
      lcDocListEntries = lcDocListEntries +
                         ENTRY(liCount,lcRequiredDocs) + {&DMS_DOCLIST_SEP} +
                         {&DMS_DOCLIST_SEP} + /* filled only by DMS responses*/
                         lcDMSDOCStatus + {&DMS_DOCLIST_SEP} +
                         "".
      IF liCount NE NUM-ENTRIES(lcRequiredDocs)
         THEN lcDocListEntries = lcDocListEntries + {&DMS_DOCLIST_SEP}.
   END.

   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
   OUTPUT STREAM sOutFile CLOSE.

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
                            {&DMS_DOCLIST_SEP}).
   fLogLine(lcCaseFileRow,lcCreateDMS).
   lcErr = fSendChangeInformation("", 
                                  Order.OrderId, 
                                  "", 
                                  {&DMS_DOCLIST_SEP},
                                   "create_cf",
                                   lcMsg).
   fLogMsg("Msg,3 : " + lcMsg + " #Status: " + lcErr).

   RETURN "".

END.

/*VFR*/
FUNCTION fCreateDocumentCase4 RETURNS CHAR
   (idStartTS AS DECIMAL,
    idEndTS AS DECIMAL):
   DEF VAR lcACCCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcSTCCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcIMEICaseTypeID    AS CHAR NO-UNDO. 
   DEF VAR lcICCCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcTariff AS CHAR NO-UNDO.
   DEF VAR lcFixedNumber AS CHAR NO-UNDO. 
   DEF VAR lcCaseTypeId AS CHAR NO-UNDO.
   DEF VAR ldeInstallment AS DECIMAL NO-UNDO.
   DEF VAR ldeMonthlyFee  AS DECIMAL NO-UNDO.
   DEF VAR liMonths AS INT NO-UNDO.
   DEF VAR ldeFinalFee AS DECIMAL NO-UNDO.
   DEF VAR lcCasefileRow   AS CHAR NO-UNDO.
   DEF VAR lcNewHandset AS CHAR NO-UNDO.
   DEF VAR lcPrevHandset AS CHAR NO-UNDO.
   DEF VAR lcNewPermanency AS CHAR NO-UNDO.
   DEF VAR lcPrevPermanency AS CHAR NO-UNDO.

   DEF BUFFER MobSub FOR MobSub.

   ASSIGN
      lcICCCaseTypeID   = '4d'
      lcACCCaseTypeID   = '4c'
      lcSTCCaseTypeID   = '4b'
      lcIMEICaseTypeID  = '4a'.

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand EQ Syst.Var:gcBrand AND
            MsRequest.ReqStatus EQ 2 AND
            MsRequest.UpdateStamp > idStartTS AND
            MsRequest.UpdateStamp < idEndTS AND
            (
             MsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}  /*81*/
             OR MsRequest.ReqType EQ {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE} /*10*/
             OR MsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}  /*0*/
             OR MsRequest.ReqType EQ {&REQTYPE_IMEI_CHANGE} /*80*/
             OR MsRequest.ReqType EQ {&REQTYPE_ICC_CHANGE} /*15*/
            ) AND
            MsRequest.ReqCparam6 NE "" AND 
            MsRequest.UpdateStamp <= MsRequest.DoneStamp:
      CASE MsRequest.ReqType:
         WHEN {&REQTYPE_ICC_CHANGE} THEN DO:
            lcCaseTypeId = lcICCCaseTypeId.
            lcCaseFileRow =
            lcCaseTypeID                                    + lcDelim +
            /*Contract_ID*/
            STRING(MsRequest.ReqCparam6)                    + lcDelim +
            /*SFID*/
            REPLACE(Msrequest.UserCode, "VISTA_", "")       + lcDelim +
            /*MSISDN*/
            STRING(MsRequest.CLI)                           + lcDelim +
            /*STC_Request_date*/
            fPrintDate(MsRequest.CreStamp)                  + lcDelim +
            /*request reason*/            
            STRING(MsRequest.ReqCparam4)                    + lcDelim +
            /*Previous_ICC*/
            STRING(MsRequest.ReqCparam3)                    + lcDelim +
            /*New_ICC*/
            STRING(MsRequest.ReqCparam2).  
         END.
         WHEN {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE}  THEN DO:

            ASSIGN
               lcCaseTypeId = lcACCCaseTypeId
               lcTariff = ""
               lcFixedNumber = "".

            /*generate tariff:*/
            FIND MobSub NO-LOCK WHERE
                 MobSub.MsSeq EQ MsRequest.MsSeq NO-ERROR.
            IF AVAIL MobSub THEN ASSIGN
               lcTariff = MobSub.CLIType
               lcFixedNumber = MobSub.FixedNumber WHEN MobSub.FixedNumber > "".

            lcCaseFileRow =
            lcCaseTypeId                                    + lcDelim +
            /*Contract_ID */
            STRING(MsRequest.ReqCparam6)                    + lcDelim +
            /*.SFID */
            REPLACE(Msrequest.UserCode, "VISTA_", "")       + lcDelim +
            /*.MSISDN*/
            STRING(MsRequest.CLI)                           + lcDelim +
            /*.ACC_Request_date*/
            fPrintDate(MsRequest.CreStamp)                  + lcDelim +
            /*.Current Tariff*/
            lcTariff                                        + lcDelim + 
            lcFixedNumber.
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
            fPrintDate(MsRequest.CreStamp)                  + lcDelim +
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
                          Msowner.Brand = Syst.Var:gcBrand AND
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
            fPrintDate(MsRequest.CreStamp)                  + lcDelim +
            /*Previous_Tariff*/            
            lcTariff                                        + lcDelim +
            /*New_Tariff*/
            STRING(MsRequest.ReqCparam2).
         END.
         WHEN {&REQTYPE_IMEI_CHANGE} THEN DO:
            lcCaseTypeId = lcIMEICaseTypeID.
            FIND FIRST Order NO-LOCK WHERE 
                       Order.Brand EQ Syst.Var:gcBrand AND
                       Order.OrderID EQ MsRequest.ReqIparam1 NO-ERROR.
            IF NOT AVAIL Order THEN DO:
               fLogLine(lcCaseFileRow,"Order not found " + lcCaseTypeId).
               NEXT.
            END. 
            ldeInstallment = fGetOfferDeferredPayment(Order.Offer,
                                                       Order.CrStamp,
                                                       OUTPUT ldeMonthlyFee,
                                                       OUTPUT liMonths,
                                                       OUTPUT ldeFinalFee).
            fGetPermanencyAndHandset(MsRequest.ReqCparam5,
                                     OUTPUT lcNewHandset,
                                     OUTPUT lcPrevHandset,
                                     OUTPUT lcNewPermanency,
                                     OUTPUT lcPrevPermanency).
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
            fCountIMEIModifications(MsRequest.MsSeq,
                                    Msrequest.UpdateStamp)  + lcDelim + 
            /*New IMEI*/ 
            STRING(MsRequest.ReqCparam2)                    + lcDelim + 
            /*New Handset*/ 
            STRING(lcNewHandset)                            + lcDelim + 
            /*New Installment*/ 
            STRING(ldeInstallment)                          + lcDelim + 
            /*New Residual value*/ 
            STRING(ldeFinalFee)                             + lcDelim + 
            /*New Permanency*/ 
            STRING(lcNewPermanency)                         + lcDelim + 
            /*Previous IMEI*/ 
            STRING(MsRequest.ReqCparam1)                    + lcDelim + 
            /*Previous Handset*/ 
            STRING(lcPrevHandset)                           + lcDelim + 
            /*Previous Installment*/ 
            STRING(ldeInstallment)                          + lcDelim + 
            /*Previous Residual value*/ 
            STRING(ldeFinalFee)                             + lcDelim + 
            /*Previous Permanency*/  
            STRING(lcPrevPermanency). 

         END. 
      END. 
      /*Document type,DocStatusCode,RevisionComment*/

      OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
      PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
      OUTPUT STREAM sOutFile CLOSE.

      fLogLine(lcCaseFileRow, "").
   END.
   RETURN "".

END.

FUNCTION fCreateDocumentCase5 RETURNS CHAR
   (iiOrderId AS INT):
   DEF VAR lcCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR lcStatusDesc    AS CHAR NO-UNDO.
   DEF VAR lcCasefileRow   AS CHAR NO-UNDO.
   DEFINE VARIABLE lcKialaCode AS CHARACTER NO-UNDO.

   DEF BUFFER DeliveryCustomer FOR OrderCustomer.

   lcCaseTypeId    = "5".

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand EQ Syst.Var:gcBrand AND
              Order.OrderID EQ iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN 
      RETURN "5:Order not available" + STRING(iiOrderId).

   IF Order.DeliveryType = {&ORDER_DELTYPE_POS}
   THEN DO:
      /*Get delivery address if it is available*/
      FIND FIRST DeliveryCustomer NO-LOCK  WHERE
                 DeliveryCustomer.Brand EQ Syst.Var:gcBrand AND
                 DeliveryCustomer.OrderID EQ iiOrderID AND
                 DeliveryCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_DELIVERY} NO-ERROR.
      IF AVAIL DeliveryCustomer
      THEN lcKialaCode = DeliveryCustomer.KialaCode.
   END.

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
   fGetSegment(Order.CustNum, order.orderid)     + lcDelim +
   /*Terminal type*/
   fGetTerminalFinanceType(iiOrderId) + lcDelim +
   ( IF Order.DeliverySecure EQ 1
     THEN STRING({&ORDER_DELTYPE_POST_SECURE})
     ELSE IF Order.DeliverySecure EQ 2
     THEN STRING({&ORDER_DELTYPE_POS_SECURE})
     ELSE STRING(Order.DeliveryType) ) + lcDelim +
   lcKialaCode + lcDelim +
   fGetTVService(Order.MsSeq, Order.OrderId).

   /*Document type,DocStatusCode,RevisionComment*/

   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
   OUTPUT STREAM sOutFile CLOSE.

   fLogLine(lcCaseFileRow, "").
   RETURN "".

END.

FUNCTION fCreateDocumentCase6 RETURNS CHAR
   (iiOrderId AS INT,
    iiMsRequest AS INT):
   DEF VAR lcCaseTypeID    AS CHAR NO-UNDO.
   DEF VAR ldeCancellationTime AS DECIMAL NO-UNDO.
   DEF VAR lcCancellationType AS CHAR NO-UNDO.
   DEF VAR lcPrevStatus AS CHAR NO-UNDO.
   DEF VAR lcCasefileRow   AS CHAR NO-UNDO.
   DEF VAR lcQ25ContractID AS CHAR NO-UNDO.

   ASSIGN
      lcCaseTypeId    = "6"
      lcCancellationType = "".

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand EQ Syst.Var:gcBrand AND
              Order.OrderID EQ iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN 
      RETURN "6:Order not available" + STRING(iiOrderId).

   lcPrevStatus = fGetOrderStatusDMS(Order.ContractID).
   lcCAncellationType = fGetCancellationInfo(Order.MsSeq,
                                             lcPrevStatus,
                                             idPeriodStart, 
                                             idPeriodEnd,
                                             OUTPUT ldeCAncellationTime).
   lcQ25ContractID = fFindQ25Cancellation(BUFFER Order, iiMsRequest).

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
   lcCancellationType              + lcDelim +
   /*Q25 Extension cancelled (if extension is cancelled */
   lcQ25ContractID.

   /*Document type,DocStatusCode,RevisionComment*/

   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
   OUTPUT STREAM sOutFile CLOSE.

   fLogLine(lcCaseFileRow, "").
   RETURN "".

END.

/*q25 Returned Terminal casefile*/
FUNCTION fCreateDocumentCase9  RETURNS CHAR
   (idStartTS AS DECIMAL,
   idEndTS AS DECIMAL):
   DEF VAR lcCasefileRow  AS CHAR NO-UNDO.   
   DEF VAR lcCaseTypeId   AS CHAR NO-UNDO.

   FOR EACH TermReturn NO-LOCK WHERE
           (TermReturn.ReturnTS < idEndTS AND
            TermReturn.ReturnTS >= idStartTS) AND
          ((TermReturn.DeviceScreen = TRUE AND
            TermReturn.DeviceStart = TRUE) OR
           (TermReturn.DeviceScreen = ? AND
            TermReturn.DeviceStart  = ?)) AND
            TermReturn.ContractID NE "" :

       /*ContractID*/
      ASSIGN
         lcCaseTypeId = "9"
         lcCaseFileRow = lcCaseTypeId + lcDelim +
                         /*ContractId*/
                         TermReturn.ContractID + lcDelim +
                         /*Salesman*/
                         TermReturn.Salesman + lcDelim +
                         /*MSisDN*/
                         TermReturn.MSISDN + lcDelim +
                         /*Terminal Request Date*/
                         fPrintDate(TermReturn.ReturnTS) .

      OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
      PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
      OUTPUT STREAM sOutFile CLOSE.

      fLogLine(lcCaseFileRow, "").
   END.
   RETURN "".  
END.
/*q25 case file 10 Q25 extensions*/
FUNCTION fCreateDocumentCase10 RETURNS CHAR
   (idStartTS AS DECIMAL,
    idEndTS AS DECIMAL):
   DEF VAR lcCaseTypeId     AS CHAR NO-UNDO.
   DEF VAR lcCasefileRow    AS CHAR NO-UNDO.
   DEF VAR lcStatuses AS CHAR NO-UNDO.

   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE liStat AS INTEGER NO-UNDO.

   lcStatuses = {&REQ_ONGOING_STATUSES} + ",2".
   do i = 1 to NUM-ENTRIES(lcStatuses):
      liStat = INT(ENTRY(i,lcStatuses)).
      FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand EQ Syst.Var:gcBrand AND
            MsRequest.ReqType EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
            MsRequest.ReqStatus eq liStat AND
            MsRequest.ActStamp >= idStartTS AND
            MsRequest.CreStamp >= idStartTS AND
            MsRequest.CreStamp < idEndTS AND
            MsRequest.ReqCparam3 EQ "RVTERM12":
         IF NOT MsRequest.UserCode BEGINS "POS_" THEN NEXT.
         /*Document type,DocStatusCode,RevisionComment*/
         ASSIGN
         lcCaseTypeID   = '10'
         lcCaseFileRow =
                      lcCaseTypeID                    + lcDelim +
                      /*Contract_ID*/
                      STRING(MsRequest.ReqCparam4)    + lcDelim +
                      /*SFID*/
                      fCutChannel(MsRequest.UserCode) + lcDelim +
                      /*MSISDN*/
                      STRING(MsRequest.CLI)           + lcDelim +
                      /*Q25 Extension_Request_date*/
                      fPrintDate(MsRequest.CreStamp)      + lcDelim +
                      /*Q25 Extension bank*/
                      STRING(Msrequest.ReqCparam6).

                     
         OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
         PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
         OUTPUT STREAM sOutFile CLOSE.

        fLogLine(lcCaseFileRow, "").

      END.
   END.
   RETURN "".

END.

/* {14} TeleSales */
FUNCTION fCreateDocumentCase14 RETURNS CHARACTER
   (iiOrderId AS INT):
   DEFINE VARIABLE lcCaseTypeID        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE lcCasefileRow       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE lcCreateDMS         AS CHARACTER   NO-UNDO.

   FIND FIRST Order NO-LOCK WHERE 
              Order.Brand = Syst.Var:gcBrand  AND
              Order.OrderID EQ iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN RETURN "14:Order not available" + STRING(iiOrderId).

   FIND FIRST OrderCustomer NO-LOCK  WHERE
              OrderCustomer.Brand EQ Syst.Var:gcBrand AND
              OrderCustomer.OrderID EQ iiOrderID      AND
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN
      RETURN "14:Ordercustomer not available" + STRING(iiOrderId).
   
   ASSIGN
      lcCaseTypeId  = "14"
      lcCaseFileRow =
         lcCaseTypeId                     + lcDelim +
         /*Contract_ID : EB5CB2*/
         Order.ContractID                 + lcDelim +
         /*Order_ID : 14566933*/
         STRING(Order.OrderID)            + lcDelim +
         /* Brand */ /* Hard coding to be removed using some function to get Brand */
         "YOI"                            + lcDelim +
         /* Convergence */
         fGetTarrifType(Order.CliType)    + lcDelim +
         /*MSISDN : 609321999*/
         Order.CLI                        + lcDelim +
         /* Fixed Number */
         fGetFixNumber(Order.OrderID)     + lcDelim +
         /*SFID: WEB*/
         Order.Salesman                   + lcDelim +
         /* DNI */
         OrderCustomer.CustID             + lcDelim +
         /*Order_date : 23-04-2015*/
         fPrintDate(Order.CrStamp)        + lcDelim +
         /* ReplacedContract : Always empty*/
         ""
         .

   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
   OUTPUT STREAM sOutFile CLOSE.

   lcCreateDMS = fUpdateDMS("", /*DmsExternalID*/
                            lcCaseTypeID,
                            Order.ContractID,
                            {&DMS_HOST_TABLE_ORDER},
                            Order.OrderId,
                            lcInitStatus,/*StatusCode*/
                            lcDMSStatusDesc,
                            Order.StatusCode,
                            0,
                            "" /*DocList*/,
                            {&DMS_DOCLIST_SEP}).
   fLogLine(lcCaseFileRow,lcCreateDMS).

   RETURN "".

END.

/*Customer category change*/
FUNCTION fCreateDocumentCase15 RETURNS CHAR
   (idStartTS AS DECIMAL,
    idEndTS AS DECIMAL):
   DEF VAR lcCaseTypeId     AS CHAR NO-UNDO.
   DEF VAR lcCasefileRow    AS CHAR NO-UNDO.
   DEF VAR lcStatuses       AS CHAR NO-UNDO.
   DEF VAR lcRequiredDocs   AS CHAR NO-UNDO.
   DEF VAR liCount          AS INT NO-UNDO.
   DEF VAR lcDocListEntries AS CHAR NO-UNDO.

    FOR EACH MsRequest EXCLUSIVE-LOCK WHERE
        MsRequest.Brand EQ Syst.Var:gcBrand AND
        MsRequest.ReqType EQ {&REQTYPE_CATEGORY_CHG} AND
        MsRequest.ReqStatus EQ {&REQUEST_STATUS_NEW} AND
        MsRequest.ActStamp >= idStartTS AND
        MsRequest.CreStamp >= idStartTS AND
        MsRequest.CreStamp < idEndTS :
     /*Document type,DocStatusCode,RevisionComment*/
        FIND Customer WHERE Customer.Brand = Syst.Var:gcBrand  AND Customer.CustNum = MSRequest.CustNum NO-LOCK NO-ERROR.
      
        ASSIGN
            lcCaseTypeID   = '15'
            lcCaseFileRow  =
                           lcCaseTypeID                    + lcDelim +   /* Case ID */
                           fPrintDate(MsRequest.CreStamp)  + lcDelim +   /* Request create date */
                           Customer.CustIdType             + lcDelim +   /* Customer ID Type */
                           MSRequest.ReqCParam2 + " - " + MSRequest.ReqCParam1  + lcDelim +  /* From - To */
                           fCutChannel(MsRequest.UserCode) + lcDelim +   /* SFID */
                           MSRequest.CLI                   + lcDelim +   /* MSISDN */
                           STRING(MSRequest.MsRequest).                  /* Request number */

        MSRequest.ReqStatus = {&REQUEST_STATUS_UNDER_WORK} .

        /* solve needed documents: */
        lcRequiredDocs = fNeededDocsCategoryChange().
        DO liCount = 1 TO NUM-ENTRIES(lcRequiredDocs):
           /* Document type, Type desc,DocStatusCode,RevisionComment */
           lcDocListEntries = lcDocListEntries +
                         ENTRY(liCount,lcRequiredDocs) + {&DMS_DOCLIST_SEP} +
                         {&DMS_DOCLIST_SEP} + /* filled only by DMS responses */
                         lcDMSDOCStatus + {&DMS_DOCLIST_SEP} +
                         "".
         IF liCount NE NUM-ENTRIES(lcRequiredDocs)
            THEN lcDocListEntries = lcDocListEntries + {&DMS_DOCLIST_SEP}.
        END.
       
        fUpdateDMS('',
                   lcCaseTypeID,
                   STRING(MSRequest.MsRequest),
                   "MsRequest",
                   MSRequest.MsRequest,
                   lcInitStatus,
                   lcDMSStatusDesc,
                   "",
                   MsRequest.CreStamp,
                   lcDocListEntries,
                   {&DMS_DOCLIST_SEP}).          
        
        OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
        PUT STREAM sOutFile UNFORMATTED lcCaseFileRow SKIP.
        OUTPUT STREAM sOutFile CLOSE.

        fLogLine(lcCaseFileRow, "").

    END.
    RETURN "".

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
            lcStatus = fCreateDocumentCase6(ttOrderList.OrderID,ttOrderList.MsRequest).
            IF lcStatus NE "" THEN fLogLine("",lcStatus).
         END.
      END.
      WHEN {&DMS_CASE_TYPE_ID_Q25_TERM_RETURN} THEN DO:
         /*From MsRequest*/
         lcStatus = fCreateDocumentCase9(idPeriodStart, idPeriodEnd).
      END.
      WHEN {&DMS_CASE_TYPE_ID_Q25_STE} THEN DO:
         /*From MsRequest*/
         lcStatus = fCreateDocumentCase10(idPeriodStart, idPeriodEnd).
      END.
      WHEN {&DMS_CASE_TYPE_ID_TELESALES} THEN DO:
         FOR EACH ttOrderList WHERE
                  ttOrderList.TeleSales EQ TRUE:
            lcStatus = fCreateDocumentCase14(ttOrderList.OrderID).
            IF lcStatus NE "" THEN fLogLine("",lcStatus).
         END.
      END.
      WHEN {&DMS_CASE_TYPE_ID_CATEGORY_CHG} THEN DO:
          lcStatus = fCreateDocumentCase15(idPeriodStart, idPeriodEnd).
      END.

   END. /*Case*/
   OUTPUT STREAM sOutFile to VALUE(icOutFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED "" SKIP.
   OUTPUT STREAM sOutFile CLOSE.

END.

/*Main functionality*/
OUTPUT STREAM sLogFile TO VALUE(icLogFile) APPEND.

ldCurrentTime = Func.Common:mMakeTS().

fLogLine("","DMS Casefile creation starts " + Func.Common:mTS2HMS(ldCurrentTime)).
fLogLine("", "Collection period: " + 
         STRING(idPeriodStart) + " " + Func.Common:mTS2HMS(idPeriodStart) + " - " + 
         STRING(idPeriodEnd) + " " + Func.Common:mTS2HMS(idPeriodEnd) ).

/* Create temp table to ensure that multiple order changes 
   do not produce extra documents. Only 1 doc/order is provided. */
fMakeTempTable(icCases, idPeriodStart, idPeriodEnd).
/*Create data files for requested types.*/
DO liCaseCount = 1 TO NUM-ENTRIES(icCases):
   lcStatus = fCreateDocumentRows(ENTRY(liCaseCount,icCases)).
END.

ldCurrentTime = Func.Common:mMakeTS().
fLogLine("","DMS Casefile creation ends " + Func.Common:mTS2HMS(ldCurrentTime)).
OUTPUT STREAM sLogFile CLOSE.
