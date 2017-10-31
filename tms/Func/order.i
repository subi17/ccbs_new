/* order.i      2006/jp

   changes:     08.02.06/aam create user account for agr.customer
                14.02.06/aam assign firstname
                20.11.06/aam new db structure
*/
{Syst/commali.i}   
{Func/fwebuser.i}
{Func/fcustdata.i}
{Func/ftmrlimit.i}
{Syst/tmsconst.i}
{Syst/eventval.i}
{Func/femailinvoice.i}
{Func/profunc.i}
{Func/custfunc.i}
{Func/log.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {Func/lib/eventlog.i}
END.

DEF BUFFER bUpdOrderCustomer FOR OrderCustomer.


FUNCTION fInvGroup RETURN CHARACTER
   (INPUT idtDate AS DATE).

   DEF VAR lcGroup AS CHAR NO-UNDO.
   
   IF      day(idtDate) < 5  THEN lcGroup = "INV1".
   ELSE if day(idtDate) < 9  THEN lcGroup = "INV2".
   ELSE if day(idtDate) < 12 THEN lcGroup = "INV3".
   ELSE if day(idtDate) < 16 THEN lcGroup = "INV4".      
   ELSE if day(idtDate) < 20 THEN lcGroup = "INV5".
   ELSE if day(idtDate) < 24 THEN lcGroup = "INV6".
   ELSE if day(idtDate) < 28 THEN lcGroup = "INV7".
   ELSE  lcGroup = "INV8".

   RETURN lcGroup.            

END.
   
FUNCTION fUpdateEmail RETURNS LOGICAL
   (INPUT iiOrder AS INT):
   
   DEF VAR liRequest AS INT NO-UNDO. 
   DEF VAR lcResult AS CHAR NO-UNDO. 

   IF OrderCustomer.Email > "" AND
      Customer.Email <> OrderCustomer.Email THEN DO:
      Customer.Email = OrderCustomer.EMail.

      IF Customer.DelType EQ {&INV_DEL_TYPE_EMAIL} OR
         Customer.DelType EQ {&INV_DEL_TYPE_EMAIL_PENDING} THEN DO:

         /* Cancel Ongoing Email Activation Request and create new */
         IF fPendingEmailActRequest(INPUT Customer.Custnum) THEN
            fCancelPendingEmailActRequest(
                          INPUT Customer.Custnum,
                          INPUT "Customer email address is changed").

         liRequest = fEmailInvoiceRequest(INPUT Func.Common:mMakeTS(),
                                          INPUT TODAY,
                                          INPUT katun,
                                          INPUT 0, /* msseq */
                                          INPUT "", /* cli */
                                          INPUT Customer.CustNum,
                                          INPUT {&REQUEST_SOURCE_MANUAL_TMS},
                                          INPUT Customer.Email,
                                          INPUT iiOrder,
                                          OUTPUT lcResult).
         IF liRequest > 0 THEN DO:
            /* If Email already validated then mark DelType EMAIL */
            IF liRequest = 1 THEN
               Customer.DelType = {&INV_DEL_TYPE_EMAIL}.
            ELSE
               Customer.DelType = {&INV_DEL_TYPE_EMAIL_PENDING}.
         END. /* IF liRequest > 0 THEN DO: */
      END. /* IF Customer.DelType EQ {&INV_DEL_TYPE_EMAIL} */

      FIND FIRST InvoiceTargetGroup WHERE
                 InvoiceTargetGroup.CustNum = Customer.CustNum AND
                 InvoiceTargetGroup.ToDate >= TODAY AND
                (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
                 InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL InvoiceTargetGroup THEN DO:
         /* If request is not already created from Customer DelType */
         IF liRequest = 0 THEN DO:
            /* Cancel existing ReqType=84 request */
            IF fPendingEmailActRequest(INPUT Customer.Custnum) THEN
               fCancelPendingEmailActRequest(
                                INPUT Customer.Custnum,
                                INPUT "Customer email address is changed").

            liRequest = fEmailInvoiceRequest(INPUT Func.Common:mMakeTS(),
                                             INPUT TODAY,
                                             INPUT katun,
                                             INPUT 0, /* msseq */
                                             INPUT "", /* cli */
                                             INPUT Customer.CustNum,
                                             INPUT {&REQUEST_SOURCE_FUSION_EMAIL},
                                             INPUT Customer.Email,
                                             INPUT iiOrder,
                                             OUTPUT lcResult).
         END. /* IF liRequest = 0 THEN DO: */
         IF liRequest > 0 THEN DO:
            /* If Email already validated then mark DelType EMAIL */
            IF liRequest = 1 THEN
               InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL}.
            ELSE
               InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}.
         END. /* IF liRequest > 0 THEN DO: */
         RELEASE InvoiceTargetGroup.
       END. /* IF AVAIL InvoiceTargetGroup THEN DO: */
   END. /* IF Customer.Email <> OrderCustomer.Email THEN DO: */
END.

FUNCTION fUpdEmailDelType RETURNS LOGICAL
   (INPUT iiOrder   AS INT):

   DEF VAR liRequest     AS INT  NO-UNDO.
   DEF VAR lcResult      AS CHAR NO-UNDO.
   DEF VAR llEmailChange AS LOG  NO-UNDO.

   IF OrderCustomer.DelType > 0 AND
      OrderCustomer.DelType <> Customer.DelType THEN DO:

      IF OrderCustomer.DelType = {&INV_DEL_TYPE_EMAIL} AND
         Customer.DelType = {&INV_DEL_TYPE_EMAIL_PENDING} THEN .
      ELSE DO:
         Customer.DelType = OrderCustomer.DelType.

         IF Customer.DelType EQ {&INV_DEL_TYPE_EMAIL} THEN DO:

            IF OrderCustomer.Email > "" AND
               Customer.Email <> OrderCustomer.Email THEN
               ASSIGN Customer.Email = OrderCustomer.EMail
                      llEmailChange  = TRUE.

            liRequest = fEmailInvoiceRequest(INPUT Func.Common:mMakeTS(),
                                             INPUT TODAY,
                                             INPUT katun,
                                             INPUT 0, /* msseq */
                                             INPUT "", /* cli */
                                             INPUT Customer.CustNum,
                                             INPUT {&REQUEST_SOURCE_MANUAL_TMS},
                                             INPUT Customer.Email,
                                             INPUT iiOrder,
                                             OUTPUT lcResult).

            /* If Email already validated then mark DelType EMAIL */
            IF liRequest = 1 THEN
               Customer.DelType = {&INV_DEL_TYPE_EMAIL}.
            ELSE
               Customer.DelType = {&INV_DEL_TYPE_EMAIL_PENDING}.

            IF llEmailChange = TRUE THEN
               FIND FIRST InvoiceTargetGroup WHERE
                          InvoiceTargetGroup.CustNum = Customer.CustNum AND
                          InvoiceTargetGroup.ToDate >= TODAY AND
                         (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
                          InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
                    EXCLUSIVE-LOCK NO-ERROR.
               IF AVAIL InvoiceTargetGroup THEN DO:
                  IF liRequest = 1 THEN
                     InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL}.
                  ELSE
                     InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}.
               END. /* IF AVAIL InvoiceTargetGroup THEN DO: */

         END. /* ELSE IF Customer.DelType = {&INV_DEL_TYPE_EMAIL} */
         /* Cancel Ongoing Email Activation Request */
         ELSE DO:
            FIND FIRST InvoiceTargetGroup WHERE
                       InvoiceTargetGroup.CustNum = Customer.CustNum AND
                       InvoiceTargetGroup.ToDate >= TODAY AND
                      (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
                       InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
                 NO-LOCK NO-ERROR.
            IF NOT AVAIL InvoiceTargetGroup AND
               fPendingEmailActRequest(INPUT Customer.Custnum) THEN
               fCancelPendingEmailActRequest(
                      INPUT Customer.Custnum,
                      INPUT "Customer invoice delivery type is changed").
         END. /* IF Customer.DelType <> {&INV_DEL_TYPE_EMAIL} */
      END. /* ELSE DO: */
   END. /* IF OrderCustomer.DelType <> Customer.DelType THEN DO: */

   fUpdateEmail(iiOrder).

END. /* FUNCTION fUpdEmailDelType */

FUNCTION fClosePendingACC RETURNS LOGICAL
    (INPUT icCloseType      AS CHARACTER,
     INPUT icCustomerIdType AS CHARACTER,
     INPUT icCustomerId     AS CHARACTER,
     INPUT iiOrder          AS INTEGER):

    DEFINE BUFFER bf_MsRequest FOR MsRequest.
    DEFINE BUFFER bf_Customer  FOR Customer.
    DEFINE BUFFER bf_CustCat   FOR CustCat.

    DEF VARIABLE ldeCurrentTime AS DECIMAL NO-UNDO.

    ASSIGN ldeCurrentTime = Func.Common:mMakeTS().

    FOR EACH bf_MsRequest WHERE bf_MsRequest.Brand     = Syst.CUICommon:gcBrand                                      AND 
                                bf_MsRequest.ReqType   = {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE}         AND 
                                LOOKUP(STRING(bf_MsRequest.ReqStatus), {&REQ_INACTIVE_STATUSES}) = 0 AND 
                                bf_MsRequest.ActStamp >= ldeCurrentTime                               NO-LOCK:

        IF ENTRY(12,bf_MsRequest.ReqCParam1,";") = "" OR 
           ENTRY(13,bf_MsRequest.ReqCParam1,";") = "" THEN
           NEXT.

        IF ENTRY(12,bf_MsRequest.ReqCParam1,";") = icCustomerIdType AND 
           ENTRY(13,bf_MsRequest.ReqCParam1,";") = icCustomerId     THEN
        DO:
            FIND FIRST bf_Customer WHERE bf_Customer.CustNum = bf_MsRequest.CustNum NO-LOCK NO-ERROR.
            IF AVAIL bf_Customer THEN
            DO:
                FIND FIRST bf_CustCat WHERE bf_CustCat.Brand = Syst.CUICommon:gcBrand AND bf_CustCat.Category = bf_Customer.Category NO-LOCK NO-ERROR.
                IF AVAIL bf_CustCat THEN 
                DO:
                    IF icCloseType = "Pro" THEN 
                    DO:
                        IF bf_CustCat.Pro = FALSE THEN 
                            NEXT.
                    END.
                    ELSE /* Non-pro */ 
                    DO:
                        IF bf_CustCat.Pro = TRUE THEN 
                            NEXT.
                    END.

                    BUFFER bf_MsRequest:FIND-CURRENT(EXCLUSIVE-LOCK, NO-WAIT).
                    IF NOT AVAIL bf_MsRequest THEN 
                    DO:
                        fLog("Order.i:fClosePendingACC: Record bf_MsRequest not available for update" ,katun).
                        NEXT.
                    END.

                    ASSIGN 
                        bf_MsRequest.ReqStatus   = {&REQUEST_STATUS_CANCELLED}
                        bf_MsRequest.UpdateStamp = Func.Common:mMakeTS()
                        bf_MsRequest.DoneStamp   = Func.Common:mMakeTS()
                        bf_MsRequest.Memo        = bf_MsRequest.Memo + 
                                                   (IF bf_MsRequest.Memo > "" THEN ", " ELSE "") + 
                                                   "Non-pro order#" + STRING(iiOrder) + " for ACCed customer is handled. That means ACCed customer " + 
                                                   "has been added as Non-pro too system, so this pending ACC request is not valid anymore".
                END.
            END.    
        END.   
           
    END.
    RELEASE bf_MsRequest.
    
    RETURN TRUE.

END FUNCTION.

FUNCTION fMakeCustomer RETURNS LOGICAL
  (INPUT iiOrder   AS INT,
   INPUT iiTarget  AS INT,
   INPUT ilNewCust AS LOG,
   INPUT iiCustNum AS INT,
   INPUT ilCleanLogs AS LOG):

   DEF BUFFER AgrCust  FOR Customer.
   DEF BUFFER InvCust  FOR Customer.
   DEF BUFFER UserCust FOR Customer.

   DEF VAR lcCategory AS CHAR NO-UNDO.

   FIND Customer WHERE
        Customer.CustNum = iiCustNum EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN RETURN FALSE.     
   
   /* iiTarget 1 = agr.cust
               2 = inv.cust
               3 = user cust
   */

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand   = Syst.CUICommon:gcBrand AND
              Order.OrderId = iiOrder
              NO-ERROR.
   IF NOT AVAILABLE Order THEN RETURN FALSE. 

   FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
              OrderCustomer.Brand   = Syst.CUICommon:gcBrand AND
              OrderCustomer.OrderID = iiOrder AND
              OrderCustomer.RowType = iiTarget NO-ERROR.
   IF NOT AVAILABLE OrderCustomer THEN RETURN FALSE. 

   ASSIGN
                         
   Customer.CustName     = TRIM(OrderCustomer.Surname1)
   Customer.SurName2     = TRIM(OrderCustomer.SurName2)
   Customer.CompanyName  = TRIM(OrderCustomer.Company) WHEN
                           OrderCustomer.CustIDType EQ "CIF"
   Customer.CompanyName  = TRIM(OrderCustomer.Company) WHEN
                           OrderCustomer.CustIDType NE "CIF" AND
                           TRIM(OrderCustomer.Company) > ""
   Customer.Profession   = TRIM(OrderCustomer.Profession) WHEN
                           TRIM(OrderCustomer.Profession) > ""
   Customer.HonTitle     = OrderCustomer.CustTitle
   Customer.FirstName    = TRIM(OrderCustomer.FirstName)
   Customer.SearchName   = SUBSTR(Customer.CustName + 
                                  Customer.FirstName,1,8)
   Customer.Address      = OrderCustomer.Address
   Customer.ZipCode      = OrderCustomer.ZipCode
   Customer.PostOffice   = OrderCustomer.PostOffice
   Customer.Country      = OrderCustomer.Country
   Customer.BankAcc      = OrderCustomer.BankCode WHEN
      LOOKUP(Order.OrderChannel,"renewal_pos_stc,retention_stc") = 0
   Customer.Region       = OrderCustomer.Region
   Customer.Nationality  = OrderCustomer.Nationality
   Customer.Language     = INTEGER(OrderCustomer.Language)
   Customer.Phone        = OrderCustomer.FixedNumber
   Customer.SMSNumber    = OrderCustomer.MobileNumber
   Customer.CustIDType   = OrderCustomer.CustIDType
   Customer.OrgId        = OrderCustomer.CustId
   Customer.BirthDay     = OrderCustomer.BirthDay
   Customer.Sex          = OrderCustomer.Sex
   
   Customer.ExtInvRef    = OrderCustomer.ExtInvRef
   Customer.FoundationDate = OrderCustomer.FoundationDate

   /* marketing values */
   Customer.DirMarkSMS   = OrderCustomer.OperSMSMarketing
   Customer.DirMarkEmail = OrderCustomer.OperEMailMarketing
   Customer.DirMarkPOST  = OrderCustomer.OperPostMarketing
   Customer.OutMarkSMS   = OrderCustomer.OutSMSMarketing
   Customer.OutMarkEmail = OrderCustomer.OutEMailMarketing
   Customer.OutMarkPOST  = OrderCustomer.OutPostMarketing
   Customer.OutMarkBank  = OrderCustomer.OutBankMarketing
   Customer.DontSharePersData = OrderCustomer.DontSharePersData.

   IF OrderCustomer.Rowtype = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} AND
      OrderCustomer.CustIdType = "CIF"
   THEN ASSIGN
           Customer.AuthCustId     = OrderCustomer.AuthCustId
           Customer.AuthCustIdType = OrderCustomer.AuthCustIdType
           .

   /* Electronic Invoice Project - update email and delivery type */
   fUpdEmailDelType(iiorder).
   
   IF OrderCustomer.AddressCodP > "" OR
      OrderCustomer.AddressCodC > "" OR
      OrderCustomer.AddressCodM > "" THEN DO:

      FIND FIRST CustomerReport WHERE
                 CustomerReport.Custnum = Customer.Custnum
      EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL CustomerReport THEN CREATE CustomerReport.
      ASSIGN
         CustomerReport.Custnum = Customer.Custnum
         CustomerReport.StreetCode = OrderCustomer.AddressCodC
         CustomerReport.CityCode = OrderCustomer.AddressCodP
         CustomerReport.TownCode = OrderCustomer.AddressCodM.
   END.
  
   IF iiTarget = 1 THEN
      Customer.InvGroup = fDefInvGroup(OrderCustomer.Region).
   
   IF ilNewCust THEN DO:
      
      IF llDoEvent THEN DO:
         DEFINE VARIABLE lhOrderCustomer AS HANDLE NO-UNDO.
         lhOrderCustomer = BUFFER OrderCustomer:HANDLE.
         RUN StarEventInitialize(lhOrderCustomer).
         RUN StarEventSetOldBuffer ( lhOrderCustomer ).
      END.
      
      ASSIGN
         Customer.InvoiceTargetRule = {&INVOICE_TARGET_RULE_DEFAULT_GROUP}
         OrderCustomer.CustNum = iiCustNum
         OrderCustomer.PersonID = "NEW" WHEN OrderCustomer.PersonID EQ ""
         Customer.InvCode      = (IF DAY(today) > 15 THEN 12 ELSE 11).
      
      IF llDoEvent THEN DO:
         RUN StarEventMakeModifyEvent ( lhOrderCustomer ).
         IF ilCleanLogs THEN fCleanEventObjects(). 
      END.

      fgetCustSegment(OrderCustomer.CustIDType, 
                      OrderCustomer.SelfEmployed,
                      OrderCustomer.pro, 
                      OUTPUT lcCategory).

      IF lcCategory > "" THEN 
      DO:
         ASSIGN Customer.Category = lcCategory.

         IF Ordercustomer.rowtype EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} THEN
            Ordercustomer.Category = lcCategory.
      END.
      ELSE  /* category according to id type */
         ASSIGN Customer.Category = OrderCustomer.Category.         

      IF NOT OrderCustomer.Pro THEN
         fClosePendingACC("Pro", Customer.CustIdType, Customer.OrgId, iiOrder).
      ELSE 
         fClosePendingACC("Non-Pro", Customer.CustIdType, Customer.OrgId, iiOrder).   

      IF iiTarget = 1 THEN DO:
         /* new user account */
         create_account(Customer.CustNum,?,?).
      END.   
         
      ELSE DO:

         FOR FIRST bUpdOrderCustomer NO-LOCK WHERE
                   bUpdOrderCustomer.Brand   = Syst.CUICommon:gcBrand AND
                   bUpdOrderCustomer.OrderID = iiOrder AND
                   bUpdOrderCustomer.RowType = 1,
             FIRST AgrCust NO-LOCK WHERE
                   AgrCust.CustNum = bUpdOrderCustomer.CustNum:
            ASSIGN Customer.InvGroup = AgrCust.InvGroup
                   Customer.AgrCust  = AgrCust.CustNum.
            
            /* user */            
            IF iiTarget = 3 THEN Customer.InvCust = AgrCust.CustNum.
         END.

         IF Customer.InvGroup = "" THEN    
            Customer.InvGroup = fDefInvGroup(OrderCustomer.Region).
      END.
      
      /* default counter limits; for all, also prepaids */
      fTMRLimit2Customer(Customer.CustNum).
   END.
   
   RETURN TRUE.

END.

FUNCTION fGetTerminalOfferItemId RETURN INTEGER
  (INPUT pcOffer AS CHARACTER, 
   INPUT pcGroup AS CHARACTER,
   INPUT pdeTime AS DECIMAL):

   DEFINE VARIABLE iOfferItemId AS INTEGER NO-UNDO. 
   iOfferItemId = -1.

   DEFINE BUFFER xOfferItem FOR OfferItem.
   DEFINE BUFFER xBillItem  FOR BillItem.

   LoopItems:
   FOR EACH xOfferItem WHERE 
      xOfferItem.Brand = Syst.CUICommon:gcBrand  AND
      xOfferItem.Offer     = pcOffer AND 
      xOfferItem.ItemType  = "BillItem" AND
      xOfferItem.EndStamp   >= pdeTime AND
      xOfferItem.BeginStamp <= pdeTime NO-LOCK:
      FIND xBillItem WHERE 
           xBillItem.Brand = Syst.CUICommon:gcBrand AND 
           xBillItem.BillCode = xOfferItem.ItemKey AND
           xBillItem.BIGroup = pcGroup NO-LOCK NO-ERROR.
      IF AVAIL xBillItem THEN DO:
         iOfferItemId = xOfferItem.OfferItemId.
         LEAVE LoopItems.
      END.
   END.
   RETURN iOfferItemId.
END.


