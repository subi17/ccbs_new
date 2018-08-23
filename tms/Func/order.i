/* order.i      2006/jp

   changes:     08.02.06/aam create user account for agr.customer
                14.02.06/aam assign firstname
                20.11.06/aam new db structure
*/
&IF "{&order_i}" NE "YES"
&THEN

&GLOBAL-DEFINE order_i YES

{Func/fwebuser.i}
{Func/fcustdata.i}
{Func/ftmrlimit.i}
{Syst/tmsconst.i}
{Syst/eventval.i}
{Func/femailinvoice.i}
{Func/profunc.i}
{Func/custfunc.i}
{Func/log.i}
{Func/msreqfunc.i}
{Func/orderfunc.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
END.


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
                                          INPUT Syst.Var:katun,
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
                                             INPUT Syst.Var:katun,
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
                                             INPUT Syst.Var:katun,
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
    DEFINE BUFFER OrderCustomer FOR OrderCustomer.
    DEFINE BUFFER Order FOR Order.

    DEF VARIABLE ldeCurrentTime AS DECIMAL NO-UNDO.
    DEF VAR liCount AS INT NO-UNDO.
    DEF VAR liLoop AS INT NO-UNDO.
    DEF VAR llISPro AS LOG NO-UNDO. 

    ASSIGN ldeCurrentTime = Func.Common:mMakeTS()
           licount = NUM-ENTRIES({&REQ_ONGOING_STATUSES}).

   DO liLoop = 1 TO licount:
      FOR EACH bf_MsRequest WHERE
               bf_MsRequest.Brand     = Syst.Var:gcBrand                             AND
               bf_MsRequest.ReqType   = {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE}         AND
               bf_MsRequest.ReqStatus = INT(ENTRY(liLoop,({&REQ_ONGOING_STATUSES}))) and
               bf_MsRequest.ActStamp >= ldeCurrentTime                           NO-LOCK: 

            IF bf_MsRequest.ReqIParam4 > 0 THEN DO:
               FIND OrderCustomer NO-LOCK WHERE
                    OrderCustomer.Brand = Syst.Var:gcBrand AND
                    OrderCustomer.OrderID = bf_MsRequest.ReqIParam4 AND
                    OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_ACC} AND
                    OrderCustomer.CustIDType = icCustomerIDType AND
                    OrderCustomer.CustID = icCustomerID NO-ERROR.
               IF NOT AVAIL OrderCustomer THEN NEXT.
            END.
            ELSE DO:
               IF NOT (ENTRY(12,bf_MsRequest.ReqCParam1,";") EQ icCustomerIDType AND
                       ENTRY(13,bf_MsRequest.ReqCParam1,";") EQ icCustomerID) THEN NEXT.
            END.

            FIND FIRST bf_Customer WHERE bf_Customer.CustNum = bf_MsRequest.CustNum NO-LOCK NO-ERROR.
            IF NOT AVAIL bf_Customer THEN NEXT.
            
            llISPro = fIsPro(bf_Customer.Category).

            IF (icCloseType = "Pro" AND llISPro = FALSE) OR
               (icCloseType NE "Pro" AND llISPro = TRUE) THEN NEXT.
                                           
            IF NOT fChangeReqStatus(bf_MsRequest.MsRequest,
                        {&REQUEST_STATUS_CANCELLED},
                       ("Non-pro order#" + STRING(iiOrder) + " for ACCed customer is handled. That means ACCed customer " + 
                        "has been added as Non-pro too system, so this pending ACC request is not valid anymore")) THEN DO:
                fLog("Order.i:fClosePendingACC: Record bf_MsRequest not available for update" , Syst.Var:katun).
                NEXT.
            END.

            IF bf_MsRequest.ReqIParam4 > 0 THEN DO:
               FIND Order NO-LOCK WHERE
                    Order.Brand = Syst.Var:gcBrand AND
                    Order.OrderID = bf_MsRequest.ReqIParam4 AND
                    Order.OrderType = {&ORDER_TYPE_ACC} AND
                    Order.StatusCode = {&ORDER_STATUS_ONGOING} NO-ERROR.
               IF AVAIL Order THEN fSetOrderStatus(Order.OrderID,{&ORDER_STATUS_CLOSED}).
            END.

       END.
    END.
    
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
   DEF BUFFER bUpdOrderCustomer FOR OrderCustomer.

   DEF VAR lcCategory AS CHAR NO-UNDO.

   FIND Customer WHERE
        Customer.CustNum = iiCustNum EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN RETURN FALSE.     
   
   /* iiTarget 1 = agr.cust
               2 = inv.cust
               3 = user cust
   */

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand   = Syst.Var:gcBrand AND
              Order.OrderId = iiOrder
              NO-ERROR.
   IF NOT AVAILABLE Order THEN RETURN FALSE. 

   FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
              OrderCustomer.Brand   = Syst.Var:gcBrand AND
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
                      OrderCustomer.custid, /* YDR-2621 */
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
                   bUpdOrderCustomer.Brand   = Syst.Var:gcBrand AND
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

/*
   Function updates latest convergent/fixed order install address
   to Customer table. 
   Preconditions: Invoice delivery method is other than Paper and 
   Fixed line has been successfully installed. Company customers 
   Contact person address is not updated.
   Project: FIAD-1 Batch to update billing address in TMS.
*/
FUNCTION fUpdateCustomerInstAddr RETURNS LOGICAL
   (INPUT iiOrder AS INT):

   DEF VAR lcNewAddress AS CHAR NO-UNDO.
   DEF VAR lcregion AS CHAR NO-UNDO.
   DEF VAR lcInvGroup AS CHAR NO-UNDO.

   DEF BUFFER bCustomer       FOR Customer.
   DEF BUFFER bOrder          FOR Order.
   DEF BUFFER bOrderComp      FOR Order.
   DEF BUFFER bOrderCustomer  FOR OrderCustomer.
   DEF BUFFER bOrderFusion    FOR OrderFusion.

   FIND FIRST bOrder NO-LOCK WHERE
              bOrder.Brand EQ Syst.Var:gcBrand AND
              bOrder.OrderId EQ iiOrder NO-ERROR.
   IF NOT AVAIL bOrder THEN RETURN FALSE.

   FIND FIRST bCustomer NO-LOCK WHERE
              bCustomer.CustNum EQ bOrder.CustNum NO-ERROR.
   IF NOT AVAIL bCustomer THEN RETURN FALSE.

   IF bCustomer.DelType EQ 1 THEN RETURN FALSE. /* Type Paper */

   /* Update adress if this is latest order if several convergent orders */
   FOR EACH bOrderComp NO-LOCK USE-INDEX CustNum WHERE
            bOrderComp.Brand EQ Syst.Var:gcBrand AND
            bOrderComp.CustNum EQ bCustomer.CustNum AND
            bOrderComp.CrStamp > bOrder.CrStamp AND
            bOrderComp.OrderId NE bOrder.OrderId:
      IF NOT fIsConvergentORFixedOnly(bOrderComp.CliType) THEN NEXT.

      FIND FIRST bOrderFusion NO-LOCK WHERE
            borderfusion.Brand EQ Syst.Var:gcBrand AND
            borderfusion.orderid = bOrderComp.Orderid AND
            bOrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_FINALIZED} NO-ERROR.
      IF AVAIL bOrderFusion THEN RETURN FALSE.
   END.
   /* Update billing address from installation address */
   FIND FIRST bOrderCustomer NO-LOCK WHERE
              bOrderCustomer.Brand   EQ Syst.Var:gcBrand AND
              bOrderCustomer.OrderId EQ bOrder.OrderId AND
              bOrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL} NO-ERROR.
   IF NOT AVAIL bOrderCustomer THEN RETURN FALSE.

   FIND FIRST bOrderFusion NO-LOCK WHERE
              borderfusion.Brand EQ Syst.Var:gcBrand AND
              borderfusion.orderid = bOrder.Orderid AND
              bOrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_FINALIZED} NO-ERROR.
   IF NOT AVAIL bOrderFusion THEN RETURN FALSE.

   /* FIAD-10 Few names come incorrectly from web coverage check */
   lcregion = bOrderCustomer.Region.
   IF bOrderCustomer.Region EQ "ISLAS BALEARES" THEN lcregion = "Baleares".
   IF bOrderCustomer.Region EQ "TENERIFE" THEN lcregion = "Sta.Cruz Tenerife".
   IF bOrderCustomer.Region EQ "LLEIDA" THEN lcregion = "Lérida".

   FIND FIRST Region NO-LOCK WHERE
              Region.RgName EQ lcregion NO-ERROR.
   IF NOT AVAIL Region THEN RETURN FALSE.

   /* FIAD-9 Make new address from different data fields in installation address */
   lcNewAddress = CAPS(bordercustomer.address).
   IF bOrderCustomer.Floor NE "" THEN lcNewAddress = lcNewAddress + " " + LEFT-TRIM(bOrderCustomer.Floor,"0").
   IF bOrderCustomer.Hand NE "" THEN lcNewAddress = lcNewAddress + bOrderCustomer.Hand.
   IF bOrderCustomer.Letter NE "" THEN lcNewAddress = lcNewAddress + " " + bOrderCustomer.Letter.
   IF bOrderCustomer.Stair NE "" THEN lcNewAddress = lcNewAddress + " " + bOrderCustomer.Stair.
   IF bOrderCustomer.Door NE "" THEN lcNewAddress = lcNewAddress + " " + bOrderCustomer.Door.
   IF bOrderCustomer.Block NE "" THEN lcNewAddress = lcNewAddress + " " + bOrderCustomer.Block.
   lcNewAddress = RIGHT-TRIM(lcNewAddress).
   lcNewAddress = REPLACE(lcNewAddress, "  ", " ").
   lcInvGroup = fDefInvGroup(Region.Region).

   FIND CURRENT bCustomer EXCLUSIVE-LOCK NO-ERROR.
   IF llDoEvent THEN DO:
      DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
      lhCustomer = BUFFER bCustomer:HANDLE.
      RUN StarEventInitialize(lhCustomer).
      RUN StarEventSetOldBuffer(lhCustomer).
   END.

   /* Update Customer */
   ASSIGN
      bCustomer.Address = lcNewAddress WHEN bCustomer.Address NE lcNewAddress
      bCustomer.ZipCode = bOrderCustomer.ZipCode WHEN bCustomer.ZipCode NE bOrderCustomer.ZipCode
      bCustomer.PostOffice = CAPS(bOrderCustomer.PostOffice) WHEN bCustomer.PostOffice NE bOrderCustomer.PostOffice
      bCustomer.Region = Region.Region WHEN bCustomer.Region NE Region.Region
      bCustomer.InvGroup = lcInvGroup WHEN bCustomer.InvGroup NE lcInvGroup.

   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent(lhCustomer).
      fCleanEventObjects().
   END.

   RETURN TRUE.

END FUNCTION.

&ENDIF
