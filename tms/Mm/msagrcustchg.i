/* msagrcustchg.i   29.04.09/aam 
*/
&IF "{&MSAGRCUSTCHG_I}" NE "YES"
&THEN
&GLOBAL-DEFINE MSAGRCUSTCHG_I YES

{Syst/commali.i}
{Func/barrfunc.i}
{Mnp/mnpoutchk.i}
{Func/orderchk.i}
{Func/fixedlinefunc.i}
{Func/profunc.i}
{Func/fcustdata.i}

&SCOPED-DEFINE ACC_OLB_BARRINGS_NOT_ALLOWED "Y_HURG"
DEF TEMP-TABLE ttCustomer NO-UNDO LIKE Customer
   FIELD cBirthDay AS CHAR
   FIELD CityCode LIKE CustomerReport.CityCode
   FIELD StreetCode LIKE CustomerReport.StreetCode
   FIELD TownCode LIKE CustomerReport.TownCode.
   
FUNCTION fCheckACCCompability RETURNS CHARACTER
   (INPUT iiCustSRC AS INT,
    INPUT iiCustDST AS INT):

   DEF BUFFER bCustCatSRC FOR CustCat.
   DEF BUFFER bCustCatDST FOR CustCat.
   DEF BUFFER bCustomerSRC FOR Customer.
   DEF BUFFER bCustomerDST FOR Customer.

   FIND FIRST bCustomerSRC NO-LOCK WHERE
              bCustomerSRC.Custnum EQ iiCustSRC NO-ERROR.
   IF NOT AVAIL bCustomerSRC THEN RETURN "Old customer not found".

   FIND FIRST bCustomerDST NO-LOCK WHERE
              bCustomerDST.Custnum EQ iiCustDST NO-ERROR.
   IF NOT AVAIL bCustomerDST THEN RETURN "New customer not found".

   FIND FIRST bCustCatSRC NO-LOCK WHERE
              bCustCatSRC.Brand EQ Syst.Var:gcBrand AND
              bCustCatSRC.Category EQ bCustomerSRC.Category NO-ERROR.
   IF NOT AVAIL bCustCatSRC THEN RETURN "Incorrect old customer category".

   FIND FIRST bCustCatDST NO-LOCK WHERE
              bCustCatDST.Brand EQ Syst.Var:gcBrand AND
              bCustCatDST.Category EQ bCustomerDST.Category NO-ERROR.
   IF NOT AVAIL bCustCatDST THEN RETURN "Incorrect new customer category".

   IF bCustCatSRC.PRO NE bCustCatDST.PRO THEN
   DO:
      IF NOT bCustCatSRC.PRO AND bCustCatDST.PRO THEN
         RETURN "ACC is not allowed between PRO-NON PRO customers".
      /* Check for any active/ongoing subscriptions. If there is any, no migration possible. */
      ELSE IF (CAN-FIND(FIRST MobSub WHERE MobSub.Brand   = Syst.Var:gcBrand              AND 
                                           MobSub.AgrCust = bCustomerDST.CustNum AND 
                                           MobSub.Cli     > ""                   NO-LOCK)) OR 
              fCheckOngoingOrders(bCustomerDST.CustIdType, bCustomerDST.OrgId, 0) THEN 
         RETURN "ACC is not allowed between PRO-NON PRO customers".
   END.   
   
   RETURN "".

END FUNCTION.

FUNCTION fPreCheckSubscriptionForACC RETURNS CHARACTER
   (INPUT iiMsSeq AS INT):
   
   DEF BUFFER MobSub FOR MobSub.

   FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE MobSub THEN
      RETURN "Unknown subscription".

   IF MobSub.MultiSimId > 0 AND
      MobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} THEN
      RETURN "ACC is not allowed for multi SIM secondary subscription".

   IF CAN-FIND(FIRST CLIType WHERE
                     CLIType.Brand = Syst.Var:gcBrand AND
                     CLIType.CLIType = (IF MobSub.TariffBundle > ""
                                        THEN MobSub.TariffBundle
                                        ELSE MobSub.CLIType) AND
                     CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL}) THEN
      RETURN "ACC is not allowed for additional line".

   RETURN "".
END.

PROCEDURE pCheckSubscriptionForACC:

   DEF INPUT  PARAMETER iiMsSeq      AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiCurrentReq AS INT  NO-UNDO.
   DEF INPUT  PARAMETER icChannel    AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER ocMessage    AS CHAR NO-UNDO.
   
   DEF BUFFER bPendingReq FOR MsRequest.
   DEF BUFFER MobSub FOR MobSub.
   DEF BUFFER MsOwner FOR MsOwner.
   DEF BUFFER Limit FOR Limit.
   DEF BUFFER MsRequest FOR MsRequest.
   
   DEF VAR lcBarrStatus   AS CHARACTER NO-UNDO. 
  
   FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE MobSub THEN DO:
      ocMessage = "Unknown subscription".
      RETURN "ERROR".
   END.

   /* Bypass check if MsSeg set to cparam */
   IF LOOKUP(STRING(iiMsSeq), fCParamC("PassConvergentACC")) = 0 THEN DO:
      /*YPR-4772*/
      /*acc is not allowed for convergent tariffs.*/
      IF fIsConvergenceTariff(MobSub.CLIType) THEN DO:
         ocMessage = "Not allowed for fixed line tariffs".
         RETURN "ERROR".
      END.
   END.

   IF TODAY - MobSub.ActivationDate < 30 THEN DO:
      ocMessage = "Subscription has not been active long enough".
      RETURN "CHECK".
   END.

   /* termination is checked separately */ 
   FOR FIRST bPendingReq NO-LOCK WHERE 
             bPendingReq.MSSeq   = Mobsub.MSSeq AND
             bPendingReq.ReqType = 18 AND
             LOOKUP(STRING(bPendingReq.ReqStatus),"2,4,9") = 0:
      ocMessage = "Subscription has a pending termination request.".
      RETURN "ERROR/SMS/312".
   END.

   /* other requests */
   FOR FIRST bPendingReq NO-LOCK WHERE 
             bPendingReq.MSSeq = Mobsub.MSSeq AND
             LOOKUP(STRING(bPendingReq.ReqStatus),"2,4,9") = 0 AND
             bPendingReq.MsRequest NE iiCurrentReq:
      ocMessage = "Subscription has a pending request.".
      RETURN "ERROR/SMS/310".
   END.

   IF MobSub.CLIType = "tarj3" AND 
      LOOKUP(STRING(MobSub.AgrCust),"233718,239696,239680,239666") = 0 
   THEN DO:
      ocMessage = "Current customer is not a preactivated one".
      RETURN "ERROR".
   END.

   IF LOOKUP(STRING(MobSub.MsStat),"4,8") = 0 THEN DO:
      ocMessage = "Subscription status is not valid for owner change".
      RETURN "ERROR".
   END.

   lcBarrStatus = fGetActiveBarrings(MobSub.MsSeq).
   IF fIsInList(lcBarrStatus,{&FRAUD_BARR_CODES} + "," +
                             {&ACC_OLB_BARRINGS_NOT_ALLOWED}) THEN DO:
      ocMessage = "Subscription has an active operator or debt barring".
      RETURN "ERROR/SMS/311".
   END.
   
   IF NOT CAN-FIND(Customer WHERE Customer.CustNum = MobSub.AgrCust) THEN DO:
      ocMessage = "Current agreement customer for subscription is invalid".
      RETURN "ERROR".
   END.

   IF NOT CAN-FIND(Customer WHERE Customer.CustNum = MobSub.InvCust) THEN DO:
      ocMessage = "Current invoicing customer for subscription is invalid".
      RETURN "ERROR".
   END.

   IF NOT CAN-FIND(Customer WHERE Customer.CustNum = MobSub.CustNum) THEN DO:
      ocMessage = "Current user for subscription is invalid".
      RETURN "ERROR".
   END.
   
   IF icChannel NE {&REQUEST_SOURCE_NEWTON} THEN
      FOR FIRST Limit NO-LOCK WHERE
                Limit.MsSeq     = MobSub.MsSeq AND
                Limit.LimitType = 3            AND
                Limit.TMRuleSeq = 0            AND
                Limit.ToDate   >= TODAY        AND
                Limit.FromDate <= TODAY        AND
                Limit.Custnum   = MobSub.Custnum AND
                Limit.LimitID   = 0            AND
                Limit.LimitAmt > 0:
         ocMessage = "Subscription has a billing suspension / prohibition".
         RETURN "ERROR".
      END.

   FIND FIRST MsOwner NO-LOCK WHERE
              MsOwner.MsSeq = MobSub.MsSeq AND
              MsOwner.TsEnd > MsOwner.TsBegin
      USE-INDEX MsSeq NO-ERROR.
   IF NOT AVAILABLE MsOwner    OR 
      MsOwner.TsEnd < 99999999 OR
      MsOwner.CustNum NE MobSub.CustNum OR
      MsOwner.InvCust NE MobSub.InvCust OR
      MsOwner.AgrCust NE MobSub.AgrCust
   THEN DO:
      ocMessage = "Timestamp history data for subscription is invalid".
      RETURN "ERROR".
   END.
      
   IF fIsMNPOutOngoing(mobsub.cli) THEN DO:
      ocMessage = "Ongoing MNP OUT request".
      RETURN "ERROR".
   END.
   
   IF iiCurrentReq > 0 THEN DO:
      FIND FIRST MsRequest NO-LOCK WHERE
                 MsRequest.MsRequest = iiCurrentReq AND
                 MsRequest.ReqType = 10 NO-ERROR.
      IF NOT AVAIL MsRequest THEN DO:
         ocMessage = "Unknown request".
         RETURN "ERROR".
      END.

      IF MsRequest.ReqIParam1 > 0 THEN DO:
         ocMessage = fCheckACCCompability(MsRequest.Custnum, 
                                          MsRequest.ReqIParam1).
         IF ocMessage > "" THEN RETURN "ERROR".
      END.
   END.

   RETURN "".

END PROCEDURE. /* pCheckSubscription */

PROCEDURE pCheckTargetCustomerForACC:

   DEF INPUT  PARAMETER iiNewCustnum AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER ocMessage    AS CHAR NO-UNDO.
   
   DEF BUFFER bACCMobsub FOR MobSub.
   DEF BUFFER bACCNewCust FOR Customer.
   
   DEF VAR lcBarrStatus AS CHARACTER NO-UNDO. 
   DEF VAR liSubLimit AS INT NO-UNDO. 
   DEF VAR lisubs AS INT NO-UNDO.
   DEF VAR liActLimit AS INT NO-UNDO.
   DEF VAR liActs AS INT NO-UNDO.
   
   IF iiNewCustnum = 0 THEN RETURN "".

   FOR EACH bACCMobsub NO-LOCK WHERE
            bACCMobsub.Brand = Syst.Var:gcBrand AND
            bACCMobsub.AgrCust = iiNewCustnum AND
            bACCMobsub.PayType = FALSE:
      lcBarrStatus = fGetActiveBarrings(bACCMobsub.MsSeq).
      IF fIsInList(lcBarrStatus,{&FRAUD_BARR_CODES} + "," + 
                                {&ACC_OLB_BARRINGS_NOT_ALLOWED}) THEN DO:
         ocMessage = "Target customer has subscription with active operator or debt barring".
         RETURN "ERROR".
      END.
   END.

   FIND FIRST bACCNewCust WHERE
              bACCNewCust.CustNum = iiNewCustnum 
   NO-LOCK NO-ERROR.
   
   IF AVAILABLE bACCNewCust AND
      NOT fSubscriptionLimitCheck(INPUT bACCNewCust.OrgId,
                                  INPUT bACCNewCust.CustIdType,
                                  INPUT fIsSelfEmpl(bACCNewCust.Category),
                                  INPUT fIsPro(bACCNewCust.Category),
                                  1,
                                  OUTPUT liSubLimit,
                                  OUTPUT liSubs,
                                  OUTPUT liActLimit,
                                  OUTPUT liActs) THEN DO:
      ocMessage = "Subscription limit exceeded".
      RETURN "ERROR".
   END. /* IF AVAILABLE bACCNewCust AND */

   RETURN "".

END PROCEDURE. 

FUNCTION fCreateAccDataParam RETURNS CHAR
   (INPUT ihCustomer AS HANDLE,
    INPUT icSalesMan AS CHAR,
    INPUT icAgrCustIdType AS CHAR,
    INPUT icAgrCustId AS CHAR,
    INPUT icStreetCode AS CHAR,
    INPUT icCityCode AS CHAR,
    INPUT icTownCode AS CHAR,
    INPUT icMandateId AS CHAR,
    OUTPUT ocError AS CHAR):

   DEF VAR lcCode AS CHAR NO-UNDO. 
   
   lcCode = 
      trim(ihCustomer::Custname) + ";" + 
      trim(ihCustomer::FirstName) + ";" +
      trim(ihCustomer::Surname2) + ";" +
      TRIM(ihCustomer::COName) + ";" + 
      TRIM(ihCustomer::Companyname) + ";" +
      TRIM(ihCustomer::Address) + ";" + 
      TRIM(ihCustomer::ZipCode) + ";" +
      TRIM(ihCustomer::PostOffice) + ";" +
      TRIM(ihCustomer::Country) + ";" +
      TRIM(ihCustomer::EMail) + ";" +
      icSalesMan + ";" +
      TRIM(ihCustomer::CustIdType) + ";" +
      TRIM(ihCustomer::OrgId) + ";" +
      (IF ihCustomer::BirthDay NE ? THEN
       STRING(ihCustomer::BirthDay, "99-99-9999") ELSE "") + ";" +
      STRING(ihCustomer::Language) + ";" + 
      TRIM(ihCustomer::HonTitle) + ";" +
      TRIM(ihCustomer::Region) + ";" +
      TRIM(ihCustomer::BankAcct) + ";" +
      TRIM(ihCustomer::Nationality) + ";" +
      (IF ihCustomer::FoundationDate NE ? THEN
         STRING(ihCustomer::FoundationDate,"99-99-9999") ELSE "") + ";" +
      TRIM(ihCustomer::smsnumber) + ";" +
      TRIM(ihCustomer::phone) + ";" +
      STRING(ihCustomer::DirMarkSMS) + ";" +
      STRING(ihCustomer::DirMarkEmail) + ";" +
      STRING(ihCustomer::DirMarkPost) + ";" +
      STRING(ihCustomer::OutMarkSMS) + ";" +
      STRING(ihCustomer::OutMarkEmail) + ";" +
      STRING(ihCustomer::OutMarkPost) + ";" +
      icStreetCode + ";" +
      icCityCode + ";" +
      ";" + /* deltype is removed */
      TRIM(icAgrCustIdType) + ";" +
      TRIM(icAgrCustId) + ";" +
      icTownCode + ";" +
      (IF icMandateId > "" THEN icMandateId ELSE "").

   IF NUM-ENTRIES(lcCode,";") > 35 THEN 
      ocError = "Check data, it cannot contain semicolons (;)".

   RETURN lcCode.  

END.
            
FUNCTION fParseAccDataParam RETURNS LOGICAL
  (INPUT icDataField AS CHARACTER,
   OUTPUT TABLE FOR ttCustomer):

   CREATE ttCustomer.
   ASSIGN
      ttCustomer.CustName        = ENTRY(1,icDataField,";")
      ttCustomer.FirstName       = ENTRY(2,icDataField,";")
      ttCustomer.Surname2        = ENTRY(3,icDataField,";")
      ttCustomer.COName          = ENTRY(4,icDataField,";")
      ttCustomer.Companyname     = ENTRY(5,icDataField,";")
      ttCustomer.Address         = ENTRY(6,icDataField,";")
      ttCustomer.ZipCode         = ENTRY(7,icDataField,";")
      ttCustomer.PostOffice      = ENTRY(8,icDataField,";")
      ttCustomer.Country         = ENTRY(9,icDataField,";")
      ttCustomer.Email           = ENTRY(10,icDataField,";")
      ttCustomer.SalesMan        = ENTRY(11,icDataField,";")
      ttCustomer.CustIdType      = ENTRY(12,icDataField,";")
      ttCustomer.OrgId           = ENTRY(13,icDataField,";")
      ttCustomer.BirthDay        = DATE(ENTRY(14,icDataField,";"))
      ttCustomer.Language        = INT(ENTRY(15,icDataField,";"))
      ttCustomer.HonTitle        = ENTRY(16,icDataField,";")
      ttCustomer.Region          = ENTRY(17,icDataField,";")
      ttCustomer.BankAcc         = ENTRY(18,icDataField,";")
      ttCustomer.Nationality     = ENTRY(19,icDataField,";")
      ttCustomer.FoundationDate  = DATE(ENTRY(20,icDataField,";"))
      ttCustomer.smsnumber       = ENTRY(21,icDataField,";")
      ttCustomer.phone           = ENTRY(22,icDataField,";")
      ttCustomer.DirMarkSMS      = LOGICAL(ENTRY(23,icDataField,";"))
      ttCustomer.DirMarkEmail    = LOGICAL(ENTRY(24,icDataField,";"))
      ttCustomer.DirMarkPost     = LOGICAL(ENTRY(25,icDataField,";"))
      ttCustomer.OutMarkSMS      = LOGICAL(ENTRY(26,icDataField,";"))
      ttCustomer.OutMarkEmail    = LOGICAL(ENTRY(27,icDataField,";"))
      ttCustomer.OutMarkPost     = LOGICAL(ENTRY(28,icDataField,";"))
      ttCustomer.StreetCode      = ENTRY(29,icDataField,";")
      ttCustomer.CityCode        = ENTRY(30,icDataField,";") 
      ttCustomer.AuthCustIdType  = ENTRY(32,icDataField,";")
      ttCustomer.AuthCustId      = ENTRY(33,icDataField,";")
      ttCustomer.TownCode        = ENTRY(34,icDataField,";")
      ttCustomer.SearchName      = SUBSTRING(ttCustomer.CustName + " " + 
                                           ttCustomer.FirstName,1,8) 
      NO-ERROR.

   IF ERROR-STATUS:ERROR THEN RETURN FALSE.
   RETURN TRUE.

END.

FUNCTION fParseAccOrderCustomer RETURNS LOGICAL
   (iiOrderID AS INT,
    OUTPUT TABLE FOR ttCustomer):

   DEF BUFFER OrderCustomer FOR OrderCustomer.
   DEF BUFFER Order FOR Order.   

   FIND Order NO-LOCK WHERE
        Order.Brand = Syst.Var:gcBrand AND
        Order.OrderID = iiOrderID NO-ERROR.
   IF NOT AVAIL Order THEN RETURN FALSE.

   FIND OrderCustomer NO-LOCK WHERE
        OrderCustomer.Brand = Syst.Var:gcBrand AND
        OrderCustomer.OrderID = Order.OrderID AND
        OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_ACC}.
   IF NOT AVAIL OrderCustomer THEN RETURN FALSE.

   CREATE ttCustomer.
   ASSIGN
      ttCustomer.CustName        = OrderCustomer.SurName1
      ttCustomer.FirstName       = OrderCustomer.FirstName
      ttCustomer.Surname2        = OrderCustomer.SurName2
      ttCustomer.COName          = "" /* not used with ACC orders */
      ttCustomer.Companyname     = OrderCustomer.Company
      ttCustomer.Address         = OrderCustomer.Address
      ttCustomer.ZipCode         = OrderCustomer.ZipCode
      ttCustomer.PostOffice      = OrderCustomer.PostOffice
      ttCustomer.Country         = OrderCustomer.Country
      ttCustomer.Email           = OrderCustomer.Email
      ttCustomer.SalesMan        = Order.SalesMan
      ttCustomer.CustIdType      = OrderCustomer.CustIDType
      ttCustomer.OrgId           = OrderCustomer.CustID
      ttCustomer.BirthDay        = OrderCustomer.BirthDay
      ttCustomer.Nationality     = OrderCustomer.Nationality
      ttCustomer.Language        = INTEGER(OrderCustomer.Language)
      ttCustomer.HonTitle        = OrderCustomer.CustTitle
      ttCustomer.Region          = OrderCustomer.Region
      ttCustomer.BankAcc         = OrderCustomer.BankCode
      ttCustomer.FoundationDate  = OrderCustomer.FoundationDate
      ttCustomer.smsnumber       = OrderCustomer.MobileNumber
      ttCustomer.phone           = OrderCustomer.FixedNumber
      ttCustomer.DirMarkSMS      = OrderCustomer.OperSMSMarketing
      ttCustomer.DirMarkEmail    = OrderCustomer.OperEMailMarketing
      ttCustomer.DirMarkPost     = OrderCustomer.OperPostMarketing
      ttCustomer.OutMarkSMS      = OrderCustomer.OutSMSMarketing
      ttCustomer.OutMarkEmail    = OrderCustomer.OutEMailMarketing
      ttCustomer.OutMarkPost     = OrderCustomer.OutPostMarketing
      ttCustomer.StreetCode      = OrderCustomer.AddressCodC
      ttCustomer.CityCode        = OrderCustomer.AddressCodP
      ttCustomer.AuthCustIdType  = OrderCustomer.AuthCustIdType
      ttCustomer.AuthCustId      = OrderCustomer.AuthCustID
      ttCustomer.TownCode        = OrderCustomer.AddressCodM
      ttCustomer.SearchName      = SUBSTRING(OrderCustomer.Surname1 + " " + 
                                             OrderCustomer.FirstName,1,8)
      NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN RETURN FALSE.
   RETURN TRUE.

END.
&ENDIF
