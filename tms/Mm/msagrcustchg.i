/* msagrcustchg.i   29.04.09/aam 
*/

{Syst/commali.i}
{Func/barrfunc.i}
{Mnp/mnpoutchk.i}
{Func/orderchk.i}
{Func/fixedlinefunc.i}
{Func/profunc.i}

&SCOPED-DEFINE ACC_OLB_BARRINGS_NOT_ALLOWED "Y_HURG"
   
FUNCTION fCheckACCCompability RETURNS CHARACTER
   (INPUT iiCustSRC AS INT,
    INPUT iiCustDST AS INT):
   DEF BUFFER bCustCatSRC FOR CustCat.
   DEF BUFFER bCustCatDST FOR CustCat.
   DEF BUFFER bCustomerSRC FOR Customer.
   DEF BUFFER bCustomerDST FOR Customer.

   FIND FIRST bCustomerSRC NO-LOCK WHERE
              bCustomerSRC.Custnum EQ iiCustSRC NO-ERROR.
   IF NOT AVAIL bCustomerSRC THEN RETURN "Customer not found".

   FIND FIRST bCustomerDST NO-LOCK WHERE
              bCustomerDST.Custnum EQ iiCustDST NO-ERROR.
   IF NOT AVAIL bCustomerDST THEN RETURN "Customer not found".

   FIND FIRST bCustCatSRC NO-LOCK WHERE
              bCustCatSRC.Brand EQ gcBrand AND
              bCustCatSRC.Category EQ bCustomerSRC.Category NO-ERROR.
   IF NOT AVAIL bCustCatSRC THEN RETURN "Incorrect customer category".

   FIND FIRST bCustCatDST NO-LOCK WHERE
              bCustCatDST.Brand EQ gcBrand AND
              bCustCatDST.Category EQ bCustomerDST.Category NO-ERROR.
   IF NOT AVAIL bCustCatSRC THEN RETURN "Incorrect customer category".

   IF bCustCatSRC.PRO EQ bCustCatDST.PRO THEN RETURN "".

   RETURN "ACC is not allwed between PRO-NON PRO customers".

END.

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
                     CLIType.Brand = gcBrand AND
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
   
   DEF VAR lcBarrStatus AS CHARACTER NO-UNDO. 
   
   FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE MobSub THEN DO:
      ocMessage = "Unknown subscription".
      RETURN "ERROR".
   END.
   /*YPR-4772*/
   /*acc is not allowed for convergent tariffs.*/
   IF fIsConvergenceTariff(MobSub.CLIType) THEN DO:       
       ocMessage = "Not allowed for fixed line tariffs".
       RETURN "ERROR".
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

   FIND FIRST MsOwner WHERE MsOwner.MsSeq = MobSub.MsSeq NO-LOCK NO-ERROR.
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
            bACCMobsub.Brand = gcBrand AND
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
                                  INPUT NO,
                                  fIsPro(bACCNewCust.Category),
                                  1,
                                  OUTPUT ocMessage,
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
