/*-----------------------------------------------------------------------------
  MODULE .......: msagrcustchg.p
  FUNCTION .....: handle requests for mobsub customer changes
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 10.01.06
  CHANGED.. ....: 27.03.06/aam separated from msrequest.i
                           jt  Msisdn handling
                               Removed TF-mobsub handling
                               commented code (seek telef)
  Version ......: Yoigo
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msreqfunc.i}
{Syst/eventval.i}
{Func/fwebuser.i}
{Func/fmakemsreq.i}
{Func/msisdn.i}
{Func/ftmrlimit.i}
{Func/fcustdata.i}
{Mm/msagrcustchg.i}
{Func/fuserright.i}
{Syst/tmsconst.i}
{Mc/invoicetarget.i}
{Func/femailinvoice.i}
{Func/fcustchangereq.i}
{Func/fsubstermreq.i}
{Func/add_lines_request.i}
{Func/fbankdata.i}
{Func/dss_request.i}
{Func/dss_matrix.i}
{Func/orderfunc.i}
{Func/custfunc.i}
{Func/addline_discount.i}
{Func/profunc.i}
   
DEF TEMP-TABLE ttOrderCustomer NO-UNDO LIKE OrderCustomer.

SESSION:SYSTEM-ALERT-BOXES = TRUE.

DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

DEF BUFFER bNewCust FOR Customer.


FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 10 THEN 
   RETURN "ERROR:Unknown request".


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMobSub AS HANDLE NO-UNDO.
   lhMobSub = BUFFER MobSub:HANDLE.
   RUN StarEventInitialize(lhMobSub).

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER bNewCust:HANDLE.
   RUN StarEventInitialize(lhCustomer).

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
   RUN StarEventInitialize(lhSingleFee).
   
   DEFINE VARIABLE lhMsOwner AS HANDLE NO-UNDO.
   lhMsOwner = BUFFER MsOwner:HANDLE.
   RUN StarEventInitialize(lhMsOwner).

   DEFINE VARIABLE lhCustContact AS HANDLE NO-UNDO.
   lhCustContact = BUFFER CustContact:HANDLE.
   RUN StarEventInitialize(lhCustContact).
END.


CASE MsRequest.ReqStatus:

   WHEN 0 THEN RUN pOwnerChange.
   
   WHEN 7 THEN RUN pCheckSubReq.
   
   WHEN 8 THEN RUN pOwnerChange.

END CASE.

fCleanEventObjects().


PROCEDURE pCheckSubReq.

   CASE fGetSubRequestState(MsRequest.MsRequest):

      WHEN 2 THEN DO:
         /* subrequest handled succesfully,
            ready to continue */
         fReqStatus(8,"").
      END.

      WHEN 3 THEN DO:
         fReqStatus(3,"Subrequest failure").
      END.

      OTHERWISE DO:
         /* back to waiting mode */
         fReqStatus(7,"").
      END.
   
   END.

END PROCEDURE.

PROCEDURE pFinalize:
   
   DEFINE INPUT PARAMETER ldReqAct AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER iiMsSeq  AS INT     NO-UNDO.
   DEFINE INPUT PARAMETER icCLI    AS CHAR    NO-UNDO.
   
   DEFINE VARIABLE ldaFromDate     AS DATE    NO-UNDO.
   DEFINE VARIABLE liTime          AS INT     NO-UNDO.
   DEFINE VARIABLE liPayType       AS INT     NO-UNDO.
   DEFINE VARIABLE ldaToDate       AS DATE    NO-UNDO.

   Func.Common:mSplitTS (INPUT  ldReqAct, 
             OUTPUT ldaFromDate, 
             OUTPUT liTime).
   IF DAY(ldaFromDate) NE 1 THEN 
      ldaFromDate = DATE(MONTH(ldaFromDate),1,YEAR(ldaFromDate)).
   IF MONTH(ldaFromDate) = 12 THEN 
      ldaToDate = DATE(12,31,YEAR(ldaFromDate)).
   ELSE ldaToDate = DATE(MONTH(ldaFromDate) + 1,1,YEAR(ldaFromDate)) - 1.

   liPayType = 1. 
   FIND FIRST MsOwner WHERE 
              MsOwner.MsSeq = iiMsSeq AND
              MsOwner.TSEnd >= 99999999 NO-LOCK NO-ERROR.
   IF AVAILABLE MsOwner AND MsOwner.PayType THEN liPayType = 2.
   
   IF liPayType = 2 THEN 
      RUN Rate/cli_prepaidrate.p (icCLI,     
                           ldaFromDate,  
                           ldaToDate,    
                           TRUE).      /* silent = true */  
   ELSE 
      RUN Rate/cli_rate.p (icCLI,
                    ldaFromDate,
                    ldaToDate,
                    TRUE).
                                                           
END PROCEDURE.

/* agreement customer change for subscription */
PROCEDURE pOwnerChange:

   DEF VAR liDefCust    AS INT  NO-UNDO. 
   DEF VAR liNewOwner   AS INT  NO-UNDO.
   DEF VAR liNewInvCust AS INT  NO-UNDO.
   DEF VAR liNewUser    AS INT  NO-UNDO.
   DEF VAR liChkCust    AS INT  NO-UNDO.
   DEF VAR liCreated    AS INT  NO-UNDO EXTENT 3.
   DEF VAR lcCustType   AS CHAR NO-UNDO.
   DEF VAR liOldOwner   AS INT  NO-UNDO.
   DEF VAR llNewCust    AS LOG  NO-UNDO. 
   DEF VAR liSubRequest AS INT  NO-UNDO.
   DEF VAR lcInfo       AS CHAR NO-UNDO. 
   DEF VAR lhRequest    AS HANDLE NO-UNDO. 
   DEF VAR lhDataField  AS HANDLE NO-UNDO.
   DEF VAR lcDataField  AS CHAR NO-UNDO.
   DEF VAR lcInvGroup   AS CHAR NO-UNDO. 
   DEF VAR liOrigStat   AS INT  NO-UNDO.
   DEF VAR llValue      AS LOG  NO-UNDO.
   DEF VAR liNewValue   AS INT  NO-UNDO.
   DEF VAR lcEmail      AS CHAR NO-UNDO.
   DEF VAR lcResult     AS CHAR NO-UNDO.
   DEF VAR liRequest    AS INT  NO-UNDO.
   
   DEF VAR liChargeReqId AS INT NO-UNDO.
   DEF VAR lcCode        AS CHAR NO-UNDO. 
   DEF VAR lcMemo        AS CHAR NO-UNDO. 
   DEF VAR lcChannel     AS CHAR NO-UNDO.
   DEF VAR lcCategory    AS CHAR NO-UNDO. 

   DEF BUFFER bMobSub       FOR MobSub.
   DEF BUFFER bMsRequest    FOR MsRequest.
   DEF BUFFER bOldCustCat   FOR CustCat.
   DEF BUFFER bACCOrder     FOR Order.

   liOrigStat = MsRequest.ReqStat.
      
   /* Set request under work status */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".
   
   ASSIGN liNewOwner = MsRequest.ReqIParam1.
      
   IF MsRequest.ReqIParam4 > 0 THEN DO:

      FIND bAccOrder NO-LOCK WHERE
           bAccOrder.brand = Syst.Var:gcBrand AND
           bAccOrder.OrderID = MsRequest.ReqIParam4 NO-ERROR. 

      IF NOT AVAIL bAccOrder THEN RETURN "ERROR: Order not found".

      IF bAccOrder.StatusCode NE {&ORDER_STATUS_ONGOING} THEN
         RETURN SUBST("ERROR: Incorrect order status &1", bAccOrder.StatusCode).

      IF NOT fParseAccOrderCustomer(
         bAccOrder.OrderID,
         OUTPUT TABLE ttCustomer BY-REFERENCE) THEN
         RETURN "ERROR: New customer data parsing failed".
      
   END.
   ELSE DO:

      IF NOT fParseAccDataParam(
        MsRequest.ReqCParam1,
        OUTPUT TABLE ttCustomer BY-REFERENCE) THEN
        RETURN "ERROR: New customer data parsing failed".
   END.

   IF NOT AVAIL ttCustomer THEN 
      RETURN "ERROR: New customer data not available§".

   /* Double check existing customer */
   IF liNewOwner = 0 THEN DO: 
      
      FIND FIRST bNewCust WHERE
         bNewCust.Brand      = Syst.Var:gcBrand AND
         bNewCust.OrgId      = ttCustomer.OrgID AND
         bNewCust.CustIdType = ttCustomer.CustIDType AND
         bNewCust.Roles NE "inactive"
      NO-LOCK NO-ERROR.
      
      IF AVAIL bNewCust THEN DO:
         liNewOwner = bNewCust.Custnum.
         FIND CURRENT MSRequest EXCLUSIVE-LOCK NO-ERROR.
         MsRequest.ReqIParam1 = bNewCust.Custnum.
         FIND CURRENT MSRequest NO-LOCK NO-ERROR.
      END.
   END.

   lcInfo = Func.ValidateACC:mCheckSubscriptionForACC(MsRequest.MsSeq,
                                                      MsRequest.MsRequest,
                                                      MsRequest.ReqIParam4,
                                                      MsRequest.ReqSource).
   
   IF lcInfo EQ "" AND MsRequest.ReqIParam1 > 0
   THEN lcInfo = Func.ValidateACC:mCheckTargetCustomerForACC(MsRequest.ReqIParam1).
   
   IF lcInfo > "" THEN DO:

      /* 'superuser' can skip some rules */
      IF ENTRY(1,lcInfo,"|") EQ "CHECK" AND
         fTokenRights(MsRequest.UserCode,"CCSUPER") = "RW"
      THEN DO:
         IF INDEX(MsRequest.Memo,SUBSTRING(lcInfo,INDEX(lcInfo,"|") + 1)) = 0 THEN DO:   
            FIND CURRENT MsRequest EXCLUSIVE-LOCK.
            MsRequest.Memo = MsRequest.Memo + 
                            (IF MsRequest.Memo > ""
                             THEN ", " 
                             ELSE "") +
                             "Superuser " + MsRequest.UserCode + 
                             " overrode rejection reason: " + SUBSTRING(lcInfo,INDEX(lcInfo,"|") + 1).
         END.
      END.
   
      ELSE DO:
         IF liOrigStat > 0 AND INDEX(ENTRY(1,lcInfo,"|"),"SMS") > 0 THEN DO:
            
            RUN Mm/acc_sendsms.p(MsRequest.MsRequest,
                            MsRequest.CustNum,
                            "Rejected",
                            IF NUM-ENTRIES(ENTRY(1,lcInfo,"|"),"/") >= 3 
                            THEN "HT:" + ENTRY(3,ENTRY(1,lcInfo,"|"),"/")
                            ELSE SUBSTRING(lcInfo,INDEX(lcInfo,"|") + 1)).
         END.
               
         fReqError(SUBSTRING(lcInfo,INDEX(lcInfo,"|") + 1)).
         RETURN "ERROR:Subscr. not valid".
      END.   
   END.
 
   FIND MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE MobSub THEN DO:
      fReqError("MobSub not found").
      RETURN.
   END.

   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq AND
              MsOwner.TSEnd >= 99999999 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MsOwner OR MsOwner.CustNum NE MobSub.CustNum OR
      MsOwner.AgrCust NE MobSub.AgrCust OR MsOwner.TSEnd < MsRequest.ActStamp
   THEN DO:
      fReqError("Invalid MSOwner data").
      RETURN.
   END. 
 
   /* nor an existing customer nbr or a name for the new customer is given */
   IF MsRequest.ReqIParam1 = 0 AND 
      MsRequest.ReqCParam1 = "" AND
      MsRequest.ReqIparam4 = 0 THEN DO:
      fReqError("Nothing to do").
      RETURN. 
   END.
   
   FIND FIRST Customer NO-LOCK WHERE
              Customer.custnum = MsRequest.Custnum NO-ERROR.
   IF NOT AVAIL Customer THEN DO:
      fReqError("Old customer not found").
   END.

   FIND bOldCustCat NO-LOCK WHERE
        bOldCustCat.Brand = Syst.Var:gcBrand AND
        bOldCustCat.Category = Customer.Category NO-ERROR.

   Func.Common:mSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).
   
   /* 1. phase for normal agr.cust change */
   IF liOrigStat = 0 THEN DO:

      /* credit check for postpaid (not for companies) */
      IF MobSub.PayType = FALSE THEN DO:
      
         IF ttCustomer.CustIdType = "CIF" THEN DO:
            fReqStatus(8,"").
         END.
            
         ELSE DO:   
            RUN pCreditCheck.
            IF RETURN-VALUE BEGINS "ERROR:" THEN DO:
               fReqError(RETURN-VALUE).
            END.
            ELSE DO:
               fReqStatus(7,"").
            END.
         END.
         
         RETURN.
      END.

      ELSE DO:
         IF MsRequest.ReqDparam1 > MsRequest.ActStamp THEN DO:
            FIND CURRENT MsRequest EXCLUSIVE-LOCK.
            MsRequest.ActStamp = MsRequest.ReqDparam1.
            
            fReqStatus(8,"").
            RETURN.
         END.
      END.
   END.

   ELSE IF liOrigStat = 8 THEN DO:
         
      /* mark final activation time and send notification to customer */
      IF MobSub.PayType = FALSE AND 
         MsRequest.ReqDParam1 > MsRequest.ActStamp
      THEN DO:
         
         RUN Mm/acc_sendsms.p(MsRequest.MsRequest,
                         MsRequest.CustNum,
                         "Accepted",
                         "").

         RUN Mm/acc_sendsms.p(MsRequest.MsRequest,
                         MsRequest.CustNum,
                         "PreviousDay",
                         "").
         
         FIND CURRENT MsRequest EXCLUSIVE-LOCK.
         MsRequest.ActStamp = MsRequest.ReqDparam1.
         
         fReqStatus(8,"").
         RETURN.
      END.
   END.
   
   IF AVAIL bAccOrder AND
      fIsConvergenceTariff(MobSub.CLIType) THEN DO:
      RUN Gwy/masmovil_acc.p(bAccOrder.OrderID).
      IF RETURN-VALUE NE "OK" THEN DO:
         fReqError(SUBST("Fixed line ACC failed: &1", RETURN-VALUE)).
         RETURN.
      END.
   END.
   
   ASSIGN liOldOwner   = MobSub.AgrCust
          liCreated    = 0
          lhRequest    = BUFFER MsRequest:HANDLE.

   /* a new customer will be created */
   DO liReqCnt = 1 TO 3:

      /* values from reqcparam1-3 */
      lhDataField = lhRequest:BUFFER-FIELD("ReqCParam" + STRING(liReqCnt)).

      lcDataField = lhDataField:BUFFER-VALUE.
    
      CASE liReqCnt:
      WHEN 1 THEN DO:
      END.

      WHEN 2 THEN DO:
         /* is agrcust also invcust */
         CASE SUBSTRING(MsRequest.ReqCParam4,2,1):
         WHEN "1" THEN liNewInvCust = liNewOwner.
         WHEN "2" THEN DO:
            liNewInvCust = MsRequest.ReqIParam2.
            IF NUM-ENTRIES(MsRequest.ReqCParam2,";") < 10 THEN DO:
               fReqError("Invoice customer data missing").
               RETURN.
            END.
         END.
         OTHERWISE liNewInvCust = 0.
         END CASE.

         IF liNewInvCust > 0 THEN NEXT.
      END.
         
      WHEN 3 THEN DO:
       
         /* is agrcust or invcust also the user */
         CASE SUBSTRING(MsRequest.ReqCParam4,3,1):
         WHEN "1" THEN liNewUser = liNewOwner.
         WHEN "2" THEN liNewUser = liNewInvCust.
         WHEN "3" THEN DO:
            liNewUser = INTEGER(ENTRY(11,MsRequest.ReqCParam3,";")).
            IF NUM-ENTRIES(MsRequest.ReqCParam3,";") < 10 THEN DO:
               fReqError("User data missing").
               RETURN.
            END.
         END. 
         OTHERWISE liNewUser = 0.
         END CASE.

         IF liNewUser > 0 THEN DO:
            /* if agrcust = user and it is a new customer number, but invcust 
               is a different one -> mark correct invcust to new owner/user */
            IF liNewUser = liNewOwner AND MsRequest.ReqIParam1 = 0 AND
               liNewInvCust NE liNewOwner THEN DO:
               
               FIND bNewCust WHERE bNewCust.CustNum = liNewInvCust 
                  NO-LOCK NO-ERROR.
               IF AVAILABLE bNewCust AND bNewCust.AgrCust = liNewOwner THEN DO:
                  FIND bNewCust WHERE bNewCust.CustNum = liNewUser 
                     EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE bNewCust THEN bNewCust.InvCust = liNewInvCust.
               END.   
            END.
               
            NEXT.
         END.
         
      END.
         
      END CASE. 

      /* create a new customer */
      IF (liReqCnt = 1 AND liNewOwner = 0) OR liReqCnt > 1 THEN DO:
      
         lcInvGroup = "".
         
         /* group from invoicing customer */
         IF liNewInvCust > 0 THEN DO:
            FIND Customer WHERE Customer.CustNum = liNewInvCust 
                 NO-LOCK NO-ERROR.
            IF AVAILABLE Customer THEN lcInvGroup = Customer.InvGroup.
         END.
         
         /* default group using region */  
         IF lcInvGroup = "" THEN 
            lcInvGroup = fDefInvGroup(ttCustomer.Region).

         liDefCust = fCParamI("DefCust" + lcInvGroup).
      
         IF liDefCust = ? OR liDefCust = 0 THEN DO:
            fReqError("Default customer not defined for " + lcInvGroup).
            RETURN.
         END. 
      
         ASSIGN liCreated[liReqCnt] = liDefCust
                llNewCust           = TRUE.
         RUN Mm/copymobcu.p(INPUT-OUTPUT liCreated[liReqCnt],
                       FALSE).
      END.
      
      /* update old customer's data if vrk has been succesful */
      ELSE IF liReqCnt = 1 AND liNewOwner > 0 THEN ASSIGN 
         liCreated[1] = liNewOwner
         llNewCust    = FALSE.
      
      FIND bNewCust WHERE bNewCust.CustNum = liCreated[liReqCnt] 
         EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE bNewCust THEN DO:

         IF bNewCust.Roles = "inactive" THEN DO:
            fReqError("Customer " + STRING(bNewCust.CustNum) + " is inactive").
            RETURN.
         END.
         
         IF NOT llNewCust AND llDoEvent THEN 
            RUN StarEventSetOldBuffer(lhCustomer).

         /* Find an original request */
         FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.

         CASE MsRequest.ReqSource:
            WHEN {&REQUEST_SOURCE_MANUAL_TMS} THEN
               lcChannel = "TMS".
            WHEN {&REQUEST_SOURCE_NEWTON} THEN
               lcChannel = "VISTA".
            WHEN {&REQUEST_SOURCE_RETAIL_NEWTON} THEN
               lcChannel = "VFR".
         END CASE.

         lcMemo = "ACC" + CHR(255) +
                  STRING(bNewCust.CustNum) + CHR(255) +
                  STRING(MobSub.MsSeq) + CHR(255) +
                  ttCustomer.SalesMan + CHR(255) +
                  lcChannel.

         /* DCH */
         IF llNewCust OR 
            (NOT llNewCust AND MobSub.PayType = FALSE AND
             NOT CAN-FIND(FIRST bMobSub WHERE
                                bMobSub.Brand     = Syst.Var:gcBrand AND
                                bMobSub.MsSeq    <> MobSub.MsSeq AND
                                bMobSub.CustNum   = bNewCust.CustNum AND
                                bMobSub.PayType   = FALSE)) THEN DO:
            ASSIGN
               bNewCust.BirthDay        = ttCustomer.BirthDay
               bNewCust.HonTitle        = ttCustomer.HonTitle
               bNewCust.FirstName       = ttCustomer.FirstName
               bNewCust.CustName        = ttCustomer.CustName
               bNewCust.Surname2        = ttCustomer.SurName2
               bNewCust.Companyname     = ttCustomer.CompanyName
               bNewCust.COName          = ttCustomer.COName
               bNewCust.Address         = ttCustomer.Address
               bNewCust.ZipCode         = ttCustomer.ZipCode
               bNewCust.PostOffice      = ttCustomer.PostOffice
               bNewCust.Country         = ttCustomer.Country
               bNewCust.CustIdType      = ttCustomer.CustIDType
               bNewCust.OrgId           = ttCustomer.OrgId
               bNewCust.Nationality     = ttCustomer.Nationality
               bNewCust.Language        = ttCustomer.Language
               bNewCust.Region          = ttCustomer.Region
               bNewCust.BankAcc         = ttCustomer.BankAcc
               bNewCust.FoundationDate  = ttCustomer.FoundationDate
               bNewCust.smsnumber       = ttCustomer.SMSNumber
               bNewCust.phone           = ttCustomer.Phone
               bNewCust.DirMarkSMS      = ttCustomer.DirMarkSMS
               bNewCust.DirMarkEmail    = ttCustomer.DirMarkEmail
               bNewCust.DirMarkPost     = ttCustomer.DirMarkPost
               bNewCust.OutMarkSMS      = ttCustomer.OutMarkSMS
               bNewCust.OutMarkEmail    = ttCustomer.OutMarkEmail
               bNewCust.OutMarkPost     = ttCustomer.OutMarkPost
               bNewCust.AuthCustIdType  = ttCustomer.AuthCustIdType
               bNewCust.AuthCustId      = ttCustomer.AuthCustId
               bNewCust.SearchName      = ttCustomer.SearchName
               bNewCust.InvGroup        = fDefInvGroup(bNewCust.Region)
                                        WHEN bNewCust.Region NE "00"
               NO-ERROR.
         
            IF ERROR-STATUS:ERROR THEN DO:
               fReqError("Wrong format in new customer data").
               RETURN.
            END.
            
            /* Category according to id type */ 
            FOR EACH CustCat NO-LOCK WHERE 
                     CustCat.Brand = Syst.Var:gcBrand: 
               IF LOOKUP(bNewCust.CustIDType,CustCat.CustIDType) > 0 THEN DO: 
                  bNewCust.Category = CustCat.Category.
                  bNewCust.PaymTerm = CustCat.PaymTerm.
                  LEAVE.
               END.
            END.

            /* Preserve Pro customer category 
               in case the old customer was pro  */
            IF AVAIL bOldCustCat AND bOldCustCat.Pro EQ TRUE THEN DO:
               fgetCustSegment(bNewCust.CustIDType, 
                               (IF bNewCust.CustIDType EQ "CIF" THEN FALSE
                                ELSE bOldCustCat.SelfEmployed),
                               bOldCustCat.pro,
                               bNewCust.OrgId,   /* YDR-2621 */
                               OUTPUT lcCategory).
               IF lcCategory > "" THEN bNewCust.Category = lcCategory.
            END.

            FIND FIRST CustomerReport WHERE
                       CustomerReport.Custnum = bNewCust.Custnum
                       EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAIL CustomerReport THEN CREATE CustomerReport.
            ASSIGN
               CustomerReport.Custnum = bNewCust.Custnum
               CustomerReport.StreetCode = ttCustomer.StreetCode
               CustomerReport.CityCode = ttCustomer.CityCode
               CustomerReport.TownCode = ttCustomer.TownCode.

            /* If customer makes an ACC to company customer and in Vista 
               there is no possibility to provide Contact person information 
               while making ACC so in this case we consider customer has chosen 
               Contact person information same as Authorized customer information
               and delete contact person if exist */
            IF bNewCust.CustIdType = "CIF" AND
               NOT llNewCust THEN DO:
               FIND FIRST CustContact WHERE
                          CustContact.Brand = Syst.Var:gcBrand AND
                          CustContact.CustNum = bNewCust.CustNum AND
                          CustContact.CustType = {&CUSTCONTACT_CONTACT}
                          EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE CustContact THEN DO:
                  IF llDoEvent THEN RUN StarEventMakeDeleteEventWithMemo(
                                          lhCustContact,
                                          Syst.Var:katun,
                                          lcMemo).
                  DELETE CustContact.
               END.
            END.

            /* Electronic Invoice project */
            lcEmail = ttCustomer.Email.
         END.

         IF liReqCnt = 1 AND lcEmail > "" AND
            bNewCust.EMail <> lcEmail THEN DO:
            bNewCust.EMail = lcEmail.

            IF bNewCust.DelType EQ {&INV_DEL_TYPE_EMAIL} OR
               bNewCust.DelType EQ {&INV_DEL_TYPE_EMAIL_PENDING} THEN DO:

               /* Cancel Ongoing Email Activation Request and create new */
               IF fPendingEmailActRequest(INPUT bNewCust.Custnum) THEN
                  fCancelPendingEmailActRequest(
                                INPUT bNewCust.Custnum,
                                INPUT "Customer email address is changed").

               liRequest = fEmailInvoiceRequest(INPUT Func.Common:mMakeTS(),
                                                INPUT TODAY,
                                                INPUT Syst.Var:katun,
                                                INPUT 0, /* msseq */
                                                INPUT "", /* cli */
                                                INPUT bNewCust.CustNum,
                                                INPUT {&REQUEST_SOURCE_ACC},
                                                INPUT bNewCust.EMail,
                                                INPUT 0, /*orderid*/
                                                OUTPUT lcResult).
               IF liRequest > 0 THEN DO:
                  /* If Email already validated then mark DelType EMAIL */
                  IF liRequest = 1 THEN
                     bNewCust.DelType = {&INV_DEL_TYPE_EMAIL}.
                  ELSE
                     bNewCust.DelType = {&INV_DEL_TYPE_EMAIL_PENDING}.
               END. /* IF liRequest > 0 THEN DO: */
            END. /* IF bNewCust.DelType EQ {&INV_DEL_TYPE_EMAIL} */
         END. /* IF lcEmail > "" AND bNewCust.EMail <> lcEmail THEN DO: */

         CASE liReqCnt:
         WHEN 1 THEN ASSIGN 
            liNewOwner          = bNewCust.CustNum
            bNewCust.InvCust    = bNewCust.CustNum WHEN llNewCust.
         
         WHEN 2 THEN ASSIGN 
            liNewInvCust        = bNewCust.CustNum
            bNewCust.InvCust    = bNewCust.CustNum.

         WHEN 3 THEN ASSIGN 
            liNewUser           = bNewCust.CustNum
            bNewCust.InvCust    = liNewInvCust.
         END CASE.
         
         IF NOT llNewCust AND llDoEvent THEN
            RUN StarEventMakeModifyEventWithMemo(
                                    lhCustomer,
                                    Syst.Var:katun,
                                    lcMemo).
         
         IF llNewCust THEN DO:
            ASSIGN 
            bNewCust.ChgStamp   = Func.Common:mMakeTS()
            bNewCust.CreUser    = Syst.Var:katun
            bNewCust.PaymCust   = liNewOwner
            bNewCust.AgrCust    = liNewOwner
            bNewCust.RepCust    = bNewCust.CustNum
            bNewCust.RateCust   = bNewCust.CustNum 
            bNewCust.ContrBeg   = TODAY.

            /* default counter limits; for all, also prepaids */
            fTMRLimit2Customer(bNewCust.CustNum).
         END.  
          
      END.

      ELSE liCreated[liReqCnt] = 0.
   END. 

   /* an existing customer has been chosen to be the new one 
      (and now the new customers have also been created) */
   
   DO liReqCnt = 1 TO 3: /* For now only agr.customer change */
       
      CASE liReqCnt:
      WHEN 1 THEN ASSIGN liChkCust  = liNewOwner
                         lcCustType = "agr.customer".
      WHEN 2 THEN ASSIGN liChkCust  = liNewInvCust
                         lcCustType = "inv.customer".
      WHEN 3 THEN ASSIGN liChkCust  = liNewUser
                         lcCustType = "user".
      END CASE.
      
      IF liChkCust = 0 THEN DO:
         fReqError("Customer not defined for " + lcCustType).
         RETURN.
      END. 
   
      FIND bNewCust WHERE bNewCust.CustNum = liChkCust
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bNewCust THEN DO:
         fReqError("New " + lcCustType + " customer not found").
         RETURN.
      END. 

   END. 

   /* move fees, create a new msowner etc. */
   RUN pMsCustMove (liNewOwner,
                    liNewInvCust,
                    liNewUser).

   FIND FIRST MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK.

   /* DSS related activity */
   RUN pUpdateDSSAccount(INPUT liNewUser,         /* NEW CustNum */
                         INPUT MsRequest.CustNum, /* OLD CustNum */
                         INPUT MsRequest.MsRequest,
                         INPUT {&REQUEST_SOURCE_ACC},
                         INPUT MsRequest.ReqDparam1, /* Act. Stamp */
                         INPUT MsRequest.UserCode).

   /* DSS2 related activity */
   RUN pUpdateDSS2Account(MobSub.Msseq,
                          INPUT MsRequest.MsRequest,
                          INPUT {&REQUEST_SOURCE_ACC},
                          INPUT MsRequest.ReqDparam1 /* Act. Stamp */
                          ).

   /* send SMS */
   IF MsRequest.SendSMS = 1 THEN 
      RUN Mm/acc_sendsms.p(MsRequest.MsRequest,
                      liNewOwner,
                      "Done",
                      "").  
   
   /* fee from owner change to new customer (actually to invoice customer) */
   IF MsRequest.CreateFees THEN DO:
   
      RUN Mm/create_charge_comp.p(
         {&REQUEST_SOURCE_MANUAL_TMS},
         Mobsub.MsSeq,   
         MsRequest.UserCode,
         MsRequest.ReqDParam2,  
         "ACC_" + (IF MobSub.PayType THEN "PREPAID" ELSE "POSTPAID"),
         MsRequest.MsRequest, 
         OUTPUT liChargeReqId) NO-ERROR.

   END.

   IF MobSub.MultiSimId > 0 AND
      MobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} THEN DO:

      FIND Customer NO-LOCK WHERE
           Customer.Custnum = liNewOwner.

      FIND FIRST bMobsub NO-LOCK USE-INDEX MultiSimID WHERE
                 bMobsub.Brand  = Syst.Var:gcBrand AND
                 bMobSub.MultiSimId = Mobsub.MultiSimId AND
                 bMobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} AND
                 bMobSub.Custnum = MsRequest.Custnum NO-ERROR.
      
      IF AVAIL bMobSub AND bMobSub.Custnum NE liNewOwner THEN DO:
         
         FIND FIRST CustomerReport WHERE
                    CustomerReport.Custnum = Customer.Custnum NO-LOCK NO-ERROR.
         

         lcInfo = Func.ValidateACC:mCheckSubscriptionForACC(bMobSub.MsSeq,
                                                            0,
                                                            0,
                                                            MsRequest.ReqSource).
      
         IF NOT ENTRY(1,lcInfo,"|") BEGINS "ERROR" THEN DO:

            lcCode = fCreateAccDataParam(
                      (BUFFER Customer:HANDLE),
                      "", /* salesman */
                      Customer.AuthCustIdType,
                      Customer.AuthCustId,
                      (IF AVAIL CustomerReport 
                       THEN CustomerReport.StreetCode ELSE ""),
                      (IF AVAIL CustomerReport 
                       THEN CustomerReport.CityCode ELSE ""),
                      (IF AVAIL CustomerReport 
                       THEN CustomerReport.TownCode ELSE ""),
                       "",
                      OUTPUT lcInfo).
                  
            IF NOT lcInfo > "" THEN DO:
               liRequest = fMSCustChangeRequest(
                  bMobSub.MsSeq,
                  "agrcust",
                  liNewOwner,
                  bMobSub.AgrCust,
                  lcCode,
                  MsRequest.actstamp,
                  FALSE, /* create fees */
                  0,
                  TRUE,  /* send SMS */
                  "",
                  ({&REQUEST_SOURCE_ACC}),
                  MsRequest.MsRequest,
                  "", /*contract id*/
                  OUTPUT lcInfo).

               /* memo */
               IF lcInfo > "" THEN
                  Func.Common:mWriteMemo("MobSub",
                       STRING(bMobSub.MsSeq),
                       bMobSub.CustNum,
                       "ACC Failed",
                       ("ACC to " + 
                       STRING(liNewOwner) + " failed: " + lcInfo)).
               ELSE DO:
                  FIND FIRST bMsRequest WHERE
                             bMsRequest.MsRequest = liRequest
                       EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAIL bMsRequest THEN
                     bMsRequest.ActStamp = MsRequest.ActStamp.
                  RELEASE bMsRequest.
               END.
            END.
         END.
      END.
   END.

   RUN pHandleAdditionalLines
      (msrequest.msseq,
       msrequest.custnum, /* old owner */
       liNewOwner,
       Msrequest.MsRequest,
       MsRequest.ActStamp).
   
   fSetSpecialTTFLimit(liNewOwner,
                       MobSub.CLIType).

   RUN pFinalize(MsRequest.ActStamp, 
                 MsRequest.MsSeq,
                 MsRequest.Cli).
   fReqStatus(2,""). 
   IF AVAIL bAccOrder THEN
      fSetOrderStatus(bAccOrder.OrderId,"6").  
   
END PROCEDURE.


PROCEDURE pMsCustMove:

   DEF INPUT PARAMETER iiNewOwner   AS INT NO-UNDO. 
   DEF INPUT PARAMETER iiNewInvCust AS INT NO-UNDO. 
   DEF INPUT PARAMETER iiNewUser    AS INT NO-UNDO. 
   
   DEF VAR liNewTarget  AS INT  NO-UNDO.
   DEF VAR liFeePeriod  AS INT  NO-UNDO. 
   DEF VAR ldtFeeFrom   AS DATE NO-UNDO.
   DEF VAR ldtFeeTo     AS DATE NO-UNDO. 
   DEF VAR ldFeeAmt     AS DEC  NO-UNDO. 
   DEF VAR liPerDays    AS INT  NO-UNDO.
   DEF VAR ldtFeeDate   AS DATE NO-UNDO.
   DEF VAR ldEndStamp   AS DEC  NO-UNDO.
   DEF VAR lcError AS CHARACTER NO-UNDO. 
   DEF VAR lcMandate    AS CHAR NO-UNDO. 
   DEF VAR ldaDate      AS DATE NO-UNDO. 
   DEF VAR liManTime    AS INT  NO-UNDO. 
   DEF VAR lcDate       AS CHAR NO-UNDO. 
   DEF VAR liOldAgrCust AS INT  NO-UNDO.

   DEF BUFFER bBillTarget FOR BillTarget.
   DEF BUFFER bOwner      FOR MSOwner.
   DEF BUFFER bOMobSub    FOR MobSub.
   
   DEF BUFFER bFixedFee   FOR FixedFee.
   DEF BUFFER bFFItem     FOR FFItem.
   DEF BUFFER bFatime     FOR Fatime.
   DEF BUFFER bLimit      FOR Limit. 
   DEF BUFFER bCounter    FOR TMCounter.
   DEF BUFFER lbMLMobSub  FOR MobSub.

   IF MsRequest.ReqIParam4 > 0 THEN DO:
      FIND OrderAction NO-LOCK WHERE
           OrderAction.Brand = Syst.Var:gcBrand AND
           OrderAction.OrderID = Order.OrderID AND
           OrderAction.ItemType = "Mandate" NO-ERROR.
      IF AVAIL OrderAction THEN lcMandate = OrderAction.ItemKey.
   END.
   ELSE IF NUM-ENTRIES(MsRequest.ReqCParam1,";") >= 35 THEN
       lcMandate = ENTRY(35,MsRequest.ReqCParam1,";").

   FIND FIRST MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK.
   
   /* user has been changed -> mark new user nbr to related tables */
   IF MobSub.CustNum NE iiNewUser AND iiNewUser > 0 THEN DO:
      
      /* make sure that new user has a similar billing target */
      FIND bBillTarget WHERE
           bBillTarget.CustNum    = MobSub.CustNum AND
           bBillTarget.BillTarget = MobSub.BillTarget NO-LOCK NO-ERROR.
         
      FIND BillTarget WHERE
           BillTarget.CustNum    = iiNewUser AND
           BillTarget.BillTarget = MobSub.BillTarget NO-LOCK NO-ERROR.
   
      liNewTarget = 0.

      IF AVAILABLE BillTarget AND AVAILABLE bBillTarget THEN DO: 

         IF BillTarget.RatePlan NE bBillTarget.RatePlan OR
            BillTarget.DiscPlan NE bBillTarget.DiscPlan
         THEN DO liReqCnt = 30 TO 99:
            IF NOT CAN-FIND(BillTarget WHERE
                            BillTarget.CustNum    = iiNewUser AND
                            BillTarget.BillTarget = liReqCnt)
            THEN DO:
               liNewTarget = liReqCnt.
              LEAVE.
            END.
         END.   
      END.

      IF NOT AVAILABLE BillTarget OR liNewTarget > 0 THEN DO:
      
         CREATE BillTarget.
      
         IF AVAILABLE bBillTarget THEN DO:
            BUFFER-COPY bBillTarget EXCEPT CustNum TO BillTarget.
            IF liNewTarget > 0 THEN BillTarget.BillTarget = liNewTarget.
         END.

         ELSE DO:
        
            FIND CLIType WHERE 
                 CLIType.Brand   = Syst.Var:gcBrand AND
                 CLIType.CLIType = MobSub.CLIType NO-LOCK NO-ERROR.
            IF AVAILABLE CLIType THEN ASSIGN 
               BillTarget.BillTarget = CLIType.BillTarget
               BillTarget.RatePlan   = CLIType.PricePlan
               BillTarget.DiscPlan   = CLIType.DiscPlan.
            ELSE ASSIGN 
               BillTarget.BillTarget = 1.
         END.
      
         BillTarget.CustNum = iiNewUser.
      END.

      /* change period */
      ASSIGN liFeePeriod = YEAR(ldtActDate) * 100 + MONTH(ldtActDate)
             ldtFeeFrom  = DATE(MONTH(ldtActDate),1,YEAR(ldtActDate)).
      IF MONTH(ldtActDate) = 12
      THEN ldtFeeTo = DATE(12,31,YEAR(ldtActDate)).
      ELSE ldtFeeTo = DATE(MONTH(ldtActDate) + 1,1,YEAR(ldtActDate)) - 1.
    
      FOR EACH FATime EXCLUSIVE-LOCK USE-INDEX MobSub WHERE
               FATime.Brand  = Syst.Var:gcBrand      AND
               FATime.MsSeq  = MobSub.MsSeq AND
               FATime.InvNum = 0            AND
               FATime.Period >= liFeePeriod:
      
         IF FATime.CustNum = iiNewUser THEN NEXT.
         
         /* split fatimes for change month to both customers */
         IF Fatime.Period = liFeePeriod THEN DO:
      
            CREATE bFatime.
            BUFFER-COPY Fatime EXCEPT FatNum FatID TO bFatime.
            ASSIGN 
               bFatime.FatNum   = NEXT-VALUE(FTSeq)
               bFatime.FatID    = NEXT-VALUE(FT-Seq)
               bFatime.CustNum  = iiNewUser
               ldFeeAmt         = (Fatime.Amt - Fatime.Used - Fatime.TransQty) 
                                  / (ldtFeeTo - ldtFeeFrom + 1)
               ldFeeAmt         = ldFeeAmt * (ldtActDate - ldtFeeFrom + 1)
               Fatime.Amt       = ldFeeAmt
               bFatime.Amt      = bFatime.Amt - Fatime.Amt
               bFatime.Used     = 0 
               bFatime.TransQty = 0.

            /* memo */
            Func.Common:mWriteMemo("FATime",
                       STRING(bFatime.FatNum),
                       bFatime.CustNum,
                       "User Change",
                       "Transferred from customer " + STRING(Fatime.CustNum)).
         END.

         /* transfer newer fatimes totally */
         ELSE DO:
            /* memo */
            Func.Common:mWriteMemo("FATime",
                       STRING(Fatime.FatNum),
                       iiNewUser,
                       "User Change",
                       "Transferred from customer " + STRING(Fatime.CustNum)).

            FATime.CustNum = iiNewUser. 
         END.   
      END.
   
      
      /* MSISDN */
      FIND FIRST MSISDN NO-LOCK WHERE 
                 MSISDN.Brand = Syst.Var:gcBrand AND
                 MSISDN.CLI = MobSub.CLI NO-ERROR.
      IF AVAILABLE MSISDN THEN DO:
         fMakeMsidnHistoryTS(RECID(MSISDN),MsRequest.ActStamp).
      END.
      FIND CURRENT MSISDN EXCLUSIVE-LOCK.
      ASSIGN MsIsdn.CustNum = iiNewUser.
      
      /* IMSI */
      FIND FIRST IMSI EXCLUSIVE-LOCK WHERE
                 IMSI.ICC = MobSub.ICC NO-ERROR.
      IF AVAILABLE IMSI THEN IMSI.CustNum = iiNewUser.
             
      /* SIM */
      FIND FIRST SIM EXCLUSIVE-LOCK WHERE
                 SIM.Brand = Syst.Var:gcBrand   AND
                 SIM.ICC   = MobSub.ICC NO-ERROR.
      IF AVAILABLE SIM THEN SIM.CustNum = iiNewUser.
      
      FOR EACH TMCounter NO-LOCK WHERE
               TMCounter.MsSeq   = MobSub.MsSeq AND
               TMCounter.CustNum = MobSub.CustNum AND
               TMCounter.ToDate > ldtActDate:
               
         FIND FIRST bCounter WHERE
                    bCounter.MsSeq     = MobSub.MsSeq AND
                    bCounter.TMRuleSeq = TMCounter.TMRuleSeq AND
                    bCounter.ToDate    = TMCounter.ToDate    AND
                    bCounter.CustNum   = iiNewUser NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bCounter THEN DO:
         
            FIND bCounter WHERE RECID(bCounter) = RECID(TMCounter)
               EXCLUSIVE-LOCK.
            bCounter.CustNum = iiNewUser.
            NEXT.
         END.
         
         FIND CURRENT bCounter EXCLUSIVE-LOCK.
         bCounter.Amount = bCounter.Amount + TMCounter.Amount.
         IF TMCounter.LimitID > bCounter.LimitID THEN 
            bCounter.LimitID = TMCounter.LimitID.

         FIND bCounter WHERE RECID(bCounter) = RECID(TMCounter) EXCLUSIVE-LOCK.
         DELETE bCounter.
      END.      

   END. 

   /* invoicing customer is changed -> move fees */
   IF MobSub.InvCust NE iiNewInvCust AND iiNewInvCust > 0 THEN DO:

      /* fees belong to old customer till the end of change month,
         unless change happens on 1. day before 3 am */
      ldtFeeDate = ldtActDate.
  
      IF DAY(ldtFeeDate) > 1 OR liActTime > 10800 THEN DO:
         IF MONTH(ldtFeeDate) = 12
         THEN ldtFeeDate = DATE(12,31,YEAR(ldtFeeDate)).
         ELSE ldtFeeDate = DATE(MONTH(ldtFeeDate) + 1,1,YEAR(ldtFeeDate)) - 1. 
      END.

      liFeePeriod = YEAR(ldtFeeDate) * 10000 + 
                    MONTH(ldtFeeDate) * 100  + 
                    DAY(ldtFeeDate).

      /* Exclude DSS fixedfee logic here, DSS logic in fdss.i */
      FOR EACH FixedFee EXCLUSIVE-LOCK WHERE
               FixedFee.Brand     = Syst.Var:gcBrand              AND
               FixedFee.HostTable = "MobSub"             AND 
               FixedFee.KeyValue  = STRING(MobSub.MsSeq) AND
               FixedFee.InUse     = TRUE                 AND
               FixedFee.CalcObj  <> {&DSS}:
         
         IF NOT CAN-FIND(FIRST FFItem OF FixedFee WHERE
                               FFItem.Billed = FALSE AND
                               FFItem.Concerns[2] > liFeePeriod)
         THEN NEXT.
         
         /* shouldn't be, but better to check */   
         IF FixedFee.CustNum = iiNewInvCust THEN NEXT. 
         
         IF LOOKUP(FixedFee.FinancedResult,{&TF_STATUSES_BANK}) > 0 OR
            FixedFee.FinancedResult EQ {&TF_STATUS_SENT_TO_BANK} THEN DO:

            FOR FIRST FixedFeeTF EXCLUSIVE-LOCK WHERE
                      FixedFeeTF.FFNum = FixedFee.FFNum:
               ASSIGN
                  FixedFeeTF.CancelStatus = "NEW"
                  FixedFeeTF.CancelReason = {&TF_CANCEL_ACC}.
             END.
         END.
         ELSE IF FixedFee.FinancedResult EQ {&TF_STATUS_WAITING_SENDING} OR
                 FixedFee.FinancedResult EQ {&TF_STATUS_HOLD_SENDING} THEN
                 FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ACC}.
         
         CREATE bFixedFee.
         BUFFER-COPY FixedFee EXCEPT 
            CustNum
            FFNum
            FinancedResult
            TFBank
            IFSStatus
            TO bFixedFee.

         ASSIGN bFixedFee.FFNum   = NEXT-VALUE(Contract)
                bFixedFee.CustNum = iiNewInvCust
                bFixedFee.BegDate = ldtFeeDate
                bFixedFee.FinancedResult = {&TF_STATUS_YOIGO} 
                  WHEN bFixedFee.BillCode BEGINS "PAYTERM" OR
                       bFixedFee.BillCode EQ "RVTERM".
                
         FOR EACH FFItem OF FixedFee EXCLUSIVE-LOCK WHERE
                  FFItem.Billed      = FALSE AND
                  FFItem.Concerns[2] > liFeePeriod:
              
            /* split for both customers */
            IF FFItem.Concerns[1] < liFeePeriod THEN DO:
            
               CREATE bFFItem.
               BUFFER-COPY FFItem EXCEPT FFNum FFItemNum TO bFFItem.
               ASSIGN bFFItem.FFItemNum   = NEXT-VALUE(Citem)
                      bFFItem.FFNum       = bFixedFee.FFNum
                      bFFItem.CustNum     = bFixedFee.CustNum
                      bFFItem.Concerns[1] = liFeePeriod.
                      
               ldtFeeFrom = fInt2Date(FFItem.Concerns[1],1).
               ldtFeeTo   = fInt2Date(FFItem.Concerns[2],2).
               
               ASSIGN ldFeeAmt            = FFItem.Amt / 
                                            (ldtFeeFrom - ldtFeeTo + 1)
                      ldFeeAmt            = ldFeeAmt * 
                                            (ldtFeeDate - ldtFeeFrom)
                      FFItem.Amt          = ldFeeAmt
                      FFItem.Concerns[2]  = YEAR(ldtFeeDate - 1) * 10000 + 
                                            MONTH(ldtFeeDate - 1) * 100  +
                                            DAY(ldtFeeDate - 1)
                      bFFItem.Amt         = bFFItem.Amt - FFItem.Amt.

            END. 

            ELSE ASSIGN FFItem.FFNum   = bFixedFee.FFNum
                        FFItem.CustNum = bFixedFee.CustNum.
         END. 

         FIND LAST FFItem OF FixedFee NO-LOCK NO-ERROR.
         IF NOT AVAILABLE FFItem 
         THEN ASSIGN FixedFee.EndPer = FixedFee.BegPer
                     FixedFee.InUse  = FALSE.
         ELSE FixedFee.EndPer = FFItem.BillPeriod.
         
         FIND FIRST bFFItem OF bFixedFee NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bFFItem
         THEN bFixedFee.InUse = FALSE.
         ELSE bFixedFee.BegPer = bFFItem.BillPer.

         /* memo */
         Func.Common:mWriteMemo("FixedFee",
                    STRING(bFixedFee.FFNum),
                    bFixedFee.CustNum,
                    "User Change",
                    "Transferred from customer " + STRING(FixedFee.CustNum)).
      END. 

      liFeePeriod = YEAR(ldtActDate) * 100 + MONTH(ldtActDate).

      /* Move future RVTERM single fees */
      FOR EACH SingleFee USE-INDEX Custnum WHERE
               SingleFee.Brand       = Syst.Var:gcBrand AND
               SingleFee.Custnum     = Mobsub.InvCust AND
               SingleFee.HostTable   = "Mobsub" AND
               SingleFee.KeyValue    = STRING(Mobsub.MsSeq) AND
               SingleFee.BillPeriod >= liFeePeriod AND
               SingleFee.CalcObj     = "RVTERM" EXCLUSIVE-LOCK:

         IF SingleFee.Billed AND
            CAN-FIND(FIRST Invoice NO-LOCK WHERE
                           Invoice.Invnum = SingleFee.Invnum AND
                           Invoice.InvType = 1) THEN NEXT.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSingleFee).
         ASSIGN
            SingleFee.Custnum = iiNewInvCust
            SingleFee.BillCode = "RVTERMF".
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSingleFee).
      END.
     
      /*YDR-2360 changes*/ 
      /* billing denials */
      FOR EACH Limit NO-LOCK USE-INDEX MsSeq WHERE
               Limit.MsSeq     = MsOwner.MsSeq   AND
               Limit.LimitType = 3               AND
               Limit.TMRuleSeq = 0               AND
               Limit.ToDate   >= ldtActDate      AND
               Limit.LimitID   = 0               AND
               Limit.CustNum   = MobSub.InvCust:
               
          CREATE bLimit.
          BUFFER-COPY Limit EXCEPT FromDate TO bLimit.
          ASSIGN
             bLimit.FromDate = ldtActDate
             bLimit.CustNum  = iiNewInvCust.

          RELEASE bLimit.

      END.

   END.
   
   /* end current msowner and create a new one */
   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq AND
              MsOwner.TSEnd >= 99999999
   EXCLUSIVE-LOCK NO-ERROR.
   
   IF NOT AVAILABLE MsOwner THEN 
   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq EXCLUSIVE-LOCK.
   
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsOwner).

   ASSIGN ldEndStamp    = MsOwner.TSEnd
          MsOwner.TSEnd = Func.Common:mSecOffSet(MsRequest.ActStamp,-1).

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsOwner).
          
   CREATE bOwner.
   BUFFER-COPY MsOwner EXCEPT CustNum TSBeg TSEnd CLIEvent TO bOwner.
   ASSIGN bOwner.CustNum = iiNewUser    WHEN iiNewUser > 0
          bOwner.TSBeg   = MsRequest.ActStamp
          bOwner.TSEnd   = ldEndStamp
          bOwner.InvCust = iiNewInvCust WHEN iiNewInvCust > 0
          bOwner.AgrCust = iiNewOwner
          bOwner.CLIEvent = "ACC".

   IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                     CLIType.CLIType = MobSub.CLIType AND
                     CLIType.PayType = {&CLITYPE_PAYTYPE_POSTPAID}) THEN DO:

      /* Create Mandate for Subscription and store it into MsOwner */
      Func.Common:mSplitTS(MsRequest.CreStamp, OUTPUT ldaDate, OUTPUT liManTime).
   
      IF LENGTH(lcMandate) >= 30 THEN
         ASSIGN
             bOwner.MandateId   = lcMandate
             lcDate             = SUBSTRING(lcMandate,25,6)
             bOwner.MandateDate = DATE(INT(SUBSTR(lcDate,3,2)),
                                       INT(SUBSTR(lcDate,5,2)),
                                       INT(SUBSTR(lcDate,1,2)) + 2000) NO-ERROR.
      ELSE DO:         
         fCalculateMandate(MobSub.MsSeq,ldaDate,iiNewUser, OUTPUT lcMandate).   
         ASSIGN
            bOwner.MandateId   = lcMandate
            bOwner.MandateDate = ldaDate.
      END.
   END.
   
   IF llDoEvent THEN fMakeCreateEvent((BUFFER bOwner:HANDLE),
                                      "",
                                      Syst.Var:katun,
                                      "").

   RELEASE MsOwner.
   RELEASE bOwner.       
   
   /* new user to mobsub */
   FIND CURRENT MobSub EXCLUSIVE-LOCK.

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobSub).
   liOldAgrCust = MobSub.AgrCust.
   ASSIGN MobSub.CustNum = iiNewUser    WHEN iiNewUser > 0
          MobSub.InvCust = iiNewInvCust WHEN iiNewInvCust > 0
          MobSub.AgrCust = iiNewOwner.
   
   /* Extraline discount will be closed WITH last date of previous month 
      if ACC is done on Extraline subscription */
   IF fCLITypeIsExtraLine(MobSub.CliType) AND 
      MobSub.MultiSimId                   GT 0  AND 
      MobSub.MultiSimType                 EQ {&MULTISIMTYPE_EXTRALINE} THEN DO:

      FIND FIRST lbMLMobSub EXCLUSIVE-LOCK WHERE 
                 lbMLMobSub.MsSeq = MobSub.MultiSimId NO-ERROR. 
      
      IF AVAIL lbMLMobSub THEN DO:
         
         /* Discount has to be closed with last date of previous month */ 
         /* ACC request will be procesed on 1st day of every month     */
         fCloseExtraLineDiscount(MobSub.MsSeq,
                                 MobSub.CliType + "DISC",
                                 TODAY).
         
         /* Hard association is also removed because ACC was done to extraline */
         ASSIGN MobSub.MultiSimId       = 0
                MobSub.MultiSimType     = 0.

      END.           
                              
   END.

   /* ADDLINE-20 Additional Line */
   IF LOOKUP(MobSub.CliType, {&ADDLINE_CLITYPES}) > 0 THEN DO:
      fCloseAddLineDiscount(MobSub.AgrCust,
                            MobSub.MsSeq,
                            MobSub.CLIType,
                            TODAY - 1).
   END.
   ELSE IF fIsConvergenceTariff(MobSub.CLIType) THEN DO:
      FOR EACH bOMobSub NO-LOCK WHERE
               bOMobSub.Brand   = Syst.Var:gcBrand      AND
               bOMobSub.AgrCust = liOldAgrCust AND
               bOMobSub.MsSeq  <> MobSub.MsSeq AND
               LOOKUP(bOMobSub.CliType, {&ADDLINE_CLITYPES}) > 0:
            fCloseAddLineDiscount(bOMobSub.AgrCust,
                                  bOMobSub.MsSeq,
                                  bOMobSub.CLIType,
                                  TODAY - 1).
      END.
      fDeactivateTVService(MobSub.MsSeq, MsRequest.UserCode).
   END.

   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobSub).
   
   fACCInvoiceTarget
      (MobSub.MsSeq,
       OUTPUT lcError).
   
   IF lcError NE "" THEN 
      Func.Common:mWriteMemo("MobSub",
                 STRING(MobSub.MsSeq),
                 MobSub.CustNum,
                 "User Change",
                 "Invoice target creation failed: " + lcError).

   RELEASE MobSub.
   
END PROCEDURE.

PROCEDURE pCreditCheck:

   DEF VAR liCheck  AS INT  NO-UNDO.
   DEF VAR lcResult AS CHAR NO-UNDO.
   
   liCheck = fSubRequest (MSRequest.MSSeq,
                          MSRequest.CLI,
                          MSRequest.CustNum,
                          FALSE, /* Fees */
                          FALSE, /* SendSMS */
                          MSRequest.UserCode,
                          Func.Common:mMakeTS(),
                          "CREDITCHECK",
                          "",
                          33,
                          MSRequest.MSrequest,
                          1,    /* 1 = mandatory */
                          OUTPUT lcResult ).

    IF liCheck = 0 THEN RETURN "ERROR:" + lcResult.
    ELSE RETURN "".
    
END PROCEDURE. /* pCreditCheck */

/* common DSS and DSS2 logic */
PROCEDURE pUpdateDSSAccount:

   DEF INPUT PARAMETER iiNewCustNum  AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiOldCustNum  AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiMainRequest AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiMainSource  AS CHAR NO-UNDO.
   DEF INPUT PARAMETER ideActStamp   AS DEC  NO-UNDO.
   DEF INPUT PARAMETER icUserCode    AS CHAR NO-UNDO.

   DEF VAR liOldDSSMsSeq             AS INT  NO-UNDO.
   DEF VAR liNewDSSMsSeq             AS INT  NO-UNDO.
   DEF VAR llDSSActiveOnOLD          AS LOG  NO-UNDO.
   DEF VAR llDSSActiveOnNEW          AS LOG  NO-UNDO.
   DEF VAR llOngoingDSSTermOnOLD     AS LOG  NO-UNDO.
   DEF VAR llOngoingDSSTermOnNEW     AS LOG  NO-UNDO.
   DEF VAR llDSSTransferred          AS LOG  NO-UNDO.
   DEF VAR ldeDSSLimit               AS DEC  NO-UNDO.
   DEF VAR ldeDataBundleLimits       AS DEC  NO-UNDO.
   DEF VAR ldeEndStamp               AS DEC  NO-UNDO.
   DEF VAR ldEndDate                 AS DATE NO-UNDO.
   DEF VAR ldtActDate                AS DATE NO-UNDO.
   DEF VAR liActTime                 AS INT  NO-UNDO.
   DEF VAR lcError                   AS CHAR NO-UNDO.
   DEF VAR ldeCurrMonthLimit         AS DEC  NO-UNDO.
   DEF VAR ldeConsumedData           AS DEC  NO-UNDO.
   DEF VAR ldeOtherMonthLimit        AS DEC  NO-UNDO.
   DEF VAR liRequest                 AS INT  NO-UNDO.
   DEF VAR liTermRequest             AS INT  NO-UNDO.
   DEF VAR lcOldDSSBundleId          AS CHAR NO-UNDO.
   DEF VAR lcNewDSSBundleId          AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO. 
      
   lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").
   
   DEF BUFFER bMobSub     FOR MobSub.
   DEF BUFFER bMsRequest  FOR MsRequest.

   /* No action if paytype is prepaid */
   IF MobSub.PayType THEN RETURN.

   Func.Common:mSplitTS(INPUT ideActStamp,OUTPUT ldtActDate,OUTPUT liActTime).

   /* end old bundles to the end of previous month */
   IF DAY(ldtActDate) = 1 THEN
      ASSIGN ldeEndStamp = Func.Common:mMake2DT(ldtActDate - 1,86399)
             ldEndDate   = (ldtActDate - 1).
   ELSE
      ASSIGN ldEndDate   = Func.Common:mLastDayOfMonth(ldtActDate)
             ldeEndStamp = Func.Common:mMake2DT(ldEndDate,86399).

   /* If ongoing DSS termination request then return */
   llOngoingDSSTermOnOLD = fOngoingDSSTerm(INPUT iiOldCustNum,
                                           INPUT ldeEndStamp).

   llOngoingDSSTermOnNEW = fOngoingDSSTerm(INPUT iiNewCustNum,
                                           INPUT ldeEndStamp).

   /* Return if DSS will not be active on NEW customer or OLD for next month */
   IF llOngoingDSSTermOnOLD AND llOngoingDSSTermOnNEW AND
      NOT (MobSub.MultiSimId > 0 AND
           MobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY}) THEN RETURN.

   llDSSActiveOnOLD = fGetDSSMsSeqLimit(INPUT iiOldCustNum,
                                        INPUT ideActStamp,
                                        OUTPUT liOldDSSMsSeq,
                                        OUTPUT ldeDSSLimit,
                                        OUTPUT lcOldDSSBundleId).
   llDSSActiveOnNEW = fGetDSSMsSeqLimit(INPUT iiNewCustNum,
                                        INPUT ideActStamp,
                                        OUTPUT liNewDSSMsSeq,
                                        OUTPUT ldeDSSLimit,
                                        OUTPUT lcNewDSSBundleId).

   /* Return if DSS is neither active on NEW customer nor OLD */
   IF NOT llDSSActiveOnOLD AND NOT llDSSActiveOnNEW AND
      NOT (MobSub.MultiSimId > 0 AND
           MobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY}) THEN RETURN.

   /* MULTISIM HANDLING BEGINS */

   /* Terminate multisim DSS from old customer */
   IF llDSSActiveOnOLD AND NOT llOngoingDSSTermOnOLD AND
      lcOldDSSBundleId EQ {&DSS} AND
      MobSub.MultiSimId > 0 AND
      MobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
      CAN-FIND (FIRST bMobsub NO-LOCK USE-INDEX MultiSimID WHERE
                      bMobsub.Brand  = Syst.Var:gcBrand AND
                      bMobSub.MultiSimId = Mobsub.MultiSimId AND
                      bMobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} AND
                      bMobSub.Custnum = iiOldCustNum) AND
      MobSub.MsSeq = liOldDSSMsSeq THEN DO:

      RUN pUpdateDSSNetwork(INPUT MobSub.MsSeq,
                            INPUT MobSub.CLI,
                            INPUT iiOldCustNum, /* Old customer num */
                            INPUT "DELETE",
                            INPUT "",           /* Optional param list */
                            INPUT iiMainRequest,
                            INPUT ldeEndStamp,
                            INPUT iiMainSource,
                            INPUT {&DSS}).

      IF NOT llDSSActiveOnNEW OR llOngoingDSSTermOnNEW THEN RETURN.
   END.

   /* Activate multisim DSS for new customer */
   IF NOT llDSSActiveOnNEW AND
      MobSub.MultiSimId > 0 AND
      MobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} AND
      CAN-FIND (FIRST bMobsub NO-LOCK USE-INDEX MultiSimID WHERE
                      bMobsub.Brand  = Syst.Var:gcBrand AND
                      bMobSub.MultiSimId = Mobsub.MultiSimId AND
                      bMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
                      bMobSub.Custnum = iiNewCustnum) AND
      NOT fOngoingDSSAct(iiNewCustnum) AND
      fIsDSSAllowed(INPUT  MobSub.CustNum,
                    INPUT  MobSub.MsSeq,
                    INPUT  (IF ideActStamp < Func.Common:mMakeTS() THEN Func.Common:mMakeTS()
                            ELSE ideActStamp),
                    INPUT {&DSS},
                    INPUT  "",
                    OUTPUT ldeCurrMonthLimit,
                    OUTPUT ldeConsumedData,
                    OUTPUT ldeOtherMonthLimit,
                    OUTPUT lcError) THEN DO:

      FIND FIRST bMobsub NO-LOCK USE-INDEX MultiSimID WHERE
                 bMobsub.Brand  = Syst.Var:gcBrand AND
                 bMobSub.MultiSimId = Mobsub.MultiSimId AND
                 bMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
                 bMobSub.Custnum = iiNewCustnum NO-ERROR.
      IF NOT AVAIL bMobSub THEN RETURN.

      liRequest = fDSSRequest(bMobSub.MsSeq,
                              bMobSub.CustNum,
                              "CREATE",
                              lcError,
                              "DSS",
                              ideActStamp,
                              {&REQUEST_SOURCE_ACC},
                              "",
                              TRUE, /* fees */
                              iiMainRequest,
                              FALSE,
                              OUTPUT lcError).
      IF liRequest > 0 THEN DO:
         FIND FIRST bMsRequest NO-LOCK WHERE
                    bMsRequest.MsSeq   = bMobSub.MsSeq   AND
                    bMsRequest.ReqType = {&REQTYPE_DSS}  AND
                    bMsRequest.ReqCParam1 = "DELETE"     AND
                    bMsRequest.ActStamp  <= ldeEndStamp  AND
                    LOOKUP(STRING(bMsRequest.ReqStatus),
                           {&REQ_INACTIVE_STATUSES}) = 0 NO-ERROR.
         IF NOT AVAIL bMsRequest THEN
            FIND FIRST bMsRequest NO-LOCK WHERE
                       bMsRequest.Brand = Syst.Var:gcBrand           AND
                       bMsRequest.ReqType = {&REQTYPE_DSS}  AND
                       bMsRequest.CustNum = iiOldCustNum    AND
                       bMsRequest.ReqCParam1 = "DELETE"     AND
                       bMsRequest.ActStamp  <= ldeEndStamp  AND
                       LOOKUP(STRING(bMsRequest.ReqStatus),
                              {&REQ_INACTIVE_STATUSES}) = 0 NO-ERROR.
         IF AVAIL bMsRequest THEN
            liTermRequest = bMsRequest.MsRequest.

         IF liTermRequest > 0 THEN DO:
            FIND FIRST bMsRequest WHERE
                       bMsRequest.MsRequest = liRequest EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL bMsRequest THEN
               bMsRequest.ReqIParam2 = liTermRequest.
         END.
      END.
      ELSE
         Func.Common:mWriteMemo("Customer",
                    STRING(iiNewCustnum),
                    iiOldCustNum,
                    "DSS Bundle Activation Failed",
                    "DSS Bundle activation (MultiSIM ACC) failed:" + lcError).
      RETURN.
   END.
   
   /* OLD CUSTOMER HANDLING BEGINS */

   /* If Current Subs. has data bundle then */
   /* it should be reduced from DSS group   */
   ldeDataBundleLimits = fGetActiveBundleLimit(INPUT MobSub.MsSeq,
                                               INPUT ideActStamp).

   llOngoingDSSTermOnOLD = fOngoingDSSTerm(INPUT iiOldCustNum,
                                           INPUT ldeEndStamp).

   /* If DSS or DSS2 is active on OLD customer */
   IF llDSSActiveOnOLD AND NOT llOngoingDSSTermOnOLD AND
      NOT (lcOldDSSBundleId EQ "DSS2" AND
           LOOKUP(Mobsub.CLIType,lcAllowedDSS2SubsType) = 0) THEN DO:

      /* If directly linked to DSS */
      IF MobSub.MsSeq = liOldDSSMsSeq THEN DO:
         IF fIsDSSTransferAllowed(INPUT MobSub.CLI,
                                  INPUT iiOldCustNum,
                                  INPUT ideActStamp,
                                  INPUT lcOldDSSBundleId,
                                  OUTPUT liOldDSSMsSeq,
                                  OUTPUT lcError) THEN DO:
            /* Transfer DSS/UPSELL */
            IF fTransferDSS(INPUT MobSub.MsSeq,
                            INPUT liOldDSSMsSeq,
                            INPUT ldEndDate,
                            INPUT icUserCode,
                            INPUT "ACC",
                            OUTPUT lcError) THEN DO:
               llDSSTransferred = TRUE.

               Func.Common:mWriteMemo("Customer",
                          STRING(iiOldCustNum),
                          iiOldCustNum,
                          "DSS Bundle/UPSELL",
                          "DSS Bundle/UPSELL is transferred from Subs.Id " +
                          STRING(MobSub.MsSeq) + " to Subs. Id " +
                          STRING(liOldDSSMsSeq)).
            END. /* IF fTransferDSS(INPUT MobSub.MsSeq,INPUT liDSSMsSeq, */
            ELSE
               Func.Common:mWriteMemo("Customer",
                       STRING(iiOldCustNum),
                       iiOldCustNum,
                       "DSS Bundle/UPSELL Transfer Failed",
                       "DSS Bundle/UPSELL was not transferred from Subs.Id " +
                       STRING(MobSub.MsSeq) + " to Subs. Id " +
                       STRING(liOldDSSMsSeq) + ". " + lcError).
         END. /* IF fIsDSSTransferAllowed(INPUT MobSub.CLI */

         /* DSS is not transferred - delete DSS group now */
         IF NOT llDSSTransferred THEN
            RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                                  INPUT Mobsub.CLI,
                                  INPUT iiOldCustNum, /* Old customer num */
                                  INPUT "DELETE",
                                  INPUT "",           /* Optional param list */
                                  INPUT iiMainRequest,
                                  INPUT ldeEndStamp,
                                  INPUT iiMainSource,
                                  INPUT lcOldDSSBundleId).
         /* If DSS is transferred then remove subs. from DSS group */
         ELSE DO:
            RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                                  INPUT Mobsub.CLI,
                                  INPUT iiOldCustNum, /* Old customer num    */
                                  INPUT "REMOVE",
                                  INPUT "",           /* Optional param list */
                                  INPUT iiMainRequest,
                                  INPUT ldeEndStamp,
                                  INPUT iiMainSource,
                                  INPUT lcOldDSSBundleId).

            /* Reduce other data active bundle limit from DSS group */
            IF ldeDataBundleLimits > 0 THEN DO:
               RUN pUpdateDSSLimit(INPUT iiOldCustNum,
                                   INPUT "REMOVE",
                                   INPUT ldeDataBundleLimits,
                                   INPUT 0,
                                   INPUT ideActStamp,
                                   OUTPUT ldeDSSLimit).

               RUN pUpdateDSSNetworkLimit(INPUT Mobsub.MsSeq,
                                          INPUT iiOldCustNum,
                                          INPUT ldeDSSLimit,
                                          INPUT "LIMIT",
                                          INPUT FALSE,
                                          INPUT iiMainRequest,
                                          INPUT ideActStamp,
                                          INPUT iiMainSource,
                                          INPUT {&DSS}).
            END. /* IF ldeDataBundleLimits > 0 THEN DO: */
         END. /* ELSE DO: */

      END. /* IF oiDSSMsSeq = MobSub.MsSeq THEN DO: */
      /* DSS is not linked directly */
      ELSE DO:
         /* If 2nd last postpaid subs. is being transferred then Delete DSS */
         IF NOT fCanDSSKeepActive(INPUT iiOldCustNum,
                                  INPUT Mobsub.MsSeq,
                                  INPUT ideActStamp,
                                  INPUT lcOldDSSBundleId,
                                  OUTPUT lcError) THEN
            RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                                  INPUT Mobsub.CLI,
                                  INPUT iiOldCustNum, /* Old customer num */
                                  INPUT "DELETE",
                                  INPUT "",           /* Optional param list */
                                  INPUT iiMainRequest,
                                  INPUT ldeEndStamp,
                                  INPUT iiMainSource,
                                  INPUT lcOldDSSBundleId).
         /* Otherwise just remove subs. from DSS group */
         ELSE DO:
            RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                                  INPUT Mobsub.CLI,
                                  INPUT iiOldCustNum, /* Old customer num    */
                                  INPUT "REMOVE",
                                  INPUT "",           /* Optional param list */
                                  INPUT iiMainRequest,
                                  INPUT ldeEndStamp,
                                  INPUT iiMainSource,
                                  INPUT lcOldDSSBundleId).

            /* Reduce other data active bundle limit from DSS group */
            IF ldeDataBundleLimits > 0 THEN DO:
               RUN pUpdateDSSLimit(INPUT iiOldCustNum,
                                   INPUT "REMOVE",
                                   INPUT ldeDataBundleLimits,
                                   INPUT 0,
                                   INPUT ideActStamp,
                                   OUTPUT ldeDSSLimit).

               RUN pUpdateDSSNetworkLimit(INPUT Mobsub.MsSeq,
                                          INPUT iiOldCustNum,
                                          INPUT ldeDSSLimit,
                                          INPUT "LIMIT",
                                          INPUT FALSE,
                                          INPUT iiMainRequest,
                                          INPUT ideActStamp,
                                          INPUT iiMainSource,
                                          INPUT lcOldDSSBundleId).
            END. /* IF ldeDataBundleLimits > 0 THEN DO: */
         END. /* ELSE DO: */
      END. /* ELSE DO: */
   END. /* IF llDSSActiveOnOLD THEN DO: */

   /* NEW CUSTOMER HANDLING BEGINS */

   /* Add subs. to DSS group if DSS is active on new customer */
   IF llDSSActiveOnNEW AND NOT llOngoingDSSTermOnNEW THEN DO:

      IF lcNewDSSBundleId EQ "DSS2" AND
         LOOKUP(Mobsub.CLIType,lcAllowedDSS2SubsType) = 0 THEN RETURN.

      RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                            INPUT Mobsub.CLI,
                            INPUT iiNewCustNum, /* New customer num    */
                            INPUT "ADD",
                            INPUT "",           /* Optional param list */
                            INPUT iiMainRequest,
                            INPUT ideActStamp,
                            INPUT iiMainSource,
                            INPUT lcNewDSSBundleId).

      /* Add other data active bundle limit to DSS group */
      IF ldeDataBundleLimits > 0 THEN DO:
         ldeDSSLimit = 0.
         RUN pUpdateDSSLimit(INPUT iiNewCustNum,
                             INPUT "UPDATE",
                             INPUT ldeDataBundleLimits,
                             INPUT 0,
                             INPUT ideActStamp,
                             OUTPUT ldeDSSLimit).

         RUN pUpdateDSSNetworkLimit(INPUT Mobsub.MsSeq,
                                    INPUT iiNewCustNum,
                                    INPUT ldeDSSLimit,
                                    INPUT "LIMIT",
                                    INPUT FALSE,
                                    INPUT iiMainRequest,
                                    INPUT ideActStamp,
                                    INPUT iiMainSource,
                                    INPUT lcNewDSSBundleId).
      END. /* IF ldeDataBundleLimits > 0 THEN DO: */

   END. /* IF llDSSActiveOnNEW THEN DO: */

END PROCEDURE. /* PROCEDURE pUpdateDSSAccount: */

PROCEDURE pUpdateDSS2Account:

   DEF INPUT PARAMETER iiMsSeq       AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiMainRequest AS INT  NO-UNDO. 
   DEF INPUT PARAMETER icMainSource  AS CHAR NO-UNDO.
   DEF INPUT PARAMETER ideActStamp   AS DEC  NO-UNDO.

   DEF VAR lcError AS CHAR NO-UNDO. 
   DEF VAR liRequest                 AS INT  NO-UNDO.
   DEF VAR lcDSS2SubsType            AS CHAR NO-UNDO. 
   DEF VAR liDSSMsSeq                AS INT  NO-UNDO.
   DEF VAR ldeCurrTS AS DEC NO-UNDO. 

   DEF BUFFER Mobsub FOR Mobsub.

   FIND Mobsub NO-LOCK WHERE
        Mobsub.MsSeQ = iiMsSeq NO-ERROR.
   IF NOT AVAIL Mobsub THEN RETURN.
   
   /* No action if paytype is prepaid */
   IF MobSub.PayType THEN RETURN.

   lcDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").
   
   IF LOOKUP(MobSub.CLIType,lcDSS2SubsType) = 0 THEN RETURN.
   
   ldeCurrTS = Func.Common:mMakeTS().

   IF fIsDSSActive(MobSub.Custnum,
                   (IF ideActStamp > ldeCurrTS
                    THEN ideActStamp
                    ELSE ldeCurrTS)) THEN RETURN.
   
   IF fOngoingDSSAct(MobSub.Custnum) THEN RETURN.

   IF NOT fIsDSSActivationAllowed(MobSub.Custnum,
                                  MobSub.MsSeq,
                                  ideActStamp,
                                  {&DSS2},
                                  OUTPUT liDSSMsSeq,
                                  OUTPUT lcError) THEN RETURN.

   liRequest = fDSSRequest(liDSSMsSeq,
                           MobSub.CustNum,
                           "CREATE",
                           "",
                           "DSS2",
                           (IF ideActStamp > ldeCurrTS
                            THEN ideActStamp
                            ELSE ldeCurrTS),
                           icMainSource,
                           "",
                           TRUE, /* create fees */
                           iiMainRequest,
                           FALSE,
                           OUTPUT lcError).
   IF liRequest = 0 THEN
      /* write possible error to a memo */
      Func.Common:mWriteMemo("MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.Custnum,
                       "DSS2 activation failed",
                       lcError).

END PROCEDURE. /* PROCEDURE pUpdateDSS2Account: */

PROCEDURE pHandleAdditionalLines:

   DEF INPUT PARAMETER iiMsSeq AS INT NO-UNDO.
   DEF INPUT PARAMETER iiOldCustNum AS INT NO-UNDO.
   DEF INPUT PARAMETER iiNewCustNum AS INT NO-UNDO.
   DEF INPUT PARAMETER iiMsRequest AS INT NO-UNDO.
   DEF INPUT PARAMETER ideActStamp AS DEC NO-UNDO.
   
   DEF VAR lcInfo AS CHAR NO-UNDO. 
   DEF VAR lcAction AS CHAR NO-UNDO. 
   DEF VAR liSubLimit AS INT NO-UNDO. 
   DEF VAR liSubs AS INT NO-UNDO. 
   DEF VAR llIsACCAllowed AS LOG NO-UNDO. 
   DEF VAR lcACCParams AS CHAR NO-UNDO. 
   DEF VAR liRequest AS INT NO-UNDO. 

   DEF VAR liMsisdnStat AS INT NO-UNDO. 
   DEF VAR liSimStat AS INT NO-UNDO. 
   DEF VAR liQuartime AS INT NO-UNDO. 
   DEF VAR lcError AS CHAR NO-UNDO. 
      
   DEF VAR ldeActStamp AS DEC NO-UNDO. 
   DEF VAR ldaACCDate AS DATE NO-UNDO. 
   DEF VAR liTime AS INT NO-UNDO.
   DEF VAR liActLimit AS INT NO-UNDO.
   DEF VAR liActs AS INT NO-UNDO.

   
   DEF BUFFER MobSub FOR MobSub.
   DEF BUFFER bMobSub FOR MobSub.
   DEF BUFFER Customer FOR Customer.
   DEF BUFFER bMsRequest FOR MsRequest.
   
   FIND MobSub NO-LOCK WHERE
        MobSub.MsSeq = iiMsSeq NO-ERROR.
   IF NOT AVAIL Mobsub OR NOT
      CAN-FIND(FIRST CLIType NO-LOCK WHERE   
                     CLIType.Brand = Syst.Var:gcBrand AND
                     CLIType.CLIType = Mobsub.TariffBundle AND
                     CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) 
      THEN RETURN.

   FIND Customer NO-LOCK WHERE
        Customer.Custnum = iiNewCustnum NO-ERROR.
   IF NOT AVAIL Customer THEN RETURN.

   /* check if other primary line exists */
   FOR EACH bMobsub NO-LOCK WHERE
            bMobsub.Brand  = Syst.Var:gcBrand AND
            bMobSub.Custnum = iiOldCustNum AND
            bMobSub.PayType = FALSE,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = Syst.Var:gcBrand AND
            CLIType.CLIType = bMobSub.TariffBundle AND
            CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN}:
      RETURN.
   END.

   llIsACCAllowed = Func.ValidateACC:mSubscriptionLimitCheck(INPUT Customer.OrgId,
                                          INPUT Customer.CustIdType,
                                          fIsSelfEmpl(Customer.Category),
                                          fIsPro(Customer.Category),
                                          1,
                                          OUTPUT liSubLimit,
                                          OUTPUT liSubs,
                                          OUTPUT liActLimit,
                                          OUTPUT liActs).
   
   IF llIsACCAllowed THEN DO:

      FIND FIRST CustomerReport WHERE
                 CustomerReport.Custnum = Customer.Custnum NO-LOCK NO-ERROR.
      lcACCParams = fCreateAccDataParam(
                (BUFFER Customer:HANDLE),
                "", /* salesman */
                Customer.AuthCustIdType,
                Customer.AuthCustId,
                (IF AVAIL CustomerReport 
                 THEN CustomerReport.StreetCode ELSE ""),
                (IF AVAIL CustomerReport 
                 THEN CustomerReport.CityCode ELSE ""),
                (IF AVAIL CustomerReport 
                 THEN CustomerReport.TownCode ELSE ""),
                 "",
                OUTPUT lcInfo).
      Func.Common:mSplitTS(ideActStamp, OUTPUT ldaACCDate, OUTPUT liTime).

   END.


   ADDITIONAL_SUBS:
   FOR EACH bMobsub NO-LOCK WHERE
            bMobsub.Brand  = Syst.Var:gcBrand AND
            bMobSub.Custnum = iiOldCustNum AND
            bMobSub.PayType = FALSE,
      FIRST CLIType NO-LOCK WHERE
            CLIType.Brand = Syst.Var:gcBrand AND
            CLIType.CLIType = (IF bMobSub.TariffBundle > ""
                               THEN bMobSub.TariffBundle
                               ELSE bMobSub.CLIType) AND
            CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL}:

      IF llIsACCAllowed AND
         liSubs < liSubLimit THEN DO:


         lcInfo = Func.ValidateACC:mCheckSubscriptionForACC(bMobSub.MsSeq,
                                                            0,
                                                            0,
                                                            MsRequest.ReqSource).

         IF NOT ENTRY(1,lcInfo,"|") BEGINS "ERROR" THEN DO:
            liRequest = fMSCustChangeRequest(
               bMobSub.MsSeq,
               "agrcust",
               iiNewCustnum,
               bMobSub.AgrCust,
               lcACCParams,
               ideActstamp,
               FALSE, /* create fees */
               0,
               TRUE,  /* send SMS */
               "",
               ({&REQUEST_SOURCE_ACC}),
               iiMsRequest,
               "", /*contract is*/
               OUTPUT lcInfo).
         
            IF lcInfo > "" THEN
               /* memo */
               Func.Common:mWriteMemo("MobSub",
                          STRING(bMobSub.MsSeq),
                          bMobSub.CustNum,
                          "Additional SIM ACC failed",
                          ("ACC to " + 
                          STRING(iiNewCustnum) + " failed: " + lcInfo)).
            ELSE DO:
              FIND FIRST bMsRequest WHERE
                         bMsRequest.MsRequest = liRequest
                   EXCLUSIVE-LOCK NO-ERROR.
              IF AVAIL bMsRequest THEN
                 bMsRequest.ActStamp = ideActstamp.
              RELEASE bMsRequest.

              liSubs = liSubs + 1.
              NEXT ADDITIONAL_SUBS.
            END.
         END.
      END.
         
      IF fHasPendingRequests
         (bMobSub.MsSeq,
          bMobSub.CLI,
          CLIType.LineType) THEN NEXT ADDITIONAL_SUBS.
     
     /* TODO: should be changed to STC ?? */
      fTermAdditionalSim(bMobSub.Msseq,
                         bMobSub.CLI,
                         bMobSub.CustNum,
                         {&SUBSCRIPTION_TERM_REASON_ADDITIONALSIM},
                         ldaACCDate,
                         {&REQUEST_SOURCE_ACC},
                         iiMsRequest,
                         OUTPUT lcError).

   END.

END PROCEDURE.
