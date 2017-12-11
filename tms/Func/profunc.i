/* ----------------------------------------------------------------------
  MODULE .......: profunc.i
  TASK .........: Functions for handling Yoigo PRO related functionality
                  Reference: YPRO-project
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......:
  CREATED ......: 24.5.2017
  CHANGED ......:
  ------------------------------------------------------------------------*/
 
{Syst/tmsconst.i}
&IF "{&YOIGOPROFUNC_I}" NE "YES"
&THEN
&GLOBAL-DEFINE YOIGOPROFUNC_I YES
{Func/fmakemsreq.i}
{Func/orderfunc.i}
{Func/femailinvoice.i}
{Func/email.i}
{Func/fixedlinefunc.i}
{Func/cparam2.i}

/* check pro */
FUNCTION fIsPro RETURNS LOGICAL
   (icCategory AS CHAR):

   DEF BUFFER CustCat FOR CustCat.

   FIND FIRST CustCat NO-LOCK WHERE
              CustCat.Brand EQ Syst.Var:gcBrand AND
              CustCat.Category EQ icCategory NO-ERROR.
              
   IF AVAIL CustCat AND Custcat.pro THEN RETURN TRUE.
   RETURN FALSE.
END.

/* check self employee */
FUNCTION fIsSelfEmpl RETURNS LOGICAL
   (icCategory AS CHAR):

   DEF BUFFER CustCat FOR CustCat.

   FIND FIRST CustCat NO-LOCK WHERE
              CustCat.Brand EQ Syst.Var:gcBrand AND
              CustCat.Category EQ icCategory NO-ERROR.

   IF AVAIL CustCat AND INDEX(custcat.catname, "self") > 0 THEN RETURN TRUE.
   RETURN FALSE.
END.

FUNCTION fGetSegment RETURNS CHAR
   (iiCustNum AS INT,
    iiorderId AS INT):

   DEF BUFFER bCustomer FOR Customer.
   DEF BUFFER bOrderCustomer FOR OrderCustomer.
   DEF BUFFER CustCat FOR CustCat.

   DEF VAR lcCategory AS CHAR NO-UNDO.

   IF iiCustNum > 0 THEN
   FIND FIRST bCustomer NO-LOCK  WHERE
              bCustomer.CustNum EQ iiCustNum
              NO-ERROR.

   IF AVAIL bCustomer THEN lcCategory = bCustomer.category.
   ELSE IF iiOrderid > 0 THEN DO:
      FIND FIRST bOrdercustomer WHERE
                 bOrdercustomer.brand EQ Syst.Var:gcBrand AND
                 bOrdercustomer.orderid EQ iiorderid AND
                 bOrdercustomer.rowtype EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
                 NO-LOCK NO-ERROR.
      IF AVAIL bOrdercustomer THEN lcCategory = bOrdercustomer.category.
   END.

   IF lcCategory > "" THEN DO:
      FIND FIRST CustCat NO-LOCK WHERE
                 CustCat.Brand = Syst.Var:gcBrand AND
                 CustCat.Category = lcCategory
                 NO-ERROR.
      IF AVAIL CustCat THEN
         RETURN CustCat.Segment.
   END.
   RETURN "Consumer".
END.

FUNCTION fGetSVAOffer RETURNS CHARACTER
    (icCliType AS CHAR,
     icDCEvent AS CHAR):

    DEFINE BUFFER bf_DiscOfferItem FOR OfferItem.

    FOR EACH Offer WHERE Offer.Brand       = Syst.Var:gcBrand AND 
                         Offer.Active      = True             AND
                         Offer.ToDate     >= TODAY            AND 
                         Offer.Offer_type = "extra_offer"     NO-LOCK,
        FIRST OfferItem WHERE OfferItem.Brand     = Syst.Var:gcBrand     AND 
                              OfferItem.Offer     = Offer.Offer          AND 
                              OfferItem.ItemType  = "OptionalBundleItem" AND
                              OfferItem.ItemKey   = icDCEvent            AND 
                              OfferItem.EndStamp >= ldeCurrentTS         NO-LOCK,
        FIRST OfferCriteria WHERE OfferCriteria.Brand        = Syst.Var:gcBrand      AND 
                                  OfferCriteria.Offer        = Offer.Offer           AND 
                                  OfferCriteria.CriteriaType = "CLIType"             AND
                                  OfferCriteria.BeginStamp  <= ldeCurrentTS          AND 
                                  LOOKUP(icCliType, OfferCriteria.IncludedValue) > 0 AND 
                                  OfferCriteria.EndStamp    >= ldeCurrentTS          NO-LOCK:

        FIND FIRST bf_DiscOfferItem WHERE bf_DiscOfferItem.Brand     = Syst.Var:gcBrand AND 
                                          bf_DiscOfferItem.Offer     = Offer.Offer      AND 
                                          bf_DiscOfferItem.ItemType  = "DiscountPlan"   AND
                                          bf_DiscOfferItem.EndStamp >= ldeCurrentTS     NO-LOCK NO-ERROR.
        IF AVAIL bf_DiscOfferItem AND bf_DiscOfferItem.ItemKey <> "" THEN 
            RETURN bf_DiscOfferItem.ItemKey.
    END.

    RETURN "".  

END FUNCTION.

/*'off', 'on', 'cancel activation', 'cancel deactivation'*/
FUNCTION fMakeProActRequest RETURNS INT(
   INPUT iiMsSeq AS INT,
   INPUT icContr AS CHAR,
   INPUT idActStamp AS DEC,
   INPUT icParam1 AS CHAR,
   INPUT icParam2 AS CHAR,
   INPUT icAction AS CHAR, 
   OUTPUT ocErr AS CHAR):

   DEF VAR liRequest AS INT  NO-UNDO.
   DEF VAR liReqType AS INT  NO-UNDO.
   DEF VAR lcError   AS CHAR NO-UNDO.
   DEF VAR lcParams  AS CHAR NO-UNDO.
   DEF VAR lcOffer   AS CHAR NO-UNDO. 

   DEF BUFFER bOwner FOR MSOwner. 

   FIND FIRST bOwner WHERE bOwner.MsSeq = iiMsSeq NO-LOCK NO-ERROR.

   IF NOT AVAIL bOwner THEN RETURN 0.
   lcParams = "SVA". /*To indicate that we are handling SVA request.*/
   IF icParam1 NE "" THEN DO:
      IF icParam1 EQ "no" THEN lcParams =  lcParams + "_NO_WAIT".
      ELSE DO:
         lcParams =  lcParams + "|" + icParam1.
         IF icParam2 NE "" THEN DO:
            lcParams = lcParams + "|" + icParam2. 
         END.
      END. 
   END.

   ASSIGN 
       lcOffer  = fGetSVAOffer(bOwner.CliType, icContr)
       lcParams = FILL("|", (4 - NUM-ENTRIES(lcParams)))
       lcParams = lcParams + lcOffer.

   DO TRANS:
   IF icAction BEGINS "cancel" THEN DO:
      IF icAction EQ "cancel activation" THEN 
         liReqType = {&REQTYPE_CONTRACT_ACTIVATION}.
      ELSE IF icAction EQ "cancel deactivation" THEN
         liReqType = {&REQTYPE_CONTRACT_TERMINATION}.
      ELSE DO:
         ocErr = "Incorrect request".
         RETURN 0.
      END.
         
      FIND FIRST MsRequest WHERE
                 MsRequest.Brand EQ Syst.Var:gcBrand AND
                 MsRequest.ReqType EQ liReqType AND
                 MsRequest.ReqStatus EQ {&REQUEST_STATUS_CONFIRMATION_PENDING} AND
                 MsRequest.ReqCParam3 EQ icContr NO-ERROR.
      IF NOT AVAIL MsRequest THEN DO:
         ocErr = "Cancellation not possible, request not found".
         RETURN 0.
      END.
      fReqStatus(4, "SVA Operation Cancellation").
      RETURN MsRequest.MsRequest.
        
   END.
   ELSE IF icAction EQ "on" THEN 
      icAction = "act".
   ELSE IF icAction EQ "off" THEN 
      icAction = "term".

      liRequest = fPCActionRequest(iiMsSeq,
                                   icContr,
                                   icAction,
                                   idActStamp,
                                   TRUE, /* fees */
                                   {&REQUEST_SOURCE_CONTRACT_ACTIVATION},
                                   "",
                                   0,
                                   FALSE,
                                   "",
                                   0,
                                   0,
                                   lcParams,
                                   OUTPUT ocErr).
   END. /*Trans*/  
   RETURN liRequest. /*bCreaReq.MsRequest.*/
END.



/*Function returns TRUE if the order exsists and it is done from PRO channel.*/
FUNCTION fIsProOrder RETURNS LOGICAL
   (iiOrderID AS INT):

   DEF BUFFER Order FOR Order.

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand EQ  Syst.Var:gcBrand AND
              Order.OrderID EQ iiOrderID NO-ERROR.

   IF INDEX(Order.orderchannel,"PRO") > 0 THEN
      RETURN TRUE.
   ELSE RETURN FALSE.
   
END.

/*Function returns True if a tariff can be defined as 2P tariff.
NOTE: False is returned in real false cases and also in error cases. */
FUNCTION fIs3PTariff RETURNS LOGICAL
   (icCliType AS CHAR):

   DEF BUFFER CLIType FOR CLIType.

   FIND FIRST CLIType NO-LOCK WHERE
              CLIType.Brand EQ Syst.Var:gcBrand AND
              CLIType.CliType EQ icCLIType NO-ERROR.
   IF AVAIL CliType AND
            CliType.TariffType EQ {&CLITYPE_TARIFFTYPE_CONVERGENT}  THEN
      RETURN TRUE.

   RETURN FALSE.
END.


/*STC is restricted from Prepaid to postpaid and 2P*/
FUNCTION fValidateProSTC RETURNS CHAR
   (iiCustomer AS INT,
    icCurrCLIType AS CHAR,
    icNewCLIType AS CHAR):

   DEF BUFFER bCurr FOR CLIType.
   DEF BUFFER bNew FOR CLIType.
   DEF BUFFER Customer FOR Customer.
   DEF BUFFER mobsub FOR mobsub.

   DEF VAR ll3PFound AS LOGICAL NO-UNDO.
   
   FIND FIRST Customer NO-LOCK WHERE
              Customer.CustNum EQ iiCustomer NO-ERROR.
   IF NOT AVAIL Customer THEN RETURN "Customer not found".
   IF NOT fIsPro(Customer.Category) THEN RETURN "". /*No PRO logic needed*/

   FIND FIRST bCurr NO-LOCK WHERE
              bCurr.Brand EQ Syst.Var:gcBrand AND
              bCurr.Clitype EQ icCurrCLIType NO-ERROR.
   IF NOT AVAIL bCurr THEN RETURN "Incorrect CLIType".

   FIND FIRST bNew NO-LOCK WHERE
              bNew.Brand EQ Syst.Var:gcBrand AND
              bNew.Clitype EQ icNewCLIType NO-ERROR.
   IF NOT AVAIL bNew THEN RETURN "Incorrect CLIType".

   IF bCurr.PayType EQ {&CLITYPE_PAYTYPE_PREPAID} THEN RETURN "". /*No PRO logic for prepaid*/

   IF bNew.Paytype EQ {&CLITYPE_PAYTYPE_PREPAID} THEN 
      RETURN "STC to Prepaid is not allowed for Pro customer".
   IF fIsFixedOnly(bNew.Clitype) AND NOT fIs3PTariff(bCurr.Clitype)  THEN DO:
      ll3PFound = FALSE.
      FOR EACH Mobsub WHERE
               Mobsub.brand EQ Syst.Var:gcBrand AND
               Mobsub.custnum EQ iiCustomer:
         IF NOT fIs3PTariff(MobSub.clitype) THEN NEXT.
         ELSE ll3PFound = TRUE.
      END.
      IF NOT ll3PFound THEN RETURN "STC to 2P is not allowed for Pro customer".  /* STC to pro allowed from mobile to 2P and if there is still convergent left and YPPI-5 3P to 2P */
   END.   
   RETURN "".
END.

/*tested, ok*/
/*Function seeks COFF order for given Msrequest.
If the order is not found the function returns an error code.*/
FUNCTION fFindCOFFOrder RETURNS CHAR
   (iiMsSeq AS INT):
   DEF BUFFER bOrder FOR Order.

   FOR EACH bOrder NO-LOCK WHERE
            bOrder.MsSeq EQ iiMsSeq BY CrStamp DESC:
      IF fIsConvergenceTariff(bOrder.CLIType) THEN
         RETURN STRING(bOrder.OrderId).
   END.

   RETURN "ERROR: Order not found for mobsub " + STRING(iiMsSeq).
END.

FUNCTION fGetProFeemodel RETURNS CHAR
   (INPUT icCliType AS CHAR):

   DEF BUFFER CLIType FOR CLIType.
   DEF BUFFER DayCampaign FOR DayCampaign.

   FOR FIRST CLIType NO-LOCK WHERE
             CLIType.Brand = Syst.Var:gcBrand AND
             CLIType.CLIType = icCliType AND
             CLIType.FixedBundle > "",
       FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand = Syst.Var:gcBrand AND
             DayCampaign.DCEvent = CLIType.FixedBundle:
      RETURN DayCampaign.FeeModel.
   END.
   FOR FIRST CLIType NO-LOCK WHERE
             CLIType.Brand = Syst.Var:gcBrand AND
             CLIType.CLIType = icCliType,
       FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand = Syst.Var:gcBrand AND
             DayCampaign.DCEvent = CLIType.clitype:
      RETURN DayCampaign.FeeModel.
   END.
   RETURN "".
END.

FUNCTION fSendEmailByRequest RETURNS CHAR
   (iiMsRequest AS INT,
    icTemplate AS CHAR):

   DEF VAR lcOutput     AS CHAR NO-UNDO.
   DEF VAR lcMailFile   AS CHAR NO-UNDO.
   DEF VAR lcMailHeader AS CHAR NO-UNDO.
   DEF VAR lcReplace    AS CHAR NO-UNDO.
   DEF VAR lcMailDir    AS CHAR NO-UNDO.
   DEF VAR lcStatus     AS CHAR NO-UNDO.

   DEF BUFFER bMsRequest FOR MsRequest.
   DEF BUFFER bCustomer  FOR Customer.
   DEF BUFFER bMobSub    FOR MobSub.
   DEF BUFFER bCliType   FOR CliType.

   FIND FIRST bMsRequest NO-LOCK WHERE
              bMsRequest.MsRequest EQ iiMsRequest NO-ERROR.
   IF NOT AVAIL bMsRequest THEN RETURN "ERROR: Request not found " +
                                   STRING(iiMsRequest).
   FIND FIRST bCustomer NO-LOCK WHERE
              bCustomer.CustNum EQ bMsRequest.CustNum.
    IF NOT AVAIL bCustomer THEN
       RETURN "ERROR: Customer of requst not found " + STRING(iiMsRequest).

   FIND FIRST bMobSub WHERE bMobSub.MsSeq = bMsRequest.MsSeq NO-LOCK NO-ERROR.
   IF AVAIL bMobSub THEN 
       FIND FIRST bCliType WHERE bCliType.CliType = bMobSub.CliType NO-LOCK NO-ERROR.

   lcOutput = fGetEmailText("EMAIL",
                             icTemplate,
                             1,
                             OUTPUT lcMailHeader).
   
   IF lcOutput EQ ""/* OR lcMailHeader EQ ""*/ THEN
      RETURN "ERROR: Email content fetching error" +
             STRING(BCustomer.CustID) + " " +
             STRING(icTemplate).

   /*Seek tags:*/
   IF INDEX(lcOutput, "#CUSTNAME") > 0 THEN
      lcOutput = REPLACE(lcOutput, "#CUSTNAME", Func.Common:mDispCustName(BUFFER bCustomer)).
   
   IF INDEX(lcOutput, "#ORDERID") > 0 THEN
      lcOutput = REPLACE(lcOutput, "#ORDERID", fFindCOFFOrder(bMsRequest.MsSeq)).
   
   IF INDEX(lcOutput, "#CONTRACTID") > 0 THEN
      lcOutput = REPLACE(lcOutput, "#CONTRACTID", (IF AVAIL bMobSub THEN bMobSub.FixedNumber ELSE STRING(bMsRequest.MsSeq))).

   IF INDEX(lcOutput, "#CUSTTYPE") > 0 THEN
      lcOutput = REPLACE(lcOutput, "#CUSTTYPE", STRING(bCustomer.CustIdType)).
   
   IF INDEX(lcOutput, "#CUSTID") > 0 THEN
      lcOutput = REPLACE(lcOutput, "#CUSTID", STRING(bCustomer.Orgid)).

   IF INDEX(lcOutput, "#PROVINCE") > 0 THEN
   DO:
      lcOutput = REPLACE(lcOutput, "#PROVINCE"       , bCustomer.Address).
      lcOutput = REPLACE(lcOutput, "#CITY"           , bCustomer.PostOffice).
      lcOutput = REPLACE(lcOutput, "#POSTALCODE"     , bCustomer.ZipCode).
   END.

   IF INDEX(lcOutput, "#LANGUAGE") > 0 THEN
   DO:
       FIND Language WHERE Language.Language = bCustomer.Language NO-LOCK NO-ERROR.
       lcOutput = REPLACE(lcOutput, "#LANGUAGE", (IF AVAIL Language THEN Language.LangName ELSE "")).
   END.

   IF INDEX(lcOutput, "#PRODUCT") > 0 THEN
       lcOutput = REPLACE(lcOutput, "#PRODUCT", (IF AVAIL bCliType THEN bCliType.CliName ELSE "")).

   IF INDEX(lcOutput, "#SFID") > 0 THEN
       lcOutput = REPLACE(lcOutput, "#SFID", "").

   IF INDEX(lcOutput, "#SECRET") > 0 THEN
       lcOutput = REPLACE(lcOutput, "#SECRET", bCustomer.RobinsonsLimit).    

   IF INDEX(lcOutput, "#EMAIL") > 0 THEN 
   DO:
      IF NUM-ENTRIES(bMSRequest.ReqCparam6) GT 2 AND ENTRY(3,bMSRequest.ReqCparam6, "|") <> "" THEN
         lcReplace = ENTRY(3,bMSRequest.ReqCparam6, "|").
      ELSE IF NUM-ENTRIES(bMSRequest.ReqCparam6) EQ 2 AND ENTRY(2,bMSRequest.ReqCparam6, "|") <> "" THEN
         lcReplace = ENTRY(2,bMSRequest.ReqCparam6, "|").
      ELSE 
         lcReplace = bCustomer.Email.

      lcOutput = REPLACE(lcOutput, "#EMAIL", lcReplace).
   END.

   IF INDEX(lcOutput, "#NUMBER") > 0 THEN 
   DO:
      lcReplace = ENTRY(2,bMsRequest.Reqcparam6, "|").
      lcOutput = REPLACE(lcOutput, "#NUMBER", lcReplace).
   END.

   IF INDEX(lcMailHeader, "#STATUS") > 0 THEN 
   DO:
      IF bMsRequest.ReqType EQ 9 THEN 
      DO:
         IF bMsRequest.ReqStatus EQ {&REQUEST_STATUS_CONFIRMATION_PENDING} THEN
             lcStatus = "3 - Pending deactivation".
         ELSE 
             lcStatus = "0 - Inactive".
      END.
      ELSE IF bMsRequest.reqtype EQ 8 THEN 
      DO:
         IF bMsRequest.reqstatus EQ {&REQUEST_STATUS_CONFIRMATION_PENDING} THEN
             lcStatus = "2 - Pending activation".
         ELSE 
             lcStatus = "1 - Active".
      END.

      lcMailHeader = REPLACE(lcMailHeader, "#STATUS", lcStatus).
   END.

   /*Set email sending parameters*/
   /*lcMailDir = "/tmp/". /*To be sure that we have some place*/
   lcMailDir = fCParam("YPRO", "YPRO_SVA_email_dir").
   lcMailFile = lcMailDir + "SVA_email" + STRING(bMsRequest.Msrequest) + ".txt".
   
   OUTPUT STREAM soutfile to VALUE(lcMailFile).
   PUT STREAM soutfile UNFORMATTED lcOutput skip.
   */
   ASSIGN
      xMailFrom = fCParamC("DefEmailSender")
      xMailAddr = fCParam("YPRO", "SVA_BO_EMAIL_ADDRESS")
      xMailSubj = lcMailHeader.
      SendMaileInvoice(lcOutput, "", "").

   /*Used email file removal or saving to logs?*/

   RETURN "".

END.

FUNCTION fgetActiveReplacement RETURNS CHAR (INPUT icClitype AS CHAR):
   DEF VAR lcSubsMappings AS CHAR NO-UNDO.
   DEF VAR lcMappedSubs AS CHAR NO-UNDO.
   DEF VAR lcSubsFrom AS CHAR NO-UNDO.
   DEF VAR lcSubsTo AS CHAR NO-UNDO.
   DEF VAR liLoop AS INT NO-UNDO.

   lcSubsMappings = fCParamC("ProSubsMigrationMappings").

   DO liloop = 1 TO NUM-ENTRIES(lcSubsMappings,"|"):
      ASSIGN
         lcMappedSubs = ENTRY(liloop, lcSubsMappings,"|")
         lcSubsFrom = ENTRY(1,lcMappedSubs,"=")
         lcSubsTo = ENTRY(2,lcMappedSubs,"=").
      IF LOOKUP(icClitype,lcSubsFrom) GE 1 THEN RETURN lcSubsTo.
   END.
   RETURN "".
END.

FUNCTION fProMigrationRequest RETURNS INTEGER
   (INPUT  iiMsseq        AS INT,        /* msseq                */
    INPUT  icCreator      AS CHARACTER,  /* who made the request */
    INPUT  icSource       AS CHARACTER,
    INPUT  iiOrig         AS INTEGER,
    OUTPUT ocResult       AS CHARACTER):

   DEF VAR liReqCreated AS INT NO-UNDO.
   DEF VAR ldActStamp AS DEC NO-UNDO.

   ocResult = fChkRequest(iiMsSeq,
                          {&REQTYPE_PRO_MIGRATION},
                          "",
                          icCreator).

   IF ocResult > "" THEN RETURN 0.

   /* set activation time */
   ldActStamp = Func.Common:mMakeTS().

   fCreateRequest({&REQTYPE_PRO_MIGRATION},
                  ldActStamp,
                  icCreator,
                  FALSE,    /* create fees */
                  FALSE).   /* sms */

   ASSIGN
      bCreaReq.ReqCParam1  = "MIGRATE"
      bCreaReq.ReqSource   = icSource
      bCreaReq.origrequest = iiOrig
      liReqCreated         = bCreaReq.MsRequest.

   RELEASE bCreaReq.

   RETURN liReqCreated.

END FUNCTION.

FUNCTION fProMigrateOtherSubs RETURNS CHAR
(INPUT iiagrcust AS INT,
 INPUT iimsseq AS INT,
 INPUT iimsrequest AS INT,
 INPUT icsalesman AS CHAR):
   DEF BUFFER bMobSub FOR Mobsub.
   DEF VAR lcResult AS CHAR NO-UNDO.
   DEF VAR liMsReq AS INT NO-UNDO.

   FOR EACH bMobsub WHERE
            bMobsub.brand EQ Syst.Var:gcBrand AND
            bMobsub.agrCust EQ iiagrCust AND
            bMobsub.msseq NE iimsseq:
      FIND FIRST Clitype WHERE
                 Clitype.brand EQ Syst.Var:gcBrand AND
                 Clitype.clitype EQ bMobsub.clitype NO-LOCK NO-ERROR.
      IF AVAIL Clitype AND
               Clitype.webstatuscode EQ {&CLITYPE_WEBSTATUSCODE_ACTIVE}
      THEN DO:
         liMsReq = fProMigrationRequest(INPUT bMobsub.Msseq,
                                        INPUT icsalesman,
                                        INPUT {&REQUEST_SOURCE_MIGRATION},
                                        INPUT iimsrequest,
                                        OUTPUT lcResult).
      END.
      ELSE IF AVAIL Clitype AND
              fgetActiveReplacement(bMobsub.clitype) GT "" THEN DO:
         /* Make iSTC according to mapping */
         liMsReq = fCTChangeRequest(bMobSub.msseq,
                        fgetActiveReplacement(bMobsub.clitype),
                        "", /* lcBundleID */
                        "", /*bank code validation is already done */
                        Func.Common:mMakeTS(),
                        0,  /* 0 = Credit check ok */
                        0, /* extend contract */
                        "" /* pcSalesman */,
                        FALSE, /* charge */
                        TRUE, /* send sms */
                        "",
                        0,
                        {&REQUEST_SOURCE_MIGRATION},
                        0,
                        iimsrequest,
                        "", /*contract id*/
                        OUTPUT lcResult).

         IF liMsReq = 0 THEN
            RETURN "ERROR: Migration STC request creation failed. " + lcResult.

      END.
      ELSE
         RETURN "ERROR: Migration failed. " + lcResult.
     
   END.
END FUNCTION.

FUNCTION fCheckOngoingOrders RETURNS LOGICAL (INPUT icCustId AS CHAR,
                                              INPUT icCustIdType AS CHAR,
                                              INPUT iimsseq AS INT):
   FOR EACH OrderCustomer NO-LOCK WHERE
            OrderCustomer.Brand      EQ Syst.Var:gcBrand AND
            OrderCustomer.CustId     EQ icCustId AND
            OrderCustomer.CustIdType EQ icCustIDType AND
            OrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
      FIRST Order NO-LOCK WHERE
            Order.Brand              EQ Syst.Var:gcBrand AND
            Order.orderid            EQ Ordercustomer.Orderid AND
            Order.msseq              NE iimsseq AND
           LOOKUP(Order.StatusCode, {&ORDER_INACTIVE_STATUSES}) = 0:
      RETURN TRUE.
   END.
   RETURN FALSE.

END FUNCTION.

&ENDIF


