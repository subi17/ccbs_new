&IF "{&PROFUNC_REQUEST_I}" NE "YES"
&THEN
&GLOBAL-DEFINE PROFUNC_REQUEST_I YES

{Syst/eventval.i}
{Func/profunc.i}
{Func/orderfunc.i}
{Func/femailinvoice.i}
{Func/email.i}
{Func/custfunc.i}
{Func/extralinefunc.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
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
              bCustomer.CustNum EQ bMsRequest.CustNum NO-ERROR.
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
       lcOutput = REPLACE(lcOutput, "#SFID", (IF AVAIL bMobSub THEN bMobSub.Salesman  ELSE bMsRequest.Salesman)).

   IF INDEX(lcOutput, "#SECRET") > 0 THEN
       lcOutput = REPLACE(lcOutput, "#SECRET", bCustomer.RobinsonsLimit).    

   IF INDEX(lcOutput, "#EMAIL") > 0 THEN 
   DO:
      IF NUM-ENTRIES(bMSRequest.ReqCparam6,"|") GT 2 AND ENTRY(3,bMSRequest.ReqCparam6, "|") <> "" THEN
         lcReplace = ENTRY(3,bMSRequest.ReqCparam6, "|").
      ELSE IF NUM-ENTRIES(bMSRequest.ReqCparam6,"|") EQ 2 AND ENTRY(2,bMSRequest.ReqCparam6, "|") <> "" THEN
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
             lcStatus = "Pending deactivation".
         ELSE IF bMsRequest.ReqStatus EQ {&REQUEST_STATUS_CANCELLED} THEN
             lcStatus = "Active".
         ELSE 
             lcStatus = "Inactive".
      END.
      ELSE IF bMsRequest.reqtype EQ 8 THEN 
      DO:
         IF bMsRequest.reqstatus EQ {&REQUEST_STATUS_CONFIRMATION_PENDING} THEN
             lcStatus = "Pending activation".
         ELSE IF bMsRequest.ReqStatus EQ {&REQUEST_STATUS_CANCELLED} THEN
             lcStatus = "Inactive".    
         ELSE 
             lcStatus = "Active".
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

END FUNCTION.



FUNCTION fGetEmailKeyValuePairs RETURNS CHAR
   (iiMsRequest   AS INT,
    icKeyValueSrc AS CHAR):

   DEF BUFFER bMsRequest FOR MsRequest.
   DEF BUFFER bCustomer  FOR Customer.
   DEF BUFFER bMobSub    FOR MobSub.
   DEF BUFFER bCliType   FOR CliType.
   DEF BUFFER bOrder     FOR Order.
   
   DEFINE VARIABLE lcOutput  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcReplace AS CHARACTER NO-UNDO.
   
   FIND FIRST bMsRequest NO-LOCK WHERE
              bMsRequest.MsRequest EQ iiMsRequest NO-ERROR.
   IF NOT AVAIL bMsRequest THEN RETURN "ERROR: Request not found " +
                                   STRING(iiMsRequest).
   FIND FIRST bCustomer WHERE
              bCustomer.CustNum EQ bMsRequest.CustNum NO-LOCK NO-ERROR.
    IF NOT AVAIL bCustomer THEN
       RETURN "ERROR: Customer of requst not found " + STRING(iiMsRequest).

   FIND FIRST bMobSub WHERE bMobSub.MsSeq = bMsRequest.MsSeq NO-LOCK NO-ERROR.
   IF AVAIL bMobSub THEN 
       FIND FIRST bCliType WHERE bCliType.CliType = bMobSub.CliType NO-LOCK NO-ERROR.
       
   FOR EACH bOrder NO-LOCK WHERE 
            bOrder.MsSeq = bMobSub.MsSeq BY bOrder.CrStamp DESC:
       IF bOrder.OrderType NE {&ORDER_TYPE_NEW} AND
          bOrder.OrderType NE {&ORDER_TYPE_MNP} AND
          bOrder.OrderType NE {&ORDER_TYPE_STC} THEN NEXT.
       LEAVE.
   END.
      
   lcOutput = icKeyValueSrc.

   IF INDEX(lcOutput, "#CONTRACTID") > 0 THEN
      lcOutput = REPLACE(lcOutput, "#CONTRACTID", STRING(bOrder.ContractID)).
   
   IF INDEX(lcOutput, "#ORDERDATE") > 0 THEN
      lcOutput = REPLACE(lcOutput, "#ORDERDATE", STRING(Func.Common:mTSToDate(bOrder.CrStamp))).
   
   IF INDEX(lcOutput, "#CUSTTYPE") > 0 THEN
      lcOutput = REPLACE(lcOutput, "#CUSTTYPE", STRING(bCustomer.CustIdType)).
   
   IF INDEX(lcOutput, "#CUSTID") > 0 THEN
      lcOutput = REPLACE(lcOutput, "#CUSTID", STRING(bCustomer.Orgid)).
      
   IF INDEX(lcOutput, "#FIRSTNAME") > 0 THEN
      lcOutput = REPLACE(lcOutput, "#FIRSTNAME", Func.Common:mDispCustName(BUFFER bCustomer)).
      
   lcOutput = REPLACE(lcOutput, "#LASTNAME", ''). /* This is taken care in FIRSTNAME */
      
   IF INDEX(lcOutput, "#PROVINCE") > 0 THEN
   DO:
      lcOutput = REPLACE(lcOutput, "#PROVINCE"       , bCustomer.Address).
      lcOutput = REPLACE(lcOutput, "#CITY"           , bCustomer.PostOffice).
      lcOutput = REPLACE(lcOutput, "#POSTALCODE"     , bCustomer.ZipCode).
   END.
   
   IF INDEX(lcOutput, "#EMAIL") > 0 THEN 
   DO:
      IF NUM-ENTRIES(bMSRequest.ReqCparam6,"|") GT 2 AND ENTRY(3,bMSRequest.ReqCparam6, "|") <> "" THEN
         lcReplace = ENTRY(3,bMSRequest.ReqCparam6, "|").
      ELSE IF NUM-ENTRIES(bMSRequest.ReqCparam6,"|") EQ 2 AND ENTRY(2,bMSRequest.ReqCparam6, "|") <> "" THEN
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
         
   RETURN lcOutput.
END FUNCTION.

FUNCTION fCheckSubscriptionTypeAllowedForProMigration RETURNS LOGICAL
   (INPUT icCliType    AS CHAR,
    OUTPUT ocCliTypeTo AS CHAR):

   DEFINE BUFFER bf_CliType FOR CliType.
   DEFINE VARIABLE llValidExtraLine AS LOGICAL NO-UNDO.
     
   FIND FIRST bf_CliType WHERE bf_CliType.Brand   EQ Syst.Var:gcBrand AND
                               bf_CliType.clitype EQ icCliType        NO-LOCK NO-ERROR.
   IF AVAIL bf_CliType THEN
   DO:
       CASE bf_CliType.WebStatusCode:                   
           WHEN {&CLITYPE_WEBSTATUSCODE_ACTIVE} OR 
           WHEN {&CLITYPE_WEBSTATUSCODE_RETIRED} THEN  
           DO:
               llValidExtraLine =  fCLITypeIsExtraLine(bf_CliType.CliType).
               IF llValidExtraLine THEN RETURN TRUE.
                                                      
               ocCliTypeTo = fgetActiveReplacement(icCliType,"STCMappingForActiveTariffs").

               IF ocCliTypeTo = "" THEN 
               DO:
                   FIND FIRST FMItem WHERE
                              FMItem.Brand     = Syst.Var:gcBrand   AND
                              FMItem.FeeModel  > ""                 AND
                              FMItem.PriceList = "PRO_" + icCliType AND
                              FMItem.FromDate <= TODAY              AND
                              FMItem.ToDate   >= TODAY              NO-LOCK NO-ERROR.
                   IF NOT AVAILABLE FMItem THEN 
                      RETURN FALSE.                         
               END.                                              
           END.  
           OTHERWISE 
           DO:
              ocCliTypeTo = fgetActiveReplacement(icCliType,"ProSubsMigrationMappings").
              IF ocCliTypeTo = "" THEN 
                 RETURN FALSE.
          END.                                      
       END CASE.               
   END.
   
   RETURN TRUE.

END FUNCTION.

FUNCTION fProMigrationRequest RETURNS INTEGER
   (INPUT  iiMsseq        AS INTEGER  ,  /* msseq                */
    INPUT  icCreator      AS CHARACTER,  /* who made the request */
    INPUT  icSource       AS CHARACTER,
    INPUT  iiOrig         AS INTEGER  , 
    OUTPUT ocResult       AS CHARACTER):

   DEF VAR liReqCreated  AS INT  NO-UNDO.
   DEF VAR lcNewCategory AS CHAR NO-UNDO.

   DEFINE BUFFER bCustomer      FOR Customer.
   DEFINE BUFFER bMobSub        FOR MobSub.
   DEFINE BUFFER bf_MobSub      FOR MobSub.
   DEFINE BUFFER bCustCat       FOR Custcat.
   DEFINE BUFFER bOrder         FOR Order.
   DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bClitype       FOR CLIType.

   DEFINE VARIABLE llHasMappingMissingForLegacyTariff AS LOGICAL NO-UNDO INIT TRUE.
   DEFINE VARIABLE lcclitypeto   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liRequest     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcError       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llValidMaping AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE lhCustomer    AS HANDLE    NO-UNDO.
   DEFINE VARIABLE llIsExtraLineOrProMigrationAllowed AS LOGICAL NO-UNDO.

   FIND bMobsub WHERE bMobsub.brand EQ Syst.Var:gcBrand AND bMobsub.MsSeq = iiMsseq NO-LOCK NO-ERROR.
   FIND bCustomer WHERE bCustomer.Brand EQ Syst.Var:gcBrand AND bCustomer.CustNum = bMobSub.AgrCust NO-LOCK NO-ERROR.
   FIND bCustCat WHERE bCustcat.Category = bCustomer.Category NO-LOCK NO-ERROR.
   
   llIsExtraLineOrProMigrationAllowed = fCheckSubscriptionTypeAllowedForProMigration(bMobSub.CliType, OUTPUT lcCliTypeTo).
   
   IF lcCliTypeTo <> "" OR llIsExtraLineOrProMigrationAllowed THEN 
   DO:
      fgetCustSegment(bCustomer.CustIdType,
                      TRUE, /* Self Employed */
                      TRUE, /* Pro */
                      bCustomer.OrgId, 
                      OUTPUT lcNewCategory).

      IF lcNewCategory NE bCustomer.Category THEN 
      DO TRANSACTION:
         lhCustomer = BUFFER bCustomer:HANDLE.

         IF llDoEvent THEN 
         DO:
            RUN StarEventInitialize(lhCustomer).
            RUN StarEventSetOldBuffer(lhCustomer).
         END.

         FIND CURRENT bCustomer EXCLUSIVE-LOCK.
         ASSIGN bCustomer.Category = lcNewCategory.
         FIND CURRENT bCustomer NO-LOCK.

         IF llDoEvent THEN 
            RUN StarEventMakeModifyEvent(lhCustomer).
      END.
      
      IF lcCliTypeTo <> "" THEN
      DO:
         liReqCreated = fCTChangeRequest(iiMsseq,                                    /* The MSSeq of the subscription to where the STC is made */
                                      lcCliTypeTo,                                /* The CLIType of where to do the STC */
                                      "",                                         /* lcBundleID */
                                      "",                                         /* bank code validation is already done */
                                      Func.Common:mMakeTS(),
                                      0,                                          /* 0 = Credit check ok */
                                      0,                                          /* extend contract */
                                      ""                                          /* pcSalesman */,
                                      FALSE,                                      /* charge */
                                      TRUE,                                       /* send sms */
                                      "",
                                      0, 
                                      {&REQUEST_SOURCE_MIGRATION},
                                      0,
                                      iiOrig,
                                      "",                                         /* contract id */
                                      OUTPUT ocResult).
         IF ocResult > "" THEN 
            RETURN 0.  
      END. 
   END.
   ELSE 
   DO TRANSACTION:
      ocResult = fChkRequest(iiMsSeq,
                             {&REQTYPE_PRO_MIGRATION},
                             "",
                             icCreator).
      IF ocResult > "" THEN 
          RETURN 0.

      fCreateRequest({&REQTYPE_PRO_MIGRATION},
                     Func.Common:mMakeTS(),
                     icCreator,
                     FALSE,    /* create fees */
                     FALSE).   /* sms */

      ASSIGN
          bCreaReq.ReqCParam1  = "MIGRATE"
          bCreaReq.ReqSource   = icSource
          bCreaReq.origrequest = iiOrig
          liReqCreated         = bCreaReq.MsRequest.

      RELEASE bCreaReq.
   END.

   RETURN liReqCreated.

END FUNCTION.


FUNCTION fProMigrateOtherSubs RETURNS CHAR
(INPUT iiagrcust AS INT,
 INPUT iimsseq AS INT,
 INPUT iimsrequest AS INT,
 INPUT icsalesman AS CHAR):

   DEF BUFFER bMobSub FOR Mobsub.

   DEF VAR lcResult    AS CHAR NO-UNDO.
   DEF VAR liRequest   AS INT  NO-UNDO.
   DEF VAR lcCliTypeTo AS CHAR NO-UNDO.

   FOR EACH bMobsub WHERE
            bMobsub.brand EQ Syst.Var:gcBrand AND
            bMobsub.agrCust EQ iiagrCust AND
            bMobsub.msseq NE iimsseq:
      FIND FIRST Clitype WHERE
                 Clitype.brand EQ Syst.Var:gcBrand AND
                 Clitype.clitype EQ bMobsub.clitype NO-LOCK NO-ERROR.
      IF AVAIL Clitype THEN 
      DO:
         CASE Clitype.WebStatusCode:
            WHEN {&CLITYPE_WEBSTATUSCODE_ACTIVE}  OR 
            WHEN {&CLITYPE_WEBSTATUSCODE_RETIRED} THEN
            DO:
               ASSIGN lcCliTypeTo = fgetActiveReplacement(bMobsub.clitype,"STCMappingForActiveTariffs").

               IF lcCliTypeTo > "" THEN 
               DO:
                  liRequest = fCTChangeRequest(bMobSub.msseq,
                                             lcCliTypeTo,
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
                   IF liRequest = 0 THEN
                      RETURN "ERROR: Migration STC request creation failed. " + lcResult.
               END.
               ELSE
               DO:
                  liRequest = fProMigrationRequest(INPUT bMobsub.Msseq,
                                                 INPUT icsalesman,
                                                 INPUT {&REQUEST_SOURCE_MIGRATION},
                                                 INPUT iimsrequest,
                                                 OUTPUT lcResult).
                  IF liRequest = 0 THEN
                      RETURN "ERROR: Migration request creation failed. " + lcResult.
               END.
            END.
            OTHERWISE
            DO:
                ASSIGN lcCliTypeTo = fgetActiveReplacement(bMobsub.clitype,"ProSubsMigrationMappings").

                IF lcCliTypeTo > "" THEN 
                DO:
                   /* Make iSTC according to mapping */
                   liRequest = fCTChangeRequest(bMobSub.msseq,
                                              lcCliTypeTo,
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

                   IF liRequest = 0 THEN
                      RETURN "ERROR: Migration STC request creation failed. " + lcResult.
                END.
            END.
         END CASE.
      END.
      ELSE
         RETURN "ERROR: Migration failed. " + lcResult.
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
   DEF VAR lcErr     AS CHAR NO-UNDO.

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
       lcParams = lcParams + FILL("|", (4 - NUM-ENTRIES(lcParams)))
       lcParams = lcParams + lcOffer.

   IF icAction BEGINS "cancel" THEN DO:
      IF icAction EQ "cancel activation" THEN 
         liReqType = {&REQTYPE_CONTRACT_ACTIVATION}.
      ELSE IF icAction EQ "cancel deactivation" THEN
         liReqType = {&REQTYPE_CONTRACT_TERMINATION}.
      ELSE DO:
         ocErr = "Incorrect request".
         RETURN 0.
      END.
         
      FIND FIRST MsRequest NO-LOCK WHERE
                 MsRequest.Brand EQ Syst.Var:gcBrand AND
                 MsRequest.ReqType EQ liReqType AND
                 MsRequest.ReqStatus EQ {&REQUEST_STATUS_CONFIRMATION_PENDING} AND
                 MsRequest.ReqCParam3 EQ icContr NO-ERROR.
      IF NOT AVAIL MsRequest THEN DO:
         ocErr = "Cancellation not possible, request not found".
         RETURN 0.
      END.

      fReqStatus(4, "SVA Operation Cancellation").

      fSendEmailByRequest(MsRequest.MsRequest,"SVA_" + icContr).

      RETURN MsRequest.MsRequest.
   END.
   ELSE IF icAction EQ "on" THEN 
      icAction = "act".
   ELSE IF icAction EQ "off" THEN 
      icAction = "term".

   DO TRANS:
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

END FUNCTION.

FUNCTION fTerminateSVAs RETURNS LOGICAL
   (INPUT iiMsseq AS INT,
    INPUT ilWaitConfirm AS LOG):
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR liAmt AS INT  NO-UNDO.
   
   DEF BUFFER MSRequest FOR MSRequest.
   DEF BUFFER bMsRequest FOR MSRequest.

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq EQ iiMsSeq AND
            MsRequest.ReqType EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
            MsRequest.ReqStatus EQ {&REQUEST_STATUS_DONE}
            BREAK BY MsRequest.ReqCparam3
            BY MsRequest.actstamp DESC:
      IF NOT FIRST-OF(MsRequest.ReqCparam3) THEN NEXT.
      IF NOT fisSVA(msRequest.reqcparam3, OUTPUT liAmt) THEN NEXT.
      IF CAN-FIND(FIRST bMsRequest NO-LOCK WHERE
                        bMsRequest.MsSeq EQ iiMsSeq AND
                        bMsRequest.ReqType EQ {&REQTYPE_CONTRACT_TERMINATION} AND
                        bMsRequest.ReqStatus EQ {&REQUEST_STATUS_DONE} AND
                        bMsrequest.reqcparam3 EQ msrequest.reqcparam3 AND
                        bMsrequest.actstamp > Msrequest.actstamp) THEN NEXT.
      fMakeProActRequest(iiMsSeq,
                         msrequest.reqcparam3,
                         Func.Common:mSecOffSet(Func.Common:mMakeTS(),60),
                         STRING(ilWaitConfirm),
                         "",
                         "term",
                         lcErr).
   END.

   RETURN TRUE.
END.

&ENDIF

